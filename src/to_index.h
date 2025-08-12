

#include <stdint.h>
#include <cmath>
#include <vector>
#include <string>
#include <algorithm>
#include <R.h>
#include <Rinternals.h>


namespace indexthis {
  
using std::vector;

enum {T_INT, T_DBL_INT, T_DBL, T_STR};

inline int double_to_uint32(const double &x){
  uint32_t y[2];
  std::memcpy(y, &x, sizeof(y));
  return y[0] + y[1];
}

inline int power_of_two(double x){
  return std::ceil(std::log2(x + 1));
}

inline uint32_t hash_single(uint32_t value, int shifter){
  return (3141592653U * value >> (32 - shifter));
}

inline uint32_t hash_double(uint32_t v1, uint32_t v2, int shifter){
  return (((3141592653U * v1) ^ (3141592653U * v2)) >> (32 - shifter));
}

inline bool is_equal_dbl(double x, double y){
  return std::isnan(x) ? std::isnan(y) : x == y;
}

//
// r_vector --------------------------------------------------------------------
//


// Class very useful to pass around the data on R vectors 
class r_vector {
  r_vector() = delete;
  
  SEXP x_conv;
  
public:
  r_vector(SEXP);
  r_vector(vector<int>);
  
  int size() const {return n;}
  
  // public properties
  int n;
  bool is_fast_int = false;
  int x_range = 0;
  int x_range_bin = 0;
  int x_min = 0;
  int type = 0;
  
  // if a non numeric non character vector has been turned into character
  // we need to keep track of protection
  bool is_protect = false;
  
  // this is only used in the quick ints algorithm
  // for factors and bool we assume there are NAs since we don't traverse the data
  //  to find the range, contrary to ints or dbl_ints
  bool any_na = true;
  int NA_value = -1;
  
  // pointers: only the valid one ends up non-null
  int *px_int = (int *) nullptr;
  double *px_dbl = (double *) nullptr;
  intptr_t *px_intptr = (intptr_t *) nullptr;
  
};

r_vector::r_vector(SEXP x){
  
  int n = Rf_length(x);
  this->n = n;
  
  bool IS_INT = false;
  
  if(TYPEOF(x) == STRSXP){
    // character
    this->type = T_STR;
    this->px_intptr = (intptr_t *) STRING_PTR_RO(x);
    
  } else if(Rf_isNumeric(x) || Rf_isFactor(x) || TYPEOF(x) == LGLSXP){
      
    if(TYPEOF(x) == REALSXP){
      // we check if the underlying structure is int
      this->px_dbl = REAL(x);
      IS_INT = true;
      double *px = REAL(x);
      double x_min = 0, x_max = 0, x_tmp;
      
      // taking care of NA corner cases
      int i_start = 0;
      while(i_start < n && std::isnan(px[i_start])){
        ++i_start;
      }
      
      bool any_na = i_start > 0;
      if(i_start < n){
        x_min = px[i_start];
        x_max = px[i_start];
        
        for(int i=i_start ; i<n ; ++i){
          x_tmp = px[i];
          
          if(std::isnan(x_tmp)){
            any_na = true;
          } else if(!(x_tmp == (int) x_tmp)){
            IS_INT = false;
            break;
          } else if(x_tmp > x_max){
            x_max = x_tmp;
          } else if(x_tmp < x_min){
            x_min = x_tmp;
          }
        }
      }      
      
      this->any_na = any_na;

      this->x_min = static_cast<int>(x_min);
      // +1 for the NAs
      this->x_range = x_max - x_min + 2;
      
      this->type = IS_INT ? T_DBL_INT : T_DBL;
    } else {
      // logical, factor and integer are all integers
      IS_INT = true;
      this->px_int = INTEGER(x);
      this->type = T_INT;
      
      if(TYPEOF(x) == INTSXP){
        int *px = INTEGER(x);
        int x_min = 0, x_max = 0, x_tmp;
        
        // taking care of NA corner cases
        int i_start = 0;
        while(i_start < n && px[i_start] == NA_INTEGER){
          ++i_start;
        }
        bool any_na = i_start > 0;
        if(i_start < n){
          x_min = px[i_start];
          x_max = px[i_start];
          
          for(int i=i_start ; i<n ; ++i){
            x_tmp = px[i];
            
            if(x_tmp > x_max){
              x_max = x_tmp;
            } else if(x_tmp < x_min){
              // NA integer is the smallest int, defined as -2147483648
              if(x_tmp == NA_INTEGER){
                any_na = true;
              } else {
                x_min = x_tmp;
              }            
            }
          }
        }
        
        this->any_na = any_na;
        this->x_min = x_min;
        // +1 for the NAs
        this->x_range = x_max - x_min + 2;
      } else if(TYPEOF(x) == LGLSXP){
        this->x_min = 0;
        // 0, 1, NA
        this->x_range = 3;
      } else {
        // factor
        SEXP labels = Rf_getAttrib(x, R_LevelsSymbol);
        // factors always start at 1
        this->x_min = 1;
        // we add 1 for the NAs
        this->x_range = Rf_length(labels) + 1;
      }
    }
    
    if(IS_INT){
      // finding out if we're in the easy case
      this->x_range_bin = power_of_two(this->x_range);    
      this->is_fast_int = this->x_range < 100000 || this->x_range <= 2*n;
      this->NA_value = this->x_range - 1;
    }
    
  } else {
    // we apply a conversion to a known type
    
    if(TYPEOF(x) == CHARSXP || TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP || 
      TYPEOF(x) == REALSXP || TYPEOF(x) == CPLXSXP || TYPEOF(x) == STRSXP || TYPEOF(x) == RAWSXP){
      // we convert to character
      SEXP call_as_character = PROTECT(Rf_lang2(Rf_install("as.character"), x));
  
      int any_error;
      this->x_conv = PROTECT(R_tryEval(call_as_character, R_GlobalEnv, &any_error));

      if(any_error){
        Rf_error("In `to_index`, the vector to index was not standard (int or real, etc) and failed to be converted to character before applying indexation._n");
      }
      
      // conversion succeeded
      this->type = T_STR;
      this->px_intptr = (intptr_t *) STRING_PTR_RO(this->x_conv);
      this->is_protect = true;
      
    } else {
      Rf_error("In `to_index`, the R vectors must be atomic. The current type is not valid.");
    }    
    
  }
}


r_vector::r_vector(vector<int> x){
  
  this-> n = x.size();
  
  this->px_int = x.data();
  this->type = T_INT;
  int x_max = *std::max_element(x.begin(), x.end());
  int x_min = *std::min_element(x.begin(), x.end());
  
  this->x_range = x_max - x_min + 1;
  this->x_range_bin = power_of_two(this->x_range);    
  this->is_fast_int = this->x_range < 100000 || this->x_range <= 2*n;
  
  this->any_na = false;
  this->NA_value = this->x_range - 1;
  
}

//
//  IndexedVector --------------------------------------------------------------
//


class IndexedVector {
public:
  
  IndexedVector() = delete;
  IndexedVector(SEXP);
  IndexedVector(vector<int> &);
  
  vector<int> firstobs;
  vector<int> table;
  vector<double> sum;
  
  int *p_index = nullptr;
  int n = 0;
  
  int n_protect = 0;
};

IndexedVector::IndexedVector(SEXP x){
  // x is the vector that will receive the index
  
  if(TYPEOF(x) != INTSXP){
    Rf_error("The class IndexedVector can only be initialized with INTEGER R objects.");
  }
  
  p_index = INTEGER(x);
  n = Rf_length(x);
  
}

IndexedVector::IndexedVector(vector<int> &x){
  
  p_index = x.data();
  n = x.size();
  
}

//
// function declarations -------------------------------------------------------
//

void to_index_main(const SEXP &x, IndexedVector &output);

void to_index_main(const SEXP &x, IndexedVector &output,
                   bool do_sum, const double *p_vec_to_sum);

void to_index_main(const r_vector &x, IndexedVector &output);

void to_index_main(const r_vector &x, IndexedVector &output, 
                   bool do_sum, const double *p_vec_to_sum);

void to_index_main(const std::vector<r_vector> &x, IndexedVector &output);

void to_index_main(const std::vector<r_vector> &x, IndexedVector &output, 
                   bool do_sum, const double *p_vec_to_sum);

} // namespace indexthis




