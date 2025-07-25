
#include "fixest_main.h"

using namespace Rcpp;
using std::vector;

// [[Rcpp::export]]
List cpp_index_table_sum(SEXP x_list, SEXP y, bool do_sum_y, bool rm_0, bool rm_1,
                         bool rm_single, IntegerVector only_slope, int nthreads,
                         bool do_refactor, SEXP r_x_sizes, IntegerVector obs2keep){
  
  // x: List of vectors of IDs (type int/num or char only)
  // y: dependent variable
  // rm_0: remove FEs where dep var is only 0
  // rm_1: remove FEs where dep var is only 0 or 1
  // rm_single: remove FEs with only one observation
  // do_sum_y: should we compute the sum_y?

  // When the data is refactored (ie x is a fixef_id_list, each element ranging from 1 to n_items):
  // - do_refactor
  // - r_x_sizes => a vector of length Q, the number of items for each FE
  // - obs2keep => vector of observations to keep. The neutral is 0.

  int Q = Rf_length(x_list);
  SEXP xq = VECTOR_ELT(x_list, 0);
  int n = Rf_length(xq);
  
  //
  // step 0: formatting the necessary input/output
  //
  
  double *py = nullptr;
  if(TYPEOF(y) == REALSXP){
    py = REAL(y);
  } else {
    // => there will be no use of y, so nullptr is OK
    // but I must ensure that beforehand: do_sum_y = rm_0 = rm_1 = false
    if(do_sum_y || rm_0){
      stop("y should not be a list when its values are assessed.");
    }
  }
  
  if(Rf_length(y) == 1){
    do_sum_y = false;
  }
  
  // I create a fake vector to avoid conditional calls later on
  vector<int> x_sizes_fake(Q, 0);
  int *px_sizes = nullptr;
  int n_keep = obs2keep.length();
  bool identical_x = false;
  
  if(do_refactor){
    // x_list: fixef_id_list
    identical_x = obs2keep[0] == 0;
    px_sizes = INTEGER(r_x_sizes);
    
  } else {
    px_sizes = x_sizes_fake.data();
    
  }
  
  // the vectors of indexed
  List res_x_index_all(Q);       // the R object that will be returned
  vector<int*> p_x_index_all(Q); // the pointers to their elements
  
  for(int q=0 ; q<Q ; ++q){
    if(identical_x){
      SEXP x_val = VECTOR_ELT(x_list, q);
      res_x_index_all[q] = x_val;
      p_x_index_all[q]   = INTEGER(x_val);
    } else {
      res_x_index_all[q] = PROTECT(Rf_allocVector(INTSXP, do_refactor ? n_keep : n));
      p_x_index_all[q]   = INTEGER(res_x_index_all[q]);
    }
  }

  vector< vector<int> > x_table_all(Q);
  vector< vector<double> > x_unik_all(Q);
  vector<int> any_pblm(Q, 0);
  vector< vector<bool> > id_pblm_all(Q);
  // The following may not be needed:
  vector< vector<double> > sum_y_all(Q);
  vector<bool> obs_removed;
  vector< vector<double> > x_removed_all(Q);
  
  vector<void *> px_all(Q);
  // vector to store modified strings
  
  //
  // the loop 
  //
  
  
  bool continue_algo = true;
  while(continue_algo){
    continue_algo = false;
    
    #pragma omp parallel for num_threads(nthreads)
    for(int q=0 ; q<Q ; ++q){
      
    }
    
    
    
  }
  
  

}