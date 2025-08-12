/***********************************************************************
 * ____________________________                                        *
 * || Fixed-effects Indexing ||                                        *
 * ----------------------------                                        *
 *                                                                     *
 * Author: Laurent R. Berge                                            *
 *                                                                     *
 * Turns the fixed-effects indicators into integers, and drops the FEs *
 * that need to be dropped because of singleton or perfect fit         *
 *                                                                     *
 **********************************************************************/

#include "Rcpp.h"
#include "to_index.h"


SEXP cpp_index_table_sum(SEXP fixef_list, SEXP y, const bool do_sum_y, 
                         const bool rm_0, const bool rm_1, const bool rm_single, 
                         const Rcpp::IntegerVector only_slope, const int nthreads){
  
  
  int Q = Rf_length(fixef_list);
  SEXP fixef_vec = VECTOR_ELT(fixef_list, 0);
  int n = Rf_length(fixef_vec);

  std::vector<bool> check_pblm(Q, true);
  if(only_slope.length() == Q){
    for(int q=0 ; q<Q ; ++q){
      // we check for pblm only if NOT only slope
      check_pblm[q] = only_slope[q] == false;
    }
  }
  
  double *py = nullptr;
  if(TYPEOF(y) == REALSXP){
    py = REAL(y);
  } else {
    // => there will be no use of y, so nullptr is OK
    // but I must ensure that beforehand: do_sum_y = rm_0 = rm_1 = false
    if(do_sum_y || rm_0 ||rm_1){
      Rcpp::stop("y should not be a list when its values are assessed.");
    }
  }
  
  // we intialize the information on the indexes to be computed
  std::vector<indexthis::IndexedVector> all_index_info(Q);
  
  
}





