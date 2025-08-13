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


void mark_obs_to_remove(std::vector<char> &removed_flag, bool &any_removed,
                        const indexthis::IndexedVector &index_info, 
                        const bool rm_0, const bool rm_1, const bool rm_single){
  
  const std::vector<int> &firstobs = index_info.get_firstobs();
  const std::vector<int> &table = index_info.get_table();
  const std::vector<double> &sum_y = index_info.get_sum();
  
  //
  // step 1: we check if at least one is removed 
  //
  
  // G: number of groups
  const int G = table.size();
  bool any_to_remove = false;
  int i_start = 0;
  for(int g = 0 ; g < G ; ++g){
    if(rm_single && table[g] == 1){
      any_to_remove = true;
      i_start = firstobs[g] - 1;
      break;
    } else if(rm_0 && sum_y[g] == 0){
      any_to_remove = true;
      i_start = firstobs[g] - 1;
      break;
    } else if(rm_1 && sum_y[g] == table[g]){
      any_to_remove = true;
      i_start = firstobs[g] - 1;
      break;
    }
  }
  
  if(!any_to_remove){
    return;
  }
  
  //
  // step 2: we mark each observation 
  //
  
  // we will remove at least one
  any_removed = true;
  
  int *p_index = index_info.get_p_index();
  const int n = index_info.size();
  for(int i = i_start ; i < n ; ++i){
    const int g = p_index[i] - 1;
    if(rm_single && table[g] == 1){
      removed_flag[i] = 1;
    } else if(rm_0 && sum_y[g] == 0){
      removed_flag[i] = 1;
    } else if(rm_1 && sum_y[g] == table[g]){
      removed_flag[i] = 1;
    }
  }
  
}

SEXP cpp_index_table_sum(SEXP fixef_list, SEXP y, const bool do_sum_y, 
                         const bool rm_0, const bool rm_1, const bool rm_single, 
                         const Rcpp::IntegerVector only_slope, const int nthreads){
  
  
  int Q = Rf_length(fixef_list);
  SEXP fixef_vec = VECTOR_ELT(fixef_list, 0);
  int n = Rf_length(fixef_vec);
  
  // do_removal => whether we check for removal
  std::vector<bool> do_removal(Q, true);
  bool any_to_check_for_removal = rm_0 || rm_1 || rm_single;
  
  if(any_to_check_for_removal && only_slope.length() == Q){
    bool any_do_removal = false;
    for(int q=0 ; q<Q ; ++q){
      // we check for pblm only if NOT only slope
      do_removal[q] = only_slope[q] == false;
      any_do_removal = any_do_removal || do_removal[q];
    }
    
    any_to_check_for_removal = any_do_removal;
  }
  
  if(!any_to_check_for_removal){
    do_removal = std::vector<bool>(Q, false);
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
  std::vector< std::vector<int> > all_index_vectors(Q);
  std::vector<indexthis::IndexedVector> all_index_info(Q);
  for(int q = 0 ; q < Q ; ++q){
    std::vector<int> &vec = all_index_vectors[q];
    vec = std::vector<int>(n);
    all_index_info[q].initialize(vec);
  }
  
  std::vector<char> removed_flag;
  if(any_to_check_for_removal){
    removed_flag = std::vector<char>(n, 0);
  }
  
  bool keep_running = true;
  while(keep_running){
    keep_running = false;
    
    bool any_removed = false;
    for(int q = 0 ; q < Q ; ++q){
      const SEXP &fixef_vec = VECTOR_ELT(fixef_list, q);
      indexthis::IndexedVector &index_info = all_index_info[q];
      indexthis::to_index_main(fixef_vec, index_info);
      
      if(do_removal[q]){
        mark_obs_to_remove(removed_flag, any_removed, index_info, rm_0, rm_1, rm_single);
      }
    }
    
  }
  
  
}





