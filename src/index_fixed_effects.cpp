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

inline std::vector<int> seq(int from, int to){
  const int n = to - from + 1;
  std::vector<int> res(n);
  for(int i = 0 ; i < n ; ++i){
    res[i] = i + from;
  }
  
  return res;
}

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
  const int n_obs = Rf_length(fixef_vec);
  
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
  std::vector< indexthis::IndexInputVector > all_input_vectors(Q);
  std::vector< std::vector<int> > all_index_vectors(Q);
  std::vector<indexthis::IndexedVector> all_index_info(Q);
  for(int q = 0 ; q < Q ; ++q){
    const SEXP &fixef_vec = VECTOR_ELT(fixef_list, q);
    all_input_vectors[q].initialize(fixef_vec);
    
    std::vector<int> &vec = all_index_vectors[q];
    vec = std::vector<int>(n_obs);
    all_index_info[q].initialize(vec);
  }
  
  // below: only used when some observations were removed
  std::vector< std::vector<int> > all_raw_input_vectors(Q);
  
  std::vector<char> removed_flag;
  std::vector<int> obs_keep;
  std::vector<int> obs_removed;
  if(any_to_check_for_removal){
    removed_flag = std::vector<char>(n_obs, 0);
  }
  
  bool keep_running = true;
  bool first_iter = true;
  while(keep_running){
    keep_running = false;
    first_iter = false;
    
    const int n_current = all_input_vectors[0].size();
    bool any_removed = false;
    #pragma omp parallel for num_threads(nthreads)
    for(int q = 0 ; q < Q ; ++q){
      const indexthis::IndexInputVector &fixef_vec = all_input_vectors[q];
      indexthis::IndexedVector &index_info = all_index_info[q];
      indexthis::to_index_main(fixef_vec, index_info);
      
      if(do_removal[q]){
        mark_obs_to_remove(removed_flag, any_removed, index_info, rm_0, rm_1, rm_single);
      }
    }
    
    if(any_removed){
      keep_running = true;
      
      if(first_iter){
        // This is the initialization: first iteration of the while loop
        obs_keep = seq(1, n_obs);
        for(int i = 0 ; i < n_current ; ++i){
          if(removed_flag[i] == 1){
            obs_removed.push_back(i);
          } else {
            obs_keep.erase(obs_keep.begin() + i);
          }
        }
        
      } else {
        // here we use the information on the obs_keep bc we nee to track 
        // which observation was removed
        for(int i = 0 ; i < n_current ; ++i){
          if(removed_flag[i] == 1){
            obs_removed.push_back(obs_keep[i]);
            obs_keep.erase(obs_keep.begin() + i);
          }
        }
        
      }
      
      if(obs_keep.empty()){
        Rcpp::List res = Rcpp::List::create(Rcpp::Named("all_removed") = true);
        return res;
      }
      
      // we take the index just computed (in index_info) as the new input
      const int n_new = obs_keep.size();
      #pragma omp parallel for num_threads(nthreads)
      for(int q = 0 ; q < Q ; ++q){
        
        // 1) we used the coputed index as new input
        int *p_index = all_index_info[q].get_p_index();
        std::vector<int> new_input(n_new);
        int index = 0;
        for(int i = 0 ; i < n_current ; ++i){
          if(removed_flag[i] == 0){
            new_input[index++] = p_index[i];
          }
        }
        
        all_raw_input_vectors[q] = std::move(new_input);
        all_input_vectors[q].initialize(all_raw_input_vectors[q]);
        
        // 2) we reset the containers of the future indexes
        all_index_vectors[q] = std::vector<int>(n_new);
        all_index_info[q].initialize(all_index_vectors[q]);
      }
      
    }
    
  }
  
  //
  // building the return object 
  //
  
  
  
  
  
}





