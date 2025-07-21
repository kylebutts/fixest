#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2025-07-21
# ~: debug tools
#------------------------------------------------------------------------------#


debug_msg = function(...){
  # writes a message in debug.txt
  msg = sma(..., .envir = parent.frame())
  message(msg)
  f = file("./../debug.txt", "a")
  writeLines(msg, f)
  close(f)
}

debug_save = function(){
  # saves all the variables from the current function
  vars = ls(envir = parent.frame())
  
  env = new.env(parent = emptyenv())
  for(v in vars){
    value = try(get(v, envir = parent.frame()), silent = TRUE)
    if(!is_error(value)){
      assign(v, value, env)
    }
  }
  
  save(list = names(env), envir = env, file = "./../debug.RData")
}

debug_load = function(){
  
  load("./../debug.RData", parent.frame())
  
}

