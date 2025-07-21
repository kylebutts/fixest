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

debug_save = function(path = NULL){
  # saves all the variables from the current function
  vars = ls(envir = parent.frame())
  
  env = new.env(parent = emptyenv())
  for(v in vars){
    value = try(get(v, envir = parent.frame()), silent = TRUE)
    if(!is_error(value)){
      assign(v, value, env)
    }
  }
  
  check_arg(path, "NULL path create")
  if(is.null(path)){
    path = "./../debug.RData"
  }
  
  save(list = names(env), envir = env, file = path)
}

debug_load = function(path = NULL){
  
  check_arg(path, "NULL path create")
  if(is.null(path)){
    path = "./../debug.RData"
  }
  
  load(path, parent.frame())
  
}

any_variable_different_from_saved = function(path = NULL){
  
  check_arg(path, "NULL path create")
  if(is.null(path)){
    path = "./../debug.RData"
  }
  
  env_old = new.env()
  load(path, envir = env_old)
  
  env_new = parent.frame()
  
  is_different = FALSE
  for(v in names(env_old)){
    x = env_old[[v]]
    y = env_new[[v]]
    if(!is_similar(x, y)){
      mema("Object {bq ? v} is not identical")
      is_different = TRUE
    }
  }
  
  return(is_different)
}


is_similar = function(x, y){
  # we focus on the values and not the attributes
  
  if(identical(x, y)){
    return(TRUE)
  }
  
  if(length(x) != length(y)){
    return(FALSE)
  }
  
  if(!identical(class(x), class(y))){
    return(FALSE)
  }
  
  if(is.function(x)){
    # functions are OK, but we check their environment
    if(!identical(formalArgs(x), formalArgs(y))){
      return(FALSE)
    }
    
    env_x = environment(x)
    env_y = environment(y)
    
    return(is_similar(env_x, env_y))
  }
  
  x_clean = x
  attributes(x_clean) = NULL
  y_clean = y
  attributes(y_clean) = NULL
  
  if(identical(x_clean, y_clean)){
    return(TRUE)
  }
  
  if(!(is.list(x) || is.environment(x))){
    return(FALSE)
  }
  
  for(i in seq_along(x)){
    if(!is_similar(x[[i]], y[[i]])){
      return(FALSE)
    }
  }
  
  return(TRUE)
}

