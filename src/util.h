//=========================================================================//
//            Author: Laurent R. Bergé, University of Bordeaux             //
//             Copyright (C) 2025-present, Laurent R. Bergé                //
//=========================================================================//

// This code is from sircon

#pragma once

// I need to comment this since my templates need c++17 to work => I only use it when I debug
// #define DEBUGFIXEST 1 

#ifdef DEBUGFIXEST

#include <string>
#include <vector>
#include <iostream>
  
using std::string;
using std::vector;

namespace util {

const string FG_DEBUG = "\033[33m";
const string FG_DEBUG_TITLE = "\033[38;2;218;112;214m";
const string FG_DEFAULT = "\033[39m";

//
// text formatting 
//
  
string txt(const vector<string>& x);
string txt(vector<string>& x);

template<typename T>
string txt(T&& x){
  
  using Tclean = typename std::remove_reference<typename std::remove_const<T>::type>::type;
  
  if constexpr (std::is_convertible_v<Tclean, std::string> or std::is_convertible_v<Tclean, std::string_view>) {
    return x;
    
  } else if constexpr (std::is_pointer_v<Tclean>){
    return std::to_string(reinterpret_cast<uintptr_t>(x));
    
  } else {
    return std::to_string(x);
  }
}

inline string txt(const char* x){
  return string(x);
}

template<typename T>
string vector_to_string(vector<T>&& x){
  if(x.empty()){
    return "[empty]";
  }
  
  using Tclean = typename std::remove_reference<typename std::remove_cv<T>::type>::type;
  
  string res;
  int iter = 0;
  
  for(auto &value : x){
    
    if(iter++ > 0){
      res += ", ";
    }
    
    if constexpr (std::is_same_v<Tclean, std::string>){
      res += "\"" + value + "\"";
      
    } else if constexpr (std::is_convertible_v<Tclean, std::string> or std::is_convertible_v<Tclean, std::string_view>) {
      res += "\"" + static_cast<std::string>(value) + "\"";
      
    } else if constexpr (std::is_pointer_v<Tclean>){
      res += std::to_string(reinterpret_cast<uintptr_t>(value));
      
    } else {
      res += std::to_string(value);
    }
  }
  
  return res;
}

template<typename T>
string vector_to_string(const vector<T>& x){
  if(x.empty()){
    return "[empty]";
  }
  
  using Tclean = typename std::remove_reference<typename std::remove_cv<T>::type>::type;
  
  string res;
  int iter = 0;
  
  for(auto &value : x){
    
    if(iter++ > 0){
      res += ", ";
    }
    
    if constexpr (std::is_same_v<Tclean, std::string>){
      res += "\"" + value + "\"";
      
    } else if constexpr (std::is_convertible_v<Tclean, std::string> or std::is_convertible_v<Tclean, std::string_view>) {
      res += "\"" + static_cast<std::string>(value) + "\"";
      
    } else if constexpr (std::is_pointer_v<Tclean>){
      res += std::to_string(reinterpret_cast<uintptr_t>(value));
      
    } else {
      res += std::to_string(value);
    }
  }
  
  return res;
}

template<typename T>
string txt(vector<T>&& x){
  return vector_to_string<T>(std::forward<vector<T>&&>(x));
}

template<typename T>
string txt(vector<T>& x){
  return vector_to_string<T>(std::forward<vector<T>&&>(x));
}

template<typename T>
string txt(const vector<T>& x){
  return vector_to_string<T>(x);
}

template<typename T1, typename T2>
string txt(T1&& x, T2&& y){
  return (txt(x) + txt(y));
}

template<typename T1, typename T2, typename... T_rest>
string txt(T1&& x, T2&& y, T_rest... rest){
  return txt(x) + txt(y, rest...);
}

inline string txt(vector<string>& x){
  return vector_to_string<std::string>(std::forward<vector<string>>(x));
}

//
// message 
//

namespace internal {
  static int debug_depth = 0;
}

template<typename... T_all>
void msg(T_all&&... all){
  string t = txt(all...);
  std::cout << t << "\n";
}

// the class and the debug function share the same name 
// so they're easy to locate and remove
template<typename... T_all>
void debug_msg(T_all&&... all){
  
  string t = txt(all...);
  
  if(internal::debug_depth > 0){
    t = string(internal::debug_depth, '=') + " " + t;
  }
  
  std::cout << FG_DEBUG << t << FG_DEFAULT << "\n";
  
}

class Debug_Msg {
public:

  template<typename... T_all>
  Debug_Msg(T_all&&... all){
    ++internal::debug_depth;
    
    string t = txt(all...);
    debug_msg(FG_DEBUG_TITLE + t + FG_DEFAULT);
  }
  
  ~Debug_Msg(){
    --internal::debug_depth;
  }
};
  

} // end namepsace util


#endif

