#include <Rcpp.h>
#include <functional>
#include <limits.h>
using namespace Rcpp;


template<typename T>
List TopXSortCpp(Rcpp::List items, std::function<int(T)> paramFunc, int count){
  
  int topXListIndeces[count];
  int topXListValues[count];
  for (int i = 0; i < count; i++) topXListValues[i] = INT_MIN;
  
  for (int index = 0; index < items.length(); index++){
    T item = items[index];
    int value = paramFunc(item);
    
    if (value > topXListValues[count-1]){
      for ( int i = (count - 1); i >= 0; i--){
        if( (i == 0) || (value <= topXListValues[i-1] ) ){
          if (count >= (i + 2)){
            for (int e = count; e >= (i + 2); e--) {
              topXListIndeces[e-1] =  topXListIndeces[e-2];
              topXListValues[e-1] =  topXListValues[e-2];
            }
          }
          topXListIndeces[i] =  index;
          topXListValues[i] = value;
          break;
        }
      }
    }
  }
  List ret = List::create();
  for (int i = 0; i < count; i++)  ret.push_back(items[topXListIndeces[i]]);
  return ret;
}

// [[Rcpp::export]]
List TopXSortCppList(List items, int count){
  
  return TopXSortCpp<List>(items, [](List node)->int{ return (int) node["freq"];}, count);
}

// [[Rcpp::export]]
List TopXSortCppS3(List items, int count){
  
  return TopXSortCpp<List>(items, [](List node)->int{ return (int) node["freq"];}, count);
}

// [[Rcpp::export]]
List TopXSortCppS4(List items, int count){
  
  return TopXSortCpp<S4>(items, [](S4 node)->int{ return (int) node.slot("freq");}, count);
}

// [[Rcpp::export]]
List TopXSortCppR5(List items, int count){
  
  return TopXSortCpp<Environment>(items, [](Environment node)->int{ return (int) node["freq"];}, count);
}

// [[Rcpp::export]]
List TopXSortCppR6(List items, int count){
  
  return TopXSortCpp<Environment>(items, [](Environment node)->int{ return (int) node["freq"];}, count);
}
