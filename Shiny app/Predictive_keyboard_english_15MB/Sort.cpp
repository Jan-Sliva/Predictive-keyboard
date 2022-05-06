#include <Rcpp.h>
#include <algorithm>
#include <float.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector SortNGramTreeWithDict(Rcpp::List items, int itemsCount, IntegerVector resultDict){
  
  int count = std::min((int) itemsCount, (int) items.length());
  
  int topXListIndeces[count];
  double topXListValues[count];
  for (int i = 0; i < count; i++) topXListValues[i] = DBL_MIN;
  
  for (int index = 0; index < items.length(); index++){
    S4 item = items[index];
    double value = item.slot("freq");
    
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
  IntegerVector ret = IntegerVector::create();
  for (int i = 0; i < count; i++)  ret.push_back(resultDict[topXListIndeces[i]]);
  return ret;
}

// [[Rcpp::export]]
List SortNGramTree(Rcpp::List items, int itemsCount){
  
  int count = std::min((int) itemsCount, (int) items.length());
  
  int topXListIndeces[count];
  double topXListValues[count];
  for (int i = 0; i < count; i++) topXListValues[i] = DBL_MIN;
  
  for (int index = 0; index < items.length(); index++){
    S4 item = items[index];
    double value = item.slot("freq");
    
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
