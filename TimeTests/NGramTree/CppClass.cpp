#include <Rcpp.h>
using namespace Rcpp;

class CppNode {
  
  public: 
    int freq;
  
  CppNode(int freq){
    this->freq = freq;
  }
};

// [[Rcpp::export]]
NumericVector CreateCppNodes(int count, int seed, int maxValue){
  
  return lapply(seq_len(count), [](int num){return num;} );
}