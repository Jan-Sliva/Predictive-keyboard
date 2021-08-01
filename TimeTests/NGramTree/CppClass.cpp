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
SEXP CreateCppNodes(int count){
  
  std::vector<CppNode>* nodes = new std::vector<CppNode>;
  
  for (int i = 0; i < count; i++){
    nodes->push_back(CppNode(i + 1));
  }
  
  return Rcpp::XPtr<std::vector<CppNode>>(nodes);
  
}


// [[Rcpp::export]]
IntegerVector GetFreqs(SEXP xptr){
  
  Rcpp::XPtr<std::vector<CppNode>> nodes(xptr);
  
  int count = nodes->size();
  
  IntegerVector ret = rep(0, count);
  
  for (int i = 0; i < count; i++){
    ret[i] = (nodes->operator[](i)).freq;
  }
  return ret;
}

// [[Rcpp::export]]
int GetFreq(SEXP xptr, int index){
  
  Rcpp::XPtr<std::vector<CppNode>> nodes(xptr);
  
  return (nodes->operator[](index)).freq;
}

// [[Rcpp::export]]
List TopXSort(SEXP xptr, int count){
  
  Rcpp::XPtr<std::vector<CppNode>> items(xptr);
  
  return List();
}

/*** R

  count <- 20

  test <- CreateCppNodes(count)
  GetFreq(test, 15)
  GetFreqs(test)
*/
