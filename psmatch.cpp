#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List psmatch(
    NumericVector pscores,
    LogicalVector is_treated
)
{
  /*... setup the problem creating the output...*/
  int n = static_cast<int>(pscores.size());
  
  IntegerVector indices(n);
  NumericVector values(n);
  values.fill(std::numeric_limits< double >::max());
  
  
  
  /*
   ... Implement your matching (start from Week 5's lab)... 
   ... You have to consider that matches are done against groups, i.e.,
   Treated (is_treated == true) must be matched to control 
   (is_treated == false)  
   */
  
  for (int i = 0; i < n; ++i) {
    
    if (is_treated[i]){
      
      double & cur_best = values[i];
      auto & cur_i      = indices[i];
      
      for (int j = 0; j < n; ++j) {
        
        if (!is_treated[j]){
        // If it is lower, then update
        double d = std::abs(pscores[i] - pscores[j]);
        if (d < cur_best) {
          
          cur_best = d;
          cur_i    = j;
          
        }

        if (d < values[j]) {
          
          values[j] = d;
          indices[j] = i;
          
        }
      
    }
      }
    
  }
    
  for (int i = 0; i < n; ++i) 
      values[i] = pscores[indices[i]];
  
  
  // Returning
  return List::create(
    _["match_id"] = indices + 1,
  _["match_pscore"] = values
  );
}
}

/*** R
set.seed(123)
pscores <- runif(10)
is_treated <- sample(c(0,1), 10, replace = TRUE)

psmatch(pscores, is_treated)
*/
