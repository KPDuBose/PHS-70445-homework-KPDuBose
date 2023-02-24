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
  
  LogicalVector treatment = is_treated;
  NumericVector treated;
  NumericVector untreated;
  
  /*
   ... Implement your matching (start from Week 5's lab)... 
   ... You have to consider that matches are done against groups, i.e.,
   Treated (is_treated == true) must be matched to control 
   (is_treated == false)  
   */
for (int i = 0; i < n; i++){
  
  if (treatment[i]){
    
    double cur_best = std::numeric_limits< double >::max(); 
    auto & cur_i    = indices[i];
    
    for (int j = 0; j < n; j++){
      
      if (!treatment[j]){
        
        double d = std::abs(pscores[i] - pscores[j]);
        
        if (d < cur_best){
          
          cur_best = d;
          cur_i = j;
          
        }
      
        if (d < values[j]) {
        
          values[j] = d;
          indices[j] = i;
        
      }
      
      }
      
      
      
    
    
    }
  
  
  treated.push_back(i);
  untreated.push_back(cur_i);
  }
  
}

int m = treated.size();
NumericVector values1(m);

int k = untreated.size();
NumericVector values2(k);


for (int i = 0; i < treated.size(); ++i) 
  values1[i] = pscores[treated[i]];

for (int i = 0; i < untreated.size(); ++i) 
  values2[i] = pscores[untreated[i]];





return List::create(
  _["match_treated_id"] = treated + 1, // We add one to match R's indices
  _["match_pscore_treated"]  = values1,
  _["match_untreated_id"] = untreated + 1,
  _["match_pscore_untreated"] = values2
);
    
}


/*** R
set.seed(123)
pscores <- runif(10)
is_treated <- sample(c(0,1), 10, replace = TRUE)

psmatch(pscores, is_treated)
*/
