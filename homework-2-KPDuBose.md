Assignment 2 - PHS 7045
================
Kline DuBose

# Due Date

Tuesday, February 28

# Background

For this assignment, you’ll be quested with speeding up some code using
what you have learned about vectorization and Rcpp.

# Part 1: Vectorizing code

## Function 1

This function generates a `n x k` dataset with all its entries
distributed Poisson with mean `lambda`.

``` r
# function that was given
fun1 <- function(n = 100, k = 4, lambda = 4){
  x <- NULL
  
  for (i in 1:n) {
    x <- rbind(x, rpois(k, lambda))
  }
  
  return(x)
}

# my function
fun1alt <- function(n = 100, k = 4, lambda = 4){
  x <- matrix(data = rpois(n * k, lambda = lambda),
              ncol = k)
  return(x)
  
}

# benchmarking
bench::mark(
  fun1(),
  fun1alt(), relative = TRUE, check = FALSE
)
```

    # A tibble: 2 × 6
      expression   min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr> <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 fun1()      17.3   23.7       1        62.8     2.90
    2 fun1alt()    1      1        20.6       1       1   

## Function 2

Like before, speed up the following functions (it is OK to use
StackOverflow)

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n)
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  rowSums(mat)
}


# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}


fun2alt <- function(mat) {
  n <- nrow(mat)
  ans <- mat
  for (i in 1:n) {
    ans[i,] <- cumsum(mat[i,])
  }
  ans
}

## Another function I'm trying to make faster
# fun2alt <- function(mat) {
#   ans <- mat
#   ans[] <- vapply(ans, cumsum, FUN.VALUE = 1)
#   ans
# }

# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
bench::mark(
  fun1(dat),
  fun1alt(dat), relative = TRUE
)
```

    # A tibble: 2 × 6
      expression     min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>   <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 fun1(dat)     6.47   3.94      1         196.      Inf
    2 fun1alt(dat)  1      1         3.73        1       NaN

``` r
# Test for the second
bench::mark(
  fun2(dat),
  fun2alt(dat), relative = TRUE
)
```

    # A tibble: 2 × 6
      expression     min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>   <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 fun2(dat)     5.72   4.44      1         1         NaN
    2 fun2alt(dat)  1      1         4.08      3.48      Inf

## Function 3

Find the column max (hint: Check out the function `max.col()`)

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow = 10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}

# My function
fun2alt <- function(x) {
  x[cbind(max.col(t(x)), 1:ncol(x))]
}

# Benchmarking
bench::mark(
  fun2(x),
  fun2alt(x), relative = TRUE
)
```

    # A tibble: 2 × 6
      expression   min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr> <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 fun2(x)     9.26   8.03      1         1        2.19
    2 fun2alt(x)  1      1         7.45      1.20     1   

# Part 2: Rcpp code

As we saw in the Rcpp week, vectorization may not be the best solution.
For this part, you must write a function using Rcpp that implements the
propensity score matching algorithm. You can use [Week 5’s
lab](https://github.com/UofUEpiBio/PHS7045-advanced-programming/issues/8#issuecomment-1424974938)
as a starting point for the problem. Your C++ file should look something
like the following:

``` rcpp

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
for (int i = 0; i < n; i++){     /* This matches someone in the treatment group to one person 
                                    in the control group*/
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

NumericVector values1(treated.size());

NumericVector values2(untreated.size());

for (int i = 0; i < treated.size(); ++i) 
  values1[i] = pscores[treated[i]];

for (int i = 0; i < untreated.size(); ++i) 
  values2[i] = pscores[untreated[i]];

return List::create(/*Returns the ID's of the treated group, the matched control
                      group, and the p-values of each respective patient*/
  _["match_treated_id"] = treated + 1, // We add one to match R's indices
  _["match_untreated_id"] = untreated + 1,
  _["match_pscore_treated"]  = values1,
  _["match_pscore_untreated"] = values2
);
    
}
```

``` r
set.seed(123)
pscores <- runif(30)
is_treated <- sample(c(0,1), 30, replace = TRUE)

psmatch(pscores, is_treated)
```

    $match_treated_id
     [1]  2  4  5 10 12 13 18 21 26 27 29

    $match_untreated_id
     [1] 22  8 20  3  3 22  6  8 22  9  1

    $match_pscore_treated
     [1] 0.78830514 0.88301740 0.94046728 0.45661474 0.45333416 0.67757064
     [7] 0.04205953 0.88953932 0.70853047 0.54406602 0.28915974

    $match_pscore_untreated
     [1] 0.6928034 0.8924190 0.9545036 0.4089769 0.4089769 0.6928034 0.0455565
     [8] 0.8924190 0.6928034 0.5514350 0.2875775
