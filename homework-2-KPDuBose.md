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
    1 fun1()      17.8   22.8       1        62.8     2.82
    2 fun1alt()    1      1        21.0       1       1   

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
    1 fun1(dat)     2.77   3.20      1         196.     12.1
    2 fun1alt(dat)  1      1         3.57        1       1  

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
    1 fun2(dat)     5.31   4.36      1         1         1  
    2 fun2alt(dat)  1      1         4.08      3.48     25.7

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
    1 fun2(x)     9.32   7.65      1         1        1.97
    2 fun2alt(x)  1      1         7.56      1.20     1   

# Part 2: Rcpp code

As we saw in the Rcpp week, vectorization may not be the best solution.
For this part, you must write a function using Rcpp that implements the
propensity score matching algorithm. You can use [Week 5’s
lab](https://github.com/UofUEpiBio/PHS7045-advanced-programming/issues/8#issuecomment-1424974938)
as a starting point for the problem. Your C++ file should look something
like the following:

``` rcpp

#include<Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
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
  
  for (int i = 0; i < n; ++1) {
    
    double & cur_best = values[i];
    auto & cur_i      = indices[i];
    
    
    
    
    
}
  
  
  // Returning
  return List::create(
    _["match_id"] = /*...*/
    _["match_pscore"] = /*...*/,
  );
}
```
