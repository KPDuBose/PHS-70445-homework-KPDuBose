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
    1 fun1()      19.4   23.2       1        62.8     2.53
    2 fun1alt()    1      1        22.1       1       1   

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
    1 fun1(dat)     2.81   3.10      1         196.     12.8
    2 fun1alt(dat)  1      1         3.52        1       1  

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
    1 fun2(dat)     5.23   4.42      1         1         1  
    2 fun2alt(dat)  1      1         4.02      3.48     23.7

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
    1 fun2(x)     8.62   7.15      1         1        1.98
    2 fun2alt(x)  1      1         7.23      1.20     1   
