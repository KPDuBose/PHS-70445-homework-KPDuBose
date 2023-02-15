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
              nrow = n,
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
    1 fun1()      17.1   22.5       1        62.8     3.03
    2 fun1alt()    1      1        19.9       1       1   

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
  # Insert code here
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
  # Insert code here
}

# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
bench::mark(
  fun1(dat),
  fun1alt(dat), relative = TRUE
)

# Test for the second
bench::mark(
  fun2(dat),
  fun2alt(dat), relative = TRUE
)
```

## Function 3

Find the column max (hint: Check out the function `{r} max.col()`)
