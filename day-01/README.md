Advent of Code Day 01
================
Lizzie Pearmain
1 December 2020

## Aim

Have a list of numbers. Two of them sum to 2020. I need to find these two numbers, and multiply them together. This is my answer.

## Read input

``` r
# read input
v <- readLines("input.txt") %>%
  as.integer()
head(v)
```

    ## [1]  261 1773 1839 1551 1781 1276

``` r
tail(v)
```

    ## [1] 1272 1636 1352 1496 1455 1488

## Loop version

``` r
aim <- 2020

for (i in 1:length(v)) {
  # i <- 1  # testing
  
  tmp.num <- v[i]  # first number
  tmp.others <- v[-i]  # vector of second numbers
  
  for (k in 1:length(tmp.others)) {
    
    # k <- 1  # testing
    
    tmp.oth <- tmp.others[k]  # second number
    tmp.res <- tmp.num + tmp.oth  # find sum
    
    # break loop when aim is reached
    if (tmp.res == aim) {
      break
    }
    
  }
  
  # break look when aim is reached
  if (tmp.res == aim) {
    break
  }
  
}
```

My numbers are: 1069 and 951.

Sum of the numbers: 2020.

Product of the numbers: 1016619.
