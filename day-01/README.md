Advent of Code Day 01
================
Lizzie Pearmain
1 December 2020

# Part 1 - two numbers

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

## Loop

I am going to use a loop to find the answer. Not the most efficient method, but it gets the job done.

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

## Results

My numbers are: 1069 and 951.

Sum of the numbers: 2020.

Product of the numbers: 1016619.

# Part 2 - three numbers

## Aim

Find the *three* numbers that sum to 2020, and calculate their product.

## Loop

Using a similar loop, but making it more efficient - I'm only storing the index of the current three numbers as i, j and k, and only comparing each number against the other numbers that I haven't compared it against.

Also using a `stop` variable to make the `if` and `break` calls neater.

``` r
aim <- 2020

stop <- F

for (i in 1:(length(v)-2)) {
  
  # i <- 1  # testing
  
  for (j in (i+1):(length(v)-1)) {
    
    # j <- i+1  # testing
    
    for (k in (j+1):length(v)) {
      
      # k <- j+1  # testing
      # cat((v[i] + v[j] + v[k]), "\n")  # print out
      
      if ((v[i] + v[j] + v[k]) == aim) {
        stop <- T
        break
      }
      
    }
    
    if(stop) {break}
    
  }
  
  if (stop) {break}
  
}
```

## Results

My numbers are: 473, 405 and 1142.

Sum of the numbers: 2020.

Product of the numbers: 218767230.
