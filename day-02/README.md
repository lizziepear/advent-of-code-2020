Advent of Code Day 02
================
Lizzie Pearmain
02 December, 2020

-   [Part 1](#part-1)
    -   [make rule](#make-rule)
    -   [apply rule](#apply-rule)
    -   [get answer](#get-answer)
-   [Part 2](#part-2)
    -   [make rule](#make-rule-1)
    -   [apply rule](#apply-rule-1)
    -   [get answer](#get-answer-1)

# Part 1

Format: '1-3 a: abcde' means the password must contain between 1 and 3 instances of the letter 'a', and the string after the colon is the password. I have to check whether each password obeys its rule.

``` r
## mini test set
test.v <- c("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")
test.df <- data.frame(string = test.v)
glimpse(test.df)
```

    ## Rows: 3
    ## Columns: 1
    ## $ string <chr> "1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"

``` r
## full set
v <- readLines("input.txt")
df <- data.frame(string = v)
glimpse(df)
```

    ## Rows: 1,000
    ## Columns: 1
    ## $ string <chr> "2-5 z: zzztvz", "2-8 d: pddzddkdvqgxndd", "4-14 r: rrrjrrrr...

## make rule

Function: Split string; define the rule; assess whether the password meets the rule.

``` r
## define function
assess_rule <- function(string) {
  
  splits <- as.character(unlist(strsplit(string, "-|\\s|:")))
  splits <- splits[splits != ""]  # remove empty strings
  
  r.min <- as.numeric(splits[1])
  r.max <- as.numeric(splits[2])
  r.letter <- as.character(splits[3])
  r.passwd <- as.character(splits[4])
  
  r.n <- str_count(r.passwd, r.letter)
  
  r.pass <- (r.n >= r.min & r.n <= r.max)

  return(r.pass)
  
}

## Vectorize the function
assess_rule_v <- Vectorize(assess_rule, vectorize.args = "string")

## test on mini set
assess_rule_v(test.v)
```

    ##     1-3 a: abcde     1-3 b: cdefg 2-9 c: ccccccccc 
    ##             TRUE            FALSE             TRUE

## apply rule

Apply to `string` field in data frame using `dplyr::mutate()`

``` r
df <- df %>%
  mutate(passes = assess_rule_v(string))

glimpse(df)
```

    ## Rows: 1,000
    ## Columns: 2
    ## $ string <chr> "2-5 z: zzztvz", "2-8 d: pddzddkdvqgxndd", "4-14 r: rrrjrrrr...
    ## $ passes <lgl> TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TR...

## get answer

How many passwords are valid?

``` r
df %>%
  group_by(passes) %>%
  count() %>%
  kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
passes
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
540
</td>
</tr>
<tr>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
460
</td>
</tr>
</tbody>
</table>
# Part 2

New rule: The numbers actually refer to positions in the password (indexing from 1), and exactly one of those positions must have this letter.

1-3 a: abcde is valid: position 1 contains a and position 3 does not.

## make rule

``` r
string <- v[1]

## define function
assess_rule_2 <- function(string) {
  
  splits <- as.character(unlist(strsplit(string, "-|\\s|:")))
  splits <- splits[splits != ""]  # remove empty strings
  
  r.a <- as.numeric(splits[1])
  r.b <- as.numeric(splits[2])
  r.letter <- as.character(splits[3])
  r.passwd <- as.character(splits[4])
  
  ## get indices of letter in password
  r.indices <- as.numeric(unlist(gregexpr(pattern = r.letter, r.passwd)))
  
  ## rule: exclusive or statement
  r.pass <- xor((r.a %in% r.indices), (r.b %in% r.indices))

  return(r.pass)
  
}

## Vectorize the function
assess_rule_2_v <- Vectorize(assess_rule_2, vectorize.args = "string")

## test on mini set
assess_rule_2_v(test.v)
```

    ##     1-3 a: abcde     1-3 b: cdefg 2-9 c: ccccccccc 
    ##             TRUE            FALSE            FALSE

## apply rule

Apply to `string` field in data frame using `dplyr::mutate()`

``` r
df <- df %>%
  mutate(passes_2 = assess_rule_2_v(string))

glimpse(df)
```

    ## Rows: 1,000
    ## Columns: 3
    ## $ string   <chr> "2-5 z: zzztvz", "2-8 d: pddzddkdvqgxndd", "4-14 r: rrrjrr...
    ## $ passes   <lgl> TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, ...
    ## $ passes_2 <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE...

## get answer

How many passwords are valid under the new rule?

``` r
df %>%
  group_by(passes_2) %>%
  count() %>%
  kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
passes\_2
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
749
</td>
</tr>
<tr>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
251
</td>
</tr>
</tbody>
</table>
