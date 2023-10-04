---
title: "Week 1 Problem Set"
author: "Paulo Magalang"
date: "2023-10-04"
output: 
  html_document: 
    keep_md: yes
---


```r
library(rethinking)
library(ggplot2)
```

1. Suppose the globe tossing data (Lecture 2, Chapter 2) had turned out to be 4 water
and 11 land. Construct the posterior distribution.


```r
# set a seed for consistency
set.seed(12345)

# generate sample, sim_globe-esque
x <- sample(c(rep("W", 4), rep("L", 11)))

compute_posterior <- function(the_sample, poss = c(0, 0.25, 0.5, 0.75, 1)) {
  W <- sum(the_sample == "W") # num of W observed
  L <- sum(the_sample == "L") # num of L observed
  ways <- sapply(poss, function(q) (q*4)^W * ((1-q)*4)^L)
  post <- ways / sum(ways)
  bars <- sapply(post, function(q) make_bar(q))
  data.frame(poss, ways, post = round(post, 3), bars)
}

post_tbl <- compute_posterior(x, poss = seq(0, 1, by = 0.1))
post_tbl
```

```
##    poss         ways  post                 bars
## 1   0.0 0.000000e+00 0.000                     
## 2   0.1 3.369516e+04 0.068 #                   
## 3   0.2 1.475740e+05 0.300 ######              
## 4   0.3 1.719742e+05 0.350 #######             
## 5   0.4 9.972490e+04 0.203 ####                
## 6   0.5 3.276800e+04 0.067 #                   
## 7   0.6 5.836665e+03 0.012                     
## 8   0.7 4.566946e+02 0.001                     
## 9   0.8 9.007199e+00 0.000                     
## 10  0.9 7.044820e-03 0.000                     
## 11  1.0 0.000000e+00 0.000
```


```r
# alternate approach
likelihood <- function(W, L, p) dbinom(W, W + L, p)
prior <- function(p) return(1)

candidates <- seq(0, 1, length = 20)
un_standard_post <- likelihood(4, 11, candidates) * prior(candidates) # posterior is proportional to likelihood * prior
plot(un_standard_post)
```

![](week1_problems_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

2. Using the posterior distribution from 1, compute the posterior predictive distribution
for the next 5 tosses of the same globe. I recommend you use the sampling method.


```r
# use table from 1, work with posterior probabilities
post_samples <- post_tbl$post

# get sim function
sim_globe <- function(p = 0.7, N = 9) {
  sample(c("W", "L"), size = N, prob = c(p, 1-p), replace = TRUE)
}

# n = 5 because an extra 5 tosses
pred_post <- sapply(post_samples, function(p) sum(sim_globe(p, 5) == "W"))
tab_post <- table(pred_post)
```


```r
samples_of_p <- sample(post_tbl$poss, 1e4, replace = TRUE, prob = post_tbl$post)
hist(samples_of_p)
```

![](week1_problems_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
sim_5_toss <- rbinom(length(samples_of_p), 5, prob = samples_of_p)
hist(sim_5_toss)
```

![](week1_problems_files/figure-html/unnamed-chunk-5-2.png)<!-- -->



3. Use the posterior predictive distribution from 2 to calculate the probability of 3 or
more water samples in the next 5 tosses.


```r
hist(sim_5_toss, prob = T)
```

![](week1_problems_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
# sum across (probability of 3 and 4 and 5)
mean(sim_5_toss >= 3)
```

```
## [1] 0.1757
```

































