# Code shamelessly pulled from:
# https://www.r-bloggers.com/simulating-random-multivariate-correlated-data-continuous-variables/
library(faux)
library(tidyverse)
library(mirt)

likert_test <- function(n, r){
  R = matrix(cbind(1, r, r, r, r, r, r, r, r, r,  #design matrix
                   r, 1, r, r, r, r, r, r, r, r,  
                   r, r, 1, r, r, r, r, r, r, r,
                   r, r, r, 1, r, r, r, r, r, r,
                   r, r, r, r, 1, r, r, r, r, r,
                   r, r, r, r, r, 1, r, r, r, r,
                   r, r, r, r, r, r, 1, r, r, r,
                   r, r, r, r, r, r, r, 1, r, r,
                   r, r, r, r, r, r, r, r, 1, r,
                   r, r, r, r, r, r, r, r, r, 1), 
             nrow=10)
  U = t(chol(R)) #transposing design matrix, cholesky decomposition
  nvars = dim(U)[1] #selecting number of variables
  n = n #set number of observations
  random.normal = matrix(rnorm(nvars*n, 0, 1), nrow=nvars, ncol=numobs);
  X = U %*% random.normal
  newX = t(X)
  
  raw = as.data.frame(newX)
  prob_3cat   <- c(.25, .50, .25)
  prob_5cat   <- c(.1, .2, .4, .2, .1)
  prob_7cat   <- c(.06, .12, .19, .26, .19, .12, .06)
  prob_9cat   <- c(.04, .08, .12, .16, .20, .16, .12, .08, .04)
  prob_11cat  <- c(.03, .06, .08, .11, .14, .16, .14, .11, .08, .06, .03)
  #putting data into likert-form
  orig.likert.3cat <- raw %>% mutate(V1 = norm2likert(V1, prob = prob_3cat),
                                     V2 = norm2likert(V2, prob = prob_3cat),
                                     V3 = norm2likert(V3, prob = prob_3cat),
                                     V4 = norm2likert(V4, prob = prob_3cat),
                                     V5 = norm2likert(V5, prob = prob_3cat),
                                     V6 = norm2likert(V6, prob = prob_3cat),
                                     V7 = norm2likert(V7, prob = prob_3cat),
                                     V8 = norm2likert(V8, prob = prob_3cat),
                                     V9 = norm2likert(V9, prob = prob_3cat),
                                     V10 = norm2likert(V10, prob = prob_3cat))
  
  orig.likert.5cat <- raw %>% mutate(V1 = norm2likert(V1, prob = prob_5cat),
                                     V2 = norm2likert(V2, prob = prob_5cat),
                                     V3 = norm2likert(V3, prob = prob_5cat),
                                     V4 = norm2likert(V4, prob = prob_5cat),
                                     V5 = norm2likert(V5, prob = prob_5cat),
                                     V6 = norm2likert(V6, prob = prob_5cat),
                                     V7 = norm2likert(V7, prob = prob_5cat),
                                     V8 = norm2likert(V8, prob = prob_5cat),
                                     V9 = norm2likert(V9, prob = prob_5cat),
                                     V10 = norm2likert(V10, prob = prob_5cat))
  
  orig.likert.7cat <- raw %>% mutate(V1 = norm2likert(V1, prob = prob_7cat),
                                     V2 = norm2likert(V2, prob = prob_7cat),
                                     V3 = norm2likert(V3, prob = prob_7cat),
                                     V4 = norm2likert(V4, prob = prob_7cat),
                                     V5 = norm2likert(V5, prob = prob_7cat),
                                     V6 = norm2likert(V6, prob = prob_7cat),
                                     V7 = norm2likert(V7, prob = prob_7cat),
                                     V8 = norm2likert(V8, prob = prob_7cat),
                                     V9 = norm2likert(V9, prob = prob_7cat),
                                     V10 = norm2likert(V10, prob = prob_7cat))
  
  orig.likert.9cat <- raw %>% mutate(V1 = norm2likert(V1, prob = prob_9cat),
                                     V2 = norm2likert(V2, prob = prob_9cat),
                                     V3 = norm2likert(V3, prob = prob_9cat),
                                     V4 = norm2likert(V4, prob = prob_9cat),
                                     V5 = norm2likert(V5, prob = prob_9cat),
                                     V6 = norm2likert(V6, prob = prob_9cat),
                                     V7 = norm2likert(V7, prob = prob_9cat),
                                     V8 = norm2likert(V8, prob = prob_9cat),
                                     V9 = norm2likert(V9, prob = prob_9cat),
                                     V10 = norm2likert(V10, prob = prob_9cat))
  
  orig.likert.11cat <- raw %>% mutate(V1 = norm2likert(V1, prob = prob_11cat),
                                      V2 = norm2likert(V2, prob = prob_11cat),
                                      V3 = norm2likert(V3, prob = prob_11cat),
                                      V4 = norm2likert(V4, prob = prob_11cat),
                                      V5 = norm2likert(V5, prob = prob_11cat),
                                      V6 = norm2likert(V6, prob = prob_11cat),
                                      V7 = norm2likert(V7, prob = prob_11cat),
                                      V8 = norm2likert(V8, prob = prob_11cat),
                                      V9 = norm2likert(V9, prob = prob_11cat),
                                      V10 = norm2likert(V10, prob = prob_11cat))
  
  three_cat_graded <- mirt(orig.likert.3cat, 1, itemtype = 'graded')
  plot(three_cat_graded, type = 'info')
  
  five_cat_graded <- mirt(orig.likert.5cat, 1, itemtype = 'graded')
  plot(five_cat_graded, type = 'info')
  
  seven_cat_graded <- mirt(orig.likert.7cat, 1, itemtype = 'graded')
  plot(seven_cat_graded, type = 'info')
  
  nine_cat_graded <- mirt(orig.likert.9cat, 1, itemtype = 'graded')
  plot(nine_cat_graded, type = 'info')
  
  eleven_cat_graded <- mirt(orig.likert.11cat, 1, itemtype = 'graded')
  plot(eleven_cat_graded, type = 'info')
  
  #extracing test information curves
  
  Theta <- matrix(seq(-6, 6, .01))
  test_info_data <- data.frame(three = testinfo(three_cat_graded, Theta),
                               five  = testinfo(five_cat_graded, Theta),
                               seven = testinfo(seven_cat_graded, Theta),
                               nine  = testinfo(nine_cat_graded, Theta),
                               eleven = testinfo(eleven_cat_graded, Theta),
                               theta = Theta)
  test_info_data <- test_info_data %>% mutate(id = row_number())
  
  #putting data into long form for plot
  
  test_info_long <- test_info_data %>% gather(key = id, value = testinfo, 1:5)
  
  #plotting results
  
  ggplot(test_info_long, aes(x = theta, y = testinfo, color = id)) + geom_line()
}

likert_test(100, .8)


r = .7 #set correlation

R = matrix(cbind(1, r, r, r, r, r, r, r, r, r,  #design matrix
                 r, 1, r, r, r, r, r, r, r, r,  
                 r, r, 1, r, r, r, r, r, r, r,
                 r, r, r, 1, r, r, r, r, r, r,
                 r, r, r, r, 1, r, r, r, r, r,
                 r, r, r, r, r, 1, r, r, r, r,
                 r, r, r, r, r, r, 1, r, r, r,
                 r, r, r, r, r, r, r, 1, r, r,
                 r, r, r, r, r, r, r, r, 1, r,
                 r, r, r, r, r, r, r, r, r, 1), 
           nrow=10)
U = t(chol(R)) #transposing design matrix, cholesky decomposition
nvars = dim(U)[1] #selecting number of variables
n = 100000 #set number of observations

set.seed(1) #set seed

random.normal = matrix(rnorm(nvars*n, 0, 1), nrow=nvars, ncol=numobs);
X = U %*% random.normal
newX = t(X)

raw = as.data.frame(newX)
orig.raw = as.data.frame(t(random.normal))

cor(raw) #simulation worked!
cor(orig.raw)


library(faux)
library(tidyverse)

prob_3cat   <- c(.25, .50, .25)
prob_5cat   <- c(.1, .2, .4, .2, .1)
prob_7cat   <- c(.06, .12, .19, .26, .19, .12, .06)
prob_9cat   <- c(.04, .08, .12, .16, .20, .16, .12, .08, .04)
prob_11cat  <- c(.03, .06, .08, .11, .14, .16, .14, .11, .08, .06, .03)

#probabilities calculated by summing the values of (1/5), (2/5), (3/5), (2/5), (1/5)
#and dividing proportions by that summed valued. all are slightly rounded off.
# test <- c((1/5), (2/5), (3/5), (2/5), (1/5))
# sum(test)
# test_div = test / 1.8

#putting data into likert-form
orig.likert.3cat <- raw %>% mutate(V1 = norm2likert(V1, prob = prob_3cat),
                                   V2 = norm2likert(V2, prob = prob_3cat),
                                   V3 = norm2likert(V3, prob = prob_3cat),
                                   V4 = norm2likert(V4, prob = prob_3cat),
                                   V5 = norm2likert(V5, prob = prob_3cat),
                                   V6 = norm2likert(V6, prob = prob_3cat),
                                   V7 = norm2likert(V7, prob = prob_3cat),
                                   V8 = norm2likert(V8, prob = prob_3cat),
                                   V9 = norm2likert(V9, prob = prob_3cat),
                                   V10 = norm2likert(V10, prob = prob_3cat))

orig.likert.5cat <- raw %>% mutate(V1 = norm2likert(V1, prob = prob_5cat),
                                   V2 = norm2likert(V2, prob = prob_5cat),
                                   V3 = norm2likert(V3, prob = prob_5cat),
                                   V4 = norm2likert(V4, prob = prob_5cat),
                                   V5 = norm2likert(V5, prob = prob_5cat),
                                   V6 = norm2likert(V6, prob = prob_5cat),
                                   V7 = norm2likert(V7, prob = prob_5cat),
                                   V8 = norm2likert(V8, prob = prob_5cat),
                                   V9 = norm2likert(V9, prob = prob_5cat),
                                   V10 = norm2likert(V10, prob = prob_5cat))

orig.likert.7cat <- raw %>% mutate(V1 = norm2likert(V1, prob = prob_7cat),
                                   V2 = norm2likert(V2, prob = prob_7cat),
                                   V3 = norm2likert(V3, prob = prob_7cat),
                                   V4 = norm2likert(V4, prob = prob_7cat),
                                   V5 = norm2likert(V5, prob = prob_7cat),
                                   V6 = norm2likert(V6, prob = prob_7cat),
                                   V7 = norm2likert(V7, prob = prob_7cat),
                                   V8 = norm2likert(V8, prob = prob_7cat),
                                   V9 = norm2likert(V9, prob = prob_7cat),
                                   V10 = norm2likert(V10, prob = prob_7cat))

orig.likert.9cat <- raw %>% mutate(V1 = norm2likert(V1, prob = prob_9cat),
                                   V2 = norm2likert(V2, prob = prob_9cat),
                                   V3 = norm2likert(V3, prob = prob_9cat),
                                   V4 = norm2likert(V4, prob = prob_9cat),
                                   V5 = norm2likert(V5, prob = prob_9cat),
                                   V6 = norm2likert(V6, prob = prob_9cat),
                                   V7 = norm2likert(V7, prob = prob_9cat),
                                   V8 = norm2likert(V8, prob = prob_9cat),
                                   V9 = norm2likert(V9, prob = prob_9cat),
                                   V10 = norm2likert(V10, prob = prob_9cat))

orig.likert.11cat <- raw %>% mutate(V1 = norm2likert(V1, prob = prob_11cat),
                                    V2 = norm2likert(V2, prob = prob_11cat),
                                    V3 = norm2likert(V3, prob = prob_11cat),
                                    V4 = norm2likert(V4, prob = prob_11cat),
                                    V5 = norm2likert(V5, prob = prob_11cat),
                                    V6 = norm2likert(V6, prob = prob_11cat),
                                    V7 = norm2likert(V7, prob = prob_11cat),
                                    V8 = norm2likert(V8, prob = prob_11cat),
                                    V9 = norm2likert(V9, prob = prob_11cat),
                                    V10 = norm2likert(V10, prob = prob_11cat))

#running mirt models

library(mirt)
library(psych)

three_cat_graded <- mirt(orig.likert.3cat, 1, itemtype = 'graded')
plot(three_cat_graded, type = 'info')

five_cat_graded <- mirt(orig.likert.5cat, 1, itemtype = 'graded')
plot(five_cat_graded, type = 'info')

seven_cat_graded <- mirt(orig.likert.7cat, 1, itemtype = 'graded')
plot(seven_cat_graded, type = 'info')

nine_cat_graded <- mirt(orig.likert.9cat, 1, itemtype = 'graded')
plot(nine_cat_graded, type = 'info')

eleven_cat_graded <- mirt(orig.likert.11cat, 1, itemtype = 'graded')
plot(eleven_cat_graded, type = 'info')

#extracing test information curves

Theta <- matrix(seq(-6, 6, .01))
test_info_data <- data.frame(three = testinfo(three_cat_graded, Theta),
                             five  = testinfo(five_cat_graded, Theta),
                             seven = testinfo(seven_cat_graded, Theta),
                             nine  = testinfo(nine_cat_graded, Theta),
                             eleven = testinfo(eleven_cat_graded, Theta),
                             theta = Theta)
test_info_data <- test_info_data %>% mutate(id = row_number())

#putting data into long form for plot

test_info_long <- test_info_data %>% gather(key = id, value = testinfo, 1:5)

#plotting results

ggplot(test_info_long, aes(x = theta, y = testinfo, color = id)) + geom_line()
