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
  
  Theta <- matrix(seq(-6, 6, .0001))
  test_info_data <- data.frame(Three = testinfo(three_cat_graded, Theta),
                               Five  = testinfo(five_cat_graded, Theta),
                               Seven = testinfo(seven_cat_graded, Theta),
                               Nine  = testinfo(nine_cat_graded, Theta),
                               Eleven = testinfo(eleven_cat_graded, Theta),
                               theta = Theta
)
  test_info_data <- test_info_data %>% mutate(`Likert Category` = row_number())
  
  #putting data into long form for plot
  
  test_info_long <- test_info_data %>% gather(key = `Likert Category`, value = testinfo, 1:5)
  
  test_info_long$`Likert Category` <- factor(test_info_long$`Likert Category`, ordered = TRUE, levels = c("Three", "Five", "Seven", "Nine", "Eleven"))
  #plotting results
  
  ggplot(test_info_long, aes(x = theta, y = testinfo, fill = `Likert Category`)) + geom_line()
}


r9 <- likert_test(100000, .90)
r8 <- likert_test(100000, .80)
r7 <- likert_test(100000, .70)
r6 <- likert_test(100000, .60)
r5 <- likert_test(100000, .50)
r4 <- likert_test(100000, .40)

rxx9 = 10
rxx95 = 20

r9data <- r9$data
r9plot <- ggplot(r9data, aes(x = theta, y = testinfo, color = `Likert Category`)) + 
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = c("#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f"))+
  labs(title = "Test Information Plot", subtitle = "r = 0.90", y = "Test Information",
       caption = "When Test Information = 10, Reliability = 0.90, When Test Information = 20, Reliability = 0.95") +
  xlab(expression(theta)) +
  geom_hline(yintercept = rxx9)+ 
  geom_hline(yintercept = rxx95)

r9plot

r8data <- r8$data
r8plot <- ggplot(r8data, aes(x = theta, y = testinfo, color = `Likert Category`)) + 
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = c("#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f"))+
  labs(title = "Test Information Plot", subtitle = "r = 0.80", y = "Test Information",
       caption = "When Test Information = 10, Reliability = 0.90, When Test Information = 20, Reliability = 0.95") +
  xlab(expression(theta)) +
  geom_hline(yintercept = rxx9)+ 
  geom_hline(yintercept = rxx95)

r8plot

r7data <- r7$data
r7plot <- ggplot(r7data, aes(x = theta, y = testinfo, color = `Likert Category`)) + 
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = c("#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f"))+
  labs(title = "Test Information Plot", subtitle = "r = 0.70", y = "Test Information",
       caption = "When Test Information = 10, Reliability = 0.90, When Test Information = 20, Reliability = 0.95") +
  xlab(expression(theta)) +
  geom_hline(yintercept = rxx9)+ 
  geom_hline(yintercept = rxx95)

r7plot

r6data <- r6$data
r6plot <- ggplot(r7data, aes(x = theta, y = testinfo, color = `Likert Category`)) + 
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = c("#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f"))+
  labs(title = "Test Information Plot", subtitle = "r = 0.60", y = "Test Information",
       caption = "When Test Information = 10, Reliability = 0.90, When Test Information = 20, Reliability = 0.95") +
  xlab(expression(theta)) +
  geom_hline(yintercept = rxx9)+ 
  geom_hline(yintercept = rxx95)

r6plot

r5data <- r5$data
r5plot <- ggplot(r7data, aes(x = theta, y = testinfo, color = `Likert Category`)) + 
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = c("#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f"))+
  labs(title = "Test Information Plot", subtitle = "r = 0.50", y = "Test Information",
       caption = "When Test Information = 10, Reliability = 0.90, When Test Information = 20, Reliability = 0.95") +
  xlab(expression(theta)) +
  geom_hline(yintercept = rxx9)+ 
  geom_hline(yintercept = rxx95)

r5plot

r4data <- r4$data
r4plot <- ggplot(r7data, aes(x = theta, y = testinfo, color = `Likert Category`)) + 
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = c("#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f"))+
  labs(title = "Test Information Plot", subtitle = "r = 0.40", y = "Test Information",
       caption = "When Test Information = 10, Reliability = 0.90, When Test Information = 20, Reliability = 0.95") +
  xlab(expression(theta)) +
  geom_hline(yintercept = rxx9)+ 
  geom_hline(yintercept = rxx95)

r4plot













n500r6 <- likert_test(500, .6)

n5000r8 <- likert_test(5000, .8)

testdata <- n5000r8$data

rxx9 = 10
rxx95 = 20

ggplot(testdata, aes(x = theta, y = testinfo, color = `Likert Category`)) + 
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = c("#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f"))+
  labs(title = "Test Information Plot", subtitle = "N = 5000, r = 0.80", y = "Test Information",
       caption = "When Test Information = 10, Reliability = 0.90, When Test Information = 20, Reliability = 0.95") +
  xlab(expression(theta)) +
  geom_hline(yintercept = rxx9)+ 
  geom_hline(yintercept = rxx95)

testdata2 <- n500r6$data

ggplot(testdata2, aes(x = theta, y = testinfo, color = `Likert Category`)) + 
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = c("#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f"))+
  labs(title = "Test Information Plot", subtitle = "N = 500, r = 0.60", y = "Test Information",
       caption = "When Test Information = 10, Reliability = 0.90, When Test Information = 20, Reliability = 0.95") +
  xlab(expression(theta)) +
  geom_hline(yintercept = rxx9)+ 
  geom_hline(yintercept = rxx95)

testdata3 <- n5000r7$data
ggplot(testdata3, aes(x = theta, y = testinfo, color = `Likert Category`)) + 
  geom_line() +
  scale_color_manual(values = c("firebrick", "sandybrown", "darkorchid", "red4", "slategrey"))+
  labs(title = "Test Information Plot", subtitle = "N = 5000, r = 0.70", y = "Test Information",
       caption = "When Test Information = 10, Reliability = 0.90, When Test Information = 20, Reliability = 0.95") +
  xlab(expression(theta)) +
  geom_hline(yintercept = rxx9)+ 
  geom_hline(yintercept = rxx95)

























testdata_three <- filter(testdata, `Likert Category` == "Three")
testdata_five <- filter(testdata, `Likert Category` == "Five")
testdata_seven <- filter(testdata, `Likert Category` == "Seven")
testdata_nine <- filter(testdata, `Likert Category` == "Nine")
testdata_eleven <- filter(testdata, `Likert Category` == "Eleven")

TestInfoPlot = 
ggplot()+
  geom_line(data = testdata_three, aes(x = Theta, y = testinfo, color = "firebrick"))+
  geom_line(data = testdata_five, aes(x = Theta, y = testinfo, color = "sandybrown"))+
  geom_line(data = testdata_seven, aes(x = Theta, y = testinfo, color = "darkorchid"))+
  geom_line(data = testdata_nine, aes(x = Theta, y = testinfo, color = "red4"))+
  geom_line(data = testdata_eleven, aes(x = Theta, y = testinfo, color = "slategrey")) +
  labs(title = "Test Information Plot", subtitle = "N = 5000, r = 0.80", y = "Test Information") +
  xlab(expression(theta))+
  scale_color_discrete(name = "Number of Likert Categories", labels = c("Three", "Five", "Seven", "Nine", "Eleven"))
print(TestInfoPlot)






## Testing to extract item info
r = .8
n = 1000

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



## Extracting item informations
three_cat_graded <- mirt(orig.likert.3cat, 1, itemtype = 'graded')
five_cat_graded <- mirt(orig.likert.5cat, 1, itemtype = 'graded')
seven_cat_graded <- mirt(orig.likert.7cat, 1, itemtype = 'graded')
nine_cat_graded <- mirt(orig.likert.9cat, 1, itemtype = 'graded')
eleven_cat_graded <- mirt(orig.likert.11cat, 1, itemtype = 'graded')

Theta <- matrix(seq(-6, 6, .001))
test_info_data <- data.frame(Three = testinfo(three_cat_graded, Theta),
                             Five  = testinfo(five_cat_graded, Theta),
                             Seven = testinfo(seven_cat_graded, Theta),
                             Nine  = testinfo(nine_cat_graded, Theta),
                             Eleven = testinfo(eleven_cat_graded, Theta),
                             theta = Theta
)
test_info_data <- test_info_data %>% mutate(`Likert Category` = row_number())

#putting data into long form for plot

test_info_long <- test_info_data %>% gather(key = `Likert Category`, value = testinfo, 1:5)

test_info_long$`Likert Category` <- factor(test_info_long$`Likert Category`, ordered = TRUE, levels = c("Three", "Five", "Seven", "Nine", "Eleven"))













plot(eleven_cat_graded, type = "rxx")

extr.11.1 <- extract.item(eleven_cat_graded, 1)
info.11.1 <- iteminfo(extr.11.1, Theta)

extr.11.2 <- extract.item(eleven_cat_graded, 2)
info.11.2 <- iteminfo(extr.11.2, Theta)

extr.11.3 <- extract.item(eleven_cat_graded, 3)
info.11.3 <- iteminfo(extr.11.3, Theta)

extr.11.4 <- extract.item(eleven_cat_graded, 4)
info.11.4 <- iteminfo(extr.11.4, Theta)

extr.11.5 <- extract.item(eleven_cat_graded, 5)
info.11.5 <- iteminfo(extr.11.5, Theta)

extr.11.6 <- extract.item(eleven_cat_graded, 6)
info.11.6 <- iteminfo(extr.11.6, Theta)

extr.11.7 <- extract.item(eleven_cat_graded, 7)
info.11.7 <- iteminfo(extr.11.7, Theta)

extr.11.8 <- extract.item(eleven_cat_graded, 8)
info.11.8 <- iteminfo(extr.11.8, Theta)

extr.11.9 <- extract.item(eleven_cat_graded, 9)
info.11.9 <- iteminfo(extr.11.9, Theta)

extr.11.10 <- extract.item(eleven_cat_graded, 10)
info.11.10 <- iteminfo(extr.11.10, Theta)

test.info.11.data <- cbind(info.11.1, info.11.2, info.11.3, info.11.4, info.11.5, info.11.6, info.11.7, info.11.8,
                    info.11.9, info.11.10, Theta)

test.info.11.data <- as.data.frame(test.info.11.data)

#summing individual item infos

test.info.11.data <- test.info.11.data %>% mutate(test.info.11.sum = info.11.1+ info.11.2+ info.11.3+ info.11.4+ info.11.5+ info.11.6+ info.11.7+ info.11.8+
                                                  info.11.9+ info.11.10)

#extrating test info from mirt

test.info.11.data$mirt <- testinfo(eleven_cat_graded, Theta)

#plotting summed item infos
plot(y = test.info.11.data$test.info.11.sum, x = test.info.11.data$V11)

#built in-mirt plot
plot(eleven_cat_graded, type = 'info')

#reliability is info - 1 / info

#plotting extracted test info 
plot(y = test.info.11.data$mirt, x = test.info.11.data$V11)


plot(Theta, info.11.10, type = 'l', main = 'Item information')

ICCplot = ggplot()+
  geom_line(data = test.info.11.data, aes(x = Theta, y = info.11.1), color = "firebrick")+
  geom_line(data = test.info.11.data, aes(x = Theta, y = info.11.2), color = "sandybrown")+
  geom_line(data = test.info.11.data, aes(x = Theta, y = info.11.3), color = "red")+
  geom_line(data = test.info.11.data, aes(x = Theta, y = info.11.4), color = "violet")+
  geom_line(data = test.info.11.data, aes(x = Theta, y = info.11.5), color = "orange")+
  geom_line(data = test.info.11.data, aes(x = Theta, y = info.11.6), color = "brown")+
  geom_line(data = test.info.11.data, aes(x = Theta, y = info.11.7), color = "gray")+
  geom_line(data = test.info.11.data, aes(x = Theta, y = info.11.8), color = "darkorchid")+
  geom_line(data = test.info.11.data, aes(x = Theta, y = info.11.9), color = "orchid")+
  geom_line(data = test.info.11.data, aes(x = Theta, y = info.11.10), color = "green")+
#  geom_line(data = test.info.11.data, aes(x = Theta, y = test.info.11.sum), color = "black")+
  xlab('Theta') + ylab('Item Information')

print(ICCplot)

#Plot test information function for 


itemplot(eleven_cat_graded, 1, type = "info")

