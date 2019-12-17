logisticFun <- function(x, D, a, b){
  z <- D*a*(x-b)
  1 / (1 + exp(-1*z))
}

D <- 1.702

a <- c(1, 1, 1, 1, 1)

b1 <- c(-1, -.5, 0, .5, 1)

library(ggplot2)

icc <- ggplot(data.frame(x = c(-6, 6)), aes(x = x)) +
  stat_function(fun = logisticFun, args = list(D = D, a = a[[1]], b=b1[[1]]), aes(colour = "1=heart rate")) +
  
  scale_colour_manual("Apgar item", values = c("#e41a1c")) +
  theme(legend.position = "bottom") +
  scale_x_continuous(name = "theta") +
  scale_y_continuous(name = "P(theta)")

icc


gradedFun <- function(x, D, a, bu, bu1){

z1 <- -D*a*(x - bu1)
z <- -D*a*(x - bu)

(exp(z1) - exp(z)) / (1 + exp(z))*(1 + exp(z1)) 

}

D <- 1.702

a <- c(1, 1, 1, 1, 1)
bu <- c(-1, -.5, 0, .5, 1)
bu1 <- c(-.5, 0, .5, 1)

icc <- ggplot(data.frame(x = c(-6, 6)), aes(x = x)) +
  stat_function(fun = gradedFun, args = list(D = D, a = a[[1]], bu=bu[[1]], bu1 = bu1[[1]]), aes(colour = "test")) +
  
  scale_colour_manual("test item", values = c("#e41a1c")) +
  theme(legend.position = "bottom") +
  scale_x_continuous(name = "theta") +
  scale_y_continuous(name = "P(theta)")

icc
