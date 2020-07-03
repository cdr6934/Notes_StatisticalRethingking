install.packages(c("devtools","mvtnorm","loo","coda"),dependencies=TRUE)
library(devtools)
install_github("rmcelreath/rethinking",ref="Experimental")


#https://www.youtube.com/watch?v=h5aPo5wXN8E

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18,]


#Define the average weight, x-bar
xbar <- mean(d2$weight)


m4_3 <- quap(
  
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + b*(weight-xbar),
    a ~ dnorm(178,20),
    b ~ dlnorm(0,1),
    sigma ~ dunif(0,50)
  ), data = d2
)


precis(m4_3)

plot(height ~ weight, data = d2, col = rangi2)
post <- extract.samples(m4_3)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve(a_map + b_map*(x - xbar), add = TRUE)

post[1:5,]

mu_at_50 <- post$a + post$b * (50 - xbar)
plot(mu_at_50)


# 
weight.seq <- seq(from=25, to = 70, by =1)
mu <- link(m4_3, data = data.frame(weight=weight.seq))
str(mu)

# Fitting a b-spline for climate 
data("cherry_blossoms")
d <- cherry_blossoms


precis(d)


m4_7 <- quap(
  alist(
    T ~ dnorm(mu, sigma),
    mu <- a + B %*% w, 
    a ~ dnorm(6,10), 
    w ~ dnorm(0,1), 
    sigma ~ dexp(1)
  )
  ,
   data = list(T = d)
  )
)




