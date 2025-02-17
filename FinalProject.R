# Fish Market Data
#Final Project: Bayesian Statistics
# Jayden Cruz and Christopher Garcia

library(rethinking)
data <- read.csv("Fish.csv")
d <- data
d <- na.omit(d) 

#head(d)
#tail(d)

d$W <- standardize(d$Weight)
d$L <- standardize(d$Length2)
d$H <- standardize(d$Height)

dcc <- d[ complete.cases(d$W,d$L,d$H) , ]

head(dcc)

library(ggdag)
library(ggplot2)
theme_set(theme_dag())

## DAG MODELS

# 1. A model for Y(Weight) with X1(Length 2).
dagify(Weight ~ Length2) %>%
  ggdag()

# 2. A model for Y(Weight) with X2(Species).
dagify(Weight ~ Height) %>%
  ggdag()

# 3. A model for Y with X1 and X2.
dagify(
  Weight ~ Height,
  Weight ~ Length2,
  Length2 ~ Height,
  Height ~ Length2
) %>%
  ggdag()

## Quadratic Models

# Selecting Priors for wieght
average_weight <- mean(dcc$W, na.rm = TRUE)

print(paste("The average weight of the fish is:", average_weight, "grams"))


weight_sd <- sd(dcc$W, na.rm = TRUE)
print(paste("STD:", weight_sd))

## Model 1: Length 2 -> Weight
m1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),        
    mu <- a + bL * L,             
    a ~ dnorm(0, 1),
    bL ~ dnorm(0.5, 2),
    sigma ~ dexp(1)              
  ), data = d                     
)
precis(m1)

#Model 1: Simulate the priors
prior <- extract.prior(m1)

xseq <- seq(min(dcc$L), max(dcc$L), length.out = 100)  

mu <- link(m1, post = prior, data = list(L = xseq))

plot(NULL, xlim = range(xseq), ylim = range(mu), 
     xlab = "Length", ylab = "Predicted Weight", main = "Simulated Priors for Model 1")

for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )

# Model 2: Height -> Weight
m2 <- quap(
  alist(
    W ~ dnorm(mu, sigma),       
    mu <- a + bH * H,            
    a ~ dnorm(0, 1),            
    bH ~ dnorm(0.5, 2),
    sigma ~ dexp(1)                
  ), data = d                     
)
precis(m2)  
#Model 2: Simulate the priors
prior <- extract.prior(m2)

xseq <- seq(min(dcc$H), max(dcc$H), length.out = 100)  

mu <- link(m2, post = prior, data = list(H = xseq))  

plot(NULL, xlim = range(xseq), ylim = range(mu), 
     xlab = "Height", ylab = "Predicted Weight", main = "Simulated Priors for Model 2")

for (i in 1:50) {
  lines(xseq, mu[i,], col = col.alpha("black", 0.3))  
}

# Model 2: Counterfactual Plot
xseq <- seq(from = min(dcc$H) - 2, to = max(dcc$H) + 2, length.out = 30)

mu <- link(m2, data = data.frame(H = xseq))

mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

plot(NULL, xlim = range(dcc$H), ylim = range(dcc$W), 
     xlab = "Height (H)", ylab = "Weight (W)", main = "Height to Predict Weight")

lines(xseq, mu_mean, lwd = 2)

shade(mu_PI, xseq)

text(x = mean(range(dcc$H)), y = max(dcc$W), 
     labels = "Model: Height -> Weight", pos = 3, cex = 1.2)

## Model 3: Length 2 + Height -> Weight
m3 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bL * L + bH * H,
    a ~ dnorm(0, 1),            
    bL ~ dnorm(0.5, 2),
    bH ~ dnorm(0.5, 2),
    sigma ~ dexp(1)
  ), data = dcc
)
precis(m3)
plot( coeftab( m1 , m2 , m3 ) , pars=c("bL","bH") )
#Model 3: Simulate the priors
prior <- extract.prior(m3)

xseq_L <- seq(min(dcc$L), max(dcc$L), length.out = 100)

xseq_H <- seq(min(dcc$H), max(dcc$H), length.out = 100)

mu <- link(m3, post = prior, data = list(L = xseq_L, H = xseq_H))

plot(NULL, xlim = range(xseq_L), ylim = range(mu), 
     xlab = "Length", ylab = "Predicted Weight", main = "Simulated Priors for Model 3: Length + Height")

for (i in 1:50) {
  lines(xseq_L, mu[i,], col = col.alpha("black", 0.3))
}

## Counterfactual Plots for all Models
#counterfactual plots
m3.a <- quap(
  alist(
    ## L -> W <- H
    W ~ dnorm( mu , sigma ) ,
    mu <- a + bH*H + bL*L ,
    a ~ dnorm( 0 , 1 ) ,
    bH ~ dnorm( 0.5 , 2 ) ,
    bL ~ dnorm( 0.5 , 2 ) ,
    sigma ~ dexp( 1 ),
    ## L -> H
    H ~ dnorm( mu_H , sigma_H ),
    mu_H <- aH + bLH*L,
    aH ~ dnorm( 0 , 0.2 ),
    bLH ~ dnorm( 0 , 0.5 ),
    sigma_H ~ dexp( 1 )
  ) , data = dcc )

L_seq <- seq( from=-2 , to=2 , length.out=30 )

#L ON W
sim_dat <- data.frame( L=L_seq )

s <- sim( m3.a , data=sim_dat , vars=c("H","W") )


plot(sim_dat$L, colMeans(s$W), ylim = c(-2, 2), type = "l",
     main = "Total counterfactual effect of L on W",
     xlab = "manipulated L",
     ylab = "counterfactual W"
)

shade( apply(s$W,2,PI) , sim_dat$L )

#H ON W

sim_dat <- data.frame( H=seq(from=-2,to=2,length.out=30) , L=0 )

s <- sim( m3.a , data=sim_dat , vars=c("W") )

plot( sim_dat$H , colMeans(s) , ylim=c(-2,2) , type = "l", main = "Total Counterfactual Effect of H on W", xlab ="manipulated H" , ylab ="counterfactual W" )

shade( apply(s,2,PI) , sim_dat$H )

#H ON L
sim_dat <- data.frame(H = seq(from = -2, to = 2, length.out = 30))
s <- sim(m3.a, data = sim_dat, vars = "L")  # Simulate L based on H

par(mar = c(5.1, 4.1, 4.1, 2.1))
plot(sim_dat$H, colMeans(s), ylim = c(-2, 2), type = "l",
     main = "Counterfactual Effect of L on H",
     xlab = "Manipulated H",
     ylab = "Counterfactual L")

shade(apply(s, 2, PI), sim_dat$H)

## Markov Chain Monte Carlo

## MCMC - Model 1: Length2 -> Weight
m1_mcmc <- ulam(
  alist(
    W ~ dnorm(mu, sigma),        
    mu <- a + bL * L,             
    a ~ dnorm(0, 1),              
    bL ~ dnorm(0.5, 2),           
    sigma ~ dexp(1)               
  ), data = dcc,              
  chains = 4,                    
  cores = 4,                     
  log_lik = TRUE                 
)

precis(m1_mcmc)
traceplot(m1_mcmc)
WAIC(m1_mcmc)

# MCMC - Model 2: Height -> Weight
m2_mcmc <- ulam(
  alist(
    W ~ dnorm(mu, sigma),        
    mu <- a + bH * H,             
    a ~ dnorm(0, 1),              
    bH ~ dnorm(0.5, 2),           
    sigma ~ dexp(1)               
  ), data = dcc,              
  chains = 4,                    
  cores = 4,                     
  log_lik = TRUE                 
)

precis(m2_mcmc)
traceplot(m2_mcmc)
WAIC(m2_mcmc)

# MCMC - Model 3: Length 2 + Height -> Weight
m3_mcmc <- ulam(
  alist(
    W~ dnorm( mu, sigma),
    mu <- a + bL * L + bH * H,
    a ~ dnorm(0, 1),              
    bL ~ dnorm(0.5, 2),   
    bH ~ dnorm(0.5, 2),
    sigma ~ dexp( 1)
  ), data = dcc,
  chains=4,
  cores=4,
  log_lik = TRUE
)

precis(m3_mcmc)
traceplot(m3_mcmc)
WAIC(m3_mcmc)

## Comparing the Models using WAIC
compare( m1_mcmc , m2_mcmc , m3_mcmc )





