# Bayesian analysis of butterfly habitat choice data
# data = 10 butterflies at patch edges, 5 choose flowers, 5 choose grass
# we want to understand the probability that a butterfly chooses flowers over grass

# 3 hypothesized values of z
probs <- c(0.1, 0.5, 0.9)

p.data_mod<-(probs^5)*((1-probs)^5) # probability of data given these 3 models - this does not change
p.mod <- 1/length(probs) # probability of the model [uninformative prior] - this does change
numerator<-p.data_mod*p.mod # numerators - P(data|model)*P(model)
p.data <- sum((probs^5)*((1-probs)^5)*p.mod) # P(data) = sum of the numerators across models
post.prob<-numerator/p.data # Bayes Theorem - probability of each model, given the data
post.prob
sum(post.prob) # checking my calculations - the sum of the probabilities of all models, given data, should be 1 [Remember the LAW OF TOTAL PROBABILITY]

# 9 hypothesized values of z
zzz = seq(0.1, 0.9, 0.05)  # 9 hypothesized values of z, replace w/ seq(0.05, 0.95, 0.05) for 19 values
p.dgm <- zzz^5*(1-zzz)^5    # P(data|model): probability of data, given these 9 models
p.mod <- 1/length(zzz)      # P(model): probability of the model [uninformative prior]
p.dgm*p.mod                  # numerators of Bayes theorum - P(data|model)*P(model)
p.dat <- sum(p.dgm*p.mod)  # P(data): probability of data, sum of the numerators across all models -> denominator
p.dgm*p.mod/p.dat            # Bayes Theorem - probability of each model, given the data
sum(p.dgm*p.mod/p.dat)       # sum of probabilities of all models, given data, should be 1 

# 9 hypothesized values of z again but now with an informative prior probability
tmp = c(6.340418e-13, 7.230104e-08, 2.238592e-05, 
        7.471029e-04, 7.251420e-03, 2.845348e-02, 
        4.500579e-02, 1.835614e-02, 2.334879e-04)  # points from the curve
priors <- tmp/sum(tmp)     # this line makes the nine priors add up to 1
probs <- seq(0.1, 0.9, 0.1) # same 9 hypothesized values of z
probs^5*(1-probs)^5          # P(data|model) - this does not change!
p.modI = priors              # P(model) with an INFORMATIVE prior
p.modU = 1/length(probs)     # P(model) with an UNinformative prior
p.dat_modI = (probs^5*(1-probs)^5*p.modI)/sum(probs^5*(1-probs)^5*p.modI) 
p.dat_modU = (probs^5*(1-probs)^5*p.modU)/sum(probs^5*(1-probs)^5*p.modU) 

# figure comparing conclusions from uninformative and informative priors
#pdf("Priorprob.pdf") #use this line and the dev.off() function below if you wish to save to file
plot(probs, p.dat_modI, type = "o", lwd = 3, xlab = "", ylab = "")
points(probs, p.dat_modU, type = "o", lwd = 3, col = "red")
points(probs, priors, type = "l", lty = "dotted", col = "gray50")
mtext(side = 1, line = 2, "Proportion choosing flowers vs. grass (z)")
mtext(side = 2, line = 2, "P(model|data) ")
legend("topleft", title = "Prior probability",
       legend = c("informative", "uninformative", "prior"), 
       lwd = c(3,3,1), cex = 0.8,
       col = c("black", "red", "gray50"), 
       lty = c("solid", "solid", "dotted"))
#dev.off()

