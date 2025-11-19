### Calculating likelihoods and plotting likelihood profiles for several different values of survival ###

# Our data for five plants: 
# 1 flowering, 3 vegetative, 1 dead
mydata = c("F", "V", "V",  "V", "D")
s.vals = seq(0.1, 0.9, 0.1) #potential survival values
f.vals = seq(0.1, 0.9, 0.1) #potential reproduction values

# Calculate the log-likelihood for a set of parameters by hand:
s = 0.8
f = 0.25

# One plant flowered and survived, three plants survived but did not flower, one plant died
(logLike = log(s*f) + 3*log(s*(1-f)) + log(1-s))

# Another way to do the same thing using a loop:

# First, create an empty vector in which to place each of our likelihoods
LLs = rep(0, length(mydata)) 

for(i in 1:length(mydata)){
  if(mydata[i] == "F") LLs[i] = log(s*f)
  if(mydata[i] == "V") LLs[i] = log(s*(1-f))
  if(mydata[i] == "D") LLs[i] = log(1-s)
}
(logLike = sum(LLs))


# You can use two for loops (a loop within a loop) to calculate the likelihood of different survival values, for a fixed flowering probability:
  
s.LL = rep(0, length(s.vals)) #s.vals are our candidate values for survival
for(j in 1:length(s.vals)){
  s = s.vals[j]
  f = 0.25
  LLs = rep(0, length(mydata))
  for(i in 1:length(mydata)){
    if(mydata[i] == "F") LLs[i] = log(s*f)
    if(mydata[i] == "V") LLs[i] = log(s*(1-f))
    if(mydata[i] == "D") LLs[i] = log(1-s)
  } # close loop over i,data
  s.LL[j] = sum(LLs)
} # close loop over j, s.vals

# The loop above creates an object that stores the log-likelihood of the data for each survival value from s.vals, given a flowering probability of 0.25:
s.LL

# Make a plot of the likelihood of each survival value:
plot(s.vals, s.LL, type = "l")

# Add axis labels and alternative types of lines:
plot(s.vals, s.LL, type = "b", xlab = "survival probability", ylab = "log likelihood | f = 0.25")
plot(s.vals, s.LL, type = "o", xlab = "survival probability", ylab = "log likelihood | f = 0.25")


### How do we search across multiple values of survival as well?? ###

# We can use a two-dimensional array, and three loops to create a likelihood surface of the data as a function of both survival and flowering:
fs.LL = array(0, dim = c(length(s.vals), length(f.vals))) 

# The array dimensions are rows and columns, large enough for a combination of 9 survival and 9 flowering probabilities
fs.LL

for(j in 1:length(s.vals)){
  s = s.vals[j] #loop through each survival value
  for(h in 1:length(f.vals)){ #for each s, estimate log-likelihood of f
    f = f.vals[h]
    LLs = rep(0, length(mydata))
    for(i in 1:length(mydata)){
      if(mydata[i] == "F") LLs[i] = log(s*f)
      if(mydata[i] == "V") LLs[i] = log(s*(1-f))
      if(mydata[i] == "D") LLs[i] = log(1-s)
    } # close loop over i,data
    fs.LL[j,h] = sum(LLs) #insert values into row j, column h
  } # close loop over h, f.vals
} # close loop over j, s.vals

#Loops get cumbersome fast!!

#Look at our array of log-likelihoods:
fs.LL

#Maximum likelihood parameters are the set of parameters that are most likely, given a certain data set. For some data sets, the value of one parameter affects the likelihood of others, so you have to maximize them all at once.

#The likelihood profile for each individual parameter is a one-dimensional plot of the likelihood of that parameter value, with all other parameters set at the maximum likelihood values for that value.
  
#Likelihood profile = graph of log-likelihood as a function of parameter value for flowering plant (MR) survival:
    
#We can get the profile by searching our array of likelihood values.

#To get the profile for survival (which changes for each row), output the maximum likelihood value in each row:
apply(fs.LL, MARGIN = 1, FUN = max)

#To plot the likelihood profile, save these results as an object:
  s.profile = apply(fs.LL, MARGIN = 1, FUN = max)
#. and then plot it:
  plot(s.vals, s.profile, type = "b", xlab = "survival probability", ylab = "log-likelihood", main = "likelihood profile for survival")
#same plot as we had before, only from the matrix of all probabilities

#Now plot the likelihood profile for flowering:
f.profile = apply(fs.LL, MARGIN = 2, FUN = max)
plot(f.vals, f.profile, type = "b", xlab = "flowering probability", ylab = "log-likelihood", main = "likelihood profile for flowering")


#### BONUS: Maximum likelihood estimation across multiple parameters ####
#To find the maximum likelihood parameters for a given data set and model(s), the general idea is to search across all parameter combinations and find the one with the highest likelihood

#This is usually done using calculus or a computer algorithm, but it can be as simple as making a likelihood surface, i.e., a matrix of all possible values, and choosing the combination with the highest likelihood

#Make a contour plot of the array of log-likelihoods:
contour(fs.LL, xlab = "survival probability", ylab = "flowering probability", main = "log-likelihood surface")

#ANOTHER EXAMPLE OF CALCULATING ML PARAMS, DIFFERENT TYPE OF PLOT
#First, choose parameters to search over, and create array to store results
s = seq(0,1,0.01)
f = seq(0,1,0.01)
likeli = array(NA, dim = c(length(f), length(s)))
length(s)
length(f)
dim(likeli)

# calculate likelihood for all pairwise combinations
for(i in 1:length(s)){
  for(j in 1:length(f)){
    likeli[j,i] = ((s[i]*f[j])^1)*((s[i]*(1-f[j]))^3)*((1-s[i])^1)
  }}

# make a contour plot of the results
contour(likeli, ylab = "survival, s", xlab = "flowering, f", main = "likelihood surface")

# contorted code to pull out maximum likelihood parameters - there may be a better way
Smax = s[trunc(which(likeli == max(likeli))/length(s))+1]
fmax = f[-(length(s)*trunc(which(likeli == max(likeli))/length(s)) - which(likeli == max(likeli)))]
points(fmax, Smax, pch = 19)



  
