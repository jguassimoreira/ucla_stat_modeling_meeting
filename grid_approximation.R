#Grid approximation/search example from chapter 2 of Statistical Rethinking
#R-code 2.3

##Steps
#1. define the grid -- pick how many points to use in estimating the posterior (or parameter of interest). Then make list of param values on the grid
#2. Compute value of prior at each parameter value on the grid. 
#3. Compute the value of likelihood at each parameter value -- akin to 'couting' the number of paths the data could have taken to have gotten to that point
#4. Compute unstandardized posterior at each param value (prior * likelihood)
#5. Standardize posterior -- divide each value by sum of all values

#define grid
p_grid = seq(from=0, to=1, length.out=20) #start, finish, number of bins/finite points

#define prior
prior = rep(1, 20) #in this case, uniform prior of 1 for each p' (potential parameter value)
#satisfies general rule of uniform priors for each point: prior = 1/(b-a)

#compute likelihood at each value in the grid
likelihood = dbinom(6, size=9, prob=p_grid) #number of 'successes' (i.e., 1s), number of events, probability of success
#when given a vector for probability, multiple likelihoods are given

#compute product of likelihood and prior
unstd.posterior = likelihood * prior

#standardize the posterior, so it sums to 1
posterior = unstd.posterior / sum(unstd.posterior)

#plot the posterior
plot(p_grid, posterior, type="b",
     xlab="probability of water", ylab="posterior probability")
mtext("20 points")


##Repeat again with fewer bins (fewer finite points for our prior), let's say 5. 
p_grid = seq(from=0, to=1, length.out=5) 


prior = rep(1, 5) 


likelihood = dbinom(6, size=9, prob=p_grid)


unstd.posterior = likelihood * prior


posterior = unstd.posterior / sum(unstd.posterior)


plot(p_grid, posterior, type="b",
     xlab="probability of water", ylab="posterior probability")
mtext("5 points") 


##The above didn't look too informative, let's try 1000 points
p_grid = seq(from=0, to=1, length.out=1000) 


prior = rep(1, 1000) 


likelihood = dbinom(6, size=9, prob=p_grid)


unstd.posterior = likelihood * prior


posterior = unstd.posterior / sum(unstd.posterior)


plot(p_grid, posterior, type="b",
     xlab="probability of water", ylab="posterior probability")
mtext("1000 points") #lookin' real good!


##Let's update our prior to reflect the fact that we know the earth is defintely not all land (we've seen at least one body of water)
prior = ifelse(p_grid < .05, 0, 1)

unstd.posterior = likelihood * prior #make new unstd posterior
posterior = unstd.posterior / sum(unstd.posterior) #standardize

plot(p_grid, posterior, type="b",
     xlab="probability of water", ylab="posterior probability, no all-land prior")
mtext("20 points") #plot

#Let's update our prior to reflect the fact that we think it's equally unlikely the earth is covered in water or land *and* that it's a 50-50 split
prior = exp(-5*abs(p_grid - .5)) 

unstd.posterior = likelihood * prior #make new unstd posterior
posterior = unstd.posterior / sum(unstd.posterior) #standardize

plot(p_grid, posterior, type="b",
     xlab="probability of water", ylab="posterior probability, no all-land prior")
mtext("20 points") #plot

