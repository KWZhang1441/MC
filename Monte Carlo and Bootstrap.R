#Monte Carlo hypothesis testing
#First let's start with a sampling distribution that we're going to be simulating from
#We're going to simulate 60 dice throws with a bias towards rolling a 6
x <- table(sample(1:6, 60, replace = TRUE, prob = c(0.15, 0.15, 0.15, 0.15, 0.15, 0.25)))

#We're going to propose a test statistic for the hypothesis test
#This should be 'extreme' when H0 is false
#Our H0 is that the dice is fair
#Our H1 is that we are biased to higher numbers
obs.test.stat <- sum((x)^2)

#By using simulations we don't have to calculate a distribution for H0 and can
#instead compare our observed test statistic to our simulated datasets
test.stat <- rep(0, 10000)
for(i in 1:10000) {
  sim <- table(sample(1:6, 60, replace = TRUE))
  test.stat[i] <- sum((sim)^2)
}
mean(test.stat > obs.test.stat)
#This is the estimated p-value
#However it seems to vary a lot: in the next scenario we can try and visualise how spread out our p-value estimates are

#Let's use a scenario where we can easily calculate the correct answer and compare it to our estimate
#We have a sample of 5 numbers taken from a pool of numbers 1 to N. We want to do a hypothesis test on
#the value of N, so we use the test statistic max(x) as it directly relates to N.

#Ho: N = 107
#H1: N < 107
#Alpha = 0.05

#Observed Sample
x <- c(61, 19, 56, 24, 16)

#Calculate exact p value
#P(max(x)<=61 | N = 107)
p <- (61*60*59*58*57)/(107*106*105*104*103)
print(p)


#Simulate p value once
M <- 1000
test.stat <- rep(0, M)
for(i in 1:M) {
  y <- sample(1:107, 5, replace = FALSE)
  test.stat[i] <- max(y)
}
sum(test.stat <= max(x))/M
#We get a p value that seems to hover above and below 0.05

#let's compute 1000 p values and look at how spread out they are
pvals <- rep(0,1000)
for(j in 1:1000) {
  M <- 1000
  test.stat <- rep(0, M)
  for(i in 1:M) {
    y <- sample(1:107, 5, replace = FALSE)
    test.stat[i] <- max(y)
  }
  pvals[j] <- sum(test.stat <= max(x))/M
}

#This is a histogram of the p-values we simulated with the red line showing alpha = 0.05
#and the purple line showing the exact p-value
library("MASS")
truehist(pvals, xlim = c(0.03, 0.08))
abline(v = 0.05, col = "red", lwd = 4)
abline(v = 0.05596113, col = "purple", lwd = 4)

#We see that around 18% of the p-values obtained would give us the wrong conclusion
sum(pvals<0.05)/10

#Repeating with M = 2000
pvals <- rep(0,1000)
for(j in 1:1000) {
  M <- 2000
  test.stat <- rep(0, M)
  for(i in 1:M) {
    y <- sample(1:107, 5, replace = FALSE)
    test.stat[i] <- max(y)
  }
  pvals[j] <- sum(test.stat <= max(x))/M
}

library("MASS")
truehist(pvals, xlim = c(0.03, 0.08))
abline(v = 0.05, col = "red", lwd = 4)
abline(v = 0.05596113, col = "purple", lwd = 4)

sum(pvals<0.05)/10

#We see a lower proportion of wrong p-values and a tighter distribution
#It looks roughly like a normal distribution, due to the fact that pvals is the mean of a random variable which is 1 when test.stat <= max(x) and 0 otherwise
#We have 2000 samples of this iid data so CLT applies




#Bootstrap
#We start with wine data from UCI's machine learning repository
#We want to explore whether higher sulphate content means better quality wines
plot(wq.red$sulphates, wq.red$quality)
plot(wq.white$sulphates, wq.white$quality)


#We split the data into two parts
med.red <- median(wq.red$sulphates)
low.s <- wq.red[wq.red$sulphates <= med.red,]
high.s <- wq.red[wq.red$sulphates >= med.red,]

#We plot the ecdf of low and high quality wines
#The process of Bootstrap (resampling from an observed sample) is equivalent to sampling from the ecdf
#as N becomes large, the ecdf converges in distribution to the actual cdf
#which is why we can use Bootstrap to find estimates without knowing the actual distribution of the data
low.ecdf <- ecdf(low.s$quality)
plot(low.ecdf)

high.ecdf <- ecdf(high.s$quality)
lines(high.ecdf, col = 'red')

#We have about 800 data points so by CLT, the sample mean is distributed normally and we can find 95% confidence intervals for the mean
t.test(low.s$quality)
t.test(high.s$quality)
#We see for low quality wines we get (5.32, 5.42)
#for high quality we get (5.87, 5.98)
#The two intervals do no overlap, so there is evidence to suggest high sulphate content wines do have a higher mean quality than low sulphate wines.

#Let's say we have the hypothesis that although high sulphate content wines are on average higher quality, but are of less consistent quality
#Since we don't know the distribution of the data, we can use bootstrap to estimate the SD of the SD
#and use that to determine whether the SD of the quality high sulphate wines is significantly higher than lower sulphate ones

high.s.sd <- rep(0, 1000)
low.s.sd <- rep(0, 1000)
for(i in 1:1000) {
  high.sample <- sample(high.s$quality, replace = TRUE)
  high.s.sd[i] <- sd(high.sample)
  
  low.sample <- sample(low.s$quality, replace = TRUE)
  low.s.sd[i] <- sd(low.s.star)
}

#We estimate the SD of the quality of high sulphate wines by just taking the SD of our sample
sd(high.s$quality)
#Using the simulation from above we can estimate the standard error of the SD
sd(high.s.sd)

#Repeating for low quality
sd(low.s$quality)
sd(low.s.sd)

#Since the gap between the estimators of the standard deviations is much larger than the standard error in either case
#We can have high confidence that the quality of high sulphate wines is less consistent

#If we wanted to do further tests, but without considering white and red wine seperately we can combine the two data sets into one data fram
#We want to keep the label of the colours though so we add a new 'colour' variable to each data frame

wq.red$colour <- factor(rep("red", nrow(wq.red)))
wq.white$colour <- factor(rep("white", nrow(wq.white)))

wq <- rbind(wq.red, wq.white)

summary(wq)
str(wq)

