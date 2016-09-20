


# Sammerize weightchange by treatment
table(wt$treatment)  # pretty evenly distributed.
by(wt$wtch, wt$treatment, summary)


# If we only compare race 1 and 3, race 3 seems to have a significantly larger weight loss 
# compared to group1.

# the ages is a continuous variable, we might want to group it into different bins somehow.




qplot(x=wtch, data = wt) +  
  facet_wrap(~treatment) 



# Let's try to plot wtch as a function of age for different race?

# Ok, time to do some real statistics 
# Let's try some linear regression with covariance: age, race as a catagorical data

# how to deal with the idea of intervention? How to test if it makes a difference or not?

# We need to bin the age?

# Why is race seem to be more significant than treatment?


# Do several test to see if the treatment is significant
# Anova, R-squared, residuals
# Let's do a test see if we should include the biomarker or not

# In Bayesian way
# Construct multiple models and do DIC, BIC, or other types of tests?
fitagerace <- lm(wtch ~ age + as.factor(race), data =wt)
summary(fitagerace)

table(wt$treatment)
# on the other hand, our samples are pretty equally distributed across treatment 1-3.

# the treatment still doesn't seem to be very significant.
fitagetreat <- lm(wtch ~ age + as.factor(treatment), data =wt)
summary(fitagetreat)


# Let's try some Baysian way of analysising things 
# Maybe the key point is about choosing prior?

# R squared looks really bad, maybe we should add some interaction term or some function like 
# log, sqrt, sq or something?

fit1<- lm(wtch ~ log(age) + as.factor(race), data =wt)
summary(fit1)
# biomarker strongly correlates with age. We should probably only include one, so it doesn't 
# inflate our variance too much.


# What's the baysian way to test those two models (w/wo intervention?)
# Ok, it's definetly related, but still not quite the same thing 
# R-squared = 0.47
# However, including both variables at the same time will increase the variance significantly 
# since the two variables are correlated.
agebio <- lm(age ~ biomarker, data=wt)
summary(agebio)

# what about we combine intervention 2,3 and see if that's significant?
