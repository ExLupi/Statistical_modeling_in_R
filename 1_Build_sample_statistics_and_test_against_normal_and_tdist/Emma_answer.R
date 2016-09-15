install.packages('LearnBayes')
library(LearnBayes)
data(studentdata)

str(studentdata)

hist(studentdata$Dvds)
ggplot(aes(x=Dvds), data=studentdata) +
  geom_histogram()+
  facet_wrap(~Gender, ncol =3) 

summary(studentdata$Dvds)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.00   10.00   20.00   30.93   30.00 1000.00      16 

quantile(studentdata$Dvds, probs =c(0.025,0.5,0.975), na.rm = TRUE)
#2.5%   50% 97.5% 
#0    20   150 

table(studentdata$Dvds)
# how is 2.5 even possible?
# very few people have a large number of DVDs.

# how to specify breaks?
# People ususally round up/down to tens. and fives when they give the answer.
barplot(table(studentdata$Dvds), ylab = "count", xlab="Race")

# there seems to be a weak correlation on this.
ggplot(aes(x= ToSleep, y=WakeUp), data=studentdata)+
  geom_point()


fit = lm(studentdata$WakeUp~studentdata$ToSleep)
summary(fit)

abline(fit)


ggplot(aes(x= ToSleep, y=WakeUp), data=studentdata)+
  geom_point()+
  geom_line(data = fit, aes(y = fit))

#doesn't work? why?
cor(studentdata$ToSleep, studentdata$WakeUp)
