# Final Project

getwd()
setwd("/Users/tylermiguel/Desktop/Bentley/ST635/Final Project/")

# load libraries
install.packages("aod")
install.packages("ggplot2")
install.packages("Rcpp")
install.packages("ResourceSelection")
library(aod)
library(ggplot2)
library(Rcpp)
library(ResourceSelection)

# logistic regression

# We are interested in the factors that influence whether a baseball player has made it to the hall of fame. The outcome (response) variable is binary (0/1); yes or no. The predictor variables of interest are the variables that we used in the pruned decision tree.

# Batting data
#####
# read file into R
batting <- read.csv("HOFBattingUpdated.csv")
head(batting)
str(batting) # structure is almost ready for analysis

# create binary variable for InductionYear
batting.inducted <- ifelse(batting$InductionYear == 0, "No", "Yes")
batting.inducted <- as.factor(batting.inducted) # convert to factor
batting <- data.frame(batting, batting.inducted) # merge new var. with dataset
names(batting) # see how the names include the new var.

# convert PrimaryPosition, which is a factor, to numeric data, then to factor
## Note: I don't know if I needed to do this, but I had an issue later in the code where when I did this it fixed is, so we will use it
str(batting$PrimaryPosition)
batting$PrimaryPosition = ifelse(batting$PrimaryPosition == "Catcher", 1, ifelse(batting$PrimaryPosition == "FirstBase", 2, ifelse(batting$PrimaryPosition == "Outfield", 3, ifelse(batting$PrimaryPosition == "SecondBase", 4, ifelse(batting$PrimaryPosition == "Shortstop", 5, 6)))))
str(batting$PrimaryPosition)
batting$PrimaryPosition <- as.factor(batting$PrimaryPosition) # turns numeric into factor

# create a contingency table to examine the data
xtabs(~ batting.inducted + PrimaryPosition, batting) # Recall primary position is now a number 1:6

# split data into train and test (70/30) -- should be same data as tree
set.seed(107) # for reproducible results (need to work more with set seed)
batting.train = sample(1:nrow(batting), size = round(0.7*nrow(batting)))
batting.test = batting[-batting.train,]
inducted.test = batting.inducted[-batting.train]

# create logit model with pruned tree variables
batlogit <- glm(batting.inducted ~ R + BAvg + AB + PrimaryPosition + HBP + CS + X3B, data = batting, family = "binomial", subset = batting.train)
summary(batlogit) # the estimate shows the change in logodds by adding 1 unit to any variable. Catcher (i.e. PrimaryPosition1) is not in the model because catcher is the default. If FirstBaseman (i.e. PrimaryPosition2) is true (i.e. binary 0/1 option is set to 1) then their is a logodds change of making the HOF, holding all other variables, is -2.417e+00. That means the the logodds decreases from being a catcher to being firstbasesmen

# get confidence intervals using log-likelihood
confint(batlogit)
# get confidence intervals using standard error
confint.default(batlogit)

# test for the effect of HBP
wald.test(Sigma = vcov(batlogit), b = coef(batlogit), Terms = 10) # not statistically sig at 5%
# test the overall effect of position
wald.test(Sigma = vcov(batlogit), b = coef(batlogit), Terms = 5:9) # statistically sig at 5%

# odds ratio
bat_hof_odds <- exp(coef(batlogit)) # changes log-odds to odds
bat_hof_odds
names(bat_hof_odds) 

# predicted probability of getting in HOF given all var. held at their mean except position
bat_data1 <- with(batting, subset = batting.train, data.frame(R = mean(R), BAvg = mean(BAvg), AB = mean(AB), PrimaryPosition = factor(1:6), HBP = mean(HBP), CS = mean(CS), X3B = mean(X3B))) 
bat_data1 # this just prepares data, as you can see, all data is the same excep PrimaryPosition

# now predict the probability of getting into HOF based on the position
bat_data1$posP <- predict(batlogit, newdata = bat_data1, type = "response")
bat_data1 # the probability of a catcher in our training data making it to the HOF is 4% (holding everything else)

# prepare to plot probability of making to HOF against batting average (seperate out by position)
# create new data set with variables used in logit model held at their means but vary BAvg and Primary position
bat_data2 <- with(batting, subset = batting.train,
                 data.frame(R = mean(R), BAvg = rep(seq(from = .1610, to = .3664, length.out = 100), 6), AB = mean(AB), PrimaryPosition = factor(rep(1:6, each = 100)), HBP = mean(HBP), CS = mean(CS), X3B = mean(X3B)))

# take new data set and predict HOF probability based on the data and logit model we created, and keep the standard errors to use a CI for graph
bat_data3 <- cbind(bat_data2, predict(batlogit, newdata = bat_data2, type="link", se=TRUE))

# add probability based on logitic distribution, and create CI intervals, LL and UL
bat_data3 <- within(bat_data3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# viz # catchers clearly have a higher probability of getting into the HOF
ggplot(bat_data3, aes(x = BAvg, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = PrimaryPosition), alpha = .2) +
  geom_line(aes(colour = PrimaryPosition), size=1)

# test for model fit using Hosmer Lemeshow
h.bat <- hoslem.test(batlogit$y, fitted(batlogit), g=10)
h.bat # because p-value is non-significant there is no evidence that the model doens't fit badly, although model isn't proven it fits well
cbind(h.bat$observed, h.bat$expected) # when we look at the expected v observed values, we see that our expected values are close to the observed values

# now lets predict the outcomes of the logit model using the test set
batting.test$pred <- predict(batlogit, newdata = batting.test, type = "response")
batting.test$predYN <- ifelse(batting.test$pred >= 0.5, "Yes", "No")
batting.test$predYN <- as.factor(batting.test$predYN)
table(batting.test$predYN, batting.test$batting.inducted)
(535+20)/(535+20+20+6) #95.52% success rate on predicting our test set

#####
# Pitching data
#####
# read file into R
pitching = read.csv("HOFPitchingUpdated.csv")
head(pitching)
str(pitching) # structure is almost ready for analysis

# create binary variable for InductionYear
pitching.inducted <- ifelse(pitching$InductionYear == 0, "No", "Yes")
pitching.inducted <- as.factor(pitching.inducted) # convert to factor
pitching <- data.frame(pitching, pitching.inducted) # merge new var. with dataset
names(pitching) # see how the names include the new var.

# split data into train and test (70/30)
set.seed(303) # for reproducible results (need to work more with set seed)
train.pitch = sample(1:nrow(pitching), size = round(0.7*nrow(pitching)))
pitching.test = pitching[-train.pitch,]
inducted.pitch.test = inducted[-train.pitch]

# create logit model with pruned tree variables
pitchlogit <- glm(pitching.inducted ~ W + SHO + GS, data = pitching, family = "binomial", subset = train.pitch)
summary(pitchlogit) # the estimate shows the change in logodds by adding 1 unit to any variable. The log-odds of making the HOF increases by 0.08 with a unit increase in Ws.

# get confidence intervals using log-likelihood
confint(pitchlogit)
# get confidence intervals using standard error
confint.default(pitchlogit)

# odds ratio
pit_hof_odds <- exp(coef(pitchlogit)) # changes log-odds to odds
pit_hof_odds
names(pit_hof_odds)

# prepare to plot probability of making to HOF against Ws
# create new data set with variables used in logit model held at their means but vary W
pit_data1 <- with(pitching, subset = train.pitch,
                  data.frame(W = rep(seq(from = 8, to = 511, length.out = 100), 6), SHO = mean(SHO), GS = mean(GS)))

# take new data set and predict HOF probability based on the data and logit model we created, and keep the standard errors to use a CI for graph
pit_data2 <- cbind(pit_data1, predict(pitchlogit, newdata = pit_data1, type="link", se=TRUE))

# add probability based on logitic distribution, and create CI intervals, LL and UL
pit_data2 <- within(pit_data2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# viz # pitchers tend to have a high probability of getting into the HOF over 188 wins
ggplot(pit_data2, aes(x = W, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .2) +
  geom_line(aes(colour = W), size=1)

# test for model fit using Hosmer Lemeshow
h.pitch <- hoslem.test(pitchlogit$y, fitted(pitchlogit), g=10)
h.pitch # because p-value is non-significant there is no evidence that the model doens't fit badly, although model isn't proven it fits well
cbind(h.pitch$observed, h.pitch$expected) # when we look at the expected v observed values, we see that our expected values are close to the observed values

# now lets predict the outcomes of the logit model using the test set
pitching.test$pred <- predict(pitchlogit, newdata = pitching.test, type = "response")
pitching.test$predYN <- ifelse(pitching.test$pred >= 0.5, "Yes", "No")
pitching.test$predYN <- as.factor(pitching.test$predYN)
table(pitching.test$predYN, pitching.test$pitching.inducted)
(357+5)/(357+9+5+2) #97.05% success rate on predicting our test set
