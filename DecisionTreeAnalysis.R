# Final Project

getwd()
setwd("/Users/tylermiguel/Desktop/Bentley/ST635/Final Project/")

# load libraries
install.packages("tree")
install.packages("mosaic")
install.packages("dplyr")
library(tree)
library(mosaic)
library(dplyr)

# Batting data
#####
# read file into R
batting = read.csv("HOFBattingUpdated.csv")
head(batting)
str(batting) # structure is ready for analysis

# create dummy variable, inducted vs not inducted to turn numeric InductionYear variable to categorical binary Y/N variable
batting.inducted = ifelse(batting$InductionYear != 0, "Yes", "No")
batting.inducted = as.factor(batting.inducted) # convert to factor
batting = data.frame(batting,batting.inducted)

# split data into train and test (70/30)
set.seed(107) # for reproducible results (need to work more with set seed)
batting.train = sample(1:nrow(batting), size = round(0.7*nrow(batting)))
batting.test = batting[-batting.train,]
inducted.test = batting.inducted[-batting.train]

# create decision tree of induction vs not induction
tree.induction = tree(batting.inducted~.-PlayerID-nameFirst-nameLast-FullName-DebutYear-FinalYear-InductionYear-votedBy, batting, subset = batting.train)
batting.tree.pred = predict(tree.induction, batting.test, type = "class")
table(batting.tree.pred,inducted.test)

(516+23)/(581) # good 92.77% successful classification rate on prediction
(516+25)/(581) # just selecting "No" everything gives us 93.11%, our model is slightly worse than guessing "no"

summary(tree.induction) # misclassifcation rate of orginal tree on training data is 3.169%

plot(tree.induction)
text(tree.induction, pretty = 0)

# prune the tree
## prune tree
set.seed(204)

# first need to do cross-validation to select approraite amount of nodes
cv.induction = cv.tree(tree.induction, FUN = prune.misclass)
names(cv.induction)
cv.induction
par(mfrow = c(1,1)) # sets the plot area to be 1 column by 1 row plotting (i.e. one plot per screen)
plot(cv.induction$size, cv.induction$dev, type = "b")
plot(cv.induction$k, cv.induction$dev, type = "b")

# prune tree
prune.induction = prune.misclass(tree.induction, best = 9)
plot(prune.induction)
text(prune.induction, pretty = 0)
tree.predict.prune = predict(prune.induction, batting.test, type = "class")
table(tree.predict.prune, inducted.test)

(525+22)/(581) # 94.15%
summary(prune.induction) # misclassifcation rate of pruned tree (3.316%) on training data is slightly larger than misclass of orginal tree on training data (3.169%) but performs better on test set which shows a better fit without overfitting

# Lets take a look at false positives and false negatives for our training set we used to build the model
# get training set data frame
batting.train.dataset <- batting[batting.train,]

# create identifier for false pos and neg for training set
batting.train.dataset <- mutate(batting.train.dataset, y.hat = predict(prune.induction, type="class"), induct.prob = predict(prune.induction)[,2])

# false positives
batting.train.dataset %>%
  filter(y.hat == "Yes" & batting.inducted == "No") %>%
  select(FullName, R, BAvg, AB, PrimaryPosition, HBP, CS, X3B, DebutYear, FinalYear, batting.inducted, y.hat, induct.prob) %>%
  arrange(desc(induct.prob))

# false negatives
batting.train.dataset %>%
  filter(y.hat == "No" & batting.inducted == "Yes") %>%
  select(FullName, R, BAvg, AB, PrimaryPosition, HBP, CS, X3B, DebutYear, FinalYear, batting.inducted, y.hat, induct.prob) %>%
  arrange(desc(induct.prob))

# Lets take a look at false positives and false negatives for our training set we used to test our model

# create identifier for false pos and neg for test set
batting.test.dataset <- mutate(batting.test, y.hat = predict(prune.induction, batting.test, type="class"), induct.prob = predict(prune.induction, batting.test)[,2])

# false positives
batting.test.dataset  %>%
  filter(y.hat == "Yes" & batting.inducted == "No") %>%
  select(FullName, R, BAvg, AB, PrimaryPosition, HBP, CS, X3B, DebutYear, FinalYear, batting.inducted, y.hat, induct.prob) %>%
  arrange(desc(induct.prob))

# false negatives 
batting.test.dataset  %>%
  filter(y.hat == "No" & batting.inducted == "Yes") %>%
  select(FullName, R, BAvg, AB, PrimaryPosition, HBP, CS, X3B, DebutYear, FinalYear, batting.inducted, y.hat, induct.prob) %>%
  arrange(desc(induct.prob))

#####
# Pitching data
##### 
# read file into R
pitching = read.csv("HOFPitchingUpdated.csv")
head(pitching)
str(pitching)

# create dummy variable, inducted vs not inducted to turn numeric InductionYear variable to categorical binary Y/N variable
inducted = ifelse(pitching$InductionYear != 0, "Yes", "No")
pitching = data.frame(pitching,inducted)
names(pitching)
str(pitching)

# split data into train and test (70/30)
set.seed(303) # for reproducible results (need to work more with set seed)
train.pitch = sample(1:nrow(pitching), size = round(0.7*nrow(pitching)))
pitching.test = pitching[-train.pitch,]
inducted.pitch.test = inducted[-train.pitch]

# create decision tree of induction vs not induction
tree.pitch.induction = tree(inducted~.-PlayerID-nameFirst-nameLast-FullName-DebutYear-FinalYear-InductionYear-votedBy, pitching, subset = train.pitch)
tree.pitch.pred = predict(tree.pitch.induction, pitching.test, type = "class")
table(tree.pitch.pred,inducted.pitch.test)

(353+7)/(373) # 96.51% successful classification rate on prediction
(353+6)/(373) # 96.25% success rate if we just select "no"

summary(tree.pitch.induction) # misclass rate on training data is 1.381%
plot(tree.pitch.induction)
text(tree.pitch.induction, pretty = 0)

# prune the tree
## prune tree
set.seed(403)

# first need to do cross-validation to select approraite amount of nodes
cv.pitch.induction = cv.tree(tree.pitch.induction, FUN = prune.misclass)
names(cv.pitch.induction)
cv.pitch.induction
plot(cv.pitch.induction$size, cv.pitch.induction$dev, type = "b")
plot(cv.pitch.induction$k, cv.pitch.induction$dev, type = "b")

# prune tree
prune.pitch.induction = prune.misclass(tree.pitch.induction, best = 4)
plot(prune.pitch.induction)
text(prune.pitch.induction, pretty = 0)
tree.pitch.pred.prune = predict(prune.pitch.induction, pitching.test, type = "class")
table(tree.pitch.pred.prune, inducted.pitch.test)

(355+6)/(373) # 96.78% slightly better
summary(prune.pitch.induction) # misclass on test set is 2.071%

# Lets take a look at false positives and false negatives for our training set we used to build the model
# get training set data frame
pitching.train.dataset <- pitching[train.pitch,]

pitching.train.dataset <- mutate(pitching.train.dataset, y.hat = predict(prune.pitch.induction, type="class"), induct.prob = predict(prune.pitch.induction)[,2])

# false positives
pitching.train.dataset %>%
  filter(y.hat == "Yes" & inducted == "No") %>%
  select(FullName, DebutYear, FinalYear, W, SHO, G, inducted, y.hat, induct.prob) %>%
  arrange(desc(induct.prob))

# false negatives 
pitching.train.dataset %>%
  filter(y.hat == "No" & inducted == "Yes") %>%
  select(FullName, DebutYear, FinalYear, W, SHO, G, inducted, y.hat, induct.prob) %>%
  arrange(desc(induct.prob))

# Lets take a look at false positives and false negatives for our test set we used to test our model

# create identifier for false pos and neg for test set
pitching.test.dataset <- mutate(pitching.test, y.hat = predict(prune.pitch.induction, pitching.test, type="class"), induct.prob = predict(prune.pitch.induction, pitching.test)[,2])

# false positives
pitching.test.dataset %>%
  filter(y.hat == "Yes" & inducted == "No") %>%
  select(FullName, DebutYear, FinalYear, W, SHO, G, inducted, y.hat, induct.prob) %>%
  arrange(desc(induct.prob))

# false negatives # training data set contains pitchers
pitching.test.dataset %>%
  filter(y.hat == "No" & inducted == "Yes") %>%
  select(FullName, DebutYear, FinalYear, W, SHO, G, inducted, y.hat, induct.prob) %>%
  arrange(desc(induct.prob))

#####