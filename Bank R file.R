






library(readr)
library(knitr)      # web widget
library(tidyverse)  # data manipulation
library(data.table) # fast file reading
library(caret)      # rocr analysis
library(ROCR)       # rocr analysis
install.packages("gridExtra")
library(kableExtra) # nice table html formating 
library(gridExtra)  # arranging ggplot in grid
library(rpart)      # decision tree
library(rpart.plot) # decision tree plotting
library(caTools)    # split 

banks <- read.csv(file.choose())
View(banks)
summary(banks)
# Data Validation #

#Check for Duplicate Rows
sum(duplicated(banks))

#Check for Missing Data
#How Many Rows Contain Missing Data
sum(!complete.cases(banks))

#How Many Rows Are Completely Missing Values In All Columns
all.empty = rowSums(is.na(banks))==ncol(banks)
sum(all.empty)

#Missing Value By Variable
sapply(banks, function(x) sum(is.na(x)))
# Data Cleaning #
# From  above results we can say that data is clean

# About The Cleaned Dataset #
# Data Structure of Cleaned Dataset
str(banks)

#Recoding 'yes' to binary
banks$y = ifelse(banks$y=='yes',1,0)
str(banks)

#Sample Rows
#Size of Dataset
nrow(banks)
ncol(banks)

#Sample Observations
head(banks)

#Data Summary
summary(banks)

#  Exploratory Data Analysis  ##
#Univariate Analysis
#Age Distribution
#The bulk of clients are between the ages of 33 (1st Quartile) and 48 (3rd Quartile) with mean lying on 41 visualized on the histogram with red vertical line.
#Boxplot of age describes essentially the same statistics but we can see outliers above the age of 65.
summary(banks$age)

gg = ggplot (banks) 
p1 = gg + geom_histogram(aes(x=age),color="black", fill="white", binwidth = 5) +
  ggtitle('Age Distribution (red mean line)') +
  ylab('Count') +
  xlab('Age') +
  geom_vline(aes(xintercept = mean(age), color = "red")) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  theme(legend.position = "none")

p2 = gg + geom_boxplot(aes(x='', y=age)) +
  ggtitle('Age Boxplot') +
  ylab('Age')

grid.arrange(p1, p2, ncol = 2)

# Age Distribution vs Marital Status That Subscribes Term Deposit
#The bulk of clients are married or divorced. Sharp drop of clients above age 60 with marital status 'divorced' and 'married'. *Single clients drop in numbers above age 40.

p3 <- ggplot(banks, aes(x=age, fill=marital)) + 
  geom_histogram(binwidth = 2, alpha=0.7) +
  facet_grid(cols = vars(y)) +
  expand_limits(x=c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  ggtitle("Age Distribution by Marital Status")

p3

# Age vs Subscription
#Most clients that subscribe are between age 25 to 45. Mean age for all clients is above 40 years of age.

mu <- banks %>% group_by(y) %>% summarize(grp.mean=mean(age))

ggplot (banks, aes(x=age)) + 
  geom_histogram(color = "blue", fill = "blue", binwidth = 5) +
  facet_grid(cols=vars(y)) + 
  ggtitle('Age Distribution by Subscription') + ylab('Count') + xlab('Age') +
  scale_x_continuous(breaks = seq(0,100,5)) +
  geom_vline(data=mu, aes(xintercept=grp.mean), color="red", linetype="dashed")

# Balance vs Subscription
#Clients that subscribe to term deposits have lower loan balances.

mu2 <- banks %>% group_by(y) %>% summarize(grp.mean2=mean(balance))

ggplot (banks, aes(x=balance)) + 
  geom_histogram(color = "blue", fill = "blue") +
  facet_grid(cols=vars(y)) + 
  ggtitle('Balance Histogram') + ylab('Count') + xlab('Balance') +
  geom_vline(data=mu2, aes(xintercept=grp.mean2), color="red", linetype="dashed")

#  Education vs Subscription
#Having higher education is seen to contribute to higher subscription of term deposit. Most clients who subscribe are from 'secondary' and 'tertiary' education levels. Tertiary educated clients have higher rate of subscription (15%) from total clients called.

ggplot(data = banks, aes(x=education, fill=y)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on Education Level") +
  xlab(" Education Level") +
  guides(fill=guide_legend(title="Subscription of Term Deposit"))

banks %>% 
  group_by(education) %>% 
  summarize(pct.yes = mean(y=="yes")*100) %>% 
  arrange(desc(pct.yes))

# Subscription based on Number of Contact during Campaign
# It can be observed from barchart that there will be no subscription beyond 7 contact during the campaign. Future campaign could improve resource utilization by setting limits to contacts during a campaign. Future campaigns can focus on first 3 contacts as it will have higher subscription rate.

ggplot(data=banks, aes(x=campaign, fill=y))+
  geom_histogram()+
  ggtitle("Subscription based on Number of Contact during the Campaign")+
  xlab("Number of Contact during the Campaign")+
  xlim(c(min=1,max=30)) +
  guides(fill=guide_legend(title="Subscription of Term Deposit"))

banks %>% 
  group_by(campaign) %>% 
  summarize(contact.cnt = n(), pct.con.yes = mean(y=="yes")*100) %>% 
  arrange(desc(contact.cnt)) %>% 
  head() 

#Duration
range(banks$duration)
summary(banks$duration)
banks %>% select(duration) %>% arrange(desc(duration)) %>% head

mu2 <- banks %>% group_by(y) %>% summarize(grp2.mean=mean(duration))

p6 <- ggplot(banks, aes(x=duration, fill = y)) +
  geom_histogram(binwidth = 2) +
  facet_grid(cols = vars(y)) +
  coord_cartesian(xlim = c(0,5000), ylim = c(0,400))

p6 + geom_vline(data = mu2, aes(xintercept = grp2.mean), color = "red", linetype = "dashed")


# Scatterplot of Duration by Age
#Less clients after age of 60. Duration during call looks similar.

banks %>% 
  ggplot(aes(age, duration)) +
  geom_point() +
  facet_grid(cols = vars(y)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  ggtitle("Scatterplot of Duration vs Age for Subscription of Term Deposit")


# Scatterplot of Duration by Campaign
#Duration on call similar for first 10 contacts during campaign. Successful subscription (y=1) occur within first 10 contacts. Much less after that.

banks %>% filter(campaign < 63) %>% 
  ggplot(aes(campaign, duration)) +
  geom_point() +
  facet_grid(cols = vars(y)) +
  ggtitle("Scatterplot of Duration vs Campaign for Subscription of Term Deposit")

# Scatterplot Matrix
#Due to large number of attributes (17 total), 8 was chosen for correlation. No clear correlation pattern can be observed as most attributes are categorical.

banks_select1 <- banks %>% select(duration, month, day, balance)
pairs(banks_select1)

banks_select2 <- banks %>% select(balance, housing, loan, campaign)
pairs(banks_select2)


#Split the Training / Testing data and Scale
#Split the dataset into traning and testing dataset and scale the numerical variables

# split into training and testing
set.seed(123)
split = sample.split(banks$y,SplitRatio = 0.70)
training_set = subset(banks, split == TRUE)
test_set = subset(banks, split == FALSE)

# scale
training_set[c(1,6,10,12,13)] = scale(training_set[c(1,6,10,12,13)])
test_set[c(1,6,10,12,13)] = scale(test_set[c(1,6,10,12,13)])

# Custom Function For Binary Class Performance Evaluation
binclass_eval = function (actual, predict) {
  cm = table(as.integer(actual), as.integer(predict), dnn=c('Actual','Predicted'))
  ac = (cm['1','1']+cm['0','0'])/(cm['0','1'] + cm['1','0'] + cm['1','1'] + cm['0','0'])
  pr = cm['1','1']/(cm['0','1'] + cm['1','1'])
  rc = cm['1','1']/(cm['1','0'] + cm['1','1'])
  fs = 2* pr*rc/(pr+rc)
  list(cm=cm, recall=rc, precision=pr, fscore=fs, accuracy=ac)
}

# Create a function for plotting distribution
# Function to be used later for plotting the prediction distribution

plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$y == 1, "TP", v)
  v <- ifelse(df$pred >= threshold & df$y == 0, "FP", v)
  v <- ifelse(df$pred < threshold & df$y == 1, "FN", v)
  v <- ifelse(df$pred < threshold & df$y == 0, "TN", v)
  
  df$pred_type <- v
  
  ggplot(data=df, aes(x=y, y=pred)) + 
    geom_violin(fill='black', color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}


#  Model - Build Model by fitting Logistic Regression
# build model by fitting Logistic Regression algorithm in Tranining dataset

# creating the classifier
classifier.lm = glm(formula = y ~ .,
                    family = binomial,
                    data = training_set)
summary(classifier.lm)

#Evaluate the Logistic Regression Prediction model with cut off value 0.30
#Predict the Logistic Regression model on testing data and evaluate by finding the accuracy and calculating/plotting ROC curve.

pred_lm = predict(classifier.lm, type='response', newdata=test_set[-17])

# plot the prediction distribution
predictions_LR <- data.frame(y = test_set$y, pred = NA)
predictions_LR$pred <- pred_lm
plot_pred_type_distribution(predictions_LR,0.30)

# choose the best threshold as 0.30
test.eval.LR = binclass_eval(test_set[, 17], pred_lm > 0.30)

# Making the Confusion Matrix
test.eval.LR$cm

# calculate accuracy, precision etc.
acc_LR=test.eval.LR$accuracy
prc_LR=test.eval.LR$precision
recall_LR=test.eval.LR$recall
fscore_LR=test.eval.LR$fscore

# print evaluation
cat("Accuracy:  ",   acc_LR,
    "\nPrecision: ", prc_LR,
    "\nRecall:    ", recall_LR,
    "\nFScore:    ", fscore_LR)

# calculate ROC
rocr.pred.lr = prediction(predictions = pred_lm, labels = test_set$y)
rocr.perf.lr = performance(rocr.pred.lr, measure = "tpr", x.measure = "fpr")
rocr.auc.lr = as.numeric(performance(rocr.pred.lr, "auc")@y.values)

# print ROC AUC
rocr.auc.lr

# plot ROC curve
plot(rocr.perf.lr,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     text.adj = c(-0.2, 1.7),
     main = 'ROC Curve')
mtext(paste('Logistic Regression - auc : ', round(rocr.auc.lr, 5)))
abline(0, 1, col = "red", lty = 2)


#Evaluate the Logistic Regression Prediction model with cut off value 0.20
#Predict the Logistic Regression model on testing data and evaluate by finding the accuracy and calculating/plotting ROC curve.

pred_lm = predict(classifier.lm, type='response', newdata=test_set[-17])

# plot the prediction distribution
predictions_LR <- data.frame(y = test_set$y, pred = NA)
predictions_LR$pred <- pred_lm
plot_pred_type_distribution(predictions_LR,0.20)

# choose the best threshold as 0.30
test.eval.LR = binclass_eval(test_set[, 17], pred_lm > 0.20)

# Making the Confusion Matrix
test.eval.LR$cm

# calculate accuracy, precision etc.
acc_LR=test.eval.LR$accuracy
prc_LR=test.eval.LR$precision
recall_LR=test.eval.LR$recall
fscore_LR=test.eval.LR$fscore

# print evaluation
cat("Accuracy:  ",   acc_LR,
    "\nPrecision: ", prc_LR,
    "\nRecall:    ", recall_LR,
    "\nFScore:    ", fscore_LR)

# calculate ROC
rocr.pred.lr = prediction(predictions = pred_lm, labels = test_set$y)
rocr.perf.lr = performance(rocr.pred.lr, measure = "tpr", x.measure = "fpr")
rocr.auc.lr = as.numeric(performance(rocr.pred.lr, "auc")@y.values)

# print ROC AUC
rocr.auc.lr

# plot ROC curve
plot(rocr.perf.lr,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     text.adj = c(-0.2, 1.7),
     main = 'ROC Curve')
mtext(paste('Logistic Regression - auc : ', round(rocr.auc.lr, 5)))
abline(0, 1, col = "red", lty = 2)

#as with cutoff value of 0.20 accuracy of model is decreasing so
# from above comparison we finalise model classifier.glm with cutoff of 0.30 .

113 