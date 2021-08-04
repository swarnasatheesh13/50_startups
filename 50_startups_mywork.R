#dataset = 50 startups
#dv = profit
#idv = apart from profit
#problem = linear regression

#loading the dataset into R
start_up <- read.csv(choose.files())

#checking for basic stuff of dataset
View(start_up)
dim(start_up)
str(start_up)
head(start_up)
tail(start_up)

#checking for missing values

colSums(is.na(start_up))

unique(start_up)

#changing the state into levels

unique(start_up$State)

start_up$State <- factor(start_up$State,
                       levels = c("New York","California","Florida"),
                       labels = c(0,1,2))

unique(start_up$State)

str(start_up)

#seeing summary
summary(start_up)

#fixing the random number using the set.seed
library(caTools)
set.seed(124)
split <- sample.split(start_up$Profit, SplitRatio = 0.75)
split
table(split)

#building the model with least ordinary square method
training <- subset(start_up, split==TRUE)
nrow(training)
test <- subset(start_up, split==FALSE)
nrow(test)

#model building

regs <- lm(Profit~., data = training)
regs
summary(regs)


regs1 <- lm(Profit~.-Administration, data=training)
regs1
summary(regs1)
#Adjusted R-squared:  0.9433 

#predict the dataset with reg1 model
pred_regs1 <-predict(regs1, newdata = test)
pred_regs1

# comparing actual profit and predicted value

pred_regs1_cbind <- cbind(test$Profit, pred_regs1)
pred_regs1_cbind


# vizualisation with actual and predicted
plot(test$Profit, col = "red",type='l',lty=1.5)
lines(pred_regs1, col="green", type='o',lty=1.8)


install.packages("lmtest")
library(lmtest)
dwtest(regs1)
#DW = 1.3759 so range of dwtest is 1-4
#multicolinearity-solution vif range is 0-5

# vif = 1/(1-r^2)
vif = 1/(1-0.9534)
vif
install.packages("faraway")
library(faraway)
vif(regs1)

R.D.Spend Marketing.Spend          State1          State2 
2.622906        2.755429        1.221825        1.318979 
# so all the five assumptions are satisfied.


