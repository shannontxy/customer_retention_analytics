setwd("C:/Users/shann/Desktop/Year 2 Sem 1/BC2406    Analytics l - Visual&Predictive Techniques/Project")

# data table
library(data.table)

# data visualisation
#install.packages("gmodels")
#install.packages("corrplot")
#install.packages("caTools")
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("caTools")
#install.packages("ggridges")
#install.packages("scales")
#install.packages("ggbeeswarm")
#install.packages("ggmosaic")
#install.packages("cluster")
#install.packages("gridExtra")
#install.packages("factoextra")
#install.packages("caret")
library(gmodels)
library(corrplot)
library(caTools)
library(ggplot2)
library(reshape2)
library(caTools)
library(ggridges)
library(scales)
library(ggbeeswarm)
library(ggmosaic)
library(cluster)
library(gridExtra)
library(cluster)  
library(factoextra)
library(caret)

# data cleaning
install.packages("mltools")
install.packages("DMwR")
install.packages("dplyr")
library(mltools)
library(DMwR)
library(dplyr)


#------------------------------------Data Cleaning: BankChurn.csv ---------------------------------------------------------------
churndata <- fread("BankChurn.csv", stringsAsFactors = TRUE)

#remove test cases 
#in customerId
class(churndata$CustomerId)
churndata$CustomerId <- as.numeric(as.character(churndata$CustomerId)) #change variables to charcters then numeric 
#in balance
class(churndata$Balance)
churndata$Balance <- as.numeric(as.character(churndata$Balance)) #change variables to charcters then numeric 

#remove na values 
sum(is.na(churndata))
churndata <- na.omit(churndata)

#remove duplicated rows 
churndata <- churndata[!(duplicated(churndata$CustomerId) | duplicated(churndata$CustomerId, fromLast = TRUE)),]

#drop columns surname, geography and numofproducts
churndata <- select(churndata, -RowNumber, -Surname)

#set categorical data 
churndata$Exited <- as.factor(churndata$Exited)
churndata$HasCrCard <- as.factor(churndata$HasCrCard)
churndata$IsActiveMember <- as.factor(churndata$IsActiveMember)
churndata$Gender <- as.factor(churndata$Gender)
churndata$Tenure <- as.factor(churndata$Tenure)
churndata$Geography <- as.factor(churndata$Geography)
churndata$NumOfProducts <- as.factor(churndata$NumOfProducts)
#churndata$Tenure <- ordered(churndata$Tenure, levels = c('0','1','2','3','4','5','6','7','8','9','10'))

#set numerical data 
churndata$Age <- as.numeric(churndata$Age)
churndata$CreditScore <- as.numeric(churndata$CreditScore)
churndata$Balance <- as.numeric(churndata$Balance)
churndata$EstimatedSalary <- as.numeric(churndata$EstimatedSalary)


#----------------------------------Data Visualisation(BankChurn.csv): Univariate Categorical---------------------------------------------------------------
#Exited
ggplot(churndata, aes(x = churndata$Exited)) + 
  geom_bar(fill = "indianred3", colour="black") +
  labs(x = "Exited", 
       y = "Count", 
       title = "Frequency by Y - Has the customer exited?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#Hascrcard 
ggplot(churndata, aes(x = churndata$HasCrCard)) + 
  geom_bar(fill = "indianred3", colour="black") +
  labs(x = "Has Credit Card", 
       y = "Count", 
       title = "Frequency by Y - Does the customer have credit card?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#IsActiveMember
ggplot(churndata, aes(x = churndata$IsActiveMember)) + 
  geom_bar(fill = "indianred3", colour="black") +
  labs(x = "Is Active Member", 
       y = "Count", 
       title = "Frequency by Y - Is the customer active?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#Gender
ggplot(churndata, aes(x = churndata$Gender)) + 
  geom_bar(fill = "indianred3", colour="black") +
  labs(x = "Gender", 
       y = "Count", 
       title = "Frequency by Y - Gender of customer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#Tenure 
ggplot(churndata, aes(x = churndata$Tenure)) + 
  geom_bar(fill = "indianred3", colour="black") +
  labs(x = "Tenure", 
       y = "Count", 
       title = "Frequency by Y - Customer Tenure?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#Geography 
ggplot(churndata, aes(x = churndata$Geography)) + 
  geom_bar(fill = "indianred3", colour="black") +
  labs(x = "Geography", 
       y = "Count", 
       title = "Frequency by Y - Geography") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#NumOfproducts
ggplot(churndata, aes(x = churndata$NumOfProducts)) + 
  geom_bar(fill = "indianred3", colour="black") +
  labs(x = "Number of Products", 
       y = "Count", 
       title = "Frequency by Y - Number of Products") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#summary - gender, tenure, hascrcard, isactivemember, exited, geography, numofproducts
catchurn <- churndata[, lapply(churndata, is.factor) == TRUE, with = FALSE, ]
head(catchurn)

ggplot(melt(catchurn, id.vars="Exited"), aes(y = value)) + 
  facet_wrap(~ variable, scales = "free", ncol=2) +
  geom_bar(fill = "lightcoral", color="black")+
  theme_minimal()+
  labs(x = "Factors", y = "Levels", title = "Barplot of Categorical X Factors") +
  theme(plot.title = element_text(hjust = 0.5))

#----------------------------------Data Visualisation(BankChurn.csv): Univariate Numerical---------------------------------------------------------------
#age - histogram
ggplot(churndata, aes(x = Age)) + 
  geom_histogram(fill = "cornflowerblue", colour="black", bins = 50) +
  labs(x = "Age", 
       y = "Count", 
       title = "Frequency by Y - Age of customers?") +
  theme(plot.title = element_text(hjust = 0.5))

#age - kernel
ggplot(churndata, aes(x = Age)) +
  geom_density(fill = "indianred3") +
  labs(x = "Age",
       y = "Density",
       title = "Customers by age") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#credit score - histogram
ggplot(churndata, aes(x = CreditScore)) + 
  geom_histogram(fill = "cornflowerblue", colour="black", bins = 50) +
  labs(x = "Credit Score", 
       y = "Count", 
       title = "Frequency by Y - Credit score of customers?") +
  theme(plot.title = element_text(hjust = 0.5))

#credit score - kernel 
ggplot(churndata, aes(x = CreditScore)) +
  geom_density(fill = "indianred3") +
  labs(x = "Credit Score",
       y = "Density",
       title = "Credit Score of customers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#balance - histogram 
ggplot(churndata, aes(x = Balance)) + 
  geom_histogram(fill = "cornflowerblue", colour="black", bins = 50) +
  labs(x = "Balance", 
       y = "Count", 
       title = "Frequency by Y - Balance of customers?") +
  theme(plot.title = element_text(hjust = 0.5))

#balance - kernel 
ggplot(churndata, aes(x = Balance)) +
  geom_density(fill = "indianred3") +
  labs(x = "Balance",
       y = "Density",
       title = "Balance of customers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma)

#estimated salary - histogram 
ggplot(churndata, aes(x = EstimatedSalary)) + 
  geom_histogram(fill = "cornflowerblue", colour="black", bins = 50) +
  labs(x = "Estimated Salary", 
       y = "Count", 
       title = "Frequency by Y - Estimated Salary of customers?") +
  theme(plot.title = element_text(hjust = 0.5))

#estimated salary - kernel 
ggplot(churndata, aes(x = EstimatedSalary)) +
  geom_density(fill = "indianred3") +
  labs(x = "Estimated Salary",
       y = "Density",
       title = "Estimated Salary of customers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma)

#summary histogram - age, credit score, balance and estimated salary 
numchurn = churndata[, lapply(churndata, is.numeric) == TRUE, with = FALSE, ]
numchurn <- select(numchurn, -CustomerId)
head(numchurn)
ggplot(melt(numchurn), aes(x = value)) + 
  facet_wrap(~ variable, scales = "free", ncol = 2) + 
  geom_histogram(fill = "lightcoral", colour="black")+
  theme_minimal()+
  labs(x = "Factors", y = "Distribution", title = "Histogram of X Factors") +
  theme(plot.title = element_text(hjust = 0.5))

#summary kernel - age, credit score, balance and estimated salary
ggplot(melt(numchurn), aes(x = value)) + 
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  geom_density(fill = "lightcoral")+
  theme_minimal()+
  labs(x = "Factors", y = "Density", title = "Density Plot of X Factors") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma)

#----------------------------------Data Visualisation(BankChurn.csv): Bivariate - Categorical vs Categorical---------------------------------------------------------------
#hascrcard vs exited
ggplot(catchurn, aes(x = HasCrCard, fill = Exited)) +
  geom_bar(position = "stack") +
  labs(x = "Has Credit Card",
       y = "Count",
       title = "Relationship between HasCrCard and Exited") +
  theme_minimal()

#isactivemember vs exited 
ggplot(catchurn, aes(x = IsActiveMember, fill = Exited)) +
  geom_bar(position = "stack") +
  labs(x = "Is Active Member?",
       y = "Count",
       title = "Relationship between IsActiveMember and Exited") +
  theme_minimal()

#gender vs exited
ggplot(catchurn, aes(x = Gender, fill = Exited)) +
  geom_bar(position = "stack") +
  labs(x = "Gender",
       y = "Count",
       title = "Relationship between Gender and Exited") +
  theme_minimal()

#Tenure vs exited
ggplot(catchurn, aes(x = Tenure, fill = Exited)) +
  geom_bar(position = "stack") +
  labs(x = "Tenure",
       y = "Count",
       title = "Relationship between Tenure and Exited") +
  theme_minimal()

#geography vs exited 
ggplot(catchurn, aes(x = Geography, fill = Exited)) +
  geom_bar(position = "stack") +
  labs(x = "Geography",
       y = "Count",
       title = "Relationship between Geography and Exited") +
  theme_minimal()

#numofproducts vs exited
ggplot(catchurn, aes(x = NumOfProducts, fill = Exited)) +
  geom_bar(position = "stack") +
  labs(x = "NumOfProducts",
       y = "Count",
       title = "Relationship between Number of Products and Exited") +
  theme_minimal()

#----------------------------------Data Visualisation(BankChurn.csv): Bivariate - Categorical vs Numerical---------------------------------------------------------------
#violin and boxplot 
#age and exited 
ggplot(churndata, aes(x = Exited, y = Age, fill = Exited)) +
  geom_violin() + 
  geom_boxplot(width = 0.4,
               fill = "orange",
               outlier.colour = "orange",
               outlier.size = 1.25) +
  labs(y = "Age",
       title = "Relationship between Age and Exited") +
  theme_minimal()

#credit score and exited 
ggplot(churndata, aes(x = Exited, y = CreditScore, fill = Exited)) +
  geom_violin() + 
  geom_boxplot(width = 0.4,
               fill = "orange",
               outlier.colour = "orange",
               outlier.size = 1.25) +
  labs(y = "Age",
       title = "Relationship between Credit Score and Exited") +
  theme_minimal()

#estimated salary and exited 
ggplot(churndata, aes(x = Exited, y = EstimatedSalary, fill = Exited)) +
  geom_violin() + 
  geom_boxplot(width = 0.4,
               fill = "orange",
               outlier.colour = "orange",
               outlier.size = 1.25) +
  labs(y = "Age",
       title = "Relationship between Estimated Salary and Exited") +
  theme_minimal()

#balance and exited
ggplot(churndata, aes(x = Exited, y = Balance, fill = Exited)) +
  geom_violin() + 
  geom_boxplot(width = 0.4,
               fill = "orange",
               outlier.colour = "orange",
               outlier.size = 1.25) +
  labs(y = "Age",
       title = "Relationship between Balance and Exited") +
  theme_minimal() 

churndata2 <- fread("BankChurn2.csv", stringsAsFactors = TRUE)

#--------------------------------------Data Cleaning:(BankChurn2.csv)---------------------------------------------------------------

churndata2 <- fread("BankChurn2.csv", stringsAsFactors = TRUE)

#combining FaceBook and Facebook 
class(churndata2$Promomethod)
churndata2$Promomethod[churndata2$Promomethod == "FaceBook"] <- "Facebook"

churndata2$Promomethod <- droplevels(churndata2$Promomethod)

#drop columns surname, geography and numofproducts
churndata2 <- select(churndata2, -RowNumber, -Surname)

#changing personalisation to 1 and control to 0 
class(churndata2$Variant)
churndata2$Variant <- as.character(churndata2$Variant)
churndata2$Variant[churndata2$Variant == "Personalisation"] <- "1"
churndata2$Variant[churndata2$Variant == "Control"] <- "0"
churndata2$Variant <- as.factor(churndata2$Variant)

#set categorical data 
churndata2$Exited <- as.factor(churndata2$Exited)
churndata2$HasCrCard <- as.factor(churndata2$HasCrCard)
churndata2$IsActiveMember <- as.factor(churndata2$IsActiveMember)
churndata2$Gender <- as.factor(churndata2$Gender)
churndata2$Tenure <- as.factor(churndata2$Tenure)
churndata$Geography <- as.factor(churndata$Geography)
churndata$NumOfProducts <- as.factor(churndata$NumOfProducts)
churndata2$Promomethod <- as.factor(churndata2$Promomethod)
churndata2$Subscriptionchannel <- as.factor(churndata2$Subscriptionchannel)
churndata2$Language_provided <- as.factor(churndata2$Language_provided)
churndata2$Language_preferred <- as.factor(churndata2$Language_preferred)
churndata2$Variant <- as.factor(churndata2$Variant)
#churndata$Tenure <- ordered(churndata$Tenure, levels = c('0','1','2','3','4','5','6','7','8','9','10'))

#set numerical data 
churndata2$Age <- as.numeric(churndata2$Age)
churndata2$CreditScore <- as.numeric(churndata2$CreditScore)
churndata2$Balance <- as.numeric(churndata2$Balance)
churndata2$EstimatedSalary <- as.numeric(churndata2$EstimatedSalary)

#----------------------------------Data Visualisation(BankChurn2.csv): Univariate Categorical---------------------------------------------------------------
#promomethod
ggplot(churndata2, aes(x = churndata2$Promomethod)) + 
  geom_bar(fill = "indianred3", colour="black") +
  labs(x = "Promomethod", 
       y = "Count", 
       title = "Frequency by Y - Promomethod used?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#subscription channel 
ggplot(churndata2, aes(x = churndata2$Subscriptionchannel)) + 
  geom_bar(fill = "indianred3", colour="black") +
  labs(x = "Subscription Channel", 
       y = "Count", 
       title = "Frequency by Y - Subscription channel?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#language provided 
ggplot(churndata2, aes(x = churndata2$Language_provided)) + 
  geom_bar(fill = "indianred3", colour="black") +
  labs(x = "Language_Provided", 
       y = "Count", 
       title = "Frequency by Y - Language Provided?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#language preferred 
ggplot(churndata2, aes(x = churndata2$Language_preferred)) + 
  geom_bar(fill = "indianred3", colour="black") +
  labs(x = "Language_Preferred", 
       y = "Count", 
       title = "Frequency by Y - Language Preferred") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#variant 
ggplot(churndata2, aes(x = churndata2$Variant)) + 
  geom_bar(fill = "indianred3", colour="black") +
  labs(x = "Variant", 
       y = "Count", 
       title = "Frequency by Y - Variant") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#summary - promomethod, subscription channel, language provided, language preferred, variant 
catchurn2 <- churndata2[, lapply(churndata2, is.factor) == TRUE, with = FALSE, ]
catchurn2 <- select(catchurn2, -Gender, -Tenure, -HasCrCard, -IsActiveMember, -Geography)
head(catchurn2)
ggplot(melt(catchurn2, id.vars="Exited"), aes(y = value)) + 
  facet_wrap(~ variable, scales = "free", ncol=2) +
  geom_bar(fill = "lightcoral", color="black")+
  theme_minimal()+
  labs(x = "Factors", y = "Levels", title = "Barplot of Categorical X Factors") +
  theme(plot.title = element_text(hjust = 0.5))

#bivariate - categorical vs categorical 
#promomethod vs exited 
ggplot(catchurn2, aes(x = Promomethod, fill = Exited)) +
  geom_bar(position = "stack") +
  labs(x = "Promomethod",
       y = "Count",
       title = "Relationship between Promomethod and Exited") +
  theme_minimal()

#subscription channel vs exited 
ggplot(catchurn2, aes(x = Subscriptionchannel, fill = Exited)) +
  geom_bar(position = "stack") +
  labs(x = "Subscription Channel",
       y = "Count",
       title = "Relationship between Subscription Channel and Exited") +
  theme_minimal()

#language_provided vs exited 
ggplot(catchurn2, aes(x = Language_provided, fill = Exited)) +
  geom_bar(position = "stack") +
  labs(x = "Language Provided",
       y = "Count",
       title = "Relationship between Language Provided and Exited") +
  theme_minimal()

#language_preferred vs exited 
ggplot(catchurn2, aes(x = Language_preferred, fill = Exited)) +
  geom_bar(position = "stack") +
  labs(x = "Language Preferred",
       y = "Count",
       title = "Relationship between Language Preferred and Exited") +
  theme_minimal()

#variant vs exited 
ggplot(catchurn2, aes(x = Variant, fill = Exited)) +
  geom_bar(position = "stack") +
  labs(x = "Variant",
       y = "Count",
       title = "Relationship between Variant and Exited") +
  theme_minimal()


#--------------------------------------CART Model 1---------------------------------------------------------------
library(data.table)
library(rpart)
library(rpart.plot)
library(caTools)

#10-fold 
set.seed(2020)  
CARTmodel1<- rpart(Exited ~ HasCrCard + IsActiveMember + Age + CreditScore 
                   + Gender + Tenure + EstimatedSalary + Balance + Geography + NumOfProducts, data = churndata, method = 'class',
                   control = rpart.control(minsplit = 20, cp = 0))

rpart.plot(CARTmodel1, nn= T, main = "Maximal Tree in BankChurn.csv")
print(CARTmodel1)
CARTmodel1$variable.importance

#Taking the CP between 5th and 6th tree
printcp(CARTmodel1)
plotcp(CARTmodel1, main = "Subtrees in BankChurn.csv")
sqrt(6.2592e-03*4.7455e-03)
cp1 <- 0.005450049

#Pruning + Printing the model 
PrunedCARTmodel1 <- prune(CARTmodel1, cp = cp1)
printcp(PrunedCARTmodel1 )
rpart.plot(PrunedCARTmodel1 , nn= T, main = "Pruned Tree with cp = 0.003673659")
print(PrunedCARTmodel1)
PrunedCARTmodel1$variable.importance

#Finding out the accuracy of CART model
set.seed(2020)
trainC1 <- sample.split(Y = churndata$Exited, SplitRatio = 0.7)
trainsetC1 <- subset(churndata,trainC1==T)
testsetC1 <- subset(churndata,trainC1==F)

predictedC1train <- predict(PrunedCARTmodel1 , newdata = trainsetC1, type='class')
table(trainsetC1$Exited, predictedC1train)
accuracyC1train <- mean(predictedC1train == trainsetC1$Exited)
accuracyC1train

predictedC1test <- predict(PrunedCARTmodel1 , newdata = testsetC1, type='class')
table(testsetC1$Exited, predictedC1test)
accuracyC1test <- mean(predictedC1test == testsetC1$Exited)
accuracyC1test

#----------------------------------Logistic Regression Model 1------------------------------------------------------
library(caTools)
library(data.table)
library(car)
setwd("C:/Users/shann/Desktop/Year 2 Sem 1/BC2406    Analytics l - Visual&Predictive Techniques/Project")
churndata <- fread("Churn_Modelling.csv", stringsAsFactors = T)
churndata$HasCrCard <- factor(churndata$HasCrCard)
churndata$IsActiveMember <- factor(churndata$IsActiveMember)

#Report summary statistics for each variable.
summary(churndata)

#Initial model
lg1<- glm(Exited ~ CreditScore+Gender+Geography+Age+Tenure
          +NumOfProducts+HasCrCard+IsActiveMember
          +EstimatedSalary+Balance, family = binomial, data = churndata)

summary(lg1)

#Check for multicollinearity
vif(lg1)

# Use OR CI to conclude on statistical significance of X variables.
#statiscally significant if CI excludes 1
OR.CI <- exp(confint(lg1))
OR.CI

#70:30 Train-test split
set.seed(2020)
trainLG1 <- sample.split(Y = churndata$Exited, SplitRatio = 0.7)
trainsetLG1 <- subset(churndata, trainLG1 == T)
testsetLG1 <- subset(churndata, trainLG1 == F)

#Train set
lg1final<- glm(Exited ~ CreditScore+Gender+Geography+Age
               +NumOfProducts+IsActiveMember
               +Balance+Tenure , family = binomial, data = trainsetLG1)

threshold1 <- 0.5
probLG1train <- predict(lg1final, type = 'response')
predictedLG1train<- ifelse(probLG1train > threshold1, "1", "0")
tableLG1train <- table(Trainset.Actual = trainsetLG1$Exited, predictedLG1train, deparse.level = 2)
tableLG1train
round(prop.table(tableLG1train),3)

#Accuracy of train set
accuracyLG1train <- mean(predictedLG1train == trainsetLG1$Exited)
accuracyLG1train 

#Test set
probLG1test <- predict(lg1final, newdata = testsetLG1, type = 'response')
predictedLG1test <- ifelse(probLG1test > threshold1, "1", "0")
tableLG1test <- table(Testset.Actual = testsetLG1$Exited, predictedLG1test, deparse.level = 2)
tableLG1test
round(prop.table(tableLG1test), 3)

#Accuracy of test set
accuracyLG1test <- mean(predictedLG1test == testsetLG1$Exited)
accuracyLG1test

#--------------------------------------CART Model 2---------------------------------------------------------------

#Train model on datasets of those who did not leave
churndata2exit0<-subset(churndata2, Exited == 0)

#10fold
set.seed(25)
CARTmodel2<- rpart(Subscriptionchannel ~ 
                     CreditScore+Geography+Gender+Age+Tenure
                   +NumOfProducts+HasCrCard+IsActiveMember
                   +EstimatedSalary+Balance+Promomethod+Language_provided+Language_preferred, data = churndata2exit0, method = 'class',
                   control = rpart.control(minsplit = 20, cp = 0))
rpart.plot(CARTmodel2, nn= T, main = "Maximal Tree in churnbank2.csv")

#Pruning tree between 4th and 5th CP
print(CARTmodel2)
printcp(CARTmodel2)
plotcp(CARTmodel2, main = "Subtrees in churnbank2.csv")
sqrt(0.00282752*0.00419416)
cp2 <- 0.0034437

#PrunedCartModel2
PrunedCARTmodel2 <- prune(CARTmodel2, cp = cp2)
printcp(PrunedCARTmodel2)
rpart.plot(PrunedCARTmodel2, nn= T, main = "Predicting Subscription Channel")
PrunedCARTmodel2$variable.importance
print(PrunedCARTmodel2)

#Finding the accuracy of CARTmodel2
set.seed(29)
trainC2 <- sample.split(Y = churndata2exit0$Subscriptionchannel, SplitRatio = 0.7)
trainsetC2 <- subset(churndata2exit0,trainC2==T)
testsetC2 <- subset(churndata2exit0,trainC2==F)

predictedC2train <- predict(PrunedCARTmodel2, newdata = trainsetC2, type='class')
table(trainsetC2$Subscriptionchannel, predictedC2train)
accuracyC2train <- mean(predictedC2train == trainsetC2$Subscriptionchannel)
accuracyC2train

predictedC2test <- predict(PrunedCARTmodel2, newdata = testsetC2, type='class')
table(testsetC2$Subscriptionchannel, predictedC2test)
accuracyC2test <- mean(predictedC2test == testsetC2$Subscriptionchannel)
accuracyC2test

#--------------------------------Logistic Regression Model 2----------------------------------------------

library(nnet)

#Predict model on dataset of those who did not leave
churndata2exit0<-subset(churndata2, Exited == 0)

# Report summary statistics for each variable.
summary(churndata2exit0)

# Check and set baseline to Rating = Neutral
levels(churndata2exit0$Subscriptionchannel)  # Default baseline is Email

# Logistic Regression with 5 categorical Y values.
lg2<- multinom(Subscriptionchannel ~ CreditScore+Geography+Gender+Age+Tenure
               +NumOfProducts+HasCrCard+IsActiveMember
               +EstimatedSalary+Balance+Promomethod
               +Language_provided+Language_preferred, data = churndata2exit0)
summary(lg2)


# Use OR CI to conclude on statistical significance of X variables.
#statiscally significant if CI excludes 1
OR.CI <- exp(confint(lg2))
OR.CI


# Calculate p-values using Z test.
# p-value<0.05 --> statistically significant 
z <- summary(lg2)$coefficients/summary(lg2)$standard.errors
pvalue <- (1 - pnorm(abs(z), 0, 1))*2  # 2-tailed test p-values
pvalue

#70:30 Train-test split
set.seed(2)
trainLG2 <- sample.split(Y = churndata2exit0$Subscriptionchannel, SplitRatio = 0.7)
trainsetLG2 <- subset(churndata2exit0, trainLG2 == T)
testsetLG2 <- subset(churndata2exit0, trainLG2 == F)

#Train set 
lg2final<- multinom(Subscriptionchannel ~ CreditScore+Geography+Gender+Age+Tenure
                    +NumOfProducts+HasCrCard+IsActiveMember
                    +Promomethod+Language_provided+Language_preferred, data = trainsetLG2)

probLG2train<- predict(lg2final, type = 'prob')
probLG2train

predictedLG2train <- predict(lg2final)
predictedLG2train

tableLG2train <- table(Trainset.Actuals = trainsetLG2$Subscriptionchannel, Model.Predict = predictedLG2train, deparse.level = 2)
tableLG2train

# Accuracy for train set
accuracyLG2train <- mean(predictedLG2train == trainsetLG2$Subscriptionchannel)
accuracyLG2train

#Test set
probLG2test <- predict(lg2final, newdata = testsetLG2, type = 'prob')
probLG2test 

predictedLG2test <-predict(lg2final, newdata = testsetLG2)
predictedLG2test

tableLG2test <- table(Testset.Actuals = testsetLG2$Subscriptionchannel, Model.Predict = predictedLG2test, deparse.level = 2)
tableLG2test

# Accuracy for test set
accuracyLG2test <-mean(predictedLG2test == testsetLG2$Subscriptionchannel)
accuracyLG2test

#--------------------------------------CART Model 3---------------------------------------------------------------

set.seed(25)
CARTmodel3 <- rpart(Variant ~ CreditScore+Geography+Gender+Age+Tenure
                    +NumOfProducts+HasCrCard+IsActiveMember
                    +EstimatedSalary+Balance+Promomethod+Language_provided+Language_preferred, data = churndata2exit0, method = 'class',
                    control = rpart.control(minsplit = 20, cp = 0))
rpart.plot(CARTmodel3, nn= T, main = "Maximal Tree in bankchurn2.csv")
print(CARTmodel3)
printcp(CARTmodel3)
plotcp(CARTmodel3, main = "Subtrees in bankchurn2.csv")
sqrt(0.03301887*0.00884434)
cp3 <- 0.01708889

PrunedCARTmodel3 <- prune(CARTmodel3, cp = cp3)
printcp(PrunedCARTmodel3)
print(PrunedCARTmodel3)
rpart.plot(PrunedCARTmodel3, nn= T, main = "Pruned Tree with cp = 0.017")

PrunedCARTmodel3$variable.importance
set.seed(20)
trainC3 <- sample.split(Y = churndata2exit0$Variant, SplitRatio = 0.7)
trainsetC3 <- subset(churndata2exit0,trainC3==T)
testsetC3 <- subset(churndata2exit0,trainC3==F)

predictedC3train <- predict(PrunedCARTmodel3, newdata = trainsetC3, type='class')
table(trainsetC3$Variant, predictedC3train)
accuracyC3train <- mean(predictedC3train == trainsetC3$Variant)
accuracyC3train

predictedC3test <- predict(PrunedCARTmodel3, newdata = testsetC3, type='class')
table(testsetC3$Variant, predictedC3test)
accuracyC3test <- mean(predictedC3test == testsetC3$Variant)
accuracyC3test

#--------------------------------Logistic Regression Model 3----------------------------------------------

#Predict model on datasets of those who did not leave
churndata2exit0<-subset(churndata2, Exited == 0)

#Initial model
lg3<- glm(Variant~ CreditScore+Geography+Gender+Age+Tenure
          +NumOfProducts+HasCrCard+IsActiveMember
          +Promomethod+Language_provided+Language_preferred, family = binomial, data = churndata2exit0)

summary(lg3)

#Check for multicollinearity
vif(lg3)

#Odds Ratio Confidence Interval
OR.CI <- exp(confint(lg3))
OR.CI

#70:30 Train-test split
set.seed(2)
trainLG3 <- sample.split(Y = churndata2exit0$Variant, SplitRatio = 0.7)
trainsetLG3 <- subset(churndata2exit0, trainLG3 == T)
testsetLG3 <- subset(churndata2exit0, trainLG3 == F)

#Train set
lg3final<- glm(Variant ~ Gender+Age+Promomethod
               +HasCrCard+EstimatedSalary , family = binomial, data = trainsetLG3)


threshold1 <- 0.5
probLG3train <- predict(lg3final, type = 'response')
predictedLG3train <- ifelse(probLG3train > threshold1, "1", "0")
tableLG3train<- table(Trainset.Actual = trainsetLG3$Variant, predictedLG3train, deparse.level = 2)
tableLG3train
round(prop.table(tableLG3train),3)

#Accuracy of train set
accuracyLG3train <- mean(predictedLG3train == trainsetLG3$Variant)
accuracyLG3train

#Test set
probLG3test <- predict(lg3final, newdata = testsetLG3, type = 'response')
predictedLG3test <- ifelse(probLG3test > threshold1, "1", "0")
tableLG3test<- table(Testset.Actual = testsetLG3$Variant, predictedLG3test, deparse.level = 2)
tableLG3test
round(prop.table(tableLG3test), 3)

#Accuracy of test set
accuracyLG3test <- mean(predictedLG3test == testsetLG3$Variant)
accuracyLG3test

#save models for interface
save(PrunedCARTmodel1, file="CARTmodel1.rda")
save(PrunedCARTmodel2, file="CARTmodel2.rda")
save(PrunedCARTmodel3, file="CARTmodel3.rda")

