
setwd("C:/Users/Ryoba Nyaisara/Desktop/data science/project_r")
getwd()

df<-read.csv("EmployeeData.csv")
View(df)
summary(df)

# Check the Null values from the Attributes
sum(is.na(df))


# Takes Subset data from the data set for easiness of data analyze

set.seed(100)
subsetIndex<-sample(1:nrow(df),0.2*nrow(df))
subset<-df[subsetIndex,]

#Attributes of Employee that left from the company

Employee_left<-subset(subset,subset$left=='1')
summary(Employee_left)



#Attributes of Employee that did not left from the company
Employee_not_left<-subset(subset,subset$left=='0')
summary(Employee_not_left)


# Different characteristics of an histogram from dataset to Employee left

hist(Employee_left$satisfaction_level)
hist(subset$satisfaction_level)

---------------------------------------------------------------------------------------------
# Correlation of the subset data from Attributes.
# here we are looking the correlation of last_evalution and satisfication level

attach(subset)
plot(last_evaluation,satisfaction_level,main="satisfaction_level and last_evaluation",
     colors(),
     xlab="last_evaluation",ylab="satisfaction_level")
------------------------------------------------------------------------------------

# here we are looking the correlation salary and time spending to company
attach(subset)

plot(factor(salary),time_spend_company,main="Salary and Time spend to company",
     xlab="Salary",ylab="Time spend")
-------------------------------------------------------------------------------------------------------------

# here we are looking the correlation avarega monthly hours and time spending 
attach(subset)
plot(average_montly_hours,time_spend_company,main = "Avarage hours Monthly and Time Spending to company",
     xlab = "Average_Monthly_hours",ylab = "Time spend to company")

-----------------------------------------------------------------------------------------------------------

# here we are looking the correlation  salary and satifaction level
attach(subset)
plot(factor(salary),satisfaction_level,xlim = range(0:4),main = "Salary and Satisfaction level",
     xlab="Salary",ylab = "satisfaction_level")

----------------------------------------------------------------------------------------------------------------------------------

# Analyze  the  department  wise  turnouts  and  find  out  the  percentage of  employees leaving from each department

depatmentdata<-aggregate(left~department,subset,mean)

View(depatmentdata)


set.seed(100)
trainingRowindex<-sample(1:nrow(subset),0.7*nrow(subset))
training<-subset[trainingRowindex,]
test<-subset[-trainingRowindex,]

summary(training)

-------------------------------------------------------------------------------------------------

#Linear Regression Model

set.seed(100)
trainingRowindex<-sample(1:nrow(subset),0.7*nrow(subset))
training<-subset[trainingRowindex,]
test<-subset[-trainingRowindex,]

# Build the model on training data
m<-lm(left~satisfaction_level + last_evaluation + number_project + average_montly_hours + time_spend_company
      + Work_accident + promotion_last_5years + department + salary,data = training)
      

m<-lm(left~satisfaction_level  + number_project + average_montly_hours + time_spend_company
      + Work_accident +  salary,data = training)

summary(m)
mad(residuals((m)))


pred<-predict(m,test)
head(pred)
head(test$left)

predIf<-ifelse(pred > 0.5,1,0)
misClasificError<-mean(predIf !=test$left)
print(paste('Accuracy',1-misClasificError))
# The accuracy for this model for employee left the company is 73.86%

-------------------------------------------------------------------------------------------
# Logistic Regression Model


set.seed(100)
trainingRowindex<-sample(1:nrow(subset),0.7*nrow(subset))
training<-subset[trainingRowindex,]
test<-subset[-trainingRowindex,]


# Build the model on training data
m2=glm(left~satisfaction_level + last_evaluation + number_project + average_montly_hours + time_spend_company
       + Work_accident + promotion_last_5years + department + salary,
       family=binomial(link = 'logit'),data = training)
summary(m2)


m2=glm(left~satisfaction_level+ number_project + average_montly_hours + time_spend_company
       + Work_accident + salary,
       family=binomial(link = 'logit'),data = training)

summary(m2)

anova(m2,test = "Chisq")


pred2<-predict(m2,test,type = 'response')
pred2 <-ifelse(pred2>0.5,1,0)
head(pred2)
head(test$left)
misClasificError<- mean(pred2 != test$left )
print(paste('Accuracy',1-misClasificError))

# The Accuracy is 79.44% from the model


-------------------------------------------------------------------------


#Decision tree

   
install.packages("rpart")
library(rpart)
set.seed(100)
trainingRowindex<-sample(1:nrow(subset),0.7*nrow(subset))
training<-subset[trainingRowindex,]
test<-subset[-trainingRowindex,]

m3<-rpart(left ~. ,data = training,method = "class")
summary(m3)

print(m3)
plot(m3, margin=0.1)
text(m3, use.n = TRUE,pretty = TRUE, cex=0.8)

predvalues<-predict(m3,newdata = test)
actualvalue<-predict(m3,newdata = training)
table(actualvalue)
predvalues


confusionMatrix(test(predvalues, test$left))

confusionMatrix(table(predvalues, test$left))
class(predvalues)
class(test$left)
table(predvalues)
table(test$left)


a <- cbind.data.frame(predvalues, test$left)
write.csv(a, "a.csv")

-----------------------------------------------------------------------------------

   
# Naïve  Bayes model

install.packages("caret")
library(caret)
set.seed(100)
trainingRowindex<-sample(1:nrow(subset),0.7*nrow(subset))
training<-subset[trainingRowindex,]
test<-subset[-trainingRowindex,]

m4<-naiveBayes(left ~satisfaction_level + last_evaluation + number_project + average_montly_hours + time_spend_company
                  + Work_accident + promotion_last_5years + department + salary  ,data = training)
m4
summary(m4)

predvalues<-predict(m4,newdata = test,type = "class")
predvalues

confusionMatrix(table(predvalues, test$left))


---------------------------------------------------------------------------------------------

# Random forest

   
install.packages("e1071")
library(e1071)
library(randomForest)

set.seed(100)
trainingRowindex<-sample(1:nrow(subset),0.7*nrow(subset))
training<-subset[trainingRowindex,]
test<-subset[-trainingRowindex,]


a=c()
i=3
for (i in 3:8) {
   m5 <- randomForest(left~. ,data = trainset, ntree = 100, mtry = i, importance = TRUE)
   predvalues <- predict(m5, test, type = "class")
   a[i-2] = mean(predvalues == test$left)
}

a

plot(3:8,a)

confusionMatrix(table(predvalues, test$left))

------------------------------------------------------------------------------------

#Build support vector machines


set.seed(100)
trainingRowindex<-sample(1:nrow(subset),0.7*nrow(subset))
training<-subset[trainingRowindex,]
test<-subset[-trainingRowindex,]


m6<-svm(left~. ,data = training, kernel = "linear", cost = 0.1)
summary(m6)

plot(m6, training,Petal.Width ~ Petal.Length)
plot(m6, training,Sepal.Width ~ Sepal.Length)


predvalues<-predict(m6,newdata = test,type = "class")
predvalues

confusionMatrix(table(predvalues, test$left))
