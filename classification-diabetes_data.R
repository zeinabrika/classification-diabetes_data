#load up the .csv data and explore in Rstudio,
diabetes <- read.csv ("diabetes.csv")
names(diabetes)
head(diabetes)
tail(diabetes)
summary(diabetes)
str(diabetes)

#test and training the data
set.seed(1234)
pd <- sample(2, nrow(diabetes),replace=TRUE, prob=c(0.8,0.2))
pd
train <- diabetes[pd==1,]
validate <- diabetes[pd==2,]
dim(train) # Retrieve the dimension of the train data set
dim(validate) # Retrieve the dimension of the validate data set

#missing value
sapply(diabetes, function(x) sum(is.na(x)))

#packeages and library
install.packages("party")
library(party) # activate "party" package
install.packages("grid")
install.packages("mvtnorm")
install.packages("modeltools")

diabetes$Outcome <- as.factor(diabetes$Outcome)
diabetes_tree <- ctree(Outcome ~  Pregnancies + Glucose + 
                     BloodPressure +Insulin+SkinThickness+DiabetesPedigreeFunction+BMI,
                     data = train)
diabetes_tree
print(diabetes_tree) # Draw the tree
plot(diabetes_tree) # Plot the tree
plot(diabetes_tree, type="simple")

#install packages for rpart plot tree
install.packages("rpart.plot")
library(rpart.plot) # plotting trees
install.packages("NMF")
library(caret)
install.packages("bigmemory")
library(rpart) #for trees

tree2 <- rpart(Outcome ~ Pregnancies + Glucose + 
                 BloodPressure +Insulin+SkinThickness+DiabetesPedigreeFunction+BMI,
               data = diabetes, method="class")
summary(tree2)
rpart.plot(tree2)

pred1 <- predict(tree2,newdata=diabetes,type="class")
table(diabetes$Outcome,pred1)

predict(diabetes_tree)
tab <- table(predict(diabetes_tree), train$Outcome)
print(tab)

sum(diag(tab))/sum(tab)
1-sum(diag(tab))/sum(tab)

test_diabetes <- table(predict(diabetes_tree, newdata= validate), validate$Outcome)
print(test_diabetes)

sum(diag(test_diabetes))/sum(test_diabetes)

1-sum(diag(tab))/sum(tab)

#Confusion Matrix for Decision Tree
pred_tree <- predict(diabetes_tree, diabetes)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, 
                                                   Actual = diabetes$Outcome)

#install "neuralnet" package. Ignore this line if "neuralnet"
#was already installed.
install.packages("neuralnet")
#activate "neuralnet" package
library(neuralnet)
  
set.seed(1234)
#sample function can be used to return a random permutation of a vector.
pd <- sample(2, nrow(diabetes), replace=TRUE, prob=c(0.6,0.4))
trainingdata <- diabetes[pd==1,]
testdata <- diabetes[pd==2,]

diabetes <- neuralnet(default10yr~
  Outcome +Pregnancies + Glucose + BloodPressure +Insulin+SkinThickness+
  DiabetesPedigreeFunction+BMI+Age, 
  trainingdata,
  hidden=3,
  lifesign="minimal",
  linear.output=FALSE,
  threshold=0.1)

plot(diabetes, rep = "best")

dia_test <- subset(testdata, select = c("Pregnancies", "Glucose", "BloodPressure", "Insulin",
                                     "SkinThickness", "DiabetesPedigreeFunction",  "BMI", "Age"))
head(dia_test) 
  
  
dia_results <- compute(diabetes, dia_test)
names(dia_results)
str(dia_results)

dia_results2 <-data.frame(actual = testdata$Outcome,
                     prediction = dia_results$net.result)
head(dia_results2)
tail(dia_results2)
dia_results2[90:105, ] # show part of the result

dia_results2$prediction <- sapply( dia_results$net.result,
                              round,digits=0)
dia_results2[90:105, ] 

confusionmatrix <- table(testdata$Outcome,dia_results2$prediction)
print(confusionmatrix) 

sum(diag(confusionmatrix))/sum(confusionmatrix) 
1-sum(diag(confusionmatrix))/sum(confusionmatrix)













