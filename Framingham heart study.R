library(mosaic)
library(ggplot2)
library(stats)
library(dplyr)
library(caTools)
library(ROCR)

#Loading in the data
framingham = read.csv("~/ADM/midterm/framingham.csv" )
View(framingham)

ggplot(data = framingham) + 
  geom_point(aes(age, heartRate))

summary(framingham)

#Creating new dataset with no missing values for tables
clean_framingham = na.omit(framingham)
View(clean_framingham)

summary(clean_framingham)

##Creating categorical variables
clean_framingham$currentSmoker = ifelse(clean_framingham$currentSmoker==1, "Smoker", "Nonsmoker")
clean_framingham$diabetes = ifelse(clean_framingham$diabetes==1, "Yes", "No")
clean_framingham$TenYearCHD = ifelse(clean_framingham$TenYearCHD==1, "CHD", "No")

###Prop tables
prop.table(table(clean_framingham$currentSmoker, clean_framingham$diabetes),1)
#More current nonsmokers have diabetes than current smokers. This could likely be
#because a diabetes diagnosis urged them to quit smoking.

prop.table(table(clean_framingham$TenYearCHD, clean_framingham$diabetes),1)
#Over three times as many subjects with diabetes has a ten year CHD risk than those
#without diabetes.

prop.table(table(clean_framingham$TenYearCHD, clean_framingham$currentSmoker),1)
#There is not a large difference in proportions of those of smoke or do not smoke
#and their risk of CHD.

#Scatter plots of various variables
ggplot(data = clean_framingham) + 
  geom_point(aes(age, heartRate))

ggplot(data = clean_framingham) + 
  geom_point(aes(BMI, heartRate, color = currentSmoker))

ggplot(data = clean_framingham) + 
  geom_point(aes(BMI, diaBP, color = currentSmoker))

ggplot(data = clean_framingham) + 
  geom_point(aes(TenYearCHD, diaBP, color = currentSmoker))

##Creating dataset with no NAs and categoricals as integers
clean_framingham = na.omit(framingham)

sample = sample.split(clean_framingham$TenYearCHD, SplitRatio = 0.7)

train = subset(clean_framingham, sample == TRUE)
test = subset(clean_framingham, sample == FALSE)


model = glm(TenYearCHD ~ ., data = train, family = binomial(link = "logit"))
summary(model)

model1 = glm(TenYearCHD ~ male + age + cigsPerDay + sysBP + glucose,
             data = train, family = binomial(link = "logit"))
summary(model1)

predictTest = predict(model1, type = "response", newdata = test)

table(test$TenYearCHD, predictTest > 0.5)

accuracy = (921+16)/(921+9+151+16); accuracy

predictTest = predict(model, type = "response", newdata = test)

table(test$TenYearCHD, predictTest > 0.5)

accuracy = (919+15)/(919+11+152+16); accuracy

ROCRpred = prediction(predictTest,test$TenYearCHD)
ROCRpref = performance(ROCRpred,"tpr","fpr")
plot(ROCRpref,colorize=TRUE) #plotting ROC curve
as.numeric(performance(ROCRpred,"auc")@y.values) #Area Under Curve

#Random forest classification
rf_framingham = clean_framingham

#rf_framingham$TenYearCHD = ifelse(rf_framingham$TenYearCHD == 1, "CHD", "No CHD")

rf_framingham$TenYearCHD = factor(rf_framingham$TenYearCHD, labels = c("No CHD", "CHD"))

#head(rf_framingham); class(rf_framingham$TenYearCHD)

train = subset(rf_framingham, sample == TRUE)
test = subset(rf_framingham, sample == FALSE)

framingham.rf = randomForest(TenYearCHD ~ male + age + cigsPerDay + sysBP
                             + glucose, data = train, ntree=250)
summary(framingham.rf)

framingham.rf1 = randomForest(TenYearCHD ~ ., data = train, ntree=250)
summary(framingham.rf1)

rf_predict = predict(framingham.rf, type = "response", newdata = test)

table(observed = test$TenYearCHD, predicted = rf_predict)

accuracy = (900+16)/(900+30+151+16); accuracy


