ss <- read.csv("ss.csv")
ssNew <- ConvertAllVars(ss) # This is my own code that converts all categorical variable to numerical ones
# and keeps a record of the categories that were replaced
ssNew1 <- ssNew$df

tapply(ss$BMI.Category,(ss$Occupation), mean)

subset1 = subset(ss, ss$Occupation == "Engineer")   #Engineers are normal weight and have good sleep
subset2 = subset(ss, ss$Occupation == "Scientist") #Scientist are all overweight, female, and don't get good sleep
subset3 = subset(ss, ss$Occupation == "Software Engineer") #Not much of a pattern here
subset4 = subset(ss, ss$Occupation == "Manager")  #only 1 observation
subset5 = subset(ss, ss$Occupation == "Nurse")  #almost all are overweight, bad sleep
subset17 = subset(ss, ss$Occupation == "Salesperson")
subset6 = subset(ss, ss$BMI.Category == "Overweight")   # there is a pattern of bad sleep here
subset7 = subset(ss, ss$BMI.Category == "Obese")  #Every single person has some type of sleeping disorder
subset8 = subset(ss, ss$BMI.Category == "Normal" | ss$BMI.Category == "Normal Weight")
subset10 = subset(ss, ss$BMI.Category == "Overweight" |ss$BMI.Category == "Obese")   # there is a pattern of bad sleep here

tapply(ss$Quality.of.Sleep, ss$BMI.Category, mean)  # clearly a trend of bad sleep the more weight you have

tapply(ss$Sleep.Duration, ss$BMI.Category, mean)

tapply(ss$Stress.Level, ss$BMI.Category, mean) #not much of a difference

tapply(ss$Physical.Activity.Level, ss$BMI.Category, mean) # surprisingly there is not much correlation at all

tapply(ss$Stress.Level, ss$Occupation, mean) #Engineers have by far the lowest levels of stress

tapply(ss$Quality.of.Sleep, ss$Occupation, mean) # Engineer has good sleep but sales has bad


# If you want a bad lifestyle, go into sales:

subset17 = subset(ss, ss$Occupation == "Salesperson" | ss$Occupation == "Sales Representative")   
subset14 = subset(ss, !(ss$Occupation == "Salesperson" | ss$Occupation == "Sales Representative")) #everyone not in sales

mean(subset14$Stress.Level) # 5.2 = stress level of non sales
mean(subset17$Stress.Level) # 7.1 = stress level of sales
(7.1-5.2)/5.2 # People in Sales have 37% more stress than those in other professions

hist(subset14$Quality.of.Sleep, xlab = "Quality of Sleep for people not in Sales(hrs)")
hist(subset17$Quality.of.Sleep, xlab = "Quality of Sleep for people in Sales(hrs)")

mean(subset14$Quality.of.Sleep) # 7.5 = quality of sleep of non sales
mean(subset17$Quality.of.Sleep) # 5.9 = quality of sleep of sales
(7.5-5.9)/5.9 # People not in Sales have 27% better quality of sleep

subset15 = subset(subset17, subset5$BMI.Category == "Normal" | subset5$BMI.Category == "Normal Weight")
nrow(subset15)/nrow(subset17) # percent of sales who are normal weight
mean(subset17$Sleep.Duration) # amount of sleep sales get

subset16 = subset(subset14, subset14$BMI.Category == "Normal" | subset14$BMI.Category == "Normal Weight")
nrow(subset16)/nrow(subset14) # percent of all none sales who are normal weight
mean(subset14$Sleep.Duration) # amount of sleep non sales get

(1-0.206)/(1-0.637) = 2.2 # Sales people are 2.2x more likely to be overweight/obese than those in other professions

7.2-6.4 = 0.8 
0.8*60 = 48(min) # People not in sales get over 45 minutes of sleep more than those in sales


# If you want a good lifestyle, become an engineer:
  
subset1 = subset(ss, ss$Occupation == "Engineer")   #Engineers are normal weight and have good sleep
subset11 = subset(ss, !(ss$Occupation == "Engineer")) #everyone other than engineer
mean(subset11$Stress.Level) # 5.7 = stress level of non engineers
mean(subset1$Stress.Level) # 3.9 = stress level of engineers
(5.7-3.9)/3.9 # People who are not engineers have 46% more stress than engineer

subset12 = subset(subset1, subset1$BMI.Category == "Normal" | subset1$BMI.Category == "Normal Weight")
nrow(subset12)/nrow(subset1) # percent of engineers who are normal weight
mean(subset1$Sleep.Duration) # amount of sleep engineers get

subset13 = subset(subset11, subset11$BMI.Category == "Normal" | subset11$BMI.Category == "Normal Weight")
nrow(subset13)/nrow(subset11) # percent of all except engineers who are normal weight
mean(subset11$Sleep.Duration) # amount of sleep non engineers get


# Sleep and Weight relationship

tapply(ss$Quality.of.Sleep, ss$Stress.Level, mean) # shows a strong negative relationship between stress and sleep

tapply(ss$Quality.of.Sleep, ss$BMI.Category, mean) # the heavier you are, the worse your sleep is

hist(subset10$Sleep.Duration, xlab = "Sleep Duration of Overweight/Obese people (hrs)") # clearly bad

hist(subset8$Sleep.Duration, xlab = "Sleep Duration of normal weight people (hrs)")

nrow(subset6)  # only 19/147 overweight people had no sleeping disorder

nrow(subset7) # 0/10 obese people had no sleeping disorder

subset9 = subset(ssNew1, ssNew1$BMI.Category == 4 |ssNew1$BMI.Category == 2)

hist(subset9$Sleep.Disorder) # this shows that 15/215 normal people have a sleeping disorder

147+10-19 # total overweight and obese people in dataset minus 19 overweight people who don't have a sleeping disorder
(138/157)/(15/215) # odds that an overweight/obese person has a sleeping disorder as compared to a normal person
# overweight person is 12.6 times more likely to have a sleeping disorder than a normal weighted person




### Categorical Predictor for BMI Category

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

multi.class.model <- rpart(BMI.Category ~ ., data = ss, cp = 0.0001) # Builds a simple categorical decision tree
rpart.plot(multi.class.model)  # Plots the tree for visualization purposes

# Calculating accuracy
ssData = randomSplitDataFrame(ss, split_ratio = 0.9) # my own method that makes a random split in the data based upon a split ration
trainData = ssData$train_data
testData = ssData$test_data
multi.class.model <- rpart(BMI.Category ~ ., data = trainData, cp = 0.0001) # Builds a simple categorical decision tree on trainData only
BMIPredictions <- predict(multi.class.model, newdata = testData)

predictions = list()
for (j in 1:nrow(BMIPredictions)){
  pred = ""
  maxProb = 0
  for (i in range(1:4)){
    if (!(is.na(BMIPredictions[j,i])) && !(is.null(BMIPredictions[j,i]))){
      # print(BMIPredictions[j,i])
      if (BMIPredictions[j,i] > maxProb){
        maxProb = BMIPredictions[j,i]
        pred = colnames(BMIPredictions)[i]
      }
    }
  }
  predictions = c(predictions, pred)
}

actualValues = testData$BMI.Category
accuracy = list()
for (i in seq_along(predictions)){
  if (predictions[[i]] == actualValues[i]){
    accuracy = c(accuracy, 1)
  }
  else{
    accuracy = c(accuracy, 0)
  }
}

BMIAccuracy = (mean(unlist(accuracy)))
BMIAccuracy #  ~ 0.95
# The model is about 95% accurate
