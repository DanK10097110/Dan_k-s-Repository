# The purpose of this code is to intake a dataframe with numerical values only 
# and output the best possible multivariate linear regression. It can also output
# a list of variables ranked by their significance in terms of their correlation
# with the output variable. 

# Note: The user will still have to intelligently create subsets of the data to try
# and find divided patterns. It is possible to automate this but this would be extremely
# computationally heavy, so on large datasets it may not be optimal.

library(tidyverse)
library(sandwich)
library(lmtest)

df <- read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/Movies2022F-4-new.csv");

regModel = LevelILinearReg(df, 3)
regModel

models = LevelIILinearReg(df, 3, 6)
models

LevelILinearReg <- function(df, n){
  # n is target column
  
  data = ConvertAllVars(df)
  df = data$df
  if (!is.null(dim(df))){
    SigVals = FeatureCollection(df, n)
    targetName = names(df)[n]
    bestFeatures = BestRegFeatures(SigVals, df, targetName)
    print(bestFeatures)
    regModel = CreateLinearReg(df, n, bestFeatures)
    return(regModel)
  }
  else{
    print("data was not converted well :(")
    return(NULL)
  }
}

subs1 <- list()
subs2 <- list()
featVals <- list()
newData <- list()
newCats <- list()
types1 <- list()

LevelIILinearReg <- function(df, n, a){
  # This function will create a piece-wise linear regression that is seperated 
  # by the subsetted column
  # n is target column
  # a is column being subsetted
  data = ConvertAllVars(df)
  newData <<- data
  regModels = list()
  if (!is.null(data)){
    df = data$df
    targetName = names(df)[n]
    categories = data$categories
    newCats <<- categories
    categories = categories[[a]]
    varTypes = data$types
    types1 <<- varTypes
    subs = AutoSubset(df, a, categories, varTypes[a])
    subs2 <<- subs
    if (!(is.null(subs))){
      subsets = subs[[1]]
      subs1 <<- subsets
      smlNumsOfSubset = subs[[2]]
      bigNumsOfSubset = subs[[3]]
      # subNames = list()
      FeatureVals = list()
      count = 0
      print("Entering for loop in LevelII")
      for (sub in subsets){
        Values <- FeatureCollection(sub, n)
        if (!is.null(Values)){
          FeatureVals = c(FeatureVals, Values)
        }
        else{
          print("Values is null in LevelII")
        }
        # count = count + 1
        # newName = cat("This is the ", count, "th subset for column", a, " with num of df rows being: ", nrow(df))
        # subNames = c(subnames, newName)
      }
      featVals <<- FeatureVals
      for (i in seq_along(FeatureVals)){
        features = BestRegFeatures(FeatureVals[[,i]], subsets[[,i]], targetName)
        newReg = CreateLinearReg(sub, n, FeatureVals[,i])
        regModels = c(regModels, newReg)
        if(is.null(features) || is.null(regModels) || is.null(newReg)){
          print("Something is not right in second for loop of LevelII")
        }
      
        # adjRSquared <- summary(newReg)$adj.r.squared
        # rse <- summary(newReg)$sigma
      }
      print("LevelII successful")
      return(list("regModels" = regModels, "smlNumsOfSubset" = smlNumsOfSubset, "bigNumsOfSubset" = bigNumsOfSubset, "subsets" = subsets))
    }
    else{
      print("AutoSubset returned null in LevelII")
    }
  }
  else{
    print("Dataframe is null in LevelII Linear Reg")
  }
  return(NULL)
}

LevelIIILinearReg <- function(df, n){
  # This will create subsets for each variable(including the target var) seperately 
  # and test which is the best column to keep for the subset
  # n is the target column index
  print("In LevelIII")
  minRMSE = 1000
  bestSubsetNum = -10
  bestSubData = list()
  trainTest = randomSplitDataFrame(df, split_ratio = 0.8)
  trainData = trainTest$train_data
  testData = trainTest$test_data
  for (i in seq_along(df)){
    subData = LevelIILinearReg(trainData, n, i)
    print("Subdata in LevelIIIlinreg found")
    newRMSE = RegComboAccuracy(testData, subData, n)
    if (newRMSE < minRMSE){
      minRMSE = newRMSE
      bestSubsetNum = i
      bestSubData = subData
    }
  }
  print("LevelIII Done :)")
  return(list("bestSubData" = bestSubData, "bestSubsetNum" = bestSubsetNum, "minRMSE" = minRMSE))
}
lvlIVCount = 0

LevelIVLinearReg <- function(df, n, subsettedColumn = -1){
  # This will be a sort of decision tree, where at each level it will find the best
  # subsets to create, getting closer and closer to a minimum RMSE, and the best overall Linear Regression
  lvlIVCount <<- lvlIVCount + 1
  bestSubbedData1 = LevelIIILinearReg(df, n)
  if(lvlIVCount <= ncol(df)^3){  #Change the power based on the desired runtime
    if (is.null(dim(bestSubbedData1))){
      newRMSE = bestSubbedData1$minRMSE
      oldRMSE = OrigRegAccuracy(df, n)
      if (oldRMSE > newRMSE){
        subData = bestSubbedData1$bestSubData
        bestNum = bestSubbedData1$bestSubsetNum
        subsets = subData$subsets
        # smlIntervalNums = list()
        # bigIntervalNums = list()
        bestSubbedDataFinal = list()
        bestSubbedDataFinal = c(bestSubbedDataFinal, bestSubbedData1)
        for (i in seq_along(subsets)){
          bestSubbedDataFinal = c(bestSubbedDataFinal, list(LevelIVLinearReg(subsets[i], n, subsettedColumn = bestNum)))
        }
        return(bestSubbedDataFinal)
    }
    else{
      mini = 0
      maxi = 0
      if (subsettedColumn != -1){
        mini = min(df[,subsettedColumn])
        maxi = max(df[,subsettedColumn])
      }
      else{
        mini = min(df)
        maxi = max(df)
      }
      smlNumsOfSubset = list(mini)
      bigNumsOfSubset = list(maxi)
      bestSubbedDataFinal = list(list(regModels = LevelILinearReg(df, n), smlNumsOfSubset = smlNumsOfSubset, bigNumsOfSubset = bigNumsOfSubset))
      random = 0  # This is just so that the computer doesn't turn the list into an object
      
      return(list("bestSubbedDataFinal" = bestSubbedDataFinal, "random" = random))
    }
  }
  else{
    print("LevelIIILinearReg did not work in LevelIVLinearReg :(")
  }
  }
  else{
    cat("LevelIV was performed ", lvlIVCount," many times while there are only ", ncols(df), "columns in current df.\n")
    return(NULL)
  }
}


FinalComboPredictionModel <- function(newData, bestSubbedDataFinal){
  # This will decide which of the seperate regressions will be used for a prediction
  # Used for Level4 regression
  # a is the subsetted column index
  # newdata is the data POINT that will be predicted
  if (length(bestSubbedDataFinal) > 2){
    firstData1 = bestSubbedDataFinal[[1]]
    subbedNum = firstData1$bestSubsetNum  #index of previous column that was subsetted
    firstData2 = firstData1$bestSubData  
    smlNumsOfSubset = firstData2$smlNumsOfSubset
    bigNumsOfSubset = firstData2$bigNumsOfSubset
    list = IntervalOfInterest(smlNumsOfSubset, bigNumsOfSubset, subbedNum, newData)
    numOfInterval = list$numOfInterval
    count = list$count
    if(count == 1){
      FinalRegModel = FinalComboPredictionModel(newData, bestSubbedDataFinal[numOfInterval+1])
      return(FinalRegModel)
    }
    else{
      print("Count is wrong in FinalComboPredictionModel")
      return(NULL)
    }
  }
  else if(length(bestSubbedDataFinal) == 2){
    regModel = bestSubbedDataFinal$regModels
    print("RegModel found in FinalComboPredictionModel")
    return(regModel)
  }
  else{
    print("bestSubbedDataFinal in FinalComboPrediction Model doesn't have the right length :((  (double sad)")
    return(NULL)
  }
}

SubsetPredictionModel <- function(df, regModels, smlNumsOfSubset, bigNumsOfSubset, a, newData){
  # This will decide which of the seperate regressions for a single column's subset
  # will be used for a prediction
  # newdata is the data POINT that will be predicted
  list = IntervalOfInterest(df, regModels, smlNumsOfSubset, bigNumsOfSubset, a, newData)
  numOfInterval = list$numOfInterval
  count = list$count
  if (numOfInterval != -10){
    if (count == 1){
      return(regModels[[numOfInterval]])
    }
    else{
      print("Some data is in multiple subsets in SubsetPredictions...")
      return(NULL)
    }
  }
  else{
    print("This data point doesn't belong to any subset here")
    return(NULL)
  }
}

IntervalOfInterest <- function(smlNumsOfSubset, bigNumsOfSubset, a, newData){
  # a is the subsetted column index
  # newdata is the data POINT that will be predicted
  
  numOfInterval = -10
  count = 0  #Will check if this only belongs to one subset(it should)
  item = newData[,a]
  for (i in seq_along(smlNumsOfSubset)){
    if (i == length(smlNumsOfSubset)){
      bigNumsOfSubset[i] = abs(bigNumsOfSubset[i])*1000 #to account for anything higher than max of the dataframe
    }
    else if(i == 1){
      smlNumsOfSubset[i] = -(abs(smlNumsOfSubset[i])*1000) #to account for anything lower than min of the dataframe
    }
    if (item > smlNumsOfSubset[i] && item <= bigNumsOfSubset[i]){
      numOfInterval = i
      count = count + 1
    }
  }
  return(list("numOfInterval" = numOfInterval, "count" = count))
}

OrigRegAccuracy <- function(df, n){
  trainTest = randomSplitDataFrame(df, split_ratio = 0.8)
  trainData = trainTest$train_data
  testData = trainTest$test_data
  model = LevelILinearReg(trainData, n)
  predictions = predict(model, testData)
  actualVals = testData[,n]
  RMSE = CalculateRMSE(predictions, actualVals)
  return(RMSE)
}

RegComboAccuracy <- function(testData, subsetData, n){
  # n is index of target col
  predictions = CalculatePrediction(subsetData, testData, n)
  actualVals = testData[,n]
  rmse = CalculateRMSE(predictions, actualVals)
  return(rmse)
}

CalculateRMSE <- function(predictions, actualVals){
  residuals = actualVals - predictions
  RMSE = (mean(residuals^2))^0.5
  return(RMSE)
}

CalculatePrediction <- function(subData, newData, n){
  regModels = subData$regModels
  smlNumsOfSubset = subData$smlNumsOfSubset
  bigNumsOfSubset = subData$bigNumsOfSubset
  linReg = SubsetPredictionModel(subData, newData, n)
  predictions = predict(linReg, newData)
  return(predictions)
}

randomSplitDataFrame <- function(data, split_ratio = 0.8) {
  # Check if split ratio is valid
  if (split_ratio <= 0 || split_ratio >= 1) {
    stop("Invalid split ratio. It should be between 0 and 1.")
  }
  
  # Create a vector of randomly shuffled indices
  shuffled_indices <- sample(seq_len(nrow(data)))
  
  # Determine the split point based on the split ratio
  split_point <- floor(split_ratio * nrow(data))
  
  # Split the indices into training and testing sets
  train_indices <- shuffled_indices[seq_len(split_point)]
  test_indices <- shuffled_indices[-seq_len(split_point)]
  
  # Split the data into training and testing sets
  train_data <- data[train_indices, ]
  test_data <- data[test_indices, ]
  
  # Return the split datasets
  return(list("train_data" = train_data, "test_data" = test_data))
}

subBlah = newdf[which(newdf[, 2] == 1), ]
subBlah1 = newdf[which(newdf[, 2] == 2), ]
subBlah3 = c(list(subBlah1), list(subBlah))

AutoSubset <- function(df, a, cats, type) {
  # This will systematically create subsets based on a given column
  # a is the index of column that will be subsetted
  # cats is the list of categories for column a
  subsets = list()
  bigNumsOfSubset = list()
  smlNumsOfSubset = list()
  smlNum = min(df[,a])
  bigNum = max(df[,a])
  range1 = bigNum-smlNum
  if (!is.null(dim(df))) {
    if (range1>1){
    if (range1 <= 10 || type[a] == "Categorical") {
      print("entering first loop of autosubset")
      for (i in range(range1)) {
        subsets <- c(subsets, list(df[which(df[, a] == i), ]))
        bigNumsOfSubset <- c(bigNumsOfSubset, i)
        smlNmsOfSubset <- c(smlNumsOfSubset, i-1)
      }
    }
    else{
      lenOfInters <- double(range1) / 10
      print("entering second loop of autosubset")
      for (i in 1:10) {
        b <- i * lenOfInters
        c <- (i - 1) * lenOfInters
        subsets <- c(subsets, df[which(df[, a] <= b & df[, a] > c), ])
        bigNumsOfSubset <- c(bigNumsOfSubset, b)
        smlNmsOfSubset <- c(smlNumsOfSubset, c)
      }
    }
    return(list(subsets, smlNumsOfSubset, bigNumsOfSubset))
    }
    else{
      print("range of values is <=1")
      # cat("categories in Autosubset is", cats,"\n")
      return(NULL)
    }
  } 
  else {
    print("dataframe is NULL in AutoSubset")
    return(NULL)
  }
}

CreateLinearReg <- function(df, n, features){
  #n is target col index
  
  # Check if target_index is within the valid range
  if (is.na(n) || n < 1 || n > ncol(df)) {
    stop("Invalid target column index.")
  }
  target_column <- colnames(df)[n]
  formula <- paste(target_column, "~", paste(features, collapse = " + "))
  model <- lm(formula, data = df)
  return(model)
}

SigVals <- list()
RegFeatures <- list()

BestRegFeatures <- function(feature_list, dataframe, targetName) {
  # This creates a final list of features for a regression after all types of features are collected
  # Sort the feature list based on t-values
  sorted_features <- feature_list[order(feature_list$t_values, decreasing = TRUE), ]
  SigVals <<- sorted_features
  LinRegFactors <- list()  # Initialize an empty list to store significant features
  count = 1
  for (feat in sorted_features[[1]]) {
    multiColinear = CheckMultiColinearity(dataframe, LinRegFactors, feat)
    if (multiColinear == FALSE){
      LinRegFactors <- c(LinRegFactors, feat)
      if (count == 1){
        LinRegFactors <- CheckSigOfFeatures(LinRegFactors, dataframe, targetName)
        count = 1
        # print(LinRegFactors)
      }
      else{
        count = count + 1
      }
    }
  }
  RegFeatures <<- LinRegFactors
  return(LinRegFactors)
}

CheckMultiColinearity <- function(df, LinRegFactors, feat){
  # print(feat)
  for (factor in LinRegFactors){
    # print(factor)
    formulaStr = paste(feat, "~", factor)
    model1 <- lm(data = df, formula = formulaStr)
    # print(summary(model1))
    std <- summary(model1)$coefficients[2, "Std. Error"]
    if (std < 0.0001){
      return(TRUE)
    }
  }
  return(FALSE)
}

# bool = CheckMultiColinearity(newdf, blah[9], blah[10])
# bool

CheckSigOfFeatures <- function(LinRegFactors, df, targetName){
  len = length(LinRegFactors)
  if (len != 0){
    formulaStr1 = paste(targetName, "~", paste(LinRegFactors, collapse = " + "))
    model1 <- lm(data = df, formula = formulaStr1)
    newFactors = list()
    rmvdFactors = list()
    # print(summary(model1))
    if (length(summary(model1)$coefficients[,"Pr(>|t|)"]) == len+1){
    for (i in seq_along(LinRegFactors)){
      pVal <- summary(model1)$coefficients[i+1, "Pr(>|t|)"]
      # print(pVal)
      if (pVal < 0.05){
        newFactors <- c(newFactors, LinRegFactors[[i]])
      }
      # else{
      #   rmvdFactors = c(rmvdFactors, LinRegFactors[[i]])
      # }
    }
    return(newFactors)
    }
    else{
      return(LinRegFactors[1:len-1])
    }
  }
  else{
    return(NULL)
  }
}

ConvertAllVars <- function(df){
  categories = list()
  types = list()
  for (i in seq_along(df)){
    variableType = CheckVarType(df, i)
    types = c(types, variableType)
    if (variableType == "Categorical"){
      data = ConvertCategoricalVar(df, i)
      df = data$df
      categories = c(categories, list(data$cats))
    }
    else if (variableType == "Time"){
      data = ConvertTimeCol(df, i)
      df = data$df
      categories = c(categories, list(data$cats))
    }
    else if (variableType == "None"){
      cat("Column", column_index, "is none type.\n")
      categories <- c(categories, list(0))
    }
    else if (variableType == "Numeric"){
      categories <- c(categories, list(0))
    }
    else{
      print("Variable type is unknown while converting")
      categories <- c(categories, list(0))
    }
  }
  print("Df has been Converted!")
  return(list("df" = df, "categories" = categories, "types" = types))
}

ConvertCategoricalVar <- function(df, n) {
      # this will automatically transfer a categorical variable to a numerical value
      # n is index of column that will be changed
      
  if (!is.null(dim(df))) {
    cats <- list()  # Use character(0) to initialize an empty character vector
    newCol <- list()  # Use integer(0) to initialize an empty integer vector
    
    for (p in df[, n]) {
      if (is.null(p)){
        stop("Value is Null in ConvertVar")
      }
      clear <- FALSE
      p <- tolower(p)
      if (length(cats) == 0) {
        cats <- c(cats, p)
      }
      num <- -1
      for (i in seq_along(cats)) {
        string <- tolower(cats[i])
        if (string == p) {
          clear <- TRUE
          num <- i
        }
      }
      if (clear == TRUE && num != -2) {
        newCol <- c(newCol, num)
      } 
      else {
        cats <- c(cats, p)
        newCol <- c(newCol, length(cats) - 1)
      }
    }
    df[, n] <- newCol
    return(list("df" = df, "cats" = cats))
  } 
  else {
    print("Dataframe is empty")
    return(NULL)
  }
}
    
ConvertTimeCol <- function(data, column_index) {
  # This converts a given column with dates into numbers, 0 being the first day
  # and each day after is referenced by the days since "day zero
  # Check if column_index is within the valid range
  dates = list()
  if (column_index < 1 || column_index > ncol(data)) {
    stop("Invalid column index.")
      }
      
      # Extract the specified column
  date_column <- data[, column_index]
      
  # Check if the column is a date column
  if (!all(sapply(date_column, function(x) inherits(x, "Date")))) {
    stop("The specified column is not a date column.")
      }
      
  # Find the earliest date (day zero)
  day_zero <- min(date_column)
      
    # Convert each date to the number of days since day zero
  days_since_start <- as.numeric(difftime(date_column, day_zero, units = "days"))
      
  #     # Create a new column with the converted values
  # new_column_name <- paste0(names(data)[column_index], "_days_since_start")
  # data[[new_column_name]] <- days_since_start
  
  uniqueDates <- unique(days_since_start)
  
  data[,column_index] <- days_since_start
      
  return(list("df" = data, "cats" = uniqueDates))
}
    
CheckVarType <- function(data, column_index) {
  # Check if column_index is within the valid range
  if (column_index < 1 || column_index > ncol(data)) {
    stop("Invalid column index.")
  }
  
  # Extract the specified column
  column <- data[, column_index]
  
  # Check the type of the variable
  if (is.numeric(column)) {
    # cat("Column", column_index, "is a numerical variable.\n")
    return("Numeric")
  } 
  else if (is.factor(column) || is.character(column)) {
    # cat("Column", column_index, "is a categorical variable.\n")
    return("Categorical")
  } 
  else if (is.Date(column) || any(sapply(column, function(x) inherits(x, "POSIXt")))) {
    # cat("Column", column_index, "is a time variable.\n")
    return("Time")
  } 
  else {
    cat("Column", column_index, "is of an unknown type.\n")
    return("None")
  }
}

FeatureCollection <- function(df, n) {
  # This Function takes in an already converted dataframe and makes a list of different
  # features and their significance of correlations.
  # n is index of target column
  if (!is.null(dim(df))) {
    SigVals <- data.frame("features" = character(0), "t_values" = numeric(0))
    SigVals <- LogarithmicVars(df, n, SigVals)
    SigVals <- ExponentialVars(df, n, SigVals)
    # SigVals <- SingleVars(df, n, SigVals)
    print("Features collected")
    cat("Dim of SigVals is:", dim(SigVals), ".\n")
    return(SigVals)
  } 
  else {
    print("Dataframe is empty in Pattern Recog")
    return(NULL)
  }
}

LogarithmicVars <- function(df, a, SigVals) {
  # This function goes through correlation of logarithmic variables
  # Originally, I wanted to incorporate the log-linear and log-log regressions,
  # but realized it was going to mess up the rest of the regression features
  # as the target column would have to be converted to a logarithmic function 
  # and would have to be re-evaluated
  for (i in seq_along(df)) {
    target <- df[, a]
    feature <- log(df[, i])
    if (a != i){
      tStat <- SigTest(target, feature)
      name = names(df)[i]
      feature <- capture.output(cat("log(",name,")"))
      SigVals <- UpdateSigVals(feature, tStat, SigVals)
      # 
      # target <- log(df[, a])
      # tStat <- SigTest(target, feature)
      # SigVals <- UpdateSigVals(feature, tStat, SigVals)
      # 
      # feature <- df[, i]
      # tStat <- SigTest(target, feature)
      # SigVals <- UpdateSigVals(feature, tStat, SigVals)
    }
  }
  return(SigVals)
}

ExponentialVars <- function(df, a, SigVals) {
  # This determines the T-Statistic of all features with different exponents
  for (i in seq_along(df)) {
    if (i != a){
      for (j in seq(0.5, 3.5, by = 0.5)) {
        target <- df[, a]
        feature <- (df[, i])^j
        tStat <- SigTest(target, feature)
        name = names(df)[i]
        feature <- capture.output(cat("I((",name,")^",j,")"))
        SigVals <- UpdateSigVals(feature, tStat, SigVals)
      }
    }
  }
  return(SigVals)
}

SigTest <- function(target, features){
  linReg <- lm(target ~ features)
  tVal <- summary(linReg)$coefficients[2, "t value"]
  tVal = abs(tVal)
  return(tVal)
}

UpdateSigVals <- function(feature, tStat, SigVals) {
  feature <- as.character(feature)
  tStat <- as.numeric(tStat)
  if(!is.null(feature) && !is.null(tStat) && !is.na(feature) && !is.na(tStat)){
    newItem <- data.frame("feature" = feature, "t_values" = tStat)
    SigVals <- rbind(SigVals, newItem)
    # cat("new SigVals length: ", nrow(SigVals), "\n")
    # cat("new SigVals dim: ", dim(SigVals), "\n")
    # cat("tStat now:", tStat, ".\n")
    return(SigVals)
  }
  else{
    print("Null value in updateSigVals")
    cat("feature:",feature,"\n")
    cat("tStat:",tStat,"\n")
    return(NULL)
  }
}

# SingleVars <- function(df, a, SigVals) {
#   # a is column of target
#   for (i in seq_along(df)) {
#     if (a != i){
#       target <- df[, a]
#       feature <- df[, i]
#       tStat <- SigTest(target, feature)
#       name = names(df)[i]
#       feature <- name
#       SigVals <- UpdateSigVals(feature, tStat, SigVals)
#     }
#   }
#   return(SigVals)
# }


# SigVals <- data.frame("features" = character(0), "t_values" = numeric(0))
# feature <- "genre"
# tStat <- 2.00
# SigVals <- UpdateSigVals(feature, tStat, SigVals)

# FilterSigVals <- function(SigVals){
#   # This will leave only the most significant form of each variable
#   # This will be written after the SigTest method is finished as it is not clear if dictionary will work  
# }

# IsSignif <- function(tVal)
#   if tVal >= 1.96:
#     return TRUE
#   else:
#     return FALSE

# MultiVar <- function(df, n, SigVals){
#   for i in range (0,df[0].length):
#     feats = []
#     GetTStats(df, i, SigVals)
# }

# GetTStats <- function(df, n, SigVals){
#   # n is size of each group - 1
#   for i in range (0,df[0].length)
#     featNums.append(i)
#   featNums = GetFeatNums(n, featNums)
# 
# }

# GetFeatNums <- function(n, featNums)
#   if n = -1:
#     return(featNums)
#   else if n = 0:
#     for i in range (0,df[0].length)
#       featNums.append(i)
#       featNums = GetFeatNums(df, n-1, featNums)
#   else:
#     for i in range (0, featNums.length)
#       for j in range (i, featNums.length)
#         featnums
# 
# GetFeatNums <- function(n, featNums){
#   if n = 0:
#     for i in range (0,featNums.length)
#       
#       return(featNums)
#   else:
#     for i in range (n, featNums.length)
#       if featNums.length < n:
#         featNums.append(i)
#         featNums = GetFeatNums(n , featNums)
#       else:
#         GetFeatNums(n-1, featNums[i])
# }
        
# CheckOVB <- function(LinRegFactors, feat, df, targetName) {
#   # formulaStr1 = paste(targetName, "~", paste(LinRegFactors, collapse = " + "))
#   # model1 <- lm(data = df, formula = formulaStr1)
#   # coef_without_feat <- coef(model1)
#   num = length(LinRegFactors)
#   formulaStr2 = paste(targetName, "~", paste(LinRegFactors, collapse = " + "), " + ", feat)
#   model2 <- lm(data = df, formula = formulaStr2)
#   pVal <- summary(model2)$coefficients[num+2, "Pr(>|t|)"]
#   # tVal1 <- summary(model2)$coefficients[num-1, "t value"]
#   # tVal = abs(tVal)
# 
#   return(pVal)
# }


# LinRegFactors <- c("predictor1", "predictor2")
# feat <- "additional_feature"
# dataframe <- data.frame(
#   response = c(1.1, 2.5, 3.9, 5.5, 6.9, 8.3),
#   predictor1 = c(4.5, 5.3, 6.1, 2.6, 7.5, 0.9),
#   predictor2 = c(1.2, 3.6, 2.7, 5.8, 12.9, 10.6),
#   additional_feature = c(11.7, 13.8, 15.9, 10.2, 11.4, 18.5)
# )
# 
# tVal = CheckOVB(LinRegFactors, feat, dataframe,"response")
# tVal# CheckOVB <- function(LinRegFactors, feat, df, targetName) {
#   # formulaStr1 = paste(targetName, "~", paste(LinRegFactors, collapse = " + "))
#   # model1 <- lm(data = df, formula = formulaStr1)
#   # coef_without_feat <- coef(model1)
#   num = length(LinRegFactors)
#   formulaStr2 = paste(targetName, "~", paste(LinRegFactors, collapse = " + "), " + ", feat)
#   model2 <- lm(data = df, formula = formulaStr2)
#   pVal <- summary(model2)$coefficients[num+2, "Pr(>|t|)"]
#   # tVal1 <- summary(model2)$coefficients[num-1, "t value"]
#   # tVal = abs(tVal)
# 
#   return(pVal)
# }


# LinRegFactors <- c("predictor1", "predictor2")
# feat <- "additional_feature"
# dataframe <- data.frame(
#   response = c(1.1, 2.5, 3.9, 5.5, 6.9, 8.3),
#   predictor1 = c(4.5, 5.3, 6.1, 2.6, 7.5, 0.9),
#   predictor2 = c(1.2, 3.6, 2.7, 5.8, 12.9, 10.6),
#   additional_feature = c(11.7, 13.8, 15.9, 10.2, 11.4, 18.5)
# )
# 
# tVal = CheckOVB(LinRegFactors, feat, dataframe,"response")
# tVal
