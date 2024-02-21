# The purpose of this code is to intake a dataframe with numerical and categorical values 
# and output the best possible multivariate linear regression tree. In the end, this 
# algorithm ended up being simillar to a regression tree, but in my humble opinion it 
# is better because I(the software engineer) know exactly what is going on in the program,
# and can thus make improvements through experimentation.

# Note: The dataset being used is a simply fir testing purposes. In reality, the 
# algorithm will work on any dataset

# Note: Additional software should be developed for time-series analysis, but for now
# what is currently set is good enough for a surface level analysis.

library(tidyverse)
library(sandwich)
library(lmtest)

df <- read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/Movies2022F-4-new.csv");

# This is built so that the target column is numeric only

regModel = LevelILinearReg(df, 3)  # Success!

models = LevelIILinearReg(df, 3, 6) # Success!

regModelsLvl_3 = LevelIIILinearReg(df, 3) # Success!

FinalRegTree = LevelIVLinearReg(df, 3) # Success!

# --------------------------------------------------------------------------

# Initializing global variables to observe all aspects of output at each level

bestSubbedData2 <- list()
subs1 <- list()
subs2 <- list()
featVals <- list()
newData <- list()
newCats <- list()
types1 <- list()
LinRegModels <- list()
subs <- list()
lvlIVCount = 0
moreSubbedData <- list()
residuals0 <- list()

newDf <- newData$df

# --------------------------------------------------------------------------

LevelILinearReg <- function(df, n){
  # n is target column
  
  data = ConvertAllVars(df)
  df = data$df
  # newDf1 <<- df
  if (!is.null(dim(df))){
    SigVals = FeatureCollection(df, n, data$types)
    targetName = colnames(df)[n]
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
    targetName = colnames(df)[n]
    categories = data$categories
    newCats <<- categories
    categories1 = categories[[a]]
    varTypes = data$types
    types1 <<- varTypes
    subs = AutoSubset(df, a, categories1, varTypes[a])
    subs2 <<- subs
    if (!(is.null(subs))){
      subsets = subs[[1]]
      subs1 <<- subsets
      smlNumsOfSubset = subs[[2]]
      bigNumsOfSubset = subs[[3]]
      # subNames = list()
      FeatureVals = list()
      newSmlNumsOfSubset = list()
      newBigNumsOfSubset = list()
      newSubsets = list()
      print("Entering for loop in LevelII")
      for (i in 1:length(subsets)){
        subi = subsets[[i]]
        if(nrow(subi) > 1){
          Values <- FeatureCollection(subi, n, varTypes)
          if (!is.null(Values)){
            FeatureVals = c(FeatureVals, list(Values))
            features = BestRegFeatures(Values, subi, targetName)
            newReg = CreateLinearReg(subi, n, features)
            if(!is.null(newReg)){
              regModels = c(regModels, list(newReg))
              newSmlNumsOfSubset = c(newSmlNumsOfSubset, smlNumsOfSubset[[i]])
              newBigNumsOfSubset = c(newBigNumsOfSubset, bigNumsOfSubset[[i]])
              subi = ConvertVarsBack(subi, categories, varTypes)
              newSubsets = c(newSubsets, list(subi))
            }
          }
        }
        else{
          print("Values is null in LevelII")
        }
        # count = count + 1
        # newName = cat("This is the ", count, "th subset for column", a, " with num of df rows being: ", nrow(df))
        # subNames = c(subnames, newName)
      }
      LinRegModels <<- regModels
      featVals <<- FeatureVals
      subs <<- newSubsets
      
      subsets = newSubsets
      smlNumsOfSubset = newSmlNumsOfSubset
      bigNumsOfSubset = newBigNumsOfSubset
      
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
  trainTest = randomSplitDataFrame(df)
  trainData = trainTest$train_data
  testData = trainTest$test_data
  testData1 = ConvertAllVars(testData)
  testData = testData1$df
  testData10 <<- testData
  for (i in seq_along(df)){
    if (i != n){
      subData = LevelIILinearReg(trainData, n, i)
      if(!is.null(subData)){
        print("Subdata in LevelIII lin reg found")
        newRMSE = RegComboAccuracy(testData, subData, n, i)
        cat("new RMSE in Lvl 3 is", newRMSE, "\n")
        if (!is.null(newRMSE) && !is.na(newRMSE)){
          if (newRMSE < minRMSE){
            minRMSE = newRMSE
            bestSubsetNum = i
          }
        }
        else{
          print("NewRMSE not found in Level 3 lin reg")
        }
      }
    }
  }
  if (bestSubsetNum != -10){
    bestSubData <- LevelIILinearReg(df, n, bestSubsetNum)
    print("LevelIII Done :)")
    return(list("bestSubData" = bestSubData, "bestSubsetNum" = bestSubsetNum, "minRMSE" = minRMSE))
  }
  else{
    print("No Subset Num found in levelIIIlinReg")
    return(NULL)
  }
}

lvlIVCount = 0

LevelIVLinearReg <- function(df, n, subsettedColumn = -1){
  # This will be a sort of decision tree, where at each level it will find the best
  # subsets to create, getting closer and closer to a minimum RMSE, and the best overall Linear Regression
  
  print("In Level IV")
  lvlIVCount <<- lvlIVCount + 1
  bestSubbedData1 = LevelIIILinearReg(df, n)
  bestSubbedData2 <<- bestSubbedData1
  if(lvlIVCount <= ncol(df)^3 & !is.null(bestSubbedData1) ){  # Change the power based on the desired maximum runtime
    if (!is.null(bestSubbedData1)){
      newRMSE = bestSubbedData1$minRMSE
      oldRMSE = OrigRegAccuracy(df, n)
      if ((oldRMSE*0.98) > newRMSE){   # 0.98 allows a 2% margin of randomness
        print("oldRMSE was bigger than newRMSE")
        cat("oldRMSE:", oldRMSE, "newRMSE:", newRMSE, "\n")
        subData = bestSubbedData1$bestSubData
        bestNum = bestSubbedData1$bestSubsetNum
        subsets = subData$subsets
        bestSubbedDataFinal = list(list(regModels = subData$regModels, smlNumsOfSubset = subData$smlNumsOfSubset, bigNumsOfSubset = subData$bigNumsOfSubset, columnIndex = bestNum, newRMSE = newRMSE))
        bestSubbedDataFinal1 <<- bestSubbedDataFinal
        if (!is.null(length(subsets))){
          for (subi in subsets){
            furtherSubbedData <- LevelIVLinearReg(subi, n, subsettedColumn = bestNum)
            bestSubbedDataFinal = c(bestSubbedDataFinal, list(furtherSubbedData))
          }
          print("Level IV successful")
          return(bestSubbedDataFinal)
        }
      }
      else{

        bestSubbedDataFinal = list(regModel = LevelILinearReg(df, n), RMSE = oldRMSE)

        print("Reached a leaf node and returned a its regression")
        return(list(bestSubbedDataFinal))
      }
  }
    else{
      print("LevelIIILinearReg did not work in LevelIVLinearReg :(")
      return(NULL)
    }
  }
  else{
    cat("LevelIV was performed ", lvlIVCount," many times while there are only ", ncol(df), "columns in current df and only", ncol(df)^3," allowed \n")
    return(NULL)
  }
}

# -------------------------------------------------------------------------

FinalComboPredictionModel <- function(newData, FinalRegressionTree){
  # This will decide which of the seperate regressions will be used for a prediction
  # Used for Level4 regression
  # a is the subsetted column index
  # newdata is the data POINT that will be predicted
  
  print("In FinalComboPredictionModel")
  if (length(FinalRegressionTree) > 1){
    firstData1 = FinalRegressionTree[[1]]
    subbedNum = firstData1$columnIndex  #index of previous column that was subsetted
    smlNumsOfSubset = firstData1$smlNumsOfSubset
    bigNumsOfSubset = firstData1$bigNumsOfSubset
    list = IntervalOfInterest(smlNumsOfSubset, bigNumsOfSubset, subbedNum, newData)
    if (!is.null(list)){
      numOfInterval = list$numOfInterval
      count = list$count
      if(count == 1){
        FinalRegModel = FinalComboPredictionModel(newData, FinalRegressionTree[[numOfInterval+1]])
        return(FinalRegModel)
      }
      else{
        print("Count is wrong in FinalComboPredictionModel")
        return(NULL)
      }
    }
  }
  else if(length(FinalRegressionTree) == 1){
    regModel = FinalRegressionTree[[1]]$regModel
    print("RegModel found in FinalComboPredictionModel")
    return(regModel)
  }
  else{
    print("bestSubbedDataFinal in FinalComboPrediction Model doesn't have the right length :((  (double sad)")
    return(NULL)
  }
}

SubsetPredictionModel <- function(regModels, smlNumsOfSubset, bigNumsOfSubset, a, newData){
  # This will decide which of the seperate regressions for a single column's subset
  # will be used for a prediction
  # newdata is the data POINT that will be predicted
  # a is the column index that was subsetted
  list = IntervalOfInterest(smlNumsOfSubset, bigNumsOfSubset, a, newData)
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
  item = newData[[a]]
  item1 <<- item
  for (i in seq_along(smlNumsOfSubset)){
    if (i == length(smlNumsOfSubset)){
      bigNumsOfSubset[[i]] = abs(bigNumsOfSubset[[i]])+1000 #to account for anything higher than max of the dataframe
    }
    else if(i == 1){
      smlNums1 <<- smlNumsOfSubset
      smlNumsOfSubset[[i]] = -abs(smlNumsOfSubset[[i]])-1000 #to account for anything lower than min of the dataframe
    }
    if (item > smlNumsOfSubset[[i]] && item <= bigNumsOfSubset[[i]]){
      numOfInterval = i
      count = count + 1
    }
  }
  return(list("numOfInterval" = numOfInterval, "count" = count))
}

OrigRegAccuracy <- function(df, n){
  trainTest = randomSplitDataFrame(df)
  trainData = trainTest$train_data
  testData = trainTest$test_data
  model = LevelILinearReg(trainData, n)
  testData1 = ConvertAllVars(testData)
  testData = testData1$df
  predictions = predict(model, testData)
  actualVals = testData[,n]
  RMSE = CalculateRMSE(predictions, actualVals)
  return(RMSE)
}

RegComboAccuracy <- function(testData, subsetData, n, i){
  # n is index of target col
  # i is index of subsetted column
  predictions = CalculatePrediction(subsetData, testData, n, i)
  preds <<- predictions
  actualVals = testData[,n]
  actVals <<- actualVals
  rmse = CalculateRMSE(predictions, actualVals)
  return(rmse)
}

CalculateRMSE <- function(predictions, actualVals){
  residuals2 = list()
  if (length(predictions) > 0){
    for(i in 1:length(predictions)){
      if (predictions[[i]] != -123.321 & !is.null(actualVals[i])){
        resid = actualVals[i] - predictions[[i]]
        resid = resid^2
        residuals2 = c(residuals2, resid)
      }
    }
  }
  else{
    print("No Predictions found when calculating RMSE")
    residuals2 <- list(100000000) # Arbitrarily high to make it unattractive
  }
  if(length(residuals2) > 0){
    residuals0 <<- residuals2
    RMSE = (mean(unlist(residuals2)))^0.5
    return(RMSE)
  }
  else{
    print("No residuals found in Calculate RMSE")
    return(10000^1000) # Arbitrarily high to make it unattractive
  }
}

CalculatePrediction <- function(subData, newData, n, i){
  # n is target column
  # i is subsetted column
  regModels = subData$regModels
  smlNumsOfSubset = subData$smlNumsOfSubset
  bigNumsOfSubset = subData$bigNumsOfSubset
  predictions = list()
  for (j in 1:nrow(newData)){
    point = newData[j,]
    newpoint <<- point
    linReg = SubsetPredictionModel(regModels, smlNumsOfSubset, bigNumsOfSubset, i, point)
    if (!is.null(linReg) & !is.null(point)){
      preds1 = predict(linReg, point)
      predictions = c(predictions, preds1)
    }
    else{
      predictions = c(predictions, -123.321) # this will be passed by in next step
    }
  }
  return(predictions)
}

randomSplitDataFrame <- function(data, split_ratio = 0.85) {
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

AutoSubset <- function(df, a, cats, type) {
  # This will systematically create subsets based on a given column
  # a is the index of column that will be subsetted
  # cats is the list of categories for column a
  subsets = list()
  bigNumsOfSubset = list()
  smlNumsOfSubset = list()
  if (!is.null(dim(df))) {
    smlNum = min(df[,a])
    bigNum = max(df[,a])
    range1 = bigNum-smlNum
    items = unique(df[,a])
    if (range1 > 0){
      if (length(items) <= 10 || type[a] == "Categorical") {
        print("entering first loop of autosubset")
        for (i in 1:length(items)) {
          subsets <- c(subsets, list(df[which(df[, a] == items[i]), ]))
          bigNumsOfSubset <- c(bigNumsOfSubset, items[i])
          if (i > 1){
            smlNumsOfSubset <- c(smlNumsOfSubset, items[i-1])
          }
          else{
            smlNumsOfSubset <- c(smlNumsOfSubset, items[i]-10)
          }
        }
      }
      else{
        lenOfInters <- range1/ 10
        print("entering second loop of autosubset")
        for (i in 1:10) {
          b <- i * lenOfInters
          c <- (i - 1) * lenOfInters
          subsets <- c(subsets, list(df[which(df[, a] <= b & df[, a] > c), ]))
          bigNumsOfSubset <- c(bigNumsOfSubset, b)
          smlNumsOfSubset <- c(smlNumsOfSubset, c)
        }
      }
      return(list(subsets, smlNumsOfSubset, bigNumsOfSubset))
    }
    else{
      print("range of values is <= 0")
      # cat("categories in Autosubset is", cats,"\n")
      return(NULL)
    }
  } 
  else {
    print("dataframe is NULL in AutoSubset")
    return(NULL)
  }
}

# -----------------------------------------------------------------

CreateLinearReg <- function(df, n, features){
  # n is target col index
  
  # Check if target_index is within the valid range
  if (is.na(n) || n < 1 || n > ncol(df)) {
    print("Invalid target column index.")
    return(NULL)
  }
  if (length(df[,1]) < 1){
    print("Not enough rows in df at createLinReg")
    return(NULL)
  }
  if (!(is.null(features))){
    if (length(features) > 0){
      target_column <- colnames(df)[n]
      formula <- paste(target_column, "~", paste(features, collapse = " + "))
      model <- lm(formula, data = df)
      return(model)
    }
  }
  target_column <- colnames(df)[n]
  formula <- paste(target_column, "~", target_column)  # if no features in regression
  model <- lm(formula, data = df) 
  return(model)
}

BestRegFeatures <- function(feature_list, dataframe, targetName) {
  # This creates a final list of features for a regression after all types of features are collected
  # Sort the feature list based on t-values
  q = dim(feature_list)
  if (!is.null(q[1])){
    if (q[1] > 0){
      sorted_features <- feature_list[order(feature_list$t_values, decreasing = TRUE), ]
      SigVals <<- sorted_features
      LinRegFactors <- list()  # Initialize an empty list to store significant features
      count = 1   # dictates the gap between checking significance of values in the regression
      num = count
      for (feat in sorted_features[[1]]) {
        multiColinear = CheckMultiColinearity(dataframe, LinRegFactors, feat)
        if (multiColinear == FALSE){
          LinRegFactors <- c(LinRegFactors, feat)
          if (count == num){
            LinRegFactors <- CheckSigOfFeatures(LinRegFactors, dataframe, targetName)
            count = 0
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
  }
  return(NULL)
}

CheckMultiColinearity <- function(df, LinRegFactors, feat){
  # print(feat)
  for (factor in LinRegFactors){
    # print(factor)
    formulaStr = paste(feat, "~", factor)
    model1 <- lm(data = df, formula = formulaStr)
    # print(summary(model1))
    if (length(summary(model1)$coefficients["Std. Error"]) == 2){
    std <- summary(model1)$coefficients[2, "Std. Error"]
    if (std < 0.0001){
      return(TRUE)
    }
    }
  }
  return(FALSE)
}

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
        if(!is.null(pVal) & !is.na(pVal)){
          if (pVal < 0.05){
            newFactors <- c(newFactors, LinRegFactors[[i]])
          }
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

# -----------------------------------------------------------------

ConvertVarsBack <- function(df1, categories, types){
  # df is the dataframe that is comprised only of numbers
  
  for (i in seq_along(df1)){
    catsOfInterest = categories[[i]]
    if (types[[i]] != "Numeric"){
      for (j in 1:nrow(df1)){
        num <- df1[j,i]
        num <- as.integer(num)
        str = catsOfInterest[[num]]
        df1[j,i] <- str
      }
      # print("Yay")
    }
  }
  print("Vars converted back")
  return(df1)
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
  newCats2 <<- categories
  return(list("df" = df, "categories" = categories, "types" = types))
}

ConvertCategoricalVar <- function(df, n) {
    # this will automatically transfer a categorical variable to a numerical value
    # n is index of column that will be changed
      
  if (!is.null(dim(df))) {
    cats <- list()
    newCol <- list()
    count = 0
    
    for (p in df[, n]) {
      if (is.null(p)){
        stop("Value is Null in ConvertVar")
      }
      clear <- FALSE
      p <- tolower(p)
      if (length(cats) == 0) {
        cats <- c(cats, p)
        # print("cats was empty but isn't now")
      }
      num = -2
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
        newCol <- c(newCol, length(cats))
        # print("New category added!")
      }
    }
    newCol <- unlist(newCol)
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

# -----------------------------------------------------------

FeatureCollection <- function(df, n, varTypes) {
  # This Function takes in an already converted dataframe and makes a list of different
  # features and their significance of correlations.
  # n is index of target column
  if (!is.null(dim(df))) {
    SigVals <- data.frame("features" = character(0), "t_values" = numeric(0))
    SigVals <- LogarithmicVars(df, n, SigVals, varTypes)
    SigVals <- ExponentialVars(df, n, SigVals, varTypes)
    SigVals <- RatioVars(df, n, SigVals, varTypes)
    # print("Features collected")
    cat("Dim of SigVals is:", dim(SigVals), ".\n")
    return(SigVals)
  }
  else {
    print("Dataframe is empty in Pattern Recog")
    return(NULL)
  }
}

SigVals <- LogarithmicVars(newDf, 2, SigVals, types1)

LogarithmicVars <- function(df, a, SigVals, varTypes) {
  # This function goes through correlation of logarithmic variables
  # Originally, I wanted to incorporate the log-linear and log-log regressions,
  # but realized it was going to mess up the rest of the regression features
  # as the target column would have to be converted to a logarithmic function 
  # and would have to be re-evaluated
  
  # a is index of target column
  
  for (i in seq_along(df)) {
    varType = varTypes[[i]]
    if (varType != "Categorical"){
      if (a != i){
        target <- df[, a]
        feature <- log(df[, i])
        tStat <- SigTest(target, feature)
        name = colnames(df)[i]
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
  }
  return(SigVals)
}

ExponentialVars <- function(df, a, SigVals, varTypes) {
  # This determines the T-Statistic of all features with different exponents
  
  # a is index of target column
  
  for (i in seq_along(df)) {
    varType = varTypes[i]
    if (varType != "Categorical"){
      if (i != a){
        for (j in seq(0.5, 3.5, by = 0.5)) {
          target <- df[, a]
          feature <- (df[, i])^j
          tStat <- SigTest(target, feature)
          name = colnames(df)[i]
          feature <- capture.output(cat("I((",name,")^",j,")"))
          SigVals <- UpdateSigVals(feature, tStat, SigVals)
        }
      }
    }
  }
  return(SigVals)
}

RatioVars <- function(df, a, SigVals, varTypes){
  # a is index of target column
  
  for (i in seq_along(df)) {
    varType = varTypes[i]
    if (varType != "Categorical"){
      if (i+1 <= ncol(df)){
        for (j in range(i+1, ncol(df))) {
          varType1 = varTypes[j]
          if (varType1 != "Categorical"){
            if (i != a && j != a){
              target <- df[, a]
              feature1 <- (df[,i])
              feature2 <- (df[,j])
              feature <- feature1/feature2
              tStat <- SigTest(target, feature)
              name1 = colnames(df)[i]
              name2 = colnames(df)[j]
              feature <- capture.output(cat("I((",name1,")/(",name2,"))"))
              SigVals <- UpdateSigVals(feature, tStat, SigVals)
            }
          }
        }
      }
    }
  }
  return(SigVals)
}

SigTest <- function(target, features){
  linReg <- lm(target ~ features)
  if (length(summary(linReg)$coefficients[, "t value"]) > 1 & !is.null(linReg)){
    tVal <- summary(linReg)$coefficients[2, "t value"]
    tVal = abs(tVal)
    return(tVal)
  }
  else{
    print("Something wrong in SigTest")
    return(NULL)
  }
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