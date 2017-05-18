  # <company> project - predicting credit default
  library(Amelia)
  library(caret)
  library(parallel)
  library(foreach)
  library(e1071)
  library(doParallel)
  
  # get the data - takes care of the NA in the num rows
  colClasses=c("character","numeric",rep('factor',3),'numeric', rep('factor',6), rep("numeric",12),'factor')
  training.data.raw <- read.csv('C:/Users/Kosta/Desktop/Modelling Examples/train.csv',header=T,
                                na.strings=c(""), colClasses = colClasses)
  
  #check missing values - none... but many poorly coded
  sapply(training.data.raw,function(x) sum(is.na(x)))
  sapply(training.data.raw, function(x) length(unique(x)))
  
  
  ##########################################
  # Data Cleaning and Imputation           #  
  ##########################################
  
  # get rid of the STRING NA's
  training.data.raw[training.data.raw=="NA"]<-NA
  
  # look up the missing values 
  missmap(training.data.raw, main = "Missing values vs observed")
  
  # fix education levels -> 5 and 6 levels should become 4
  training.data.raw$education[training.data.raw$education=="5.0" |
                                training.data.raw$education=="6.0" |
                                training.data.raw$education=="0.0"]<-as.factor('4.0')
  # fix marriage status
  training.data.raw$marriage[training.data.raw$marriage=='0.0'] <- as.factor('3.0')
  
  # fix pay_x columns - combine the values of -2 and 0 into the same bin as -1 for "duly paid"
  training.data.raw$pay_1[training.data.raw$pay_1=='-2.0' | training.data.raw$pay_1=='0.0' ] <- as.factor('-1.0')
  training.data.raw$pay_2[training.data.raw$pay_2=='-2.0' | training.data.raw$pay_2=='0.0' ] <- as.factor('-1.0')
  training.data.raw$pay_3[training.data.raw$pay_3=='-2.0' | training.data.raw$pay_3=='0.0' ] <- as.factor('-1.0')
  training.data.raw$pay_4[training.data.raw$pay_4=='-2.0' | training.data.raw$pay_4=='0.0' ] <- as.factor('-1.0')
  training.data.raw$pay_5[training.data.raw$pay_5=='-2.0' | training.data.raw$pay_5=='0.0' ] <- as.factor('-1.0')
  training.data.raw$pay_6[training.data.raw$pay_6=='-2.0' | training.data.raw$pay_6=='0.0' ] <- as.factor('-1.0')
  
  
  # for numeric take the mean of the column to impute them
  training.data.raw$pay_amt6[is.na(training.data.raw$pay_amt6)] <- mean(training.data.raw$pay_amt6,na.rm=T)
  training.data.raw$bill_amt6[is.na(training.data.raw$bill_amt6)] <- mean(training.data.raw$bill_amt6,na.rm=T)
  training.data.raw$pay_amt5[is.na(training.data.raw$pay_amt5)] <- mean(training.data.raw$pay_amt5,na.rm=T)
  training.data.raw$bill_amt5[is.na(training.data.raw$bill_amt5)] <- mean(training.data.raw$bill_amt5,na.rm=T)
  
  # for factor get the mode for pay_6 and pay_5
  all.levels <- unique(training.data.raw$pay_6)
  mode.level <- all.levels[which.max(tabulate(match(all.levels, training.data.raw$pay_6)))]
  training.data.raw$pay_6[is.na(training.data.raw$pay_6)] <- mode.level
  
  all.levels <- unique(training.data.raw$pay_5)
  mode.level <- all.levels[which.max(tabulate(match(all.levels, training.data.raw$pay_5)))]
  training.data.raw$pay_5[is.na(training.data.raw$pay_5)] <- mode.level
  
  
  ## improv
  pay <- c("pay_1", "pay_2", "pay_3", "pay_4", "pay_5", "pay_6")
  
  #condensing pay variables because they can be related to each other (ie. sequential)
  #indicator of number of on-time payments in the period provided
  training.data.raw$pay_ontime <- rowSums(training.data.raw[,pay] == -1)
  
  #backing up data set at this point
  backup <- training.data.raw
  
  #expect that bill_amt and pay_amt are related as well, and both are related to limit_bal
  #create new column for difference between bill and payment (not using ratio because of 0 bills) -- represents balance after paying
  training.data.raw$pay_diff1 <- training.data.raw$bill_amt1 - training.data.raw$pay_amt1
  training.data.raw$pay_diff2 <- training.data.raw$bill_amt2 - training.data.raw$pay_amt2
  training.data.raw$pay_diff3 <- training.data.raw$bill_amt3 - training.data.raw$pay_amt3
  training.data.raw$pay_diff4 <- training.data.raw$bill_amt4 - training.data.raw$pay_amt4
  training.data.raw$pay_diff5 <- training.data.raw$bill_amt5 - training.data.raw$pay_amt5
  training.data.raw$pay_diff6 <- training.data.raw$bill_amt6 - training.data.raw$pay_amt6
  ##
  
  
  # leave out customer id column out of the dataset since it is not a predictor
  data <- training.data.raw[ , names(training.data.raw) != "customer_id"]
  
  ##########################################
  # start modeling - takes time            #
  ##########################################
  # Calculate the number of cores and Initiate cluster
  no_cores <- detectCores() - 1
  registerDoParallel(no_cores)
  # make the predictions reproducible
  set.seed(98765)
  # build a preliminary model
  train_control <- trainControl(method="repeatedcv", number=2, repeats = 2, classProbs = TRUE, summaryFunction = mnLogLoss)
                                #summaryFunction= twoClassSummary)
  
  start <- Sys.time()
  # train the model - Random Forest (runs in parallel)
  model <- train(default_oct~., data=data, trControl=train_control, method="parRF", metric="logLoss") #rf gbm LogitBoost
  stopImplicitCluster()
  registerDoSEQ()
  print('Finished training the model')
  Sys.time() - start
  #model.rf <- model
  # summarize results
  print(model)
  
  #confusion matrix
  #confusionMatrix(predictions, validation$default_oct)
  #used the confusion matrix to determine which ratio did not sacrifice too much specificity for an increase in sensitivity. Lost 10% accuracy
  #after adjusting ratios
  
  #View variable importances
  varImp(model$finalModel)
  table(training.data.raw$default_oct)
  
  ##########################################
  # Cleaning of test data (same cleaning as actual data)                 
  ##########################################
  
  # import the testing data set and perform general cleaning
  colClasses=c("character","numeric",rep('factor',3),'numeric', rep('factor',6), rep("numeric",12))
  test.data.raw <- read.csv('C:/Users/Kosta/Desktop/Modelling Examples/test.csv',header=T,
                                na.strings=c(""), colClasses = colClasses)
  
  #check missing values - none... check the coding though
  sapply(test.data.raw,function(x) sum(is.na(x)))
  
  #check unique vals
  sapply(test.data.raw, function(x) length(unique(x)))
  
  # get rid of the STRING NA's
  test.data.raw[test.data.raw=="NA"]<-NA
  
  # visualize the missing values 
  missmap(test.data.raw, main = "Missing values vs observed")
  
  # fix education levels -> 5 and 6 levels should become 4
  test.data.raw$education[test.data.raw$education=="5.0" |
                            test.data.raw$education=="6.0" |
                            test.data.raw$education=="0.0"]<-as.factor('4.0')
  # fix marriage status
  test.data.raw$marriage[test.data.raw$marriage=='0.0'] <- as.factor('3.0')
  
  # fix pay_x columns - combine the values of -2 and 0 into the same bing as -1 for "duly paid"
  test.data.raw$pay_1[test.data.raw$pay_1=='-2.0' | test.data.raw$pay_1=='0.0' ] <- as.factor('-1.0')
  test.data.raw$pay_2[test.data.raw$pay_2=='-2.0' | test.data.raw$pay_2=='0.0' ] <- as.factor('-1.0')
  test.data.raw$pay_3[test.data.raw$pay_3=='-2.0' | test.data.raw$pay_3=='0.0' ] <- as.factor('-1.0')
  test.data.raw$pay_4[test.data.raw$pay_4=='-2.0' | test.data.raw$pay_4=='0.0' ] <- as.factor('-1.0')
  test.data.raw$pay_5[test.data.raw$pay_5=='-2.0' | test.data.raw$pay_5=='0.0' ] <- as.factor('-1.0')
  test.data.raw$pay_6[test.data.raw$pay_6=='-2.0' | test.data.raw$pay_6=='0.0' ] <- as.factor('-1.0')
  
  
  # for numeric take the mean
  test.data.raw$pay_amt6[is.na(test.data.raw$pay_amt6)] <- mean(training.data.raw$pay_amt6,na.rm=T)
  test.data.raw$bill_amt6[is.na(test.data.raw$bill_amt6)] <- mean(training.data.raw$bill_amt6,na.rm=T)
  test.data.raw$pay_amt5[is.na(test.data.raw$pay_amt5)] <- mean(training.data.raw$pay_amt5,na.rm=T)
  test.data.raw$bill_amt5[is.na(test.data.raw$bill_amt5)] <- mean(training.data.raw$bill_amt5,na.rm=T)
  
  # for factor get the mode for pay_6 and pay_5
  all.levels <- unique(training.data.raw$pay_6)
  mode.level <- all.levels[which.max(tabulate(match(all.levels, training.data.raw$pay_6)))]
  test.data.raw$pay_6[is.na(test.data.raw$pay_6)] <- mode.level
  
  all.levels <- unique(training.data.raw$pay_5)
  mode.level <- all.levels[which.max(tabulate(match(all.levels, training.data.raw$pay_5)))]
  test.data.raw$pay_5[is.na(test.data.raw$pay_5)] <- mode.level
  
  #condensing pay variables because they can be related to each other (ie. sequential)
  #indicator of number of on-time payments in the period provided
  test.data.raw$pay_ontime <- rowSums(test.data.raw[,pay] == -1)
  
  #expect that bill_amt and pay_amt are related as well, and both are related to limit_bal
  #create new column for difference between bill and payment (not using ratio because of 0 bills) -- represents balance after paying
  test.data.raw$pay_diff1 <- test.data.raw$bill_amt1 - test.data.raw$pay_amt1
  test.data.raw$pay_diff2 <- test.data.raw$bill_amt2 - test.data.raw$pay_amt2
  test.data.raw$pay_diff3 <- test.data.raw$bill_amt3 - test.data.raw$pay_amt3
  test.data.raw$pay_diff4 <- test.data.raw$bill_amt4 - test.data.raw$pay_amt4
  test.data.raw$pay_diff5 <- test.data.raw$bill_amt5 - test.data.raw$pay_amt5
  test.data.raw$pay_diff6 <- test.data.raw$bill_amt6 - test.data.raw$pay_amt6
  ##
  
  ##########################################
  # Get predictions and make a CSV         #
  ##########################################
  
  test.data <- test.data.raw[ , names(test.data.raw) != "customer_id"]
  # get the predictions
  predictions <- predict(model, newdata=test.data, type="prob")
  
  # get a column of customers' Id's
  cust.ids <- test.data.raw[ , names(test.data.raw) == "customer_id"]
  # combine the predictions into a dataframe
  results <- data.frame(col1=cust.ids, col2=predictions$yes)
  # name the dataframe as per requirement
  names(results) <- c('customer_id','pr_y')
  
  make_submission('Kosta_Blank_Random_Forest_v3', results)
  
  
  
  
  make_submission <- function(name, dataframe) {
    ## Parameters
    ## ----------
    ##  name:               string, your name
    ##  submission_number:  int or string
    ##  dataframe:         data.frame [5999, 2], customer ids and 
    ##                          predicted probabilties on the test set
    
    # check input data frame appropriate shape
    if(nrow(dataframe) != 5999 | ncol(dataframe) != 2) {
      stop('data frame is wrong shape. Expecting [5999, 2]')
      # check input data for correct column names
    } else if(names(dataframe)[1] != 'customer_id') {
      stop('column 1 name is wrong. Expecting customer_id')
    }
    
    filename <- paste0(name, '.csv')
    write.csv(dataframe, filename, row.names = FALSE)
    
    return(paste(filename, 'created'))
  }
  
