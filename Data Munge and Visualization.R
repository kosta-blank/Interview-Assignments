# <company> Project
# Kosta Blank 
# 02/25/2017

library(dplyr)
library(sqldf)
library(binr)
library(Amelia)
library(jsonlite)
library(mice)
library(VIM)
library(Hmisc)
library(MASS)
library(ggplot2)
library(parallel)
library(corrplot)

setwd('C:/Users/Kosta/Desktop/Project/data-challenge-data-master')
hmda_init <- function() {
  # load the datasets into memory
  institution_df<- read.csv('2012_to_2014_institutions_data/prod/user/sam/coaf/adhoc/tjy118/data/HMDA_Data/2012_to_2014_institutions_data.csv')
  loans_df <- read.csv('2012_to_2014_loans_data/prod/user/sam/coaf/adhoc/tjy118/data/HMDA_Data/2012_to_2014_loans_data.csv')
  
  # set all string NAs to N/A
  #institution_df[institution_df=='NA'] <- NA
  #loans_df[loans_df=='NA'] <- NA
  loans_df <- as.data.frame(lapply(loans_df,function(x) if(is.character(x)|is.factor(x)) gsub("\\bNA\\b",NA,x) else x))
  
  # Instruments dataset - changing ints to factors
  institution_df[,'Agency_Code'] <- as.factor(institution_df[,'Agency_Code'])
  institution_df[,'As_of_Year'] <- as.factor(institution_df[,'As_of_Year'])
  
  # Loans dataset  - changing ints to factors 
  loans_df[,'Agency_Code'] <- as.factor(loans_df[,'Agency_Code'])
  loans_df[,'As_of_Year'] <- as.factor(loans_df[,'As_of_Year'])
loans_df[,'State_Code'] <- as.factor(loans_df[,'State_Code'])
  loans_df[,'Conforming_Limit_000'] <- as.factor(loans_df[,'Conforming_Limit_000'])
  
  #create new ID for merging
  institution_df$id <- apply( institution_df[ , c('As_of_Year','Agency_Code', 'Respondent_ID') ] , 1 , paste , collapse = "_" )
  loans_df$id <- apply( loans_df[ , c('As_of_Year','Agency_Code', 'Respondent_ID') ] , 1 , paste , collapse = "_" )
  
  # merge the dataset on the custom_id
  joined_df <- sqldf("SELECT loans_df.*, institution_df.respondent_name_ts
                     FROM loans_df INNER JOIN 
                     institution_df ON loans_df.id = institution_df.id")
  print(Sys.time())
  # create the bins for the Loan_Amount_000
  bins <- bins.quantiles(joined_df$Loan_Amount_000, 10, 12, verbose = FALSE)
  
  # get ranges list as charachters
  ranges = list()
  for(index in 1:length(bins$binct)) {
    ranges[index] <- paste(bins$binlo[index],bins$binhi[index],sep = '-')
  }
  # calculate where every loan falls (what bin it is in)
  joined_df$loan_bins <- cut(joined_df$Loan_Amount_000, breaks=c(-Inf,bins$binhi,Inf), labels=c(ranges,paste('over ', bins$binhi[length(bins$binhi)])))
  
  # sanity check
  #sanity_check_loans <- sqldf('select loan_bins,Loan_Amount_000 from joined_df')
  return(joined_df)
}

hmda_to_json <- function(df_hmda=NA, states=NA, conventional=NA, loan.purp = NA, years = NA,pretty = FALSE) {
  # check if the dataframe is null or empty
  if (!is.data.frame(df_hmda) | nrow(df_hmda)==0){ 
    stop("DataFrame cannot be null/empty.")
  }
  
  # check if the states are in list or char values
  if(is.character(states) || is.list(states)) {
    df_hmda <- df_hmda[df_hmda$State %in% states,]
  }
  # check if the user wants to see the conventional loans
  if(is.character(conventional)) {
    df_hmda <- df_hmda[df_hmda$Conventional_Conforming_Flag == toupper(conventional),]
  }
  
  # check if the user wants to chose the year
  if(is.character(years)) {
    df_hmda <- df_hmda[df_hmda$As_of_Year %in% years,]
  }
  
  # check if the user wants to see the specific loan type description
  if(is.character(loan.purp)) {
    df_hmda <- df_hmda[toupper(df_hmda$Loan_Purpose_Description) == toupper(loan.purp),]
  }
  
  # last check before getting the df back to the user - should return a json error
  if(nrow(df_hmda)==0) {
    #stop('After subsetting the number of rows is 0. No results for your subquery')
    return(toJSON('ERROR: [No rows to show. Every row was filtered out.]'))
  }
  
  return(toJSON(df_hmda, pretty = pretty))
}


######### CODE STARTS HERE ########################

# run the main method and load the data
df <- hmda_init()
cat('Done loading, merging, and cleaning the csv files.')

# test that hmda_to_json works
json_test <- hmda_to_json(df,states = list('VA','DE','MD'), years = c('2012','2014'), conventional = 'y', pretty = F)


############ #2 Quality Check ############## 
# main fields to be analyzed and checked for missing values
main_fields <- c('As_of_Year','Respondent_ID', 'Loan_Amount_000','Applicant_Income_000', 
                 'Census_Tract_Number', 'Conforming_Limit_000')

names(main_fields) <- c('Year','Respondent','Loan Amount','Appl Income','Census Tract','Conf Limit' )
# visualize all of the missing values from the data set
aggr_plot <- aggr(df[,main_fields], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, cex.number = 1,
                  labels=names(main_fields), cex.axis=.8, ylab=c("Histogram of missing data","Pattern"))

# Quality check for Loan_Amount_000 and Respondent_Name
loan_amount_descr <- describe(df$Loan_Amount_000)$counts['missing']
# if the loan amount is missing, we should delete that data (we can also impute it in the future)
if(as.integer(loan_amount_descr) != 0) {
  df <- df[!is.na(df$Loan_Amount_000),]
  cat('Deleted ', loan_amount_descr, ' rows due to missing loan amount.')
}

# check if there are any respondent names missing... should not delete those columns
respondent_name_descr <- describe(df$Respondent_Name_TS)$counts['missing']


# prepare data for imputation with Amelia
predictor_fields <- c( 'Loan_Amount_000','Applicant_Income_000', 
                       'FFIEC_Median_Family_Income','State','As_of_Year','Conforming_Limit_000') # ,'Census_Tract_Number',,

# change types of variables in order to perform imputation
df[,'FFIEC_Median_Family_Income'] <- as.numeric(as.character(df[,'FFIEC_Median_Family_Income']))
df[,'Tract_to_MSA_MD_Income_Pct'] <- as.numeric(as.character(df[,'Tract_to_MSA_MD_Income_Pct']))
df[,'Applicant_Income_000'] <- as.numeric(as.character(df[,'Applicant_Income_000']))
df[,'As_of_Year'] <- ordered(df[,'As_of_Year'])
df[,'Conforming_Limit_000'] <- ordered(df[,'Conforming_Limit_000'])

# the income of people is not well reported -> missing values need to be imputed
cores <- detectCores() - 1
# need to specify nominal and ordinal variables for imputation
imp <- amelia(df[,predictor_fields], m=5,parallel = 'multicore', ncpus = cores,noms=c('State'),ords=c('As_of_Year','Conforming_Limit_000') )

# get the average of multiple imputations for income - aggregate for better estimate
appl_inc <- 'Applicant_Income_000'
imputed_income <- cbind(imp$imputations$imp1[appl_inc],
                        imp$imputations$imp2[appl_inc],
                        imp$imputations$imp3[appl_inc],
                        imp$imputations$imp4[appl_inc],
                        imp$imputations$imp5[appl_inc])
avg_imputed_inc <- as.data.frame(rowMeans(imputed_income))
rownames(imputed_income) <- rownames(imp$imputations$imp1)
colnames(avg_imputed_inc) <- 'Real/Imputed Income'

# insert the income column with the imputed values into the original dataset
imputed_full_df <- merge(avg_imputed_inc, df, by="row.names",all.x=TRUE)
imputed_full_df$imputedIncomeFlag <- ifelse(is.na(imputed_full_df$Applicant_Income_000),1,0)
imputed_full_df$Applicant_Income_000 <- NULL
rownames(imputed_full_df) <- imputed_full_df[,1]
imputed_full_df[,1] <- NULL

df <- imputed_full_df
# make a variable to recognize each county/state
df$County_State <- as.factor(paste(df$State, df$County_Name, sep = '_'))

#######################################################################3
######### 3 visual narratives   #######################################3
#######################################################################3

# check the variables' correlation
corrplot(cor(df[,sapply(df, is.numeric)],use="complete"), method = 'number')

# might not be a great idea because people take less loans?
barplot(prop.table(table(df$As_of_Year))*100,  xlab="Years",col=c('blue'), main='Amount of Loans Taken in Each Year',ylab = 'Percent of Total Loans')
abline(h=22,v=0, col = 'red', lwd = 2)

# distribution of income (should be right skewed bell curve... it is not - bad data? )
hist(as.numeric(df$`Real/Imputed Income`),xlim = c(0,700),col="red", xlab="Income in Thousands", 
     main="Applicants Income for Loans Application", ylab='Perc',freq = TRUE)

mostFrequentLoan <- describe(df$County_State)

# get the total count of the loans per county_state
county_state_loan_counts <- as.data.frame(summary(df$County_State))
# get the df of all data from the top 10 counties with the highest amount of loans
frq_loans_df <- df[df$County_State %in% rownames(county_state_loan_counts)[1:10],]

hist(frq_loans_df$FFIEC_Median_Family_Income,ylim = c(2000,120000), xlim = c(45000,87000),col="red", xlab="Income in Thousands", 
     main="Applicants Income for Loans Application, Median Income <90K, Top 10 Counties By Number of Loan")
# the very most frequent is the 105K median income
hist(frq_loans_df$FFIEC_Median_Family_Income,ylim = c(2000,260000),xlim = c(100000,116000),col="red", xlab="Income in Thousands", 
     main="Applicants Income for Loans Application, Median Income >100K, Top 10 Counties By Number of Loan")

population_wo_top <- subset(df,  !(County_State %in% frq_loans_df$County_State))

# vs the overal population
hist(population_wo_top$FFIEC_Median_Family_Income,ylim = c(2000,120000),xlim = c(45000,87000),col="red", xlab="Income in Thousands", 
     main="Applicants Income for Loans Application, Median Income <90K, The Other Counties")
# the very most frequent is the 105K median income
hist(population_wo_top$FFIEC_Median_Family_Income,ylim = c(2000,260000),xlim = c(100000,116000),col="red", xlab="Income in Thousands", 
     main="Applicants Income for Loans Application, Median Income >100K, The Other Counties")


# method that summarizes the market size by year and state
get.bar.chart <- function(df=NA, states=NA,years=NA, purpose = NA) {
  df_temp <-fromJSON(hmda_to_json(df_hmda = df, states = states, years = years, loan.purp = purpose))
  current_directory <- paste0(getwd(),'/')
  extenstion <- '.png'
  cat(length(df_temp))
  
  # make sure to sort the years from smallest to biggest
  order(years,decreasing = FALSE)
  
  for(state in states) {
    sum_of_loans <- integer()
    for(year in years) {
      
      year_state_df <- df_temp[df_temp$State == state & df_temp$As_of_Year == year,]
      sum_of_loans <- c(sum_of_loans, sum(year_state_df$Loan_Amount_000))
      #cat('the length of the df for year:', year, 'and state:', state, 'is', dim(year_state_df))
    }
    #name <- paste(state,'summary', sep='_')
    #print( paste0(current_directory,name,extenstion))
    total_sum <- sum(sum_of_loans)
    percents <- sum_of_loans / total_sum
    names(percents) <- years

    if(is.na(purpose)) {
      purpose <- 'All'
    }
    name <- paste(state, purpose,'summary', sep='_')
    print( paste0(current_directory,name,extenstion))
    png(filename=paste0(current_directory ,name,extenstion))
    barplot(percents, col = 'blue',main = paste('State of', state, " Loan Purpose:",purpose),ylim = c(0, .5))
    
    
    dev.off()
  }
}

get.bar.chart(df, states = c('VA','MD','WV','DE','DC'), years = c('2012','2013','2014'), purpose = 'Refinance')
get.bar.chart(df, states = c('VA','MD','WV','DE','DC'), years = c('2012','2013','2014'), purpose = 'Purchase')


hist(as.numeric(df$FFIEC_Median_Family_Income),col="red", xlab="Income in THousands", 
     main="Median Family Income Distribution")

hist(as.numeric(df$Loan_Amount_000), xlim=c(0,5000)  ,col="red", xlab="Loans in THousands",  #uniformly distributed
     main="Distribuitons of Loans")

hist(as.numeric(df$Number_of_Owner_Occupied_Units),col="red", xlab="People", 
     main="Owner Occupied Units")   # nice bell curve

# important - all of our data mostly refinance


get.pie.chart <- function(field, name) {
  piepercent<- round(100*table(field)/sum(table(field)), 1)
  # Plot the chart.
  pie(table(field), labels = piepercent, main = name ,col = rainbow(length(table(field)), start=.24), clockwise = TRUE)
  legend("bottomleft",names(table(field)), cex = 0.7, bty = 'n',
         fill = rainbow(length(table(field)), start=.24))
}

get.pie.chart(df$Loan_Purpose_Description, "Loans")
get.pie.chart(df$Lien_Status_Description, "Lien")
get.pie.chart(df$Loan_Type_Description, "Loans Types")


##################################################################4
########## Analytics   ##########################################4
#################################################################4
get.similar.counties <- function(df_mini, county_state, purpose=NA, similarity.by='avg.of.loans') {
  # make sure that the df has county_state column
  if(!any(is.na(df_mini$County_State))) {
    # get the frequency of loans per county -> percent for purchase/refinance
    frequency_of_loans <- as.data.frame(df_mini %>%
                                          group_by(County_State,Loan_Purpose_Description) %>%
                                          summarise (amount_of_loans = n()) %>%
                                          mutate(percent_freq = amount_of_loans * 100 / sum(amount_of_loans)))
    # get frequency for the average loan amount for puchase/refinance
    average_amounts <-as.data.frame(df_mini %>%
                                      group_by(County_State,Loan_Purpose_Description) %>%
                                      summarise(average_loan = mean(Loan_Amount_000)))
    # mer the data sets
    full_merge <- (merge(x = frequency_of_loans, y = average_amounts, by = c('County_State','Loan_Purpose_Description'), all = TRUE))
    # check user input - what do we want to compare by?
    if(similarity.by=='avg.of.loans') {
      full_merge <- full_merge[order(full_merge$average),]
    }
    else if(similarity.by=='amount.of.loans') {
      full_merge <- full_merge[order(full_merge$amount_of_loans),]
    }
    else if(similarity.by=='type.of.loans') {
      full_merge <- full_merge[order(full_merge$percent_freq),]
    }
    else {
      stop('The values for similarity.by has to be one of the following:
           avg.of.loans,amount.of.loans, or type.of.loans ')
    }
    
    # rename the rows to help sorting
    rownames(full_merge) <- 1:nrow(full_merge)
    # add a padding on top and bottom to not run into the error of getting a negative index
    full_merge <- rbind(c(NA,NA,0,0,0),c(NA,NA,0,0,0),full_merge,c(NA,NA,0,0,0),c(NA,NA,0,0,0))
    
    # if user specified the purpose -> choose only what requested
    if(!is.na(purpose)) {
      full_merge <- full_merge[full_merge$Loan_Purpose_Description==purpose,]
      full_merge <- full_merge[which(full_merge$County_State %in% c(county_state)) + c(-2,-1,0,1,2), ]
      # make sure to return only the rows without NA
      return(toJSON(full_merge[!is.na(full_merge$County_State),]))
    }
    # if user didnt chose -> get both, the refinance and purchase info and return in sorted
    else {
      part1 <- full_merge[full_merge$Loan_Purpose_Description=='Purchase',]
      part1 <- part1[which(part1$County_State %in% c(county_state)) + c(-2,-1,0,1,2), ]
      part2 <- full_merge[full_merge$Loan_Purpose_Description=='Refinance',]
      part2 <- part2[which(part2$County_State %in% c(county_state)) + c(-2,-1,0,1,2), ]
      full_merge <- rbind(part1,part2)
      return(toJSON(full_merge[!is.na(full_merge$County_State),]))
    }
    }
  else {
    cat('Error. Make sure to have "County_State" column in the data frame.')
  }
}

# testing the method
edge_case <- fromJSON(get.similar.counties(df,'DC_',purpose = "Refinance",similarity.by = 'amount.of.loans'))
va_refin <- fromJSON(get.similar.counties(df,'VA_EMPORIA',purpose = "Refinance",similarity.by = 'amount.of.loans'))
