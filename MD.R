#################################################################
## Marketing analytics                                         ##
## Lecture 3 Exercise in Individual Market Response Models	   ##
## Logistic Regression                                         ##
## admittance Data Exercise                                    ##
##                                                             ## 
#################################################################

# free memory
#rm(list = ls())
#gc()


# Load files from a directory and merge them to one file
#getwd() 
#setwd("C:/test/")


# Read the Data
data<-read.csv("F:\\Users\\saleh.fares\\Downloads\\adult_income.csv",header=T)




# Show the first few rows of the data
head(data)


#Show attributes  
attributes(data)

# Recode ? to NA
data$age[data$age==" ?"] <- NA
data$workclass[data$workclass==" ?"] <- NA
data$education_num[data$education_num==" ?"] <- NA
data$marital_status[data$marital_status==" ?"] <- NA
data$occupation[data$occupation==" ?"] <- NA
data$race[data$race==" ?"] <- NA
data$sex[data$sex==" ?"] <- NA
data$capital_gain[data$capital_gain==" ?"] <- NA
data$capital_loss[data$capital_loss==" ?"] <- NA
data$hours_per_week[data$hours_per_week==" ?"] <- NA
data$native_country[data$native_country==' ?'] <- NA
data$income_high[data$income_high==" ?"] <- NA


# Omit all na rows since we have 30K record and the NA rows are only 2399
row.has.na <- apply(data, 1, function(x){any(is.na(x))})

sum(row.has.na)

data.filtered <- data[!row.has.na,]

#Display data  
summary(data.filtered)


#Convert tot dummy
results <- fastDummies::dummy_cols(data.filtered,select_columns = "income_high" ,remove_first_dummy = TRUE)

results2 <- fastDummies::dummy_cols(results,select_columns = "sex" ,remove_first_dummy = TRUE)

df = subset(results2, select = -c(sex,income_high) )


# Convert categorical variables to numeric

must_convert<-sapply(df,is.factor)       
stage2<-sapply(df[,must_convert],unclass)
f1<-cbind(df[,!must_convert],stage2) 

colnames(f1)[colnames(f1)=="sex_ Female"] <- "sex_Female"
#convert some columns to a factor to indicate that these columns should be treated as a categorical variable
f1$workclass <- factor(f1$workclass)
f1$marital_status <- factor(f1$marital_status)
f1$occupation <- factor(f1$occupation)
f1$race <- factor(f1$race)
f1$native_country <- factor(f1$native_country)
f1$sex_Female <- factor(f1$sex_Female)

finaldata = subset(f1, select = -c(ID) )
head(finaldata)

#
#At this stage the data clean does not have NA values and it is all numeric 
#Ready to be splitted and fitted
#
#
library(caret)
library(klaR)

split=0.50
trainIndex <- createDataPartition(finaldata$income_high_Yes, p=split, list=FALSE)
data_train <- finaldata[ trainIndex,]
data_test <- finaldata[-trainIndex,]

model <- glm( income_high_Yes ~ age + education_num  + hours_per_week  + workclass + marital_status + occupation + native_country + sex_Female, family = "binomial", data=data_train)
summary(model)

library(MASS)

# Step AIC in Both
#
stepAIC(model,direction = "both")
#income_high_Yes ~ age + education_num + hours_per_week + workclass + 
#marital_status + occupation + sex_Female as the best model


modelfinal <- glm( income_high_Yes ~ age + education_num + hours_per_week + workclass + 
                     marital_status + occupation + sex_Female , family = "binomial", data=data_train)
summary(modelfinal)


# Now we get the individual risk factors (or odds ratios).
# The 2 at the end is the number of decimals we want.
round(exp(coef(modelfinal)), 2) 



# make predictions


x_test <- subset(data_test, select = -c(income_high_Yes) )
y_test <- subset(data_test, select = c(income_high_Yes) )
predictions <- predict(modelfinal, x_test)
# summarize results
head(predictions)


for(i in 1:length(predictions)){
  if(predictions[i] >= 0.05){
    
    predictions[i] = 1
    
  } else {
    
    predictions[i] = 0
    
  }

}

confusionMatrix(predictions$class, y_test)
