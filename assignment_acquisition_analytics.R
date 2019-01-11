##------------Assignment Acquisition Analytics Bank Marketing---------------------##

options(max.print=100000)
library(ggplot2)
library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)
library(dplyr)

# Load bank marketing data
bank_data<- read.csv("bank_marketing.csv")

### Checkpoint - 1 Data preparation
# First lets add the unique ID to the data frame
bank_data$ID <- seq.int(nrow(bank_data))
bank_data <- bank_data[, c(22,1:21)]

# Checking structure of dataset 
str(bank_data)

# Summary of dataset
summary(bank_data)

## Data understanding for client information

# Checking response rate of prospect customer

response <- 4640/(36548+4640)*100
response

# Checking missing values
sum(is.na(bank_data))

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram(binwidth=5, colour = "red")

# Let's check the outlier in the variables 
quantile(bank_data$age,seq(0,1,0.01))

# Box plot 
boxplot(bank_data$age)

# Capping the upper values of age with 71.
bank_data[(which(bank_data$age>71)),]$age <- 71

# Binning the age variable and store it into "binning.age".
bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"
bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket
agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)

# changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

# Let's see the response rate of each age bucket in the plot
ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20) # only 75 observations are there

View(Bank_data_age20) # only 75 entries are there
summary(Bank_data_age20)

# Checking structure of dataset
str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job
levels(bank_data$job)

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking Marital status
summary(bank_data$marital)

# Let's replace Unknown level to married
levels(bank_data$marital)[4] <- "married"

# Plotting marital status
plot_response(bank_data$marital,"marital")

# Let's see the education variables
plot_response(bank_data$education,"Education")

# Reducing the levels of education variable
levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot
plot_response(bank_data$education,"Education_levels")

#-------------------------------------------------------
# Let's see the default variable
table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-6] # here large amount of data is missing(unknown) so we will drop this variable

#-------------------------------------------------------

# Let's understand the housing variables 
summary(bank_data$housing)

plot_response(bank_data$housing, "Housing") #we dont have the informatio for 990 prospects

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"
summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status") # we dont have the informatio for 990 prospects
#-------------------------------------------------------
# some observations
# 1
# FOr age response rate varies significantly across the age groups, with the groups 16-20 and 
# senior citizens having high response rates

# 2
# Marital status, education do not seem to be good predictors of response
# Housing loan, personal loan do not seem to be good predictors of response

#-------------------------------------------------------

## Data understanding for campaign information

#  Next variable is Contact, Let's see the response rate of each mode 
summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

#-------------------------------------------------------

# Next variable is "Month" i.e contact month. 
plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Let's do the same of "day_of_week" variable
plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------

# Now, Let's see the "duration" variable: Which is Quantitative variable
# Let's check the histogram 
ggplot(bank_data,aes(duration))+geom_histogram(colour = "red")

# Let's see the summary of this variable once 
summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean) # higher the duration higher the response chance

# no - 219.0389
# yes - 532.4904

bank_data <- bank_data[,-23]

## Definitely the outlier is present in the dataset
# So let's check the percentile distribution of duration 
quantile(bank_data$duration,seq(0,1,0.01))

# So, capping the duration seconds at 99% which is 1271.3sec 
bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 
ggplot(bank_data,aes(duration))+geom_histogram(colour = "red")

#-------------------------------------------------------

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 
summary(bank_data$campaign)

# Let's see the percentile distribution of this variable
boxplot(bank_data$campaign)

quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14
bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot
ggplot(bank_data,aes(campaign))+geom_histogram(colour = "red")

#-------------------------------------------------------

# Next variable is "pdays"
# Let's first convert this variable to factor type
bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary
summary(bank_data$pdays)

levels(bank_data$pdays)

# Reducing the levels of this variable to 3.
levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"

# Also,lets see the respose rate of each levels. 
plot_response(bank_data$pday,"Pday")

# Number of prospects under each category
table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"

summary(bank_data$previous)

plot_response(bank_data$previous,"Previous_contacts")

#-------------------------------------------------------

# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')

summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#-------------------------------------------------------
# some observations
# 1 Month of contact seems a significant predictor of response rate
# 2 Day of month does not seem very important
# 3 Call duration seems an important predictor

#-------------------------------------------------------

## #-- social and economic context attributes

# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram(colour = "red")

#-------------------------------------------------------


# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram(colour = "red")

#-------------------------------------------------------

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)

#-------------------------------------------------------

### Checkpoint 2 
# Model Building 
# Build a logistic regression model without using the variable 'duration' 

# Removing binning variables and ID column

bank_data <- bank_data[, -22]
bank_data <- bank_data[, -1]

#creating dummy variables

bank_data$response <- as.integer(bank_data$response)

bank_data <- dummy.data.frame(bank_data)

bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

#---------------------------------------------------------    

# splitting into train and test data

set.seed(1)

split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)

train <- bank_data[split_indices, ]

test <- bank_data[!split_indices, ]

nrow(train)/nrow(bank_data)

nrow(test)/nrow(bank_data)

#---------------------------------------------------------    

## generating models
model_1 = glm(response ~ ., data = train, family = "binomial")
summary(model_1)

# Null deviance: 20299, Residual deviance: 11606, AIC: 11702
# Notice that there are many insignificant variables in the model

# we will use step-wise function to remove the extremely insignificant variables
model_2<- stepAIC(model_1, direction="both")
summary(model_2)
# AIC: 11670

# As per our goal we are modeling without "duration" variable
model_3<- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                jobtechnician + jobunemployed + educationprofessional.course + 
                educationTertiary_Education + contactcellular + monthaug + 
                monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                day_of_weekmon + day_of_weekthu + campaign + pdaysContacted_in_first_10days + 
                pdaysContacted_after_10days + previousLess_than_3_times + 
                poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                euribor3m, family = "binomial", data = train)
summary(model_3)

#remove insignificant variables and multicollinear ones from the model on the basis of VIF and p-value
vif(model_3)

# "emp.var.rate" variable. has very high VIF value so remove it
# build model 4 excluding "emp.var.rate"

model_4<- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                jobtechnician + jobunemployed + educationprofessional.course + 
                educationTertiary_Education + contactcellular + monthaug + 
                monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                day_of_weekmon + day_of_weekthu + campaign + pdaysContacted_in_first_10days + 
                pdaysContacted_after_10days + previousLess_than_3_times + 
                poutcomefailure + cons.price.idx + cons.conf.idx + 
                euribor3m, family = "binomial", data = train)
summary(model_4)
vif(model_4)

# "previousLess_than_3_times" variable. has very high VIF value so remove it
# build model 5 excluding "previousLess_than_3_times"

model_5<- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                jobtechnician + jobunemployed + educationprofessional.course + 
                educationTertiary_Education + contactcellular + monthaug + 
                monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                day_of_weekmon + day_of_weekthu + campaign + pdaysContacted_in_first_10days + 
                pdaysContacted_after_10days + 
                poutcomefailure + cons.price.idx + cons.conf.idx + 
                euribor3m, family = "binomial", data = train)
summary(model_5)
vif(model_5)

# we have reached optimum values of VIF, so next base on P-value we will consider the variables

# monthjun has p value 0.42425, so insignificant. Removing
model_6<- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                jobtechnician + jobunemployed + educationprofessional.course + 
                educationTertiary_Education + contactcellular + monthaug + 
                monthmar + monthmay + monthnov + day_of_weekfri + 
                day_of_weekmon + day_of_weekthu + campaign + pdaysContacted_in_first_10days + 
                pdaysContacted_after_10days + 
                poutcomefailure + cons.price.idx + cons.conf.idx + 
                euribor3m, family = "binomial", data = train)
summary(model_6)

# jobunemployed is insignificant p value 0.29506 Removing
model_7<- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                jobtechnician + educationprofessional.course + 
                educationTertiary_Education + contactcellular + monthaug + 
                monthmar + monthmay + monthnov + day_of_weekfri + 
                day_of_weekmon + day_of_weekthu + campaign + pdaysContacted_in_first_10days + 
                pdaysContacted_after_10days + 
                poutcomefailure + cons.price.idx + cons.conf.idx + 
                euribor3m, family = "binomial", data = train)
summary(model_7)

# day_of_weekthu is insignificant p value 0.272924  Removing
model_8<- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                jobtechnician + educationprofessional.course + 
                educationTertiary_Education + contactcellular + monthaug + 
                monthmar + monthmay + monthnov + day_of_weekfri + 
                day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                pdaysContacted_after_10days + 
                poutcomefailure + cons.price.idx + cons.conf.idx + 
                euribor3m, family = "binomial", data = train)
summary(model_8)

# educationprofessional.course is insignificant p value 0.176772  Removing
model_9<- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                jobtechnician + educationTertiary_Education + contactcellular + monthaug + 
                monthmar + monthmay + monthnov + day_of_weekfri + 
                day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                pdaysContacted_after_10days + 
                poutcomefailure + cons.price.idx + cons.conf.idx + 
                euribor3m, family = "binomial", data = train)
summary(model_9)

# day_of_weekfri is insignificant p value 0.110097  Removing
model_10<- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                jobtechnician + educationTertiary_Education + contactcellular + monthaug + 
                monthmar + monthmay + monthnov + 
                day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                pdaysContacted_after_10days + 
                poutcomefailure + cons.price.idx + cons.conf.idx + 
                euribor3m, family = "binomial", data = train)
summary(model_10)

# Although, all variables have a p value below 0.05, the number of variables is still too large.
# we can continue removing the variables till the significance level < 0.001

# remove jobadmin.
model_11<- glm(formula = response ~ jobretired + jobstudent + 
                 jobtechnician + educationTertiary_Education + contactcellular + monthaug + 
                 monthmar + monthmay + monthnov + 
                 day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + 
                 poutcomefailure + cons.price.idx + cons.conf.idx + 
                 euribor3m, family = "binomial", data = train)
summary(model_11)

# remove jobtechnician
model_12<- glm(formula = response ~ jobretired + jobstudent + 
                 educationTertiary_Education + contactcellular + monthaug + 
                 monthmar + monthmay + monthnov + 
                 day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + 
                 poutcomefailure + cons.price.idx + cons.conf.idx + 
                 euribor3m, family = "binomial", data = train)
summary(model_12)

# remove educationTertiary_Education
model_13<- glm(formula = response ~ jobretired + jobstudent + 
                 contactcellular + monthaug + 
                 monthmar + monthmay + monthnov + 
                 day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + 
                 poutcomefailure + cons.price.idx + cons.conf.idx + 
                 euribor3m, family = "binomial", data = train)
summary(model_13)

# remove monthnov
model_14<- glm(formula = response ~ jobretired + jobstudent + 
                 contactcellular + monthaug + 
                 monthmar + monthmay + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure + cons.price.idx + cons.conf.idx + 
                 euribor3m, family = "binomial", data = train)
summary(model_14)

# remove monthaug 
model_15<- glm(formula = response ~ jobretired + jobstudent + 
                 contactcellular + monthmar + monthmay + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure + cons.price.idx + cons.conf.idx + 
                 euribor3m, family = "binomial", data = train)
summary(model_15)

# remove jobstudent
model_16<- glm(formula = response ~ jobretired + contactcellular + monthmar + monthmay + 
                 day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure + cons.price.idx + cons.conf.idx + 
                 euribor3m, family = "binomial", data = train)
summary(model_16)
vif(model_16)

# AIC: AIC: 16129 Null deviance: 20299 Residual deviance: 16103
## In final model_16 a]we have *** meaning high significance.

##**** Conclusion from model
# job as retired, contact as cellular, months mar and may, day monday, ampaign, contacted in and after 10 days
# previous outcome as failure, cons.price.idx, cons.conf.idx, euribor3m are important parameters
# to response

#Finalizing model_16
final_model<-model_16

## MODEL EVALUATION

#PRediciton on test data
test_pred = predict(final_model, type = "response", newdata = test[,-61])

summary(test_pred)

# Putting prediction in test data frame
test$pred_probability <- test_pred

# We are getting probability range between 1.14% to 92% with mean as 11.27% and median as 6.192%
# Third qurtile as 10.63%

# we have arrange data in decreasing order further in next points
#--------------------------------------------------------- 

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

test_predicted_response <- factor(ifelse(test_pred >= 0.50, "yes", "no"))

# # Creating confusion matrix for identifying the model evaluation.
conf <- confusionMatrix(test_predicted_response, test$response, positive = "yes")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  test_predicted_response <- factor(ifelse(test_pred >= cutoff, "yes", "no"))
  conf <- confusionMatrix(test_predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    
summary(test_pred)
# Creating cutoff values from 0.01141 to 0.92081 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.011,.92,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------   
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.04)]
cutoff

# Let's choose a cutoff value of 7.52% for final model

test_predicted_response <- factor(ifelse(test_pred >= 0.0752, "yes", "no"))

conf_final <- confusionMatrix(test_predicted_response, test$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

### checkpoint 3 
# Create a data frame with the variables:
# prospect ID, actual response, predicted response, 
# predicted probability of response, duration of call in seconds, and cost of call

# Lets add prospect ID to the dataframe
test$ID <- seq.int(nrow(test))

newdata <- select(test, ID, duration, response, pred_probability)

newdata <- mutate(newdata, predicted_response = factor(ifelse(newdata$pred_probability >= 0.0752, "yes", "no")), call_cost = 0.033*(duration) + 0.8)

newdata <- newdata[, c(1:3,5,4,6)]

#---------------------------------------------------------    

### Checkpoint 4
# Find the number of top X% prospects you should target to meet the business objective
# Report the average call duration for targeting the top X% prospects to the CMO (report this as a comment in the R file)


##### HERE CONSIDERING MODEL EVALUATION FOR ONLY TEST DATA, NOT CONSIDERING THE WHOLE DATA
##### THERE WILL NOT BE MUCH DIFFERNCE BETWEEN TWO RESULT, SO DOING ON TEST ONLY
##### (IT IS PERFORM SEPARATELY AND FOUND NOT SIGNIFICANT DIFFFERNECE) #####


## For train already evaluated the results and for test we are evaluating below

# sorting the probabilities in decreasing order 
newdata <- newdata[order(newdata$pred_probability, decreasing = T), ]

# Save the copy of final dataframe
write.csv(newdata,"newdata_final_DF.csv")

#---------------------------------------------------------  

# plotting the lift chart

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift
newdata$response <- as.factor(ifelse(newdata$response=="yes",1,0))

LG = lift(newdata$response, newdata$pred_probability, groups = 10)

## We should target the top 5 buckets, so it will be targetting 80% responders as per objective
## so we just have to call 6178 people to get 80% response

## The average call duration for targetting top 50% data

# our data is in descending order of probabilities so can consider top 50% data
nrow(newdata)*0.5 # 50% data have 6178 rows

avg_call_top_50 <- mean(head(newdata$duration,6178))
avg_call_top_50
# average call duration for targettingprospect is 261.96 seconds

#--------------------------------------------------------- 
### cost optimization
# total cost when call all people is
total_cost <- sum(newdata$call_cost)
total_cost # 112725 INR

# total cost when call top 50% people
total_cost_top_50 <- sum(head(newdata$call_cost, 6178))
total_cost_top_50 # 58350.37

# the lift is 1.593391 as per created table

#####------- Extra assumption to understanding -------#####
# as per lecture guidline cost is
# for all people
12356/1392

# for top 50 decile
6178/1109
#####-------#####

#--------------------------------------------------------- 

### checkpoint 5
# Create a lift chart
# The x-axis contains the number of prospects contacted; the y-axis contains the ratio: 
# response rate using the model/ response rate without using the model

# Gain Chart from LG

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart from LG

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")

# The Cumulative Lift of 3.4 for top two deciles,
# means that when selecting 20% of the records based on the model, 

# Cumulative gains and lift charts are a graphical representation of the advantage of using 
# a predictive model to choose which customers to contact.

#####-------------------------------------Conclusion------------------------------------------
## We should target the top 5 buckets, so it will be targetting 80% responders as per objective
## so we just have to call 6178 people to get 80% response

## the lift we are getting is 1.593391

## total cost when call all people is 112725 INR
## total cost when call top 50% people is 58350.37

#####------------------------------------------------------------------------------------------
