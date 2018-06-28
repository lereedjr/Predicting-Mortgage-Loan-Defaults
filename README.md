# Predicting-Mortgage-Loan-Defaults
The goal of the project is to see if the data from Lending Club can be used to predict mortgage defaults.  

# Description
This project stemmed from the need to find potential data that could be a good predictor for insurance claims and/or payment defaults on homeowners insurance policies.  I mainly focused on how predictable the loan data is on mortgage defaults because I think this provides an ample base for the predictability of the data.  I used a supervised learning technique in R to analyze this data.  The dataset came from the following site https://www.kaggle.com/wordsforthewise/lending-club. 

# Data Observations
Some questions that stemmed from data exploration.

* Is the mortgage data a good predictor of defaults?
* Which variables are highly correlated with loan status?
* Will credit information be a highly correlated variable? 

The original dataset had 150 variables and 1,646,801 observations.  The dataset consisted of observations pertaining to car loans, debt consolidation, educational, wedding, etc.

# Data Cleaning
The dataset was consolidated to only focus on mortgage loans (excluding those taken out for home improvement purposes).  I used the following code to remove the unnecessary loan types from the dataset.

accepted <- accepted[accepted$purpose!="" & accepted$purpose!="car"& accepted$purpose!="educational" & accepted$purpose!="major_purchase" & accepted$purpose!="other" & accepted$purpose!="vacation" & accepted$purpose!="credit_card" & accepted$purpose!="medical" & accepted$purpose!="renewable_energy" & accepted$purpose!="wedding" & accepted$purpose!="debt_consolidation" & accepted$purpose!="moving" & accepted$purpose!="small_business" & accepted$purpose!="home_improvement", ]

At this point in the process there are 7,286 observations and 150 variables. I then cleaned up some of the punctuations from variables like emp.title.
The next step was to change all of the character variables to numeric values.  Loan Status which had options like fully charged, charged off, current, etc. were changed to either 0 or 1.  1 represented "no default" and 0 represented "default".

accepted$loan_status[accepted$loan_status == "Fully Paid"] <- 1                                                                         
accepted$loan_status[accepted$loan_status == "Charged Off"] <- 0                                                                         
accepted$loan_status[accepted$loan_status == "Current"] <- 1                                                                             
accepted$loan_status[accepted$loan_status == "Default"] <- 0                                                                             
accepted$loan_status[accepted$loan_status == "Does not meet the credit policy. Status:Charged off"] <- 0                                 
accepted$loan_status[accepted$loan_status == "Does not meet the credit policy. Status:Charged Off"] <- 0                                 
accepted$loan_status[accepted$loan_status == "Does not meet the credit policy. Status:Fully Paid"] <- 1                                 
accepted$loan_status[accepted$loan_status == "In Grace Period"] <- 1                                                                     
accepted$loan_status[accepted$loan_status == "Late (16-30 days)"] <- 0                                                                   
accepted$loan_status[accepted$loan_status == "Late (31-120 days)"] <- 0                                                                 

The other character variables that were edited were emp_length, grade, home_ownership, verification_status, pymnt_plan, purpose, initial_list_status, application_type, verification_status_joint, disbursement_method, debt_settlement_flag, and settlement_status.  Then the character variables were changed to numeric variables using the code below.

accepted_3$id <- as.numeric(accepted_3$id)                                                                                               
accepted_3$issue_d <- as.numeric(accepted_3$issue_d)                                                                                     
accepted_3$addr_state <- as.numeric(accepted_3$addr_state)                                                                               
accepted_3$earliest_cr_line <- as.numeric(accepted_3$earliest_cr_line)                                                                   
accepted_3$last_pymnt_d <- as.numeric(accepted_3$last_pymnt_d)                                                                           
accepted_3$last_credit_pull_d <- as.numeric(accepted_3$last_credit_pull_d)                                                               
accepted_3$sec_app_earliest_cr_line <- as.numeric(accepted_3$sec_app_earliest_cr_line)                                                   
accepted_3$debt_settlement_flag_date <- as.numeric(accepted_3$debt_settlement_flag_date)                                                 
accepted_3$settlement_date <- as.numeric(accepted_3$settlement_date)                                                                     
accepted_3$term <- as.numeric(accepted_3$term)                                                                                           
accepted_3$grade <- as.numeric(accepted_3$grade)                                                                                         
accepted_3$emp_length <- as.numeric(accepted_3$emp_length)                                                                               
accepted_3$home_ownership <- as.numeric(accepted_3$home_ownership)                                                                       
accepted_3$verification_status <- as.numeric(accepted_3$verification_status)                                                             
accepted_3$loan_status <- as.numeric(accepted_3$loan_status)                                                                             
accepted_3$pymnt_plan <- as.numeric(accepted_3$pymnt_plan)                                                                               
accepted_3$purpose <- as.numeric(accepted_3$purpose)                                                                                     
accepted_3$initial_list_status <- as.numeric(accepted_3$initial_list_status)                                                             
accepted_3$application_type <- as.numeric(accepted_3$application_type)                                                                   
accepted_3$verification_status_joint <- as.numeric(accepted_3$verification_status_joint)                                                 
accepted_3$disbursement_method <- as.numeric(accepted_3$disbursement_method)                                                             
accepted_3$debt_settlement_flag <- as.numeric(accepted_3$debt_settlement_flag)                                                           
accepted_3$settlement_status <- as.numeric(accepted_3$settlement_status)                                                                 

Then the missing values were replaced using the code below.

for(i in 1:ncol(accepted_3)){                                                                                                           
  accepted_3[is.na(accepted_3[,i]), i] <- mean(accepted_3[,i], na.rm = TRUE)                                                             
}

I then viewed that data after cleaning.                                                                                                  
summary(accepted)                                                                                                                       
str(accepted)

# EDA

Below is the loan status count after the data was reduced to mortgage loans only.

accepted_3 %>% group_by(loan_status) %>% summarise(count = n())                                                                         
 A tibble: 2 x 2                                                                                                                         
  loan_status count                                                                                                                     
        <dbl> <int>                                                                                                                     
           0  1044                                                                                                                     
           1  6224                                                                                                                     

I also removed some of the dti outliers.        

hist(accepted$dti)                                                                                                                       
remove_outliers <- function(x, na.rm = TRUE, ...) {                                                                                          qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)                                                                                H <- 1.5 * IQR(x, na.rm = na.rm)                                                                                                        y <- x                                                                                                                                  y[x < (qnt[1] - H)] <- NA                                                                                                                y[x > (qnt[2] + H)] <- NA                                                                                                                y                                                                                                                                   
 }                                                                                                                                       
dti_no_outliers <- remove_outliers(accepted$dti)                                                                                         
hist(dti_no_outliers)                                                                                                                   

I created a few visuals to take a look at the credit scores.

hist(accepted_2$fico_range_high)

![High Credit Historam](hist_high.png)

hist(accepted_2$fico_range_low)

![Low Credit Histogram](hist_low.png)

The total balance of the accounts was reviewed in the plot below.  The loan amounts ranged from $100,000 to %500,000 which is a typical range for mortgage loans.

hist(accepted_3$tot_cur_bal, xlab = "Total Current Balance", main = "Histogram of Current Balance of Accounts", col = topo.colors(10))

![Total Current Blanace](balance.png)

# Models
The following packages need to be loaded for each model and graphs.

library(dplyr)                                                                                                                           
library(ggplot2)                                                                                                                         
library(caret)                                                                                                                           
library(rpart)
library(rpart.plot)                                                                                                                     
library(randomForest)                                                                                                                   
library(corrplot)                                                                                                                       
library(e1071)                                                                                                                           
library(xgboost)                                                                                                                         
library(stringr)                                                                                                                         library(tm)                                                                                                                             
library(rms)                                                                                                                             
library(glmnet)                                                                                                                         
library(pscl)
library(ROCR)

#### Logistic Regression Model

I first split my data into a test and training set.  I then fit the model and the results produced nothing entirely significant.

set.seed(123)                                                                                                                           
ind = sample(2, nrow(accepted_6), replace = TRUE, prob=c(0.7,0.3))                                                                       
trainloan = accepted_6[ind == 1,]                                                                                                       
testloan = accepted_6[ind == 2,]                                                                                                         
model <- glm(loan_status ~., family=binomial(link='logit'),data=trainloan)                                                               
summary(model)                                                                                                                           

![GLM](GLM.PNG)

However after running the anova model it produced some better results to work with.  I utilized the deviance statistic and the model statistics to determine which variables could be removed in order to run the Random Forsest and Decision Tree models.

anova(model, test="Chisq")

![ANOVA](ANOVA.PNG)

#### Random Forest Model

set.seed(123)                                                                                                                           
indrf = sample(2, nrow(accepted_6), replace = TRUE, prob=c(0.7,0.3))                                                                     
trainloanrf = accepted_6[indrf == 1,]                                                                                                   
testloanrf = accepted_6[indrf == 2,]                                                                                                     
loan.rf = randomForest(loan_status ~ ., data=trainloanrf, importance = T)                                                               
loan.rf                                                                                                                                 

![Random Forest](RandomForest1.PNG)

loan.prediction = predict(loan.rf, testloanrf)                                                                                           
confusionMatrix(table(loan.prediction, testloanrf$loan_status))

Confusion Matrix and Statistics

![Random Forest](RandomForest.PNG)              
                         
The importance variables are shown below.

![Variable Importance](importance.png)

#### Decision Tree Model

set.seed(123)                                                                                                                           
inddt = sample(2, nrow(accepted_6), replace = TRUE, prob=c(0.7,0.3))                                                                     
trainloandt = accepted_6[inddt == 1,]                                                                                                   
testloandt = accepted_6[inddt == 2,]                                                                                                     
confusionMatrix(table(predictions, testloandt$loan_status))                                                                             

![Decision Tree Confusion Matrix](DecisionTree.PNG)

![Decision Tree](Decision_Tree.png)

prune.tree = prune(loan.rp, cp = loan.cp)                                                                                               
rpart.plot(prune.tree,tweak=1.3)                                                                                                         
predictions.pt = predict(prune.tree, testloandt, type="class")                                                                           
confusionMatrix(table(predictions.pt, testloandt$loan_status))

![Decision Tree Pruned](DecisionTreePruned.PNG)

![Decision Tree Pruned](Decision_Tree_Pruned.png)


# Analysis

* The model that performed the best was the Random Forest model with 90.5% Accuracy.
* The Decision Tree Model had a 90.2% Accuracy.
* The GLM had a 90.1% Accuracy.
* Credit score was an important feature for the Random Forest Model.
* Each model didn't out perform the other.  The Random Forest Model just performed slightly better than the other models.
* A lot of the variables were highly correlated and were good indicators of defaults versus no defaults.

# Conclusion

* I believe there is high correlation between loan status and credit score information.
* I think that this data is very applicable to homeowners insurance and can be a great predictor for loss information.
* I would like to take some next steps and apply the mortgage loan analysis to actual homeowners insurance data for further support.

# References 
https://www.kaggle.com/wordsforthewise/lending-club
