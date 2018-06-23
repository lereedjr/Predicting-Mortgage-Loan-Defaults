# Predicting-Mortgage-Loan-Defaults
The goal of the project is to see if the data from Lending Club can be used to predict mortgage defaults.  

# Description
This project stemmed from the need to find potential data that could be a good predictor for insurance claims and/or payment defaults on homeowners insurance policies.  I mainly focused on how predictable the loan data is on mortgage defaults because I think this provides an ample base for the predictability of the data.  I used a supervised learning technique in R to analyze this data.  The dataset came from the following site https://www.kaggle.com/wordsforthewise/lending-club. 

# Data Observations
Some questions that stemmed from data exploration.
1. Is the mortgage data a good predictor of defaults?
2. Which variables are highly correlated with loan status?
3. Will credit information be a highly correlated variable? 

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


# EDA

# Models

# Analysis

# Conclusion

# References 
