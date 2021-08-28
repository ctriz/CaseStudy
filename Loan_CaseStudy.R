# setwd("C:\Upgrad\Loan Case Stuy -Group")

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(scales)

loan <- read.csv("loan.csv")

length_records <- nrow(loan$id) # Total number of records
unique_records <- n_distinct(loan$id) #Distinct records

# Analyze the dataset, the data dictionary and the business problem

# Clean up redundant columns with NA, blanks

loan_clean <- loan[!sapply(loan, function (z) all(is.na(z) | z == "" | z == " " ))]
loan_clean %>% distinct(loan_clean)

# Take only relevant subset of data for analysis

loan_clean <- loan_clean %>% select(c(id,member_id, loan_amnt,
                                      term,int_rate,installment,
                                      grade,sub_grade,emp_title,emp_length,
                                      home_ownership,annual_inc,verification_status,
                                      issue_d,loan_status,purpose,title,
                                      zip_code,addr_state,dti,delinq_2yrs,	
                                      earliest_cr_line,inq_last_6mths,open_acc,
                                      pub_rec,revol_bal,revol_util,total_acc,	last_credit_pull_d))


# # ************* Data Cleaning Tasks Begins ********* 

# Format the dates

loan_clean$issue_d =  parse_date_time(x = loan_clean$issue_d,
                                            orders = c("by","yb"))
loan_clean$issue_d <- as.POSIXct(loan_clean$issue_d, format = "%m/%d/%Y")

loan_clean$earliest_cr_line =  parse_date_time(x = loan_clean$earliest_cr_line,
                                      orders = c("by","yb","jb","db","bd"))
loan_clean$earliest_cr_line <- as.POSIXct(loan_clean$earliest_cr_line, format = "%m/%d/%Y")

loan_clean$last_credit_pull_d =  parse_date_time(x = loan_clean$last_credit_pull_d,
                                               orders = c("by","yb","jb","db","bd"))
loan_clean$last_credit_pull_d <- as.POSIXct(loan_clean$last_credit_pull_d, format = "%m/%d/%Y")


# Convert to Numeric | remove extra Char 


loan_clean$int_rate <- as.numeric(gsub("[%]","",loan_clean$int_rate)) # removing the % char
loan_clean$revol_util <- as.numeric(gsub("[%]","",loan_clean$revol_util)) # removing the % char
loan_clean$emp_length <- as.numeric(gsub("[^0-9]","",loan_clean$emp_length)) # remove all non-numric
loan_clean$term <- as.numeric(gsub("[months]","",loan_clean$term)) # remove extra char
loan_clean$annual_inc <- sapply(loan_clean$annual_inc, as.integer) # convert float into integer
loan_clean$zip_code <- gsub("xx","",loan_clean$zip_code)

# Fill up the NA by doing statistical analysis

summary(loan_clean$emp_length) # Need to fill the NA's
ggplot(data = loan_clean, aes(x = "", y = emp_length)) + geom_boxplot()  # mean is 5.0
loan_clean$emp_length[is.na(loan_clean$emp_length)] <- 5 # replace NA with the mean value


# # ************* Data Cleaning Tasks Begins ************


# Split the dataset into 1. "Customer Demography" and 2. "Customer Financials" for easy analysis

cust_dmg <- loan_clean %>% 
  select(c(id,member_id,emp_title,emp_length,
                                           home_ownership,annual_inc,
                                           issue_d,loan_status,title,
                                           zip_code,addr_state)) # Customer Demography

str(cust_dmg)
head(cust_dmg,2)

cust_fin <- loan_clean %>% select(c(id,member_id,loan_amnt,term,int_rate,installment,
                                            grade,sub_grade,verification_status,
                                            issue_d,loan_status,purpose,dti,delinq_2yrs,	
                                            earliest_cr_line,inq_last_6mths,open_acc,
                                            pub_rec,revol_bal,revol_util,total_acc,last_credit_pull_d)) #Customer Financial

str(cust_fin)
head(cust_fin,2)

#write.csv(cust_dmg, file = "Cust Diag.csv", row.names = FALSE, na="", col.names=FALSE, sep=",")

# Some univariate analysis on Cust Demography

hist(cust_dmg$emp_length)
hist(cust_dmg$issue_d_yr)

# Do bivariate analysis for Customer Demography by plotting bar charts

ggplot(cust_dmg, aes(home_ownership)) + geom_bar(aes(fill=loan_status),width=0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Loan Status across Home Ownership") # loan applicants and defaulters more for rented and mortgaged homes


ggplot(cust_dmg, aes(addr_state)) + geom_bar(aes(fill=loan_status),width=0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Loan Status across States") # defaulters more in the state of CA, FL,NJ, NY, TX


ggplot(cust_dmg, aes(loan_status,emp_length)) + geom_boxplot(varwidth=T, fill="plum") +
  scale_y_continuous(name = "Mean Employment Terms",
                    breaks = seq(0, 10.0, 0.50),
                    limits=c(0, 10),0) +
      labs(title="Box plot", 
     subtitle="Loan Status grouped by Emlplyment Term",
     caption="Source: loan",
     x="Loan Status") # 25% of defaulters are employed between 1-2 yrs and 50% of it between 2-5 yrs


cust_dmg$issue_d_yr = year(cust_dmg$issue_d)

ggplot(cust_dmg, aes(issue_d_yr)) + geom_bar(aes(fill=loan_status),width=0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Defaulters across Years for specific States") # Number of loan applicants and defaulters are high in 2011


ggplot(cust_dmg, aes(annual_inc,emp_length)) + geom_point() + 
  geom_smooth(method="lm", se=F) + geom_jitter(width = .5, size=1) 


# Loan Defaulter dataset for Customer Demography

# Subset of Customer Deomgraphy as Defaulters for home Ownership as either Rented or Mortgaged for further analysis

cust_dmg_dflt = cust_dmg %>% filter(loan_status == "Charged Off") %>% 
  filter(home_ownership %in% c("RENT","MORTGAGE")) 

# Subset this farther only for states like CA, FL,NJ, NY, TX where the defaulters are high in number


cust_dmg_dflt = cust_dmg_dflt %>% 
  filter(addr_state %in% c("CA","FL","NJ","NY","TX")) 

# multivariate analysis for customer demography for defaulters

ggplot(cust_dmg_dflt, aes(addr_state,emp_length)) + 
  geom_boxplot(aes(fill=factor(home_ownership))) + 
  scale_y_continuous(name = "Mean Employment Terms",
                     breaks = seq(0, 10.0, 0.50),
                     limits=c(0, 10),0) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Box plot", 
       subtitle="Employment Terms grouped by State for defaulters",
       caption="Source: loan",
       x="State")


# Across the states for Mortaged house, 50% of the defaulters has employment term atleast less than 7 yrs
# Across the states for Rented house, 50% of the defaulters has employment term atleast than 4 yrs


# Any corelation between the continuous variable?

cor_a1 = cor(cust_dmg_dflt$emp_length,cust_dmg_dflt$annual_inc) # Low Co-relation [0.13]

# So, armed with all the information we can plot the variables that impact the loan status

ggplot(cust_dmg, aes(issue_d_yr,emp_length)) + geom_point(aes(col=loan_status,size=home_ownership)) + geom_smooth(method="lm", se=F)
  
#_________________________________________
#_________________________________________


#bivariate analysis for cust financials 

#Bar charts for discrete variables

ggplot(cust_fin, aes(grade)) + geom_bar(aes(fill=loan_status),width=0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Loan Status across Grade") # Loan applicants and defaulters high for grades B,C,D

  
ggplot(cust_fin, aes(sub_grade)) + geom_bar(aes(fill=loan_status),width=0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Loan Status across Sub Grade") # Loan status has not relationship on sub-grade


ggplot(cust_fin, aes(verification_status)) + geom_bar(aes(fill=loan_status),width=0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Loan Status across Verification Status") # Loan status has not relationship on verification_status



ggplot(cust_fin, aes(purpose)) + geom_bar(aes(fill=loan_status),width=0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Loan Status across Loan Purpose") # Loan applicant and defaulter is high for the purpose of debt consolidation

ggplot(cust_fin, aes(delinq_2yrs)) + geom_bar(aes(fill=loan_status),width=0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Loan Status across Deliquent Status of 2 years ") # # Loan applicant and defaulter is high for the value of delinq_2yrs is 0


# Subset the dataset for delinq_2yrs=0 and loan_status = default for further analysis

cust_fin_dflt = cust_fin %>%  filter(loan_status == "Charged Off") %>% 
  filter(delinq_2yrs == 0) 
  
# Scatterplot for continuous variables

ggplot(cust_fin_dflt, aes(revol_util,dti)) + geom_point(aes(col=purpose)) + geom_smooth(method="lm", se=F)

str(cust_fin_dflt)
cor_b1 = cor(cust_fin_dflt$revol_util,cust_fin_dflt$dti) # Low Co-relation [0.13]

# Bar Chart for bi-variate analysis

ggplot(cust_fin_dflt, aes(revol_util)) + geom_bar(aes(fill=purpose),width=0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Loan Status across Revol_util") # There is a huge spike of defaulters for revol_util = 0

ggplot(cust_fin_dflt, aes(dti)) + geom_bar(aes(fill=purpose),width=0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Loan Status across Dti") # There is a huge spike of defaulters for dti = 0 and 18

ggplot(cust_fin_dflt, aes(purpose)) + geom_bar(aes(fill=loan_status),width=0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Loan Purpose", 
       subtitle="Loan Status across Loan Purpose") # Huge spike for loan taken for debt consolidation

# So, armed with all the information we can plot the variables that impact the loan status

ggplot(cust_fin, aes(dti,revol_util)) + geom_point(aes(col=loan_status)) + geom_smooth(method="lm", se=F)


