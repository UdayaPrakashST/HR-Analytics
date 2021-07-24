#__________________________________________________________________________________________________________________

#                                          LOAN CASE STUDY

#__________________________________________________________________________________________________________________

# loading required library
library(dplyr)
library(tidyr)
library(ggplot2)


# reading loan data from loan.csv file
loanData <- read.csv("loan.csv",stringsAsFactors = FALSE,check.names = FALSE)

# checking structure of loan data
str(loanData)


# -------------------------------------------------------------------------------------------------------------
#           Data cleaning
#--------------------------------------------------------------------------------------------------------------

#removing customer behaviour variables
behaviorVariable <- c("delinq_2yrs",
                      "earliest_cr_line",
                      "inq_last_6mths",
                      "open_acc",
                      "pub_rec",
                      "revol_bal",
                      "revol_util",
                      "total_acc",
                      "out_prncp",
                      "out_prncp_inv",
                      "total_pymnt",
                      "total_pymnt_inv",
                      "total_rec_prncp",
                      "total_rec_int",
                      "total_rec_late_fee",
                      "recoveries",
                      "collection_recovery_fee",
                      "last_pymnt_d",
                      "last_pymnt_amnt",
                      "next_pymnt_d",
                      "last_credit_pull_d",
                      "application_type")

loanData <- loanData[,!(colnames(loanData) %in% behaviorVariable)]

# calculating percentage of  NA in loan data
naMatrix <- loanData %>% summarize_all(funs(sum(is.na(.)) / length(.)))
naMatrix <- gather(naMatrix,key='feature',value = 'missing_percentage')

# filter rows that have NA values less than 0.15
filterCol <- filter(naMatrix,missing_percentage < 0.15)

# filter from loan data set according to filterCol
temp <- filterCol[,1]
loanData <- loanData[,(colnames(loanData) %in% temp)]


# removing columns that have 0s 
# and Also, removing meaningless variables such member_id, id, url,desc,emp_title, zip_code,addr_state,title 
red_variables <- c("member_id","id","acc_now_delinq","chargeoff_within_12_mths","pymnt_plan","initial_list_status","delinq_amnt","pub_rec_bankruptcies", "tax_liens","collections_12_mths_ex_med","policy_code",
                   "url","desc","emp_title","zip_code","title")

loanData <- loanData[,!(colnames(loanData) %in% red_variables)]


#verify NA values
loanData %>% summarise_all(funs(sum(is.na(.))))

# subsitute % in interest rate column
loanData$int_rate <- as.numeric(gsub("%", "", loanData$int_rate))

# converting columns - loan amount , funded amount and funded amount inv to numeric
loanData$loan_amnt <- as.numeric(loanData$loan_amnt)
loanData$funded_amnt <- as.numeric(loanData$funded_amnt)
loanData$funded_amnt_inv <- as.numeric(loanData$funded_amnt_inv)

# checking structure of loan data
str(loanData)

# cleaning emp_length by subsituting years or year with blank
loanData$emp_length <- gsub("years","",loanData$emp_length)
loanData$emp_length <- gsub("year","",loanData$emp_length)

# removing NA values from emp_length
loanData <- loanData[-which(loanData$emp_length == "n/a"), ]

# removing loan status "Current" loan from loanData set
loanData <- loanData[-which(loanData$loan_status == "Current"), ]


# creating annual income category as high for income > 60000; medium for 31000 to 60000; and low 1 to 30000
loanData$annual_inc_cat <- ifelse(loanData$annual_inc <= 30000,"Low",ifelse(loanData$annual_inc <= 60000,"Medium","High"))


# filter loan status with charged off 
loanData$loan_status <- tolower(loanData$loan_status)
loanDefault <- filter(loanData,loan_status == "charged off")

# converting loan status to factor
loanData$loan_status <- factor(loanData$loan_status)

# write clean data in output file
write.csv(loanData, file = "cleanLoan.csv")


#--------------------------------------------------------------------------------------------------------------
#     Univariate analysis
#-------------------------------------------------------------------------------------------------------------

# Plot1 --- determine percentage of loans status  as charged off/current/fully paid
#       --- geom_bar() for plotting bar plot with count
#       --- ggtitle() for adding title to graph
#       --- labs() for adding labels to x and y axis
plot1 <- ggplot(loanData, aes ( x = loanData$loan_status, y = ..prop..,group = 1))+ geom_bar(stat = "count")
plot1 <- plot1 + scale_y_continuous(labels = scales::percent_format())
plot1 <- plot1 + ggtitle("Loan status in percentage") + labs(x="Loan status", y="Percentage of loans")
plot1


# Plot2 --- plot for emp_length and count of charged off applicants for loans which shows that emp_length > 10 years are most likely defaulters
#       --- geom_bar() for plotting bar plot with count
#       --- ggtitle() for adding title to graph
#       --- labs() for adding labels to x and y axis
#       --- scale_y_continous for making y axis scale
plot2 <- ggplot(data = loanDefault, aes(emp_length)) + geom_bar(stat="count") 
plot2 <- plot2 + scale_y_continuous(trans='log2') + geom_text(stat='count',aes(label=..count..),vjust=-0.5)
plot2 <- plot2 + labs(x = "Number of Years", y = "Count of charged off Applicants") 
plot2 <- plot2 + ggtitle("No of Years for which the Applicants are employed")
plot2


# Plot3 --- for loan term vs total number of charged off loan applicants, This plot show that 36 months terms are more charged off
#       --- geom_bar() for plotting bar plot with count
#       --- ggtitle() for adding title to graph
#       --- labs() for adding labels to x and y axis
#       --- geom_text() for  showing values of  bar 
plot3 <- ggplot(loanDefault, aes ( x = loanDefault$term))+ geom_bar(stat = "count")
plot3 <- plot3 + ggtitle("Number of charged off loans with their terms")
plot3 <- plot3 + labs(x = "Term(in months)", y = "Number of charged off loans")
plot3 <- plot3 + geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)
plot3


# Plot 4 for comparsion of  home ownership for charged off applicants
#       --- geom_bar() for plotting bar plot with count
#       --- ggtitle() for adding title to graph
#       --- labs() for adding labels to x and y axis
#       --- geom_text() for  showing values of  bar 
# This plot shows that rented and mortage home ownership has most charged off loans
plot4 <- ggplot(loanDefault, aes ( x = loanDefault$home_ownership))+ geom_bar(stat = "count")
plot4 <- plot4 + ggtitle("Number of charged off loans Vs Home ownership of applicant")
plot4 <- plot4 + labs( x = "Home ownership", y = "Number of charged off loans")
plot4 <- plot4 + geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)
plot4


# Plot5 --- for comparision of loan purpose with count of charged off loan
#       --- geom_bar() for plotting bar plot with count
#       --- ggtitle() for adding title to graph
#       --- labs() for adding labels to x and y axis
#       --- geom_text() for  showing values of  bar 
# This plot shows that loans with purpose as "debt consilation" are most defaulter.
plot5 <- ggplot(loanDefault, aes ( x = loanDefault$purpose))+ geom_bar(stat = "count")
plot5 <- plot5 + ggtitle("Number of charged off loans Vs Loan purpose")
plot5 <- plot5 + labs( x = "Purpose", y = "Number of charged off loans")
plot5 <- plot5 + geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)
plot5


# Plot6 --- comparison of verfication status with number of charged off loans
#       --- geom_bar() for plotting bar plot with count
#       --- ggtitle() for adding title to graph
#       --- labs() for adding labels to x and y axis
#       --- geom_text() for  showing values of  bar 
# this plot shows that when income source is not verified or verified then chrged off loan are more
plot6 <- ggplot(loanDefault, aes (  x = loanDefault$verification_status)) + geom_bar(stat = "count")
plot6 <- plot6 + ggtitle("Number of charged off loans Vs income verification status")
plot6 <- plot6 + labs(x = "Verification status", y = "Number of charged off loans")
plot6 <- plot6 + geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)
plot6


# Plot7 --- Number of charged off loans in each state
#       --- geom_bar() for plotting bar plot with count
#       --- ggtitle() for adding title to graph
#       --- labs() for adding labels to x and y axis
#       --- geom_text() for  showing values of  bar 
# This plot shows that CA and NY state have more charged off loans
plot7 <- ggplot(loanDefault, aes (  x = loanDefault$addr_state)) + geom_bar(stat = "count")
plot7 <- plot7 + ggtitle("Number of charged off loans in each state")
plot7 <- plot7 + labs( x = "State", y = "Number of charged off loans")
plot7 <- plot7 + geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)
plot7

# Plot8 --- Comparison of charged off loans vs grade
#       --- geom_bar() for plotting bar plot with count
#       --- ggtitle() for adding title to graph
#       --- labs() for adding labels to x and y axis
#       --- geom_text() for  showing values of  bar 
# This plot shows that B, C , D grade have more charged off loans
plot8 <- ggplot(loanDefault, aes ( x = loanDefault$grade))+ geom_bar(stat = "count")
plot8 <- plot8 + ggtitle("Number of charged off loans in each grade")
plot8 <- plot8 + labs(x = "Grade", y = "Number of charged off loans")
plot8 <- plot8 + geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)
plot8


# Plot9 --- Comparison of charged off loans vs sub grade
#       --- geom_bar() for plotting bar plot with count
#       --- ggtitle() for adding title to graph
#       --- labs() for adding labels to x and y axis
#       --- geom_text() for  showing values of  bar
# This plot shows that most CHARGED OFF cases are in B3 ~ C3 and also D2~E1
plot9 <- ggplot(loanDefault, aes (  x = loanDefault$sub_grade)) + geom_bar(stat = "count")
plot9 <- plot9 + ggtitle("Number of charged off loans in each grade")
plot9 <- plot9 + labs(x = "Sub Grade", y = "Number of charged off loans")
plot9 <- plot9 + geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)
plot9

# distribution of annual income
summary(loanDefault$annual_inc)

# distribution of dti
boxplot(loanDefault$dti)

# Plot10 --- Analysis of annual income category for cherged of loans
#       --- geom_bar() for plotting bar plot with count
#       --- ggtitle() for adding title to graph
#       --- labs() for adding labels to x and y axis
#       --- geom_text() for  showing values of  bar
# This plot shows that mostly defaulter are high for medium and high annaul income people
plot10 <- ggplot(loanDefault, aes ( x = loanDefault$annual_inc_cat)) + geom_bar(stat = "count")
plot10 <- plot10 + ggtitle("Number of charged off loans in annual income category")
plot10 <- plot10 + labs(x = "Annual Income category", y = "Number of charged off loans")
plot10 <- plot10 + geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)
plot10

#-----------------------------------------------------------------------------------------------------------
#   Bivariate analysis
#-----------------------------------------------------------------------------------------------------------

# Plot11 --- comparison of proportion of charged off loans and fully paid loans in each state
#        --- geom_bar() for plotting bar plot with count
#        --- ggtitle() for adding title to graph
#        --- labs() for adding labels to x and y axis
# This plot shows that on comparison of proportion of charged off loans and fully paid loans 
# NE has highly  proportion of charged off loans compared to all other states
plot11 <- ggplot(loanData, aes (  x = loanData$addr_state, fill = factor(loanData$loan_status))) + geom_bar(stat = "count", position = "fill")
plot11 <- plot11 + ggtitle("Proportion of charged off/fully paid loans in each State")
plot11 <- plot11 + labs(x = "State", y = "Proportion of charged off/fully paid loans", fill = "Loan Status")
plot11


# Plot12 --- comparison of proportion of charged off loans and fully paid loans vs purpose
#        --- geom_bar() for plotting bar plot with count
#        --- ggtitle() for adding title to graph
#        --- labs() for adding labels to x and y axis
# this plot shows that small business has most charged off loans
plot12 <- ggplot(loanData, aes ( x = loanData$purpose, fill = factor(loanData$loan_status)))+ geom_bar(stat = "count",position = "fill")
plot12 <- plot12 + ggtitle("Proportion of charged off/fully paid loans Vs Purpose")
plot12 <- plot12 + labs(x = "Purpose", y = "Proportion of charged off/fully paid loans", fill = "Loan Status")
plot12

# Plot13 --- comparison of proportion of charged off loans and fully paid loans vs home ownership
#        --- geom_bar() for plotting bar plot with count
#        --- ggtitle() for adding title to graph
#        --- labs() for adding labels to x and y axis
# This plot shows that when ownership status is "others" then charged off laons are most
plot13 <- ggplot(loanData, aes ( x = loanData$home_ownership, fill = factor(loanData$loan_status)))+ geom_bar(stat = "count",position = "fill")
plot13 <- plot13 + ggtitle("Proportion of charged off/fully paid loans Vs Home ownership")
plot13 <- plot13 + labs(x = "Home ownership", y = "Proportion of charged off/fully paid loans", fill = "Loan Status")
plot13

# Plot14 --- comparison of proportion of charged off loans and fully paid loans vs Verification Status
#        --- geom_bar() for plotting bar plot with count
#        --- ggtitle() for adding title to graph
#        --- labs() for adding labels to x and y axis
# This plot shows that Even though, LC has verified borrowers and borrowers sources, the percentage of defaulters is higher than "Not Verified" cases.
plot14 <- ggplot(loanData, aes (  x = loanData$verification_status, fill = factor(loanData$loan_status))) + geom_bar(stat = "count" ,position = "fill") + scale_y_continuous(labels = scales::percent_format())
plot14 <- plot14 + ggtitle("Proportion of charged off/fully paid loans Vs Verification Status")
plot14 <- plot14 + labs(x = "Verification Status", y = "Proportion of charged off/fully paid loans", fill = "Loan Status")
plot14


# Plot15 --- comparison of Proportion of charged off/fully paid loans Vs Grade
#        --- geom_bar() for plotting bar plot with count
#        --- ggtitle() for adding title to graph
#        --- labs() for adding labels to x and y axis
# This plot shows that G and F grade are more charged off loans
plot15 <- ggplot(loanData, aes ( x = loanData$grade, fill = factor(loanData$loan_status)))+ geom_bar(stat = "count",position = "fill")
plot15 <- plot15 + ggtitle("Proportion of charged off/fully paid loans Vs Grade")
plot15 <- plot15 + labs(x = "Grade", y = "Proportion of charged off/fully paid loans", fill = "Loan Status")
plot15

# plot16 --- Proportion of charged off/fully paid loans Vs employement length
#        --- geom_bar() for plotting bar plot with count
#        --- ggtitle() for adding title to graph
#        --- labs() for adding labels to x and y axis
# This plot shows that Borrowers employed for 10+ years are most likely to default the loan compared to other durations
plot16 <- ggplot(loanData, aes ( x = loanData$emp_length, fill = factor(loanData$loan_status)))+ geom_bar(stat = "count", position = "fill")
plot16 <- plot16 + ggtitle("Proportion of charged off/fully paid loans Vs employement length")
plot16 <- plot16 + labs(x = "Employed length", y = "Proportion of charged off/fully paid loans", fill = "Loan Status")
plot16

# Plot17 --- Proportion of charged off/fully paid loans Vs Annual Income category
#        --- geom_bar() for plotting bar plot with count
#        --- ggtitle() for adding title to graph
#        --- labs() for adding labels to x and y axis
# This plot shows that low annaul income category ( 1- 30000) are more defaulters than others
plot17 <- ggplot(loanData, aes ( x = loanData$annual_inc_cat, fill = factor(loanData$loan_status))) + geom_bar(stat = "count",position = "fill")
plot17 <- plot17 + ggtitle("Proportion of charged off/fully paid loans Vs Annual Income category")
plot17 <- plot17 + labs(x = "Annual Income category", y = "Proportion of charged off/fully paid loans", fill = "Loan Status")
plot17

# Plot18 --- Proportion of charged off/fully paid loans Vs Loan Term
#        --- geom_bar() for plotting bar plot with count
#        --- ggtitle() for adding title to graph
#        --- labs() for adding labels to x and y axis
# This plot shows that 60 months term loans are more defaulters
plot18 <- ggplot(loanData, aes ( x = loanData$term, fill = factor(loanData$loan_status)))+ geom_bar(stat = "count", position = "fill")
plot18 <- plot18 + ggtitle("Proportion of charged off/fully paid loans Vs Loan Term")
plot18 <- plot18 + labs(x = "Term", y = "Proportion of charged off/fully paid loans", fill = "Loan Status")
plot18



