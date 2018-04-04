#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("stringr")
#install.packages("sqldf")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(sqldf)

##Loading data
loan = read.csv("loan.csv", na.strings = c("", " ", NA), stringsAsFactors = F)

##Checking for duplicates
sum(duplicated(loan$id))
sum(duplicated(loan$member_id) == TRUE)

#Data cleaning/manipulating
loan$zip_code = NULL
loan$mths_since_last_major_derog = NULL
loan$policy_code = NULL
loan$pymnt_plan = NULL
loan = subset(loan, select = -c(53:105, 107:111))

loan$emp_length[which(loan$emp_length == 'n/a')] = "0 years" #emp-Length: Need to make NAs as 0
loan$total_rec_late_fee = round(loan$total_rec_late_fee, 2) # total_rec_late_fee ( Needs to format the decimal) Need to round to 2 decimals

loan$earliest_cr_line = paste("01-", loan$earliest_cr_line) ## Assuming the first day of month
loan$last_pymnt_d = paste(loan$last_pymnt_d,"2018") ##Filling year as 2018 as per date observed in excel
loan$last_credit_pull_d = paste(loan$last_credit_pull_d,"2018") ##Filling year as 2018 as per date observed in excel

loan$termInYrs = as.integer(str_sub(loan$term, 2, 3)) ## Removing extra strings in term fields

##Removing '%' symbol from rate of interest to calculate outliers in rate of interest
loan$int_rate = as.double(str_remove_all(loan$int_rate, "%"))

## total_amount_due = installment * tenure
loan$total_amount_due = loan$installment * loan$termInYrs
loan$gain = loan$total_amount_due - (loan$total_rec_prncp + loan$total_rec_int)

########## Univariate Analysis ######################
#Creating bins based on employee length
for(i in 1:nrow(loan)){
  if(loan$emp_length[i] == 'n/a'){
    loan$empl_exp[i] <- "0 years"
  }
  else if(loan$emp_length[i]=="< 1 year"){
    loan$empl_exp[i] <- "Less than 1 year"
  }
  else if(loan$emp_length[i]=="1 year" || loan$emp_length[i]=="2 years" ||
          loan$emp_length[i]=="3 years" || loan$emp_length[i]=="4 years" ||
          loan$emp_length[i]=="5 years"){
    loan$empl_exp[i] <- "Between 1 & 5 years"
  }
  else if(loan$emp_length[i]=="6 years" || loan$emp_length[i]=="7 years" ||
          loan$emp_length[i]=="8 years" || loan$emp_length[i]=="9 years"){
    loan$empl_exp[i] <- "Between 6 & 9 years"
  }
  else if(loan$emp_length[i]=="10+ years"){
    loan$empl_exp[i] <- "10+ years"
  }
}    

n_rec <- nrow(loan)

#Taking summary based on different fields.

grades_summ <- sqldf("select grade, loan_status, count(*) as count from loan group by grade, loan_status")
grades_summ$perc <- paste((round(grades_summ$count/n_rec, 4)*100),"%",sep = "")

term_summ <- sqldf("select term, loan_status, count(*) as count from loan group by term, loan_status")
term_summ$perc <- paste((round(term_summ$count/n_rec, 4)*100),"%",sep = "")

loan_summ <- sqldf("select loan_status, count(*) as count from loan group by loan_status")
loan_summ$perc <- paste((round(loan_summ$count/n_rec, 4)*100),"%",sep = "")

home_ownr_summ <- sqldf("select home_ownership, loan_status, count(*) as count from loan group by home_ownership, loan_status")
home_ownr_summ$perc <- paste((round(home_ownr_summ$count/n_rec, 4)*100),"%",sep = "")

verification_summ <- sqldf("select verification_status, loan_status, count(*) as count from loan group by verification_status, loan_status")
verification_summ$perc <- paste((round(verification_summ$count/n_rec, 4)*100),"%",sep = "")

emp_exp_summ <- sqldf("select empl_exp, loan_status, count(*) as count from loan group by empl_exp, loan_status")
emp_exp_summ$perc <- paste((round(emp_exp_summ$count/n_rec, 4)*100),"%",sep = "")

loan_purpose_summ <- sqldf("select purpose, loan_status, count(*) as count from loan group by purpose, loan_status")
loan_purpose_summ$perc <- paste((round(loan_purpose_summ$count/n_rec, 4)*100),"%",sep = "")

## Impact of total payment on fund amount considering loan purpose and loan status
ggplot(data = loan, aes(x=loan_amnt, y=total_pymnt)) + geom_point() + facet_wrap(~loan$purpose)
ggplot(data = loan, aes(x=loan_amnt, y=total_pymnt)) + geom_point() + facet_wrap(~loan$loan_status)

########## Segmentated Analysis ######################
loan_debit_consol_sub = subset(loan, purpose == "debt_consolidation")
ggplot(data = loan_debit_consol_sub, aes(x=loan_amnt)) + geom_histogram(binwidth = 500) + ggtitle("Univariate analysis on loan amout")
ggplot(data = loan_debit_consol_sub, aes(x= annual_inc)) + geom_histogram(binwidth = 500) + ggtitle("Univariate analysis on loan amout")
ggplot(data = loan_debit_consol_sub, aes(x=loan_amnt, y=total_rec_prncp)) + geom_smooth() + ggtitle("bi-variate - loan amount vs total principle recovered")
ggplot(data = loan_debit_consol_sub, aes(x=term)) + stat_count() + ggtitle("snapshot of loan amount tenure")
ggplot(data = loan_debit_consol_sub, aes(x=home_ownership)) + stat_count() + ggtitle("snapshot of house ownership")
ggplot(data = loan_debit_consol_sub, aes(x=emp_length)) + stat_count() + ggtitle("employee experience snapshot")

loan_chargedoff_consol_sub = subset(loan, loan_status == "Charged Off")
ggplot(data = loan_chargedoff_consol_sub, aes(x=emp_length)) + stat_count() + ggtitle("employee experience snapshot")
ggplot(data = loan_chargedoff_consol_sub, aes(x=purpose), col=purpose) + stat_count() + ggtitle("purpose snapshot")
ggplot(data = loan_chargedoff_consol_sub, aes(x=loan_amnt, y=total_rec_prncp)) + geom_smooth() + ggtitle("bi-variate - loan amount vs total principle recovered")
ggplot(data = loan_chargedoff_consol_sub, aes(x=home_ownership)) + stat_count() + ggtitle("snapshot of house ownership")

loan_current_consol_sub = subset(loan, loan_status == "Current")
ggplot(data = loan_current_consol_sub, aes(x=emp_length)) + stat_count() + ggtitle("employee experience snapshot")
ggplot(data = loan_current_consol_sub, aes(x=purpose), col=purpose) + stat_count() + ggtitle("purpose snapshot")
ggplot(data = loan_current_consol_sub, aes(x=loan_amnt, y=total_rec_prncp)) + geom_smooth() + ggtitle("bi-variate - loan amount vs total principle recovered")
ggplot(data = loan_current_consol_sub, aes(x=home_ownership)) + stat_count() + ggtitle("snapshot of house ownership")

ggplot(data = loan_chargedoff_consol_sub, aes(x=factor(purpose), y=loan_amnt)) + geom_bar(stat = "identity")
ggplot(data = loan_current_consol_sub, aes(x=factor(purpose), y=loan_amnt)) + geom_bar(stat = "identity")

## Subsetting data based on charged-off and current also on debit consolidation
loan_seg <- sqldf("select * from loan where purpose=='debt_consolidation' and loan_status in ('Charged Off', 'Current')")

## showcasing loan amount data as histogram after segmentation
ggplot(data = loan_seg, aes(x=loan_amnt)) + geom_histogram(binwidth = 1000, colour = 'blue')

##Plotting box-plot for interest rates to identify the outliers
ggplot(data = loan, aes(x=id, y=int_rate)) + geom_boxplot(alpha=0.2, outlier.color='red') + ggtitle("rate of interest box plot")

## Subsetting data based on rate of interest outliers
##subsetting loan data based on outliers and the segment having highest number of customers
loan_seg$perc_int <- as.double(str_extract_all(loan_seg$int_rate, "\\d+(\\.\\d+){0,1}"))
high_RateOfInterest_seg <- sqldf("select * from loan_seg where perc_int>=20") ## Interest rates falling under high risk
low_RateOfInterest_seg <- sqldf("select * from loan_seg where perc_int>=12 and perc_int<=17") ## Interest rates falling under low risk with high count


#Dataframes for the segmented analysis
loan_amt_seg <- sqldf("select * from loan_seg where loan_amnt>20000 and loan_amnt<36000")

## Identifying the number of customers falling under segments of interest
loan_amt_seg$pending_loan_amt = loan_amt_seg$loan_amnt - loan_amt_seg$total_rec_prncp
sqldf("select count(member_id) from loan_amt_seg where pending_loan_amt > 0 and pending_loan_amt<10000")
sqldf("select count(member_id) from loan_amt_seg where pending_loan_amt >10000 and pending_loan_amt < 20000")
sqldf("select count(member_id) from loan_amt_seg where pending_loan_amt >20000 and pending_loan_amt < 30000")
sqldf("select count(member_id) from loan_amt_seg where pending_loan_amt>30000")
ggplot(data = loan_amt_seg, aes(x=pending_loan_amt)) + geom_histogram(binwidth = 10000, color='blue') + ggtitle("count of customers with pending due based on loan amount outliers")

high_RateOfInterest_seg$pending_loan_amt = high_RateOfInterest_seg$loan_amnt - high_RateOfInterest_seg$total_rec_prncp
sqldf("select count(member_id) from high_RateOfInterest_seg where pending_loan_amt > 0 and pending_loan_amt<10000")
sqldf("select count(member_id) from high_RateOfInterest_seg where pending_loan_amt >10000 and pending_loan_amt < 20000")
sqldf("select count(member_id) from high_RateOfInterest_seg where pending_loan_amt >20000 and pending_loan_amt < 30000")
sqldf("select count(member_id) from high_RateOfInterest_seg where pending_loan_amt>30000")
ggplot(data = high_RateOfInterest_seg, aes(x=pending_loan_amt)) + geom_histogram(binwidth = 10000, color='blue') + ggtitle("count of customers with pending due based on high interest rates")

low_RateOfInterest_seg$pending_loan_amt = low_RateOfInterest_seg$loan_amnt - low_RateOfInterest_seg$total_rec_prncp
sqldf("select count(member_id) from low_RateOfInterest_seg where pending_loan_amt > 0 and pending_loan_amt<10000")
sqldf("select count(member_id) from low_RateOfInterest_seg where pending_loan_amt >10000 and pending_loan_amt < 20000")
sqldf("select count(member_id) from low_RateOfInterest_seg where pending_loan_amt >20000 and pending_loan_amt < 30000")
sqldf("select count(member_id) from low_RateOfInterest_seg where pending_loan_amt>30000")
ggplot(data = low_RateOfInterest_seg, aes(x=pending_loan_amt)) + geom_histogram(binwidth = 10000, color='blue') + ggtitle("count of customers with pending due based on low interest rates")



