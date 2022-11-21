
#rm(list=ls()); #Cleaning variables
#detach(); #Detaching datasets
#getwd() # Getting current directory
#setwd("/yourfolder") # Setting work directory (it should be git folder)




LOAN <- read.csv('data/Loan-Approval-Prediction.csv', na.strings = c('',' ','NA'))
attach(LOAN)

table(Gender)
#    Female   Male 
#13    112    489 
table(Married)
#  No Yes 
#3 213 398 
table(Dependents)
#       0   1   2  3+ 
#  15 345 102 101  51 
table(Education)
#Graduate    Not Graduate 
#480          134
table(Self_Employed)
#    No Yes
#32 500  82 
table(ApplicantIncome)
table(CoapplicantIncome)
table(LoanAmount)
table(Loan_Amount_Term)
#12  36  60  84 120 180 240 300 360 480 
#1   2   2   4   3  44   4  13 512  15 
table(Credit_History)
#  0   1 
#89 475 
table(Property_Area)
# Rural Semiurban     Urban 
# 179       233       202
table(Loan_Status)
#N   Y 
#192 422 


  
#Removing the NaNs
LOAN_data = na.omit(LOAN)
detach(LOAN)
attach(LOAN_data)

#-------------------------------------------------------------------------------
# Chi-squared #####
table(Gender, Loan_Status)
chisq.test(LOAN_data$Gender, LOAN_data$Loan_Status, correct=FALSE)
#X-squared = 1.9972, df = 1, p-value = 0.1576
#Reject the null hypothesis. Gender and Loan Status are independent.

table(Married, Loan_Status)
chisq.test(LOAN_data$Married, LOAN_data$Loan_Status, correct=FALSE)
#X-squared = 6.0557, df = 1, p-value = 0.01386
#Fail to reject the null hypothesis. Married and Loan Status are not independent.

table(Dependents, Loan_Status)
chisq.test(LOAN_data$Dependents, LOAN_data$Loan_Status)
#X-squared = 2.9006, df = 3, p-value = 0.4072
#Reject the null hypothesis. Dependents and Loan Status are independent.

table(Education, Loan_Status)
chisq.test(LOAN_data$Education, LOAN_data$Loan_Status, correct=FALSE)
#X-squared = 2.2482, df = 1, p-value = 0.1338
#Reject the null hypothesis. Education and Loan Status are independent.

table(Self_Employed, Loan_Status)
chisq.test(LOAN_data$Self_Employed, LOAN_data$Loan_Status, correct=FALSE)
#X-squared = 0.57846, df = 1, p-value = 0.4469
#Reject the null hypothesis. Self_Employed and Loan Status are independent.

table(Property_Area, Loan_Status)
chisq.test(LOAN_data$Property_Area, LOAN_data$Loan_Status)
#X-squared = 12.226, df = 2, p-value = 0.002214
#Fail to reject the null hypothesis. Married and Loan Status are not independent.

#-------------------------------------------------------------------------------
  
#It returns a warning
#chisq.test(LOAN_data$Loan_Amount_Term, LOAN_data$Loan_Status)

table(Loan_Amount_Term, Loan_Status)
#Loan_Amount_Term2=ifelse(Loan_Amount_Term=='240','240-300',
#                         ifelse(Loan_Amount_Term=='300','240-300',
#                         ifelse(Loan_Amount_Term=='360','>=360',
#                         ifelse(Loan_Amount_Term=='480','>=360','<240'
#                                ))))
Loan_Amount_Term2=ifelse(Loan_Amount_Term=='360','360','≠360')

table(Loan_Amount_Term2, Loan_Status)
chisq.test(Loan_Amount_Term2, LOAN_data$Loan_Status, correct=FALSE)

#-------------------------------------------------------------------------------
  
#Scatterplot matrix
pairs(~ApplicantIncome+CoapplicantIncome+LoanAmount,
      data = LOAN_data,
      main="Scatterplot matrix")

#Add column
LStatus=ifelse(Loan_Status=='Y',1,0)

#Remove column
#Loan_ID

# Plots ######

# Simple Bar Plot
counts <- table(LOAN_data$Loan_Status)
barplot(counts, main="Loan Distribution",
        xlab="Approved or not")

counts <- table(Loan_Amount_Term2)
barplot(counts, main="Loan Term Distribution",
        xlab="Term in months")

counts <- table(LOAN_data$Education)
barplot(counts, main="Education Distribution")

counts <- table(LOAN_data$Dependents)
barplot(counts, main="Dependents Distribution")

counts <- table(LOAN_data$Self_Employed)
barplot(counts, main="Self Employment Distribution")

#-------------------------------------------------------------------------------
  
#Complete model
model_z <- glm (LStatus~Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+
                  CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area, 
                family = binomial, data=LOAN_data)
summary (model_z)
coef(model_z)

#First model
model_1 <- glm (LStatus~Married+ApplicantIncome+CoapplicantIncome+LoanAmount+
                  Loan_Amount_Term2+factor(Credit_History)+Property_Area, 
                family = binomial, data=LOAN_data)
summary (model_1)
coef(model_1)

paste(coef(model_1), names(coef(model_1)), sep = '*', collapse = ' + ')


#Stepwise
step(model_1, direction="both", trace = FALSE)

#Stepwise with interactions
step(model_1, scope = . ~ .^2, direction = 'both', trace = FALSE)

library(MASS)
stepAIC(model_1, direction = "both", trace = FALSE)

model3 <- glm (LStatus~Married+ApplicantIncome+LoanAmount+Loan_Amount_Term2+
                 factor(Credit_History)+Property_Area+
                 ApplicantIncome*Loan_Amount_Term2+ #interaction 1
                 Married*Loan_Amount_Term2, #interaction 2
               family = binomial, data=LOAN_data)
summary (model3)


#-------------------------------------------------------------------------------
# INTERACTION GRAPH 1

seq_value = LOAN_data$ApplicantIncome
sequence = seq(min(seq_value),max(seq_value),by=((max(seq_value) - min(seq_value)) / 99))
plot(1, type = "n", #n means does not produce any points
xlab = "Applicant Income", ylab = "Loan status (log of odds)",
xlim = c(min(seq_value), max(seq_value)), 
ylim = c(0.0, 1) #probabilities
)
legend("topleft", legend = c("Loan Amount term = 360","Loan Amount term ≠ 360"), col = c("blue", "red"), lty=1:1, cex = 0.6)
# = 360
list_of_log_of_odds = predict(model3,
newdata=data.frame(ApplicantIncome=sequence,Loan_Amount_Term2="360",Married="No", Credit_History=0, Property_Area="Rural", LoanAmount=0),
type="response"
)
lines(sequence,list_of_log_of_odds,col="blue")
# ≠ 360
list_of_log_of_odds = predict(model3,
newdata=data.frame(ApplicantIncome=sequence,Loan_Amount_Term2="≠360",Married="No", Credit_History=0, Property_Area="Rural", LoanAmount=0),
type="response"
)
lines(sequence,list_of_log_of_odds,col="red")





# INTERACTION GRAPH 2
plot(1, type = "n", #n means does not produce any points
     xlab = "", ylab = "Loan status (log of odds)",
     xlim = c(-0.5, 1.5), 
     ylim = c(0, 0.2), #probabilities
     xaxt = "n"
)
axis(1, at = c(0, 1),
labels = c("Loan Amount term = 360", "Loan Amount term ≠ 360"))

legend("topleft", legend = c("Married","Not married"), col = c("blue", "red"), lty=1:1, cex = 0.6)
# = MARRIED
list_of_log_of_odds = predict(model3,
  newdata=data.frame(ApplicantIncome=0,Loan_Amount_Term2=c("360","≠360"),Married="Yes", Credit_History=0, Property_Area="Rural", LoanAmount=0),
  type="response"
)
lines(c(0,1),list_of_log_of_odds,col="blue")


# = NOT MARRIED
list_of_log_of_odds = predict(model3,
  newdata=data.frame(ApplicantIncome=0,Loan_Amount_Term2=c("360","≠360"),Married="No", Credit_History=0, Property_Area="Rural", LoanAmount=0),
  type="response"
)
lines(c(0,1),list_of_log_of_odds,col="red")

#-------------------------------------------------------------------------------
# Compare model 3 and model 4

#Modelo without the interaction Married+LoanTerm
model4 <-  glm (LStatus~Married+ApplicantIncome+LoanAmount+Loan_Amount_Term2+
      factor(Credit_History)+Property_Area+
      ApplicantIncome*Loan_Amount_Term2+ #interaction 1
    family = binomial, data=LOAN_data)
summary (model4)



significance = 0.05

#Compare models using pvalue and chisquare

full_model_deviance = deviance(model3)
full_df = df.residual(model3)

reduced_model_deviance = deviance(model4)
reduced_df = df.residual(model4)

deviance_difference = reduced_model_deviance - full_model_deviance
p_value = pchisq(
  deviance_difference,
  (reduced_df-full_df),
  lower.tail = FALSE
)

print(p_value)


#-------------------------------------------------------------------------------

