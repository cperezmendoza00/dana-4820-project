
#rm(list=ls()); #Cleaning variables
#detach(); #Detaching datasets
#getwd() # Getting current directory
#setwd("/yourfolder") # Setting work directory (it should be git folder)

file = paste(getwd(),"/data/Loan-Approval-Prediction.csv", sep = "")
DS = read.csv(file, sep = ",") 
names(DS)
#head(DS)
unique(DS$Loan_Amount_Term)

DS$LStatus=ifelse(DS$Loan_Status=="Y", 1, 0)
DS$Loan_Amount_Term2=ifelse(DS$Loan_Amount_Term=="360", 1, 0)



model = glm(
  LStatus ~
    + Married
    + factor(Credit_History)
    + Property_Area
    + ApplicantIncome
    + Loan_Amount_Term2
    + Married*Loan_Amount_Term2
    + ApplicantIncome*Loan_Amount_Term2
  ,data=DS)

summary(model)



#-------------------------------------------------------------------------------
# INTERACTION GRAPH


seq_value = DS$yearsexperience
sequence = seq(min(seq_value),max(seq_value),by=((max(seq_value) - min(seq_value)) / 99))

plot(1, type = "n", #n means does not produce any points
     xlab = "Years of experience", ylab = "Callback (log of odds)",
     xlim = c(min(DS$yearsexperience), max(DS$yearsexperience)), 
     ylim = c(0, 0.3) #probabilities
)


legend("topleft", legend = c("Skills 1","Skills 0"), col = c("blue", "red"), lty=1:1, cex = 0.6)

#MODEL WITH INTERACTION

#ESPECIAL SKILLS
list_of_log_of_odds = predict(model3,
                              newdata=data.frame(yearsexperience=sequence,specialskills=0,cityjob="Boston",race="black",employmentholes=0),
                              type="response"
)
#list_of_probabilities = (exp(list_of_log_of_odds)) / (1 + (exp(list_of_log_of_odds)))
lines(sequence,list_of_log_of_odds,col="red")

#ESPECIAL SKILLS
list_of_log_of_odds = predict(model3,
                              newdata=data.frame(yearsexperience=sequence,specialskills=1,cityjob="Boston",race="black",employmentholes=0),
                              type="response"
)
#list_of_probabilities = (exp(list_of_log_of_odds)) / (1 + (exp(list_of_log_of_odds)))
lines(sequence,list_of_log_of_odds,col="blue")





#-------------------------------------------------------------------------------

