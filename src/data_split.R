#Packages
install.packages('car')
library(car)



#Put the data set on the variable "data".
data = Loan.Approval_clean

#Split 70% to train and 30% to test.
dt = sort(sample(nrow(data), nrow(data)*.7))
train<-data[dt,]
test<-data[-dt,]


#Model
model <- lm(data$LoanAmount ~ data$ApplicantIncome + data$CoapplicantIncome, data=data)
vif(model)


