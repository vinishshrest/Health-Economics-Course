data = read.csv("/Users/amanee/Documents/ECON 339/xid-64283921_2.csv")
head(data)

dataIncome = read.csv("/Users/amanee/Documents/ECON 339/xid-65947720_2.csv")
head(data)

#Question 1
hist(annualincome)

#Question 2
names(data)[1] <- 'person_id'
new_data <- merge(data,dataIncome,by='person_id',all.x=T)
head(new_data)

#Question 3
new_data<-subset(new_data,dmeduc<99)
q3 <- lm(dbirwt~dmeduc)
summary(q3)

#Question 4
new_data$low_bwgt <- 0
new_data$low_bwgt[new_data$dbirwt < 2500] <- 1

#Question 5
proportions_low_bwgt <-rep(0,18)
for(i in 1:18){
  proportions_low_bwgt[i] <- mean(new_data$low_bwgt[new_data$dmeduc==(i-1)])
}
proportions_low_bwgt

#Question 6
plot(proportions_low_bwgt)

## VS. create another vector of education: educ <- seq(from = 1, to = 17, by = 1)
## VS. comments plot(educ, proportions_low_bwgt)

#Question 7
new_data$aboveHS[new_data$dmeduc>12] <- 1.
new_data$aboveHS[new_data$dmeduc<=12] <- 0.

#Question 8
q8 <- lm(new_data$low_bwgt~new_data$aboveHS)
summary(q8)

#Question 9
##White is represented by 0 and Black is represented by 1
new_data$race[new_data$race_white>=1] <- 0.
new_data$race[new_data$race_white<1] <- 1.
head(new_data)

q9 <- lm(new_data$race_white~new_data$aboveHS)
summary(q9)

#Question 10
lm(new_data$low_bwgt~new_data$dmeduc)
lm(new_data$low_bwgt~new_data$aboveHS)

#Question 11
q11 <- lm(new_data$low_bwgt~new_data$aboveHS+new_data$annualincome)
summary(q11)





