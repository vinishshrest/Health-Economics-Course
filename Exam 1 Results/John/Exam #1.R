data <- read.csv("/Users/johnnykoco/Downloads/NCHS_birthweight2000_sample.csv")
datainc <-read.csv("/Users/johnnykoco/Downloads/NCHS_income.csv")

#Section 1 Q1
head(datainc)
hist(datainc$annualincome)
head(data)

#Q2
datanew <-merge(data,datainc, by.x=c("X"),by.y=c("person_id"))
head(datanew)

#Q3
datanew <- subset(datanew, dmeduc < 99)
reg <-lm(dbirwt ~ dmeduc, data = data)
summary(reg)
#For every point the mother's education increases, baby birthweight increases
#by 1.72 pounds


#Q4
datanew$low_bwgt <- 0
datanew$low_bwgt[datanew$dbirwt < 2500] <- 1

#Q5
mean(datanew$low_bwgt[datanew$dmeduc == 12])
mean(datanew$low_bwgt[datanew$dmeduc == 11])
mean(datanew$low_bwgt[datanew$dmeduc == 10])
mean(datanew$low_bwgt[datanew$dmeduc == 09])
mean(datanew$low_bwgt[datanew$dmeduc == 08])
mean(datanew$low_bwgt[datanew$dmeduc == 07])
mean(datanew$low_bwgt[datanew$dmeduc == 06])
mean(datanew$low_bwgt[datanew$dmeduc == 05])
mean(datanew$low_bwgt[datanew$dmeduc == 04])
mean(datanew$low_bwgt[datanew$dmeduc == 03])
mean(datanew$low_bwgt[datanew$dmeduc == 02])
mean(datanew$low_bwgt[datanew$dmeduc == 01])
mean(datanew$low_bwgt[datanew$dmeduc == 0])

#Q6
datanew <- subset(datanew, dmeduc < 99)
plot(data$dmeduc,data$low_bwgt) 
#######################################
#######################################
# VS comment: you are supposed to plot 
# the proportion of low birth weight. This would be the 
# mean that you calculated in Q5
#######################################
#######################################

#Q7
datanew$aboveHS[datanew$dmeduc>12] <- 1.
datanew$aboveHS[datanew$dmeduc<12] <- 0.
head(datanew)

#Q8
reg2 <-lm(low_bwgt ~ aboveHS, data = data)
summary (reg2)  ##VS comment space is included delete space
#I wasnt able to get this regression to work, but based off of the previous data
#I would assume that if the mothers education is above high school education
#then the birthweight of the child has a lower chance of being low

#Q9
data$race_white <-ifelse(data$race_white='1', 1,0)

#Q10


#Q11
reg3 <- lm(low_bwgt, above_HS, annualincome)
summary(reg3)

################
################
#VS comment:  lm(low_bwgt ~ above_HS + annualincome, data = data) ## add variables in regression using +