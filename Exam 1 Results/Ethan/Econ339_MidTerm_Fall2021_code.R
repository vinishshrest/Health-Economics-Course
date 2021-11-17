data <- read.csv("C:/Users/eberr/OneDrive/Desktop/NCHS_birthweight2000_sample.csv")
data
head(data)

datainc <- read.csv("C:/Users/eberr/OneDrive/Desktop/NCHS_income.csv")
datainc
head(datainc)

#1
hist(datainc$annualincome)

#2
datanew <- merge(data, datainc, by = "person_id", all.x = T)
datanew
head(datanew)

#3
datanew <- subset(datanew, dmeduc != 99)
datanew
reg1 <- lm(dbirwt ~ dmeduc, data = datanew)
reg1
summary(reg1)
#This shows a small correlation between the mother's education and infant birth weight. Thus, the higher the mother's education we should expect to see an increase in infant birth weight.

#4
datanew$low_bwgt <- 0
datanew$low_bwgt[datanew$dbirwt < 2500] <- 1

#5
mean(datanew$low_bwgt[datanew$dmeduc == 12])

################################################
################################################
# 
# VS comment: youd want to do this for each and every value of mother's education
# Then plot the relationship between mother's education and proportion of low birthweight
###############################################
###############################################

#6
plot(datanew$dmeduc, datanew$low_bwgt) 
plot

#7
datanew$aboveHS <- 0
datanew$aboveHS[datanew$dmeduc > 12] <- 1

#8
reg2 <- lm(low_bwgt ~ aboveHS, data = datanew)
reg2
summary(reg2)
#These two variables are negatively correlated leading to the assumption that as mothers achieve an education of high school or higher the chances of having a low birth weight child decreases by 2.8 percentage points.
## VS comments: note that this is an association, there is nothing to assume here -- 
## just say "mothers with high school or higher have a reduced probability of low birth weight infant by 2.8 percentage points compared to 
## moms without high school

#9
datanew$race <- 0
datanew$race[datanew$race_white == 0] <- 1 #set race so that it represents non-white individuals
reg3 <- lm(race ~ aboveHS, data = datanew)
reg3
summary(reg3)
#This regression shows a negative correlation between non-white individuals and achieving a high school education. This suggests that non-white individuals are 3.3 percentage points less likely to have a high school education or higher.

#10
#The evidence of race playing a factor in the probability of a high school education means that it is affecting the probability of low birth weight and so it is impossible to know if high school education is truly affecting the probability of a low birth weight child.
## VS comments: Good job 

#11
reg4 <- lm(low_bwgt ~ aboveHS + annualincome, data = datanew)
reg4
summary(reg4)
#Adding annual income to the regression accounts for one of the many variables that may be affecting the probability of low birth weight. I prefer this because it more accurately shows the impact of each factor on the infant birth weights and helps prevent omitted variable bias.