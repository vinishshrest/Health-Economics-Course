data <- read.csv("C:/Users/sjame/OneDrive/Desktop/NCHS_birthweight2000_sample.csv")
datainc <- read.csv("C:/Users/sjame/OneDrive/Desktop/NCHS_income.csv")
head(data)
head(datainc)

##Question 1
hist(datainc$annualincome)

##Question 2
datanew <- merge(data, datainc, by.x="person_id", by.y="person_id", all.x =TRUE)
head(datanew)

##Question 3
datanew2 <- subset(datanew, dmeduc < 99)
lm(dbirwt~dmeduc,datanew2)
## Education gradient is positively related to the birth weight of offspring. Beta coefficient = 19.22, intercept = 3083.29.

##Question 4
datanew2$low_bwgt <- 0
datanew2$low_bwgt[datanew2$dbirwt < 2500] <- 1

##Question 5
mean(datanew$low_bwgt[datanew$dmeduc == 17]) # 6.69%
mean(datanew$low_bwgt[datanew$dmeduc == 16]) # 5.42%
mean(datanew$low_bwgt[datanew$dmeduc == 15]) # 6.72%
mean(datanew$low_bwgt[datanew$dmeduc == 14]) # 5.91%
mean(datanew$low_bwgt[datanew$dmeduc == 13]) # 6.57%
mean(datanew$low_bwgt[datanew$dmeduc == 12]) # 7.9%
mean(datanew$low_bwgt[datanew$dmeduc == 11]) # 9.3%
mean(datanew$low_bwgt[datanew$dmeduc == 10]) # 13.7%
mean(datanew$low_bwgt[datanew$dmeduc == 9]) # 11.6%
mean(datanew$low_bwgt[datanew$dmeduc == 8]) # 6.82%
mean(datanew$low_bwgt[datanew$dmeduc == 7]) # 13.04%
mean(datanew$low_bwgt[datanew$dmeduc == 6]) # 8.54%
mean(datanew$low_bwgt[datanew$dmeduc == 5]) # 6.66%


##Question 6
plot(datanew2$dbirwt, datanew2$dmeduc)

##########################################
##########################################
#
# Sam, you are supposed to plot the relationship between mother's education and 
# proportion of low birthweight, not birthweight.
##########################################
##########################################

##Question 7
datanew2$lesshigh_scl<- 0
datanew2$lesshigh_scl[datanew2$dmeduc>=12] <- 1
##Question 8
datanew2$abovehigh_scl<- 0
datanew2$abovehigh_scl[datanew2$dmeduc>=12] <- 1

lm(low_bwgt~abovehigh_scl,datanew2)
##Question 9
lm(race_white~abovehigh_scl,datanew2)
##Beta Coefficient - .0121, intercept .7925 Comment - white race is positively correlated to those that are educated past the high school level.
## Race is not determined by if they have completed high school. however, high school is likely related to race.

#######################################
#######################################
#
# VS comments: use reg <- lm(race_white~abovehigh_scl,datanew2) (stores your coefficients)
#   sumnmary(reg)
#######################################
#######################################

lm(low_bwgt~abovehigh_scl, datanew2)
##If they are above high school education, they are less likely to have a low birth weight baby.

####################################################
####################################################
# VS comments: Think of omitted variable bias -- education is related to race but race is missing from the specification.
####################################################
####################################################
     