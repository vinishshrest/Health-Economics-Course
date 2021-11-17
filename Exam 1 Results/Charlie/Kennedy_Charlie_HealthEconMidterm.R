##### Section 1 #####
data <- read.csv("C:/Users/Charl/Dropbox/My PC (CharliesLaptop)/Downloads/NCHS_birthweight2000_sample.csv")
datainc <- read.csv("C:/Users/Charl/Dropbox/My PC (CharliesLaptop)/Downloads/NCHS_income.csv")

hist(data$annualincome)
head(data)
head(datainc)

datanew <- merge(data, datainc, by = "person_id", all.x= T)
head(datanew)

datanew <- subset(datanew, dmeduc < 99)

datanew$prop_low_bwgt[datanew$dmeduc == 0] <- 0

datanew$low_bwgt <- 0
datanew$low_bwgt[datanew$dbirwt < 2500] = 1

mean(datanew$low_bwgt[datanew$dmeduc == 0])
mean(datanew$low_bwgt[datanew$dmeduc == 1])
mean(datanew$low_bwgt[datanew$dmeduc == 2])
mean(datanew$low_bwgt[datanew$dmeduc == 3])
mean(datanew$low_bwgt[datanew$dmeduc == 4])
mean(datanew$low_bwgt[datanew$dmeduc == 5])
mean(datanew$low_bwgt[datanew$dmeduc == 6])
mean(datanew$low_bwgt[datanew$dmeduc == 7])
mean(datanew$low_bwgt[datanew$dmeduc == 8])
mean(datanew$low_bwgt[datanew$dmeduc == 9])
mean(datanew$low_bwgt[datanew$dmeduc == 10])
mean(datanew$low_bwgt[datanew$dmeduc == 11])
mean(datanew$low_bwgt[datanew$dmeduc == 12])
mean(datanew$low_bwgt[datanew$dmeduc == 13])
mean(datanew$low_bwgt[datanew$dmeduc == 14])
mean(datanew$low_bwgt[datanew$dmeduc == 15])
mean(datanew$low_bwgt[datanew$dmeduc == 16])
mean(datanew$low_bwgt[datanew$dmeduc == 17])

datanew$prop_low_bwgt[datanew$dmeduc == 0] = 0
datanew$prop_low_bwgt[datanew$dmeduc == 1] = 0
datanew$prop_low_bwgt[datanew$dmeduc == 2] = 0
datanew$prop_low_bwgt[datanew$dmeduc == 3] = 0.125
datanew$prop_low_bwgt[datanew$dmeduc == 4] = 0
datanew$prop_low_bwgt[datanew$dmeduc == 5] = 0.06666667
datanew$prop_low_bwgt[datanew$dmeduc == 6] = 0.08536585
datanew$prop_low_bwgt[datanew$dmeduc == 7] = 0.1304348
datanew$prop_low_bwgt[datanew$dmeduc == 8] = 0.06818182
datanew$prop_low_bwgt[datanew$dmeduc == 9] = 0.1160221
datanew$prop_low_bwgt[datanew$dmeduc == 10] = 0.136612
datanew$prop_low_bwgt[datanew$dmeduc == 11] = 0.09302326
datanew$prop_low_bwgt[datanew$dmeduc == 12] = 0.07935223
datanew$prop_low_bwgt[datanew$dmeduc == 13] = 0.06574394
datanew$prop_low_bwgt[datanew$dmeduc == 14] = 0.05910165
datanew$prop_low_bwgt[datanew$dmeduc == 15] = 0.06716418
datanew$prop_low_bwgt[datanew$dmeduc == 16] = 0.05423729
datanew$prop_low_bwgt[datanew$dmeduc == 17] = 0.06685237

###################################
###################################
#
# VS comment: You'd want to store these values in a vector Charlie. You might make an error typing it.
# For instance: lb_educ17 =  mean(datanew$low_bwgt[datanew$dmeduc == 17]) do for other values of education as well
# then lb <- c(lb_educ0, lb_educ1, ...)

help(plot)
plot(datanew$dmeduc, datanew$prop_low_bwgt, main = "")
title(main="Mothers Years of Schooling & Proportion of Infants with Low Birth Weights")

datanew$aboveHS[datanew$dmeduc<=12] = 0
####If aboveHS = 0 then it is at or below highschool
datanew$aboveHS[datanew$dmeduc>12] = 1

reg2 = lm(low_bwgt~aboveHS, datanew)
summary(reg2)

reg3 = lm(race_white~aboveHS, datanew)
summary(reg3)

reg4 = lm(low_bwgt~aboveHS, datanew)
summary(reg4)

#
# VS comment: Your comments are missing. What do you think of omitted variable bias?