data = read.csv("/Users/leahsine/Dropbox/My Mac (Leah’s MacBook Pro)/Desktop/Class/ECON 334/NCHS_birthweight2000_sample.csv")
head(data)
datainc = read.csv("/Users/leahsine/Dropbox/My Mac (Leah’s MacBook Pro)/Desktop/Class/ECON 334/NCHS_income.csv")
head(datainc)

hist(datainc$annualincome)
datanew = merge(data, datainc, by = "person_id", all.x=T)
head(datanew)

datanew = subset(datanew, dmeduc<99) 

reg1 = lm(dbirwt~dmeduc, datanew)
summary(reg1)

datanew$low_bwgt = 0
datanew$low_bwgt [datanew$dbirwt < 2500] = 1

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

###################################################
###################################################
###################################################
# VS comment: What you have done here is fine. 
# However, typing values as you did can lead to mistakes. One 
# can always mistype. So what you want to do is store your means that 
# you calcualted above. For instance for educ of 17, 
lbmean17 = mean(datanew$low_bwgt[datanew$dmeduc == 17])
# then do for other values of education. 
# Then create a vector.
prop_lowbwt <- c(lbmean0, lbmean1, ......)
# use a sequence command to get c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17); try educ <- seq(from = 1, to = 17, by = 1) 
# then plot(educ, prop_lowbwt)



help(plot)
plot(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), c(0,0,0,.125,0,.066,.085,.130,.068,.116,.136,.093,.079,.066,.059,.067,.054,.067))

datanew$aboveHS[datanew$dmeduc<=12] = 0
### If aboveHS = 0 then it is at or below highschool
datanew$aboveHS[datanew$dmeduc>12] = 1

reg2 = lm(low_bwgt~aboveHS, datanew) ##VS comments. add comments; eg. regression of low birth weight on above HS educ
summary(reg2)

reg3 = lm(race_white~aboveHS, datanew)  ##VS comments. race on above HS
summary(reg3)

reg4 = lm(low_bwgt~aboveHS, datanew)
summary(reg4)

reg5 = lm(low_bwgt~aboveHS+annualincome, datanew)
summary(reg5)

#################################
#################################
#
# VS: Good work. 
##1. Keep comments; this'll help you to stay on track
##2. Do some readings on vectors and matrix, storing an object in different forms, various types of data (string, numeric, character, factor)
##3. Never take shortcuts, 
## plot(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), c(0,0,0,.125,0,.066,.085,.130,.068,.116,.136,.093,.079,.066,.059,.067,.054,.067))
## This is an example of a shortcut. 
##4. Keep practicing; use R for things, in other classes, at home; instead of excel. 
