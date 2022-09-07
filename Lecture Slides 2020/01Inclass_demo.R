###################################
###################################
#
# In lecture codes (Lecture 1)
#
###################################
###################################

height <- rnorm(n = 1000, mean = 5.7, sd = 1.2)
# histogram
hist(height)
mean(height) #short version
sd(height)

# longer version
sumofx <- sum(height)
n <- length(height)
mean_height <- sumofx/n

########################
########################
#
# Calcuate the standard dev.
#
########################
########################
#1. shorter 
#2. longer way



##############################
##############################
#
# 1. weighted average 
#
##############################
##############################

exams <- c(100, 100, 100)
exam_weights <- c(0.25, 0.25, 0.5)
your_score <- c(100, 90, 75)

final_score <- 100 * 0.25 + 90 * 0.25 + 75 * 0.5

mean(your_score) # wrong statistic



#######################################
#######################################
#
# Covariance 
#
#######################################
#######################################

X <- rnorm(1000, mean = 0, sd = 1)
Y <- 5 + 2 * X + rnorm(1000, 0, 1)

plot(X, Y)

# create means of X and Y
meanX <- mean(X)
meanY <- mean(Y)

# (X - E(X)) #nolint
spreadX <- X - meanX
# (Y - E(Y)) #nolint
spreadY <- Y - meanY

#covariance
covXY <- sum(spreadX * spreadY)/(length(X)-1)

# short version
cov(X, Y)

# product of sd of X and Y
product_sd <- sqrt(var(X) * var(Y))

corrXY <- covXY / product_sd
cor(X, Y)













