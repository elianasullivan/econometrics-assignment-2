#econometrics assignment #2

                                                ####### PROBLEM #1 #######
#import the data from excel
library(readxl)
star <- read_excel("Documents/Documents - Eliana’s MacBook Pro/Minerva/last one/second semester/econometrics/week 12/star.xlsx")

# part a -- estimate a regression equation
model_1 <- lm(mathscore ~ small + aide + tchexper + boy + white_asian, data = star) 
# print a summary of the model
summary(model_1)

# part b -- estimate the model in part a with fixed effects 
# use 'factor' to create dummy variables out of all of the school id's (except for one to avoid collinearity)
model_2 <- lm(mathscore ~ factor(schid) + small + aide + tchexper + boy + white_asian, data = star) 
# print a summary of the model
summary(model_2)

# part c -- test for the significance of the school fixed effects
# calculate the restricted SSE
sse_r <- sum(residuals(model_1)^2)

# calculate the unresricted SSE
sse_u <- sum(residuals(model_2)^2)

# set up J, N, and K
j <- 78
n <- nrow(star)
k <- 78+5

# calculate the f-statistic
f_stat <- ((sse_r-sse_u)/j)/(sse_u/(n-k))
f_stat


# part d -- estimate the model in part (a) with random effects

install.packages("plm")
library(plm)

# make R recognize the data as panel data
star_panel <- pdata.frame(star,index=c("schid"))

# estimate the model
model_3 <- plm(mathscore~small+aide+tchexper+boy+white_asian, data = star_panel,
               random.method="swar", 
               model="random")

# print a summary of the model
summary(model_3)

# part e 
summary(model_2)
summary(model_3)

# the rest of part e was calculated in an excel spreadsheet
# an example calculation is shown in the text, and the outputs are shown in the sheet


                                                ####### PROBLEM #2 #######
#download the data
library(readxl)
chard <- read_excel("Documents/Documents - Eliana’s MacBook Pro/Minerva/last one/second semester/econometrics/week 12/chard.xlsx")

install.packages("data.table")
library(data.table)

#part a
# create the model
rev_model <- lm(q ~ xper + cap + lab, data = chard)

# print a summary of the model
summary(rev_model)

# part b

##THIS VALUE IS CHANGED FOR EACH OF THE SUBPARTS##
# i = 10, ii = 20, iii = 30
exper <- 10

# set up the averages and the variance - covariance matrix
lab_avg <- mean(chard$lab)
cap_avg <- mean(chard$cap)
matrix <- vcov(rev_model)

# from a t-table given that df = 71, signficance = 0.025, one-tailed 
t_value <- 1.9939

# calculate variance
variance <- var(residuals(rev_model))+matrix[c(1),c(1)]+exper^2*matrix[c(2),c(2)]+cap_avg^2*matrix[c(3),c(3)]+lab_avg^2*matrix[c(4),c(4)]+
  2*exper*matrix[c(1),c(2)]+2*cap_avg*matrix[c(1),c(3)]+2*lab_avg*matrix[c(1),c(4)]+2*exper*cap_avg*matrix[c(2),c(3)]+
  2*exper*lab_avg*matrix[c(2),c(4)]+2*cap_avg*lab_avg*matrix[c(3),c(4)]
    
# calculate standard error
standard_error <- sqrt(variance)
standard_error

# calculate q_0 and the 95% confidence bounds automatically with interval = "predict"
prediction <- predict(rev_model, newdata = data.frame(xper = exper, cap = cap_avg, lab = lab_avg, age = age_avg), interval = "predict")
prediction

# this is to calulate the upper and lower bounds manually
lower <- prediction - t_value*standard_error
upper <- prediction + t_value*standard_error
print(lower, prediction, upper)

#part c

# regress experience on age, cap and lab to use age as an instrument for xper
hausman <- lm(xper ~ age + cap + lab, data = chard)

# take the residuals of this regression
resids <- residuals(hausman)

# use these residuals in the original model for the hausman test
new_regression <- lm(q ~ xper + cap + lab + resids, data = chard)

# show the results
summary(new_regression)

# part d

# download necessary packages
install.packages("AER")
library(AER)

# use instrumental variables regression (ivreg)
iv_regression <- ivreg(q ~ xper + cap + lab | cap + lab + age, data = chard, x = TRUE)
summary(iv_regression)

# this gets some extra info
summary(iv_regression, vcov = sandwich, diagnostics = TRUE)


# part e 
##THIS VALUE IS CHANGED FOR EACH OF THE SUBPARTS##
# i = 10, ii = 20, iii = 30
exper <- 10

# set up the averages and the variance - covariance matrix
lab_avg <- mean(chard$lab)
cap_avg <- mean(chard$cap)
age_avg <- mean(chard$age)

# from a t-table given that df = 71, signficance = 0.025, one-tailed 
t_value <- 1.9939

# calculate q_0 and the 95% confidence bounds automatically
prediction <- predict(iv_regression, newdata = data.frame(xper = exper, cap = cap_avg, lab = lab_avg, age = age_avg), interval = "predict")

matrix <- vcov(iv_regression)

variance <- var(residuals(iv_regression))+matrix[c(1),c(1)]+exper^2*matrix[c(2),c(2)]+cap_avg^2*matrix[c(3),c(3)]+lab_avg^2*matrix[c(4),c(4)]+
  2*exper*matrix[c(1),c(2)]+2*cap_avg*matrix[c(1),c(3)]+2*lab_avg*matrix[c(1),c(4)]+2*exper*cap_avg*matrix[c(2),c(3)]+
  2*exper*lab_avg*matrix[c(2),c(4)]+2*cap_avg*lab_avg*matrix[c(3),c(4)]

standard_error <- sqrt(variance)

lower <- prediction - t_value*standard_error
upper <- prediction + t_value*standard_error
print(c(lower,prediction,upper))