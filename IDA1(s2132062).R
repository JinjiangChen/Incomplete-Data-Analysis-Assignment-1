#Incomplete Data Analysis
#Assignment 2 
#Jinjiang Chen
#s2132062

# 3.(a)
set.seed(1)
z1 = rnorm(500,mean = 0, sd = 1)
z2 = rnorm(500,mean = 0, sd = 1)
z3 = rnorm(500,mean = 0, sd = 1)
Y1 = 1 + z1
Y2 = 5 + 2*z1 +z2
data = data.frame(Y1 , Y2)
a = 2
b =0
Y2_mis = which(a*(data$Y1 - 1) + b * (data$Y2 -5) + z3 < 0)
Y2_1 = ifelse (a*(data$Y1 - 1) + b * (data$Y2 -5) + z3 < 0 , NA ,data$Y2)
data2 = data = data.frame(Y1, Y2_1)
data2
Y2_obs = Y2[-Y2_mis]
plot(density(Y2_obs), lwd = 2, col = "blue", xlab = "Y2", ylab = "density", main = "marginal distribution of Y2(a=2,b=0)")
lines(density(Y2), lwd = 2, col = "red")
legend(8, 0.2, legend = c("complete data", "observed data"), col = c("red", "blue"), lty = c(1, 1))


# 3.(b)
fit = lm(Y2 ~ Y1, data = data)
summary(fit)
fit$coefficients

predicted_sri = predict(fit, newdata = data) + rnorm(nrow(data), 0, sigma(fit))
predicted_sri
Y2_2 = ifelse(is.na(Y2_1), predicted_sri, data2$Y2_1)
data3 = data.frame(Y1, Y2_2)

plot(density(Y2_2), lwd = 2, col = "blue", xlab = "Y2", ylab = "density", main = "marginal distribution of Y2(a=2,b=0)")
lines(density(Y2), lwd = 2, col = "red")
legend(9, 0.18, legend = c("complete data", "observed data"), col = c("red", "blue"), lty = c(1, 1))


# 3.(c)
set.seed(1)
z1 = rnorm(500,mean = 0, sd = 1)
z2 = rnorm(500,mean = 0, sd = 1)
z3 = rnorm(500,mean = 0, sd = 1)
Y1 = 1 + z1
Y2 = 5 + 2*z1 +z2
data = data.frame(Y1 , Y2)
a = 0
b = 2
Y2_mis_2 = which(a*(data$Y1 - 1) + b * (data$Y2 -5) + z3 < 0)
Y2_1_2 = ifelse (a*(data$Y1 - 1) + b * (data$Y2 -5) + z3 < 0 , NA ,data$Y2)
data2 = data = data.frame(Y1, Y2_1_2)
data2
Y2_obs_2 = Y2[-Y2_mis_2]
plot(density(Y2_obs_2), lwd = 2, col = "blue",xlim = c(-6, 14),  xlab = "Y2", ylab = "density", main = "marginal distribution of Y2(a=0,b=2)")
lines(density(Y2), lwd = 2, col = "red")
legend(8, 0.28, legend = c("complete data", "observed data"), col = c("red", "blue"), lty = c(1, 1))


# 3.(d)
fit = lm(Y2 ~ Y1, data = data)
summary(fit)
fit$coefficients

predicted_sri = predict(fit, newdata = data) + rnorm(nrow(data), 0, sigma(fit))
predicted_sri
Y2_2_2 = ifelse(is.na(Y2_1_2), predicted_sri, data2$Y2_1_2)
data3 = data.frame(Y1, Y2_2_2)

plot(density(Y2_2_2), lwd = 2, col = "blue", xlab = "Y2", ylab = "density", main = "marginal distribution of Y2(a=0,b=2)")
lines(density(Y2), lwd = 2, col = "red")
legend(9, 0.18, legend = c("complete data", "observed data"), col = c("red", "blue"), lty = c(1, 1))


# 4.(a)
load("~/Desktop/databp.Rdata")
View(databp)
ind = which(is.na(databp$recovtime) == FALSE)
mrecovtime = mean(databp$recovtime, na.rm = TRUE)
serecovtime = sd(databp$recovtime, na.rm = TRUE)/sqrt(length(ind))
mrecovtime; serecovtime
cor(databp$logdose, databp$recovtime, use = "complete")
cor(databp$bloodp, databp$recovtime, use = "complete")


# 4.(b)
ret_mis = ifelse(is.na(databp$recovtime), mrecovtime, databp$recovtime)
mean(ret_mis)
sd(ret_mis)/sqrt(length(ret_mis))
cor(ret_mis, databp$logdose)
cor(ret_mis, databp$bloodp)


# 4.(c)
fit = lm(recovtime ~ logdose + bloodp , data = databp)
summary(fit)
fit$coefficients
predri = predict(fit, newdata = databp)
ret_ri = ifelse(is.na(databp$recovtime) == TRUE, predri, databp$recovtime)
mri = mean(ret_ri)
seri = sd(ret_ri)/sqrt(length(ret_ri))
mri; seri
cor(ret_ri, databp$logdose)
cor(ret_ri, databp$bloodp)



# 4.(d)
set.seed(1)
predsri = predict(fit, newdata = databp) + rnorm(25, 0 , sigma(fit))
ret_sri = ifelse(is.na(databp$recovtime)== TRUE, predsri, databp$recovtime)
msri = mean(ret_sri)
sesri = sd(ret_sri)/sqrt(length(ret_sri))
msri; sesri
cor(ret_sri, databp$logdose)
cor(ret_sri, databp$bloodp)



# 4.(e)
mlod = mean(databp$logdose)
mbld = mean(databp$bloodp)
mlod; mbld
ind_donors4 = which(is.na(databp$recovtime) == FALSE & databp$logdose < mlod & databp$bloodp < mbld)
ind_donors4
donor4 = sample(ind_donors4, 1, replace = TRUE)
donor4
ind_donors10 = which(is.na(databp$recovtime) == FALSE & databp$logdose > mlod & databp$bloodp > mbld)
ind_donors10
donor10 = sample(ind_donors10, 1, replace = TRUE)
donor10
ind_donors22 = which(is.na(databp$recovtime) == FALSE & databp$logdose > mlod & databp$bloodp > mbld)
ind_donors22
donor22 = sample(ind_donors22, 1, replace = TRUE)
donor22  
ret_hd = c(databp$recovtime[is.na(databp$recovtime) == FALSE], databp$recovtime[donor4], databp$recovtime[donor10], databp$recovtime[donor22])  
mret_hd = mean(ret_hd)
seret_hd = sd(ret_hd)/sqrt(length(ret_hd))
mret_hd;seret_hd
