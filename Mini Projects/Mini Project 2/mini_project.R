# # Read the data from roadrace.csv
data.1 <- read.csv("./Desktop/code/UTD\ Course\ Work/Statistical\ Methods\ For\ Data\ Science\ CS_6313/Mini\ Projects/Mini\ Project\ 2/roadrace.csv")

# Apply barplot function on column 11 and generate plots for variables Maine and Away
barplot(c(sum(data.1$Maine == 'Maine'), sum(data.1$Maine == 'Away')), names.arg = c('Maine', 'Away'), space = 0.2, ylab = 'Runners')

maine.sum = sum(data.1$Maine=='Maine')
away.sum = sum(data.1$Maine=='Away')
print(maine.sum)
print(away.sum)
#Total Runners
total <- sum(data.1$Maine=='Maine')+sum(data.1$Maine=='Away')
print(total)

#Calculate percentage
maine.percent <- (maine.sum/total)*100
print(maine.percent)

away.percent <- (away.sum/total)*100
print(away.percent)

#Histogram for Maine group run time
maine.runtime <- data.1$Time..minutes.[which(data.1$Maine=='Maine')] 
hist(maine.runtime, xlim = range(0,200), ylim = range(0,2000))

#Histogram for Away group run time
away.runtime <- data.1$Time..minutes.[which(data.1$Maine=='Away')]
hist(away.runtime, xlim = range(0,200), ylim = range(0,2000))


summary(maine.runtime)
summary(away.runtime)

mean(maine.runtime)
mean(away.runtime)

range(maine.runtime)
range(away.runtime)

sd(maine.runtime)
sd(away.runtime)

IQR(maine.runtime)
IQR(away.runtime)


#boxplot
boxplot(maine.runtime, away.runtime, names=c('Maine', 'Away'))



male.runner.age = strtoi(data.1$Age[which(data.1$Sex=='M')])
female.runner.age = strtoi(data.1$Age[which(data.1$Sex=='F')])
boxplot(male.runner.age, female.runner.age, names=c('MaleRunnersAge', 'FemaleRunnersAge'))

summary(male.runner.age)
mean(male.runner.age)
range(male.runner.age)
sd(male.runner.age)
IQR(male.runner.age)

summary(female.runner.age)
mean(female.runner.age)
range(female.runner.age)
sd(female.runner.age)
IQR(female.runner.age)


# Return the MOM and MLE
mle.mom <- function(n, theta){
  sample_data = runif(n, 0, theta)
  method_moments = 2 * mean(sample_data)
  max_likelihood = max(sample_data)
  return (c(max_likelihood, method_moments))
}

# to calculate the mean squared error
mse <- function (n, theta){
  estimator = replicate(1000, mle.mom(n,theta))
  estimator = (estimator-theta)^2
  estimator.mom = estimator[c(TRUE, FALSE)]
  estimator.mle = estimator[c(FALSE, TRUE)]
  return (c(mean(estimator.mle),mean(estimator.mom)))
}

mse(1,1)

d= c(1,2,3,4,5)
d[c(FALSE,TRUE)]

#mean squared for all combinations of theta
mse.1.1 = mse(1,1)
mse.1.5 = mse(1,5)
mse.1.50 = mse(1,50)
mse.1.100 = mse(1,100)
mse.2.1 = mse(2,1)
mse.2.5 = mse(2,5)
mse.2.50 = mse(2,50)
mse.2.100 = mse(2,100)
mse.3.1 = mse(3,1)
mse.3.5 = mse(3,5)
mse.3.50 = mse(3,50)
mse.3.100 = mse(3,100)
mse.5.1 = mse(5,1)
mse.5.5 = mse(5,5)
mse.5.50 = mse(5,50)
mse.5.100 = mse(5,100)
mse.10.1 = mse(10,1)
mse.10.5 = mse(10,5)
mse.10.50 = mse(10,50)
mse.10.100 = mse(10,100)
mse.30.1 = mse(30,1)
mse.30.5 = mse(30,5)
mse.30.50 = mse(30,50)
mse.30.100 = mse(30,100)

#draw graphs
# fixed n varying theta
# n -> 1
plot(c(1,5,50,100), 
     c(mse.1.1[1],mse.1.5[1], mse.1.50[1], mse.1.100[1]), type="b", 
     xlab = 'theta', ylab = 'MSE', col = 'red', main = "n = 1")
lines(c(1,5,50,100), c(mse.1.1[2],mse.1.5[2], mse.1.50[2], 
                       mse.1.100[2]), type="b", col = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), 
       text.col = c('black','black'),lty = 1, pch = 1, 
       inset =0.01, ncol = 1, cex = 0.6, bty = 'n')

#n -> 2
plot(c(1,5,50,100), 
     c(mse.2.1[1],mse.2.5[1], mse.2.50[1], mse.2.100[1]), type="b", 
     xlab = 'theta', ylab = 'MSE', col = 'red', main = "n = 2")
lines(c(1,5,50,100), c(mse.2.1[2],mse.2.5[2], mse.2.50[2], 
                       mse.2.100[2]), type="b", col = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), 
       text.col = c('black','black'),lty = 1, pch = 1, 
       inset =0.01, ncol = 1, cex = 0.6, bty = 'n')


#n -> 3
plot(c(1,5,50,100), 
     c(mse.3.1[1],mse.3.5[1], mse.3.50[1], mse.3.100[1]), type="b", 
     xlab = 'theta', ylab = 'MSE', col = 'red', main = "n = 3")
lines(c(1,5,50,100), c(mse.3.1[2],mse.3.5[2], mse.3.50[2], 
                       mse.3.100[2]), type="b", col = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), 
       text.col = c('black','black'),lty = 1, pch = 1, 
       inset =0.01, ncol = 1, cex = 0.6, bty = 'n')

#n -> 5
plot(c(1,5,50,100), 
     c(mse.5.1[1],mse.5.5[1], mse.5.50[1], mse.5.100[1]), type="b", 
     xlab = 'theta', ylab = 'MSE', col = 'red', main = "n = 5")
lines(c(1,5,50,100), c(mse.5.1[2],mse.5.5[2], mse.5.50[2], 
                       mse.5.100[2]), type="b", col = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), 
       text.col = c('black','black'),lty = 1, pch = 1, 
       inset =0.01, ncol = 1, cex = 0.6, bty = 'n')


#n -> 10
plot(c(1,5,50,100), 
     c(mse.10.1[1],mse.10.5[1], mse.10.50[1], mse.10.100[1]), type="b", 
     xlab = 'theta', ylab = 'MSE', col = 'red', main = "n = 10")
lines(c(1,5,50,100), c(mse.10.1[2],mse.10.5[2], mse.10.50[2], 
                       mse.10.100[2]), type="b", col = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), 
       text.col = c('black','black'),lty = 1, pch = 1, 
       inset =0.01, ncol = 1, cex = 0.6, bty = 'n')

#n -> 30
plot(c(1,5,50,100), 
     c(mse.30.1[1],mse.30.5[1], mse.30.50[1], mse.30.100[1]), type="b", 
     xlab = 'theta', ylab = 'MSE', col = 'red', main = "n = 30")
lines(c(1,5,50,100), c(mse.30.1[2],mse.30.5[2], mse.30.50[2], 
                       mse.30.100[2]), type="b", col = 'blue')
legend("topleft", legend = c("MLE", "MOM"), col = c('red', 'blue'), 
       text.col = c('black','black'),lty = 1, pch = 1, 
       inset =0.01, ncol = 1, cex = 0.6, bty = 'n')

#fixed theta varying n
#theta -> 1
plot(c(1,2,3,5,10,30), c(mse.1.1[1],mse.2.1[1], mse.3.1[1], mse.5.1[1], 
     mse.10.1[1], mse.30.1[1]), type="b", ylab = 'MSE',
     xlab = 'n', col = 'red', main = "theta = 1")
lines(c(1,2,3,5,10,30), c(mse.1.1[2],mse.2.1[2], mse.3.1[2], mse.5.1[2], 
                          mse.10.1[2], mse.30.1[2]), type="b", col = 'blue')
legend("topright", legend = c("MLE", "MOM"), col = c('red', 'blue'), 
       text.col = c('black','black'),lty = 1, pch = 1, 
       inset =0.01, ncol = 1, cex = 0.6, bty = 'n')

#theta -> 50
plot(c(1,2,3,5,10,30), c(mse.1.50[1],mse.2.50[1], mse.3.50[1], mse.5.50[1], 
                         mse.10.50[1], mse.30.50[1]), type="b", ylab = 'MSE',
     xlab = 'n', col = 'red', main = "theta = 50")
lines(c(1,2,3,5,10,30), c(mse.1.50[2],mse.2.50[2], mse.3.50[2], mse.5.50[2], 
                          mse.10.50[2], mse.30.50[2]), type="b", col = 'blue')
legend("topright", legend = c("MLE", "MOM"), col = c('red', 'blue'), 
       text.col = c('black','black'),lty = 1, pch = 1, 
       inset =0.01, ncol = 1, cex = 0.6, bty = 'n')

#theta -> 100
plot(c(1,2,3,5,10,30), c(mse.1.100[1],mse.2.100[1], mse.3.100[1], mse.5.100[1], 
                         mse.10.100[1], mse.30.100[1]), type="b", ylab = 'MSE',
     xlab = 'n', col = 'red', main = "theta = 100")
lines(c(1,2,3,5,10,30), c(mse.1.100[2],mse.2.100[2], mse.3.100[2], mse.5.100[2], 
                          mse.10.100[2], mse.30.100[2]), type="b", col = 'blue')
legend("topright", legend = c("MLE", "MOM"), col = c('red', 'blue'), 
       text.col = c('black','black'),lty = 1, pch = 1, 
       inset =0.01, ncol = 1, cex = 0.6, bty = 'n')

#theta -> 5
plot(c(1,2,3,5,10,30), c(mse.1.5[1],mse.2.5[1], mse.3.5[1], mse.5.5[1], 
                         mse.10.5[1], mse.30.5[1]), type="b", ylab = 'MSE',
     xlab = 'n', col = 'red', main = "theta = 5")
lines(c(1,2,3,5,10,30), c(mse.1.5[2],mse.2.5[2], mse.3.5[2], mse.5.5[2], 
                          mse.10.5[2], mse.30.5[2]), type="b", col = 'blue')
legend("topright", legend = c("MLE", "MOM"), col = c('red', 'blue'), 
       text.col = c('black','black'),lty = 1, pch = 1, 
       inset =0.01, ncol = 1, cex = 0.6, bty = 'n')







