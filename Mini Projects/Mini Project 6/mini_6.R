#read from file
pr_cancer_data=read.csv(file="./prostate_cancer.csv",sep=",", header=T)
str(pr_cancer_data)

#fetching the column data
subject = pr_cancer_data$subject
psa = pr_cancer_data$psa
weight = pr_cancer_data$weight
cancervol = pr_cancer_data$cancervol
age = pr_cancer_data$age
benpros = pr_cancer_data$benpros
vesinv = pr_cancer_data$vesinv
capspen = pr_cancer_data$capspen
gleason = pr_cancer_data$gleason

#Distributions of psa and log(psa)
boxplot(psa)
boxplot(log(psa))

#Based on the boxplots log(psa) has a better distribution
y=log(psa)

plot(weight, y)
fit1 <- lm(y ~ weight, data = pr_cancer_data)
abline(fit1)
summary(fit1)

plot(cancervol, y)
fit2 <- lm(y ~ cancervol, data = pr_cancer_data)
abline(fit2)
summary(fit2)

plot(age, y)
fit3 <- lm(y ~ age, data = pr_cancer_data)
abline(fit3)
summary(fit3)

plot(benpros, y)
fit4 <- lm(y ~ benpros, data = pr_cancer_data)
abline(fit4)
summary(fit4)

plot(factor(vesinv), y)
fit5 <- lm(y ~ factor(vesinv), data = pr_cancer_data)

summary(fit5)

plot(capspen, y)
fit6 <- lm(y ~ capspen, data = pr_cancer_data)
abline(fit6)
summary(fit6)

plot(gleason, y)
fit7 <- lm(y ~ gleason, data = pr_cancer_data)
abline(fit7)
summary(fit7)

#start with cancervol and compare to cancervol + vesinv (two best significance levels)
fit8 = lm(y ~ cancervol+factor(vesinv), data = pr_cancer_data)
anova(fit2, fit8)

#Result of anova(fit2, fit8): p = 0.002953, so vesinv is significant
fit9 = lm(y ~ capspen+factor(vesinv)+cancervol, data = pr_cancer_data)
anova(fit8, fit9)

#Result of anova(fit8, fit9): P = 0.7616, so capspen is not significant
fit10 = lm(y ~cancervol + factor(vesinv) + gleason, data = pr_cancer_data)
anova(fit8, fit10)

#Result of anova(fit8, fit10): p = 0.003804, so gleason is significant
fit11 = lm(y ~ cancervol + factor(vesinv) + gleason + age + benpros + weight , data = pr_cancer_data)
anova(fit10,fit11)

#Result of anova(fit10,fit11): p = 0.007466, so at least one of age + benpros + weight is significant
fit12 = lm(y ~ cancervol + factor(vesinv) + gleason + age, data = pr_cancer_data)
anova(fit10,fit12)

#Result of anova(fit10,fit12): P = 0.2995, so age is not significant
fit13 = lm(y ~ cancervol + factor(vesinv) + gleason + benpros, data = pr_cancer_data)
anova(fit10,fit13)

#Result of anova(fit10,fit13): P = 0.0007054, so benpros is significant

fit14 = lm(y ~ cancervol + factor(vesinv) + gleason + benpros + weight, data=pr_cancer_data)
anova(fit13,fit14)

#Result of anova(fit13,fit14): P = 0.4527, so weight is not significant

#Model: y ~ cancervol + gleason + benpros + vesinv

#Compare our model to forward selection method 
fit10.forward <- step(lm(y ~ 1, data = pr_cancer_data),
                      scope = list(upper = ~cancervol + age + weight + benpros + factor(vesinv) + capspen + gleason),
                      direction = "forward")

#Result: y ~ cancervol + gleason + benpros + vesinv
#compare our model to backward selection method
fit11.backward <- step(lm(y ~ cancervol + age + weight + benpros + factor(vesinv) + capspen + gleason, data = pr_cancer_data),
                       scope = list(lower = ~1), direction = "backward")
#result: y ~ cancervol + benpros + vesinv + gleason

#compare our model to both selection method 
fit12.both <- step(lm(y ~ 1, data = pr_cancer_data), 
                   scope = list(lower = ~1, upper = ~cancervol + age + weight + benpros + factor(vesinv) + capspen + gleason), 
                   direction = "both")

#result: cancervol + gleason + benpros + vesinv
#  residual plot 
plot(fitted(fit13), resid(fit13)) 
abline(h = 0)
#  plot of absolute residuals 
plot(fitted(fit13), abs(resid(fit13)))
#  normal QQ plot 
qqnorm(resid(fit13)) 
qqline(resid(fit13))
#qq plot looks accurate
#All vars in our model are signifcant

summary(fit13)
get.mode <- function(k) {
  u = unique(k)
  u[which.max(tabulate(match(k, u)))]
}

#Get the means of cancervolm benpros, gleason, and mode of vesinv.

cancervol.mean = mean(pr_cancer_data$cancervol)
benpros.mean = mean(pr_cancer_data$benpros)
temp.vesinv.list = as.numeric(unlist(factor(pr_cancer_data$vesinv)))

vesinv.mode = get.mode(temp.vesinv.list)

gleason.mean = mean(pr_cancer_data$gleason)

#Model : y ~ cancervol + factor(vesinv) + gleason + benpros

#Find prediction using sample means
prediction = (fit13$coefficients[1] + cancervol.mean * fit13$coefficients[2] 
              +0*fit13$coefficients[3]+gleason.mean*fit13$coefficients[4]
              +benpros.mean*fit13$coefficients[5])
print(prediction)
print(exp(prediction))
