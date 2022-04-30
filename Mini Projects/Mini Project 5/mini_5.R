body.temp.heart.rate = read.csv("bodytemp-heartrate.csv", header=T)
#male and female separate datasets
male = subset(body.temp.heart.rate, body.temp.heart.rate$gender==1)
female = subset(body.temp.heart.rate, body.temp.heart.rate$gender==2)

# boxplots
boxplot(male$body_temperature, female$body_temperature, 
        main = "Boxplots of body temperatures", names = c('Males', 'Females'), ylab = "Temperature")

# QQ Plot
qqnorm(male$body_temperature, main='Q-Q Plot for male')
qqline(male$body_temperature)
qqnorm(female$body_temperature, main='Q-Q Plot for female')
qqline(female$body_temperature)

t.test(male$body_temperature, female$body_temperature, alternative = 'two.sided', var.equal = F)

boxplot(male$heart_rate, female$heart_rate, 
        main = "Boxplots of heart rate", names = c('Males', 'Females'), ylab = "Heart Rate")

# QQ Plot
qqnorm(male$heart_rate, main='Q-Q Plot for male')
qqline(male$heart_rate)
qqnorm(female$heart_rate, main='Q-Q Plot for female')
qqline(female$heart_rate)

t.test(male$heart_rate, female$heart_rate, alternative = 'two.sided', var.equal = F)


plot(male$heart_rate, male$body_temperature, pch=1, main='Scatter plot for males')
abline(lm(male$body_temperature~male$heart_rate))

plot(female$heart_rate, female$body_temperature, pch=1, main='Scatter plot for females')
abline(lm(female$body_temperature~female$heart_rate))

cor(male$body_temperature,male$heart_rate)
cor(female$body_temperature,female$heart_rate)

nboot <- 5000
nVals = c(5, 10, 30, 100)
lambdaVals = c(0.01, 0.1, 1, 10)
alpha = 0.05
for (n in nVals) {
  for (l in lambdaVals) {
    x = replicate(5000,sort(rexp(n, l)))
    mean.x = colMeans(x)
    sd.x =apply(x, 2, sd)
    conf.int.lower = mean.x + -1 * qnorm(1 - (alpha/2)) * sd.x/sqrt(n)
    conf.int.higher = mean.x + 1 * qnorm(1 - (alpha/2)) * sd.x/sqrt(n)
    coverage.prop= (1/l >= conf.int.lower & 1/l <= conf.int.higher) 
    cat(n, l, mean(coverage.prop==TRUE), "\n")
  }
}

plot(c(0.01,0.1,1,10), c(0.9412, 0.937, 0.9392, 0.9402), main = "N = 100", xlab = 'Lambda', ylab = 'Proportions', col = 'blue', type = 'b', xlim = c(0.01,10), ylim = c(0,1))
lines(c(0.01,0.1,1,10), c(0.9526,0.9428,0.9458,0.9434), col = 'red', type = 'b')

lambda.star <- function(x){
    n <- length(x)
    xbar <- 1/mean(x)
    xstar <- replicate(1000, mean(rexp(n, xbar)))
    return (xstar)
}
nboot <- 5000
nVals = c(5, 10, 30, 100)
lambdaVals = c(0.01, 0.1, 1, 10)
for (n in nVals) {
  for (l in lambdaVals) {
    x <- rexp(n,rate=l)
    lambda.boot.dist <- replicate(5000, sort(lambda.star(rexp(n,rate=l))))
    conf.ints.lower = lambda.boot.dist[ceiling(25),]
    conf.ints.higher = lambda.boot.dist[floor(975),]
    coverage.prop= (1/l >= conf.ints.lower & 1/l <= conf.ints.higher)
    cat(n, l, mean(coverage.prop==TRUE), "\n")
  }
}


