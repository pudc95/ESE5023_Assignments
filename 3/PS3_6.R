library(MASS)
library(leaps)
library(tidyr)
data(cpus)
str(cpus)

index <- sample(nrow(cpus),nrow(cpus)*0.80)
cpus_train <- cpus[index,]
cpus_test  <- cpus[-index,]

#6.1
train_result <- regsubsets(perf~syct+mmin+mmax+cach+chmin+chmax, 
                            data=cpus_train, nbest= 2)
plot(train_result, scale="bic")
summary(train_result)

#6.2
model <- lm(perf~syct+mmin+mmax+cach+chmax, data = cpus_train)
summary(model)
pre <- predict(model, cpus_test)
a <- seq(1,42,1)
cpus_test_new <- as.data.frame(cpus_test)
plot(pre, cpus_test_new$perf, xlab = 'Predict', ylab = 'True')
fit <- lm(pre~cpus_test_new$perf)
summary(fit)
abline(fit, lwd = 2, col = "red")
sqrt(mean((cpus_test_new$perf - pre)^2)/42)
