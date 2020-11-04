setwd('G:/Sustech/Work/CPER/Assignment/3')
BBT <- read.csv("PS3-5.csv", header = T)
Vel <- BBT$Vel
Dis <- BBT$Dis
Dis <- Dis * 1e6 * 1e12 * 30.9
#5.1
plot(Vel, Dis, xlab = "Velocity(km per second)", ylab = "Distance(km)")

#5.2
fit1 <- lm(Dis~Vel)
abline(fit1, lwd = 2,col = 'red')
summary(fit1)

#5.3
fit2 <- lm(Dis~Vel-1)
abline(fit2, lwd = 2,col = 'blue')
summary(fit2)

