hse <- read.table("~/Downloads/housing.data.txt", quote="\"", comment.char="")
colnames(hse) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
View(hse)
pairs(~MEDV+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT,data=hse, 
      main="Simple Scatterplot Matrix")

# fit model
fit1<-lm(MEDV~.,data=hse)

summary(fit1)

# QQ Plot
qqnorm(fit1$residuals)
qqline(fit1$residuals)

# Residual Plot
par(mfrow = c(2,2))
plot(hse$CRIM,residuals(fit1),xlab="CRIM", main="Residuals vs CRIM")
plot(hse$ZN,residuals(fit1),xlab="ZN", main="Residuals vs ZN")
plot(hse$INDUS,residuals(fit1),xlab="INDUS", main="Residuals vs INDUS")
plot(hse$CHAS,residuals(fit1),xlab="CHAS", main="Residuals vs CHAS")
plot(hse$NOX,residuals(fit1),xlab="NOX", main="Residuals vs NOX")
plot(hse$RM,residuals(fit1),xlab="RM", main="Residuals vs RM")
plot(hse$AGE,residuals(fit1),xlab="AGE", main="Residuals vs AGE")
plot(hse$DIS,residuals(fit1),xlab="DIS", main="Residuals vs DIS")
plot(hse$RAD,residuals(fit1),xlab="RAD", main="Residuals vs RAD")
plot(hse$TAX,residuals(fit1),xlab="TAX", main="Residuals vs TAX")
plot(hse$PTRATIO,residuals(fit1),xlab="PTRATIO", main="Residuals vs PTRATIO")
plot(hse$B,residuals(fit1),xlab="B", main="Residuals vs B")
plot(hse$LSTAT,residuals(fit1),xlab="LSTAT", main="Residuals vs LSTAT")

# Backward step wise
scope<-list(upper=MEDV~., lower=Sales~1)
fit1.backward<-step(fit1,direction='backward',scope=scope)

# The complete model
fit1<-lm(MEDV~.,data=hse)
# Reduced Model
fit2 <- lm(MEDV~CRIM+ZN+CHAS+NOX+RM+DIS+RAD+TAX+PTRATIO+B+LSTAT, data = hse)
# Square the RM variable
fit3 <- lm(MEDV~CRIM+ZN+CHAS+NOX+RM+I(RM^2)+DIS+RAD+TAX+PTRATIO+B+LSTAT, data = hse)
# Square the LSTAT model
fit4 <- lm(MEDV~CRIM+ZN+CHAS+NOX+RM+DIS+RAD+TAX+PTRATIO+B+LSTAT+I(LSTAT^2), data = hse)

# Both squared
fit5 <- lm(MEDV~CRIM+ZN+CHAS+NOX+RM+I(RM^2)+DIS+RAD+TAX+PTRATIO+B+LSTAT+I(LSTAT^2), data = hse)

# Partial F test between the complete model and reduced
anova(fit2, fit3)
anova(fit2, fit4)
anova(fit2, fit5)

# Summary for fit5
summary(fit5)

# QQ Normal for the new model
qqnorm(fit5$residuals)
qqline(fit5$residuals)

summary(hse)