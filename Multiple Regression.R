Cars <- read.csv(file.choose()) # choose the Cars.csv data set


View(Cars)
dim(Cars)
attach(Cars)
summary(Cars)
str(HP)
str(Cars)

# Finding the correlation betweenn Output (MPG) & (HP,VOL,SP)-Scatter plot
windows()
plot(Cars)
pairs(Cars)
cor(Cars)
plot(HP,SP)


#Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Cars)

# The Linear Model of interest
m1 <- lm(MPG ~ VOL+ HP + SP+ WT)

summary(m1)

# Prediction based on only Volume 
mv <- lm(MPG ~ VOL,data = Cars)
summary(mv) # Volume is significant

# Prediction based on only Weight
mw <- lm(MPG ~ WT,data = Cars)
summary(mw) # Weight is significant

# Prediction based on Volume and Weight
mvw <- lm(MPG ~ VOL + WT,data = Cars)
summary(mvw) # Both are Insignificant



# It is Better to delete influential observations rather than deleting entire column which is 
library(car)
library(mvinfluence)

## plotting Influential measures 
windows()
influencePlot(m1)

# Regression after deleting the 77th observation, which is influential observation
m2 <- lm(MPG ~ VOL+SP+HP+WT,data=Cars[-77,])
summary(m2)

# Regression after deleting the 77th & 71st Observations
m3 <- lm(MPG ~ VOL+ SP+ HP+ WT,data=Cars[-c(71,1,77),])
summary(m3)

## Variance Inflation factor to check collinearity b/n variables 
vif(m1)

## vif>10 then there exists collinearity among all the variables 

finalmodel <- lm(MPG ~ VOL + SP + HP,data=Cars)

summary(finalmodel)

## Added Variable plot to check correlation b/n variables and o/p variable
windows()
avPlots(m1)
avPlots(finalmodel)

## VIF and AV plot has given us an indication to delete "wt" variable
predict(finalmodel,newdata = Cars)
summary(Cars)
windows()
plot(test)
## Final model
finalmodel1<-lm(MPG ~ log(VOL)+log(SP)+log(HP),data = Cars)
summary(finalmodel1)

predict(finalmodel,newdata = Testcar)
windows()# Evaluate model LINE assumptions 
avPlots(finalmodel)
avPlots(model.car)

stepAIC(m1)
