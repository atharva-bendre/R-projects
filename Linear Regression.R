wc.at <- read.csv(file.choose())
dim(wc.at)
View(wc.at)
attach(wc.at)
summary(wc.at)
windows()
#Plotting of dataset
plot(AT,Waist)
cor(AT,Waist)
cor(Waist,AT)
#dim(wc.at)
class(wc.at)
colnames(wc.at)
str(wc.at)
sd(Waist)

# Implementation of Linear
m1 <- lm(AT ~ Waist,data = wc.at)
summary(m1)
pred1 <- predict(m1,newdata = wc.at)
pred1
class(PV)
PV <- as.data.frame(pred1)

logm1 <- lm(AT ~ log(Waist),data = wc.at)
summary(logm1)

m2 <- lm(log(AT) ~ (Waist),data = wc.at)
summary(m2)
final <-data.frame(wc.at,PV)

pred1 <- predict(lmodel1,newdata = wc.at1)
pred1

x <- read.csv(file.choose())
# R-squared value for the above model is 0.68. 
# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
lmodel2 <- lm(AT ~ log(Waist),data = wc.at)  # Regression using logarthmic transformation
summary(lmodel2)
pred <- predict(lmodel2,newdata = test)
class(pred)
pred <- as.data.frame(pred)
final1<-data.frame(test,pred)

pred <- predict(lmodel1,newdata = x )
x<- read.csv(file.choose()) # choose the wc-at.csv data set
# R-squared value for the above model is 0.6723. 
# we may have to do different transformation better R-squared value
# Applying different transformations

# Exponential model 
lmodel3 <- lm(log(AT) ~ (Waist),data = wc.at) # regression using Exponential model
summary(lmodel3)

#SQRT Transformation
lmodel4 <- lm((AT) ~ sqrt(Waist),data = wc.at) # regression using Exponential model
summary(lmodel4)

lmodel5 <- lm((AT) ~ (Waist * Waist),data = wc.at) # regression using Exponential model
summary(lmodel5)

pred <- predict(lmodel3,newdata = wc.at)
exp(pred)
pred <- as.data.frame(exp(pred))
pred<-exp(pred)
final <- cbind(wc.at,pred)
final1 <- data.frame(wc.at,pred)

pred <- predict(lmodel3,newdata = x )

a <-exp(pred)
pred <- as.data.frame(a)
final2 <-data.frame(x,pred)
final3 <- rbind(wc.at,test1)

final2<-data.frame(test,pred)
View(final2)
pred1 <- predict(lmodel3,newdata = x )
x<- read.csv(file.choose()) # choose the wc-at.csv data set

log(90)
exp(pred1)
# R-squared value has increased from 0.68 to 0.73 
# Higher the R-sqaured value - Better chances of getting good model 
# for Waist and addipose Tissue
