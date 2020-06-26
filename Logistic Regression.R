getwd()
claimaints<- read.csv(file.choose()) 
View(claimaints)
attach(claimaints)
summary(claimaints)
colSums(is.na(claimaints))

#Logistic Regression
a<- na.omit(claimaints)
colSums(is.na(a))
summary(a)
colnames(claimaints)
str(claimaints)
str(ATTORNEY)
str(CLMSEX)
str(as.factor(CLMSEX))

m1<- glm(ATTORNEY ~ factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT)
         +(CLMAGE)+(LOSS), family=binomial,data = claimaints)
summary(m1)

prob<- predict(m1,claimaints)
(prob)
pvprob<- as.data.frame(prob)
View(pvprob)
#final<- cbind(pvprob,a)
table(claimaints$ATTORNEY)
table(prob>=0.5)

confusion<- table(claimaints$ATTORNEY, prob>=0.5)
confusion

sum(diag(confusion))
accuracy<- sum(diag(confusion))/sum(confusion)
accuracy
