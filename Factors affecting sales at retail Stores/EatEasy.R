library(ggplot2)
library(dplyr)
library(tidyr)
library(lmtest)
library(sandwich)
library(car)
library(knitr)
library(stargazer)

sales <- read.csv2("Data_EatEasy_text.txt",header = T, strip.white=TRUE,sep = '\t',stringsAsFactors = T)
str(sales)
for (i in 1:12)
  print (length(unique(sales[,i])))

sum(is.na(sales))
sum(!complete.cases(sales))

unique(sales$Item_Fat_Content)
unique(sales$Item_Type)
unique(sales$Outlet_Identifier)
unique(sales$Outlet_Size)
unique(sales$Outlet_Location_Type)
unique(sales$Outlet_Identifier)


sum(sales[,3]=="low fat")
sales[which(sales[,3]=="low fat"),3]<- "LF"
sum(sales[,3]=="low fat")

sum(sales[,3]=="Low Fat")
sales[which(sales[,3]=="Low Fat"),3]<- "LF"
sum(sales[,3]=="Low Fat")

sum(sales[,3]=="Regular")
sales[which(sales[,3]=="Regular"),3]<- "reg"
sum(sales[,3]=="Regular")

sum(sales[,3]=="LF")
sum(sales[,3]=="reg")

levels(sales$Item_Fat_Content)
sales$Item_Fat_Content<-as.factor(sales$Item_Fat_Content)
sales$Item_Fat_Content<-droplevels(sales$Item_Fat_Content,"low fat")
levels(sales$Item_Fat_Content)
table(sales$Item_Fat_Content)

sales$Item_Weight<-as.numeric(as.character(sales$Item_Weight))
sales$Item_Visibility<-as.numeric(as.character(sales$Item_Visibility))
sales$Item_MRP<-as.numeric(as.character(sales$Item_MRP))


salesData<-sales
salesData<-salesData[,-c(1,3,5)]
salesData<-salesData[,-c(4:9)]



Item_Fat_Content_LF<- rep(0, length(sales$Item_Fat_Content))
Item_Fat_Content_LF[which(sales[,3]=="LF")]<- 1
salesData$Item_Fat_Content_LF<-Item_Fat_Content_LF


unique(sales$Outlet_Size)
Outlet_Size_L<- rep(0, length(sales$Outlet_Size))
Outlet_Size_M<- rep(0, length(sales$Outlet_Size))
Outlet_Size_L[which(sales[,9]=="Large")]<- 1
Outlet_Size_M[which(sales[,9]=="Medium")]<- 1
salesData$Outlet_Size_M<-Outlet_Size_M
salesData$Outlet_Size_L<-Outlet_Size_L




unique(sales$Outlet_Location_Type)
Outlet_Location_Type_T1<- rep(0, length(sales$Outlet_Location_Type))
Outlet_Location_Type_T2<- rep(0, length(sales$Outlet_Location_Type))
Outlet_Location_Type_T1[which(sales[,10]=="Tier 1")]<- 1
Outlet_Location_Type_T2[which(sales[,10]=="Tier 2")]<- 1
salesData$Outlet_Location_Type_T1<-Outlet_Location_Type_T1
salesData$Outlet_Location_Type_T2<-Outlet_Location_Type_T2


unique(sales$Outlet_Type)
levels(sales$Outlet_Type)
Outlet_Type_SM1<- rep(0, length(sales$Outlet_Type))
Outlet_Type_SM2<- rep(0, length(sales$Outlet_Type))
Outlet_Type_SM3<- rep(0, length(sales$Outlet_Type))
Outlet_Type_SM1[which(sales[,11]=="Supermarket Type1")]<- 1
Outlet_Type_SM2[which(sales[,11]=="Supermarket Type2")]<- 1
Outlet_Type_SM3[which(sales[,11]=="Supermarket Type3")]<- 1
salesData$Outlet_Outlet_Type_SM1<-Outlet_Type_SM1
salesData$Outlet_Outlet_Type_SM2<-Outlet_Type_SM2
salesData$Outlet_Outlet_Type_SM3<-Outlet_Type_SM3


unique(sales$Outlet_Identifier)
levels(sales$Outlet_Identifier)
Outlet_Identifier_13<- rep(0, length(sales$Outlet_Identifier))
Outlet_Identifier_17<- rep(0, length(sales$Outlet_Identifier))
Outlet_Identifier_18<- rep(0, length(sales$Outlet_Identifier))
Outlet_Identifier_19<- rep(0, length(sales$Outlet_Identifier))
Outlet_Identifier_27<- rep(0, length(sales$Outlet_Identifier))
Outlet_Identifier_35<- rep(0, length(sales$Outlet_Identifier))
Outlet_Identifier_45<- rep(0, length(sales$Outlet_Identifier))
Outlet_Identifier_46<- rep(0, length(sales$Outlet_Identifier))
Outlet_Identifier_49<- rep(0, length(sales$Outlet_Identifier))
Outlet_Identifier_13[which(sales[,7]=="OUT013")]<- 1
Outlet_Identifier_17[which(sales[,7]=="OUT017")]<- 1
Outlet_Identifier_18[which(sales[,7]=="OUT018")]<- 1
Outlet_Identifier_19[which(sales[,7]=="OUT019")]<- 1
Outlet_Identifier_27[which(sales[,7]=="OUT027")]<- 1
Outlet_Identifier_35[which(sales[,7]=="OUT035")]<- 1
Outlet_Identifier_45[which(sales[,7]=="OUT045")]<- 1
Outlet_Identifier_46[which(sales[,7]=="OUT046")]<- 1
Outlet_Identifier_49[which(sales[,7]=="OUT049")]<- 1
salesData$Outlet_Identifier_13<-Outlet_Identifier_13
salesData$Outlet_Identifier_17<-Outlet_Identifier_17
salesData$Outlet_Identifier_18<-Outlet_Identifier_18
salesData$Outlet_Identifier_19<-Outlet_Identifier_19
salesData$Outlet_Identifier_27<-Outlet_Identifier_27
salesData$Outlet_Identifier_35<-Outlet_Identifier_35
salesData$Outlet_Identifier_45<-Outlet_Identifier_45
salesData$Outlet_Identifier_46<-Outlet_Identifier_46
salesData$Outlet_Identifier_49<-Outlet_Identifier_49
salesData$sales<-as.numeric(as.character(sales$Item_Outlet_Sales))
  
str(salesData)


fitall <- lm(sales ~ ., salesData)
summary(fitall)



fit1 <- lm(sales ~Item_MRP, salesData)
summary(fit1)

fit2 <- lm(sales ~Item_MRP+ Outlet_Outlet_Type_SM1, salesData)
summary(fit2)

fit3 <- lm(sales ~Item_MRP+ Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2, salesData)
summary(fit3)

fit4 <- lm(sales ~Item_MRP+ Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2+Outlet_Outlet_Type_SM3, salesData)
summary(fit4)

fit5 <- lm(sales ~Item_MRP+Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2+Outlet_Outlet_Type_SM3+Outlet_Identifier_17, salesData)
summary(fit5)

fit6 <- lm(sales ~Item_MRP+ Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2+Outlet_Outlet_Type_SM3+Outlet_Identifier_17+Outlet_Identifier_35, salesData)
summary(fit6)



fit7 <- lm(sales ~Item_MRP+ Item_Visibility +Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2+Outlet_Outlet_Type_SM3, salesData)
summary(fit7)

fit8 <- lm(sales ~Item_MRP+ Item_Fat_Content_LF +Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2+Outlet_Outlet_Type_SM3, salesData)
summary(fit8)

fit9 <- lm(sales ~Item_MRP+ Outlet_Size_L +Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2+Outlet_Outlet_Type_SM3, salesData)
summary(fit9)

fit10 <- lm(sales ~Item_MRP+ Outlet_Size_M +Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2+Outlet_Outlet_Type_SM3, salesData)
summary(fit10)

fit11 <- lm(sales ~Item_MRP+ Outlet_Location_Type_T1+Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2+Outlet_Outlet_Type_SM3, salesData)
summary(fit11)

fit12 <- lm(sales ~Item_MRP+ Outlet_Location_Type_T1 +Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2+Outlet_Outlet_Type_SM3, salesData)
summary(fit12)


fit13 <- lm(sales ~Item_MRP+ Item_Weight +Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2+Outlet_Outlet_Type_SM3, salesData)
summary(fit13)

fitfin <- lm(sales ~Item_MRP+ Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2+Outlet_Outlet_Type_SM3+Outlet_Identifier_17+Outlet_Identifier_35, salesData)
summary(fitfin)

stargazer( fit1, fit2, fit3, fit4,fit5, fit6, fit7, fit8,fit9, fit10, fit11, fit12,fit13, se = list(NULL, NULL, NULL, NULL, NULL, NULL,NULL, NULL, NULL, NULL, NULL, NULL),
          title = "Sales Dataset", type = "text",
          star.cutoffs = c(0.05, 0.01, 0.001), df = FALSE, digits = 3, omit.stat = c("adj.rsq"))

#library(AER)
#lht(fit1, c('hotelResort Hotel = 0'))



ggplot(fitfin) + 
  geom_point(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, color = "#C12795", size = 1)+ 
  ggtitle("Residuals Plot with OLS Regression Model") +
  xlab("Fitted Values") + ylab("Residuals")
par(mfrow=c(2,2))
plot(fitfin)
vif(fitfin)
bptest(fitfin)
# Hetroskedastic

salesData$resi <- fitfin$residuals
varfunc.ols <- lm(log(resi^2) ~ log(Item_MRP+ Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2+Outlet_Outlet_Type_SM3+Outlet_Identifier_17+Outlet_Identifier_35), data = salesData)
salesData$varfunc <- exp(varfunc.ols$fitted.values)
salesData.gls <- lm(log(sales) ~Item_MRP+ Outlet_Outlet_Type_SM1 + Outlet_Outlet_Type_SM2, data =  salesData)
bptest(salesData.gls )


ggplot(salesData.gls) + 
  geom_point(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, color = "#C12795", size = 1)+ 
  ggtitle("Residuals Plot with OLS Regression Model") +
  xlab("Fitted Values") + ylab("Residuals")



