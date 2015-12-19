setwd("~/Desktop/F/data science/GS_hack")

train=read.csv("Initial_Training_Data.csv")

test=read.csv("Initial_Test_Data.csv")
str(train)


#which(is.na(train$Days_to_Settle))
#train$Days_to_Settle[7179]=3

#which(train$Industry_Group=="Industry Group58")
#train=rbind(train[1:817,],train[819:nrow(train),])
#which(train$Industry_Group=="Industry Group77")
#train=rbind(train[1:7863,],train[7865:nrow(train),])
#which(train$Coupon_Frequency=="Coupon Frequency5")
#train=rbind(train[1:6855,],train[6857:nrow(train),])

which(train$Coupon_Frequency=="Coupon Frequency5")
train=rbind(train[1:6856,],train[6858:8500,])
which(train$Currency=="Currency1")
train=rbind(train[1:4699,],train[4701:8499,])

test$Risk_Stripe="Stripe 0"
dat=rbind(train,test)

table(is.na(dat))
summary(dat)

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)




#levels(test$Industry_Group)=levels(train$Industry_Group)
#levels(test$Industry_Sector)=levels(train$Industry_Sector)
#levels(test$Coupon_Type)=levels(train$Coupon_Type)
#levels(test$Coupon_Frequency)=levels(train$Coupon_Frequency)
#levels(test$Seniority)=levels(train$Seniority)
#levels(test$SP_Rating)=levels(train$SP_Rating)
#levels(test$Moody_Rating)=levels(train$Moody_Rating)

##########################features########################
f=rpart(as.integer(Risk_Stripe)~SP_Rating,data=train)
fancyRpartPlot(f)

dat$SP_Ratingn=0
dat$SP_Ratingn[dat$SP_Rating=="sp rating7"]=1
dat$SP_Ratingn[dat$SP_Rating=="sp rating11" || dat$SP_Rating=="sp rating12" || dat$SP_Rating=="sp rating13" || dat$SP_Rating=="sp rating14" || dat$SP_Rating=="sp rating15" ||dat$SP_Rating=="sp rating16" || dat$SP_Rating=="sp rating18" ||dat$SP_Rating=="sp rating21" ||dat$SP_Rating=="sp rating4"]=2
dat$SP_Ratingn[dat$SP_Rating=="sp rating0" || dat$SP_Rating=="sp rating17" || dat$SP_Rating=="sp rating19" || dat$SP_Rating=="sp rating3" ||  dat$SP_Rating=="sp rating5" || dat$SP_Rating=="sp rating8"]=3


#f=rpart(as.integer(Risk_Stripe)~Seniority,data=train)
#fancyRpartPlot(f)

#dat$Seniorityn=0
#dat$Seniorityn[dat$Seniority=="Seniority0" || dat$Seniority=="Seniority10" || dat$Seniority=="Seniority5" || dat$Seniority=="Seniority8" || dat$Seniority=="Seniority9"]=1

date=strsplit(as.character(dat$Issue_Date), split='[-]')
for( j in 1:13441)
{
  dat$Issue_Month[j]=date[[j]][2]
  dat$Issue_Year[j]=date[[j]][3]
}
dat$Issue_Month[which(is.na(dat$Issue_Month))]="Jul"
dat$Issue_Year[which(is.na(dat$Issue_Year))]="15"
dat$Issue_Month=as.factor(dat$Issue_Month)
dat$Issue_Year=as.factor(dat$Issue_Year)


date1=strsplit(as.character(dat$Maturity_Date), split='[-]')
for( j in 1:13441)
{
  dat$Maturity_Month[j]=date1[[j]][2]
  dat$Maturity_Year[j]=date1[[j]][3]
}
dat$Maturity_Month[which(is.na(dat$Maturity_Month))]="Jul"
dat$Maturity_Year[which(is.na(dat$Maturity_Year))]="15"
dat$Maturity_Year=as.factor(dat$Maturity_Year)
dat$Maturity_Month=as.factor(dat$Maturity_Month)

dat$yeardiff=as.integer(dat$Maturity_Year)-as.integer(dat$Issue_Year)




dat$Moody_Rating[dat$Moody_Rating=="moody rating21"]="moody rating10"
dat$Moody_Rating[dat$Moody_Rating=="moody rating24"]="moody rating10"
dat$Moody_Rating[dat$Moody_Rating=="moody rating26"]="moody rating10"
dat$Moody_Rating[dat$Moody_Rating=="moody rating27"]="moody rating10"
dat$Moody_Rating[dat$Moody_Rating=="moody rating23"]="moody rating10"
dat$Moody_Rating[dat$Moody_Rating=="moody rating29"]="moody rating10"
dat$Moody_Rating[dat$Moody_Rating=="moody rating30"]="moody rating10"

dat$Maturity_Year=as.integer(dat$Maturity_Year)
dat$Issue_Year=as.integer(dat$Issue_Year)

dat$diffyear=dat$Maturity_Year-dat$Issue_Year

# check for all variables by making them continuous 
# and binning and check with factors too as compared to continuos as it is improving
# the prediction and do whatever else you know

dat$Country_Of_Domicile_int=as.integer(dat$Country_Of_Domicile)
dat$Moody_Rating_int=as.integer(dat$Moody_Rating)
dat$Industry_Group_int=as.integer(dat$Industry_Group)

train=dat[1:8498,]
test=dat[8499:nrow(dat),]

###########benchmark solution############

library(randomForest)
library(e1071)
#fit=randomForest(Risk_Stripe~-SP_Rating+as.integer(SP_Rating)-Moody_Rating+as.integer(Moody_Rating)-Currency+as.integer(Currency)-Is_Emerging_Market+as.integer(Is_Emerging_Market)-Seniority+as.integer(Seniority)-Callable+as.integer(Callable)-Days_to_Settle+as.integer(Days_to_Settle)-Issue_Date+as.integer(Issue_Date)-Collateral_Type+as.integer(Collateral_Type)-Coupon_Frequency+as.integer(Coupon_Frequency)-Coupon_Type+as.integer(Coupon_Type)-Maturity_Date+as.integer(Maturity_Date)-Industry_Group+as.integer(Industry_Group)-Industry_Sector+as.integer(Industry_Sector)-Industry_SubGroup+as.integer(Industry_SubGroup)-Issuer_Name+as.integer(Issuer_Name)-Ticker+as.integer(Ticker)-Country_Of_Domicile+as.integer(Country_Of_Domicile), data=train,ntree=100, importance=TRUE)


#fit=randomForest(Risk_Stripe~.-SP_Rating+as.integer(SP_Rating)-Issuer_Name+as.integer(Issuer_Name)-Ticker+as.integer(Ticker)-Industry_SubGroup+as.integer(Industry_SubGroup)-Industry_Group+as.integer(Industry_Group)-Maturity_Date+as.integer(Maturity_Date)-Issue_Date+as.integer(Issue_Date)-ISIN,data=t,ntree=100, importance=TRUE)


#library(rpart)
#fit=rpart(Risk_Stripe~SP_Rating+Moody_Rating+Currency+Is_Emerging_Market+Seniority+Callable+Collateral_Type+Coupon_Frequency+Coupon_Type+Industry_Group+Industry_Sector+Industry_SubGroup,data=train)
set.seed(432)
fit=randomForest(Risk_Stripe~+Industry_Sector+Coupon_Type+ Collateral_Type+SP_Ratingn+SP_Rating+Country_Of_Domicile_int+Moody_Rating_int+Industry_Group_int, data=train, ntree=400, importance=TRUE, mtry=4, replace =FALSE)

#fit=randomForest(Risk_Stripe~SP_Ratingn+Country_Of_Domicile+Moody_Rating+Seniority+as.integer(Industry_Group), data=train, ntree=400, importance=TRUE)


pred=predict(fit, test)
test$Risk_Stripe=pred
s<-data.frame(ISIN=test$ISIN,Risk_Stripe=test$Risk_Stripe)
write.csv(s,file="submit10.csv",row.names=FALSE)


#################################################decision tree solution##########
library(rpart)
d=rpart(Risk_Stripe~as.integer(Seniority)+as.integer(Industry_Group),data=train)
pred=predict(d,test)
a=c()
for(i in 1:4943)
{
  a[i]=names(which(pred[i,]==max(pred[i,])))
}

s<-data.frame(ISIN=test$ISIN,Risk_Stripe=a)
write.csv(s,file="submit2.csv",row.names=FALSE)

#########################    SVM  ####################################
#fit=svm(Risk_Stripe~SP_Ratingn+ Moody_Rating + Seniority + Callable, data=train)

fit=svm(Risk_Stripe~Days_to_Settle+SP_Ratingn-Country_Of_Domicile+as.integer(Country_Of_Domicile)-Moody_Rating+as.integer(Moody_Rating)-Seniority+as.integer(Seniority)-Industry_Group+as.integer(Industry_Group), data=train)
pred=predict(fit, test)
test$Risk_Stripe=pred
s<-data.frame(ISIN=test$ISIN,Risk_Stripe=test$Risk_Stripe)
write.csv(s,file="submit7.csv",row.names=FALSE)
##################################################
library(adabag)
ada<-boosting(Risk_Stripe~Industry_Sector+Coupon_Type+ Collateral_Type+SP_Ratingn+SP_Rating+Country_Of_Domicile_int+Moody_Rating_int+Industry_Group_int, data=train, boos=TRUE, mfinal=3,coeflearn='Breiman')
pred=predict.boosting(ada,test)
test$Risk_Stripe=pred[[4]]
s<-data.frame(ISIN=test$ISIN,Risk_Stripe=test$Risk_Stripe)
write.csv(s,file="submit10.csv",row.names=FALSE)

#################################################

fit=kmeans(train,3)
