#reading in the train and test data sets
setwd("~/Desktop/F/data science/GS_hack")
train=read.csv("Final_Training_Data.csv")
test=read.csv("Final_Test_Data.csv")

str(train)

#combining train and test
test$Risk_Stripe="Stripe 0"
dat=rbind(train,test)

table(is.na(dat))
summary(dat)

#libraries used for making good decision tree plots
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


#creating feature bins on the basis of decision tree results

############
f=rpart(as.integer(Risk_Stripe)~SP_rating,data=train)
fancyRpartPlot(f)

dat$SP_Ratingn=0
dat$SP_Ratingn[dat$SP_rating=="sp_rating11" | dat$SP_rating=="sp_rating13" | dat$SP_rating=="sp_rating14" | dat$SP_rating=="sp_rating17"]=1
dat$SP_Ratingn[dat$SP_rating=="sp_rating1" | dat$SP_rating=="sp_rating2" | dat$SP_rating=="sp_rating5" | dat$SP_rating=="sp_rating7"]=2

#################
f=rpart(as.integer(Risk_Stripe)~Seniority,data=train)
fancyRpartPlot(f)
dat$Seniorityn=0
dat$Seniorityn[dat$Seniority=="Seniority1" || dat$Seniority=="Seniority2" || dat$Seniority=="Seniority0"]=1
###############

f=rpart(as.integer(Risk_Stripe)~Coupon_Type,data=train)
fancyRpartPlot(f)

dat$Coupon_Type_bin=0
dat$Coupon_Type_bin[dat$Coupon_Type=="Coupon Type1" | dat$Coupon_Type=="Coupon Type2" | dat$Coupon_Type=="Coupon Type3"]=1
#############

f=rpart(as.integer(Risk_Stripe)~Collateral_Type,data=train)
fancyRpartPlot(f)

dat$Collateral_Type_bin=0
dat$Collateral_Type_bin[dat$Collateral_Type=="Collateral Type3" ]=1
dat$Collateral_Type_bin[dat$Collateral_Type=="Collateral Type1" | dat$Collateral_Type=="Collateral Type5" | dat$Collateral_Type=="Collateral Type6" ]=2
#############


#Extracting month and year from date
date=strsplit(as.character(dat$Issue_Date), split='[-]')
for( j in 1:1268)
{
  dat$Issue_Month[j]=date[[j]][2]
  dat$Issue_Year[j]=date[[j]][3]
}


dat$Issue_Month=as.factor(dat$Issue_Month)
dat$Issue_Year=as.factor(dat$Issue_Year)

#converting month into bins using the boxplot
boxplot(as.integer(Risk_Stripe)~Issue_Month,data=train)
dat$Issue_Month_bin=0
dat$Issue_Month_bin[dat$Issue_Month=="Apr" | dat$Issue_Month=="Mar" | dat$Issue_Month=="Sep" | dat$Issue_Month=="May" | dat$Issue_Month=="Jun" | dat$Issue_Month=="Jan"]=1
dat$Issue_Month_bin[dat$Issue_Month=="Aug"]=2
dat$Issue_Month_bin[dat$Issue_Month=="Dec" | dat$Issue_Month=="Feb" | dat$Issue_Month=="Jul" | dat$Issue_Month=="Nov" | dat$Issue_Month=="Oct"]=3
#############

########doing the same as above for matureity date
date1=strsplit(as.character(dat$Maturity_Date), split='[-]')
for( j in 1:1268)
{
  dat$Maturity_Month[j]=date1[[j]][2]
  dat$Maturity_Year[j]=date1[[j]][3]
}

dat$Maturity_Year=as.factor(dat$Maturity_Year)
dat$Maturity_Month=as.factor(dat$Maturity_Month)

#removing some outliers########
dat$Moody_rating[dat$Moody_rating=="moody_rating28"]="moody_rating0"
dat$Moody_rating[dat$Moody_rating=="moody_rating23"]="moody_rating0"
dat$Moody_rating[dat$Moody_rating=="moody_rating25"]="moody_rating0"
dat$Moody_rating[dat$Moody_rating=="moody_rating26"]="moody_rating0"
dat$Moody_rating[dat$Moody_rating=="moody_rating6"]="moody_rating0"
dat$Moody_rating[dat$Moody_rating=="moody_rating19"]="moody_rating0"

dat$Moody_rating=as.factor(dat$Moody_rating)

dat$Maturity_Year=as.integer(dat$Maturity_Year)
dat$Issue_Year=as.integer(dat$Issue_Year)

#creating a new variable year difference between issue and maturity
dat$yeardiff=as.integer(dat$Maturity_Year)-as.integer(dat$Issue_Year)


dat$Country_Of_Domicile_int=as.integer(dat$Country_Of_Domicile)
dat$Moody_Rating_int=as.integer(dat$Moody_rating)
dat$Industry_Group_int=as.integer(dat$Industry_Group)
dat$Issue_Date=as.integer(dat$Issue_Date)
dat$Maturity_Date_int=as.integer(dat$Maturity_Date)
dat$Ticker_int=as.integer(dat$Ticker)
dat$Issuer_Name=as.integer(dat$Issuer_Name)


train=dat[1:906,]
test=dat[907:nrow(dat),]

###########benchmark solution############

library(randomForest)

##final randomForest model
set.seed(42)
fit=randomForest(Risk_Stripe~Ticker_int+Country_Of_Domicile+yeardiff+Currency+Industry_Sector+Coupon_Type+ Collateral_Type_bin+SP_rating+Moody_rating+Industry_Group_int, data=train, ntree=1000, mtry=5, replace =FALSE)


#writing output to csv file
pred=predict(fit, test)
test$Risk_Stripe=pred
s<-data.frame(ISIN=test$ISIN,Risk_Stripe=test$Risk_Stripe)
write.csv(s,file="output.csv",row.names=FALSE)

