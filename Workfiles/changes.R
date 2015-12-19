dat$Moody_Rating[dat$Moody_Rating=="moody rating21"]="moody rating10"
dat$Moody_Rating[dat$Moody_Rating=="moody rating24"]="moody rating10"
dat$Moody_Rating[dat$Moody_Rating=="moody rating26"]="moody rating10"
dat$Moody_Rating[dat$Moody_Rating=="moody rating27"]="moody rating10"
dat$Moody_Rating[dat$Moody_Rating=="moody rating23"]="moody rating10"
dat$Moody_Rating[dat$Moody_Rating=="moody rating29"]="moody rating10"
dat$Moody_Rating[dat$Moody_Rating=="moody rating30"]="moody rating10"
####################################
dat$Collateral_Type[dat$Collateral_Type=="Collateral Type15"]="Collateral Type4"
dat$Collateral_Type[dat$Collateral_Type=="Collateral Type26"]="Collateral Type4"
dat$Collateral_Type[dat$Collateral_Type=="Collateral Type22"]="Collateral Type4"
dat$Collateral_Type[dat$Collateral_Type=="Collateral Type7"]="Collateral Type4"
dat$Collateral_Type[dat$Collateral_Type=="Collateral Type11"]="Collateral Type4"
dat$Collateral_Type[dat$Collateral_Type=="Collateral Type23"]="Collateral Type4"
#####################################

dat$Collateral_int=as.integer(dat$Collateral_Type)
dat$collateral_bin=0
dat$collateral_bin[dat$Collateral_int<9.5]=1
dat$collateral_bin[dat$Collateral_int>9.5 & dat$Collateral_int<10]=2
dat$collateral_bin[dat$Collateral_int>=10]=3
dat$collateral_bin=as.factor(dat$collateral_bin)

library(rpart)
d=rpart(as.integer(Risk_Stripe)~Collateral_int, data=train)
fancyRpartPlot(d)
dat$Collateral_Type[dat$Collateral_Type=="Collateral Type15" | dat$Collateral_Type=="Collateral Type26" | dat$Collateral_Type=="Collateral Type22" | dat$Collateral_Type=="Collateral Type7" |dat$Collateral_Type=="Collateral Type11" |  dat$Collateral_Type=="Collateral Type23"]

###########################
d=rpart(as.integer(Risk_Stripe)~Industry_Sector,data=train)
fancyRpartPlot(d)

dat$Industry_Sector_bin=0
dat$Industry_Sector_bin[dat$Industry_Sector=="Industry Sector0" | dat$Industry_Sector=="Industry Sector10" | dat$Industry_Sector=="Industry Sector11" | dat$Industry_Sector=="Industry Sector6" ]=1
dat$Industry_Sector_bin[dat$Industry_Sector=="Industry Sector1" | dat$Industry_Sector=="Industry Sector4" | dat$Industry_Sector=="Industry Sector5" ]=2
dat$Industry_Sector_bin[dat$Industry_Sector=="Industry Sector2" | dat$Industry_Sector=="Industry Sector3" | dat$Industry_Sector=="Industry Sector7"]=3




#dat$Coupon_Type_int=0
#dat$Coupon_Type_int[as.integer(dat$Coupon_Type)<1.5]=1
#dat$Coupon_Type_int[as.integer(dat$Coupon_Type)>=1.5 & as.integer(dat$Coupon_Type)<2.5]=2
#dat$Coupon_Type_int[as.integer(dat$Coupon_Type)>=2.5]=3
#dat$Coupon_Type_int=as.factor(dat$Coupon_Type_int)







####################################
#prop.table(table(train$Seniority,train$Risk_Stripe),2)
#dat$Seniorityn=0
#dat$Seniorityn[dat$Seniority=="Seniority2"]=1
#dat$Seniorityn[(dat$Seniority=="Seniority3") | (dat$Seniority=="Seniority4")]=2
#dat$Seniorityn[(dat$Seniority=="Seniority0" | dat$Seniority=="Seniority1" | dat$Seniority=="Seniority6" | dat$Seniority=="Seniority7")]=3
#dat$Seniorityn[dat$Seniority=="Seniority8" | dat$Seniority=="Seniority9"]=4

dat$Tickerint=as.integer(dat$Ticker)