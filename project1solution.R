JHS <- read.csv("jhst_proj1.csv", header = T, na.strings = c("","Unknown", "NA"))
#1
JHS$IdealHealthBMIV1[JHS$BMIV1 <25] <- 1
JHS$IdealHealthBMIV1[JHS$BMIV1 >=25] <- 0
#2
JHS$AGEgrp[JHS$AGEV1 > 20 & JHS$AGEV1 <=40 ] <- 1
JHS$AGEgrp[JHS$AGEV1 > 40 & JHS$AGEV1 <= 50] <- 2
JHS$AGEgrp[JHS$AGEV1 > 50 &JHS$AGEV1 <= 60]<- 3
JHS$AGEgrp[JHS$AGEV1 > 60 & JHS$AGEV1 <=70 ]  <- 4
JHS$AGEgrp[JHS$AGEV1 >70] <- 5
#3
JHS$IdealHealthBPV1[JHS$SBPV1 < 120 & JHS$DBPV1 < 80] <-1
JHS$IdealHealthBPV1[JHS$SBPV1 >= 120 |JHS$DBPV1 >= 80] <-0
#4
JHS$IdealHealthCholV1[JHS$TOTCHOLV1 < 240] <-1
JHS$IdealHealthCholV1[JHS$TOTCHOLV1 >= 240] <-0
#5
JHS$IdealHealthSMKV1[JHS$SMK3CATV1=="Ideal Health"] <- 1
JHS$IdealHealthSMKV1[JHS$SMK3CATV1=="Poor Health" | JHS$SMK3CATV1=="Intermediate Health"] <-0
#6
JHS$IdealHealthPAV1[JHS$PA3CATV1=="Ideal Health"] <- 1
JHS$IdealHealthPAV1[JHS$PA3CATV1=="Poor Health" | JHS$PA3CATV1=="Intermediate Health"] <-0
#7
JHS$IdealHealthNutritionV1[JHS$NUTRITION3CATV1=="Ideal Health"] <- 1
JHS$IdealHealthNutritionV1[JHS$NUTRITION3CATV1=="Poor Health" | JHS$NUTRITION3CATV1=="Intermediate Health"] <- 0
#8
JHS$Simple6 <- apply(JHS[,c(35,37:41)], 1, sum)
JHS$Simple6
#9
#apply(ds, MARGIN, FUN, na.rm=TRUE) to remove na value
JHS <- JHS[is.na(JHS$DIABETESV1)==FALSE,]
JHS <- JHS[is.na(JHS$IdealHealthBMIV1)==FALSE,]
JHS <- JHS[is.na(JHS$IdealHealthBPV1)==FALSE,]
JHS <- JHS[is.na(JHS$IdealHealthCholV1)==FALSE,]
JHS <- JHS[is.na(JHS$IdealHealthSMKV1)==FALSE,]
JHS <- JHS[is.na(JHS$IdealHealthPAV1)==FALSE,]
JHS <- JHS[is.na(JHS$IdealHealthNutritionV1)==FALSE,]

###table value of age
mean(JHS$AGEV1)#54.37
sd(JHS$AGEV1)#11.55
mean(JHS$AGEV1[JHS$DIABETESV1=="No"])#53.36
sd(JHS$AGEV1[JHS$DIABETESV1=="No"])#11.67
mean(JHS$AGEV1[JHS$DIABETESV1=="Yes"])#59.30
sd(JHS$AGEV1[JHS$DIABETESV1=="Yes"])#9.54
t.test(JHS$AGEV1[JHS$DIABETESV1=="No"],JHS$AGEV1[JHS$DIABETESV1=="Yes"], var.equal=TRUE)

###table value of age catogories
sum(JHS$AGEgrp==1)
sum(JHS$AGEgrp==1&JHS$DIABETESV1=="No") #244
sum(JHS$AGEgrp==1&JHS$DIABETESV1=="Yes") #11
sum(JHS$AGEgrp==2)
sum(JHS$AGEgrp==2&JHS$DIABETESV1=="No") #499
sum(JHS$AGEgrp==2&JHS$DIABETESV1=="Yes") #50
sum(JHS$AGEgrp==3) #622
sum(JHS$AGEgrp==3&JHS$DIABETESV1=="No") #501
sum(JHS$AGEgrp==3&JHS$DIABETESV1=="Yes") #121
sum(JHS$AGEgrp==4) #587
sum(JHS$AGEgrp==4&JHS$DIABETESV1=="No") #438
sum(JHS$AGEgrp==4&JHS$DIABETESV1=="Yes") #149
sum(JHS$AGEgrp==5) #185
sum(JHS$AGEgrp==5&JHS$DIABETESV1=="No") #141
sum(JHS$AGEgrp==5&JHS$DIABETESV1=="Yes") #44

colnames= c("No","Yes")
rownames_age= c("20-40",">40-50",">50-60",">60-70",">70")
table_age <- matrix(c(244,499,501,438,141,11,50,121,149,44),nrow = 5,ncol = 2,dimnames=list(rownames_age, colnames))
chisq.test(table_age,title("Chi-squared test for age"))
oddsratio(table_age, method="wald")

###table value of  Male
sum(JHS$SexV1=="Male") #790
sum(JHS$SexV1=="Male"&JHS$DIABETESV1=="No") #666
sum(JHS$SexV1=="Male"&JHS$DIABETESV1=="Yes") #124
table(JHS$SexV1, JHS$DIABETESV1)
prop.table(table(JHS$SexV1, JHS$DIABETESV1) ,2)
chisq.test(table(JHS$SexV1, JHS$DIABETESV1))

###table for Body mass index 
mean(JHS$BMIV1)
sd(JHS$BMIV1)
mean(JHS$BMIV1[JHS$DIABETESV1=="No"])   
sd(JHS$BMIV1[JHS$DIABETESV1=="No"])  
mean(JHS$BMIV1[JHS$DIABETESV1=="Yes"])
sd(JHS$BMIV1[JHS$DIABETESV1=="Yes"])
#test if the variences  are different?
var.test(JHS$BMIV1[JHS$DIABETESV1=="No"],JHS$BMIV1[JHS$DIABETESV1=="Yes"], conf.level=0.95)
# variences are not different
t.test(JHS$BMIV1[JHS$DIABETESV1=="No"],JHS$BMIV1[JHS$DIABETESV1=="Yes"], var.equal=TRUE)
# reject H0, so true difference in means is not equal to 0.

### table for ideal health bmi
table(JHS$IdealHealthBMIV,JHS$IdealHealthBMIV)
prop.table(table(JHS$IdealHealthBMIV,JHS$IdealHealthBMIV))
idealHealthBMI_table <- table(JHS$IdealHealthBMIV1,JHS$DIABETESV1)
idealHealthBMI_table
chisq.test(idealHealthBMI_table,correct=FALSE)
###table for SBP
mean(JHS$SBPV1)
sd(JHS$SBPV1)
mean(JHS$SBPV1[JHS$DIABETESV1=="No"])
sd(JHS$SBPV1[JHS$DIABETESV1=="No"])
mean(JHS$SBPV1[JHS$DIABETESV1=="Yes"])
sd(JHS$SBPV1[JHS$DIABETESV1=="Yes"])
var.test(JHS$SBPV1[JHS$DIABETESV1=="No"],JHS$SBPV1[JHS$DIABETESV1=="Yes"])
#var equal
t.test(JHS$SBPV1[JHS$DIABETESV1=="No"],JHS$SBPV1[JHS$DIABETESV1=="Yes"], var.equal = T)
###table for DBPV1
mean(JHS$DBPV1)
sd(JHS$DBPV1)
mean(JHS$DBPV1[JHS$DIABETESV1=="No"])
sd(JHS$DBPV1[JHS$DIABETESV1=="No"])
mean(JHS$DBPV1[JHS$DIABETESV1=="Yes"])
sd(JHS$DBPV1[JHS$DIABETESV1=="Yes"])
var.test(JHS$DBPV1[JHS$DIABETESV1=="No"],JHS$DBPV1[JHS$DIABETESV1=="Yes"])
#var equal
t.test(JHS$DBPV1[JHS$DIABETESV1=="No"],JHS$DBPV1[JHS$DIABETESV1=="Yes"], var.equal = T)

###table for IdealHealthBPV1
table(ifelse(JHS$IdealHealthBPV==1,1,0))
prop.table(table(ifelse(JHS$IdealHealthBPV==1,1,0)))
IdealHealthBPV1_table <- table(JHS$IdealHealthBPV1, JHS$DIABETESV1)
prop.table(IdealHealthBPV1_table,2)
sum(JHS$IdealHealthBPV1==0)
sum(JHS$IdealHealthBPV1==1)
chisq.test(IdealHealthBPV1_table,correct=FALSE)

###table for TOTCHOLV1
mean(JHS$TOTCHOLV1)
sd(JHS$TOTCHOLV1)
mean(JHS$TOTCHOLV1[JHS$DIABETESV1=="No"])
sd(JHS$TOTCHOLV1[JHS$DIABETESV1=="No"])
mean(JHS$TOTCHOLV1[JHS$DIABETESV1=="Yes"])
sd(JHS$TOTCHOLV1[JHS$DIABETESV1=="Yes"])
var.test(JHS$TOTCHOLV1[JHS$DIABETESV1=="No"],JHS$TOTCHOLV1[JHS$DIABETESV1=="Yes"])
#variences are equal
t.test(JHS$TOTCHOLV1[JHS$DIABETESV1=="No"],JHS$TOTCHOLV1[JHS$DIABETESV1=="Yes"], var.equal = T)

###table for IdealHealthCholV1
table(ifelse(JHS$IdealHealthCholV1==1,1,0))
prop.table(table(ifelse(JHS$IdealHealthCholV1==1,1,0)))
IdealHealthCholV1_table <- table(JHS$IdealHealthCholV1,JHS$DIABETESV1)
prop.table(IdealHealthCholV1_table,2)
chisq.test(IdealHealthCholV1_table,correct=FALSE)

###IdealHealthNutritionV1
table(ifelse(JHS$IdealHealthNutritionV1==1,1,0))
prop.table(table(ifelse(JHS$IdealHealthNutritionV1==1,1,0)))
IdealHealthNutritionV_table <- table(JHS$IdealHealthNutritionV, JHS$DIABETESV1)
IdealHealthNutritionV_table
prop.table(IdealHealthNutritionV_table,2)
#chisq.test(IdealHealthNutritionV_table,correct=F) need to use fisher
fisher.test(IdealHealthNutritionV_table)

###table for IdealHealthSMKV1
table(ifelse(JHS$IdealHealthSMKV1==1,1,0))
prop.table(table(ifelse(JHS$IdealHealthSMKV1==1,1,0)))
IdealHealthSMKV1_table <- table(JHS$IdealHealthSMKV1, JHS$DIABETESV1)
IdealHealthSMKV1_table
prop.table(IdealHealthSMKV1_table,2)
chisq.test(IdealHealthSMKV1_table,correct=F)

###table for IdealHealthPAV1
table(ifelse(JHS$IdealHealthPAV1==1,1,0))
prop.table(table(ifelse(JHS$IdealHealthPAV1==1,1,0)))
IdealHealthPAV1_table <- table(JHS$IdealHealthPAV1, JHS$DIABETESV1)
IdealHealthPAV1_table
prop.table(IdealHealthPAV1_table,2)
chisq.test(IdealHealthPAV1_table,correct=F)


###table for Simple6
mean(JHS$Simple6)
sd(JHS$Simple6)
mean(JHS$Simple6[JHS$DIABETESV1=="No"])
sd(JHS$Simple6[JHS$DIABETESV1=="No"])
mean(JHS$Simple6[JHS$DIABETESV1=="Yes"])
sd(JHS$Simple6[JHS$DIABETESV1=="Yes"])
var.test(JHS$Simple6[JHS$DIABETESV1=="No"],JHS$Simple6[JHS$DIABETESV1=="Yes"])
#variences are equal
t.test(JHS$Simple6[JHS$DIABETESV1=="No"],JHS$Simple6[JHS$DIABETESV1=="Yes"], var.equal = T)

###table for INCOMEV1
table(JHS$INCOMEV1,JHS$INCOMEV1)
prop.table(table(JHS$INCOMEV1))
table(JHS$INCOMEV1,JHS$DIABETESV1)
prop.table(table(JHS$INCOMEV1,JHS$DIABETESV1),2)
chisq.test(table(JHS$INCOMEV1,JHS$DIABETESV1),correct=FALSE)

###1-C test for Type of Insurance and table for insurance
table(JHS$PRIVATEPUBLICINSV1,JHS$PRIVATEPUBLICINSV1)
prop.table(table(JHS$PRIVATEPUBLICINSV1))

Insurance_table <- table(JHS$PRIVATEPUBLICINSV1,JHS$DIABETESV1)
prop.table(table(JHS$PRIVATEPUBLICINSV1,JHS$DIABETESV1),2)
chisq.test(table(JHS$PRIVATEPUBLICINSV1,JHS$DIABETESV1),correct=FALSE)

tb2 <-rbind(Insurance_table[4,],Insurance_table[3,],Insurance_table[2,],Insurance_table[1,])
rownames(tb2)<-c("Uninsured","Public Only ","Private Only","Private & Public")
oddsratio(tb2, method="wald")

###1-D newdata with FPGV1&FPGV2&FPGV3 
newdata <- JHS[JHS$DIABETESV1=="No"&JHS$DIABETESV2=="No"&JHS$DIABETESV3=="No",]
newdata<- newdata[is.na(newdata$DIABETESV1)==FALSE,]
newdata <- newdata[is.na(newdata$DIABETESV2)==FALSE,]
newdata <- newdata[is.na(newdata$DIABETESV3)==FALSE,]
newdata <- newdata[is.na(newdata$FPGV1)==FALSE,]
newdata <- newdata[is.na(newdata$FPGV2)==FALSE,]
newdata <- newdata[is.na(newdata$FPGV3)==FALSE,]
##  mean difference between visit2&visit1
newdata$diff_vis1_vis2 <- newdata$FPGV1 - newdata$FPGV2
t.test(newdata$diff_vis1_vis2,mu=0)
##  mean difference between visit3&visit2
newdata$diff_vis2_vis3 <- newdata$FPGV2 - newdata$FPGV3
t.test(newdata$diff_vis2_vis3,mu=0)

write.csv(JHS,"newdat0329.csv")
write.csv(newdata,"newdat0329.csv")
