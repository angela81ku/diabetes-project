####project2
pj2<- read.csv("jhst_proj2final.csv" ,header = T, na.strings = c("","Unknown", "NA"))
### table 1a
nondiabetic<- pj2[pj2$DiabetesV3 =="No",]
nondiabetic <- nondiabetic[!is.na(nondiabetic$FPGV3),]
nondiabetic <- nondiabetic[!is.na(nondiabetic$PrivatepublicInsV3),]
table(nondiabetic$PrivatepublicInsV3) 

nondiabetic$PrivatepublicInsV3.f <-factor(nondiabetic$PrivatepublicInsV3,
                                          levels=c("Private & Public", "Private Only", "Public Only", "Uninsured"))
# model1
res1<-lm(FPGV3~PrivatepublicInsV3.f,data=nondiabetic)
summary(res1)
# model2
res2<-lm(FPGV3~PrivatepublicInsV3.f+ageV3+sex,data=nondiabetic)
summary(res2)


### table 1b
oneb <- pj2[!is.na(pj2$DiabetesV3),]
table(oneb$DiabetesV3)
oneb <- oneb[!is.na(oneb$ageV3),]
oneb <- oneb[!is.na(oneb$sex),]
oneb <- oneb[!is.na(oneb$PrivatepublicInsV3),]
table(oneb$DiabetesV3)
#table(pj2$DiabetesV3, useNA = "ifany")#
table(oneb$PrivatepublicInsV3,useNA = "ifany") #

oneb$PrivatepublicInsV3.f <-factor(oneb$PrivatepublicInsV3,
                                          levels=c("Private & Public", "Private Only", "Public Only", "Uninsured"))
is.factor(oneb$PrivatepublicInsV3.f)


# model1
logres1<-glm(DiabetesV3~PrivatepublicInsV3.f, binomial, data=oneb)

# beta & associated standard erroe
summary(logres1)
summary(logres1)$coefficients

# provides odds ratios and 95% c.i.
exp(cbind(OR=coef(logres1), confint(logres1)))

# model2
logres2<-glm(DiabetesV3~PrivatepublicInsV3.f+ageV3+sex, binomial, data=oneb)

# beta & associated standard error
summary(logres2)
summary(logres2)$coefficients

# provides odds ratios and 95% c.i.
exp(cbind(OR=coef(logres2), confint(logres2)))

# compare model1 to model2
#anova(logres1, logres2,test="Chisq")

### table2a non-diabetic individuals
# remove all the missing value 
table2a <- pj2[pj2$DiabetesV3 =="No",]
table2a <- table2a[!is.na(table2a$ageV3),]
table2a <- table2a[!is.na(table2a$sex),]
table2a <- table2a[!is.na(table2a$BMIV3),]
table2a <- table2a[!is.na(table2a$sbpV3),]
table2a <- table2a[!is.na(table2a$dbpV3),]
table2a <- table2a[!is.na(table2a$totcholV3),]
table2a <- table2a[!is.na(table2a$idealhealthSMKv3),]
table2a <- table2a[!is.na(table2a$idealhealthPAV3),]
table2a <- table2a[!is.na(table2a$idealHealthNutritionv3),]
table2a <- table2a[!is.na(table2a$Simple6),]
# check number
table(table2a$DiabetesV3)
# ageV3+sex
resageV3sex<-lm(FPGV3~ageV3+sex,data=table2a)
summary(resageV3sex)

# bmiv3
resbmi<-lm(FPGV3~BMIV3+ageV3+sex,data=table2a)
summary(resbmi)
# sbpV3
ressbpV3<-lm(FPGV3~sbpV3+ageV3+sex,data=table2a)
summary(ressbpV3)

# dbpV3
resdbpV3<-lm(FPGV3~dbpV3+ageV3+sex,data=table2a)
summary(resdbpV3)

# totcholV3
restotcholV3<-lm(FPGV3~totcholV3+ageV3+sex,data=table2a)
summary(restotcholV3)
# idealhealthSMKv3
residealhealthSMKv3<-lm(FPGV3~idealhealthSMKv3+ageV3+sex,data=table2a)
summary(residealhealthSMKv3)
# idealhealthPAV3
residealhealthPAV3<-lm(FPGV3~idealhealthPAV3+ageV3+sex,data=table2a)
summary(residealhealthPAV3)
# idealHealthNutritionv3
residealHealthNutritionv3<-lm(FPGV3~idealHealthNutritionv3+ageV3+sex,data=table2a)
summary(residealHealthNutritionv3)

# Simple6
resSimple6<-lm(FPGV3~Simple6+ageV3+sex,data=table2a)
summary(resSimple6)

# model2 for table2a
res_multi<-lm(FPGV3~ageV3+sex+BMIV3+sbpV3+dbpV3+totcholV3+idealhealthSMKv3+idealhealthPAV3+idealHealthNutritionv3,data=table2a)
summary(res_multi)



### table2b with diabetes status

# remove all the missing value 
twob <- pj2[!is.na(pj2$DiabetesV3),]
table(twob$DiabetesV3)
dim(twob)
twob <- twob[!is.na(twob$ageV3),]
twob <- twob[!is.na(twob$sex),]

twob <- twob[!is.na(twob$BMIV3),]
twob <- twob[!is.na(twob$sbpV3),]
twob <- twob[!is.na(twob$dbpV3),]
twob <- twob[!is.na(twob$totcholV3),]
twob <- twob[!is.na(twob$idealhealthSMKv3),]
twob <- twob[!is.na(twob$idealhealthPAV3),]
twob <- twob[!is.na(twob$idealHealthNutritionv3),]
twob <- twob[!is.na(twob$Simple6),]
table(twob$DiabetesV3)
# model1

## ageV3+sex
logagesex<-glm(DiabetesV3~ageV3+sex, binomial, data=twob)
# beta & associated standard erroe
summary(logagesex)$coefficients
# provides odds ratios and 95% c.i.
exp(cbind(OR=coef(logagesex), confint(logagesex)))
## BMIV3
logbmi<-glm(DiabetesV3~BMIV3+ageV3+sex, binomial, data=twob)
summary(logbmi)$coefficients
exp(cbind(OR=coef(logbmi), confint(logbmi)))
# sbpV3
logsbp<-glm(DiabetesV3~sbpV3+ageV3+sex, binomial, data=twob)
summary(logsbp)$coefficients
exp(cbind(OR=coef(logsbp), confint(logsbp)))

# dbpV3
logdbpV3<-glm(DiabetesV3~dbpV3+ageV3+sex,binomial, data=twob)
summary(logdbpV3)$coefficients
exp(cbind(OR=coef(logdbpV3), confint(logdbpV3)))

# totcholV3
logtotcholV3<-glm(DiabetesV3~totcholV3+ageV3+sex,binomial,data=twob)
summary(logtotcholV3)$coefficients
exp(cbind(OR=coef(logtotcholV3), confint(logtotcholV3)))
# idealhealthSMKv3
logidealhealthSMKv3<-glm(DiabetesV3~idealhealthSMKv3+ageV3+sex,binomial,data=twob)
summary(logidealhealthSMKv3)$coefficients
exp(cbind(OR=coef(logidealhealthSMKv3), confint(logidealhealthSMKv3)))

# idealhealthPAV3
logidealhealthPAV3<-glm(DiabetesV3~idealhealthPAV3+ageV3+sex,binomial,data=twob)
summary(logidealhealthPAV3)$coefficients
exp(cbind(OR=coef(logidealhealthPAV3), confint(logidealhealthPAV3)))
# idealHealthNutritionv3
logidealHealthNutritionv3<-glm(DiabetesV3~idealHealthNutritionv3+ageV3+sex,binomial,data=twob)
summary(logidealHealthNutritionv3)$coefficients
exp(cbind(OR=coef(logidealHealthNutritionv3), confint(logidealHealthNutritionv3)))
# Simple6
logSimple6<-glm(DiabetesV3~Simple6+ageV3+sex,binomial, data=twob)
summary(logSimple6)$coefficients
exp(cbind(OR=coef(logSimple6), confint(logSimple6)))

#model2 for table2b
logmulti_2b<-glm(DiabetesV3~BMIV3+sbpV3+dbpV3+totcholV3+idealhealthSMKv3+idealhealthPAV3+idealHealthNutritionv3+ageV3+sex, binomial, data=twob)

# beta & associated standard error
summary(logmulti_2b)$coefficients
exp(cbind(OR=coef(logmulti_2b), confint(logmulti_2b)))


###Table 3

t3_model1<-lm(FPGV3~ageV3+sex+BMIV3+totcholV3+totcholV3:sex,data=table2a)
summary(t3_model1)

t3_model2<-lm(FPGV3~ageV3+sex+BMIV3+totcholV3+BMIV3:sex,data=table2a)
summary(t3_model2)
