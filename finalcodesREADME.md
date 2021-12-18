# GROUPPROJECT-18DEC2021
---
  title: "FINAL PROJECT"
author: "Kouakou H. Allah, Cisse Serigne, Joel Andrade"
date: "12/18/2021"
output: html_document

---
  #title: "Correlation and Logit Models
  #authors: "Cisse, Serigne, Kouakou H. Allah and Joel Andrade.
  #output: html_document
  # INTRODUCTIONS: 
  #This working data set is called chs2019	and provides sample code to use when analyzing survey data. There are 8,803 observations and 152 variables in the dataset				*
  #The stratification (nesting) variable is strata and this survey data needs to be analyzed using a special procedure in SAS. We have imported the data set as SAS in R Studio. We want to study how individuals in New York City allocate their resources to produce Health, health insurance correlated to the roles of age, housing location and education.
  #We assume that Individuals invest in themselves through education, training and health. The goal is to increase earnings. But we cannot confirm that current conditions such outcomes.
  #Also, we believe that Health is a productive good which produces healthy days. This data mentions mood sentiments, which are not the focus of our research. We do predict a relevance of
  #health status based on age, levels of income, education, gender and neighborhoods.
  # We argue also that Education improves efficiency in production, thus educations matters to choices of Health coverage, Health insurance options, so on.
  # We argue also that there is a relationship between the labor-leisure trade-off with respect to allocation of employment to wage-earning activities.
#SUMMRY STATS AND MOVE ON DO SOME FANCY STUFF 
library(tibble)
library(data.table)
library(dplyr)
library(standardize)
library(Matrix)
library(MatrixCorrelation)
library(stringi)
library(stringr)
library(ggplot2)
library(ggplot.multistats)
library(ggplotAssist)
library(tidyverse)
library(tidyr)
library(tidyselect)
library(shiny)
library(shinyAce)
library(shinyWidgets)
library(standardize)
library(StandardizeText)
library(randomForest)
library(randomForestExplainer)
library(stargazer)
attach(chs2019_public)
``` {r} summary(chs2019_public)
#Early steps, you can remember in your head and transform them into factors.

length(chs2019_public)
list(chs2019_public)
``` {r} summary(chs2019_public$generalhealth)

HealthyGroup <- as.factor(as.numeric(generalhealth == 1 | generalhealth ==2 | generalhealth ==3))
table(HealthyGroup)
#Because we have look Thank you Kevin
ExcellentHealth  <- as.factor(as.numeric(generalhealth ==1))
``` {r} table(ExcellentHealth)
VeryGoodHealth <- as.factor(as.numeric(generalhealth ==2))
``` {r}table(VeryGoodHealth)
GoodHealth <- as.factor(as.numeric(generalhealth ==3))
``` {r}table(GoodHealth)
FairHealth <- as.factor(as.numeric(generalhealth ==4))
``` {r}table(FairHealth)
PoorHealth <- as.factor(as.numeric(generalhealth ==5))
``` {r}table(PoorHealth)
Demographic <- as.factor(as.numeric(chs2019_public$age18_64 >0))
``` {r}summary(Demographic)
``` {r}table(Demographic)
BlackPeoples <- as.factor(as.numeric(newrace ==2))
``` {r}table(BlackPeoples)
HispanicPeoples <- as.factor(as.numeric(newrace ==3))
``` {r}table(HispanicPeoples)
WhitePeoples <- as.factor(as.numeric(newrace ==1))
``` {r} table(WhitePeoples)
AsianPeoples <- as.factor(as.numeric(newrace ==4))
``` {r}table(AsianPeoples)
OtherPeoples <-as.factor(as.numeric(newrace ==5))
``` {r}table(OtherPeoples)
educ_nohs <- as.factor(as.numeric(education==1))
``` {r}table(educ_nohs)
educ_hs <- as.factor(as.numeric(education==2))
``` {r}table(educ_hs)
educ_smcoll <- as.factor(as.numeric(education==3))
``` {r} table(educ_smcoll)                        
educ_collegegraduate <- as.factor(as.numeric(education==4))
``` {r}table(educ_collegegraduate)
EmployedforWage <- as.factor(as.numeric(employment19==1))
``` {r}table(EmployedforWage)
SelfEmployed <- as.factor(as.numeric(employment19==2))
``` {r}table(SelfEmployed)
UnemployedOver1yr <-as.factor(as.numeric(employment19==2))
``` {r}table(UnemployedOver1yr)
Unemployedless1yr <-as.factor(as.numeric(employment19==4))
``` {r}table(Unemployedless1yr)
Homemaker <-as.factor(as.numeric(employment19==5))
``` {r}table(Homemaker)
Student <-as.factor(as.numeric(employment19==6))
``` {r}table(Student)
Retired <-as.factor(as.numeric(employment19==7))
``` {r}table(Retired)
UnableToWOrk <-as.factor(as.numeric(employment19==8))
``` {r}table(UnableToWOrk)
PrivateInsurance<- as.factor(as.numeric(insure5==1)) 
``` {r}table(PrivateInsurance) 
Medicare<- as.factor(as.numeric(insure5==2)) 
``` {r}table(Medicare) 
Medicaid <- as.factor(as.numeric(insure5==3)) 
``` {r}table(Medicaid) 
OtherInsurance <- as.factor(as.numeric(insure5==4)) 
``` {r}table(OtherInsurance) 
NoInsurance <- as.factor(as.numeric(insure5==5)) 
``` {r}table(NoInsurance)

``` {r}summary(chs2019_public$nutrition46)
``` {r}summary(chs2019_public$newrace)
``` {r}summary(chs2019_public$generalhealth)
``` {r}summary(chs2019_public$employment19)
``` {r}summary(chs2019_public$bthcontrollastsex19)
``` {r}summary(chs2019_public$maritalstatus19)
``` {r}summary(chs2019_public$weightall)
``` {r}table(chs2019_public$weightall)

print(chs2019_public)
#Let's create a subset of healthy peoples and reported eXCELLENT ("1), Very Good ("2") and Good ('3)pick_use1 <- ((chs2019_public$generalhealth== 1) & (chs2019_public$generalhealth== 2) & (chs2019_public$generalhealth== 3) & (agegroup > 1))
pick_use1 <- ((chs2019_public$generalhealth== 1) | (chs2019_public$generalhealth== 2) | (chs2019_public$generalhealth== 3)) & (agegroup > 1)
``` {r}summary(pick_use1)
dat_use1 <- subset(chs2019_public, pick_use1)
``` {r}summary(pick_use1)
``` {r}summary(dat_use1)
require(standardize)
# The variable Insure5 means Type of health insurance coverage provided in this population sample.
# We want to know if level of work activity provides insurance coverage regardless of locations of living

# "Borough" demonstrates the five boroughs of new York City 
# "dphonew06" explains the District Public Health Offices in New York City
OLS0 <- lm(insure5 ~ weightall + BlackPeoples + WhitePeoples + AsianPeoples +  HispanicPeoples  + OtherPeoples + ExcellentHealth + VeryGoodHealth + FairHealth + PoorHealth + educ_hs + educ_nohs +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Unemployedless1yr + UnemployedOver1yr + Homemaker + Student + Retired + UnableToWOrk)
``` {r}summary(OLS0)
stargazer(OLS0, type = "text", title = "Relationshionship between Health Insurance, Nutrition and BMI" )
stargazer(OLS0, type = "text")
exp(OLS0$coefficients)
``` {r, echo=FALSE} plot plot(coef(OLS0))
par(mfrow=c(2,2))
``` {r, echo=FALSE} plot(OLS0,col="blue",pch=16,cex=1,lwd=1,lty=2)

#Now let's look at the impact poverty line based on Zipcodes across the City:
OLS1 <- lm(imputed_neighpovgroup4_1418 ~ weightall +  insure5 +   BlackPeoples + WhitePeoples + AsianPeoples +  HispanicPeoples +OtherPeoples + ExcellentHealth + VeryGoodHealth + FairHealth + PoorHealth + educ_hs + educ_nohs +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Unemployedless1yr + UnemployedOver1yr + Homemaker + Student + Retired + UnableToWOrk)
``` {r}summary(OLS1)
stargazer(OLS1, type = "text", title = "Relationshionship between Community Support, Nutrition and BMI" )
exp(OLS1$coefficients, type = "text", title = "Relationshionship between Community Support, Nutrition and BMI")
exp(OLS1$coefficients, type = "text")
exp(OLS1$coefficients)



#Standard Agency area-based poverty measure, based on % of population in respondent's zip code living below 100% FPL per American Community Survey 2014-2018, with imputation of missing cases.
#Neighborhood poverty; percent of zip code population living below 100% FPL per American Community Survey, 2014-2018
``` {r}summary(chs2019_public$imputed_neighpovgroup4_1418)
``` {r} table(chs2019_public$imputed_neighpovgroup4_1418)

#  1=  0 - <10% (low pov)   1,493
#  2=  10 - <20%            3,717
#  3=  20 - <30%            2,272
#  4=  30 - <100% (very hi) 1,321

summary(imputed_neighpovgroup4_1418)

#Now let's look at the impact poverty line based on Zipcodes across the City:  
OLS1 <- lm(imputed_neighpovgroup4_1418 ~ weightall +  insure5 +   BlackPeoples + WhitePeoples + AsianPeoples +  HispanicPeoples +OtherPeoples + ExcellentHealth + VeryGoodHealth + FairHealth + PoorHealth + educ_hs + educ_nohs +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Unemployedless1yr + UnemployedOver1yr + Homemaker + Student + Retired + UnableToWOrk)
``` {r}summary(OLS1)
stargazer(OLS1, type = "text")
exp(OLS1$coefficients)
plot(coef(OLS1))
par(mfrow=c(2,2))
plot(OLS1,col="red",pch=16,cex=1,lwd=1,lty=2)
plot(OLS1,col="red",pch=40,cex=1,lwd=1,lty=2)

OLS2 <- lm(nutrition46 ~weightall + insure5 + BlackPeoples + WhitePeoples + AsianPeoples +  HispanicPeoples +  HispanicPeoples  + OtherPeoples + ExcellentHealth + VeryGoodHealth + FairHealth + PoorHealth + educ_hs + educ_nohs +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Unemployedless1yr + UnemployedOver1yr + Homemaker + Student + Retired + UnableToWOrk)

``` {r}summary(OLS2)
stargazer(OLS2, type = "text", title = "Racial implications of Community Support, Nutrition and Insurance" )
stargazer(OLS3, type = "text", title = "Racial implications of Community Support, Nutrition and Health Insurance")
summary(chs2019_public$imputed_povertygroup)
WealthyGroup <-as.factor(as.numeric(imputed_povertygroup==5))
BlackPeoples <- as.factor(as.numeric(newrace ==2))
LOGIT <- glm(WealthyGroup ~ weightall + insure5 + BlackPeoples + WhitePeoples + HispanicPeoples +
               ExcellentHealth  + VeryGoodHealth + FairHealth + PoorHealth
             +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Student + Retired, family=binomial)
LOGIT <- glm(WealthyGroup ~ weightall + insure5 + BlackPeoples + WhitePeoples + HispanicPeoples +
               ExcellentHealth  + VeryGoodHealth + FairHealth + PoorHealth
             +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Student + Retired, family=binomial)
``` {r}summary(LOGIT)









# Let's see if good nutrition is correlated to levels of education, race, zipcdes, health status and status of employment
summary(nutrition46)
OLS2 <- lm(nutrition46 ~weightall + insure5 + BlackPeoples + WhitePeoples + AsianPeoples +  HispanicPeoples +  HispanicPeoples  + OtherPeoples + ExcellentHealth + VeryGoodHealth + FairHealth + PoorHealth + educ_hs + educ_nohs +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Unemployedless1yr + UnemployedOver1yr + Homemaker + Student + Retired + UnableToWOrk)
``` {r}summary(OLS2)
stargazer(OLS2, type = "text")
exp(OLS2$coefficients)
plot(coef(OLS2))
par(mfrow=c(2,2))
plot(OLS2,col="brown",pch=16,cex=1,lwd=1,lty=2)











summary(chs2019_public$imputed_povertygroup)
#set factors

OLS3 <- lm(imputed_povertygroup ~ weightall + insure5 + BlackPeoples + WhitePeoples + AsianPeoples +  HispanicPeoples  + OtherPeoples + ExcellentHealth + VeryGoodHealth + FairHealth + PoorHealth + educ_hs + educ_nohs +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Unemployedless1yr + UnemployedOver1yr + Homemaker + Student + Retired + UnableToWOrk)
``` {r}summary(OLS3)
stargazer(OLS3, type = "text")
exp(OLS3$coefficients)
plot(coef(OLS3))
par(mfrow=c(2,2))
plot(OLS3,col="Green",pch=16,cex=1,lwd=1,lty=2)

``` {r}summary(chs2019_public$imputed_povertygroup)
WealthyGroup <-as.factor(as.numeric(imputed_povertygroup==5))

BlackPeoples <- as.factor(as.numeric(newrace ==2))

LOGIT <- glm(WealthyGroup ~ weightall + insure5 + BlackPeoples + WhitePeoples + HispanicPeoples + AsianPeoples+
               ExcellentHealth  + VeryGoodHealth + FairHealth + PoorHealth +
               + educ_hs + educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Homemaker + Student + Retired, family=binomial)
``` {r}summary(LOGIT)


OLSTEST <- lm(imputed_neighpovgroup4_1418 ~ weightall + insure5 + BlackPeoples + WhitePeoples + HispanicPeoples + AsianPeoples+
                ExcellentHealth  + VeryGoodHealth + FairHealth + PoorHealth +
                + educ_hs + educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Homemaker + Student + Retired)
``` {r}summary(OLSTEST)

HealthyGroup <- as.factor(as.numeric(generalhealth == 1 | generalhealth ==2 | generalhealth ==3))
LOGIT2 <- glm(HealthyGroup ~ weightall + insure5 + BlackPeoples + WhitePeoples + HispanicPeoples + PoorHealth
              +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Student + Retired, family=binomial)
``` {r}summary(LOGIT2)


#We set Health Insurance coverage as Independent variable. Level of poverty by area is the dependent variable
to_be_predicted<- data.frame(insure5=="1",    data = pick_use1)
to_be_predicted$yhat<-predict(LOGIT2, to_be_predicted, type="response")
summary(to_be_predicted$yhat)

#OUTput
# Min.  1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0000  0.6943  0.8308  0.7584  0.9089  0.9725     522 
#We see that out residents with Private insurance coverag are more likely to cover their nutrition and watch their weight 75.84% have been vaccinated to the flu shot


#Poverty levels and race are closely relatd based on this relationship with OLS3.
#We can  see that Optimal health stock/incentives will decline as the person ages if the depreciation rate of health increases as a person ages. We can see that race matters to our population sample.

#Benefits of good health are greater for high wage workers in NYC so they demand higher optimal health facilitations such as Medicaid, Medicare.   

#The more educated people are, the less costly it is to generate health resulting in a higher optimal health stock for these groups based on race, age and kinds of employment.

# Individuals will allocate resources in order to produce health capital in to order to maintain their activity and health status and their job and earnings.


HealthyGroup \<- as.factor(as.numeric(generalhealth == 1 | generalhealth ==2 | generalhealth ==3)) 
LOGIT2 <- glm(HealthyGroup ~ weightall + insure5 + BlackPeoples + WhitePeoples + HispanicPeoples + PoorHealth + GoodHealth + educ_hs + educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Student + Retired+ Homemaker, family=binomial) summary(LOGIT2) 
plot(LOGIT2) 
library(stargazer) 
stargazer(OLS0,OLS1,OLS2,OLS3, type="text", font.size = "small", digits = 2, title = "Linear Regressions Outputs")

summary(chs2019_public$weightall)
str(chs2019_public)
TESTDEC <- ggplot(chs2019_public, aes(x=insure5, y = age18_64))+ geom_bar()
TESTDEC
plot(TESTDEC)
TESTDEC <- ggplot(chs2019_public, aes(x=BlackPeoples, y = age18_64))+ geom_bar(stat = "identity")
TESTDEC <- ggplot(chs2019_public, aes(x=BlackPeoples, y = age18_64))
plot(TESTDEC)
TESTDEC <- ggplot(chs2019_public, aes(x=BlackPeoples, y = age18_64))+ geom_boxplot()
plot(TESTDEC)
TESTDEC <- ggplot(chs2019_public, aes(x=weightall, y = insure5))+ geom_bar(stat = "identity")+ coord_flip()
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=BlackPeoples, y = age18_64))+ geom_bar(stat = "identity")
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=BlackPeoples, y = age18_64))+ geom_boxplot()
plot(TESTDEC)
TESTDEC <- ggplot(chs2019_public, aes(x=BlackPeoples, y = age18_64))+ GeomBar()
TESTDEC <- ggplot(chs2019_public, aes(x=BlackPeoples, y = age18_64))+ Geom_bar()
TESTDEC <- ggplot(chs2019_public, aes(x=BlackPeoples, y = age18_64))+ geom_bar()
plot(TESTDEC)
TESTDEC <- ggplot(chs2019_public, aes(x=PoorHealth, y = age18_64))+ geom_bar()
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=PoorHealth, y = age18_64))+ geom_bar(stat="Poor Peoples Health")
TESTDEC <- ggplot(chs2019_public, aes(x=BlackPeoples, y = age18_64))+ geom_bar(stat = "identity")
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=weightall, y = insure5))+ geom_bar(stat = "identity")+ coord_flip()
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=weightall, y = insure5))+ geom_bar(stat = "identity")
TESTDEC
HealthyGroup <- as.factor(as.numeric(generalhealth == 1 | generalhealth ==2 | generalhealth ==3))
LOGIT2 <- glm(HealthyGroup ~ weightall + insure5 + BlackPeoples + WhitePeoples + HispanicPeoples + PoorHealth + GoodHealth + educ_hs
              +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Student + Retired+ Homemaker, family=binomial)
TESTDEC <- ggplot(chs2019_public, aes(x=weightall, y = imputed_neighpovgroup4_1418))+ geom_bar(stat = "Weight & Wealth")
TESTDEC <- ggplot(chs2019_public, aes(x=weightall, y = imputed_neighpovgroup4_1418))+ geom_bar(stat ="weightwealth")
TESTDEC <- ggplot(chs2019_public, aes(x=weightall, y = imputed_neighpovgroup4_1418))+ geom_bar(stat ="weight")
TESTDEC <- ggplot(chs2019_public, aes(x=weightall, y = imputed_neighpovgroup4_1418))+ geom_point(stat ="weight")
TESTDEC <- ggplot(chs2019_public, aes(x=weightall, y = imputed_neighpovgroup4_1418))+ geom_point()
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=weightall, y = imputed_neighpovgroup4_1418))+ geom_col()
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=weightall, y = insure5))+ geom_bar(stat = "identity")
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=weightall, y = imputed_neighpovgroup4_1418))+ geom_col()
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=age18_64, y = insure5))+ geom_bar(stat = "identity")+ coord_flip()+xlim(18,64)
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=age18_64, y = insure5))+ geom_col(stat = "identity")+ coord_flip()+xlim(18,64)
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=age18_64, y = insure5))+ geom_point(stat = "identity")+ coord_flip()+xlim(18,64)
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=age18_64, y = insure5))+ geom_blank(stat = "identity")+ coord_flip()+xlim(18,64)
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=age18_64, y = insure5))+ geom_bar(stat = "identity")+ coord_flip()+xlim(18,64)
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=age18_64, y = insure5))+ geom_bar(stat = "identity")+ coord_flip()+xlim(18,64)
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=age18_64, y = insure5))+ geom_bar(stat = "identity")+xlim(18,64)
TESTDEC
TESTDEC <- ggplot(chs2019_public, aes(x=age18_64, y = insure5))+ geom_bar(stat = "identity")+xlim(18,64)
plot(TESTDEC)
TESTDEC <- ggplot(chs2019_public, aes(x=age18_64, y = insure5))+ geom_bar(stat = "identity")+coord_flip()
TESTDEC
TESTDEC1 <- ggplot(chs2019_public, aes(x= insure5))+ geom_bar()
TESTDEC1
TESTDEC1X <- ggplot(chs2019_public, aes(x= generalhealth))+ geom_bar()
TESTDEC1 <- ggplot(chs2019_public, aes(x= insure5))+ geom_bar()
TESTDEC1
TESTDEC1X <- ggplot(chs2019_public, aes(x= generalhealth))+ geom_bar()
plot(TESTDEC1X)
chs2019_public$KEVIN <- factor(chs2019_public$generalhealth, levels = c(1,2,3,4,5),
                               labels =c("ExcellentHealth", "VeryGoodHealth", "GoodHealth", "FairHealth ", "PoorHealth"))
TESTDEC2 <- ggplot(chs2019_public, aes(x= KEVIN))+ geom_bar()+
  plot(TESTDEC2)
TESTDEC2 <- ggplot(chs2019_public, aes(x= KEVIN))+ geom_bar()
plot(TESTDEC2)
chs2019_public$HEALTH <- factor(chs2019_public$generalhealth, levels = c(1,2,3,4,5),
                                labels =c("ExcellentHealth", "VeryGoodHealth", "GoodHealth", "FairHealth ", "PoorHealth"))
TESTDEC2 <- ggplot(chs2019_public, aes(x= HEALTH))+ geom_bar()
T <-TESTDEC+coord_flip()
TESTDEC22 <- ggplot(chs2019_public, aes(x= HEALTH))+ geom_bar()+coord_flip()
plot(TESTDEC22)
TESTDEC3 <- ggplot(chs2019_public, aes(x= HEALTH))+ geom_bar()+coord_flip()
plot(TESTDEC3)
TESTDEC2 <- ggplot(chs2019_public, aes(x= HEALTH))+ geom_bar()
plot(TESTDEC2)
TESTDEC3 <- ggplot(chs2019_public, aes(x= HEALTH))+ geom_bar()+coord_flip()+ scale_fill_hue(c=34)+ ggtitle("HealthCommunity")
plot(TESTDEC3)
plot(TESTDEC4)
chs2019_public$RACE <- factor(chs2019_public$newrace, levels = c(1,2,3,4,5),
                              labels =c("BlackPeoples", "HispanicPeoples", "WhitePeoples", "AsianPeoples ", "OtherPeoples"))
TESTRACE <-ggplot(chs2019_public, aes(x= RACE))+ geom_bar()+coord_flip()
plot(TESTRACE)
TESTRACE1 <- ggplot(chs2019_public, aes(x= RACE,fill=RACE))+ geom_bar()+coord_flip()+ scale_fill_hue(c=40)+ ggtitle("RACIALCommunity")
plot(TESTRACE1)
TESTRACE1A <- ggplot(chs2019_public, aes(x= RACE,fill=RACE))+ geom_bar()+ scale_fill_hue(c=40)+ ggtitle("RACIALCommunity")
plot(TESTRACE1A)
TESTDEC4 <- ggplot(chs2019_public, aes(x= HEALTH,fill=HEALTH))+ geom_bar()+ scale_fill_hue(c=40)+ ggtitle("HealthCommunity")
plot(TESTDEC4)
chs2019_public$EDUCATED <- factor(chs2019_public$education, levels = c(1,2,3,4),
                                  labels =c("educ_nohs", "educ_hs", "educ_smcoll", "educ_collegegraduate"))
TESTEDUCATED <-ggplot(chs2019_public, aes(x= EDUCATED))+ geom_bar()+coord_flip()
plot(TESTEDUCATED)
TESTEDUCATED1 <- ggplot(chs2019_public, aes(x= EDUCATED,fill=EDUCATED))+ geom_bar()+coord_flip()+ scale_fill_hue(c=40)+ ggtitle("EDUCATEDCommunity")
plot(TESTEDUCATED1)
TESTEDUCATED1A <- ggplot(chs2019_public, aes(x= EDUCATED,fill=EDUCATED))+ geom_bar()+ scale_fill_hue(c=40)+ ggtitle("EDUCATED Community")
plot(TESTEDUCATED1A)
TESTRACE1 <- ggplot(chs2019_public, aes(x= RACE,fill=RACE))+ geom_bar()+coord_flip()+ scale_fill_hue(c=40)+ ggtitle("RACIALCommunity")
plot(TESTRACE1)
TESTRACE1A <- ggplot(chs2019_public, aes(x= RACE,fill=RACE))+ geom_bar()+ scale_fill_hue(c=40)+ ggtitle("RACIALCommunity")
plot(TESTRACE1A)
TESTDEC3 <- ggplot(chs2019_public, aes(x= HEALTH))+ geom_bar()+coord_flip()+ scale_fill_hue(c=34)+ ggtitle("HealthCommunity")
plot(TESTDEC3)
plot(TESTDEC4)
TESTEDUCATED <-ggplot(chs2019_public, aes(x= EDUCATED))+ geom_bar()+coord_flip()
plot(TESTEDUCATED)
TESTEDUCATED1 <- ggplot(chs2019_public, aes(x= EDUCATED,fill=EDUCATED))+ geom_bar()+coord_flip()+ scale_fill_hue(c=40)+ ggtitle("EDUCATEDCommunity")
plot(TESTEDUCATED1)
TESTEDUCATED1A <- ggplot(chs2019_public, aes(x= EDUCATED,fill=EDUCATED))+ geom_bar()+ scale_fill_hue(c=40)+ ggtitle("EDUCATED Community")
plot(TESTEDUCATED1A)
#scatter plot
TESTRACE <-ggplot(chs2019_public, aes(x= RACE, y= employment19))+ geom_point(size =2, shape= 50)
plot(TESTRACE)
TESTRACE <-ggplot(chs2019_public, aes(x= RACE, y= employment19))+ geom_point(aes(size=8803))
plot(TESTRACE)
chs2019_public$EMPLOYMENT <- factor(chs2019_public$employment19, levels = c(1,2,3,4,5,6,7,8),
                                    labels =c("EmployedforWage", "SelfEmployed", "UnemployedOver1yr", "Unemployedless1yr", "Homemaker", "Student", "Retired", "UnableToWOrk"))
TESTEMPLOYMENT <-ggplot(chs2019_public, aes(x= EMPLOYMENT))+ geom_bar()+coord_flip()
plot(TESTEMPLOYMENT)
TESTEMPLOYMENT1 <- ggplot(chs2019_public, aes(x= EMPLOYMENT,fill=EMPLOYMENT))+ geom_bar()+coord_flip()+ scale_fill_hue(c=40)+ ggtitle("EMPLOYMENT IN THE Community")
plot(TESTEMPLOYMENT1)
TESTEMPLOYMENT1A <- ggplot(chs2019_public, aes(x= EMPLOYMENT,fill=EMPLOYMENT))+ geom_bar()+ scale_fill_hue(c=40)+ ggtitle("EMPLOYMENT IN THE Community")
plot(TESTEMPLOYMENT1A)
#LET'S REMEMBER THAT
chs2019_public$HEALTH <- factor(chs2019_public$generalhealth, levels = c(1,2,3,4,5),
                                labels =c("ExcellentHealth", "VeryGoodHealth", "GoodHealth", "FairHealth ", "PoorHealth"))
TEST5 <- ggplot(chs2019_public, aes(age18_64, wt20_dual, colour=HEALTH))+ geom_boxplot()
TEST5+ labs(colour = "HEALTH")
TEST5 <- ggplot(chs2019_public, aes(age18_64, insure19r, colour=HEALTH))+ geom_boxplot()
TEST5+ labs(colour = "HEALTH")
TEST5 <- ggplot(chs2019_public, aes(age18_64, insure5, colour=HEALTH))+ geom_boxplot()
TEST5+ labs(colour = "HEALTH")
TEST5 <- ggplot(chs2019_public, aes(cid, wt20_dual, colour=HEALTH))+ geom_point()
TEST5+ labs(colour = "HEALTH")
TEST6 <- ggplot(chs2019_public, aes(age21up, wt20_dual, colour=HEALTH))+ geom_area()
TEST7 <- ggplot(chs2019_public, aes(age21up, nutrition46, colour=HEALTH))+ geom_area()
TEST7 + labs(colour = "HEALTH")
TEST8 <- ggplot(chs2019_public, aes(age21up, nutrition47, colour=HEALTH))+ geom_area()
TEST8 + labs(colour = "HEALTH")
TEST9 <- ggplot(chs2019_public, aes(cid, wt20_dual, colour=EMPLOYMENT))+ geom_point()
TEST9+ labs(colour = "EMPLOYMENT")
TEST10 <- ggplot(chs2019_public, aes(insure5, newrace, colour=EMPLOYMENT))+ geom_point()
TEST10+ labs(colour = "EMPLOYMENT")
TEST11 <- ggplot(chs2019_public, aes(insure5, newrace, colour=EMPLOYMENT))+ geom_bar()
TEST11+ labs(colour = "EMPLOYMENT")
TEST12 <- ggplot(chs2019_public, aes(insure5, newrace, colour=EMPLOYMENT))+ geom_col()
TEST12+ labs(colour = "EMPLOYMENT")
TEST13 <- ggplot(chs2019_public, aes(age21up, nutrition46, colour=HEALTH))+ geom_area()
TEST13 + labs(colour = "HEALTH")
TEST14 <- ggplot(chs2019_public, aes(age21up, newrace, colour=HEALTH))+ geom_area()
TEST14 + labs(colour = "HEALTH")
TEST15 <- ggplot(chs2019_public, aes(age21up, newrace, colour=HEALTH))+ geom_curve()
TEST15 + labs(colour = "HEALTH")
TEST16 <- ggplot(chs2019_public, aes(age21up, newrace, colour=HEALTH))+ geom_crossbar()
TEST16 + labs(colour = "HEALTH")
TEST17<- ggplot(chs2019_public, aes(age21up, newrace, colour=HEALTH))+ geom_dotplot()
TEST17 + labs(colour = "HEALTH")

TEST18 <- ggplot(chs2019_public, aes(age21up, nutrition47, colour=HEALTH))+ geom_density()
TEST18 + labs(colour = "HEALTH")
TEST19 <- ggplot(chs2019_public, aes(age21up, newrace, colour=HEALTH))+ geom_jitter()
TEST19 + labs(colour = "HEALTH")
TEST20 <- ggplot(chs2019_public, aes(age21up, newrace, colour=HEALTH))+ geom_freqpoly()
TEST20 + labs(colour = "HEALTH")
TEST21 <- ggplot(chs2019_public, aes(BlackPeoples, age21up, colour=HEALTH))+ geom_freqpoly()
TEST21 + labs(colour = "HEALTH")
TEST22 <- ggplot(chs2019_public, aes(newrace, age21up, colour=HEALTH))+ geom_freqpoly()
TEST22 + labs(colour = "HEALTH")
TEST23 <- ggplot(chs2019_public, aes(wt20_dual, age21up, colour=HEALTH))+ geom_freqpoly()
TEST23 + labs(colour = "HEALTH")
TEST24 <- ggplot(chs2019_public, aes(nutrition46, age21up, colour=HEALTH))+ geom_freqpoly()
TEST24 + labs(colour = "HEALTH")
TEST25 <- ggplot(chs2019_public, aes(insure5, newrace, colour=EMPLOYMENT))+ geom_col()+ labs(colour = "EMPLOYMENT")
TEST25+ labs(colour = "EMPLOYMENT")
TEST26 <- ggplot(chs2019_public, aes(nutrition46, age21up, colour=HEALTH))+ geom_freqpoly()+ labs(colour = "EMPLOYMENT")
TESTDEC6
TESTDEC27 <- ggplot(chs2019_public, aes(nutrition46, age21up, colour=HEALTH))+ geom_freqpoly()+ labs(colour = "HEALTH")
stargazer(OLS0,OLS1,OLS2,OLS3, type="text", font.size = "small", digits = 2, title = "Linear Regressions Outputs")
to_be_predicted$yhat<-predict(LOGIT2, to_be_predicted, type="response")
library(stargazer)
stargazer(OLS0,OLS1,OLS2,OLS3, type="text", font.size = "small", digits = 2, title = "Linear Regressions Outputs")
stargazer(LOGIT,LOGIT2, type="text", font.size = "small", digits = 2, title = "Linear Regressions Outputs")
#LET'S REMEMBER THAT
chs2019_public$HEALTH <- factor(chs2019_public$generalhealth, levels = c(1,2,3,4,5),
                                labels =c("ExcellentHealth", "VeryGoodHealth", "GoodHealth", "FairHealth ", "PoorHealth"))
TESTDEC5 <- ggplot(chs2019_public, aes(age18_64, insure5, colour=HEALTH))+ geom_boxplot()
TESTDEC5+ labs(colour = "HEALTH")
TESTDEC5 <- ggplot(chs2019_public, aes(cid, wt20_dual, colour=HEALTH))+ geom_point()
#LET'S REMEMBER THAT
chs2019_public$HEALTH <- factor(chs2019_public$generalhealth, levels = c(1,2,3,4,5),
                                labels =c("ExcellentHealth", "VeryGoodHealth", "GoodHealth", "FairHealth ", "PoorHealth"))
TESTDEC5 <- ggplot(chs2019_public, aes(age18_64, insure5, colour=HEALTH))+ geom_boxplot()
#lOOKING AT THE RELATIONSHIP BETWEEN POPULATION IN RELATION WITH BMI AND HEALTH STATUS.
# Despite the presence of 152 variables, we count only 02 continous variable wt20_dual_q1 and wt20_dual, both serving to evaluate to stree the impact / weight of CHS2019 ON THE PREVIOUS STUDIES.
library(ggplot2)
library(ggplot.multistats)
install.packages("ggplot2")
install.packages("ggplot2")
install.packages("ggplot2")
install.packages("ggplot2")
install.packages("ggplot2")
install.packages("ggplot2")
require(ggplot2)
TESTDEC6 <- ggplot(chs2019_public, aes(insure5, newrace, colour=EMPLOYMENT))+ geom_col()
TESTDEC6 <- ggplot(chs2019_public, aes(insure5, newrace, colour=EMPLOYMENT))+ geom_col()
TESTDEC6+ labs(colour = "EMPLOYMENT")
TESTDEC6 <- ggplot(chs2019_public, aes(insure5, newrace, colour= EmployedforWage))+ geom_col()
TESTDEC6+ labs(colour = "EMPLOYMENT")


