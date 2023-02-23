# For use with Retention/Churn Exercise

library(BBmisc)# nice normalize function
library(purrr)
library(C50)
library(psych)
library(neuralnet)
library(dplyr)
library(pdp)
library(gmodels)
library(randomForest)
library(lubridate)
library(caret)

#Read in data file ** put in your path **

Transact.df = read.csv("C:/Users/sriwi/OneDrive/Documents/MBA/Smith School of Business/Semester 4/BUMK 747 CRM Analytics/Retention Churn Exercise/ISMSDataset1.csv")

# Fix dates - Create year variable

Transact.df$TRANSACTION_DATE_2<-ymd(dmy_hms(Transact.df$TRANSACTION_DATE))
Transact.df$TRANSACTION_DATE_YR<-year(Transact.df$TRANSACTION_DATE_2)

#Aggregate to household level, create variables you want
# NB: These are just example. Lots of opportunity for feature
# engineering here.

# NOTE: This will throw a warning, but that's OK ... 

hh_transact <- subset(Transact.df,TRANSACTION_DATE_YR<=2004) %>% group_by(HOUSEHOLD_ID) %>% 
  summarise(firstpurch = min(as.Date(TRANSACTION_DATE_2)),
            lastpurch02 = max(as.Date(TRANSACTION_DATE_2[TRANSACTION_DATE_YR<=2002])), #most recent purchase as of 12-31-2002
            purch98 = length(TRANSACTION_NBR[TRANSACTION_DATE_YR==1998]),
            purch99 = length(TRANSACTION_NBR[TRANSACTION_DATE_YR==1999]),
            purch00 = length(TRANSACTION_NBR[TRANSACTION_DATE_YR==2000]),
            purch01 = length(TRANSACTION_NBR[TRANSACTION_DATE_YR==2001]),
            purch02 = length(TRANSACTION_NBR[TRANSACTION_DATE_YR==2002]),
            purch03 = length(TRANSACTION_NBR[TRANSACTION_DATE_YR==2003]),
            purch04 = length(TRANSACTION_NBR[TRANSACTION_DATE_YR==2004]),
            doll98 = sum(EXTENDED_PRICE[TRANSACTION_DATE_YR==1998]),
            doll99 = sum(EXTENDED_PRICE[TRANSACTION_DATE_YR==1999]),
            doll00 = sum(EXTENDED_PRICE[TRANSACTION_DATE_YR==2000]),
            doll01 = sum(EXTENDED_PRICE[TRANSACTION_DATE_YR==2001]),
            doll02 = sum(EXTENDED_PRICE[TRANSACTION_DATE_YR==2002]),
            AGE_H_HEAD = first(AGE_H_HEAD),
            CHILDERN_PRESENCE = first(CHILDERN_PRESENCE),
            INCOME = first(INCOME),
            GENDER_H_HEAD = first(GENDER_H_HEAD),
            GENDER_INDIVIDUAL = first(GENDER_INDIVIDUAL),
            MALE_CHID_AGE_0_2 = first(MALE_CHID_AGE_0_2),
            MALE_CHID_AGE_3_5 = first(MALE_CHID_AGE_3_5),
            MALE_CHID_AGE_6_10 = first(MALE_CHID_AGE_6_10),
            MALE_CHID_AGE_11_15 = first(MALE_CHID_AGE_11_15),
            MALE_CHID_AGE_16_17 = first(MALE_CHID_AGE_16_17),
            FEMALE_CHID_AGE_0_2 = first(FEMALE_CHID_AGE_0_2),
            FEMALE_CHID_AGE_3_5 = first(FEMALE_CHID_AGE_3_5),
            FEMALE_CHID_AGE_6_10 = first(FEMALE_CHID_AGE_6_10),        
            FEMALE_CHID_AGE_11_15 = first(FEMALE_CHID_AGE_11_15),
            FEMALE_CHID_AGE_16_17 = first(FEMALE_CHID_AGE_16_17),       
            UNKNOWN_CHID_AGE_0_2 = first(UNKNOWN_CHID_AGE_0_2),
            UNKNOWN_CHID_AGE_3_5 = first(UNKNOWN_CHID_AGE_3_5),        
            UNKNOWN_CHID_AGE_6_10 = first(UNKNOWN_CHID_AGE_6_10),
            UNKNOWN_CHID_AGE_11_15 = first(UNKNOWN_CHID_AGE_11_15),      
            UNKNOWN_CHID_AGE_16_17 = first(UNKNOWN_CHID_AGE_16_17),
            numcat = n_distinct(CATEGORY_DESCRIPTION[TRANSACTION_DATE_YR>=1998 & TRANSACTION_DATE_YR<=2002]),
            numbrand = n_distinct(TRANSACTION_TYPE_DESCRIPTION[TRANSACTION_DATE_YR>=1998 & TRANSACTION_DATE_YR<=2002])
  )

# You will still need to deal with missings
#  For numerics, maybe just use means; for factors maybe "UNKNOWN"

# Below is one approach. There are many others.
# means for income and age

hh_transact$AGE_H_HEAD <- ifelse(is.na(hh_transact$AGE_H_HEAD), mean(hh_transact$AGE_H_HEAD,na.rm=TRUE),hh_transact$AGE_H_HEAD)
hh_transact$INCOME <- ifelse(is.na(hh_transact$INCOME), mean(hh_transact$INCOME,na.rm=TRUE),hh_transact$INCOME)

# New category for variables

hh_transact$CHILDERN_PRESENCE <- as.factor(ifelse(hh_transact$CHILDERN_PRESENCE=="N" | hh_transact$CHILDERN_PRESENCE=="Y",hh_transact$CHILDERN_PRESENCE, "UNKNOWN"))
hh_transact$GENDER_H_HEAD <- as.factor(hh_transact$GENDER_H_HEAD)
hh_transact$GENDER_INDIVIDUAL <- as.factor(ifelse(hh_transact$GENDER_INDIVIDUAL=="F" | hh_transact$GENDER_INDIVIDUAL=="M" ,hh_transact$GENDER_INDIVIDUAL, "UNKNOWN"))

# We want to get rid of those households that had their first purchase after 2002

hh_transact <- hh_transact[year(hh_transact$firstpurch)<=2002,]


# These will be useful in RFM calc

hh_transact$numpurch <- hh_transact$purch98 + hh_transact$purch99 + hh_transact$purch00 + hh_transact$purch01 + hh_transact$purch02
hh_transact$dollpurch <- hh_transact$doll98 + hh_transact$doll99 + hh_transact$doll00 + hh_transact$doll01 + hh_transact$doll02
hh_transact$recent <- as.numeric(as.Date("2002-12-31")-as.Date(hh_transact$lastpurch02))

# The two basic "retention" measures: at least on purchase in 2003 ...

hh_transact$ret03 <- (hh_transact$purch03>0)

# ... at least one purchase in 2004

hh_transact$ret04 <- (hh_transact$purch04>0)

#############################################################################
# RFM measures as of end of 2002

# Recency Measure is most recent purchase
# Frequency is purchase rate since acquired (purchases per )
# Monetary is dollar purchase rate

# Average purchase frequency from first purchase until end of 2002:

hh_transact$frequent <- hh_transact$numpurch/(as.numeric(as.Date("2002-12-31")- hh_transact$firstpurch))*30

# Average purchase amount from first purchase until end of 2002:

hh_transact$monetary <- hh_transact$dollpurch/(as.numeric(as.Date("2002-12-31")- hh_transact$firstpurch))*30

# Now put into "quintiles" - there are 19474 hh's, so lets do 4000 - 4000 - 4000 - 4000 - 3474

hh_transact <- as.data.frame(hh_transact[order(hh_transact$recent),])
hh_transact$rix <- 1:nrow(hh_transact) # recency index
hh_transact$R_Q<-ceiling(hh_transact$rix/4000)

hh_transact <- as.data.frame(hh_transact[order(-hh_transact$frequent),])
hh_transact$fix <- 1:nrow(hh_transact) # frequency index
hh_transact$F_Q<-ceiling(hh_transact$fix/4000)

hh_transact <- as.data.frame(hh_transact[order(-hh_transact$monetary),])
hh_transact$mix <- 1:nrow(hh_transact) # monetary index
hh_transact$M_Q<-ceiling(hh_transact$mix/4000)

# Create one RFM score

hh_transact$RFMscore <- hh_transact$R_Q*100+hh_transact$F_Q*10+hh_transact$M_Q

#Neural Network

#spliting


set.seed(13343)
train_ind <- createDataPartition(y = hh_transact$ret03,p=.8,list = FALSE)
training <- hh_transact[train_ind,]
test <- hh_transact[-train_ind,]

# To check balance

prop.table(table(hh_transact$ret03))
prop.table(table(training$ret03))
prop.table(table(test$ret03))

# Divide training sample to create validation sample 
set.seed(34331)
val_ind <- createDataPartition(y = training$ret03,p=.25,list = FALSE)
val <- training[val_ind,]
training <- training[-val_ind,]

prop.table(table(val$ret03))


nnet = neuralnet(ret03~RFMscore+purch98+purch99+purch00+purch01+purch02+INCOME,
                 training, hidden = 6, threshold = 0.01, linear.output = FALSE)
plot(nnet)





