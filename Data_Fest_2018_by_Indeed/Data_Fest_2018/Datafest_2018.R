setwd("/Users/junhyukjang/Desktop/FROM OLD MAC/FOLDERS FROM MAC/2018 DataFest")
library("dplyr")
#install.packages('data.table')
library("data.table")
library(lubridate)
# Read Data
df <- fread("datafest2018.csv")
# 23columns and 65535rows
# data cleaning 
df$avgOverallRating <- as.numeric(df$avgOverallRating)
df$numReviews <- as.numeric(df$numReviews)
df$descriptionCharacterLength <- as.numeric(df$descriptionCharacterLength)
df$descriptionWordCount <- as.numeric(df$descriptionWordCount)
df$experienceRequired <- as.numeric(df$experienceRequired)
df$estimatedSalary <- as.numeric(df$estimatedSalary)
df$supervisingJob <- as.factor(df$supervisingJob)
df$licenseRequiredJob <- as.factor(df$licenseRequiredJob)
df$jobAgeDays <- as.integer(df$jobAgeDays)
df$clicks <- as.integer(df$clicks)
df$localClicks <- as.integer(df$localClicks)

df1 <- df
head(df1)
head(df1$experienceRequired,50)
head(df1$salaryCurrency,50)
head(df1$country,50)
head(df1$localClicks)

group_by(df1,country) %>%  summarise(length = length(estimatedSalary)) %>% max()
a <- group_by(df1,industry) %>%  summarise(length = length(industry)) 

# the most associated industry with company.
sort(a$length,decreasing = T)

unique(df1$experienceRequired)
sum(is.na(d12$experienceRequired))

table(df1$country)
head(df1$estimatedSalary)

###
length(unique(df$jobId))
length(unique(df1[df1$jobAgeDays]))
b <- df1[df1$jobAgeDays == "100"]
head(b$jobId,10)
df1[df1$jobId == "job0000045"]
length(unique(df1$jobId))
df[df1$jobId == "job0000045"]
df[df1$jobId == "job0000019"]

df[df1$jobId == "job0000045"]
## 
table(df1$jobAgeDays)

dim(df1)


df1 = cbind(df1,c(rep("0", nrow(df1))))
df1[1,24] = "0"

#df1=df1[,-24]
dim(df1)

#2:100000
#100001:200000
#200001:300000
#300001:400000
#400001:500000
#500001:600000
#600001:700000
#700001:800000
#800001:900000
#900001:1000000
#1000001:1100000
#1100001:1200000
#1200001:1300000
#1300001:1400000
#1400001:14586035

df1[1,24]

df1[2,24]

for(i in 2:100000){
        if(df1[i,21] == "0"){
                if(df1[i,3] == df1[i-1,3]){
                        df1[i,24] = "1"
                }
        }else{
                df1[i,24] = "0"
        }
}

dplyr::cumsum

## Subsetting
df1$notlocal = df1$clicks-df1$localClicks
df2 <- df1 %>% group_by(jobId) %>% summarise(mean.clicks = mean(clicks), 
                                             mean.localclicks = mean(localClicks),
                                             mean.notlocal = mean(notlocal),
                                             maxdays = which.max(jobAgeDays) + 1,
                                             totalclicks = mean.clicks*maxdays,
                                             total.local = mean.localclicks*maxdays,
                                             total.notlocal = mean.notlocal*maxdays) 

df3 <- df1 %>% group_by(jobId) %>% slice(which.max(jobAgeDays))

df5 <- left_join(df3, df2, by = "jobId")
write.csv(df5,file = "bagel.csv",row.names = F)
#Create Bagel.
bagel <- fread("bagel.csv")

#summary for clicks
summary(bagel$mean.clicks)
summary(bagel$mean.localclicks)
summary(bagel$mean.notlocal)

hist(bagel$mean.clicks, xlim = c(0,150),breaks =   1000)

# high low medium
quantile(bagel$mean.clicks,c(0.33,0.66))
quantile(bagel$mean.localclicks,c(0.33,0.66))
quantile(bagel$mean.notlocal,c(0.33,0.66))

#total clicks
bagel$totallevel[bagel$mean.clicks <= 15.44444] <- "Low"
bagel$totallevel[bagel$mean.clicks > 15.44444 & bagel$mean.clicks < 22.07143] <- "Medium"
bagel$totallevel[bagel$mean.clicks >= 22.07143] <- "High"

table(bagel$totallevel)
#local clicks
bagel$locallevel[bagel$mean.localclicks <= 1.275000] <- "Low"
bagel$locallevel[bagel$mean.localclicks > 1.275000 & bagel$mean.localclicks < 3.217391] <- "Medium"
bagel$locallevel[bagel$mean.localclicks >= 3.217391] <- "High"
#non-local clicks
bagel$notlocal_level[bagel$mean.notlocal <= 13.37500] <- "Low"
bagel$notlocal_level[bagel$mean.notlocal > 13.37500 & bagel$mean.notlocal < 18.40909] <- "Medium"
bagel$notlocal_level[bagel$mean.notlocal >= 18.40909] <- "High"
#experience level 
bagel$experienceLevel <- "NA"
bagel$experienceLevel[bagel$experienceRequired > 5] <- "High"
bagel$experienceLevel[bagel$experienceRequired  > 2 & bagel$experienceRequired <= 5  ] <- "Medium"
bagel$experienceLevel[bagel$experienceRequired <= 2] <- "Low"
head(bagel$experienceLevel,100)

plot(table(bagel$avgOverallRating),xlim = c(0,5))
# ordinal 
library("MASS")
chisq.test(bagel$experienceLevel,bagel$educationRequirements)
chisq.test(bagel$experienceLevel,bagel$estimatedSalary)
chisq.test()

low <- bagel$locallevel[bagel$locallevel == "Low"]
length(bagel$locallevel[bagel$locallevel == "Medium"])
length(bagel$locallevel[bagel$locallevel == "High"])

bagel$locallevel <- as.factor(bagel$locallevel)
ordinal <- polr(bagel$locallevel ~ bagel$experienceLevel + bagel$educationRequirement +
                        bagel$normTitleCategory + bagel$supervisingJob + bagel$licenseRequiredJob, Hess = TRUE)
summary(ordinal)
coeffs <- coef(summary(ordinal))
p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2
cbind(coeffs, "p value" = round(p,3))

# random forest

#install.packages("reprtree")
library("randomForest")
library("reprtree")
rf = randomForest(locallevel ~   licenseRequiredJob + estimatedSalary  ,
                           data=abc,
                        importance=T,
                        do.trace=TRUE,
                        na.rm = T,
                        ntree=50)
colnames(abc)
rf$importance
importance(rf, type=2)
getTree(rf,1,labelVar = T)

usbagel$supervisingJob[usbagel$supervisingJob == "" ]<- NA
bagel$supervisingJob <- as.factor
normTitleCategory + supervisingJob + licenseRequiredJob

# us.bagel
usbagel <- bagel[bagel$country == "US",]
usbagel <- usbagel[usbagel$educationRequirements != "", ]
usbagel <- usbagel[usbagel$supervisingJob != "", ]

usbagel$experienceLevel <- "NA"
usbagel$experienceLevel[usbagel$experienceRequired==0] <- "A"
usbagel$experienceLevel[usbagel$experienceRequired>0] <- "B"
usbagel$experienceLevel[usbagel$experienceRequired>2] <- "C"
usbagel$experienceLevel[usbagel$experienceRequired>5] <- "D"
usbagel$experienceLevel[usbagel$experienceRequired>15] <- "E"

str(usbagel)
usbagel$licenseRequiredJob <- as.factor(usbagel$licenseRequiredJob)
sum(is.na(usbagel$supervisingJob))
sum(is.na(usbagel$licenseRequiredJob))
sum(is.na(usbagel$estimatedSalary))
sum(is.na(usbagel$locallevel))

load("/Users/junhyukjang/Downloads/abc.RData")
#divide data
library(dplyr)
library("ggplot2")
levels(abc$experienceLevel)
expA = abc[abc$experienceLevel == "A",]
expB = abc[abc$experienceLevel == "B",]
expC = abc[abc$experienceLevel == "C",]
expD = abc[abc$experienceLevel == "D",]
expE = abc[abc$experienceLevel == "E",]
expNA = abc[abc$experienceLevel == "NA",]

abc$experienceLevel <- as.character(abc$experienceLevel)
abc$experienceLevel[is.na(abc$experienceLevel)] <- "NA"
str(abc)
abc$experienceLevel <- as.factor(abc$experienceLevel)
#all groups
ggplot(abc,aes(x=ratio.local,group=experienceLevel,fill=experienceLevel))+
        geom_histogram(position="identity",alpha=0.3,binwidth=0.05)+theme_bw()

par(mfrow=c(4,1))
#all types
ggplot(abc,aes(x=ratio.local, fill=experienceLevel))+geom_histogram()+facet_grid(~experienceLevel)+theme_bw() + 
        facet_wrap(~experienceLevel)

#A
ggplot(expA,aes(x=ratio.local, fill=experienceLevel))+geom_histogram()+facet_grid(~experienceLevel)+theme_bw()
        
#B
ggplot(expB,aes(x=ratio.local, fill=experienceLevel))+geom_histogram()+facet_grid(~experienceLevel)+theme_bw()

#C
ggplot(expC,aes(x=ratio.local, fill=experienceLevel))+geom_histogram()+facet_grid(~experienceLevel)+theme_bw()

#D
ggplot(expD,aes(x=ratio.local, fill=experienceLevel))+geom_histogram()+facet_grid(~experienceLevel)+theme_bw()
#E
ggplot(expE,aes(x=ratio.local, fill=experienceLevel))+geom_histogram()+facet_grid(~experienceLevel)+theme_bw()
# NA
ggplot(expNA,aes(x=ratio.local, fill=experienceLevel))+geom_histogram()+facet_grid(~experienceLevel)+theme_bw()

sort(abc$normTitleCategory,decreasing = T)

# #
management <- abc[abc$normTitleCategory=="management",]
mednurse <- abc[abc$normTitleCategory=="mednurse",]
techsoftware <- abc[abc$normTitleCategory=="techsoftware",]
sales <- abc[abc$normTitleCategory=="sales",]
install <- abc[abc$normTitleCategory=="instsall",]
admin <- abc[abc$normTitleCategory=="admin",]
driver <- abc[abc$normTitleCategory=="driver",]
retail <- abc[abc$normTitleCategory=="retail",]
accounting <- abc[abc$normTitleCategory=="accounting",]
food <- abc[abc$normTitleCategory=="food",]

write.csv(abc,file = "abc.csv",row.names = F)

#######
setwd("/Users/junhyukjang/Desktop/2018 DataFest")
tab <- fread("tab.csv")
tab$bur <- rep(NA,nrow(tab))
tab$bur <- as.numeric(tab$bur)
colnames(tab)
tab[tab$normTitleCategory=="tech",39] <- 6
tab[tab$normTitleCategory=="techhelp",39] <- 6
tab[tab$normTitleCategory=="techinto",39] <- 6

unique(tab$normTitleCategory)

# 7
tab$bur
tab[tab$normTitleCategory=="care",39] <- 7
tab[tab$normTitleCategory=="therapy",39] <- 7
tab[tab$normTitleCategory=="socialscience",39] <- 7
tab[tab$normTitleCategory=="childcare",39] <- 7
tab[tab$normTitleCategory=="education",39] <- 7
tab[tab$normTitleCategory=="meddr",39] <- 7
tab[tab$normTitleCategory=="meddental",39] <- 7
tab[tab$normTitleCategory=="mednurse",39] <- 7
tab[tab$normTitleCategory=="pharmacy",39] <- 7
# 1
tab[tab$normTitleCategory=="mining",39] <- 1
tab[tab$normTitleCategory=="construction",39] <- 1
# 2 
tab[tab$normTitleCategory=="manufacture",39] <- 2
#3
tab[tab$normTitleCategory=="aviation",39] <- 3
tab[tab$normTitleCategory=="customer",39] <- 3
tab[tab$normTitleCategory=="driver",39] <- 3
tab[tab$normTitleCategory=="transport",39] <- 3
tab[tab$normTitleCategory=="retail",39] <- 3
tab[tab$normTitleCategory=="warehouse",39] <- 3
#4
tab[tab$normTitleCategory=="math",39] <- 4
tab[tab$normTitleCategory=="media",39] <- 4

tab[tab$normTitleCategory=="finance",39] <- 5
tab[tab$normTitleCategory=="insurance",39] <- 5
tab[tab$normTitleCategory=="realestate",39] <- 5
tab[tab$normTitleCategory=="legal",39] <- 5


tab[tab$normTitleCategory=="tech",39] <- 6
tab[tab$normTitleCategory=="techhelp",39] <- 6
tab[tab$normTitleCategory=="techinto",39] <- 6
tab[tab$normTitleCategory=="techsoftware",39] <- 6
tab[tab$normTitleCategory=="install",39] <- 6
tab[tab$normTitleCategory=="accounting",39] <- 6
tab[tab$normTitleCategory=="arch",39] <- 6
tab[tab$normTitleCategory=="management",39] <- 6
tab[tab$normTitleCategory=="hr",39] <- 6
tab[tab$normTitleCategory=="admin",39] <- 6
tab[tab$normTitleCategory=="marketing",39] <- 6
tab[tab$normTitleCategory=="project",39] <- 6
tab[tab$normTitleCategory=="science",39] <- 6
tab[tab$normTitleCategory=="veterinary",39] <- 6
tab[tab$normTitleCategory=="engchem",39] <- 6
tab[tab$normTitleCategory=="engcivil",39] <- 6
tab[tab$normTitleCategory=="engelectric",39] <- 6
tab[tab$normTitleCategory=="engid",39] <- 6
tab[tab$normTitleCategory=="engmech",39] <- 6
tab[tab$normTitleCategory=="medtech",39] <- 6
tab[tab$normTitleCategory=="project",39] <- 6
tab[tab$normTitleCategory=="sales",39] <- 6

tab[tab$normTitleCategory=="care",39] <- 7
tab[tab$normTitleCategory=="therapy",39] <- 7
tab[tab$normTitleCategory=="socialscience",39] <- 7
tab[tab$normTitleCategory=="childcare",39] <- 7
tab[tab$normTitleCategory=="education",39] <- 7
tab[tab$normTitleCategory=="meddr",39] <- 7
tab[tab$normTitleCategory=="meddental",39] <- 7
tab[tab$normTitleCategory=="mednurse",39] <- 7
tab[tab$normTitleCategory=="pharmacy",39] <- 7


tab[tab$normTitleCategory=="food",39] <- 8
tab[tab$normTitleCategory=="customer",39] <- 8
tab[tab$normTitleCategory=="hospitality",39] <- 8
tab[tab$normTitleCategory=="personal",39] <- 8
tab[tab$normTitleCategory=="arts",39] <- 8
tab[tab$normTitleCategory=="sports",39] <- 8

tab[tab$normTitleCategory=="sanitation",39] <- 9
tab[tab$normTitleCategory=="personal",39] <- 9

tab[tab$normTitleCategory=="protective",39] <- 10
tab[tab$normTitleCategory=="military",39] <- 10

colnames(tab)
tab2 <- tab[,c(1,39)]
tab2 <- na.omit(tab2)
tab3 <- tab[,c(1,39)]
summary(tab3)
tab3 <- na.omit(tab3)

is.na(tab2)
str(tab2)

m1 <- tab2[tab2$date < "2016-12-1",]













