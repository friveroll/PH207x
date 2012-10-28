####################################################################
#Frequency. In this question, we examine the reported numbers of   #
#hospitalizations for cardiac events in the United States for each #
#month in the period January 1991 to December 1992.                #
####################################################################

hospitalizations <- read.csv("https://dl.dropbox.com/u/4828275/hospitalizations.csv")

# 1. In the year 1991, what is the relative frequency of 
# hospitalizations in September?

sep1991.rf <- hospitalizations[9,2]/sum(hospitalizations[2])
print(sep1991.rf)

# 2. Is the absolute frequency of hospitalizations in September 
# 1991 greater than the absolute frequency of hospitalizations in 
# September 1992?

sep1991.af <- hospitalizations[9,2]/(sum(hospitalizations[2])+ sum(hospitalizations[4]))
sep1992.af <- hospitalizations[9,4]/(sum(hospitalizations[2])+ sum(hospitalizations[4]))

sep1991.af > sep1992.af

# 3. Restricting to the year 1992, calculate the relative frequency of 
# hospitalizations in September.  Comparing this estimate with the 
# relative frequency calculation in part (a), is the relative frequency 
# of hospitalizations in September higher in 1991 or in 1992?

sep1992.rf <- hospitalizations[9,4]/sum(hospitalizations[4])

if (sep1991.rf < sep1992.rf)
{
  print("1992")
}
if (sep1991.rf > sep1992.rf)
{
  print("1991")
}  

################################################
#                                              #    
#   Generating a table with relative and       #
# absolute frequencies for both 1991 & 1992    #
#                                              #
################################################

rfrq1991 <- rep(NA, nrow(hospitalizations))
rfrq1992 <- rep(NA, nrow(hospitalizations))
tfrq1991 <- rep(NA, nrow(hospitalizations))
tfrq1992 <- rep(NA, nrow(hospitalizations))

for (i in 1:nrow(hospitalizations))
{
  rfrq1991[i] <- hospitalizations[i,2]/sum(hospitalizations[2])
  rfrq1992[i] <- hospitalizations[i,4]/sum(hospitalizations[4])
  tfrq1991[i] <- hospitalizations[i,2]/(sum(hospitalizations[2])+ 
    sum(hospitalizations[4]))
  tfrq1992[i] <- hospitalizations[i,4]/(sum(hospitalizations[2])+ 
    sum(hospitalizations[4]))
}

data.frame(rfrq1991, rfrq1992, tfrq1991, tfrq1992, 
           row.names=c("Jan", "Feb", "Mar", "Apr",
                       "May", "Jun", "Jul", "Aug",
                       "Sep", "Oct", "Nov", "Dec"))

##################################################################
# We will use the Framingham dataset to explore data types,      #
# and graphs in this questions. We will examine the indicator    #
# of prevalent hypertension at exam 1(variable name: prevhyp1).  #
##################################################################

if (!"foreign" %in% installed.packages())
{
  install.packages("foreign", dependencies = TRUE)
}
library("foreign") 

data <- read.dta("https://dl.dropbox.com/u/4828275/fhs.dta")

attach(data)

## 2. How many individuals in the Framingham dataset 
## had prevalent hypertension at exam 1?

summary(prevhyp1)[2]

## 3. What is the relative frequency of prevalent hypertension 
## at exam 1? Express your answer as a proportion of the whole 
## (e.g. 10% would be .10).

as.vector(summary(prevhyp1)[2])/length(prevhyp1)

## 4. Among the individuals with prevalent hypertension at 
## exam 1, how many are female?

prevhyp1_fem <- length(prevhyp1[prevhyp1 == "Yes" & sex1 == "Female"])
print(prevhyp1_fem)

## 5. What proportion of individuals with prevalent hypertension 
## at exam 1 are female? Express your answer as a proportion of 
## the whole (e.g. 10% would be .10).

by(prevhyp1, sex1, summary)

prevhyp1_fem/length(prevhyp1[prevhyp1 == "Yes"])

## 6. Which graph would you use to summarize the distribution of 
## the indicator for prevalent hypertension at exam 1 in the study 
## population?

barplot(table(prevhyp1, sex1)[c(2,4)], 
        col=c("darkblue","red"), 
        ylim=c(0,800), 
        names.arg=c("Male", "Female"), 
        xlab="Sex", 
        ylab="Prevalent hypertension at exam 1")

#####################
## BODY MASS INDEX ##
#####################

## Again using the Framingham dataset, we examine the continuous 
## variable, body mass index (BMI).

## 1. To quickly examine the interquartile range for BMI at 
## exam 1 in the study population, which graph would you use?

boxplot(bmi1~sex1, xlab="Sex", ylab="BMI", main="Body Mass Index at exam 1", col=c("darkblue","red"))

## 2. We say an individual has high BMI at exam 1 if his BMI is greater than 25.
## How many individuals in the dataset have high BMI at exam 1?

length(na.omit(bmi1[bmi1>25]))/length(na.omit(bmi1))

## Out of the 4,415 participants with a BMI measurement at exam 1. What percent 
## had high BMI at exam 1) (an individual has high BMI at exam 1 if his BMI is 
## greater than 25) Express your answer as a proportion of the whole 
## (e.g. 10\% would be .10).

high_bmi1/length(na.omit(bmi1))

## 4. Make a scatter plot of BMI at exam 1 (bmi1) versus BMI at exam 2 (bmi2). 
## In general, higher BMI at exam 1 is associated with a _________ BMI at exam 2.

plot(bmi1~bmi2)
#Higher

## BMI CONTINUED

## 1. What is the mean BMI at exam 1 in the study population?
mean(bmi1, na.rm=T)

## 2. The median BMI at exam 1 is 25.0 in the study population. 
## Comparing the mean and median, do these data suggest that the 
## distribution of BMI at exam 1 is right skewed or left skewed?
## 3. Is the mean BMI at exam 1 higher in males or females?

by(bmi1, sex1, summary)

## 4. Should you compare the mode for BMI at exam 1 in males 
## versus females?

bmi1_by_sex1 <- table(bmi1, sex1) 
subset(bmi1_by_sex1[,1], bmi1_by_sex1[,1]==max(bmi1_by_sex1[,1])) 

subset(bmi1_by_sex1[,2], bmi1_by_sex1[,2]==max(bmi1_by_sex1[,2]))

## 5. Is the IQR for BMI at exam 1 larger in males or females?

IQR(bmi1[sex1=="Female"], na.rm=T) > IQR(bmi1[sex1=="Male"], na.rm=T)

## Now, for the remaining parts of this question, restrict your study 
## population to the subset of participants with BMI measures at 
## exam 1 and exam 2.
## 6. What is the mean change in BMI from exam 1 to exam 2? 
## (Note: Change in BMI is defined as BMI at exam 2 minus BMI at exam1. 
## You need to generate this variable in Stata).

bmi_delta_12 <- na.omit(bmi2 - bmi1)
mean(bmi_delta_12)

## 7. What is the standard deviation of the change in BMI from exam 1 to exam 2?

sd(bmi_delta_12)

## 8. What is the range of changes in BMI from exam 1 to exam 2?

max(bmi_delta_12) - min(bmi_delta_12)

## 9. Assuming that the empirical rule applies in this situation, 
## we expect that 95% of individuals will have a change in BMI 
## between exams 1 and 2 that lies within the interval

c(mean(bmi_delta_12)-2*sd(bmi_delta_12), 
  mean(bmi_delta_12)+2*sd(bmi_delta_12))