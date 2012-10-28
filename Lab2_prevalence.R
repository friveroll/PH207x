if (!"foreign" %in% installed.packages())
{
  install.packages("foreign", dependencies = TRUE)
}
library("foreign") #needed for read.dta function

### Import Stata Data ###
data <- read.dta("https://dl.dropbox.com/u/4828275/fhs.dta",convert.factors = TRUE ,missing.type = TRUE)

attach(data)


#Calculate the proportion of people at each visit that report current smok-
#ing (NA+) and the proportion of people at each visit that report current
#smoking among those with data on smoking status at that visit (NA-).
#In this data set, current smoking status us coded as \0 = not current
#smoker, 1= current smoker"

if (!"epicalc" %in% installed.packages())
{
  install.packages("epicalc", dependencies = TRUE)
}
library("epicalc")

tab1(cursmoke1, graph=F, cum.percent = any(is.na(cursmoke1)))
tab1(cursmoke2, graph=F, cum.percent = any(is.na(cursmoke2)))
tab1(cursmoke3, graph=F, cum.percent = any(is.na(cursmoke3)))


#Calculate the proportion of people at each visit that report current 
#smoking among those with data on smoking status at all 3 visits.

cursmokenotmiss <- na.exclude(data.frame(cursmoke1, cursmoke2, cursmoke3))

tab1(cursmokenotmiss$cursmoke1, graph=F)
tab1(cursmokenotmiss$cursmoke2, graph=F)
tab1(cursmokenotmiss$cursmoke3, graph=F)

#Calculate the change in smoking prevalence between the 1st and 2nd visit.
if (!"gmodels" %in% installed.packages())
{
  install.packages("gmodels", dependencies = TRUE)
}
library("gmodels")

with(data, CrossTable(cursmoke1, cursmoke2, missing.include=TRUE, format="SPSS"))

#Calculate the change in smoking prevalence between the 1st and 2nd visit 
#among those with data on smoking status at both visits.

with(data, CrossTable(cursmoke1, cursmoke2, format="SPSS"))
  
#Calculate the prevalence of coronary heart disease (CHD) at visit 1 by 
#categories of cigarettes per day

data$packs1 <- NA # initialize packs1
data$packs1 [data$cigpday1==0] <- 0
data$packs1 [data$cigpday1>=1 & data$cigpday1 <= 20] <- 1
data$packs1 [data$cigpday1>=21 & data$cigpday1 <= 40] <- 2
data$packs1 [data$cigpday1>=41 & !is.na(data$cigpday1)] <- 3

with(data, CrossTable(packs1, prevchd1, format="SPSS"))
