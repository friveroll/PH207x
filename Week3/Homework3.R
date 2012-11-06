#######################
# PROBABILITY AND BMI #
#######################
library("foreign")
data <- read.dta("https://dl.dropbox.com/u/4828275/fhs.dta",
                 convert.factors = TRUE,
                 missing.type = TRUE)
attach(data)

bmi1high <- NA
bmi1high[bmi1 > 25 & !is.na(bmi1)] <- 1
bmi1high[bmi1 <= 25] <- 0

bmi2high <- NA
bmi2high[bmi2 > 25 & !is.na(bmi2)] <- 1
bmi2high[bmi2 <= 25] <- 0

library("epicalc")
by (bmi1high, bmi2high, tab1)

##########################################
# PROBABILITY OF AGE AND SMOKING EVENTS. #
##########################################

age_smoking <- read.csv("https://dl.dropbox.com/u/4828275/age_smoking.csv")
#age_smoking <- read.csv("~/Documents/GitHub/PH207x/Week3/age_smoking.csv")

# 1. Eight categories representing age and smoking status groups are shown 
# in the table above. Are these groups:?
# A: Mutually Exclusive & Exhaustive

sum(age_smoking[,2]) + sum(age_smoking[,3])

# 2. What is the probability of A?
sum(age_smoking[,3])

# 3. What is the probability the complement of B?
1 - sum(age_smoking[4,c(2,3)])

# 4. What is the probability that a randomly selected individual is a non-smoker
# who is younger than 60 years old at exam 1?
sum(age_smoking[1:3,2])


#####################
# Titanic Survival. #
#####################

#titanic <- read.csv("~/Documents/GitHub/PH207x/Week3/titanic.csv")
titanic <- read.csv("https://dl.dropbox.com/u/4828275/titanic.csv")

# 1. All Women
Women <- subset(titanic, Age.Sex == "Women")
sum(subset(Women, Survival.Status == "Survived")[,4])/sum(Women[,4])

# 2. All Children
Child <- subset(titanic, Age.Sex == "Child")
sum(subset(Child, Survival.Status == "Survived")[,4])/sum(Child[,4])

# 3. All Women or Children
Child_Women <- rbind(Child, Women)
sum(subset(Child_Women, Survival.Status == "Survived")[,4])/sum(Child_Women[,4])

# 4. All First Class Passengers
First_class <- subset(titanic, Passenger.Class == "First")
sum(subset(First_class, Survival.Status == "Survived")[,4])/sum(First_class[,4])


############################################################
# BMI AND CUMULATIVE INCIDENCE AND INCIDENCE RATE OF DEATH #
############################################################

#bmi_inc <- read.csv("~/Documents/GitHub/PH207x/Week3/bmi_inc.csv")
bmi_inc <- read.csv("https://dl.dropbox.com/u/4828275/bmi_inc.csv")

C_I <- bmi_inc[,3]/bmi_inc[,2]

I_R <- bmi_inc[,3]/bmi_inc[,4] * 1000

data.frame("BMI"=bmi_inc[,1], "Cummulative Inc"=C_I, "Inc Rate"=I_R)

############################################################
# BMI AND CUMULATIVE INCIDENCE AND INCIDENCE RATE OF DEATH #
############################################################

if (!"epiR" %in% installed.packages())
{
  install.packages("epiR", dependencies = TRUE)
}
library(epiR)

underwt <- ifelse(bmi1<18.5, c(1), c(0)) 
normalwt <- ifelse(bmi1>=18.5 & bmi1 <= 25, c(1), c(0))
overwt <- ifelse(bmi1>25 & bmi1 <= 30, c(1), c(0))
obese <- ifelse(bmi1>30 & !is.na(bmi1), c(1), c(0))

bmi_cat <- c("underwt", "normalwt", "overwt", "obese")
for (i in 1:4)
{
	eval(parse(text = paste(bmi_cat[i],'_t <- matrix(nrow =2, ncol=2)', sep="")))
	eval(parse(text = paste(bmi_cat[i],'_t[,1] <- tapply(death=="Yes",',bmi_cat[i],', sum)', sep="")))
	eval(parse(text = paste(bmi_cat[i],'_t[,2] <- tapply(timedth,',bmi_cat[i],', sum)', sep="")))
	eval(parse(text = paste(bmi_cat[i],'_t <- rbind(',bmi_cat[i],'_t[2,]',',',bmi_cat[i],'_t[1,])', sep="")))
  cat("\n\n")
  print(bmi_cat[i])
  eval(parse(text = paste('epi.2by2(',bmi_cat[i],'_t, method = "cohort.time", conf.level = 0.95, units = 1000, homogeneity = "breslow.day", verbose = F)', sep="")))
}

#########################
# BMI AND CHD INCIDENCE #
#########################

for (i in 1:4)
{
	eval(parse(text = paste(bmi_cat[i],
                          '_t2 <- matrix(nrow =2, ncol=2)', sep="")))
	eval(parse(text = paste(bmi_cat[i],
                          '_t2[,1] <- tapply(anychd=="Yes",',
                          bmi_cat[i],', sum)', sep="")))
	eval(parse(text = paste(bmi_cat[i],'_t2[,2] <- tapply(timechd,'
                          ,bmi_cat[i],', sum)', sep="")))
	eval(parse(text = paste(bmi_cat[i],'_t2 <- rbind(',
                          bmi_cat[i],'_t2[2,]',
                          ',',bmi_cat[i],'_t2[1,])', sep="")))
  cat("\n\n")
  print(bmi_cat[i])
  eval(parse(text = paste('epi.2by2(',bmi_cat[i],'_t2, method = "cohort.time", conf.level = 0.95, units = 1000, homogeneity = "breslow.day", verbose = F)', sep="")))
}


#########################################
# HIGH BLOOD PRESSURE AND CHD INCIDENCE #
#########################################

highbp1 <- NULL
highbp1[sysbp1>=140 | diabp1 >= 90] <- 1
highbp1[sysbp1<140 & diabp1 < 90] <- 0

# Generate the 2 by 2 Table
anychd_highbp1 <- matrix(nrow =2, ncol=2)
# Add column 1 
anychd_highbp1[,1] <- tapply(anychd=="Yes",highbp1,sum)
# Add column 2
anychd_highbp1[,2] <- tapply(timechd,highbp1,sum)

# Now the table look like this
#    Disease +   Time at risk
# Expose -   c   d
# Expose +   a   b

# Get the correct table
anychd_highbp1 <- rbind(anychd_highbp1[2,], anychd_highbp1[1,])

#    Disease +   Time at risk
# Expose +   a   b
# Expose -   c   d

#Use anychd_highbp1 to get Incidence Rate per 1000 persons (units = 1000)
epi.2by2(anychd_highbp1, method = "cohort.time", conf.level = 0.95, 
         units = 1000, homogeneity = "breslow.day", verbose = F)
