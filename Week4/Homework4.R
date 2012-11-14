#####################
# Titanic Survival. #
#####################

titanic <- read.csv("~/Documents/GitHub/PH207x/Week3/titanic.csv")
#titanic <- read.csv("https://dl.dropbox.com/u/4828275/titanic.csv")

# All Women or Children
Child_Women <- subset(titanic, Age.Sex == "Women" | Age.Sex == "Child")
Child_Women_R1 <- sum(subset(Child_Women, Survival.Status == "Survived")[,4])/sum(Child_Women[,4])

# All man
Man <- subset(titanic, Age.Sex == "Man")
Man_R0 <- sum(subset(Man, Survival.Status == "Survived")[,4])/sum(Man[,4])

# Risk Ratio R1 / R0
Child_Women_R1/Man_R0

#First Class Risk Ratio
Child_Women_1st <- subset(Child_Women, Passenger.Class == "First")
Child_Women_1st_R1 <- sum(subset(Child_Women_1st, Survival.Status == "Survived")[,4])/sum(Child_Women_1st[,4])

Man_1st <- subset(Man, Passenger.Class == "First")
Man_1st_R0 <- sum(subset(Man_1st, Survival.Status == "Survived")[,4])/sum(Man_1st[,4])

Child_Women_1st_R1/Man_1st_R0

#Second Class Risk Ratio
Child_Women_2nd <- subset(Child_Women, Passenger.Class == "Second")
Child_Women_2nd_R1 <- sum(subset(Child_Women_2nd, Survival.Status == "Survived")[,4])/sum(Child_Women_2nd[,4])

Man_2nd <- subset(Man, Passenger.Class == "Second")
Man_2nd_R0 <- sum(subset(Man_2nd, Survival.Status == "Survived")[,4])/sum(Man_2nd[,4])

Child_Women_2nd_R1/Man_2nd_R0

#Third Class Risk Ratio
Child_Women_3th <- subset(Child_Women, Passenger.Class == "Third")
Child_Women_3th_R1 <- sum(subset(Child_Women_3th, Survival.Status == "Survived")[,4])/sum(Child_Women_3th[,4])

Man_3th <- subset(Man, Passenger.Class == "Third")
Man_3th_R0 <- sum(subset(Man_3th, Survival.Status == "Survived")[,4])/sum(Man_3th[,4])

Child_Women_3th_R1/Man_3th_R0

#####

BP <- read.csv("https://dl.dropbox.com/u/4828275/BP.csv", header = T)
#BP <- read.csv("~/Documents/GitHub/PH207x/Week4/BP.csv")

#1 RR II & III Vs I
(sum(BP[2:3,5])/sum(BP[2:3,6]))/(BP[1,5]/BP[1,6])

#2 RR I Vs III

(BP[3,5]/BP[3,6])/(BP[1,5]/BP[1,6])

#3 RR II Vs I
(BP[2,5]/BP[2,6])/(BP[1,5]/BP[1,6])


#############

library("foreign")
data <- read.dta("https://dl.dropbox.com/u/4828275/fhs.dta",
                 convert.factors = TRUE,
                 missing.type = TRUE)

data <- read.dta("C:\\Users\\felipillo\\Documents\\GitHub\\PH207x\\Data\\fhs.dta",
                  convert.factors = TRUE,
                  missing.type = TRUE)

attach(data)

bmi1_obese <- NA 
bmi1_obese[bmi1>=30] <- 1
bmi1_obese[bmi1>=18.5 & bmi1 < 25] <- 0
bmi1_obese <- na.exclude(bmi1_obese)

table <- rbind(table(na.exclude(subset(bmi1_obese, death=="No"))), 
	     table(na.exclude(subset(bmi1_obese, death=="Yes"))))

death1 <- na.exclude(death[bmi1>=30 | (bmi1>=18.5 & bmi1 < 25)])

table <- (table(list(death1, bmi1_obese)))
table[1,1] <- (table(list(death1, bmi1_obese)))[2,2]
table[2,2] <- (table(list(death1, bmi1_obese)))[1,1]
colnames(table) <- c("Disease","Control")
rownames(table) <- c("Exposed","Unexposed")

source("https://dl.dropbox.com/u/4828275/calcma.R")
calcMA(table)



