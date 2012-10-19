######## Exploring the data #############

if (!"foreign" %in% installed.packages())
{
  install.packages("foreign", dependencies = TRUE)
}
library("foreign") #needed for read.dta function

### Import Stata Data ###
data <- read.dta("Data/fhs.dta",convert.factors = TRUE ,missing.type = TRUE)


### Attach the Data dataframe ###
attach(data)

### Order data alphabetically ###
data <- data[order(colnames(data))]

#Log files
?sink #(needs to print both input and output)

### Look at data ###
names(data)
head(data)

### Summarize variables ###
summary(sysbp1)

#Brute force
mean(sysbp1)
sd(sysbp1)
range(sysbp1)
length(sysbp1)
length(na.omit(sysbp1))
quantile(sysbp1)

#Using an external package

if (!"Hmisc" %in% installed.packages())
{
  install.packages("Hmisc", dependencies = TRUE)
}
library("Hmisc")
describe(sysbp1)   # describe is similar to summarize in Stata
describe(glucose1)

### Create subset ###
glucose1.Female <- glucose1[sex1=="Female"] # or data$glucose1[data$sex1=="Female"]
diabetes1.female <- diabetes1[sex1=="Female"]

#glucose1_women <- subset(data, sex1 == "Female", c(sex1, glucose1))
#diabetes1_women <- subset(data, sex1 == "Female", c(sex1, diabetes1))

agecat1 <- c(rep(NA, length(age1)))

for (i in 1:length(age1))
{
  if (age1[i] <= 39)
  {
    agecat1[i] <- 1
  }
  else if (age1[i] >= 40 & age1[i] < 50)
  {
    agecat1[i] <- 2
  }
  else if (age1[i] >= 50 & age1[i] < 60)
  {
    agecat1[i] <- 3
  }
  else if (age1[i] >= 60 & age1[i] <= 70)
  {
    agecat1[i] <- 4
  }
}

agecat1 <- factor(agecat1, labels = c("30-39", "40-49", "50-59", "60-70"))

agecat1.tbl <- table(agecat1)
agecat1.tbl
prop.table(agecat1.tbl)
cumsum(agecat1.tbl)
cumsum(agecat1.tbl)/margin.table(agecat1.tbl)
########Cleaning data#############

# Create new variable (squaring age1 variable to agesq1)
agesq1 <- age1^2
# Describing the new variables
describe(data.frame(age1, agesq1))

# Creating Variable labels (Using Hmisc package)
attributes(data)$var.labels <- NA

attributes(data)$var.labels[
  which(attributes(data)$names == "agecat1")]  <- "Age Categories"

# Create age groups using the cut function
# It takes a numerix vector (x) and cuts it based on the specified breaks.
# This function creates labels such as (0,40] based on the input, but since we want
# the numbers 1:4 as labels we simply specify labels=FALSE. Since the breaks are specified
# as upper open limits (i.e. break 40 means age1<40), we use right=FALSE
data$agecat1 <- cut(x=data$age1, breaks=c(0,40,50,60,100), labels=FALSE, right=FALSE)

# Check to verifiy recoding was successful (compare age1 and agecat1 from 50 random records)
View(data[sample(1:nrow(data), 50), c('agecat1', 'age1')])

# View tabulate (as in Stata)
mytable <- table(agecat1)          # Frequency table
mytable
prop.table(mytable)                     # Frequency proportions
cumsum(mytable)                         # Cummulative Frequency
cumsum(mytable)/margin.table(mytable)   # Cummulative Frequency proportions

# Create Value labels for agecat1
data$agecat1 <- factor(data$agecat1,
                       levels = c(1,2,3,4),
                       labels = c("0-39", "40-49", "50-59", "60-100"))

########  Graphing  ######## 

### Boxplots ###
# Make a Boxplot of total cholesterol at visit 1
boxplot(data$totchol1)

# Split totchol1 data into 2 boxplots based on sex 
boxplot(data$totchol1 ~ data$sex1)

### Histograms ###
# Make a histogram of totchol1
hist(data$totchol1)

# Make a histogram of total choesterol for females with prevalent CHD
hist(data[data$sex1 == "Female" & data$prevchd1 == "Yes", "totchol1"])

# Splitting histograms is a bit trikier in R. There are few options
# Option 1 - plot histograms in the same window, but on separate and not necessarily
# identical axes
par(mfrow = c(1,2)) # Allows 2 plots to be arranged horisontally in the same window
hist(data[data$sex1 == "Female", "totchol1"])
hist(data[data$sex1 == "Male", "totchol1"])
par(mfrow = c(1,1)) # Reset plotting window

# Option 2 - install NCStats package, which allows using formula in histogram, 
# much like boxplot above
# NCStats is likely to require a few dependencies, which you can install manually using
# install.packages
if (!"NCStats" %in% installed.packages())
{
  source("http://www.rforge.net/NCStats/InstallNCStats.R")
}

library(NCStats)
# Plot Histograms separated by Level
hist(data$totchol1~data$sex1)

# Option 3 - we can plot both histograms on the same axes, using different colours for levels
hist(data[data$sex1 == "Female", "totchol1"], col = "blue")
hist(data[data$sex1 == "Male", "totchol1"], add = TRUE, col = "red")

### Scatterplot ###
# Make a scatter plot with bmi1 on the x-axis and totchol1 on the y-axis
plot(data$bmi1, data$totchol1)

# Plot bmi1 vs totchol for Females
plot(data[data$sex1 == "Female", "bmi1"], data[data$sex1 == "Female", "totchol1"])

# 2 scatterplots
# Plot bmi1 vs totchol for Males and Females on the same graph using different colours
plot(data[data$sex1 == "Female", "bmi1"], data[data$sex1 == "Female", "totchol1"], col = "blue")
points(data[data$sex1 == "Male", "bmi1"], data[data$sex1 == "Male", "totchol1"], col = "red")

# Plot bmi1 vs totchol for Males and Females on two separate graphs
par(mfrow = c(1,2)) # Allows 2 plots to be arranged horisontally in the same window
plot(data[data$sex1 == "Female", "bmi1"], data[data$sex1 == "Female", "totchol1"])
plot(data[data$sex1 == "Male", "bmi1"], data[data$sex1 == "Male", "totchol1"])
par(mfrow = c(1,1)) # Reset plotting window

