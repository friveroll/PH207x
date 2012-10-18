######## Exploring the data #############

library(package="foreign") #needed for read.dta function

### Import Stata Data ###
data <- read.dta("Week1/fhs.2d92301d751b.dta")

### Order data alphabetically ###
data <- data[order(colnames(data))]

#Log files
?sink #(needs to print both input and output)

### Look at data ###
names(data)
head(data)

### Summarize variables ###
#Brute force
mean(data$sysbp1)
sd(data$sysbp1)
range(data$sysbp1)
length(data$sysbp1)
length(na.omit(data$sysbp1))
quantile(data$sysbp1)

#Using an external package
library(package="Hmisc")
describe(data$sysbp1)   # describe is similar to summarize in Stata
describe(data$glucose1)

### Create subset ###
glucose1_women <- subset(data, sex1 == "Female", c(sex1, glucose1))
diabetes1_women <- subset(data, sex1 == "Female", c(sex1, diabetes1))

########Cleaning data#############

# Create new variable (squaring age1 variable to data$agesq1)
data$agesq1 <- data$age1^2
# Describing the new variables
describe(subset(data, select = c(age1, agesq1)))

# Creating Variable labels (Using Hmisc package)
attributes(data)$var.labels <- NA

attributes(data)$var.labels[
  which(attributes(data)$names == "agecat1")] 
<- "Age Categories"

# Create age groups using the cut function
# It takes a numerix vector (x) and cuts it based on the specified breaks.
# This function creates labels such as (0,40] based on the input, but since we want
# the numbers 1:4 as labels we simply specify labels=FALSE. Since the breaks are specified
# as upper open limits (i.e. break 40 means age1<40), we use right=FALSE
data$agecat1 <- cut(x=data$age1, breaks=c(0,40,50,60,100), labels=FALSE, right=FALSE)

# Check to verifiy recoding was successful (compare age1 and agecat1 from 50 random records)
View(data[sample(1:nrow(data), 50), c('agecat1', 'age1')])

# View tabulate (as in Stata)
mytable <- table(data$agecat1)          # Frequency table
mytable
prop.table(mytable)                     # Frequency proportions
cumsum(mytable)                         # Cummulative Frequency
cumsum(mytable)/margin.table(mytable)   # Cummulative Frequency proportions

# Create Value labels for agecat1
data$agecat1 <- factor(data$agecat1,
                       levels = c(1,2,3,4),
                       labels = c("0-39", "40-49", "50-59", "60-100"))

# Graphing

# Boxplot
# 2 boxplots

# Histogram
# 2 histograms

# Scatterplot
# 2 scatterplots