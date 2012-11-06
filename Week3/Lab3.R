# For epiR we need to build a 2 by 2 frequency table like this one

# 	 Disease +	 Time at risk
# Expose +	 a	 b
# Expose -	 c	 d

# Generate the 2 by 2 Table
table1 <- matrix(nrow =2, ncol=2)
# Add column 1 
table1[,1] <- tapply(death=="Yes", cursmoke1, sum)
# Add column 2
table1[,2] <- tapply(timedth, cursmoke1, sum)

# Now the table look like this
# 	 Disease +	 Time at risk
# Expose -	 c	 d
# Expose +	 a	 b

# Get the correct table
table1 <- rbind(table1[2,], table1[1,])

#Install and Load library epiR
if (!"epiR" %in% installed.packages())
{
  install.packages("epiR", dependencies = TRUE)
}
library(epiR)

#Use table1 to get Incidence Rate per 100 persons (units = 100)
epi.2by2(table1, method = "cohort.time", conf.level = 0.95, 
        units = 100, homogeneity = "breslow.day", verbose = F)