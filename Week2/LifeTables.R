life_table_hwk2 <- read.csv("~/GitHub/PH207x/Data/life_table_hwk2.csv")

X1950.X1952 <- matrix(nrow = 3, ncol = 3)

#ndx Number of people dying between ages x and x + n
X1950.X1952[,1] <- (embed(life_table_hwk2[,2],2) - embed(life_table_hwk2[,2],2)[,1])[,2]
#nqx Probability of dying between ages x and x + n
X1950.X1952[,2] <- X1950.X1952[,1]/life_table_hwk2[1:3,2]
#npx Probability of surviving between ages x and x + n
X1950.X1952[,3] <- 1 - X1950.X1952[,2]

X1990.X1992 <- matrix(nrow = 3, ncol = 3)

#ndx Number of people dying between ages x and x + n
X1990.X1992[,1] <- (embed(life_table_hwk2[,3],2) - embed(life_table_hwk2[,3],2)[,1])[,2]
#nqx Probability of dying between ages x and x + n
X1990.X1992[,2] <- X1990.X1992[,1]/life_table_hwk2[1:3,3]
#npx Probability of surviving between ages x and x + n
X1990.X1992[,3] <- 1 - X1990.X1992[,2]

#3. Define the absolute survival increase over the 40 year span as p1−p2, 
#where p1 is the chance of surviving from age x to age x+n in 1990-1992 
#and p2 is the chance of surviving from age x to age x+n in 1950-1952. 
#Which age group has the greatest absolute survival increase?

abs_surv_inc <- X1990.X1992[,3] -X1950.X1952[,3]

#4. Define the relative survival increase over the 40 year span as (p1−p2)p2, 
#where p1 is the chance of surviving from age x to age x+n in 1990-1992 and 
#p2 is the chance of surviving from age x to age x+n in 1950-1952. 
#Which age group has the greatest relative survival increase?

rel_surv_inc <- abs_surv_inc/X1950.X1952[,3]

data.frame(abs_surv_inc, rel_surv_inc)