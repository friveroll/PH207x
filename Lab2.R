if (!"foreign" %in% installed.packages())
{
  install.packages("foreign", dependencies = TRUE)
}
library("foreign") #needed for read.dta function

### Import Stata Data ###
data <- read.dta("Data/fhs.dta",convert.factors = TRUE ,missing.type = TRUE)


if (!"Epi" %in% installed.packages())
{
  install.packages("Epi", dependencies = TRUE)
}

library("Epi") 

attach(data)

# tab1 cursmoke* , missing

stat.table(factor(cursmoke1, exclude=NULL), contents = 
  list(N=count(), '(%)'=percent(factor(cursmoke1, exclude=NULL))), data=data, margin = T)

stat.table(factor(cursmoke2, exclude=NULL), contents = 
  list(N=count(), '(%)'=percent(factor(cursmoke2, exclude=NULL))), data=data, margin = T)

stat.table(factor(cursmoke3, exclude=NULL), contents = 
  list(N=count(), '(%)'=percent(factor(cursmoke3, exclude=NULL))), data=data, margin = T)


#gen cursmokenotmiss = cursmoke1<. & cursmoke2<. & cursmoke3<.

cursmokenotmiss <- na.omit(data.frame(cursmoke1, cursmoke2, cursmoke3)

#tab1 cursmoke1 cursmoke2 cursmoke3 if cursmokenotmiss==1, missing

stat.table(cursmoke1, contents = list(N=count(), '(%)'=percent(cursmoke1)), data=cursmokenotmiss, margin = T)                           

stat.table(cursmoke2, contents = list(N=count(), '(%)'=percent(cursmoke2)), data=cursmokenotmiss, margin = T)
                           
stat.table(cursmoke3, contents = list(N=count(), '(%)'=percent(cursmoke3)), data=cursmokenotmiss, margin = T)
                           


                             
stat.table(list(cursmoke1,cursmoke2), data=data, margin=T))

stat.table(list(factor(cursmoke1),factor(cursmoke2)), data=data, contents = percent(cursmoke2), margin=c(T,T))


stat.table(list(cursmoke1,cursmoke2), data=data, margin=c(T,F))

length(cursmoke2[cursmoke1 == "Yes" & cursmoke2 == "Yes"])

cursmoke2 <- factor(cursmoke2, exclude= NULL)
cursmoke1 <- factor(cursmoke2, exclude= NULL)


length(subset(cursmoke2, cursmoke1 == "Yes"))
length(subset(cursmoke2, cursmoke1 == "No"))


subset(cursmoke1, cursmoke2[cursmoke2 == NA])

cursmoke_2Vs1_No_Yes  <- 0

if(data$cursmoke2 == "No" & data$cursmoke1 == "Yes") cursmoke_2Vs1_No_Yes + 1
#          2   
# 1    No   Yes
# No    1    2

# Yes   3    4

cursmoke_2Vs1 <- NA # initialize cursmoke_2Vs1
cursmoke_2Vs1 [data$cursmoke2 == "No" & data$cursmoke1 == "No"] <- 1
cursmoke_2Vs1 [data$cursmoke2 == "Yes" & data$cursmoke1 == "No"] <- 2
cursmoke_2Vs1 [is.na(data$cursmoke2) & data$cursmoke1 == "No"] <- 3
cursmoke_2Vs1 [data$cursmoke2 == "No" & data$cursmoke1 == "Yes"] <- 4
cursmoke_2Vs1 [data$cursmoke2 == "Yes" & data$cursmoke1 == "Yes"] <- 5
cursmoke_2Vs1 [is.na(data$cursmoke2) & data$cursmoke1 == "Yes"] <- 6



cursmoke_2Vs1.m <- as.matrix(summary(factor(cursmoke_2Vs1)))

cursmoke1_Vs_cursmoke2 <- matrix(nrow=12, ncol=4)
#No Frq
cursmoke1_Vs_cursmoke2[1,1:4] <- c(cursmoke_2Vs1.m[1:3], cursmoke_2Vs1.m[1]+cursmoke_2Vs1.m[2]+cursmoke_2Vs1.m[3])
#Yes Frq
cursmoke1_Vs_cursmoke2[5,1:4] <- c(cursmoke_2Vs1.m[4:6], cursmoke_2Vs1.m[4]+cursmoke_2Vs1.m[5]+cursmoke_2Vs1.m[6])
#Total Frq
cursmoke1_Vs_cursmoke2[9,1:4] <- cursmoke1_Vs_cursmoke2[1,1:4] + cursmoke1_Vs_cursmoke2[5,1:4]

#No row %
for (i in 1:4){cursmoke1_Vs_cursmoke2[2,i] <- c(cursmoke1_Vs_cursmoke2[1,i]/cursmoke1_Vs_cursmoke2[1,4]*100)}

#No column %
for (i in 1:4){cursmoke1_Vs_cursmoke2[3,i] <- c(cursmoke1_Vs_cursmoke2[1,i]/cursmoke1_Vs_cursmoke2[9,i]*100)}

#No cell %
for (i in 1:4){cursmoke1_Vs_cursmoke2[4,i] <- c(cursmoke1_Vs_cursmoke2[1,i]/cursmoke1_Vs_cursmoke2[9,4]*100)}

#Yes row%
for (i in 1:4){cursmoke1_Vs_cursmoke2[6,i] <- c(cursmoke1_Vs_cursmoke2[5,i]/cursmoke1_Vs_cursmoke2[5,4]*100)}

#Yes column%
for (i in 1:4){cursmoke1_Vs_cursmoke2[7,i] <- c(cursmoke1_Vs_cursmoke2[5,i]/cursmoke1_Vs_cursmoke2[9,i]*100)}

#Yes cell%
for (i in 1:4){cursmoke1_Vs_cursmoke2[8,i] <- c(cursmoke1_Vs_cursmoke2[5,i]/cursmoke1_Vs_cursmoke2[9,4]*100)}

#Total row%
for (i in 1:4){cursmoke1_Vs_cursmoke2[10,i] <- c(cursmoke1_Vs_cursmoke2[9,i]/cursmoke1_Vs_cursmoke2[9,4]*100)}

#Total column%
for (i in 1:4){cursmoke1_Vs_cursmoke2[11,i] <- c(cursmoke1_Vs_cursmoke2[9,i]/cursmoke1_Vs_cursmoke2[9,i]*100)}

#Total cell%
for (i in 1:4){cursmoke1_Vs_cursmoke2[12,i] <- c(cursmoke1_Vs_cursmoke2[9,i]/cursmoke1_Vs_cursmoke2[9,4]*100)}

cursmoke1_Vs_cursmoke2 <- round(as.data.frame(cursmoke1_Vs_cursmoke2),2)

names(cursmoke1_Vs_cursmoke2) <- c("No", "Yes", "NA", "Total")

cursmoke1_Vs_cursmoke2