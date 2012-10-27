data2 <- read.dta("Data/lifetable.dta",convert.factors = TRUE ,missing.type = TRUE)

data2

50_52_dying

attach(life_table_hwk2)

X50to52dying <- NA

for (i in 1:3) X50to52dying[i] <- X1950.1952[i] - X1950.1952[i+1]

X90to92dying <- NA

for (i in 1:3) X90to92dying[i] <- X1990.1992[i] - X1990.1992[i+1]

for (i in 1:3) print(X50to52dying[i]/ X1950.1952[i])

