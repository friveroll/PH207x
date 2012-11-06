gen bmi1high = .
replace bmi1high = 1 if bmi1 > 25 & bmi1 < .
replace bmi1high  = 0 if bmi1 <= 25

gen bmi2high = .
replace bmi2high = 1 if bmi2 > 25 & bmi2 < .
replace bmi2high  = 0 if bmi2 <= 25

tabulate bmi1high bmi2high

recode bmi1 (min/18.5=1) (18.5/25=2) (25/30=3) (30/max=4),gen(bmigrp)
ta bmigrp,gen(bmig)
ir anychd bmig1 timechd
ir anychd bmig2 timechd
ir anychd bmig3 timechd
ir anychd bmig4 timechd

generate highbp1=.
replace highbp1=1 if (sysbp1>=140 | diabp1 >= 90)
replace highbp1=0 if (sysbp1<140 & diabp1<90)

ir anychd highbp1 timechd
