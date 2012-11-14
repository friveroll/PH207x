use "C:\Users\felipillo\Documents\GitHub\PH207x\Data\fhs.dta", clear

gen bmi1_obese =.
replace bmi1_obese=1 if (bmi1>=30 & bmi1<.)
replace bmi1_obese=0 if (bmi1>=18.5 & bmi1 < 25)

cs death bmi1_obese, or

ir anychd bmi1_obese timechd if prevchd1 == 0

gen ob_owt_normal = .
replace ob_owt_normal=1 if ((bmi1>=30 & bmi1<.) | (bmi1>25 & bmi1 <= 30))
replace ob_owt_normal=0 if (bmi1>=18.5 & bmi1 < 25)

ir anychd ob_owt_normal timechd if prevchd1 == 0

gen uw_normal =.
replace uw_normal=1 if (bmi1<18.5)
replace uw_normal=0 if (bmi1>=18.5 & bmi1 < 25)

ir anychd uw_normal timechd if prevchd1 == 0
