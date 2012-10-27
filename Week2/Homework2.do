*BMI and CHD Prevalence at the 1956 Exam

use "C:\Users\felipillo\Documents\GitHub\PH207x\Data\fhs.dta", clear

gen bmi1_cat=.
replace bmi1_cat=1 if (bmi1<18.5)
replace bmi1_cat=2 if (bmi1>=18.5 & bmi1 <= 25)
replace bmi1_cat=3 if (bmi1>25 & bmi1 <= 30)
replace bmi1_cat=4 if (bmi1>30 & bmi1<.)

tab bmi1_cat prevchd1, cell row column

*DIABETES PREVALENCE

*Use Stata and the NHLBI data set to report the prevalence of diabetes
*among only those participants who attended and had non-missing data for
*diabetes at all three examinations. 
*(Hint: There were 3,206 such participants.)

*What is the prevalence of diabetes at the 1st, 2nd and 3rd exam?

gen diabetestot = diabetes1<. & diabetes2<. & diabetes3<.
tab1  diabetes1 diabetes2 diabetes3 if diabetestot == 1

*BMI AND HYPERTENSION PREVALENCE

*Use Stata and the NHLBI data set to create the four categories of 
*body mass index as defined in the first question.

*1. What is the prevalence of hypertension (prevhyp1=1) at the 1956 
*exam for each of the body mass index class?

tab  bmi1_cat  prevhyp1, cell column row

*HYPERTENSION AND BLOOD PRESSURE

*Use Stata to create a binary variable (highbp1) to represent the 
*presence/absence of hypertension at the 1956 examination

generate highbp1=.
replace highbp1=1 if (sysbp1>=140 | diabp1 >= 90)
replace highbp1=0 if (sysbp1<140 & diabp1<90)

*What is the prevalence of CHD (prevchd1=1) at the 1956 exam for 
*each class of highbp1?

tab highbp1 prevchd1 , cell column row


*HYPOTHETICAL LIFE TABLE
*The table below lists the number of individuals at age "x" for 
*a hypothetical population in 1950-1952 and 1990 - 1992

insheet using "C:\Users\felipillo\Documents\GitHub\PH207x\Data\life_table_hwk2.csv", clear



