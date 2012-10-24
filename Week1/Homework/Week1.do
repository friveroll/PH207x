** CARDIAC HOSPITALIZATIONS **

* In this question, we examine the reported numbers of hospitalizations 
* for cardiac events in a hypothetical population for each month in the 
* period January 1991 to December 1992.

insheet using "C:\Users\felipillo\Documents\GitHub\PH207x\Data\hospitalizations.csv"



** PREVALENT HYPERTENSION **

* 4. Among the individuals with prevalent hypertension at exam 1, how many 
* are female?

by sex1, sort : summarize prevhyp1 [fweight = prevhyp1]

* 6. Which graph would you use to summarize the distribution of 
* the indicator for prevalent hypertension at exam 1 in the 
* study population?

graph bar (count) prevhyp1 [fweight = prevhyp1], over(sex1)
