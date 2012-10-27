use "C:\Users\felipillo\Documents\GitHub\PH207x\Data\fhs.dta", clear

tab1 cursmoke1 cursmoke2 cursmoke3, missing
tab1 cursmoke1 cursmoke2 cursmoke3
gen cursmokenotmiss = cursmoke1<. & cursmoke2<. & cursmoke3<.
tab1 cursmoke1 cursmoke2 cursmoke3 if cursmokenotmiss==1, missing
tabulate cursmoke1 cursmoke2, cell column miss row
tabulate cursmoke1 cursmoke2, cell column row


gen packs1=.
replace packs1=0 if (cigpday1==0) 
replace packs1=1 if (cigpday1>=1 & cigpday1 <= 20)
replace packs1=2 if (cigpday1>=21 & cigpday1 <= 40)
replace packs1=3 if (cigpday1>=41 & cigpday1<.)

tabulate packs1 prevchd1, cell column row

