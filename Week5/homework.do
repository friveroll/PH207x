*** Central Limit Theorem and Confidence Intervals ***

*3. Suppose we take a large number of samples of size 75. What proportion of the samples would we 
*expect to have a sample mean xˉ that lies between 106 and 110 g/L?
di normal(1.3856)-normal(-1.3856)

*4. Suppose instead we repeatedly took random samples of size 25. What proportion of the samples would we 
*expect to have a sample mean xˉ that lies between 106 and 110 g/L?
di normal(0.8)- normal(-0.8)

*5. Again, suppose we repeatedly took samples of size 75. What proportion of the samples would we expect to 
*have a mean less than xˉ=103?
*di normal(-3.4641)
di normal((103-108)/(12.5/sqrt(75)))

*6. If we repeatedly took samples of size 75, we would expect that, in 20% of the samples, xˉ would be greater than ____? 
di 1-invnormal(0.8)*(12.5/sqrt(75))+108

*7. After taking a sample of size 75, we found that the sample mean was xˉ=103. Construct a 95% confidence interval for μ.
cii 75 103 12.5

*Hypothesis Testing with known Variance 

*Test statistic 
di (103-108)/(12.5/sqrt(25))

*1 H0 mu = mu0
di 2*normal(-2)

* H1 mu0 < 108
di normal(-2)

* H1 mu0 > 108
di 1-normal(-2)

***Confidence intervals and testing with unknown variance*** 

*Construct a two-sided 95% confidence interval for μ.
cii 15 115 10.2


*Conduct a one-sample t-test to test the null hypothesis that the mean hemoglobin level is equal to 108 g/L, 
*versus the alternative that the mean is not equal to 108 g/L, at the α=0.01 level.

*What is your test statistic?
di invttail(14, 0.01)

*What is your p-value? (2 sided test)
di 2* ttail(14,(115-108)/(10.2/sqrt(15)))