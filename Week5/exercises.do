sum bmi1
* Calculate a 90% predictive interval for X.
di  25.84616 - invnormal(0.95)*4.101821
di  25.84616 + invnormal(0.95)*4.101821

* For a random sample of size 10, calculate a 90% predictive interval for the sample mean of X.
di  25.84616 - invnormal(0.95)*(4.101821/sqrt(10))
di  25.84616 + invnormal(0.95)*(4.101821/sqrt(10))
