calcMA <- function(mymatrix,alpha=0.05,referencerow=2)
  {
     numrow <- nrow(mymatrix)
     myrownames <- rownames(mymatrix)
     for (i in 1:numrow)
     {
        rowname <- myrownames[i]
        DiseaseUnexposed <- mymatrix[referencerow,1]
        ControlUnexposed <- mymatrix[referencerow,2]
        if (i != referencerow)
        {
           DiseaseExposed <- mymatrix[i,1]
           ControlExposed <- mymatrix[i,2]
           totExposed <- DiseaseExposed + ControlExposed
           totUnexposed <- DiseaseUnexposed + ControlUnexposed
           probDiseaseGivenExposed <- DiseaseExposed/totExposed
           probDiseaseGivenUnexposed <- DiseaseUnexposed/totUnexposed

           probControlGivenExposed <- ControlExposed/totExposed
           probControlGivenUnexposed <- ControlUnexposed/totUnexposed

           # calculate the relative risk
           relativeRisk <- probDiseaseGivenExposed/probDiseaseGivenUnexposed
           print(paste("category =", rowname, ", relative risk = ", round(relativeRisk, 4)))

           # calculate a confidence interval
           confidenceLevel <- (1 - alpha)*100
           sigma <- sqrt((1/DiseaseExposed) - (1/totExposed) +
                         (1/DiseaseUnexposed) - (1/totUnexposed))
      
           # sigma is the standard error of estimate of log of relative risk
           z <- qnorm(1-(alpha/2))
           lowervalue <- relativeRisk * exp(-z * sigma)
           uppervalue <- relativeRisk * exp( z * sigma)
           print(paste("category =", rowname, ", ", confidenceLevel,
              "% confidence interval = [",round(lowervalue, 4),",",round(uppervalue,4),"]"))

           #calculate risk difference
           riskDiff <- round(probDiseaseGivenExposed - probDiseaseGivenUnexposed, 4)
           lowervalue_rd <- riskDiff * exp(-z * sigma)
           uppervalue_rd <- riskDiff * exp(z * sigma)

           print(paste("category =", rowname, ", risk difference = ",round(riskDiff, 4)))
           print(paste("category =", rowname, ", ", confidenceLevel,
                      "% confidence interval = [",round(lowervalue_rd, 4),",",round(uppervalue_rd,4),"]"))           

           #Calculate Disease Odds Ratio
           oddsRatio <- (probDiseaseGivenExposed*probControlGivenUnexposed)/
                        (probControlGivenExposed*probDiseaseGivenUnexposed)

           lowervalue_or <- oddsRatio * exp(-z * sigma)
           uppervalue_or <- oddsRatio * exp(z * sigma)

           print(paste("category =", rowname, ", odds ratio = ",round(oddsRatio,4)))
           print(paste("category =", rowname, ", ", confidenceLevel,
                      "% confidence interval = [",round(lowervalue, 4),",",round(uppervalue, 4),"]"))



        }
     }
  }