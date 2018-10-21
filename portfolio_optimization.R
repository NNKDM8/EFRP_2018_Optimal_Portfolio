#install.packages("tidyverse")
library(tidyverse)
#install.packages("pdfetch")
library(pdfetch)
#install.packages("quadprog")
library(quadprog)

# GLOBAL PARAMETERS
# Fraction of data used for learning
learningFraction <- 0.7 

# Number of stocks used 
nStock <- 5

# GET DATA AND PREPROCESS ##############################################
#Select and download 5 US stocks from separate indicies //financial/healthcare/services/tech/consumer goods
GE <- pdfetch_YAHOO("GE", from = "2013-01-01", to = "2018-10-14", fields = "adjclose")
JPM <- pdfetch_YAHOO("JPM", from = "2013-01-01", to = "2018-10-14", fields = "adjclose")
BA <- pdfetch_YAHOO("BA", from = "2013-01-01", to = "2018-10-14", fields = "adjclose")
AAPL <- pdfetch_YAHOO("AAPL", from = "2013-01-01", to = "2018-10-14", fields = "adjclose")
JNJ <- pdfetch_YAHOO("JNJ", from = "2013-01-01", to = "2018-10-14", fields = "adjclose")
  
#Logdiff adjusted cloe for yileds
df <- data.frame(GE, JPM, BA, AAPL, JNJ)
fun <- function(x) {
  as.matrix(diff(log (x)))
}

fullData <- sapply(df, fun)

# Partition the data
allLines <- nrow(fullData)
learningLines <- floor(allLines * learningFraction)

#learningLines

# In sample data to train on
learningData <- fullData[1:learningLines, ]

# Out of sample data to test on
validationData <- fullData[learningLines+1:allLines, ]

# THE OPTIMAL PORTFOLIO SELECTION ##############################################
#Get the Covariance matrix for the learning data only
Dmat <- matrix(cov(learningData), nrow = nStock, ncol = nStock)
#Dmat

# Coeff vector for linear part
dvec <- matrix(rep(0.0, nStock), nrow = nStock, ncol = 1)
#dvec

# Constraints matrix
A.Equality <- matrix(rep(1, nStock), ncol=1)
Amat <- cbind(A.Equality, diag(nStock), -diag(nStock))
#Amat

# No short and no weight over 1
bvec <- c(1, rep(0, nStock), rep(-1.0, nStock))
bvec

# Solv for optimal portfolio
qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
optimalWeights <- qp$solution
optimalWeights

# Equal weight portfolio for benchmarking
equalWeights <- matrix(rep(1/nStock, nStock), nrow = nStock, ncol = 1)
equalWeights



