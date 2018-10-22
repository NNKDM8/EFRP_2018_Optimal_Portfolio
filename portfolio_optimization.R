#install.packages("tidyverse")
library(tidyverse)
#install.packages("pdfetch")
library(pdfetch)
#install.packages("quadprog")
library(quadprog)
#install.packages("MLmetrics")
library(MLmetrics)
library(utils)

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
validationData <- fullData[(learningLines+1):allLines, ]

# THE OPTIMAL PORTFOLIO SELECTION ##############################################
#Get the Covariance matrix for the learning data
Dmat <- matrix(cov(learningData), nrow = nStock, ncol = nStock)
#Dmat

#Get the Covariance matrix for the test data
Validmat <- matrix(cov(validationData), nrow = nStock, ncol = nStock)

#Get the Covariance matrix for the full data
fullMat <- matrix(cov(fullData), nrow = nStock, ncol = nStock)

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

# Solv for optimal portfolio on the training data
qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
optimalWeights_training <- qp$solution
optimalWeights_training

# Solv for optimal portfolio on the validation data
qp <- solve.QP(Validmat, dvec, Amat, bvec, meq=1)
optimalWeights_validation <- qp$solution
optimalWeights_validation

# Solv for optimal portfolio on the full data
qp <- solve.QP(fullMat, dvec, Amat, bvec, meq=1)
optimalWeights_full <- qp$solution
optimalWeights_full

#Random weight
set.seed(1)
rand <- runif(5)
randWeights <- rand/sum(rand)
randWeights

# Equal weight portfolio for benchmarking
equalWeights <- matrix(rep(1/nStock, nStock), nrow = nStock, ncol = 1)
equalWeights


# Calculating the predicted, the optimal and equal weighted portfolio
validationData <- validationData %>% as.tibble %>%
  mutate(portfolio = rowSums(t(t(validationData) * optimalWeights_training))) %>%
  mutate(portfolio_ideal = rowSums(t(t(validationData) * optimalWeights_validation))) %>%
  mutate(portfolio_equal = rowSums(t(t(validationData) * as.vector(equalWeights)))) %>%
  mutate(portfolio_full = rowSums(t(t(validationData) * optimalWeights_full))) %>%
  mutate(portfolio_random = rowSums(t(t(validationData) * randWeights)))


# Mean and variance of the stocks and portfolios

risks <- sapply(validationData, var)
mean_return <- sapply(validationData, mean)

results <- data.frame("Stock" = c("GE", "JPM", "BA", "AAPL", "JNJ", "portfolio", "portfolio_ideal", "portfolio_equal", "portfolio_full", "portfolio_random"), "Mean" = mean_return, "Risk" = risks)

# Check if the predicted portfolio performanced better than the equal weighted portfolio

results

# While the risk of the predicted portfolio is lower than the equal weighted's risk, the return is far lower
# The optimal portfolio outperforms the predicted, but for risk-tolerant investors the equal weighted portfolio is effective

portf_error <- MSE(y_pred = validationData$portfolio, y_true = validationData$portfolio_ideal)

portf_equal_error <- MSE(y_pred = validationData$portfolio_equal, y_true = validationData$portfolio_ideal)

portf_error < portf_equal_error # Is the MSE of the predicted portfolio lower?

