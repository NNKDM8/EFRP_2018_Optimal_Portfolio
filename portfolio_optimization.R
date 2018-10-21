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
#Get the Covariance matrix for the learning data
Dmat <- matrix(cov(learningData), nrow = nStock, ncol = nStock)
#Dmat

#Get the Covariance matrix for the test data
Validmat <- matrix(cov(validationData), nrow = nStock, ncol = nStock)

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
optimalWeights <- qp$solution
optimalWeights

# Solv for optimal portfolio on the validation data
qp <- solve.QP(Validmat, dvec, Amat, bvec, meq=1)
optimalWeights_validation <- qp$solution
optimalWeights_validation

# Equal weight portfolio for benchmarking
equalWeights <- matrix(rep(1/nStock, nStock), nrow = nStock, ncol = 1)
equalWeights


# Calculating the predicted, the optimal and equal weighted portfolio

validationData <- validationData %>% as.tibble %>%
  mutate(portfolio = GE * optimalWeights_training[1] + JPM * optimalWeights_training[2] +
           BA * optimalWeights_training[3] + AAPL * optimalWeights_training[4] + JNJ * optimalWeights_training[5]) %>%
  mutate(portfolio_ideal = GE * optimalWeights_validation[1] + JPM * optimalWeights_validation[2] +
           BA * optimalWeights_validation[3] + AAPL * optimalWeights_validation[4] + JNJ * optimalWeights_validation[5]) %>%
  mutate(portfolio_equal = GE * equalWeights[1] + JPM * equalWeights[2] + BA * equalWeights[3] + AAPL * equalWeights[4] + JNJ * equalWeights[5])

# Mean and variance of the stocks and portfolios

risks <- sapply(validationData, var)
meanreturn <- sapply(validationData, mean)

results <- data.frame("Stock" = c("GE", "JPM", "BA", "AAPL", "JNJ", "portfolio", "portfolio_ideal", "portfolio_equal"), "Mean" = mean_return, "Risk" = risks)

# Check if the predicted portfolio performanced better than the equal weighted portfolio

view(results)

# While the risk of the predicted portfolio is lower than the equal weighted's risk, the return is far lower
# The optimal portfolio outperforms the predicted, but for risk-tolerant investors the equal weighted portfolio is effective

portf_error <- MSE(y_pred = validationData$portfolio, y_true = validationData$portfolio_ideal)

portf_equal_error <- MSE(y_pred = validationData$portfolio_equal, y_true = validationData$portfolio_ideal)

portf_error < portf_equal_error # Is the MSE of the predicted portfolio lower?




