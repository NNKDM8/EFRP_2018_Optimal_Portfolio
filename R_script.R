install.packages("tidyverse")
library(tidyverse)
install.packages("pdfetch")
library(pdfetch)
install.packages("quadprog")
library(quadprog)

#Select and download 5 US stocks from separate indicies //financial/healthcare/services/tech/consumer goods
GE <- pdfetch_YAHOO("GE", from = "2013-01-01", to = "2018-10-14")
JPM <- pdfetch_YAHOO("JPM", from = "2013-01-01", to = "2018-10-14")
BA <- pdfetch_YAHOO("BA", from = "2013-01-01", to = "2018-10-14")
AAPL <- pdfetch_YAHOO("AAPL", from = "2013-01-01", to = "2018-10-14")
JNJ <- pdfetch_YAHOO("JNJ", from = "2013-01-01", to = "2018-10-14")
  
#Or up to date?
#GE_now <- pdfetch_YAHOO("GE",from = "2013-01-01")

#Logdiff for yileds
this_is_it <- data.frame ( diff ( log ( GE$GE.adjclose )) , diff ( log ( JPM$JPM.adjclose )) , diff ( log ( BA$BA.adjclose )) , diff ( log ( AAPL$AAPL.adjclose )) , diff ( log ( JNJ$JNJ.adjclose )))

#Drop the missing values
returns <- drop_na(this_is_it)

#Get the Covariance matrix of these returns
kovariancia <- cov(returns)

kovariancia

# The optimal portfolio
nStock <- 5

# Dmat is the correlation mx
#Dmat <- matrix(c(356.25808, 12.31581, 261.88302, 12.31581, 27.24840, 18.50515, 261.88302, 18.50515,535.45960), nrow=3, ncol=3)
Dmat <- matrix(kovariancia, nrow = nStock, ncol = nStock)
Dmat

# Coeff vector for linear part
dvec <- matrix(rep(0.0, nStock), nrow = nStock, ncol = 1)
dvec

#dvec <- matrix(c(9.33, 3.33, 9.07), nrow=3, ncol=1)
#dvec

#A.Equality <- matrix(c(1,1,1), ncol=1)
A.Equality <- matrix(rep(1, nStock), ncol=1)
#Amat <- cbind(A.Equality, dvec, diag(3), -diag(3))
Amat <- cbind(A.Equality, diag(nStock), -diag(nStock))
Amat

#bvec <- c(1, 5.2, rep(0, 3), rep(-0.5, 3))
bvec <- c(1, rep(0, nStock), rep(-0.5, nStock))
bvec

qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
qp$solution

## Two time the covariance matrx
#Dmat <- 2*matrix(c(1,-1/2,-1/2,1), nrow = 2, byrow=TRUE)
#
## Linear part (have to be zero)
#dvec <- c(0, 0)
#Amat    <- diag(2)
#bvec <- c(0, 0)
#sol  <- solve.QP(Dmat, dvec, Amat, bvec, meq=0)
#sol
#


