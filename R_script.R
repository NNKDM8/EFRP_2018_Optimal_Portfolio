#Install and load packages

install.packages("pdfetch")
library(pdfetch)


#Select and download 5 US stocks from separate indicies //financial/healthcare/services/tech/consumer goods

GE <- pdfetch_YAHOO("GE", from = "2013-01-01", to = "2018-10-14")
JPM <- pdfetch_YAHOO("JPM", from = "2013-01-01", to = "2018-10-14")
BA <- pdfetch_YAHOO("BA", from = "2013-01-01", to = "2018-10-14")
AAPL <- pdfetch_YAHOO("AAPL", from = "2013-01-01", to = "2018-10-14")
JNJ <- pdfetch_YAHOO("JNJ", from = "2013-01-01", to = "2018-10-14")
  
#Or up to date?
GE_now <- pdfetch_YAHOO("GE",from = "2013-01-01")

#Save logreturns in one dataset

this_is_it <- data.frame ( diff ( log ( GE$GE.adjclose )) , diff ( log ( JPM$JPM.adjclose )) , diff ( log ( BA$BA.adjclose )) , diff ( log ( AAPL$AAPL.adjclose )) , diff ( log ( JNJ$JNJ.adjclose )))

#Drop the missing values

install.packages("tidyverse")
library(tidyverse)

returns <- drop_na(this_is_it)

#Get the Covariance matrix of these returns

kovariancia <- cov(returns)

kovariancia
