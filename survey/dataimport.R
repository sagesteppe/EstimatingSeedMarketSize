library(surveydown)

setwd('/home/sagesteppe/Documents/assoRted/EstimatingSeedMarketSize/survey')
db <- sd_db_connect(
  gssencmode = NULL
)
data <- sd_get_data(db)
head(data)
