library(fable)
library(fpp3)
library(tsibble)
library(dplyr)

setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')
annual <- read.csv('../data/processed/NoFires-TotalArea_byDOIRegion.csv')

# each area needs to have the same steps taken with it. 

x <- filter(annual, REG_NAME == 'South Atlantic Gulf') |>
#  mutate(TotalArea_Acre = log(TotalArea_Acre)) |>
  as_tsibble(key = REG_NAME, index = FIRE_YEAR)
# 1) determine if the data are stationary or not. 

  
# the null hypothesis of the kwiattkowsi-phillips-schmidt-shin (kpss) test is that
# the data are stationary. p. values of < 0.05  indicate the data deviate
# from this assumption, and differencing is required. 
  
# note this test only reports p values on a range of 0.01 to 0.1 by rounding
kpss <- x |> 
  fabletools::features(TotalArea_Acre, feasts::unitroot_kpss)

# 2) if data are not stationary, they need to be made so via differencing

if(kpss$kpss_pvalue < 0.05){
  
  # determine how many differences are needed to make data stationary. 
  ndiffs <- x |>
    fabletools::features(TotalArea_Acre, feasts::unitroot_ndiffs) 
  
  # now perform the differencing. 
  x <- x|>
  dplyr::mutate(diff_Exports =
             tsibble::difference(TotalArea_Acre, differences = ndiffs$ndiffs))
  
}

x |>
  autoplot(TotalArea_Acre) +
  labs(title="Total Areas Acre (burned)", y="Acres")

x_fit <- x |>
  model(
        stepwise = ARIMA(TotalArea_Acre),
        search = ARIMA(TotalArea_Acre, stepwise=FALSE)
        )

x_fit |>
  pivot_longer(
    !REG_NAME,
    names_to = "Model name",
    values_to = "Orders"
    )

mod_sel <- glance(x_fit) |> 
  arrange(AICc) |> 
  select(.model:BIC)

top_mod <- mod_sel$.model[1] # selects top model. 

ob <- x_fit |>
  select(all_of(top_mod))
  
K <- sum(ob[[1]][[1]][["fit"]][["spec"]][c('p', 'q')])
  
# we can test to see if the residuals are randomly distributed. if p > 0.05 we reject
# evidence that the residuals are non-randomly distributed. 
  
# note that the dof below are K = p + q from the top model. 
lb_res <- augment(x_fit) |>
  filter(.model=='search') |>
  fabletools::features(.innov, ljung_box, lag = 10, dof = K)

lb_res$lb_pvalue
if(lb_res$lb_pvalue > 0.05){
  
  # if the residuals are randomly distributed, we can use a simple forecast method. 
  preds <- x_fit |>
    fabletools::forecast(h = 10) |> 
    filter(.model== top_mod)
  
} else {
  
  preds <- x_fit |>
    fabletools::forecast(h = 10, bootstrap = TRUE) |> 
    filter(.model== top_mod) 
  
}


x |>
  stretch_tsibble(.init = 10) |>
  model(
    ETS(TotalArea_Acre),
    ARIMA(TotalArea_Acre)
  ) |>
  forecast(h = 1) |>
  accuracy(x) |> 
  select(.model, RMSE:MAPE)

  ?accuracy

# now get point estimates for each step in the prediction time period
preds_lvls <- preds |>
  fabletools::hilo(level = c(80, 95)) |>
  fabletools::unpack_hilo(c("80%", "95%"))

x_fit |> # plot the diagnostics from the top model
  dplyr::select(top_mod) |> 
  feasts::gg_tsresiduals() 

preds |> # plot the original time series and forecast predictions 
  autoplot(x)

ob
# White noise ARIMA(0,0,0)
# Moving average ARIMA(0,0,x)