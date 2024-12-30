library(fable)
library(fpp3)
library(tsibble)
library(dplyr)


tourism %>%
  filter(Region == "Melbourne") %>% 
  model(ETS(sqrt(Trips)))

global_economy |>
  filter(Code == "CAF") |>
  gg_tsdisplay(difference(Exports), plot_type='partial')


setwd('~/Documents/assoRted/SeedMarketSizingTalk/scripts')
annual <- read.csv('../data/processed/NoFires-TotalArea_byDOIRegion.csv')






# each area needs to have the same steps taken with it. 

# 1) determine if the data are stationary or not. 

kpss <- global_economy |>
  filter(Code == "CAF") |>
  
  # the null hypothesis of the kwiattkowsi-phillips-schmidt-shin test is that the 
  # data are stationary. p. values of < 0.05  indicate the data deviate
  # from this assumption, and differencing is required. 
  
  # note this test only reports p values on a range of 0.01 to 0.1 by rounding
  fabletools::features(Exports, feasts::unitroot_kpss)

# 2) if data are not stationary, they need to be made so via differencing

if(kpss$kpss_pvalue > 0.05){
  
  # determine how many differences are needed to make data stationary. 
  ndiffs <- global_economy |> 
    filter(Code == "CAF") |>
    fabletools::features(Exports, feasts::unitroot_ndiffs) 
  
  # now perform the differencing. 
  caf <- global_economy |> 
    filter(Code == "CAF") |>
    mutate(diff_Exports = tsibble::difference(Exports, differences = ndiffs$ndiffs))
  
}

caf |>
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")

caf_fit <- caf |>
  model(
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise=FALSE)
        )

caf_fit |>
  pivot_longer(
    !Country,
    names_to = "Model name",
    values_to = "Orders"
    )

mod_sel <- glance(caf_fit) |> 
  arrange(AICc) |> 
  select(.model:BIC)

top_mod <- mod_sel$.model[1] # selects top model. 

ob <- caf_fit |>
  select(all_of(top_mod))
  
K <- sum(ob[[1]][[1]][["fit"]][["spec"]][c('p', 'q')])
  
# we can test to see if the residuals are randomly distributed. if p > 0.05 we reject
# evidence that the residuals are non-randomly distributed. 
  
# note that the dof below are K = p + q from the top model. 
lb_res <- augment(caf_fit) |>
  filter(.model=='search') |>
  fabletools::features(.innov, ljung_box, lag = 10, dof = K)

lb_res$lb_pvalue
if(lb_res$lb_pvalue > 0.05){
  
  # if the residuals are randomly distributed, we can use a simple forecast method. 
  preds <- caf_fit |>
    fabletools::forecast(h = 10) |> 
    filter(.model== top_mod)
  
} else {
  
  preds <- caf_fit |>
    fabletools::forecast(h = 10, bootstrap = TRUE) |> 
    filter(.model== top_mod) 
  
}

# now get point estimates for each step in the prediction time period
preds_lvls <- preds |>
  fabletools::hilo(level = c(80, 95)) |>
  fabletools::unpack_hilo(c("80%", "95%"))

preds |> # plot the original time series and forecast predictions 
  autoplot(global_economy)

caf_fit |> # plot the diagnostics from the top model
  dplyr::select(top_mod) |>
  feasts::gg_tsresiduals()

