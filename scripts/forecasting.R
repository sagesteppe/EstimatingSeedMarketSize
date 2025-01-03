library(fable)
library(fpp3)
library(tsibble)
library(dplyr)
library(gt)

setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')
annual <- read.csv('../data/processed/NoFires-TotalArea_byDOIRegion.csv')

# each area needs to have the same steps taken with it. 

x <- filter(annual, REG_NAME == 'North Atlantic-Appalachian')
x1 <- split(annual, f = annual$REG_NAME)

lapply(x1, forecasting)

forecasting <- function(x){
  
  
  x <- tsibble::as_tsibble(x, key = REG_NAME, index = FIRE_YEAR)
  
  ### perform ARIMA modelling ###
  # all other methods are very simple to compute, ARIMA has a few steps, so we'll
  # perform this first, and then compare the best ARIMA model to the simpler models
  # which serve as benchmarks (which ARIMA seldom outperforms with messy fire data).
  
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
  
  x_fit <- x |>
    model(
      stepwise = fable::ARIMA(TotalArea_Acre),
      search = fable::ARIMA(TotalArea_Acre, stepwise=FALSE)
    )
  
  x_fit |>
    pivot_longer(
      !REG_NAME,
      names_to = "Model name",
      values_to = "Orders"
    )
  
  # select models based on delta AIC. These are generally virtually identical between
  # the options. 
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
    # otherwise we need to ues bootstrap simulation
    preds <- x_fit |>
      fabletools::forecast(h = 10, bootstrap = TRUE) |> 
      filter(.model== top_mod) 
    
  }
  
  
  x_fit |> # plot the diagnostics from the top model
    dplyr::select(top_mod) |> 
    feasts::gg_tsresiduals() 
  
  # these need to be recomputed for each time period, 1, 5, and 10 year prediction. 
  forecasts <- x |>
    tsibble::stretch_tsibble(.init = 10) |>
    model(
      mean = fable::MEAN(TotalArea_Acre),
      ets = fable::ETS(TotalArea_Acre),
      arima = fable::ARIMA(TotalArea_Acre), 
      naive = fable::NAIVE(TotalArea_Acre),
      drift = fable::RW(TotalArea_Acre ~ drift())
    ) |> 
    fabletools::forecast(h = 10) 
  
  # evaluate the different methods
  forecasts_eval <- forecasts |>
    fabletools::accuracy(x) |> 
    dplyr::arrange(MASE) |>
    dplyr::select(.model, RMSE:MAE, MAPE:MASE)
  
  # now get point estimates for each step in the prediction time period
  preds_lvls <- forecasts |>
    dplyr::filter(.model == forecasts_eval$.model[1]) |> 
    fabletools::hilo(level = c(80, 95)) |> 
    fabletools::unpack_hilo(c("80%", "95%")) 
  
  # White noise 	ARIMA(0,0,0) with no constant
  # Random walk 	ARIMA(0,1,0) with no constant
  # Random walk with drift 	ARIMA(0,1,0) with a constant
  # Autoregression 	ARIMA(p,0,0)
  # Moving average 	ARIMA(0,0,q)
  
  exp <- preds_lvls |> 
    dplyr::group_by(.id,.model) |> 
    dplyr::mutate(h = row_number()) |> 
    dplyr::filter(h %in% c(1, 5, 10)) |> 
    dplyr::ungroup() |> 
    
    # set predictions below 0 to zero 
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.double), ~ dplyr::if_else(. < 0, 0, .)
      )
    ) 
  
  
  ############ Table comparing the different methods ###########################
  library(gt)
  
  forecasts_eval |>
    dplyr::rename(Model = '.model') |>
    gt::gt() |>
    gt::tab_header(
      title = md("**Top Performing Model**"),
      subtitle = exp$REG_NAME[1]) |>
    gt::data_color(
      columns = 2:5,
      method = "factor",
      palette = c('#ffffcc', '#c2e699', '#78c679', '#31a354', '#006837'),
      reverse = TRUE
    ) |>
    gt::tab_source_note(
      source_note = 
        md("*Models ranked soley by mean absolute scaled error (MASE).*")
    ) |>
    tab_options(
      table.background.color = "#FFFFFF00") |>
    gtsave(
      paste0('../results/Plots/Forecasts/Model-', gsub(' ', '_', exp$REG_NAME[1]), '.png'), 
      expand = 5)
  
  
  ##################### Plotting the Results  ####################################
  ################################################################################
  
  cols <- c("Observed"="#0A0908","Forecast"="#C8553D")
  ribs <- c('95% CI' = '#A0BCCF', '80% CI' = '#008FCC')
  
  p <- exp |>
    ggplot2::ggplot(
      aes(x = FIRE_YEAR, y = .mean, color = 'Forecast')) + 
    ggplot2::geom_ribbon(
      aes(ymin = `95%_lower`, ymax = `95%_upper`, fill = '95% CI'), color = NA) + 
    ggplot2::geom_ribbon(
      aes(ymin = `80%_lower`, ymax = `80%_upper`, fill = '80% CI'), color = NA) + 
    
    ggplot2::geom_line(data = x, 
                       aes(y = TotalArea_Acre, color = 'Observed'), lwd = 0.75) + 
    ggplot2::geom_point(data = x, 
                        aes(y = TotalArea_Acre, color = 'Observed')) + 
    
    ggplot2::geom_line(lwd = 0.85) + 
    ggplot2::geom_point() + 
    
    ggplot2::scale_fill_manual(
      name = 'Confidence\nInterval', 
      values = ribs, 
      drop = FALSE
    ) +  
    ggplot2::scale_colour_manual(
      name="Observed &\nForecast",
      values= cols, 
      guide = guide_legend(override.aes=aes(fill=NA))
    ) + 
    
    ggplot2::theme_minimal() + 
    ggplot2::labs(
      subtitle = paste0('method: ', exp$.model[1]),
      y = 'Total Burned Area (Acres)', 
      x = 'Year',
      title = paste0(exp$REG_NAME[1], ' fire forecasts')
    ) + 
    ggplot2::facet_wrap(
      ~ h, ncol = 1, scales= 'free_y'
    ) + 
    ggplot2::scale_y_continuous(labels = scales::comma) 
  
  ggsave(
    paste0('../results/Plots/Forecasts/Forecast-', gsub(' ', '_', exp$REG_NAME[1]), '.png') , 
    bg = 'white'
  )
}


png_files <- list.files(
  '../results/Plots/Forecasts', 
  pattern = ".*png$", full.names = TRUE
)

fcasts <- png_files[grep('Forecast-', png_files)]
mod_sel <- png_files[grep('Model-', png_files)]

gifski::gifski(
  mod_sel,
  gif_file = '~/Documents/assoRted/SeedMarketSizingTalk/images/ForecastModelSelection.gif', 
  width = 800, height = 600, delay = 5)

gifski::gifski(
  fcasts,
  gif_file = '~/Documents/assoRted/SeedMarketSizingTalk/images/fcasts.gif', 
  width = 800, height = 600, delay = 5)