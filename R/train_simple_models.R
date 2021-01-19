#' A function to train simple univariate algorithms
#'
#' train_simple_models() is a function that allows you to train univariate time series models
#' @param data Training data (training(splits)) passed to each model
#' @param seasonality Controls the seasonality
#'
#' @export

train_simple_models <- function(data = training(splits),
                                seasonality = "daily") {

    library(modeltime)

    colnames(data)[1:2] <- c("date", "outcome")

    # ARIMA
    if (seasonality == "daily") {
        model_fit_arima <- arima_reg(seasonal_period = 7) %>%
            set_engine("auto_arima") %>%
            fit(outcome ~ date, data = data)

        # ETS
        model_fit_ets <- exp_smoothing(seasonal_period = 7) %>%
            set_engine("ets") %>%
            fit(outcome ~ date, data = data)

        # TBATS
        model_fit_tbats <- seasonal_reg(
            seasonal_period_1 = 7,
            seasonal_period_2 = 365.25
        ) %>%
            set_engine("tbats") %>%
            fit(outcome ~ date, data = data)

        # Seasonal decomposition with ARIMA
        model_fit_stlm_arima <- seasonal_reg(
            seasonal_period_1 = 7,
            seasonal_period_2 = 365.25
        ) %>%
            set_engine("stlm_arima") %>%
            fit(outcome ~ date, data = data)

        # Seasonal decomposition with ETS
        model_fit_stlm_ets <- seasonal_reg(
            seasonal_period_1 = 7,
            seasonal_period_2 = 365.25
        ) %>%
            set_engine("stlm_ets") %>%
            fit(outcome ~ date, data = data)

        # Prophet
        model_fit_prophet <- prophet_reg(
            changepoint_num    = 25,
            changepoint_range  = 0.8,
            seasonality_daily  = TRUE,
            seasonality_weekly = TRUE,
            seasonality_yearly = TRUE
        ) %>%
            set_engine("prophet") %>%
            fit(outcome ~ date, data = data)



    } else if (seasonality == "weekly") {

        model_fit_arima <- arima_reg(seasonal_period = 52) %>%
            set_engine("auto_arima") %>%
            fit(outcome ~ date, data = data)

        # ETS
        model_fit_ets <- exp_smoothing(seasonal_period = 52) %>%
            set_engine("ets") %>%
            fit(outcome ~ date, data = data)

        # TBATS
        model_fit_tbats <- seasonal_reg(
            seasonal_period_1 = 52
        ) %>%
            set_engine("tbats") %>%
            fit(outcome ~ date, data = data)

        # Seasonal decomposition with ARIMA
        model_fit_stlm_arima <- seasonal_reg(
            seasonal_period_1 = 52
        ) %>%
            set_engine("stlm_arima") %>%
            fit(outcome ~ date, data = data)

        # Seasonal decomposition with ETS
        model_fit_stlm_ets <- seasonal_reg(
            seasonal_period_1 = 52
        ) %>%
            set_engine("stlm_ets") %>%
            fit(outcome ~ date, data = data)

        # Prophet
        model_fit_prophet <- prophet_reg(
            changepoint_num    = 25,
            changepoint_range  = 0.8,
            seasonality_weekly = TRUE,
            seasonality_yearly = TRUE
        ) %>%
            set_engine("prophet") %>%
            fit(outcome ~ date, data = data)



    } else if (seasonality == "monthly") {

        model_fit_arima <- arima_reg(seasonal_period = 12) %>%
            set_engine("auto_arima") %>%
            fit(outcome ~ date, data = data)

        # ETS
        model_fit_ets <- exp_smoothing(seasonal_period = 12) %>%
            set_engine("ets") %>%
            fit(outcome ~ date, data = data)

        # TBATS
        model_fit_tbats <- seasonal_reg(
            seasonal_period_1 = 12
        ) %>%
            set_engine("tbats") %>%
            fit(outcome ~ date, data = data)

        # Seasonal decomposition with ARIMA
        model_fit_stlm_arima <- seasonal_reg(
            seasonal_period_1 = 12
        ) %>%
            set_engine("stlm_arima") %>%
            fit(outcome ~ date, data = data)

        # Seasonal decomposition with ETS
        model_fit_stlm_ets <- seasonal_reg(
            seasonal_period_1 = 12
        ) %>%
            set_engine("stlm_ets") %>%
            fit(outcome ~ date, data = data)

        # Prophet
        model_fit_prophet <- prophet_reg(
            changepoint_num    = 25,
            changepoint_range  = 0.8,
            seasonality_yearly = TRUE
        ) %>%
            set_engine("prophet") %>%
            fit(outcome ~ date, data = data)

    } else {

        model_fit_arima <- arima_reg(seasonal_period = "auto") %>%
            set_engine("auto_arima") %>%
            fit(outcome ~ date, data = data)

        # ETS
        model_fit_ets <- exp_smoothing(seasonal_period = "auto") %>%
            set_engine("ets") %>%
            fit(outcome ~ date, data = data)

        # TBATS
        model_fit_tbats <- seasonal_reg(
            seasonal_period_1 = "auto"
        ) %>%
            set_engine("tbats") %>%
            fit(outcome ~ date, data = data)

        # Seasonal decomposition with ARIMA
        model_fit_stlm_arima <- seasonal_reg(
            seasonal_period_1 = "auto"
        ) %>%
            set_engine("stlm_arima") %>%
            fit(outcome ~ date, data = data)

        # Seasonal decomposition with ETS
        model_fit_stlm_ets <- seasonal_reg(
            seasonal_period_1 = "auto"
        ) %>%
            set_engine("stlm_ets") %>%
            fit(outcome ~ date, data = data)

        # Prophet
        model_fit_prophet <- prophet_reg(
            changepoint_num    = 25,
            changepoint_range  = 0.8
        ) %>%
            set_engine("prophet") %>%
            fit(outcome ~ date, data = data)

    }



    # Model table
    model_simple_tbl <- modeltime_table(
        model_fit_arima,
        model_fit_ets,
        model_fit_tbats,
        model_fit_stlm_arima,
        model_fit_stlm_ets,
        model_fit_prophet
    )

    return(model_simple_tbl)

}
