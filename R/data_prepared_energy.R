#' Prepares data for energy forecasting
#'
#' @param data Data frame to be used. Should contain id column, added if not present
#' @param outcome_var Name of the outcome variable. It's renamed as outcome
#' @param horizon The forecast horizon denoted as interger value
#' @param xreg_tbl Data frame with external regressors
#' @param slidify_period Sliding periods used in tk_augment_slidify. If not given c(1/4, 1/3, 1/2, 1) * horizon is used
#' @param transformation Transformation to be applied. Defaults to none. Possible values: log, log1p
#' @param pacf_threshold Threshold to determine which pacf lags to when creating fourier_period. Defaults to 0.2
#' @param no_fourier_terms Number of fourier periods to include. Defaults to 10
#' @param fourier_k Maximum order(s) of fourier terms
#' @param use_horizon_lag Should lag of outcome variable equal to the horizon be used? Defaults to TRUE
#'

prepare_energy_data <- function(data, outcome_var, horizon, xreg_tbl, slidify_period, transformation = "none",
                                fourier_periods = NULL, drop_na = TRUE, use_holidays = FALSE, holidays_tbl,
                                pacf_threshold = 0.2, no_fourier_terms = 10, fourier_k = 3,
                                use_horizon_lag = TRUE) {

    require(tidyverse)
    require(timetk)
    require(tidymodels)
    require(modeltime)

    # Return list
    return_list <- list()

    # Rename outcome
    df <- data %>%
        rename("outcome" = outcome_var)

    # Add id column if it is not present
    if (!"id" %in% names(data))
        df <- df %>%
        add_column(id = "id")

    if(transformation == "none") {
        df
    } else if (transformation == "log") {
        df <- df %>%
            mutate(outcome = log(outcome))
    } else if (transformation == "log1p") {
        df <- df %>%
            mutate(outcome = log1p(outcome))
    }

    # Future and prepared data
    df <- df %>%
        mutate(id = as_factor(id)) %>%
        future_frame(datetime, .length_out = horizon, .bind_data = TRUE) %>%
        fill(-outcome, .direction = "down") %>%
        ungroup()


    # Add XREG
    if (!is.null(xreg_tbl)) {
        df <- df %>%
            left_join(xreg_tbl)
    }

    # Holidays
    if (use_holidays) {
        df <- df %>%
            left_join(holidays_tbl)
    }


    # Fourier period
    if (is.null(fourier_periods)) {

        fourier_periods <- df %>%
            group_by(id) %>%
            tk_acf_diagnostics(datetime, outcome) %>%
            ungroup() %>%
            filter(abs(PACF) > pacf_threshold) %>%
            count(lag) %>%
            arrange(desc(n)) %>%
            filter(lag > 1) %>%
            dplyr::slice(1:no_fourier_terms) %>%
            pull(lag)

        fourier_periods <- c(fourier_periods, horizon/2,  horizon)
        fourier_periods <- unique(fourier_periods)

    }

    # Full data
    if (is.null(slidify_period)) {
        slidify_period <- c(1/4, 1/3, 1/2, 1) * horzion
        slidify_period <- round(slidify_period, 0)
    }

    if (use_horizon_lag) {


        full_data_tbl <- df %>%
            arrange(id, datetime) %>%
            group_by(id) %>%
            tk_augment_fourier(datetime, .periods = fourier_periods, .K = fourier_k) %>%
            tk_augment_lags(.value = outcome, .lags = horizon) %>%
            tk_augment_slidify(
                contains("outcome_lag"),
                .f = ~mean(.x, na.rm = TRUE),
                .period  = slidify_period,
                .partial = TRUE,
                .align   = "center"
            ) %>%
            ungroup() %>%
            rowid_to_column(var = "rowid")

    } else {

        full_data_tbl <- df %>%
            arrange(id, datetime) %>%
            group_by(id) %>%
            tk_augment_fourier(datetime, .periods = fourier_periods, .K = fourier_k) %>%
            ungroup() %>%
            rowid_to_column(var = "rowid")

    }

    # Necessary since the rolling features may contain NA's
    if (drop_na) {
        data_prepared_tbl <- full_data_tbl %>%
            filter(!is.na(outcome)) %>%
            drop_na()
    } else {
        data_prepared_tbl <- full_data_tbl %>%
            filter(!is.na(outcome))
    }


    # Future data
    future_data <- full_data_tbl %>%
        filter(is.na(outcome))


    # address nan and na in variables _lag
    future_data <- future_data %>%
        mutate(across(.cols = contains("_lag"),
                      .fns = ~ ifelse(is.nan(.x), NA, .x))) %>%
        mutate(across(.cols = contains("_lag"),
                      .fns  = ~ replace_na(.x, 0)))

    # Split
    splits <- data_prepared_tbl %>%
        time_series_split(datetime, assess = horizon, cumulative = TRUE)


    # Return data
    return_list$full_data     <- full_data_tbl
    return_list$data_prepared <- data_prepared_tbl
    return_list$future_data   <- future_data
    return_list$splits        <- splits
    return_list$horizon       <- horizon
    return_list$fourier_periods <- fourier_periods


    return(return_list)

}
