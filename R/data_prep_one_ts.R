#' A function to prepare a single time series for forecasting algorithms
#'
#' data_prep_single_ts() is function that allows you to create machine ready data
#' @param data A data frame that contains only two variables, date and the outcome
#' @param dep_var Unquoted name of the dependent variable
#' @param date_group Frequency of the data, defaults to day
#' @param prepros The preprocessing of the outcome variable. Defaults to log
#' @param spline_degree The degree of the natural spline, defaults to 3
#' @param fourier_k Number or orders to include for each sine/cosine
#' @param horzon The forecasting horizon, defaults to 30
#' @param use_xreg Are you using external regressors, defaults to FALSE
#' @param zero_out_fridagar Should holidays be set to zero for the forecast horizon due to e.g. covid, defaults to 1 year into the future
#' @param fridagar Do you want to use holidays, defaults to FALSE
#'
#' @export
#'
data_prep_single_ts <- function(data, dep_var, date_group = "day", clean_outliers = TRUE,
                                prepros = "log", spline_degree = 3, fourier_k = 5, horizon = 30,
                                use_xreg = FALSE, zero_out_fridagar = today() + years(1),
                                fridagar = FALSE) {

    # packages
    library(tidyverse)
    library(timetk)
    library(tidymodels)
    library(modeltime)


    # Aggregate data by date_group
    data <- data %>%
        mutate(date = floor_date(date, date_group)) %>%
        group_by(date) %>%
        summarise(outcome = sum({{dep_var}}, na.rm = TRUE)) %>%
        # summarise(outcome = sum(value, na.rm = TRUE)) %>%
        mutate(outcome = ifelse(outcome < 0, 0, outcome))


    # Fridagar
    if (fridagar) {
        fridagar <- fridagar_raw %>%
            mutate(date = as.Date(date)) %>%
            mutate(fridagur = 1,
                   date = floor_date(date, date_group)) %>%
            group_by(date) %>%
            summarise(dagar = sum(fridagur))
    }



    # Transformation ----
    if (prepros == "log") {
        data <- data %>%
            mutate(outcome = log(outcome))
    } else if (prepros == "log1p") {
        data <- data %>%
            mutate(outcome = log1p(outcome))
    } else if (prepros == "log_interval") {
        data <- data %>%
            mutate(outcome = log_interval_vec(outcome, limit_lower = 0, offse = 1))
    } else if (prepros == "box_cox") {
        data <- data %>%
            mutate(outcome = box_cox_vec(outcome))
    } else {
        data
    }


    # Time series signature ----
    if (date_group == "day") {
        data_prep <- data %>%
            tk_augment_timeseries_signature() %>%
            select(-contains(".iso"), -contains(".xts"), -contains("hour"), -contains("minute"), -diff,
                   -contains("second"), -contains("am.pm"))

    } else if (date_group == "week") {
        data_prep <- data %>%
            tk_augment_timeseries_signature() %>%
            select(-contains(".iso"), -contains(".xts"), -contains("hour"), -contains("minute"), -diff,
                   -contains("second"), -contains("am.pm"), -wday, -wday.lbl, -month)

    } else if (date_group == "month") {
        data_prep <- data %>%
            tk_augment_timeseries_signature() %>%
            select(-contains(".iso"), -contains(".xts"), -contains("hour"), -contains("minute"), -diff,
                   -contains("second"), -contains("am.pm"), -contains("week"), -contains("mday"), -contains("day"))

    } else if (date_group == "quarter") {
        data_prep <- data %>%
            tk_augment_timeseries_signature() %>%
            select(-contains(".iso"), -contains(".xts"), -contains("hour"), -contains("minute"), -diff,
                   -contains("second"), -contains("am.pm"), -contains("week"), -contains("mday"),
                   -contains("day"), -contains("month"))
    } else {
        data_prep <- data %>%
            tk_augment_timeseries_signature() %>%
            select(-contains(".iso"), -contains(".xts"), -contains("hour"), -contains("minute"), -diff,
                   -contains("second"), -contains("am.pm"), -contains("week"), -contains("mday"),
                   -contains("day"), -contains("month"), -contains("quater"), -contains("half"))
    }


    # ACF and PACF for fourier peridos ----
    fourier_periods <- data_prep %>%
        tk_acf_diagnostics(.date_var = date, .value = outcome) %>%
        filter(abs(PACF) > 0.1) %>%
        filter(lag > 1) %>%
        pull(lag)

    acf_plot <- data_prep %>% plot_acf_diagnostics(.date_var = date, .value = outcome)


    # Fourier terms ----
    data_prep <- data_prep %>%
        tk_augment_fourier(
            .date = date,
            .periods = fourier_periods,
            .K = fourier_k
        )

    fourier_plot <- data_prep %>%
        plot_time_series_regression(
            date,
            .formula = outcome ~ .,
        )


    # Lags and rolling features ----
    lag_use <- fourier_periods[fourier_periods > horizon]
    lag_use <- min(lag_use)

    ## Create a rule not to use lag if it will shrink my data set by 10% or more
    if (lag_use > 0.1 * nrow(data_prep)) {
        lag_use = 0
    } else {
        lag_use
    }

    data_prep <- data_prep %>%
        tk_augment_lags(outcome, .lags = lag_use) %>%
        drop_na()

    ## Rolling
    data_prep <- data_prep %>%
        tk_augment_slidify(
            .value   = paste0("outcome_lag", lag_use),
            .f       = mean,
            .period  = c(4, 12, 52),
            .align   = "center",
            .partial = TRUE
        )


    # Create full data set ----
    data_prep_use <- data %>%

        # add future window
        bind_rows(
            future_frame(.data = ., .date_var = date, .length_out = horizon)
        ) %>%

        # Add lags
        tk_augment_lags(outcome, .lags = lag_use) %>%

        # # Rolling lags
        tk_augment_slidify(
            .value   = paste0("outcome_lag", lag_use),
            .f       = mean,
            .period  = c(10, 30, 60),
            .align   = "center",
            .partial = TRUE
        )

    # Fix column names
    data_prep_use <- data_prep_use %>%
        ungroup() %>%
        select(-contains("paste0")) %>%
        bind_cols(data_prep_use %>%
                      ungroup() %>%
                      select(contains("paste0")) %>%
                      set_names("outcome_lag_roll_1", "outcome_lag_roll_2", "outcome_lag_roll_3"))


    ## frídagar
    # Núlla út daga ef við á (t.d. ef hátíðir eru felldar niður vegna covid

    if (use_xreg) {
        fridagar <- fridagar %>%
            mutate(dagar = case_when(date >= as.Date(zero_out_fridagar) ~  0,
                                     TRUE ~ dagar))

        # Bý til lags eftir date_group
        if (date_group == "day") {
            fridagar <- fridagar %>%
                tk_augment_lags(.value = dagar, .lags = 1:31)
        } else if (date_group == "week") {
            fridagar <- fridagar %>%
                tk_augment_lags(.value = dagar, .lags = 1:5) # fimman hér byggir á ccf
        } else {
            fridagar <- fridagar %>%
                tk_augment_lags(.value = dagar, .lags = 1:2)
        }
    }


    ## Skeiti frídögum við
    if (use_xreg) {

        data_prep_use <- data_prep_use %>%
            left_join(fridagar) %>%
            pivot_longer(cols = contains("dagar")) %>%
            replace_na(list(value = 0)) %>%
            pivot_wider(names_from = name, values_from = value)

    } else {
        data_prep_use
    }



    # Modeling and forecasting data ----
    data_prepared_tbl <- data_prep_use %>%
        filter(!(is.na(outcome)))

    forecast_tbl <- data_prep_use %>%
        filter(is.na(outcome))




    # Clean time series for training
    if (date_group == "day") {
        train_cleand <- training(splits) %>%
            mutate(outcome = ts_clean_vec(outcome, period = 365))
    } else if (date_group == "week") {
        train_cleand <- training(splits) %>%
            mutate(outcome = ts_clean_vec(outcome, period = 52))
    } else if (date_group == "month") {
        train_cleand <- training(splits) %>%
            mutate(outcome = ts_clean_vec(outcome, period = 12))
    } else {
        train_cleand <- training(splits)
    }



    # Recipe - time series signature ----
    if (date_group == "day") {
        recipe_spec_base <- recipe(outcome ~ .,
                                   data = train_cleand) %>%
            # Time sereis features
            step_timeseries_signature(date) %>%
            step_rm(contains(".iso"), contains(".xts"), contains("hour"), contains("minute"),
                    contains("second"), contains("am.pm"))

    } else if (date_group == "week") {
        recipe_spec_base <- recipe(outcome ~ .,
                                   data = train_cleand) %>%
            # Time sereis features
            step_timeseries_signature(date) %>%
            step_rm(contains(".iso"), contains(".xts"), contains("hour"), contains("minute"),
                    contains("second"), contains("am.pm"), date_wday, date_month)

    } else if (date_group == "month") {
        recipe_spec_base <- recipe(outcome ~ .,
                                   data = train_cleand) %>%
            # Time sereis features
            step_timeseries_signature(date) %>%
            step_rm(contains(".iso"), contains(".xts"), contains("hour"), contains("minute"),
                    contains("second"), contains("am.pm"), contains("week"), contains("mday"),
                    contains("day"))

    } else if (date_group == "quarter") {
        recipe_spec_base <- recipe(outcome ~ .,
                                   data = train_cleand) %>%
            # Time sereis features
            step_timeseries_signature(date) %>%
            step_rm(contains(".iso"), contains(".xts"), contains("hour"), contains("minute"),
                    contains("second"), contains("am.pm"), contains("week"), contains("mday"),
                    contains("day"), contains("month"))

    } else  {
        recipe_spec_base <- recipe(outcome ~ .,
                                   data = train_cleand) %>%
            # Time sereis features
            step_timeseries_signature(date) %>%
            step_rm(contains(".iso"), contains(".xts"), contains("hour"), contains("minute"),
                    contains("second"), contains("am.pm"), contains("week"), contains("mday"),
                    contains("day"), contains("month"), contains("quarter"), contains("half"))
    }


    ## Klára recipe
    recipe_spec_base <- recipe_spec_base %>%

        # Standardize
        step_normalize(matches("(index.num)|(year)|(yday)")) %>%

        step_mutate_at(contains("week"), fn = as.factor) %>%

        # dummy encoding
        step_dummy(all_nominal(), one_hot = TRUE) %>%

        # Fourier
        step_fourier(date, period = fourier_periods, K = fourier_k) %>%
        step_zv(all_predictors())


    ## Spline recipe
    recipe_spec_spline <- recipe_spec_base %>%
        step_rm(date) %>%
        step_ns(ends_with("index.num"), deg_free = spline_degree) %>%
        step_rm(contains("outcome_lag"))

    ## Lag recipe
    recipe_spec_lag <- recipe_spec_base %>%
        step_rm(date) %>%
        step_naomit(contains("outcome_lag"))

    # Spline and lag recipe
    recipe_spec_both <- recipe_spec_base %>%
        step_rm(date) %>%
        step_ns(ends_with("index.num"), deg_free = spline_degree) %>%
        step_naomit(contains("outcome_lag"))


    # Return list ----
    return_list <- list(
        "fourier_plot"      = fourier_plot,
        "cv_plot"           = cv_plot,
        "fourier_terms"     = fourier_periods,
        "splits"            = splits,
        "horizon"           = horizon,

        "data"              = data_prep,
        "data_prep"         = data_prepared_tbl,
        "data_clean"        = train_cleand,
        "forecast_data"     = forecast_tbl,
        "recipe_base"       = recipe_spec_base,
        "recipe_spline"     = recipe_spec_spline,
        "recipe_lag"        = recipe_spec_lag,
        "recipe_both"       = recipe_spec_both
    )


    return(return_list)

}
