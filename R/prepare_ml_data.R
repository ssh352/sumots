#' Prepares data for modeling
#'
#' @param data Data frame with data on the right date format e.g. daily, weekly, monthly and with column named 'id'
#' @param outcome_var Name of the outcome variable. The function will change the outcome variable name to 'outcome'
#' @param negative_to_zero Recodes negative values as zero, defaults to TRUE
#' @param max_gap_size The maximum length that the outcome can be zero. If the interval is larger than max_gap_size then only use data after the interval
#' @param trailing_zero Extends all time series, back and forward, so they will be the same length. Defaults to FALSE
#' @param transformation Should the series be transformed, e.g. log or log1p. Defaults to none
#' @param use_holidays Should national holidays be included. As of now this has to be a dataframe supplied by the user to the holidays_to_use argument
#' @param holidays_to_use_1 Data frame of dummy holidays. Outcome of the create_holiday() function: fridagar_tbl
#' @param holidays_to_use_2 Data frame of holidays, one variable.
#' @param horizon The forecast horizon
#' @param drop_na When creating data_prepared_tbl, should NA's be dropped. Defaults to TRUE
#' @param fill_na_with_zero Used when drop_na = FALSE to fill missing values with zero instead of dropping them.
#' @param clean Should the data be cleand for outliers. Defaults to FALSE
#' @param use_holiday_to_clean Uses fridagar_one_var from the create_holiday() function to revert series to original value if cleand
#' @param pacf_threshold Threshold for where to cut the PACF to choose terms for the fourier calculation
#' @param no_fourier_terms Number of fourier terms, defultas to 5
#' @param fourier_k The fourier term order, defaults to 5
#' @param slidify_period The window size, defaults to c(4, 8)
#' @param use_seasonal_lag Should lag of outcome variable, equal to the seasonality, be used. Defaults to TRUE.
#' @param seasonal_frequency The frequency of the data. E.g. 52 for weekly data
#' @param use_own_fourier Should you use your own fourier terms? Defaults to FALSE
#' @param fourier_terms The fourier terms to include.
#' @param recursive_data Should the data be prepared for a recursive forecasting. Defaults to FALSE.
#' @param no_recursive_lag The number of lags to be.
#' @param xreg External regressors to add
#'
#' @return List with data_prepared, future_data, train_data, splits and horizon


data_prep_func <- function(data, outcome_var, negative_to_zero = FALSE, fix_gap_size = FALSE, max_gap_size,
                           trailing_zero = FALSE, transformation = "none", use_holidays = FALSE,
                           holidays_to_use_1, holidays_to_use_2, use_seasonal_lag = TRUE, seasonal_frequency,
                           horizon, clean = FALSE, drop_na = TRUE,  use_holiday_to_clean = FALSE,
                           holiday_for_clean,  use_abc_category = FALSE, pacf_threshold = 0.2, no_fourier_terms = 5,
                           fourier_k = 5, slidify_period = c(4, 8), use_own_fourier = FALSE, fourier_terms,
                           recursive_data = FALSE, no_recursive_lag, xreg = NULL, fill_na_with_zero = TRUE) {

    require(tidyverse)
    require(timetk)
    require(tidymodels)

    # return list
    return_list <- list()

    # Rename outcome
    df <- data %>%
        rename("outcome" = outcome_var)

    df <- df %>%
        arrange(id, date)


    # Negative values
    if(negative_to_zero) {
        df <- df %>%
            mutate(outcome = ifelse(outcome < 0 , 0, outcome))

    } else {
        df
    }


    # Gap size
    if (fix_gap_size) {
        df <- df %>%
            group_by(id) %>%
            mutate(max_date = max(date)) %>%
            ungroup() %>%
            complete(id, date) %>%
            mutate(outcome = ifelse(is.na(outcome), 0, outcome)) %>%
            fill(-outcome, .direction = "down") %>%
            group_by(id) %>%

            # leading zeros first
            mutate(cumsum_sala = cumsum(outcome)) %>%
            filter(cumsum_sala > 0, date <= max_date) %>%
            select(-max_date) %>%

            # choose only items with sales in the past 52 weeks
            mutate(no_sala = ifelse(outcome == 0, 1, 0)) %>%
            mutate(cum_no_sala = rollapplyr(no_sala, width = max_gap_size, FUN = sum, partial = TRUE),
                   indicator = case_when(
                       lag(cum_no_sala) == max_gap_size & outcome > 0 ~ 1,
                       all(cum_no_sala < max_gap_size) ~ 1,
                       TRUE ~ 0),
                   cum_indicator = cumsum(indicator)) %>%
            filter(cum_indicator >= 1) %>%
            select(-c(cumsum_sala, no_sala, cum_no_sala, indicator, cum_indicator)) %>%
            ungroup()}



    # Trailing zeros
    if(trailing_zero) {
        df <- df %>%
            complete(id, date) %>%
            group_by(id) %>%
            fill(-outcome, .direction = "up") %>%
            fill(-outcome, .direction = "down") %>%
            ungroup() %>%
            mutate(outcome = ifelse(is.na(outcome), 0 , outcome))
    } else {

        df <- df %>%
            complete(id, date) %>%
            # Hendi út trailing zeros
            mutate(outcome = ifelse(is.na(outcome), 0, outcome),
                   zero_helper = ifelse(outcome == 0, 0, 1)) %>%
            group_by(id) %>%
            mutate(cumsum_zero = cumsum(zero_helper)) %>%
            filter(cumsum_zero > 0) %>%
            ungroup() %>%
            # drop_na(ft) %>%
            select(-contains("zero"), -contains("cumsum")) %>%
            drop_na()

    }

    # Transformations
    if(transformation == "none") {
        df
    } else if (transformation == "log") {
        df <- df %>%
            mutate(outcome = log(outcome))
    } else if (transformation == "log1p") {
        df <- df %>%
            mutate(outcome = log1p(outcome))
    }

    # Remove ID with one obs
    one_obs_id_tbl <- df %>%
        group_by(id) %>%
        mutate(no_obs = n()) %>%
        filter(no_obs == 1) %>%
        pull(id)

    df <- df %>%
        group_by(id) %>%
        mutate(no_obs = n()) %>%
        filter(no_obs > 1) %>%
        select(-no_obs)

    # Future and prepared data
    df <- df %>%
        ungroup() %>%
        complete(date, id) %>%
        replace_na(list(outcome = 0)) %>%
        mutate(id = as_factor(id)) %>%
        group_by(id) %>%
        future_frame(date, .length_out = horizon, .bind_data = TRUE) %>%
        fill(-outcome, .direction = "down") %>%
        ungroup() %>%
        drop_na(-outcome)

    if (!is.null(xreg)) {
        df <- df %>%
            left_join(xreg, by = "date")
    }


    # Holidays
    if (use_holidays) {

        if(!is.null(holidays_to_use_2)) {
            df <- df %>%
                left_join(holidays_to_use_1) %>%
                left_join(holidays_to_use_2)

        } else {
            df <- df %>%
                left_join(holidays_to_use_1)

        }

    } else {
        df
    }


    # Fourier period
    if (use_own_fourier) {
        fourier_terms <- fourier_terms

    } else {
        if (use_abc_category) {
            fourier_periods <- df %>%
                filter(str_detect(abc, c("a|b"))) %>%
                group_by(id) %>%
                tk_acf_diagnostics(date, outcome) %>%
                ungroup() %>%
                filter(abs(PACF) > pacf_threshold) %>%
                count(lag) %>%
                arrange(desc(n)) %>%
                filter(lag > 1) %>%
                dplyr::slice(1:no_fourier_terms) %>%
                pull(lag)
        } else {
            fourier_periods <- df %>%
                group_by(id) %>%
                tk_acf_diagnostics(date, outcome) %>%
                ungroup() %>%
                filter(abs(PACF) > pacf_threshold) %>%
                count(lag) %>%
                arrange(desc(n)) %>%
                filter(lag > 1) %>%
                dplyr::slice(1:no_fourier_terms) %>%
                pull(lag)
        }

        fourier_periods <- c(fourier_periods, 52/2,  52)
        fourier_periods <- unique(fourier_periods)

        fourier_terms <- fourier_periods
    }



    # Full data
    if (recursive_data) {

        if (use_seasonal_lag) {
            if (use_abc_category) {
                full_data_tbl <- df %>%
                    arrange(id, abc, date) %>%
                    group_by(id) %>%
                    tk_augment_fourier(date, .periods = fourier_terms, .K = fourier_k) %>%
                    tk_augment_lags(.value = outcome, .lags = c(1:no_recursive_lag, seasonal_frequency - 1, seasonal_frequency, seasonal_frequency + 1)) %>%
                    tk_augment_slidify(
                        contains(paste0("outcome_lag", seasonal_frequency)),
                        .f = ~mean(.x, na.rm = TRUE),
                        .period  = slidify_period,
                        .partial = TRUE,
                        .align   = "center"
                    ) %>%
                    ungroup() %>%
                    rowid_to_column(var = "rowid")

            } else {
                full_data_tbl <- df %>%
                    arrange(id, date) %>%
                    group_by(id) %>%
                    tk_augment_fourier(date, .periods = fourier_terms, .K = fourier_k) %>%
                    tk_augment_lags(.value = outcome, .lags = c(1:no_recursive_lag, seasonal_frequency - 1, seasonal_frequency, seasonal_frequency + 1)) %>%
                    tk_augment_slidify(
                        contains(paste0("outcome_lag", seasonal_frequency)),
                        .f = ~mean(.x, na.rm = TRUE),
                        .period  = slidify_period,
                        .partial = TRUE,
                        .align   = "center"
                    ) %>%
                    ungroup() %>%
                    rowid_to_column(var = "rowid")
            }

        } else {

            if (use_abc_category) {
                full_data_tbl <- df %>%
                    arrange(id, abc, date) %>%
                    group_by(id) %>%
                    tk_augment_fourier(date, .periods = fourier_terms, .K = fourier_k) %>%
                    tk_augment_lags(.value = outcome, .lags = c(1:no_recursive_lag)) %>%
                    ungroup() %>%
                    rowid_to_column(var = "rowid")

            } else {
                full_data_tbl <- df %>%
                    arrange(id, date) %>%
                    group_by(id) %>%
                    tk_augment_fourier(date, .periods = fourier_terms, .K = fourier_k) %>%
                    tk_augment_lags(.value = outcome, .lags = c(1:no_recursive_lag)) %>%
                    ungroup() %>%
                    rowid_to_column(var = "rowid")
            }

        }


    } else {

        if (use_seasonal_lag) {
            if (use_abc_category) {
                full_data_tbl <- df %>%
                    arrange(id, abc, date) %>%
                    group_by(id) %>%
                    tk_augment_fourier(date, .periods = fourier_terms, .K = fourier_k) %>%
                    tk_augment_lags(.value = outcome, .lags = seasonal_frequency) %>%
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
                    arrange(id, date) %>%
                    group_by(id) %>%
                    tk_augment_fourier(date, .periods = fourier_terms, .K = fourier_k) %>%
                    tk_augment_lags(.value = outcome, .lags = seasonal_frequency) %>%
                    tk_augment_slidify(
                        contains("outcome_lag"),
                        .f = ~mean(.x, na.rm = TRUE),
                        .period  = slidify_period,
                        .partial = TRUE,
                        .align   = "center"
                    ) %>%
                    ungroup() %>%
                    rowid_to_column(var = "rowid")
            }

        } else {

            if (use_abc_category) {
                full_data_tbl <- df %>%
                    arrange(id, abc, date) %>%
                    group_by(id) %>%
                    tk_augment_fourier(date, .periods = fourier_terms, .K = fourier_k) %>%
                    ungroup() %>%
                    rowid_to_column(var = "rowid")

            } else {
                full_data_tbl <- df %>%
                    arrange(id, date) %>%
                    group_by(id) %>%
                    tk_augment_fourier(date, .periods = fourier_terms, .K = fourier_k) %>%
                    ungroup() %>%
                    rowid_to_column(var = "rowid")
            }

        }

    }





    # data prepared
    # drop_na
    if (drop_na) {
        data_prepared_tbl <- full_data_tbl %>%
            filter(!is.na(outcome)) %>%
            drop_na()
    } else if (fill_na_with_zero) {
        data_prepared_tbl <- full_data_tbl %>%
            filter(!is.na(outcome)) %>%
            mutate(across(.cols = contains("_lag"),
                          .fns  = ~ replace_na(.x, 0)))
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
        time_series_split(date, assess = horizon, cumulative = TRUE)


    # Train data
    if(clean) {
        if (use_holiday_to_clean) {

            # Create holiday variable to "unclean" series if there is a holiday
            date_variable <- seq.Date(from = as.Date("1990-01-01"),
                                      to   = as.Date("2040-12-31"),
                                      by   = "day")

            date_tbl <- tibble(date = date_variable)

            holiday_clean_date <- date_tbl %>%
                mutate(date = floor_date(date, "week", week_start = 1)) %>%
                distinct() %>%
                left_join(holiday_for_clean) %>%
                mutate(holiday = ifelse(is.na(value), 0, 1)) %>%
                mutate(lead_holiday = lead(holiday),
                       lag_holiday  = lag(holiday)) %>%
                replace(is.na(.), 0) %>%
                mutate(value = rowSums(. [2:4])) %>%
                mutate(value = ifelse(value == 0, 0, 1)) %>%
                select(-contains("holiday")) %>%
                rename("holiday" = "value")

            if (transformation == "log") {
                train_data <- training(splits) %>%
                    group_by(id) %>%
                    mutate(cumsum_zero = cumsum(outcome)) %>%
                    filter(cumsum_zero > 0) %>%
                    mutate(outcome = exp(outcome)) %>%
                    mutate(outcome_clean = ts_clean_vec(outcome)) %>%
                    left_join(holiday_clean_date) %>%
                    mutate(holiday = ifelse(is.na(holiday), 0, 1)) %>%
                    mutate(outcome = case_when(holiday == 1 ~ outcome,
                                               TRUE ~ outcome_clean)) %>%
                    select(-outcome_clean) %>%
                    mutate(outcome = log(outcome))

            } else if (transformation == "log1p") {
                train_data <- training(splits) %>%
                    group_by(id) %>%
                    mutate(cumsum_zero = cumsum(outcome)) %>%
                    filter(cumsum_zero > 0) %>%
                    mutate(outcome = expm1(outcome)) %>%
                    mutate(outcome_clean = ts_clean_vec(outcome)) %>%
                    left_join(holiday_clean_date) %>%
                    mutate(holiday = ifelse(is.na(holiday), 0, 1)) %>%
                    mutate(outcome = case_when(holiday == 1 ~ outcome,
                                               TRUE ~ outcome_clean)) %>%
                    select(-outcome_clean) %>%
                    mutate(outcome = log1p(outcome))

            } else {
                train_data <- training(splits) %>%
                    group_by(id) %>%
                    mutate(cumsum_zero = cumsum(outcome)) %>%
                    filter(cumsum_zero > 0) %>%
                    mutate(outcome_clean = ts_clean_vec(outcome)) %>%
                    left_join(holiday_clean_date) %>%
                    mutate(holiday = ifelse(is.na(holiday), 0, 1)) %>%
                    mutate(outcome = case_when(holiday == 1 ~ outcome,
                                               TRUE ~ outcome_clean)) %>%
                    select(-outcome_clean)
            }



        } else {

            if (transformation == "log") {
                train_data <- training(splits) %>%
                    group_by(id) %>%
                    mutate(cumsum_zero = cumsum(outcome)) %>%
                    filter(cumsum_zero > 0) %>%
                    mutate(outcome = exp(outcome)) %>%
                    mutate(outcome_clean = ts_clean_vec(outcome))

            } else if (transformation == "log1p") {
                train_data <- training(splits) %>%
                    group_by(id) %>%
                    mutate(cumsum_zero = cumsum(outcome)) %>%
                    filter(cumsum_zero > 0) %>%
                    mutate(outcome = expm1(outcome)) %>%
                    mutate(outcome_clean = ts_clean_vec(outcome))
            } else {
                train_data <- training(splits) %>%
                    group_by(id) %>%
                    mutate(cumsum_zero = cumsum(outcome)) %>%
                    filter(cumsum_zero > 0) %>%
                    mutate(outcome_clean = ts_clean_vec(outcome))
            }
        }


    } else {
        train_data <- training(splits)
    }

    return_list$full_data     <- full_data_tbl
    return_list$data_prepared <- data_prepared_tbl
    return_list$future_data   <- future_data
    return_list$splits        <- splits
    return_list$train_data    <- train_data
    return_list$horizon       <- horizon
    return_list$one_obs_id    <- one_obs_id_tbl
    return_list$fourier_terms <- fourier_terms

    return(return_list)

}

