#' Function to tune N-BEATS
#'
#' @param id A quoted column name that tracks the GluonTS FieldName "item_id"
#' @param freq A pandas timeseries frequency such as "5min" for 5-minutes or "D" for daily.
#' @param recipe A gluonts recipe
#' @param horizon The forecast horizon
#' @param length The number of distinct hyperparameter for each tunable parameter except loss_function which is set to MASE
#' @param cv_slice_limit How many slice/folds in the tsCV
#' @param assess The number of samples used for each assessment resample
#' @param skip A integer indicating how many (if any) additional resamples to skip to thin the total amount of data points in the analysis resample.
#' @param initial The number of samples used for analysis/modeling in the initial resample.
#' @param epochs Number of epochs
#' @param lookback Lookback length. If NULL, will be randomly chosen
#' @param batch_size Number of examples in each batch
#' @param bagging_size The number of models that share the parameter combination of 'context_length' and 'loss_function'.
#' @param learn_rate Learning rate
#' @param loss_function Any of MASE, MAPE sMAPE. Defaults to MASE when loss_function = NULL
#' @param scale Scales numeric data by id group using mean = 0, standard deviation = 1 transformation.
#'
#'

tune_nbeats <- function(id, freq, recipe, splits, horizon, length, cv_slice_limit, assess = "12 weeks",
                        skip = "4 weeks", initial = "12 months", epochs = NULL, lookback = NULL, bagging_size = NULL,
                        learn_rate = NULL, loss_function = NULL, scale = NULL, batch_size = NULL) {


    nbeats_grid <- data.frame(
        epochs          = if (is.null(epochs)) sample(100, size = length, replace = TRUE) else epochs,
        lookback_length = if (is.null(lookback)) sample(1:7, size = length, replace = TRUE) else lookback,
        batch_size      = if (is.null(batch_size)) round(runif(length, min = 32, max = 1024), 0) else batch_size,
        learn_rate      = if (is.null(learn_rate)) runif(length, min = 1e-5, max = 1e-2) else learn_rate,
        loss_function   = if (is.null(loss_function)) "MASE" else loss_function,
        scale           = if (is.null(scale)) sample(c(TRUE, FALSE), length, replace = TRUE) else scale,
        bagging_size    = if (is.null(bagging_size)) sample(1:10, size = length, replace = TRUE) else bagging_size
    )


    nbeats_grid <- distinct(nbeats_grid)

    resamples_tscv <- time_series_cv(
        data =  training(splits),
        cumulative  = TRUE,
        initial     = initial,
        assess      = assess,
        skip        = skip,
        slice_limit = cv_slice_limit
    )

    model_table <- modeltime_table()

    nbeats_list <- list()
    cv_list     <- list()


    for(i in 1:nrow(nbeats_grid)) {

        message(str_glue("Parameter set number {i} of {nrow(nbeats_grid)}"))
        message(str_glue("Epochs: {nbeats_grid$epochs[[i]]}"))
        message(str_glue("Number of different lookback periods: {nbeats_grid$lookback_length[i]}"))
        message(str_glue("Batch size: {nbeats_grid$batch_size[i]}"))
        message(str_glue("Learning rate: {nbeats_grid$learn_rate[i]}"))
        message(str_glue("Scale: {nbeats_grid$scale[i]}"))
        message(str_glue("Bagging: {nbeats_grid$bagging_size[i]}"))


        model_spec <- nbeats(
            id                = id,
            freq              = freq,
            prediction_length = horizon,
            epochs            = nbeats_grid$epochs[i],
            lookback_length   = 1:nbeats_grid$lookback_length[i] * horizon,
            batch_size        = nbeats_grid$batch_size[i],
            learn_rate        = nbeats_grid$learn_rate[i],
            loss_function     = nbeats_grid$loss_function[i],
            scale             = nbeats_grid$scale[i],
            bagging_size      = nbeats_grid$bagging_size[i],
        ) %>%
            set_engine("gluonts_nbeats_ensemble")

        for (j in 1:cv_slice_limit) {

            byrja <- Sys.time()

            wflw_fit_nbeats <- workflow() %>%
                add_model(model_spec) %>%
                add_recipe(recipe) %>%
                fit(training(resamples_tscv$splits[[j]]))

            cv_accuracy <- wflw_fit_nbeats %>%
                modeltime_table() %>%
                modeltime_accuracy(testing(resamples_tscv$splits[[j]])) %>%
                add_column(fold = paste("fold_", j))

            enda <- Sys.time() - byrja
            cv_accuracy$tune_time <- enda

            cv_list[[j]] <- cv_accuracy %>% bind_cols(nbeats_grid[i,])
        }

        nbeats_list[[i]] <- bind_rows(cv_list)

    }

    nbeats_list <- bind_rows(nbeats_list)

    return(nbeats_list)

}
