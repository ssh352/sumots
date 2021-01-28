#' Function to tune DeepAR
#'
#' @param id A quoted column name that tracks the GluonTS FieldName "item_id"
#' @param freq A pandas timeseries frequency such as "5min" for 5-minutes or "D" for daily.
#' @param recipe A gluonts recipe
#' @param horizon The forecast horizon
#' @param length The number of distinct hyperparameter for each tunable parameter
#' @param cv_slice_limit How many slice/folds in the tsCV
#' @param assess The number of samples used for each assessment resample
#' @param skip A integer indicating how many (if any) additional resamples to skip to thin the total amount of data points in the analysis resample.
#' @param initial The number of samples used for analysis/modeling in the initial resample.
#' @param epochs Number of epochs. Importance 1 of 7
#' @param lookback Lookback length. If NULL, will be randomly chosen. Importance 2 of 7
#' @param batch_size batch_size Number of examples in each batch. Importance 3 of 7
#' @param learn_rate Learning rate. Importance 4 of 7
#' @param num_cells Number of RNN cells for each layer. Importance 5 of 7
#' @param num_layers Number of RNN layers. No info on importance
#' @param scale Scales numeric data by id group using mean = 0, standard deviation = 1 transformation. No info on importance


tune_deepar <- function(id, freq, recipe, horizon, splits, length, cv_slice_limit, most_important = TRUE, assess = "12 weeks",
                        skip = "4 weeks", initial = "12 months", epochs = NULL, lookback = NULL, batch_size = NULL,
                        learn_rate = NULL, num_cells = NULL, num_layers = NULL, scale = NULL) {


    if (most_important) {
        gluonts_grid <- data.frame(
            epochs          = if (is.null(epochs)) sample(100, size = length, replace = TRUE) else epochs,
            lookback_length = if (is.null(lookback)) sample(1:7 * horizon, size = length, replace = TRUE) else lookback,
            batch_size      = if (is.null(batch_size)) round(runif(length, min = 32, max = 512), 0) else batch_size,
            learn_rate      = if (is.null(learn_rate)) runif(length, min = 1e-4, max = 1e-1) else learn_rate,
            num_cells       = 40,
            num_layers      = 2,
            scale           = FALSE
        )
    } else {
        gluonts_grid <- data.frame(
            epochs          = if (is.null(epochs)) sample(100, size = length, replace = TRUE) else  epochs,
            lookback_length = if (is.null(lookback)) sample(1:7 * horizon, size = length, replace = TRUE) else lookback,
            batch_size      = if (is.null(batch_size)) round(runif(length, min = 32, max = 512), 0) else batch_size,
            learn_rate      = if (is.null(learn_rate)) runif(length, min = 1e-4, max = 1e-1) else learn_rate,
            num_cells       = if (is.null(num_cells)) sample(30:200, size = length, replace = TRUE) else num_cells,
            num_layers      = if (is.null(num_layers)) sample(1:8, size = length, replace = TRUE) else num_layers,
            scale           = if (is.null(scale)) sample(c(TRUE, FALSE), size = length, replace = TRUE) else scale
        )
    }

    gluonts_grid <- distinct(gluonts_grid)

    resamples_tscv <- time_series_cv(
        data = training(splits),
        cumulative  = TRUE,
        initial     = initial,
        assess      = assess,
        skip        = skip,
        slice_limit = cv_slice_limit
    )

    model_table <- modeltime_table()

    deepar_list <- list()
    cv_list     <- list()

    for(i in 1:nrow(gluonts_grid)) {

        message(str_glue("Parameter set number {i} of {nrow(gluonts_grid)}"))
        message(str_glue("Epochs: {gluonts_grid$epochs[i]}"))
        message(str_glue("Lookback: {gluonts_grid$lookback_length[i]}"))
        message(str_glue("Batch size: {gluonts_grid$batch_size[i]}"))
        message(str_glue("Learning rate: {gluonts_grid$learn_rate[i]}"))
        message(str_glue("Number of cells: {gluonts_grid$num_cells[i]}"))
        message(str_glue("Number of layers: {gluonts_grid$num_layers[i]}"))
        message(str_glue("Scale: {gluonts_grid$scale[i]}"))



        model_spec <- deep_ar(
            id                = id,
            freq              = freq,
            prediction_length = horizon,
            epochs            = gluonts_grid$epochs[i],
            lookback_length   = gluonts_grid$lookback_length[i],
            batch_size        = gluonts_grid$batch_size[i],
            learn_rate        = gluonts_grid$learn_rate[i],
            num_cells         = gluonts_grid$num_cells[i],
            num_layers        = gluonts_grid$num_layers[i],
            scale             = gluonts_grid$scale[i],
            cell_type         = "lstm",
        ) %>%
            set_engine("gluonts_deepar")

        for (j in 1:cv_slice_limit) {

            wflw_fit_deepar_1 <- workflow() %>%
                add_model(model_spec) %>%
                add_recipe(recipe) %>%
                fit(training(resamples_tscv$splits[[j]]))

            cv_accuracy <- wflw_fit_deepar_1 %>%
                modeltime_table() %>%
                modeltime_accuracy(testing(resamples_tscv$splits[[j]])) %>%
                add_column(fold = paste("fold_", j))

            cv_list[[j]] <- cv_accuracy %>% bind_cols(gluonts_grid[i,])
        }

        deepar_list[[i]] <- bind_rows(cv_list)

    }

    deepar_list <- bind_rows(deepar_list)

    return(deepar_list)
}
