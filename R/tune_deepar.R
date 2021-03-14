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
#' @param multiple_gpu Should more than one GPU be used
#' @param no_gpu How many, if more than one, should be used
#' @param min_obs_cv_train Minimum observation in the training set during cross validation


tune_deepar <- function(id, freq, recipe, horizon, splits, length, cv_slice_limit, most_important = TRUE, assess = "12 weeks",
                        skip = "4 weeks", initial = "12 months", multiple_gpu = FALSE, no_gpu, min_obs_cv_train = 1,
                        epochs = NULL, lookback = NULL, batch_size = NULL, learn_rate = NULL,
                        num_cells = NULL, num_layers = NULL, scale = NULL, dropout = NULL) {


    if (most_important) {
        gluonts_grid <- data.frame(
            epochs          = if (is.null(epochs)) sample(100, size = length, replace = TRUE) else epochs,
            lookback_length = if (is.null(lookback)) sample(1:7 * horizon, size = length, replace = TRUE) else lookback,
            batch_size      = if (is.null(batch_size)) sample(seq(32, 512, 32), size = length, replace = TRUE) else batch_size,
            learn_rate      = if (is.null(learn_rate)) runif(length, min = 1e-4, max = 1e-1) else learn_rate,
            num_cells       = 40,
            num_layers      = 2,
            scale           = FALSE,
            dropout         = 0.1
        )
    } else {
        gluonts_grid <- data.frame(
            epochs          = if (is.null(epochs)) sample(100, size = length, replace = FALSE) else  epochs,
            lookback_length = if (is.null(lookback)) sample(1:7 * horizon, size = length, replace = TRUE) else lookback,
            batch_size      = if (is.null(batch_size)) sample(seq(32, 512, 32), size = length, replace = TRUE) else batch_size,
            learn_rate      = if (is.null(learn_rate)) runif(length, min = 1e-4, max = 1e-1) else learn_rate,
            num_cells       = if (is.null(num_cells)) sample(30:100, size = length, replace = TRUE) else num_cells,
            num_layers      = if (is.null(num_layers)) sample(1:8, size = length, replace = TRUE) else num_layers,
            scale           = if (is.null(scale)) sample(c(TRUE, FALSE), size = length, replace = TRUE) else scale,
            dropout         = if (is.null(dropout)) runif(length, min = 0, max = 0.2) else dropout
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
    wflw_list   <- list()
    wflw_return <- list()

    # Create accuracy log file
    if(!dir.exists("accuracy_log")) {
        dir.create("accuracy_log")
    }

    log_accuracy_file_name <- paste0("log_accuracy", "_", timestamp(prefix = "", suffix = "", quiet = TRUE), ".csv")
    path_to_file <- paste0("accuracy_log/", log_accuracy_file_name)
    path_to_file <- gsub(" ", "_", path_to_file)
    path_to_file <- gsub(":", "_", path_to_file)


    for(i in 1:nrow(gluonts_grid)) {
        message("")
        message(str_glue("Parameter set number {i} of {nrow(gluonts_grid)}"))
        message(str_glue("Epochs: {gluonts_grid$epochs[i]}"))
        message(str_glue("Lookback: {gluonts_grid$lookback_length[i]}"))
        message(str_glue("Batch size: {gluonts_grid$batch_size[i]}"))
        message(str_glue("Learning rate: {gluonts_grid$learn_rate[i]}"))
        message(str_glue("Number of cells: {gluonts_grid$num_cells[i]}"))
        message(str_glue("Number of layers: {gluonts_grid$num_layers[i]}"))
        message(str_glue("Scale: {gluonts_grid$scale[i]}"))
        message(str_glue("Dropout: {gluonts_grid$dropout[i]}"))

        if(multiple_gpu) {

            ctx_list = if (no_gpu == 2) {
                list(mxnet$gpu(0), mxnet$gpu(1))
            } else if (no_gpu == 3) {
                list(mxnet$gpu(0), mxnet$gpu(1), mxnet$gpu(2))
            } else {
                list(mxnet$gpu(0), mxnet$gpu(1), mxnet$gpu(2), mxnet$gpu(3))
            }


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
                dropout           = gluonts_grid$dropout[i],
                cell_type         = "lstm"
            ) %>%
                set_engine("gluonts_deepar", ctx = ctx_list)

        } else {
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
                dropout           = gluonts_grid$dropout[i],
                cell_type         = "lstm"
            ) %>%
                set_engine("gluonts_deepar")
        }


        for (j in 1:cv_slice_limit) {

            id_to_remove <- training(resamples_tscv$splits[[j]]) %>%
                group_by(id) %>%
                summarise(no = n_distinct(date)) %>%
                filter(no == min_obs_cv_train) %>%
                pull(id)

            wflw_fit_deepar_1 <- workflow() %>%
                add_model(model_spec) %>%
                add_recipe(recipe) %>%
                fit(training(resamples_tscv$splits[[j]]) %>% filter(!id %in% id_to_remove))

            cv_accuracy <- wflw_fit_deepar_1 %>%
                modeltime_table() %>%
                modeltime_accuracy(testing(resamples_tscv$splits[[j]])) %>%
                add_column(fold = paste0("fold_", j))

            cv_accuracy_summary <- cv_accuracy %>%
                group_by(.model_id, .model_desc, fold) %>%
                summarise(mae   = mean(mae, na.rm = TRUE),
                          mape  = mean(mape, na.rm = TRUE),
                          mase  = mean(mase, na.rm = TRUE),
                          smape = mean(smape, na.rm = TRUE),
                          rmse  = mean(rmse, na.rm = TRUE),
                          rsq   = mean(rsq, na.rm = TRUE))

            cv_list[[j]] <- cv_accuracy_summary %>% bind_cols(gluonts_grid[i,])
        }

        deepar_list[[i]] <- bind_rows(cv_list)
        wflw_return[[i]] <- wflw_fit_deepar_1



        if(!file.exists(path_to_file)) {
            message("Writing data to {path_to_file}")
            bind_rows(deepar_list) %>% readr::write_csv(path_to_file)

        } else {
            message("Reading in old data and writing new to {path_to_file}")
            old_file <- readr::read_csv(path_to_file)
            new_file <- bind_rows(bind_rows(deepar_list), old_file)
            new_file %>% readr::write_csv(path_to_file)

        }

    }

    deepar_list      <- bind_rows(deepar_list)
    best_model_index <- which(deepar_list$rmse == min(deepar_list$rmse))
    best_model       <- wflw_return[[best_model_index]]

    return_list <- list()
    return_list$deepar_list <- deepar_list
    return_list$best_model  <- best_model

    return(return_list)
}
