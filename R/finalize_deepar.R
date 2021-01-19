#' Function to select best DeepAR based on chosen metric
#'
#' @param accuracy_table Table from tune_deepar()
#' @param id A quoted column name that tracks the GluonTS FieldName "item_id"
#' @param freq A pandas timeseries frequency such as "5min" for 5-minutes or "D" for daily.
#' @param recipe GluonTS recipe
#' @param split objects
#' @param metric The metric used to choose the best model


finalize_deepar <- function(accuracy_table, id, freq, recipe, splits, metric = "rmse") {


    if (metric == "rmse") {
        best_tune <- accuracy_table %>% filter(rmse == min(rmse))
    } else {
        best_tune <- accuracy_table %>% filter(mase == min(mase))
    }

    model_spec <- deep_ar(
        id                = id,
        freq              = freq,
        prediction_length = horizon,
        lookback_length   = best_tune$lookback_length,
        batch_size        = best_tune$batch_size,
        learn_rate        = best_tune$learn_rate,
        num_cells         = best_tune$num_cells,
        scale             = best_tune$scale,
        epochs            = best_tune$epochs,
        num_layers        = best_tune$num_layers,
        cell_type         = "lstm"
    ) %>%
        set_engine("gluonts_deepar")

    wflw_fit_deepar <- workflow() %>%
        add_model(model_spec) %>%
        add_recipe(recipe) %>%
        fit(training(splits))

    return(wflw_fit_deepar)

}
