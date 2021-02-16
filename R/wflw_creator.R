#' A helper function to create workflow passed to the ml_tune() function
#'
#' wflw_creater() is a function that creates workflow
#'
#' @param model_spec A model specification created with the Parsnip or Modeltime package
#' @param ml_recipe Recipe for the models
#' @param resamples_kfold Resamples used for tuning parameters
#' @param grid_size The size of the grid of parameters
#' @export
#'

wflw_creator <- function(model_spec, ml_recipe, resamples_kfold, grid_size = grid_size, parallel_over = parallel_over) {

    wflw <- workflow() %>%
        add_model(model_spec) %>%
        add_recipe(ml_recipe)

    tune_results <- tune_grid(
        object     = wflw,
        resamples  = resamples_kfold,
        param_info = parameters(wflw),
        grid       = grid_size,
        control    = control_grid(verbose = TRUE, allow_par = TRUE, parallel_over = parallel_over)
    )

    best_results <- tune_results %>%
        show_best(metric = "rmse", n = 1)

    # Lag transformer
    lag_transformer <- function(data) {
        data %>%
            tk_augment_lags(outcome, .lags = 1:horizon)
    }

    wflw_fit <- wflw %>%
        finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
        fit(training(splits))


    return(wflw_fit)

}
