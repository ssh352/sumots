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

wflw_creator <- function(model_spec, ml_recipe, resamples_kfold, grid_size = grid_size, parallel_type) {

    return_list <- list()

    engine <- model_spec$engine

    wflw <- workflow() %>%
        add_model(model_spec) %>%
        add_recipe(ml_recipe)

    # tune_results <- tune_grid(
    #     object     = wflw,
    #     resamples  = resamples_kfold,
    #     param_info = parameters(wflw),
    #     grid       = grid_size,
    #     control    = control_grid(verbose = TRUE, allow_par = TRUE, parallel_over = parallel_type)
    # )

    if(engine %in% c("lightgbm", "catboost")) {
        tune_results <- tune_grid(
            object     = wflw,
            resamples  = resamples_kfold,
            param_info = parameters(wflw) %>%
                update(
                    sample_size = sample_prop(range = c(0, 1))
                ),
            grid       = grid_size,
            control    = control_grid(verbose = TRUE, allow_par = TRUE, parallel_over = parallel_type)
        )

    } else {
        tune_results <- tune_grid(
            object     = wflw,
            resamples  = resamples_kfold,
            param_info = parameters(wflw),
            grid       = grid_size,
            control    = control_grid(verbose = TRUE, allow_par = TRUE, parallel_over = parallel_type)
        )

    }

    best_results <- tune_results %>%
        show_best(metric = "rmse", n = 1)

    tune_plot <- tune_results %>%
        autoplot()

    fin_wflw <- wflw %>%
        finalize_workflow(parameters = best_results %>% dplyr::slice(1))

    wflw_fit <- fin_wflw %>%
        fit(training(splits))

    return_list$fitted_workflow    <- wflw_fit
    return_list$finalized_workflow <- fin_wflw
    return_list$tune_plot          <- tune_plot


    return(return_list)

}
