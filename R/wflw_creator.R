#' A helper function to create workflow passed to the ml_tune() function
#'
#' wflw_creater() is a function that creates workflow
#'
#' @param model_spec A model specification created with the Parsnip or Modeltime package
#' @param ml_recipe Recipe for the models
#' @param resamples_kfold Resamples used for tuning parameters
#' @param grid_size The size of the grid of parameters
#'
#'

wflw_creator <- function(model_spec, ml_recipe, resamples_kfold, grid_size = grid_size, parallel_type, learn_rate, min_n, tree_depth, loss_reduction) {

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
                    sample_size    = sample_prop(range = c(0, 1)),
                    learn_rate     = if (is.null(learn_rate)) learn_rate(range = c(-10, -1), trans = log10_trans()) else learn_rate(range = c(learn_rate[1], learn_rate[2]), trans = log10_trans()),
                    min_n          = if (is.null(min_n)) min_n(range = c(2L, 40L), trans = NULL) else min_n(range = c(min_n[1], min_n[2]), trans = NULL),
                    tree_depth     = if (is.null(tree_depth)) tree_depth(range = c(1L, 15L), trans = NULL) else tree_depth(range = c(tree_depth[1], tree_depth[2]), trans = NULL),
                    loss_reduction = if (is.null(loss_reduction)) loss_reduction(range = c(-10, 1.5), trans = log10_trans()) else loss_reduction(range = c(loss_reduction[1], loss_reduction[2]), trans = log10_trans())

                ),
            grid       = grid_size,
            control    = control_grid(verbose = TRUE, allow_par = TRUE, parallel_over = parallel_type)
        )

    } else {
        tune_results <- tune_grid(
            object     = wflw,
            resamples  = resamples_kfold,
            param_info = parameters(wflw) %>%
                update(
                    learn_rate     = if (is.null(learn_rate)) learn_rate(range = c(-10, -1), trans = log10_trans()) else learn_rate(range = c(learn_rate[1], learn_rate[2]), trans = log10_trans()),
                    min_n          = if (is.null(min_n)) min_n(range = c(2L, 40L), trans = NULL) else min_n(range = c(min_n[1], min_n[2]), trans = NULL),
                    tree_depth     = if (is.null(tree_depth)) tree_depth(range = c(1L, 15L), trans = NULL) else tree_depth(range = c(tree_depth[1], tree_depth[2]), trans = NULL),
                    loss_reduction = if (is.null(loss_reduction)) loss_reduction(range = c(-10, 1.5), trans = log10_trans()) else loss_reduction(range = c(loss_reduction[1], loss_reduction[2]), trans = log10_trans())

                ),
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
