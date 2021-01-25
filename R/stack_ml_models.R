#' Creates super-learner by stacking differnt ML models
#'
#' @param submodels_resamples A modeltime table (mdl_time_tbl) object with a column containing resample results. Created with modeltime_fit_resamples() from the modeltime.resample package
#' @param kfold Number of folds in K-fold cross validation
#' @param grid Size of grid to tune the meta-learner. Gets passed to tune::tune_grid()
#' @param models Which models to try for the meta-learner. Possible choices: glmnet, rf, nnet, xgboost, cubist and svm
#' @param parallel Should the calculations be run in parallel. Defaults to TRUE
#' @param verbose A logical for logging results as they are generated. Defaults to TRUE
#' @return A list of length two with ensemble accuracy and a modeltime_table to be used for forecasting

stack_ml_models <- function(submodels_resamples, kfold, grid, models, parallel = TRUE, verbose = TRUE) {

    ensemble_accuracy_tbl <- tibble()
    ensemble_modeltime_tbl <- modeltime_table()

    n_models <- nrow(submodels_resamples)


    # Elastic net stack
    if ("glmnet" %in% models) {
        ensemble_fit_glmnet_kfold <- submodels_resamples %>%
            ensemble_model_spec(
                model_spec = linear_reg(
                    penalty = tune(),
                    mixture = tune()
                ) %>%
                    set_engine("glmnet"),
                kfolds = kfold,
                grid   = grid,
                control = control_grid(
                    verbose = verbose,
                    allow_par = parallel
                )
            ) %>%
            modeltime_table()

        ensemble_modeltime_tbl <- ensemble_modeltime_tbl %>%
            combine_modeltime_tables(ensemble_fit_glmnet_kfold)

        ensemble_glmnet_accuracy <- ensemble_modeltime_tbl %>%
            modeltime_accuracy(testing(splits))

        ensemble_tbl <- ensemble_accuracy_tbl %>%
            bind_rows(ensemble_glmnet_accuracy)
    }


    # Random forest stack
    if ("rf" %in% models) {
        ensemble_fit_ranger_kfold <- submodels_resamples %>%
            ensemble_model_spec(
                model_spec = rand_forest(
                    trees = tune(),
                    min_n = tune(),
                    mtry  = tune()
                ) %>%
                    set_engine("ranger"),

                kfolds = kfold,
                grid   = grid,
                param_info = parameters(
                    list(
                        mtry = mtry(range = c(1, n_models)),
                        trees = trees(),
                        min_n = min_n()
                    )
                ),
                control = control_grid(verbose = verbose,
                                       allow_par = parallel)
            ) %>%
            modeltime_table()

        ensemble_modeltime_tbl <- ensemble_modeltime_tbl %>%
            combine_modeltime_tables(ensemble_fit_ranger_kfold)

        ensemble_rf_accuracy <- ensemble_fit_ranger_kfold %>%
            modeltime_accuracy(testing(splits))

        ensemble_tbl <- ensemble_tbl %>%
            bind_rows(ensemble_rf_accuracy)
    }


    # Neural network stack
    if ("nnet" %in% models) {
        ensemble_fit_nnet_kfold <- submodels_resamples %>%
            ensemble_model_spec(
                model_spec = mlp(
                    hidden_units = tune(),
                    penalty = tune(),
                    epochs  = tune(),
                    dropout = tune()
                ) %>%
                    set_engine("nnet"),
                kfolds = kfold,
                grid   = grid,
                control = control_grid(verbose = TRUE, allow_par = TRUE)
            ) %>%
            modeltime_table()

        ensemble_modeltime_tbl <- ensemble_modeltime_tbl %>%
            combine_modeltime_tables(ensemble_fit_nnet_kfold)

        ensemble_nnet_accuracy <- ensemble_fit_nnet_kfold %>%
            modeltime_accuracy(testing(splits))

        ensemble_tbl <- ensemble_tbl %>%
            bind_rows(ensemble_nnet_accuracy)
    }


    # XGBoost stack
    if ("xgboost" %in% models) {
        ensemble_fit_xgb_kfold <- submodels_resamples %>%
            ensemble_model_spec(
                model_spec = boost_tree(
                    mtry           = tune(),
                    trees          = tune(),
                    min_n          = tune(),
                    tree_depth     = tune(),
                    learn_rate     = tune(),
                    loss_reduction = tune(),
                    sample_size    = tune()
                ) %>%
                    set_engine("xgboost"),

                kfolds = kfold,
                grid   = grid,
                param_info = parameters(
                    list(
                        mtry           = mtry(range = c(1, n_models)),
                        trees          = trees(),
                        min_n          = min_n(),
                        tree_depth     = tree_depth(),
                        learn_rate     = learn_rate(),
                        loss_reduction = loss_reduction(),
                        sample_size    = sample_size(range = c(0, 1))
                    )
                ),
                control = control_grid(verbose = verbose,
                                       allow_par = parallel)
            ) %>%
            modeltime_table()

        ensemble_modeltime_tbl <- ensemble_modeltime_tbl %>%
            combine_modeltime_tables(ensemble_fit_xgb_kfold)

        ensemble_xgb_accuracy <- ensemble_fit_xgb_kfold %>%
            modeltime_accuracy(testing(splits))

        ensemble_tbl <- ensemble_tbl %>%
            bind_rows(ensemble_xgb_accuracy)
    }


    # Cubist stack
    if ("cubist" %in% models) {
        ensemble_fit_cubist_kfold <- submodels_resamples %>%
            ensemble_model_spec(
                model_spec = cubist_rules(
                    committees = tune(),
                    neighbors  = tune(),
                    max_rules  = tune()
                ) %>%
                    set_engine("Cubist"),
                kfolds = kfold,
                grid   = grid,
                control = control_grid(verbose = verbose,
                                       allow_par = parallel)
            ) %>%
            modeltime_table()

        ensemble_modeltime_tbl <- ensemble_modeltime_tbl %>%
            combine_modeltime_tables(ensemble_fit_cubist_kfold)

        ensemble_cubist_accuracy <- ensemble_fit_cubist_kfold %>%
            modeltime_accuracy(testing(splits))

        ensemble_tbl <- ensemble_tbl %>%
            bind_rows(ensemble_cubist_accuracy)
    }


    # SVM stack
    if ("svm" %in% models) {
        ensemble_fit_svm_kfold <- submodels_resamples %>%
            ensemble_model_spec(
                model_spec = svm_rbf(
                    mode      = "regression",
                    cost      = tune(),
                    rbf_sigma = tune(),
                    margin    = tune()
                ) %>%
                    set_engine("kernlab"),

                kfolds  = kfold,
                grid    = grid,
                control = control_grid(verbose = verbose, allow_par = parallel)
            ) %>%
            modeltime_table()

        ensemble_modeltime_tbl <- ensemble_modeltime_tbl %>%
            combine_modeltime_tables(ensemble_fit_svm_kfold)

        ensemble_svm_accuracy <- ensemble_fit_svm_kfold %>%
            modeltime_accuracy(testing(splits))

        ensemble_tbl <- ensemble_tbl %>%
            bind_rows(ensemble_svm_accuracy)
    }

    # RETURN
    return_list <- list()
    return_list$accuracy_tbl  <- ensemble_tbl
    return_list$modeltime_tbl <- ensemble_modeltime_tbl

    return(return_list)

}
