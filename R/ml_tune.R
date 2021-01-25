#' Function to tune ML algos for multiple time series forecasting
#'
#' ml_tune() will tune up to nine different ML algorithms used in forecasting
#' @param parsnip_recipe Recipe for parsnip models
#' @param modeltime_recipe Recipe for modeltime models
#' @param vfold Number of folds used in K-fold cross validation
#' @param grid_size The size of the hyperparameter grid used for tuning the parameters
#' @param recursive Is the model recursive or not. Defaults to FALSE
#' @param return What do you want to return. List of workflows, modeltime table or both?
#' @param models Choose which models to use. Choose any combination of xgboost, rf, cubist, svm_rbf, svm_poly, glmnet, knn, mars or prophet_boost
#'



ml_tune <- function(parsnip_recipe, modeltime_recipe, vfold, grid_size, recursive = FALSE, return = c("modellist", "modeltable", "both"),
                    models = c("xgboost", "rf", "cubist", "svm_rbf", "svm_poly", "glmnet", "knn", "mars", "prophet_boost")
) {

    # Libraries
    require(tidyverse)
    require(timetk)
    require(modeltime)
    require(tidymodels)
    require(modeltime.ensemble)
    require(rules)
    require(future)
    require(doFuture)
    require(tictoc)

    # Cross validation
    #set.seed(123)

    resamples_kfold <- training(splits) %>% vfold_cv(v = vfold)

    model_list <- list()
    model_table <- modeltime_table()


    # XGBoost
    if ("xgboost" %in% models) {

        message("Start tuning XGBoost")
        tic()

        model_spec_xgboost_tune <- boost_tree(
            mode           = "regression",
            mtry           = tune(),
            trees          = 1000,
            min_n          = tune(),
            tree_depth     = tune(),
            learn_rate     = tune(),
            loss_reduction = tune()
        ) %>%
            set_engine("xgboost")

        wflw_fit_xgboost <- wflw_creator(model_spec_xgboost_tune, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, recursive)

        model_list$xgboost <- wflw_fit_xgboost

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_xgboost %>% modeltime_tabl)

        message("Finish tuning XGBoost")
        toc()
    }


    # Random forest
    if ("rf" %in% models) {

        message("Start tuning Random Forest")
        tic()

        model_spec_rf_tune <- rand_forest(
            mode  = "regression",
            mtry  = tune(),
            trees = 1000,
            min_n = tune()
        ) %>%
            set_engine("ranger")

        wflw_fit_rf <- wflw_creator(model_spec_rf_tune, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, recursive)
        model_list$rf <- wflw_fit_rf

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_rf %>% modeltime_table())

        message("Finish tuning Random Forest")
        toc()

    }


    # Cubist
    if ("cubist" %in% models) {

        message("Start tuning Cubist")
        tic()

        model_spec_cubist_tune <- cubist_rules(
            mode       = "regression",
            committees = tune(),
            neighbors  = tune(),
            max_rules  = tune()
        ) %>%
            set_engine("Cubist")

        wflw_fit_cubist <- wflw_creator(model_spec_cubist_tune, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, recursive)
        model_list$cubist <- wflw_fit_cubist

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_cubist %>% modeltime_table())

        message("Finish tuning Cubist")
        toc()

    }


    # SVM - Radial basis
    if ("svm_rbf" %in% models) {

        message("Start tuning SVM (radial basis)")
        tic()

        model_spec_svm_rbf_tune <- svm_rbf(
            mode      = "regression",
            cost      = tune(),
            rbf_sigma = tune(),
            margin    = tune()
        ) %>%
            set_engine("kernlab")

        wflw_fit_svm_rbf <- wflw_creator(model_spec_svm_rbf_tune, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, recursive)
        model_list$svm_rbf <- wflw_fit_svm_rbf

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_svm_rbf %>% modeltime_table())

        message("Finish tuning SVM (radial basis")
        toc()

    }


    # SVM - Polynomial
    if ("svm_poly" %in% models) {

        message("Start tuning SVM (polynomial)")
        tic()

        model_spec_svm_poly_tune <- svm_poly(
            mode         = "regression",
            cost         = tune(),
            degree       = tune(),
            scale_factor = tune(),
            margin       = tune()
        ) %>%
            set_engine("kernlab")

        wflw_fit_svm_poly <- wflw_creator(model_spec_svm_poly_tune, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, recursive)
        model_list$svm_poly <- wflw_fit_svm_poly

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_svm_poly %>% modeltime_table())

        message("Finish tuning SVM (polynomial")
        toc()

    }


    # Elastic Net
    if ("glmnet" %in% models) {

        message("Start tuning Elastic Net")
        tic()

        model_spec_glmnet <- linear_reg(
            mode    = "regression",
            penalty = tune(),
            mixture = tune()
        ) %>%
            set_engine("glmnet")

        wflw_fit_glmnet <- wflw_creator(model_spec_glmnet, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, recursive)
        model_list$glmnet <- wflw_fit_glmnet

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_glmnet %>% modeltime_table())

        message("Finish tuning Elastic net")
        toc()

    }


    # KNN
    if ("knn" %in% models) {

        message("Start tuning KNN")
        tic()

        model_spec_knn <- nearest_neighbor(
            mode        = "regression",
            neighbors   = tune(),
            dist_power  = tune(),
            weight_func = "optimal"
        ) %>%
            set_engine("kknn")

        wflw_fit_knn <- wflw_creator(model_spec_knn, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, recursive)
        model_list$knn <- wflw_fit_knn

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_knn %>% modeltime_table())

        message("Finish tuning KNN")
        toc()

    }


    # MARS
    if ("mars" %in% models) {

        message("Start tuning MARS")
        tic()

        model_spec_mars <- mars(
            mode        = "regression",
            num_terms   = tune(),
            prod_degree = tune()
        ) %>%
            set_engine("earth")

        wflw_fit_mars <- wflw_creator(model_spec_mars, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, recursive)
        model_list$mars <- wflw_fit_mars

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_mars %>% modeltime_table())

        message("Finish tuning MARS")
        toc()

    }


    # Prophet_boost
    if ("prophet_boost" %in% models) {

        message("Start tuning Prophet Boost")
        tic()

        model_spec_prophet <- prophet_boost(
            mode = "regression",
            changepoint_num    = tune(),
            changepoint_range  = tune(),
            seasonality_yearly = FALSE,
            seasonality_weekly = FALSE,

            mtry           = tune(),
            trees          = 1000,
            min_n          = tune(),
            tree_depth     = tune(),
            learn_rate     = tune(),
            loss_reduction = tune()
        ) %>%
            set_engine("prophet_xgboost")

        wflw_fit_prophet_boost <- wflw_creator(model_spec_prophet, modeltime_recipe, resamples_kfold = resamples_kfold, grid_size, recursive)
        model_list$prophet_boost <- wflw_fit_prophet_boost

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_prophet_boost %>% modeltime_table())

        message("Finish tuning Prophet Boost")
        toc()
    }


    # Return
    return_list <- list()
    return_list$models <- model_list
    return_list$modeltime_table <- model_table


    if (return == "modellist") {
        return(model_list)
    } else if (return == "modeltable") {
        return(model_table)
    } else {
        return(return_list)
    }

}
