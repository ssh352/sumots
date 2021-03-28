#' Function to tune ML algos for multiple time series forecasting
#'
#' ml_tune() will tune up to nine different ML algorithms used in forecasting
#' @param parsnip_recipe Recipe for parsnip models
#' @param modeltime_recipe Recipe for modeltime models
#' @param vfold Number of folds used in K-fold cross validation
#' @param grid_size The size of the hyperparameter grid used for tuning the parameters
#' @param return What do you want to return. List of workflows, modeltime table or both?
#' @param models Choose which models to use. Choose any combination of xgboost, rf, cubist, svm_rbf, svm_poly, glmnet, knn, mars or prophet_boost
#' @param parallel_over A single string containing either "resamples" or "everything" describing how to use parallel processing. See ?control_grid
#' @param cv_repeats How many CV repeats to use
#' @param learn_rate Upper and lower bound of learning rate to try out during tuning. NULL equals default values from the dials package
#' @param min_n Upper and lower bound of min_n to try out during tuning. NULL equals default values from the dials package
#' @param tree_depth Upper and lower bound of tree_depth to try out during tuning. NULL equals default values from the dials package
#' @param loss_reduction Upper and lower bound of loss_reduction to try out during tuning. NULL equals default values from the dials package
#' @param include_simple_model_ensemble Should simple average of all the models be included? Defaults to TRUE
#' @param top_ensemble Creates an ensemble only from the top models. Defaults to 3. Set = NULL if you don't want to use.
#' @param save_modeltime_table Should modeltime table be saved after tuning each algorithm. Defaults to TRUE



ml_tune <- function(parsnip_recipe, modeltime_recipe, vfold, grid_size, cv_repeats, parallel_type = "everything",
                    return = c("modellist", "modeltable", "both"),
                    models = c("xgboost", "rf", "cubist", "svm_rbf", "svm_poly", "glmnet", "knn", "mars", "prophet_boost", "lightgbm", "catboost"),
                    learn_rate = NULL, min_n = NULL, tree_depth = NULL, loss_reduction = NULL, save_modeltime_table = TRUE,
                    include_simple_model_ensemble = TRUE, top_ensemble = 3
                    ) {

    # Libraries
    require(lightgbm)

    # Tidymodels
    require(tidymodels)
    require(treesnip)
    require(tidyverse)

    require(timetk)
    require(modeltime)
    require(modeltime.ensemble)
    require(future)
    require(doFuture)
    require(tictoc)


    # Create path where models are saved
    if(!dir.exists("modeltime_table")) {
        dir.create("modeltime_table")
    }

    # Create a sub-directory for each tuning process
    new_dir_name <- paste("Tune",  timestamp(prefix = "", suffix = "", quiet = TRUE), paste0("fold", vfold), paste0("grid", grid_size), sep = "_")
    new_dir_name <- gsub(" ", "-", new_dir_name)
    new_dir_name <- gsub(":", "-", new_dir_name)

    dir.create(paste0("modeltime_table/", new_dir_name))

    path_to_file    <- paste0("modeltime_table/", new_dir_name, "/modeltimetable.rds")


    # Cross validation
    #set.seed(123)

    resamples_kfold <- training(splits) %>% vfold_cv(v = vfold, repeats = cv_repeats)

    model_list <- list()
    model_table <- modeltime_table()
    finalized_wflw <- list()
    tune_plot      <- list()

    # Light GBM
    if ("lightgbm" %in% models) {
        message("Start tuning Light GBM")
        tic()

        model_spec_lightgbm_tune <- boost_tree(
            mode           = "regression",
            mtry           = tune(),
            trees          = 1000,
            min_n          = tune(),
            tree_depth     = tune(),
            learn_rate     = tune(),
            loss_reduction = tune(),
            sample_size    = tune()
            #stop_iter      = tune()
        ) %>%
            set_engine("lightgbm")

        wflw_fit_lightgbm <- wflw_creator(model_spec_lightgbm_tune, recipe_spec_catlight, resamples_kfold = resamples_kfold, grid_size, parallel_type,
                                          learn_rate, min_n, tree_depth, loss_reduction)

        model_list$lightgbm <- wflw_fit_lightgbm$fitted_workflow

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_lightgbm$fitted_workflow %>% modeltime_table())

        finalized_wflw$lightgbm <- wflw_fit_lightgbm$finalized_workflow

        tune_plot$lightgbm <- wflw_fit_lightgbm$tune_plot

        # Save the current state of the modeltime table
        if(save_modeltime_table) {
            model_table %>%
                write_rds(path_to_file)
        }



        message("Finish tuning Lightgbm")
        toc()
        gc()
    }


    # Catboost
    if ("catboost" %in% models) {
        message("Start tuning Catboost")

        set_dependency("boost_tree", eng = "catboost", "catboost")
        set_dependency("boost_tree", eng = "catboost", "treesnip")

        model_spec_catboost_tune <- boost_tree(
            mode           = "regression",
            mtry           = tune(),
            trees          = 1000,
            min_n          = tune(),
            tree_depth     = tune(),
            learn_rate     = tune(),
            loss_reduction = tune(),
            sample_size    = tune()
            #stop_iter      = tune()
        ) %>%
            set_engine("catboost")

        wflw_fit_catboost <- wflw_creator(model_spec_catboost_tune, recipe_spec_catlight, resamples_kfold = resamples_kfold, grid_size, parallel_type,
                                          learn_rate, min_n, tree_depth, loss_reduction)

        model_list$catboost <- wflw_fit_catboost$fitted_workflow

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_catboost$fitted_workflow %>% modeltime_table())

        finalized_wflw$catboost <- wflw_fit_catboost$finalized_workflow

        tune_plot$catboost <- wflw_fit_catboost$tune_plot


        # Save the current state of the modeltime table
        if(save_modeltime_table) {
            model_table %>%
                write_rds(path_to_file)
        }



        message("Finish tuning Catboost")
        toc()
        gc()
    }


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
            loss_reduction = tune(),
            sample_size    = tune()
            #stop_iter      = tune()
        ) %>%
            set_engine("xgboost")

        wflw_fit_xgboost <- wflw_creator(model_spec_xgboost_tune, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, parallel_type,
                                         learn_rate, min_n, tree_depth, loss_reduction)

        model_list$xgboost <- wflw_fit_xgboost$fitted_workflow

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_xgboost$fitted_workflow %>% modeltime_table())

        finalized_wflw$xgboost <- wflw_fit_xgboost$finalized_workflow

        tune_plot$xgboost <- wflw_fit_xgboost$tune_plot


        # Save the current state of the modeltime table
        if(save_modeltime_table) {
            model_table %>%
                write_rds(path_to_file)
        }



        message("Finish tuning XGBoost")
        toc()

        gc()
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

        wflw_fit_rf <- wflw_creator(model_spec_rf_tune, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, parallel_type,
                                    learn_rate, min_n, tree_depth, loss_reduction)

        model_list$ranger <- wflw_fit_rf$fitted_workflow

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_rf$fitted_workflow %>% modeltime_table())

        finalized_wflw$ranger <- wflw_fit_rf$finalized_workflow

        tune_plot$ranger <- wflw_fit_rf$tune_plot


        # Save the current state of the modeltime table
        if(save_modeltime_table) {
            model_table %>%
                write_rds(path_to_file)
        }



        message("Finish tuning Random Forest")
        toc()
        gc()
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

        wflw_fit_cubist <- wflw_creator(model_spec_cubist_tune, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, parallel_type,
                                        learn_rate, min_n, tree_depth, loss_reduction)

        model_list$Cubist <- wflw_fit_cubist$fitted_workflow

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_cubist$fitted_workflow %>% modeltime_table())

        finalized_wflw$Cubist <- wflw_fit_cubist$finalized_workflow

        tune_plot$Cubist <- wflw_fit_cubist$tune_plot


        # Save the current state of the modeltime table
        if(save_modeltime_table) {
            model_table %>%
                write_rds(path_to_file)
        }



        message("Finish tuning Cubist")
        toc()
        gc()
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

        wflw_fit_svm_rbf <- wflw_creator(model_spec_svm_rbf_tune, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, parallel_type,
                                         learn_rate, min_n, tree_depth, loss_reduction)

        model_list$svm_rbf <- wflw_fit_svm_rbf$fitted_workflow

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_svm_rbf$fitted_workflow %>% modeltime_table() %>% mutate(.model_desc = "KERNLAB - RBF"))

        finalized_wflw$svm_rbf <- wflw_fit_svm_rbf$finalized_workflow

        tune_plot$svm_rbf <- wflw_fit_svm_rbf$tune_plot


        # Save the current state of the modeltime table
        if(save_modeltime_table) {
            model_table %>%
                write_rds(path_to_file)
        }



        message("Finish tuning SVM (radial basis)")
        toc()
        gc()

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

        wflw_fit_svm_poly <- wflw_creator(model_spec_svm_poly_tune, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, parallel_type,
                                          learn_rate, min_n, tree_depth, loss_reduction)

        model_list$svm_poly <- wflw_fit_svm_poly$fitted_workflow

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_svm_poly$fitted_workflow %>% modeltime_table()%>% mutate(.model_desc = "KERNLAB - Poly"))

        finalized_wflw$svm_poly <- wflw_fit_svm_poly$finalized_workflow

        tune_plot$svm_poly <- wflw_fit_svm_poly$tune_plot


        # Save the current state of the modeltime table
        if(save_modeltime_table) {
            model_table %>%
                write_rds(path_to_file)
        }



        message("Finish tuning SVM (polynomial)")
        toc()
        gc()
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

        wflw_fit_glmnet <- wflw_creator(model_spec_glmnet, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, parallel_type,
                                        learn_rate, min_n, tree_depth, loss_reduction)

        model_list$glmnet <- wflw_fit_glmnet$fitted_workflow

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_glmnet$fitted_workflow %>% modeltime_table())

        finalized_wflw$glmnet <- wflw_fit_glmnet$finalized_workflow

        tune_plot$glmnet <- wflw_fit_glmnet$tune_plot


        # Save the current state of the modeltime table
        if(save_modeltime_table) {
            model_table %>%
                write_rds(path_to_file)
        }



        message("Finish tuning Elastic net")
        toc()
        gc()
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

        wflw_fit_knn <- wflw_creator(model_spec_knn, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, parallel_type,
                                     learn_rate, min_n, tree_depth, loss_reduction)

        model_list$kknn <- wflw_fit_knn$fitted_workflow

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_knn$fitted_workflow %>% modeltime_table())

        finalized_wflw$kknn <- wflw_fit_knn$finalized_workflow

        tune_plot$kknn <- wflw_fit_knn$tune_plot


        # Save the current state of the modeltime table
        if(save_modeltime_table) {
            model_table %>%
                write_rds(path_to_file)
        }



        message("Finish tuning KNN")
        toc()
        gc()
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

        wflw_fit_mars <- wflw_creator(model_spec_mars, parsnip_recipe, resamples_kfold = resamples_kfold, grid_size, parallel_type,
                                      learn_rate, min_n, tree_depth, loss_reduction)

        model_list$mars <- wflw_fit_mars$fitted_workflow

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_mars$fitted_workflow %>% modeltime_table())

        finalized_wflw$mars <- wflw_fit_mars$finalized_workflow

        tune_plot$mars <- wflw_fit_mars$tune_plot


        # Save the current state of the modeltime table
        if(save_modeltime_table) {
            model_table %>%
                write_rds(path_to_file)
        }



        message("Finish tuning MARS")
        toc()
        gc()
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
            loss_reduction = tune(),
            sample_size    = tune()

        ) %>%
            set_engine("prophet_xgboost")


        wflw_fit_prophet_boost <- wflw_creator(model_spec_prophet, modeltime_recipe, resamples_kfold = resamples_kfold, grid_size, parallel_type,
                                               learn_rate, min_n, tree_depth, loss_reduction)

        model_list$prophet_xgboost <- wflw_fit_prophet_boost$fitted_workflow

        model_table <- model_table %>%
            combine_modeltime_tables(wflw_fit_prophet_boost$fitted_workflow %>% modeltime_table())

        finalized_wflw$prophet_xgboost <- wflw_fit_prophet_boost$finalized_workflow

        tune_plot$prophet_xgboost <- wflw_fit_prophet_boost$tune_plot


        # Save the current state of the modeltime table
        if(save_modeltime_table) {
            model_table %>%
                write_rds(path_to_file)
        }



        message("Finish tuning Prophet Boost")
        toc()
        gc()
    }


    # Create ensemble
    if (include_simple_model_ensemble) {

        model_table <- model_table %>%
            ensemble_average(type = "mean") %>%
            modeltime_table() %>%
            combine_modeltime_tables(model_table)

    }

    if (!is.null(top_ensemble)) {

        top_models <- model_table %>%
            modeltime_accuracy(testing(splits)) %>%
            arrange(rmse) %>%
            slice_head(n = top_ensemble) %>%
            pull(.model_id)

        model_table <- model_table %>%
            filter(.model_id %in% top_models) %>%
            ensemble_average(type = "mean") %>%
            modeltime_table() %>%
            combine_modeltime_tables(model_table)

    }


    # Return
    return_list <- list()
    return_list$models <- model_list
    return_list$modeltime_table <- model_table
    return_list$finalized_workflows <- finalized_wflw
    return_list$tune_plot <- tune_plot

    if (return == "modellist") {
        return(model_list)
    } else if (return == "modeltable") {
        return(model_table)
    } else {
        return(return_list)
    }

}
