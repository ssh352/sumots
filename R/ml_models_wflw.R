#' A function to train simple univariate algorithms
#'
#' train_simple_models() is a function that allows you to train univariate time series models
#' @param recipe_base Recipe for the modeltime algorithms and NNETAR
#' @param recipe_spline Recipe with natural spliens
#' @param recipe_lag Recipe with lags
#' @param recipe_both Recipe with splines and lags
#' @param data Data to use
#' @param use_models Models to use
#' @param skip_models Character vector of models to skip, defaults to NULL
#'
#' @export
#'
ml_models_wflw <- function(
    recipe_base, recipe_spline, recipe_lag, recipe_both, data = training(splits),
    use_models = c("glmnet", "mars", "svm_poly", "svm_rbf", "knn",
                   "rf", "xgboost", "cubist", "nnet", "nnetar",
                   "arima_boost", "prophet_boost"),
    skip_models = NULL) {

    use_models <- use_models[!(use_models %in% skip_models)]

    list_models <- list()

    # glmnet ----
    if ("glmnet" %in% use_models) {

        model_spec_glmnet <- linear_reg(
            mode = "regression",
            penalty = 0.5,
            mixture = 0.5
        ) %>%
            set_engine("glmnet")

        set.seed(123)
        wflw_fit_glmnet_spline <- workflow() %>%
            add_model(model_spec_glmnet) %>%
            add_recipe(recipe_spline) %>%
            fit(data)

        set.seed(123)
        wflw_fit_glmnet_lag <- workflow() %>%
            add_model(model_spec_glmnet) %>%
            add_recipe(recipe_lag) %>%
            fit(data)

        set.seed(123)
        wflw_fit_glmnet_both <- workflow() %>%
            add_model(model_spec_glmnet) %>%
            add_recipe(recipe_both) %>%
            fit(data)

        list_models$wflw_fit_glmnet_spline <- wflw_fit_glmnet_spline
        list_models$wflw_fit_glmnet_lag    <- wflw_fit_glmnet_lag
        list_models$wflw_fit_glmnet_both   <- wflw_fit_glmnet_both
    }

    # mars ----
    if ("mars" %in% use_models) {

        model_spec_mars <- mars(
            mode = "regression",
            num_terms = 5
        ) %>%
            set_engine("earth", endspan = 6)

        set.seed(123)
        wflw_fit_mars_spline <- workflow() %>%
            add_model(model_spec_mars) %>%
            add_recipe(recipe_spline) %>%
            fit(data)

        set.seed(123)
        wflw_fit_mars_lag <- workflow() %>%
            add_model(model_spec_mars) %>%
            add_recipe(recipe_lag) %>%
            fit(data)

        set.seed(123)
        wflw_fit_mars_both <- workflow() %>%
            add_model(model_spec_mars) %>%
            add_recipe(recipe_both) %>%
            fit(data)

        list_models$wflw_fit_mars_spline <- wflw_fit_mars_spline
        list_models$wflw_fit_mars_lag    <- wflw_fit_mars_lag
        list_models$wflw_fit_mars_both   <- wflw_fit_mars_both

    }

    # svm polynomial ----
    if ("svm_poly" %in% use_models) {

        model_spec_svm_poly <- svm_poly(
            mode = "regression",
            cost = 1,
            degree = 1,
            scale_factor = 1,
            margin = 0.1
        ) %>%
            set_engine("kernlab")

        set.seed(123)
        wflw_fit_svm_poly_spline <- workflow() %>%
            add_model(model_spec_svm_poly) %>%
            add_recipe(recipe_spline) %>%
            fit(data)

        set.seed(123)
        wflw_fit_svm_poly_lag <- workflow() %>%
            add_model(model_spec_svm_poly) %>%
            add_recipe(recipe_lag) %>%
            fit(data)

        set.seed(123)
        wflw_fit_svm_poly_both <- workflow() %>%
            add_model(model_spec_svm_poly) %>%
            add_recipe(recipe_both) %>%
            fit(data)

        list_models$wflw_fit_svm_poly_spline <- wflw_fit_svm_poly_spline
        list_models$wflw_fit_svm_poly_lag    <- wflw_fit_svm_poly_lag
        list_models$wflw_fit_svm_poly_both   <- wflw_fit_svm_poly_both

    }

    # svm radial basis function ----
    if ("svm_rbf" %in% use_models) {

        model_spec_svm_rbf <- svm_rbf(
            mode = "regression",
            cost = 1,
            rbf_sigma = 1,
            margin = 0.1
        ) %>%
            set_engine("kernlab")

        set.seed(123)
        wflw_fit_svm_rbf_spline <- workflow() %>%
            add_model(model_spec_svm_rbf) %>%
            add_recipe(recipe_spline) %>%
            fit(data)

        set.seed(123)
        wflw_fit_svm_rbf_lag <- workflow() %>%
            add_model(model_spec_svm_rbf) %>%
            add_recipe(recipe_lag) %>%
            fit(data)

        set.seed(123)
        wflw_fit_svm_rbf_both <- workflow() %>%
            add_model(model_spec_svm_rbf) %>%
            add_recipe(recipe_both) %>%
            fit(data)

        list_models$wflw_fit_svm_rbf_spline <- wflw_fit_svm_rbf_spline
        list_models$wflw_fit_svm_rbf_lag    <- wflw_fit_svm_rbf_lag
        list_models$wflw_fit_svm_rbf_both   <- wflw_fit_svm_rbf_both

    }

    # KNN ----
    if ("knn" %in% use_models) {

        model_spec_knn <- nearest_neighbor(
            mode = "regression",
            neighbors = 2,
            dist_power = 2,
            weight_func = "optimal"
        ) %>%
            set_engine("kknn")

        set.seed(123)
        wflw_fit_knn_spline <- workflow() %>%
            add_model(model_spec_knn) %>%
            add_recipe(recipe_spline) %>%
            fit(data)

        set.seed(123)
        wflw_fit_knn_lag <- workflow() %>%
            add_model(model_spec_knn) %>%
            add_recipe(recipe_lag) %>%
            fit(data)

        set.seed(123)
        wflw_fit_knn_both <- workflow() %>%
            add_model(model_spec_knn) %>%
            add_recipe(recipe_both) %>%
            fit(data)

        list_models$wflw_fit_knn_spline <- wflw_fit_knn_spline
        list_models$wflw_fit_knn_lag    <- wflw_fit_knn_lag
        list_models$wflw_fit_knn_both   <- wflw_fit_knn_both

    }


    # Random Forest ----
    if ("rf" %in% use_models) {

        model_spec_rf <- rand_forest(
            mode  = "regression",
            mtry  = 10,
            trees = 1000,
            min_n = 1
        ) %>%
            set_engine("randomForest")

        set.seed(123)
        wflw_fit_rf_spline <- workflow() %>%
            add_model(model_spec_rf) %>%
            add_recipe(recipe_spline) %>%
            fit(data)

        set.seed(123)
        wflw_fit_rf_lag <- workflow() %>%
            add_model(model_spec_rf) %>%
            add_recipe(recipe_lag) %>%
            fit(data)

        set.seed(123)
        wflw_fit_rf_both <- workflow() %>%
            add_model(model_spec_rf) %>%
            add_recipe(recipe_both) %>%
            fit(data)

        list_models$wflw_fit_rf_spline <- wflw_fit_rf_spline
        list_models$wflw_fit_rf_lag    <- wflw_fit_rf_lag
        list_models$wflw_fit_rf_both   <- wflw_fit_rf_both
    }


    # XGBoost ----
    if ("xgboost" %in% use_models) {

        model_spec_boost <- boost_tree(
            mode = "regression"
        ) %>%
            set_engine("xgboost")

        set.seed(123)
        wflw_fit_xgboost_spline <- workflow() %>%
            add_model(model_spec_boost) %>%
            add_recipe(recipe_spline) %>%
            fit(data)
        #
        set.seed(123)
        wflw_fit_xgboost_lag <- workflow() %>%
            add_model(model_spec_boost) %>%
            add_recipe(recipe_lag) %>%
            fit(data)

        set.seed(123)
        wflw_fit_xgboost_both <- workflow() %>%
            add_model(model_spec_boost) %>%
            add_recipe(recipe_both) %>%
            fit(data)

        list_models$wflw_fit_xgboost_spline <- wflw_fit_xgboost_spline
        list_models$wflw_fit_xgboost_lag    <- wflw_fit_xgboost_lag
        list_models$wflw_fit_xgboost_both   <- wflw_fit_xgboost_both
    }


    # Cubist ----
    if ("cubist" %in% use_models) {

        model_spec_cubist <- cubist_rules() %>%
            set_engine("Cubist")

        set.seed(123)
        wflw_fit_cubist_spline <- workflow() %>%
            add_model(model_spec_cubist) %>%
            add_recipe(recipe_spline) %>%
            fit(data)

        set.seed(123)
        wflw_fit_cubist_lag <- workflow() %>%
            add_model(model_spec_cubist) %>%
            add_recipe(recipe_lag) %>%
            fit(data)

        set.seed(123)
        wflw_fit_cubist_both <- workflow() %>%
            add_model(model_spec_cubist) %>%
            add_recipe(recipe_both) %>%
            fit(data)

        list_models$wflw_fit_cubist_spline <- wflw_fit_cubist_spline
        list_models$wflw_fit_cubist_lag    <- wflw_fit_cubist_lag
        list_models$wflw_fit_cubist_both   <- wflw_fit_cubist_both

    }

    # Neural net ----
    if ("nnet" %in% use_models) {

        model_spec_nnet <- mlp(
            mode = "regression"
        ) %>%
            set_engine("nnet")

        set.seed(123)
        wflw_fit_nnet_spline <- workflow() %>%
            add_model(model_spec_nnet) %>%
            add_recipe(recipe_spline) %>%
            fit(data)

        set.seed(123)
        wflw_fit_nnet_lag <- workflow() %>%
            add_model(model_spec_nnet) %>%
            add_recipe(recipe_lag) %>%
            fit(data)

        set.seed(123)
        wflw_fit_nnet_both <- workflow() %>%
            add_model(model_spec_nnet) %>%
            add_recipe(recipe_both) %>%
            fit(data)

        list_models$wflw_fit_nnet_spline <- wflw_fit_nnet_spline
        list_models$wflw_fit_nnet_lag    <- wflw_fit_nnet_lag
        list_models$wflw_fit_nnet_both   <- wflw_fit_nnet_both

    }

    # NNETAR ----
    if("nnetar" %in% use_models) {

        model_spec_nnetar <- nnetar_reg(
        ) %>%
            set_engine("nnetar")

        set.seed(123)
        wflw_fit_nnetar_base <- workflow() %>%
            add_model(model_spec_nnetar) %>%
            add_recipe(recipe_base) %>%
            fit(data %>% drop_na())

        list_models$wflw_fit_nnetar_base <- wflw_fit_nnetar_base
    }


    # Prophet boost ----
    if ("prophet_boost" %in% use_models) {

        model_spec_prophet_boost <- prophet_boost(
            changepoint_num    = 25,
            changepoint_range  = 0.9

        ) %>%
            set_engine("prophet_xgboost")


        set.seed(123)
        wflw_fit_prophet_boost <- workflow() %>%
            add_model(model_spec_prophet_boost) %>%
            add_recipe(recipe_base) %>%
            fit(data)

        list_models$wflw_fit_prophet_boost <- wflw_fit_prophet_boost
    }

    # Arima boost ----
    if ("arima_boost" %in% use_models) {

        model_spec_arima_boost <- arima_boost(
        ) %>%
            set_engine("auto_arima_xgboost")

        set.seed(123)
        wflw_fit_arima_boost <- workflow() %>%
            add_model(model_spec_arima_boost) %>%
            add_recipe(recipe_base) %>%
            fit(data)

        list_models$wflw_fit_arima_boost <- wflw_fit_arima_boost
    }



    return(list_models)

}
