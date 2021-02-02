#' Find best weights for each model in an ensemble
#'
#' @param submodels Modeltable with models to average
#' @param n_weight Number of different weights per weight. Example: n_weight = 3 means expand_grid(w1 = 0:3, w2 = 0:3) for two models


best_weighted_average <- function(submodels, n_weight, parallel = TRUE, splits) {

    n_models <- nrow(submodels)
    weight_name <- paste0("w", 1:n_models)

    mat <- matrix(rep(0:n_weight, n_models), ncol = n_models)
    df_mat <- as.data.frame(mat)
    df_grid <- expand.grid(df_mat)
    names(df_grid) <- weight_name

    df_grid$count_zero <- rowSums(df_grid == 0)

    df_grid <- df_grid %>%
        filter(count_zero < 2) %>%
        select(-count_zero)


    if (parallel) {

        n_cores <- parallel::detectCores()
        cl <- parallel::makeCluster(n_cores)
        doParallel::registerDoParallel(cl)

        message(str_glue("{nrow(df_grid)} combinations in parallel with {n_cores} cores"))

        metrics_tbl <- foreach(i = 1:nrow(df_grid),
                               .combine = rbind,
                               .packages = c("tidyverse", "modeltime", "modeltime.ensemble", "tidymodels")) %dopar% {

                                   submodels %>%
                                       ensemble_weighted(loadings = as.numeric(df_grid[i, ])) %>%
                                       modeltime_table() %>%
                                       modeltime_accuracy(testing(splits)) %>%
                                       add_column(weight_row = paste0(i))

                               }

        parallel::stopCluster(cl)


        weights_row_use <- metrics_tbl %>%
            bind_rows() %>%
            arrange({{order_by}}) %>%
            dplyr::slice(1) %>%
            pull(weight_row)

    } else {

        error_list <- list()
        for (i in 1:nrow(df_grid)) {

            message(str_glue("Combination {i} of {nrow(df_grid)}"))

            model_ensemble <- submodels %>%
                ensemble_weighted(loadings = as.numeric(df_grid[i, ])) %>%
                modeltime_table()

            error_list[[i]] <- model_ensemble %>%
                modeltime_accuracy(testing(splits)) %>%
                add_column(weight_row = paste0(i))
        }

        weights_row_use <- error_list %>%
            bind_rows() %>%
            arrange(mase) %>%
            dplyr::slice(1) %>%
            pull(weight_row)
    }



    weights_use <- as.numeric(df_grid[weights_row_use, ])

    # Final model

    model_ensemble_weighted <- submodels %>%
        ensemble_weighted(loadings = weights_use) %>%
        modeltime_table()

    return(model_ensemble_weighted)

}
