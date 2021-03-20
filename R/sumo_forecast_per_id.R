#' Assign a different model to each time series
#'
#' @param model_tbl A modeltime_table that has been refitted to the whole dataset
#' @param A split object
#' @param future_data Data frame for the future period i.e. a future frame
#' @param data_prepared Dataset used for training and testing (data_prepared_tbl)
#'


sumo_forecast_per_id <- function(model_tbl, splits, future_data, data_prepared, transformation) {

    # Create forecat with all models for all IDs
    total_fc_tbl <- model_tbl %>%
        modeltime_forecast(
            new_data = testing(splits),
            actual_data = data_prepared,
            keep_data = TRUE
        )

    # Find best model per ID
    if (transformation == "none") {

        best_model_per_id <- total_fc_tbl %>%
            select(.model_desc, .index, .value, id) %>%
            pivot_wider(names_from = .model_desc, values_from = .value) %>%
            filter(.index >= min(testing(splits)$date)) %>%
            pivot_longer(cols = -c(.index:ACTUAL)) %>%
            drop_na() %>%
            group_by(id, name) %>%
            summarize_accuracy_metrics(
                truth = ACTUAL,
                estimate = value,
                metric_set = default_forecast_accuracy_metric_set()
            ) %>%
            group_by(id) %>%
            slice_min(rmse, n = 1) %>%
            ungroup()

        # Choose best model for each ID
        best_model_tbl <- total_fc_tbl %>%
            left_join(best_model_per_id) %>%
            mutate(velja = case_when(.model_desc == "ACTUAL" ~ "Choose",
                                     .model_desc != "ACTUAL" & .model_desc == name ~ "Choose",
                                     TRUE ~ "Remove")) %>%
            filter(velja == "Choose") %>%
            select(id, date, .value, .key) %>%
            set_names("id", "date", "value", "key")


        # Create forecast for future data
        total_future_fc_tbl <- model_tbl %>%
            modeltime_forecast(
                new_data = future_data,
                actual_data = data_prepared,
                keep_data = TRUE
            ) %>%
            left_join(best_model_per_id) %>%
            mutate(velja = case_when(.model_desc == "ACTUAL" ~ "Choose",
                                     .model_desc != "ACTUAL" & .model_desc == name ~ "Choose",
                                     TRUE ~ "Remove")) %>%
            filter(velja == "Choose") %>%
            select(id, date, .value, .key) %>%
            set_names("id", "date", "value", "key")


    } else if (transformation == "log") {

        best_model_per_id <- total_fc_tbl %>%
            mutate(.value = exp(.value)) %>%
            select(.model_desc, .index, .value, id) %>%
            pivot_wider(names_from = .model_desc, values_from = .value) %>%
            filter(.index >= min(testing(splits)$date)) %>%
            pivot_longer(cols = -c(.index:ACTUAL)) %>%
            drop_na() %>%
            group_by(id, name) %>%
            summarize_accuracy_metrics(
                truth = ACTUAL,
                estimate = value,
                metric_set = default_forecast_accuracy_metric_set()
            ) %>%
            group_by(id) %>%
            slice_min(rmse, n = 1) %>%
            ungroup()

        # Choose best model for each ID
        best_model_tbl <- total_fc_tbl %>%
            mutate(.value = exp(.value)) %>%
            left_join(best_model_per_id) %>%
            mutate(velja = case_when(.model_desc == "ACTUAL" ~ "Choose",
                                     .model_desc != "ACTUAL" & .model_desc == name ~ "Choose",
                                     TRUE ~ "Remove")) %>%
            filter(velja == "Choose") %>%
            select(id, date, .value, .key) %>%
            set_names("id", "date", "value", "key")

        # Create forecast for future data
        total_future_fc_tbl <- model_tbl %>%
            modeltime_forecast(
                new_data = future_data,
                actual_data = data_prepared,
                keep_data = TRUE
            ) %>%
            mutate(.value = exp(.value)) %>%
            left_join(best_model_per_id) %>%
            mutate(velja = case_when(.model_desc == "ACTUAL" ~ "Choose",
                                     .model_desc != "ACTUAL" & .model_desc == name ~ "Choose",
                                     TRUE ~ "Remove")) %>%
            filter(velja == "Choose") %>%
            select(id, date, .value, .key) %>%
            set_names("id", "date", "value", "key")


    } else {

        best_model_per_id <- total_fc_tbl %>%
            mutate(.value = expm1(.value)) %>%
            select(.model_desc, .index, .value, id) %>%
            pivot_wider(names_from = .model_desc, values_from = .value) %>%
            filter(.index >= min(testing(splits)$date)) %>%
            pivot_longer(cols = -c(.index:ACTUAL)) %>%
            drop_na() %>%
            group_by(id, name) %>%
            summarize_accuracy_metrics(
                truth = ACTUAL,
                estimate = value,
                metric_set = default_forecast_accuracy_metric_set()
            ) %>%
            group_by(id) %>%
            slice_min(rmse, n = 1) %>%
            ungroup()

        # Choose best model for each ID
        best_model_tbl <- total_fc_tbl %>%
            mutate(.value = expm1(.value)) %>%
            left_join(best_model_per_id) %>%
            mutate(velja = case_when(.model_desc == "ACTUAL" ~ "Choose",
                                     .model_desc != "ACTUAL" & .model_desc == name ~ "Choose",
                                     TRUE ~ "Remove")) %>%
            filter(velja == "Choose") %>%
            select(id, date, .value, .key) %>%
            set_names("id", "date", "value", "key")

        # Create forecast for future data
        total_future_fc_tbl <- model_tbl %>%
            modeltime_forecast(
                new_data = future_data,
                actual_data = data_prepared,
                keep_data = TRUE
            ) %>%
            mutate(.value = expm1(.value)) %>%
            left_join(best_model_per_id) %>%
            mutate(velja = case_when(.model_desc == "ACTUAL" ~ "Choose",
                                     .model_desc != "ACTUAL" & .model_desc == name ~ "Choose",
                                     TRUE ~ "Remove")) %>%
            filter(velja == "Choose") %>%
            select(id, date, .value, .key) %>%
            set_names("id", "date", "value", "key")

    }

    # Return
    return_list <- list()

    return_list$test_set_predictions <- best_model_tbl
    return_list$future_predictions   <- total_future_fc_tbl

    return(return_list)

}
