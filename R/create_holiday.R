#' Creates holiday variables to be used in modeling
#'
#' @param data Data set with date and character vector of holiday names
#' @param lead_lag_one TRUE if lead and lag of the dummy are also 1 instead of -0.5 for lag and 0.5 for lead to capture different effect of lead and lag holiday
#'
#' @returns Returns list with two data set: holidays_for_ml which is for ML algos and holidays_for_clean which will be used when cleaning the data


create_holiday <- function(data, lead_lag_one = TRUE) {

    # Bý til breytu fyrir alla daga frá 1990 til 2040
    # Skeiti frídögum við og breyti yfir í vikugögn
    # Þessi skeiting er gerð til að lead og lag frídagur komi rétt inn
    # Annars skapast hætta á því að lag frídagur verði ekki í vikunni á undan heldur
    # mörgum vikum á undan. date_variable reddar þessu
    date_variable <- seq.Date(from = as.Date("1990-01-01"),
                              to   = as.Date("2040-12-31"),
                              by   = "day")

    date_tbl <- tibble(date = date_variable)


    # Bý til wide format af frídögum með heitið holiday_
    holiday_factor <- data %>%
        mutate(date = as.Date(date),
               date = floor_date(date, "week", week_start = 1),
               year = year(date)) %>%
        group_by(date) %>%
        dplyr::slice(1) %>%
        group_by(year) %>%
        mutate(holiday_factor = paste("holiday", 1:n(), sep = "_")) %>%
        ungroup() %>%
        select(-c(holiday, year)) %>%
        mutate(helper = 1) %>%
        pivot_wider(names_from = holiday_factor, values_from = helper)


    # Bý til lead og lag fyrir hvern frídag
    # Ef lead_lag_one = TRUE þá bý ég til lead og lag sem eru bæði jafn og 1
    # líkt og dummy. Ef FALSE þá bý ég til -0.5 og +0.5 fyrir lead og lag til að fanga
    # ólík áhrif vikuna fyrir frídag og vikuna eftir frídag

    if (lead_lag_one) {
        holiday_factor_final <- date_tbl %>%
            mutate(date = floor_date(date, "week", week_start = 1)) %>%
            distinct() %>%
            left_join(holiday_factor) %>%
            pivot_longer(cols = contains("holiday"),
                         names_to = "holiday",
                         values_to = "holiday_dummy") %>%
            group_by(holiday) %>%
            mutate(lead_value = lead(holiday_dummy),
                   lag_value  = lag(holiday_dummy)) %>%
            replace(is.na(.), 0) %>%
            ungroup() %>%
            mutate(value = rowSums(.[3:5])) %>%
            select(-c(lead_value, lag_value, holiday_dummy)) %>%
            pivot_wider(names_from = holiday, values_from = value)

    } else {

        holiday_factor_final <- date_tbl %>%
            mutate(date = floor_date(date, "week", week_start = 1)) %>%
            distinct() %>%
            left_join(holiday_factor) %>%
            pivot_longer(cols = contains("holiday"),
                         names_to = "holiday",
                         values_to = "holiday_dummy") %>%
            group_by(holiday) %>%
            mutate(lead_value = lead(holiday_dummy),
                   lag_value  = lag(holiday_dummy)) %>%
            replace(is.na(.), 0) %>%
            mutate(lag_value  = ifelse(lag_value == 0, 0, -0.5),
                   lead_value = ifelse(lead_value == 0, 0, 0.5)) %>%
            ungroup() %>%
            mutate(value = rowSums(.[3:5])) %>%
            select(-c(lead_value, lag_value, holiday_dummy)) %>%
            pivot_wider(names_from = holiday, values_from = value)
    }


    # Lengd milli frídaga
    # Ef langt er á milli frídaga er líklegt að frídagurinn verði "stærri"
    lengd_milli_fridaga <- data %>%
        select(-holiday) %>%
        distinct() %>%
        mutate(date = as.Date(floor_date(date, "week", week_start = 1))) %>%
        mutate(lengd_milli = as.numeric(date - lag(date)) / 7) %>%
        group_by(date) %>%
        filter(lengd_milli == max(lengd_milli, na.rm = TRUE))


    lengd_milli_fridaga <- date_tbl %>%
        mutate(date = floor_date(date, "week", week_start = 1)) %>%
        distinct() %>% left_join(lengd_milli_fridaga) %>%
        mutate(lengd_milli = ifelse(is.na(lengd_milli), 0, lengd_milli),
               lengd_milli_lead = lead(lengd_milli),
               lengd_milli_lag  = lag(lengd_milli))


    fridagar_tbl <- holiday_factor_final %>%
        left_join(lengd_milli_fridaga)


    # Ein breyta með frídögum
    fridagar_one_var <- fridagar_tbl %>%
        select(date, contains("holiday_")) %>%
        pivot_longer(cols = contains("holiday_")) %>%
        group_by(date) %>%
        summarise(value = sum(value)) %>%
        mutate(value = ifelse(value == 0, 0, 1))

    # Return list
    return_list <- list()
    return_list$fridagar_tbl <- fridagar_tbl
    return_list$fridagar_one <- fridagar_one_var

    return(return_list)

}
