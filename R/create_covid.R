#' Creates dummy variable for Covid
#'
#' @param data Data frame with one date column named date and one covid column named covid
#' @param fjolda_mork Threshold for daily covid cases for the dummy to be 1
#'

create_covid <- function(data, fjolda_mork, horizon, freq) {


    if (freq == "week") {
        covid <- data %>%
            mutate(covid_dummy = ifelse(covid >= fjolda_mork, 1, 0)) %>%
            mutate(date = floor_date(date, "week", week_start = 1)) %>%
            group_by(date) %>%
            summarise(covid_dummy = max(covid_dummy))

    } else if (freq == "month") {
        covid <- data %>%
            mutate(covid_dummy = ifelse(covid >= fjolda_mork, 1, 0)) %>%
            mutate(date = floor_date(date, "month")) %>%
            group_by(date) %>%
            summarise(covid_dummy = max(covid_dummy))

    } else {
        covid <- data
    }


    covid <- covid %>%
        future_frame(date, .length_out = horizon, .bind_data = TRUE) %>%
        fill(covid_dummy, .direction = "down")

    return(covid)

}
