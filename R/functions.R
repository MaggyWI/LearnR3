#' Import data from the DIME study dataset.
#'
#' @param file_path Path to the CSV file.
#' @param n Optional parameter that limits the number of rows. Default is 100.
#'
#' @returns A data frame.
#'
import_dime <- function(file_path, n = 100) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = n
    )
  return(data)
}



#' Import the csv files
#'
#' @param folder_path give the here("folder_path") where to look
#'
#' @returns tibble or data
#'
import_csv_files <- function(folder_path) {
  files <- folder_path |>
    fs::dir_ls(glob = "*.csv")
  data <- files |>
    purrr::map(import_dime) |>
    purrr::list_rbind(names_to = "file_path_id")
  return(data)
}



#' Get participant ID as first column
#'
#' @param data
#'
#' @returns table
#'
get_participant_id <- function(data) {
  data_with_id <- data |>
    dplyr::mutate(
      id = stringr::str_extract(
        file_path_id,
        "[:digit:]+\\.csv$"
      ) |>
        stringr::str_remove("\\.csv$") |>
        as.integer(),
      .before = file_path_id
    ) |>
    select(-file_path_id)
  return(data_with_id)
}


#' Prepare the date columns in DIME CGM and sleep data for joining.
#'
#' @param data The data that has the datetime column.
#' @param column The datetime column to convert to date and hour.
#'
#' @returns A tibble/data.frame
#'
prepare_dates <- function(data, column) {
  prepared_dates <- data |>
    dplyr::mutate(
      date = lubridate::as_date({{ column }}),
      hour = lubridate::hour({{ column }}),
      .before = {{ column }}
    )
  return(prepared_dates)
}



#' Making summary sorting by column
#'
#' @param data
#' @param column
#' @param functions You can enter them in list(average = mean) for example
#'
#' @returns Table
#'
summarise_column <- function(data, column, functions) {
  summarised_column <- data |>
    dplyr::select(-tidyselect::contains("timestamp"), -tidyselect::contains("datetime")) |>
    dplyr::group_by(dplyr::pick(-{{ column }})) |>
    dplyr::summarise(dplyr::across({{ column }}, functions),
                     .groups = "drop"
    )
  return(summarised_column)
}



#' Pivot wider the sleep types
#'
#' @param data
#'
#' @returns table resulting from pivot
#'
sleep_types_to_wider <- function(data) {
  wider <- data |> tidyr::pivot_wider(names_from = sleep_type, values_from = seconds_sum, names_prefix = "seconds_")
  return(wider)
}



#' Clean and prepare the CGM data for joining.
#'
#' @param data The CGM dataset.
#'
#' @returns A cleaner data frame.
#'
clean_cgm <- function(data) {
  cleaned <- data |>
    get_participant_id() |>
    prepare_dates(device_timestamp) |>
    dplyr::rename(glucose = historic_glucose_mmol_l) |>
    summarise_column(glucose, list(mean = mean, sd = sd))
  return(cleaned)
}


#' Clean and prepare the SLEEP data for joining.
#'
#' @param data The SLEEP dataset.
#'
#' @returns A cleaner data frame.
#'
clean_sleep <- function(data) {
  cleaned <- data |>
    get_participant_id() |>
    dplyr::rename(datetime = date) |>
    prepare_dates(datetime) |>
    summarise_column(seconds, list(sum = sum)) |>
    sleep_types_to_wider()
  return(cleaned)
}




#' Convert the participant details data to long and clean it up.
#'
#' @param data The DIME participant details data.
#'
#' @returns A data frame.
#'
clean_participant_details <- function(data) {
  cleaned <- data |>
    tidyr::pivot_longer(
      tidyselect::ends_with("date"),
      names_to = NULL, values_to = "date"
    ) |>
    dplyr::group_by(dplyr::pick(-date)) |>
    tidyr::complete(
      date = seq(min(date), max(date), by = "1 day")
    )
  return(cleaned)
}
