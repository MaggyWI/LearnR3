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
    dplyr::rename(glucose = historic_glucose_mmol_l)
  return(cleaned)
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
