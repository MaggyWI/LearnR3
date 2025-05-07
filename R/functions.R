
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



