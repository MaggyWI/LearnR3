
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

