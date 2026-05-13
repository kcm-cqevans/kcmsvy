#' Reshape Survey Data
#'
#' This function reshapes a survey dataframe from wide to long format, allowing for
#' optional dropping of specified columns and NA values. Users can specify columns
#' to pivot, columns not to pivot, columns to drop, and whether to drop rows with NA
#' values in the pivoted columns.
#'
#' @param data A dataframe containing survey data.
#' @param pivot_cols Optional vector of string column names to pivot into longer format.
#'   If NULL and `not_pivot_cols` is specified, all columns not in `not_pivot_cols` will be pivoted.
#' @param not_pivot_cols Optional vector of string column names not to pivot into longer format.
#'   If specified, `pivot_cols` should be NULL.
#' @param drop_cols Optional vector of string column names to be dropped from `data` before reshaping.
#' @param drop_na Logical; if TRUE, rows with NA values in the `response` column after pivoting will be dropped.
#' @import dplyr
#' @import tidyr
#' @return A long format dataframe with specified or inferred pivoted columns, optionally excluding specified columns
#'   and/or rows with NA values.
#' @export
svyreshape_long <- function(data, pivot_cols = NULL, not_pivot_cols = NULL, drop_cols = NULL, drop_na = FALSE) {
  # Validate parameters
  if (!is.null(pivot_cols) && !is.null(not_pivot_cols)) {
    stop("Specify either pivot_cols or not_pivot_cols, not both.")
  }

  if (is.null(pivot_cols) && is.null(not_pivot_cols)) {
    stop("At least one of pivot_cols or not_pivot_cols must be provided.")
  }

  if (!is.null(drop_cols)) {
    if (!all(drop_cols %in% names(data))) {
      stop("All drop_cols must be column names in data.")
    }
    # Drop specified columns
    data <- select(data, -all_of(drop_cols))
  }

  cols_to_pivot <- if (!is.null(not_pivot_cols)) {
    setdiff(names(data), not_pivot_cols)
  } else {
    pivot_cols
  }

  # Reshape the data
  data_long <- data %>%
    tidyr::pivot_longer(cols = all_of(cols_to_pivot), names_to = "axis_var", values_to = "response_category") %>%
    as.data.frame()

  if (drop_na) {
    # Drop rows with NA in 'response' after reshaping
    data_long <- data_long %>% filter(!is.na(response))
  }

  return(data_long)
}
