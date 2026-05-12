#' Title
#'
#' @param data dataset
#' @param element_var element_var
#' @param response_category response_category
#' @param wgt weight
#' @param groupby_var groupby_var
#' @param binary_response_category if it's binary, this will remove the 0s
#'
#' @returns a collapsed dataset that shows the proportion of each response across the element variable. could also include this by a third variable
#' @export
#'
#' @examples
#' #' test <- data.frame(
#'   rider = c("rider", "non-rider", "rider", "non-rider", "rider",  "non-rider", "non-rider", "rider"),
#'   transit_interest = c("interested", "very "interested", "somewhat interested",
#'   "not all interested", "interested", "very "interested", "somewhat interested", "not all interested"),
#'   wgt   = c(1.2, 0.8, 1.1, 0.9, 1.2, 0.8, 1.1, 0.9)
#' )
#' collapse<-svy_collapse_multi(test, element_var = "rider", response_category = "transit_interest", wgt="wgt")
svy_collapse_multi <- function(data,
                               element_var = NULL,
                               response_category = NULL,
                               wgt = NULL,
                               groupby_var = NULL,
                               binary_response_category = FALSE) {

  # --- 1) Default to canonical names if args are NULL ---
  element_var       <- if (is.null(element_var))       "element_var"       else element_var
  response_category <- if (is.null(response_category)) "response_category" else response_category
  wgt               <- if (is.null(wgt))               "wgt"               else wgt
  # groupby_var: can be NULL, a single string, or a character vector of column names

  # Validate that required columns exist in `data`
  missing_cols <- setdiff(
    c(element_var, response_category, wgt, if (!is.null(groupby_var)) groupby_var),
    names(data)
  )
  if (length(missing_cols) > 0) {
    stop("These columns are missing in `data`: ", paste(missing_cols, collapse = ", "))
  }

  # --- 2) Canonicalize names *only if* user passed non-canonical names ---
  # If the user supplied alternate names, rename them to canonical inside a local copy.
  data_std <- data

  # Protect against existing canonical names to avoid accidental overwrite
  if (!identical(element_var, "element_var") && ("element_var" %in% names(data_std))) {
    stop("Column 'element_var' already exists in `data`. Refusing to overwrite. ",
         "Either pass element_var = 'element_var' or rename the existing column.")
  }
  if (!identical(response_category, "response_category") && ("response_category" %in% names(data_std))) {
    stop("Column 'response_category' already exists in `data`. Refusing to overwrite. ",
         "Either pass response_category = 'response_category' or rename the existing column.")
  }

  # NEW: Similar safety for groupby_var when renaming a single grouping column
  if (!is.null(groupby_var) && length(groupby_var) == 1 &&
      !identical(groupby_var, "groupby_var") && ("groupby_var" %in% names(data_std))) {
    stop("Column 'groupby_var' already exists in `data`. Refusing to overwrite. ",
         "Either pass groupby_var = 'groupby_var' or rename the existing column.")
  }

  # Perform renames
  if (!identical(element_var, "element_var")) {
    data_std <- dplyr::rename(data_std, element_var = !!rlang::sym(element_var))
    element_var <- "element_var"
  }
  if (!identical(response_category, "response_category")) {
    data_std <- dplyr::rename(data_std, response_category = !!rlang::sym(response_category))
    response_category <- "response_category"
  }

  # NEW: Canonicalize groupby_var *only if* a single column was supplied
  # If multiple columns were supplied, we leave them as-is (cannot rename multiple to one name).
  if (!is.null(groupby_var) && length(groupby_var) == 1 && !identical(groupby_var, "groupby_var")) {
    data_std <- dplyr::rename(data_std, groupby_var = !!rlang::sym(groupby_var))
    groupby_var <- "groupby_var"
  }

  # --- 3) Tidyselect-friendly grouping vectors ---
  grp_vars_with_group <- c(groupby_var, element_var, response_category)
  grp_vars_no_group   <- c(element_var, response_category)

  # --- 4) Prefilter NAs on relevant columns ---
  # Support single or multiple grouping columns:
  prefilter_grouped <- function(df) {
    if (length(groupby_var) == 1) {
      df %>%
        dplyr::filter(!is.na(.data[[groupby_var]]),
                      !is.na(.data[[element_var]]),
                      !is.na(.data[[response_category]]))
    } else {
      df %>%
        dplyr::filter(dplyr::if_all(dplyr::all_of(groupby_var), ~ !is.na(.)),
                      !is.na(.data[[element_var]]),
                      !is.na(.data[[response_category]]))
    }
  }
  prefilter_ungrouped <- function(df) {
    df %>%
      dplyr::filter(!is.na(.data[[element_var]]),
                    !is.na(.data[[response_category]]))
  }

  # --- 5) Build survey design with a constant ID column (avoids `~1` + tidyselect issues) ---
  add_design <- function(df) {
    df %>%
      srvyr::as_survey_design(ids = 1, weights = !!rlang::sym(wgt))
  }

  # --- 6) Branches, with tidyselect-safe group_by() ---
  if (!is.null(groupby_var) && binary_response_category == TRUE) {
    data_std %>%
      prefilter_grouped() %>%
      add_design() %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars_with_group))) %>%
      srvyr::summarize(prop = 100 * srvyr::survey_mean(variable = .data[[response_category]], na.rm = TRUE),
                       .groups = 'drop') %>%
      dplyr::filter(.data[[response_category]] == 100 | .data[[response_category]] == 1) %>%
      dplyr::mutate(proplabel = paste0(round(prop, 1), "%"))

  } else if (!is.null(groupby_var) && binary_response_category == FALSE) {
    data_std %>%
      prefilter_grouped() %>%
      add_design() %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars_with_group))) %>%
      srvyr::summarize(prop = 100 * srvyr::survey_mean(variable = .data[[response_category]], na.rm = TRUE),
                       .groups = 'drop') %>%
      dplyr::mutate(proplabel = paste0(round(prop, 1), "%"))

  } else if (is.null(groupby_var) && binary_response_category == FALSE) {
    data_std %>%
      prefilter_ungrouped() %>%
      add_design() %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars_no_group))) %>%
      srvyr::summarize(prop = 100 * srvyr::survey_mean(variable = .data[[response_category]], na.rm = TRUE),
                       .groups = 'drop') %>%
      dplyr::mutate(proplabel = paste0(round(prop, 1), "%"))

  } else if (is.null(groupby_var) && binary_response_category == TRUE) {
    data_std %>%
      prefilter_ungrouped() %>%
      add_design() %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars_no_group))) %>%
      srvyr::summarize(prop = 100 * srvyr::survey_mean(variable = .data[[response_category]], na.rm = TRUE),
                       .groups = 'drop') %>%
      dplyr::filter(.data[[response_category]] == 100 | .data[[response_category]] == 1) %>%
      dplyr::mutate(proplabel = paste0(round(prop, 1), "%"))
  }
}
