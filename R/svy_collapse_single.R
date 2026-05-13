#' Title
#' @import dplyr
#' @import rlang
#' @importFrom srvyr as_survey_design survey_prop
#' @importFrom dplyr across
#'
#' @param data dataset that you are using
#' @param axis_var the variable that you want to take the weighted mean or weighted proportion of, axis_var ="commute_freq"
#' @param wgt wgt = survey weight, if you're not using weights, just create a column called wgt that is all equal to 1
#'
#' @returns a collapsed set of survey-weighted means
#' @export
#'
#' @examples
#' test <- data.frame(
#'   rider = c("A", "B", "A", "C"),
#'   wgt   = c(1.2, 0.8, 1.1, 0.9)
#' )
#' collapsed<-svy_collapse_single(data=test, axis_var = "rider", wgt="wgt")
svy_collapse_single <- function(data,
                                axis_var = NULL,
                                wgt = NULL,
                                groupby_var = NULL) {

  # --- 1) Use provided names exactly ---
  if (is.null(axis_var)) stop("axis_var must be supplied")
  if (is.null(wgt))      stop("wgt must be supplied")

  # --- 2) Validate columns ---
  needed_cols <- c(axis_var, wgt, if (!is.null(groupby_var)) groupby_var)
  missing_cols <- setdiff(needed_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in `data`: ", paste(missing_cols, collapse = ", "))
  }

  # --- 3) Prefilter NA values ---
  if (is.null(groupby_var)) {
    data_f <- data %>%
      dplyr::filter(
        !is.na(.data[[axis_var]]),
        !is.na(.data[[wgt]])
      )
  } else if (length(groupby_var) == 1) {
    data_f <- data %>%
      dplyr::filter(
        !is.na(.data[[axis_var]]),
        !is.na(.data[[wgt]]),
        !is.na(.data[[groupby_var]])
      )
  } else {
    # multiple grouping variables
    data_f <- data %>%
      dplyr::filter(
        !is.na(.data[[axis_var]]),
        !is.na(.data[[wgt]]),
        dplyr::if_all(dplyr::all_of(groupby_var), ~ !is.na(.))
      )
  }

  # --- 4) Survey design ---
  design <- data_f %>%
    srvyr::as_survey_design(ids = NULL, weights = !!rlang::sym(wgt))

  # --- 5) Grouping structure ---
  grouping_vars <- if (is.null(groupby_var)) {
    axis_var
  } else {
    c(groupby_var, axis_var)
  }

  # --- 6) Calculate proportions + CI ---
  out <- design %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
    srvyr::summarize(
      prop = survey_prop(na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      dplyr::across(c(prop, prop_low, prop_upp), ~ . * 100),
      proplabel = paste0(round(prop, 1), "%")
    )

  out
}
