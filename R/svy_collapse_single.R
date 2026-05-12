svy_collapse_single <- function(data, element_var = NULL, wgt = NULL) {
  # Default to canonical names if present
  element_var <- if (is.null(element_var)) "element_var" else element_var
  wgt         <- if (is.null(wgt))         "wgt"         else wgt

  # Validate columns
  missing <- setdiff(c(element_var, wgt), names(data))
  if (length(missing) > 0) {
    stop("Missing columns in `data`: ", paste(missing, collapse = ", "))
  }

  # Build simple survey design (no clustering)
  design <- data %>%
    filter(!is.na(.data[[element_var]]), !is.na(.data[[wgt]])) %>%
    as_survey_design(ids = NULL, weights = !!rlang::sym(wgt))

  # Proportion per level of the variable (with CI), then scale to percent
  out <- design %>%
    group_by(!!rlang::sym(element_var)) %>%
    summarize(
      prop = survey_prop(na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    ) %>%
    # Convert estimate + CI to percent
    mutate(
      across(c(prop, prop_low, prop_upp), ~ . * 100),
      proplabel = paste0(round(prop, 1), "%")
    )

  # Rename the grouping column to literal "element_var"
  # (tidyselect-safe; works whether you passed "element_var" or a different name)
  out <- out %>%
    dplyr::rename_with(~ "element_var", dplyr::all_of(element_var))

  return(out)
}
