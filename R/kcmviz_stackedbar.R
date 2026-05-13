#' Stacked Bar
#'
#' @param data A data frame containing the agreement data.
#' @param outcome_var The outcome variable representing the proportion of agreement.
#' @param prop_labels Labels for the proportions.
#' @param var_labels Labels for the variables.
#' @param value_labels Labels for the values.
#' @param main_title The main title of the plot.
#' @param subtitle The subtitle of the plot.
#' @param source_info Information about the data source.
#' @param rev_values Logical indicating whether to reverse the order of value labels.
#' @param rev_variables Logical indicating whether to reverse the order of variable labels.
#' @param hide_small_values Logical indicating whether to hide small values in the plot.
#' @param order_bars Logical indicating whether to order the bars in the plot.
#' @param subtitle_h_just Horizontal justification of the subtitle.
#' @param fixed_aspect_ratio Logical indicating whether to maintain a fixed aspect ratio for the plot.
#' @param color_scheme A vector of colors for the plot.
#' @param label_size The size of the labels.
#' @param text_position The position of the text labels.
#' @import ggrepel
#' @import ggtext
#' @import ggplot2
#' @return A ggplot object representing the agreement visualization.
#' @export
kcmviz_stackedbar <- function(
    data,
    prop = NULL,       # now auto-detected from prop/percent
    prop_labels = NULL,       # auto-detect proplabel
    axis_var = NULL,          # replaces element_var
    response_category = NULL,      # response categories
    main_title = "",
    subtitle = "",
    source_info = "",
    rev_values = FALSE,
    rev_variables = FALSE,
    hide_small_values = TRUE,
    order_bars = FALSE,
    subtitle_h_just = 0,
    fixed_aspect_ratio = TRUE,
    legendnrow = 1,
    color_scheme = NULL,      # global palette
    colors = NULL,            # user-supplied vector
    textsize_xaxis = 16,
    textsize_yaxis = 16
) {

  # ------------------------------------------------------------
  # AUTO-DETECT COLUMNS
  # ------------------------------------------------------------
  if (is.null(axis_var)) {
    if ("axis_var" %in% names(data)) axis_var <- "axis_var"
    else stop("Please supply axis_var.")
  }
  if (!axis_var %in% names(data))
    stop("axis_var not found in data: ", axis_var)

  if (is.null(response_category)) {
    if ("response_category" %in% names(data)) response_category <- "response_category"
    else stop("Please supply response_category.")
  }

  # prop/outcome auto-detection
  if (is.null(prop)) {
    if ("prop" %in% names(data)) prop <- "prop"
    else if ("percent" %in% names(data)) prop <- "percent"
    else stop("Please supply prop.")
  }

  if (is.null(prop_labels)) {
    if ("proplabel" %in% names(data)) prop_labels <- "proplabel"
    else stop("Please supply prop_labels.")
  }

  df <- data %>%
    mutate(
      .x     = .data[[axis_var]],
      .val   = .data[[response_category]],
      .prop  = .data[[prop]],
      .prop  = if (max(.prop, na.rm = TRUE) <= 1) .prop * 100 else .prop,
      .lab   = .data[[prop_labels]]
    )

  # ------------------------------------------------------------
  # COLOR SYSTEM
  # ------------------------------------------------------------
  if (!is.null(colors)) {

    mycolors <- colors

  } else if (!is.null(color_scheme)) {

    if (!color_scheme %in% names(COLOR_SCHEMES))
      stop("color_scheme not found in COLOR_SCHEMES.")
    mycolors <- COLOR_SCHEMES[[color_scheme]]

  } else {

    mycolors <- DEFAULT_STACKED_COLORS
  }

  n_vals <- length(unique(df$.val))
  mycolors <- mycolors[seq_len(n_vals)]

  # ------------------------------------------------------------
  # ORDERING
  # ------------------------------------------------------------
  if (rev_values)
    df$.val <- factor(df$.val, levels = rev(unique(df$.val)))
  else
    df$.val <- factor(df$.val, levels = unique(df$.val))

  if (rev_variables)
    df$.x <- factor(df$.x, levels = rev(unique(df$.x)))
  else
    df$.x <- factor(df$.x, levels = unique(df$.x))

  # ------------------------------------------------------------
  # LABEL HIDING
  # ------------------------------------------------------------
  df$.lab_show <- ifelse(df$.prop >= 5 | !hide_small_values, df$.lab, NA)

  # ------------------------------------------------------------
  # PLOT
  # ------------------------------------------------------------
  p <- ggplot(df, aes(
    fill = .val,
    y = .prop,
    x = .x,
    label = .lab_show
  )) +
    geom_bar(position = "stack", stat = "identity", width = 0.6) +
    geom_text(
      position = position_stack(vjust = 0.5),
      color = "#FFFFFF",
      fontface = "bold",
      size = 5
    ) +
    coord_flip() +
    scale_fill_manual(values = mycolors) +
    scale_y_continuous(
      labels = function(y) paste0(y, "%"),
      expand = c(0.01, 0.01)
    ) +
    labs(
      title = main_title,
      subtitle = subtitle,
      caption = source_info,
      x = "",
      y = ""
    ) +
    guides(
      fill = guide_legend(nrow = legendnrow, reverse = TRUE)
    ) +
    theme(
      text = element_text(size = 14, family = "sans"),
      plot.title = element_text(size = 17, face = "bold"),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(size = 12, hjust = 0, color = "#36454F"),
      plot.caption.position = "plot",

      axis.line.x = element_line(linewidth = 0.6, color = "black"),
      axis.text.x = element_text(size = textsize_xaxis, color = "black"),
      axis.text.y = element_text(size = textsize_yaxis, color = "black"),
      axis.ticks = element_blank(),

      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(color = "#585860", size = 0.35, linetype = 2),

      legend.position = "top",
      legend.justification = "left",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.margin = margin(b = 5),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),

      plot.margin = margin(t = 20, r = 20, b = 20, l = 5)
    )

  if (fixed_aspect_ratio)
    p <- p + theme(aspect.ratio = 0.35)

  return(p)
}
