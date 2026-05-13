#' KCM Survey Data - Bar Graph Visualization
#'
#' This function creates bar graphs for showing the distribution of responses -- both across single-response survey
#' data questions as well as multiple select responses.
#' @param data This is the datagram that will be used for the visualization. It should include columns for the category,
#' as well as the proportion (prop) and the nicely labeled proportion (prop_label). Using the "svycollapse" function, you can
#' create the data frame necessary to create all the variables.
#' @param prop This is the proportion of responses that fall in the category. For categorical variables, it'll be the
#' proportion of respondents that fit in each category (adding to 100%). For multiple select, you'll need to make sure to
#' transpose to long form. Then, it'll be the proportion of respondents who selected yes or no.
#' @param proplabel This is the same as the proportion, except with formatting and labeling appropriate for graphs.
#' @param axis_var This is the variable used for the axis -- the descriptive variable.
#' @param ymin This is the minimum of the y-axis for the graph
#' @param ymax This is the maximum of the y-axis for the graph
#' @param main_title This is the main title of the grpah
#' @param subtitle This is the subtitle of the graph (i.e indicating a subgroup or more description)
#' @param source_info This is the source of the data for the graph. If it is for Q2 of 2023 Rider/Non-Rider - you can
#' add source_info="Source: Rider/Non-Rider, Q2 2023"
#' @param order This is specifying whether the order of the bars are in ascending or descending order.
#' @param color_scheme Default color = #FDB71A
#' @param horiz If horiz=TRUE, the graph bars are horizontal and y-axis = %. If horiz= FALSE, then graph is vertical with
#' x axis = %.
#' @param textsize_yaxis text size of x axis
#' @param textsize_xaxis text size of y axis
#' @import ggrepel
#' @import ggtext
#' @import ggplot2
#' @import ggtext
#' @import stringr
#' @return Pretty graph
#' @export
#'

kcmviz_bar<-function(data,
                     axis_var = NULL,   # string; auto-detect "axis_var" if NULL
                     prop        = NULL,   # string; auto-detect "prop" or "percent" if NULL
                     proplabel   = NULL,   # string; auto-detect "proplabel" if NULL
                     ymin = 0,
                     ymax = 100,
                     main_title = "",
                     subtitle = "",
                     source_info = "",
                     order = c("ascend", "descend", "none"),
                     color_scheme = "#006633",
                     horiz = TRUE,         # caller intent; inverted internally via flip
                     textsize_yaxis = 16,
                     textsize_xaxis = 16) {

  order <- match.arg(order)

  # --- Invert horizontal behavior (as per your earlier requirement) ---
  flip <- !isTRUE(horiz)  # horiz=TRUE -> vertical; horiz=FALSE -> horizontal

  # --- Auto-detect axis_var if not provided ---
  if (is.null(axis_var)) {
    if ("axis_var" %in% names(data)) {
      axis_var <- "axis_var"
    } else {
      stop("Please provide `axis_var`, or include a column named 'axis_var' in `data`.")
    }
  }
  if (!axis_var %in% names(data)) {
    stop("`axis_var` not found in `data`: ", axis_var)
  }

  # --- Auto-detect prop if not provided ---
  if (is.null(prop)) {
    if ("prop" %in% names(data)) {
      prop <- "prop"
    } else if ("percent" %in% names(data)) {
      prop <- "percent"
    } else {
      stop("Could not auto-detect a proportion column. Provide `prop` (e.g., 'prop' or 'percent').")
    }
  }
  if (!prop %in% names(data)) {
    stop("`prop` not found in `data`: ", prop)
  }

  # --- Auto-detect proplabel if not provided ---
  if (is.null(proplabel) && "proplabel" %in% names(data)) {
    proplabel <- "proplabel"
  }
  if (!is.null(proplabel) && !proplabel %in% names(data)) {
    stop("`proplabel` not found in `data`: ", proplabel)
  }

  # --- Prepare plotting frame ---
  df <- data %>%
    mutate(
      .x    = .data[[axis_var]],
      .prop = .data[[prop]],
      # Convert 0–1 proportions to percent; leave 0–100 as-is
      .prop = if (is.numeric(.prop) && max(.prop, na.rm = TRUE) <= 1) .prop * 100 else .prop,
      .lab  = if (!is.null(proplabel)) .data[[proplabel]] else paste0(round(.prop, 1), "%")
    )

  # --- Order rows by .prop ---
  df <- switch(
    order,
    "ascend"  = df %>% arrange(.prop),
    "descend" = df %>% arrange(desc(.prop)),
    "none"    = df
  )

  # Preserve sorted order in the axis
  df <- df %>% mutate(.x = factor(.x, levels = .x))

  # --- Build base plot ---
  update_geom_defaults("text", list(family = "sans"))

  p <- ggplot(df, aes(x = .x, y = .prop)) +
    geom_col(color = color_scheme, fill = color_scheme, width = 0.75) +
    geom_text(aes(label = .lab),
              vjust = if (flip) 0.5 else -0.5,
              hjust = if (flip) -0.1 else 0.5,
              size  = 6, fontface = "bold", color = color_scheme) +
    scale_y_continuous(limits = c(ymin, ymax),
                       expand = c(0, 0.3),
                       labels = function(x) paste0(x, "%")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = if (flip) 50 else 9)) +
    labs(title   = main_title,
         y       = "",
         x       = "",
         caption = source_info,
         subtitle = subtitle) +
    theme(
      text = element_text(size = 18, family = "sans"),
      plot.title = element_text(size = 24, family = "sans", face = "bold"),
      plot.caption = element_text(size = 16, hjust = 0.02, vjust = 2, family = "sans", color = "#585860"),
      plot.subtitle = element_text(size = 18, family = "sans", color = "#242424"),
      axis.line = element_line(size = 0.5, color = "black"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.text.x = element_text(size = textsize_xaxis, family = "sans", color = "black"),
      axis.text.y = element_text(size = textsize_yaxis, family = "sans", color = "black"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = if (!flip) element_line(color = "#585860", size = 0.35, linetype = 2) else element_blank(),
      panel.grid.major.x = if (flip) element_line(color = "#585860", size = 0.35, linetype = 2) else element_blank()
    )

  # Apply coord_flip() for horizontal (based on inverted flag)
  if (flip) {
    p <- p + coord_flip()
  }

  return(p)
}
