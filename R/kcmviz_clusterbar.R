
#' KCM Data Visualization - Clustered Bar Chart
#' For showing proportions of an X variable across group Z.
#' For example, you might might to show the proportion of people who cite each barrier to transit use across their rider status.
#'
#' @param data Data frame that you are referencing
#' @param element_var The axis variable, in the example above - this will be the column that holds all of the barrier types.
#' @param lower_bound lower bound of confidence intervals
#' @param upper_bound upper bound of confidence intervals
#' @param prop This is the calculated proportion, without formatting for display
#' @param proplabel The labelled proportion variable. This is the actual value you will be displaying, ideally in format XX.X%
#' @param groupby_var This is the grouping variable. In the example above, this will be rider status.
#' @param ymin Value 0-100 where you want you graph axis to start, can be left blank
#' @param ymax Value 0-100 where you want you graph axis to end, can be left blank
#' @param main_title Title of your graph, goes top left
#' @param source_info Source information, goes bottom left. Example "Source: Rider/Non-Rider 2024"
#' @param subtitle Subtitle - if applicable.
#' @param sort Sort - the way you want to order bars -- default is alphabetically. sort="group1_asc" will sort in ascending order the values for group 1 (e.g., riders), sort="group2_desc" will sort in descending order based on values for group 2 (e.g., non-riders)
#' @param horiz Do you want your graph to be horizontal (i.e., with bars that go up and down)? Or graph to be vertical (so that bars goes left to right?)
#' @param y_label Label for Y axis
#' @param x_label Label for X axis
#' @param color_scheme Color choices, you can specify as "color_scheme=c(#color, #color)" or you can use the default colors.
#' @param label_size size of labels on top of bars
#' @param text_position position of text on top of bars
#' @param textsize_yaxis Text size of y axis
#' @param textsize_xaxis Text size of x axis
#'
#' @return A nice pretty graph
#' @export
kcmviz_clusterbar<- function(data,
                             element_var = data$element_var,
                             prop = data$prop,
                             lower_bound = data$prop_low, upper_bound = data$prop_upp,
                             proplabel = data$proplabel,
                             groupby_var = data$groupby_var,
                             ymin = 0, ymax = 100,
                             main_title = "", source_info = "", subtitle = "",
                             sort = "",
                             horiz = TRUE,
                             y_label = "", x_label = "",
                             color_scheme = c("#390854", "#F57F29", "#FDB71A", "#31859F", "#006633"),
                             label_size = 4.25,
                             text_position = 0.75,
                             textsize_yaxis = 16,
                             textsize_xaxis = 16) {
  # Ensure expected columns exist
  needed <- c("element_var", "prop", "proplabel", "groupby_var")
  miss <- setdiff(needed, names(data))
  if (length(miss) > 0) {
    stop("Missing columns in `data`: ", paste(miss, collapse = ", "))
  }

  # Compute the desired order of element_var based on sort directive
  # All groups will use the same x-order derived from the specified target group.
  # "group1_asc"/"group1_desc" -> use the first group level;
  # "group2_asc"/"group2_desc" -> second level; "group3_*" -> third level.
  get_target_group_order <- function(df, group_index, ascending = TRUE) {
    lvls <- unique(df$groupby_var)
    if (length(lvls) < group_index) {
      stop("Requested group index ", group_index, " but only ",
           length(lvls), " group(s) present.")
    }
    target <- lvls[group_index]
    out <- df %>%
      filter(groupby_var == target) %>%
      arrange(if (ascending) prop else desc(prop)) %>%
      pull(element_var)
    # In case of ties, `arrange` is stable; ensure unique order
    unique(out)
  }

  # Derive the order vector based on `sort`
  order_vec <- NULL
  if (sort == "group1_asc") {
    order_vec <- get_target_group_order(data, group_index = 1, ascending = TRUE)
  } else if (sort == "group1_desc") {
    order_vec <- get_target_group_order(data, group_index = 1, ascending = FALSE)
  } else if (sort == "group2_asc") {
    order_vec <- get_target_group_order(data, group_index = 2, ascending = TRUE)
  } else if (sort == "group2_desc") {
    order_vec <- get_target_group_order(data, group_index = 2, ascending = FALSE)
  } else if (sort == "group3_asc") {
    order_vec <- get_target_group_order(data, group_index = 3, ascending = TRUE)
  } else if (sort == "group3_desc") {
    order_vec <- get_target_group_order(data, group_index = 3, ascending = FALSE)
  } else if (sort == "alpha") {
    order_vec <- sort(unique(data$element_var))
  }

  # If we computed an order, set factor levels on element_var
  if (!is.null(order_vec)) {
    # Include any levels not present in the target (e.g., if some element_var only appears in other groups)
    all_levels <- unique(c(order_vec, unique(data$element_var)))
    # Keep target-derived levels first, followed by any extras in alpha order to avoid NA placement
    extras <- setdiff(all_levels, order_vec)
    final_levels <- c(order_vec, sort(extras))
    data <- data %>%
      mutate(
        element_var = factor(element_var, levels = final_levels)
      )
  } else {
    # Default: keep current order of appearance
    data <- data %>%
      mutate(element_var = factor(element_var, levels = unique(element_var)))
  }

  # Colors
  fill_colors <- paste0(color_scheme, "")

  # Shared plot bits
  update_geom_defaults("text", list(family = "inter"))

  base_plot <- ggplot(
    data = data,
    aes(
      x = factor(element_var, levels = levels(element_var)),
      y = prop,
      fill = groupby_var,
      color = groupby_var
    )
  ) +
    geom_bar(position = "dodge", stat = "identity", width = 0.75) +
    geom_text(
      aes(label = proplabel, y = prop, group = groupby_var),
      position = position_dodge(width = text_position),
      size = label_size, fontface = "bold",
      show.legend = FALSE,
      vjust = if (horiz) -0.5 else NULL,
      hjust = if (!horiz) -0.05 else NULL
    ) +
    scale_fill_manual(values = fill_colors) +
    scale_color_manual(values = color_scheme) +
    scale_y_continuous(limits = c(ymin, ymax), expand = expansion(mult = 0.002)) +
    labs(
      title = main_title,
      y = y_label,
      x = x_label,
      caption = source_info
    ) +
    { if (subtitle != "") labs(subtitle = subtitle) } +
    theme(
      text = element_text(size = 16, family = "inter"),
      plot.title = element_text(size = 20, family = "inter", face = "bold"),
      plot.caption = element_text(size = 16, vjust = 2, hjust = 0.02, family = "inter", color = "#585860"),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(linewidth = 0.6, linetype = "solid", colour = "black"),
      axis.line.y = element_line(linewidth = 0.6, linetype = "solid", colour = "black"),
      axis.text.x = element_text(size = textsize_xaxis, family = "inter-light", color = "black"),
      axis.text.y = element_text(size = textsize_yaxis, family = "inter-light", color = "black"),
      axis.ticks = element_blank(),
      legend.position = "top",
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.title = element_blank(),
      legend.justification = "left",
      legend.margin = margin(t = 0, b = 0),
      legend.text = element_markdown(family = "inter-light", size = 15)
    )

  if (horiz) {
    base_plot +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 9)) +
      panel_grid_major_y()
  } else {
    base_plot +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 45)) +
      panel_grid_major_x() +
      coord_flip(expand = TRUE)
  }
}

# Helper functions for grid lines (optional)
panel_grid_major_y <- function() {
  theme(panel.grid.major.y = element_line(color = "#585860", size = 0.35, linetype = 2))
}
panel_grid_major_x <- function() {
  theme(panel.grid.major.x = element_line(color = "#585860", size = 0.35, linetype = 2))
}
