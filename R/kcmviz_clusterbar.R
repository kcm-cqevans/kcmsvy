#' KCM Data Visualization - Clustered Bar Chart
#' For showing proportions of an X variable across group Z.
#' For example, you might might to show the proportion of people who cite each barrier to transit use across their rider status.
#'
#' @param data Data frame that you are referencing
#' @param axis_bar The axis variable, in the example above - this will be the column that holds all of the barrier types.
#' @param lower_bound lower bound of confidence sansvals
#' @param upper_bound upper bound of confidence sansvals
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


kcmviz_clusterbar <- function(
    data,
    axis_var = NULL,
    prop = NULL,
    proplabel = NULL,
    groupby_var = NULL,
    lower_bound = NULL,
    upper_bound = NULL,
    ymin = 0,
    ymax = 100,
    main_title = "",
    subtitle = "",
    source_info = "",
    sort = "",
    horiz = TRUE,
    y_label = "",
    x_label = "",
    color_scheme = NULL,   # uses global COLOR_SCHEMES
    colors = NULL,         # user custom palette
    label_size = 4.25,
    text_position = 0.75,
    textsize_yaxis = 16,
    textsize_xaxis = 16
) {

  # ------------------------------------------------------------
  # ORIENTATION (Option A)
  # horiz = TRUE → horizontal chart → coord_flip()
  # ------------------------------------------------------------
  flip <- isTRUE(horiz)

  # ------------------------------------------------------------
  # AUTO-DETECT COLUMNS (consistent with bar + stacked)
  # ------------------------------------------------------------
  # axis_var
  if (is.null(axis_var)) {
    if ("axis_var" %in% names(data)) axis_var <- "axis_var"
    else stop("Please supply axis_var.")
  }
  if (!axis_var %in% names(data))
    stop("axis_var not found in data: ", axis_var)

  # prop column
  if (is.null(prop)) {
    if ("prop" %in% names(data)) prop <- "prop"
    else if ("percent" %in% names(data)) prop <- "percent"
    else stop("Could not auto-detect prop column.")
  }
  if (!prop %in% names(data))
    stop("prop not found in data: ", prop)

  # proplabel
  if (is.null(proplabel) && "proplabel" %in% names(data)) {
    proplabel <- "proplabel"
  }
  if (!is.null(proplabel) && !proplabel %in% names(data)) {
    stop("proplabel not found: ", proplabel)
  }

  # --- AUTO-DETECT groupby_var (your requested logic)
  if (is.null(groupby_var)) {
    if ("groupby_var" %in% names(data)) {
      groupby_var <- "groupby_var"
    } else {
      stop("Please supply groupby_var or include a column named 'groupby_var'.")
    }
  }
  if (!groupby_var %in% names(data))
    stop("groupby_var not found in data: ", groupby_var)


  # ------------------------------------------------------------
  # PREP DATA
  # ------------------------------------------------------------
  df <- data %>%
    mutate(
      .x     = .data[[axis_var]],
      .group = .data[[groupby_var]],
      .prop  = .data[[prop]],
      .prop  = if (max(.prop, na.rm = TRUE) <= 1) .prop * 100 else .prop,
      .lab   = if (!is.null(proplabel)) .data[[proplabel]]
      else paste0(round(.prop, 1), "%")
    )

  # ------------------------------------------------------------
  # SORTING
  # ------------------------------------------------------------
  if (sort %in% c("group1_asc", "group1_desc",
                  "group2_asc", "group2_desc",
                  "group3_asc", "group3_desc")) {

    gindex    <- as.numeric(substr(sort, 6, 6))
    ascending <- grepl("asc$", sort)
    g_levels  <- unique(df$.group)

    if (length(g_levels) < gindex)
      stop("Not enough groups for sorting index.")

    target_group <- g_levels[gindex]

    ordering <- df %>%
      filter(.group == target_group) %>%
      arrange(if (ascending) .prop else desc(.prop)) %>%
      pull(.x)

    extras       <- setdiff(unique(df$.x), ordering)
    final_levels <- c(ordering, sort(extras))

    df <- df %>% mutate(.x = factor(.x, levels = final_levels))

  } else if (sort == "alpha") {

    df <- df %>% mutate(.x = factor(.x, levels = sort(unique(.x))))

  } else {

    df <- df %>% mutate(.x = factor(.x, levels = unique(.x)))
  }


  # ------------------------------------------------------------
  # COLOR SYSTEM (global + custom)
  # ------------------------------------------------------------
  if (!is.null(colors)) {
    mycolors <- colors
  } else if (!is.null(color_scheme)) {
    if (!color_scheme %in% names(COLOR_SCHEMES))
      stop("Unknown color_scheme: ", color_scheme)
    mycolors <- COLOR_SCHEMES[[color_scheme]]
  } else {
    mycolors <- DEFAULT_CLUSTER_COLORS
  }

  n_groups <- length(unique(df$.group))
  mycolors <- mycolors[seq_len(n_groups)]


  # ------------------------------------------------------------
  # PLOT
  # ------------------------------------------------------------
  p <- ggplot(df, aes(
    x = .x,
    y = .prop,
    fill = .group,
    color = .group
  )) +
    geom_col(position = position_dodge(width = 0.75), width = 0.75) +
    geom_text(
      aes(label = .lab),
      position = position_dodge(width = text_position),
      size = label_size,
      fontface = "bold",
      vjust = if (!flip) -0.4 else 0.5,
      hjust = if (flip) -0.1 else 0.5,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = mycolors) +
    scale_color_manual(values = mycolors) +
    scale_y_continuous(
      limits = c(ymin, ymax),
      expand = c(0, 0.03),
      labels = function(x) paste0(x, "%")
    ) +
    scale_x_discrete(
      labels = function(x) stringr::str_wrap(
        x,
        width = if (flip) 45 else 9
      )
    ) +
    labs(
      title = main_title,
      subtitle = subtitle,
      caption = source_info,
      x = x_label,
      y = y_label
    ) +
    guides(
      fill  = guide_legend(nrow = 1),
      color = guide_legend(nrow = 1)
    ) +
    theme(
      text = element_text(size = 16, family = "sans"),
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16),
      plot.caption = element_text(size = 14, hjust = 0, color = "#585860"),
      plot.caption.position = "plot",

      axis.line = element_line(size = 0.4, color = "black"),
      axis.text.x = element_text(size = textsize_xaxis, color = "black"),
      axis.text.y = element_text(size = textsize_yaxis, color = "black"),
      axis.ticks = element_blank(),

      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank(),

      panel.grid.major.y =
        if (!flip) element_line(color = "#585860", size = 0.35, linetype = 2)
      else element_blank(),
      panel.grid.major.x =
        if (flip) element_line(color = "#585860", size = 0.35, linetype = 2)
      else element_blank(),

      legend.position = "top",
      legend.justification = "left",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.margin = margin(b = 5),
      legend.title = element_blank(),

      plot.margin = margin(t = 20, r = 20, b = 20, l = 5)
    )

  if (flip) p <- p + coord_flip()

  return(p)
}
