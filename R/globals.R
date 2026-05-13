utils::globalVariables(c("prop", "prop_low", "prop_upp"))



COLOR_SCHEMES <- list(
  agree_dis5 = c("#FF0000", "#FF6666", "#FFCC33", "#338585", "#006666"),
  agree_dis4 = c("#FF0000", "#FF6666", "#338585", "#006666"),
  two_color   = c("#31859F", "#FF7B21"),
  three_color = c("#FDB71A", "#31859F", "#006633"),
  five_color  = c("#FDB71A", "#F57F29", "#31859F", "#006633", "#390854"),
  continuous  = c("#125A56", "#00767B", "#238F9D", "#42A7C6",
                  "#FD9A44", "#F57634", "#E94C1F", "#555555")
)

DEFAULT_CLUSTER_COLORS <- COLOR_SCHEMES$five_color
DEFAULT_STACKED_COLORS <- COLOR_SCHEMES$continuous
DEFAULT_SINGLE_BAR_COLORS <- COLOR_SCHEMES$three_color

KCM_1 <- "#F57F29"
KCM_2 <- "#006633"
KCM_3 <- "#FDB71A"
KCM_4 <- "#390854"

