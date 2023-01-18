# Hypothesis test map Themes
# require ggplot2

htTheme3 <- theme_gray() + theme(
  plot.title = element_text(hjust = 0.5, vjust = -1),
  plot.subtitle = element_text(hjust = 0.5, vjust = 0),
  plot.caption = element_text(hjust = -.5),
  plot.margin = unit(c(.3, .5, .5, .5), 'cm'),

  strip.background = element_blank(),
  strip.text = element_text(size = 10, margin = margin(2)),
  strip.text.x = element_text(margin = margin(3, 0, 2, 0)),
  strip.text.y = element_text(angle = -90, margin = margin(0, 5, 0, 2)),
  panel.border = element_rect(fill = FALSE, colour = gray(.70)),
  panel.grid = element_blank(),
  panel.background = element_rect(fill = rgb(.95, 1, 1)),
  panel.spacing = unit(0, "cm"),

  # title=element_text(vjust=-2),
  axis.title.x = element_text(margin = margin(2)),
  axis.title.y = element_text(margin = margin(2)),
  axis.ticks = element_blank(),
  axis.text = element_blank(),

  legend.position = "top",
  legend.box.spacing = unit(0, "cm"),
  legend.spacing = unit(0, "cm")
)

htTheme1Col<- theme_gray() + theme(
  plot.title = element_text(hjust = 0.5, vjust = -1),
  plot.subtitle = element_text(hjust = 0.5, vjust = 0),
  plot.caption = element_text(hjust = -.5),
  plot.margin = unit(c(.3, .5, .5, .5), 'cm'),

  strip.background = element_blank(),
  strip.text = element_text(size = 10, margin = margin(2)),
  strip.text.x = element_text(margin = margin(3, 0, 2, 0)),
  strip.text.y = element_text(angle = -90, margin = margin(0, 5, 0, 2)),
  panel.border = element_rect(fill = FALSE, colour = gray(.70)),
  panel.grid = element_blank(),
  panel.background = element_rect(fill = rgb(.95, 1, 1)),
  panel.spacing = unit(0, "cm"),

  # title=element_text(vjust=-2),
  axis.title.x = element_text(margin = margin(0)),
  axis.title.y = element_text(margin = margin(2)),
  axis.ticks = element_blank(),
  axis.text = element_blank(),

  legend.position = "none"
)

htTheme1Row<- theme_gray() + theme(
  plot.title = element_text(hjust = 0.5, vjust = -1),
  plot.subtitle = element_text(hjust = 0.5, vjust = 0),
  plot.caption = element_text(hjust = -.5),
  plot.margin = unit(c(.3, .5, .5, .5), 'cm'),

  strip.background = element_blank(),
  strip.text = element_text(size = 10, margin = margin(2)),
  strip.text.x = element_text(margin = margin(3, 0, 2, 0)),
  strip.text.y = element_text(margin = margin(0, 5, 0, 2)),
  panel.border = element_rect(fill = FALSE, colour = gray(.70)),
  panel.grid = element_blank(),
  panel.background = element_rect(fill = rgb(.95, 1, 1)),
  panel.spacing = unit(0, "cm"),

  # title=element_text(vjust=-2),
  axis.title.x = element_text(margin = margin(0)),
  axis.title.y = element_text(margin = margin(2)),
  axis.ticks = element_blank(),
  axis.text = element_blank(),

  legend.position = "none"
)
