#------------------------------
# Create theme for ggplot map
#------------------------------
theme_map <- theme(
        text = element_text(family = 'Helvetica Neue', color = '#444444',
                            size = 18),
        panel.background = element_rect(fill = '#CCCCCC'),
        plot.background = element_rect(fill = '#CCCCCC'),
        legend.background = element_rect(fill = '#CCCCCC'),
        panel.grid = element_blank(),
        plot.subtitle = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.key.size = unit(0.5, "in"),
        legend.text = element_text(size = 16),
        legend.justification = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
)

