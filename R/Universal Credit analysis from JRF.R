library(tidyverse)
library(ggfittext)

# Source: https://www.jrf.org.uk/press/over-400-constituencies-set-to-see-families-hit-by-univseral-credit-cut
labour_seats_affected <- 
  tribble(
    ~`Constituency name`, ~`Percentage of all working-age families (with or without children) impacted by the cut`,
    "Birmingham, Hodge Hill", .54,
    "Bradford West", .52,
    "Bradford East", .46,
    "Birmingham, Perry Barr",	.44,
    "Edmonton", .42,
    "East Ham", .42,
    "Birmingham, Hall Green", .42,
    "Oldham West and Royton", .40,
    "Tottenham", .40,
    "Blackburn", .40
  )

labour_seats_affected |> 
  ggplot(aes(x = reorder(`Constituency name`, `Percentage of all working-age families (with or without children) impacted by the cut`, sum), y = `Percentage of all working-age families (with or without children) impacted by the cut`, label = scales::percent(`Percentage of all working-age families (with or without children) impacted by the cut`, accuracy = 1))) +
  geom_col(fill = "#05853A") +
  geom_bar_text() +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    panel.grid = element_blank(), 
    plot.title.position = "plot"
  ) +
  labs(
    title = "Percentage of all working-age families (with or without children) impacted by the cut",
    caption = "Source: Joseph Rowntree Foundation",
    x = NULL,
    y = NULL
  )
  
ggsave("output/jrf analysis.png", width = 200, height = 125, units = "mm")
