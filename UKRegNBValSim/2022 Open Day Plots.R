

library(scales)


data <- data %>%
  rename(
    "Protecting the environment (from destruction or pollution)" = "Self_SVS_1",
    "Broadmindedness (being tolerant of different ideas or beliefs)" = "Self_SVS_2",
    "Social justice (correcting injustice, protecting societies weak and vulnerable)" = "Self_SVS_3",
    "Helpfulness (helping friends and family)" = "Self_SVS_4",
    "Honesty (being genuine and sincere)" = "Self_SVS_5",
    "Social power (having control over others, being dominant)" = "Self_SVS_6",
    "Wealth (having material possessions, money)" = "Self_SVS_7",
    "Success (achieving one's goals)" = "Self_SVS_8",
    "Ambition (being hard-working, aspiring)" = "Self_SVS_9",
    "Curiosity (being interested in everything, novelty, change)" = "Self_SVS_10",
    "Freedom (forming own opinions, making own decisions)" = "Self_SVS_11",
    "A varied life (filled with challenge, novelty and change)" = "Self_SVS_12",
    "An exciting life (having stimulating experiences)" = "Self_SVS_13",
    "Pleasure (fulfilling one's desires)" = "Self_SVS_14",
    "Enjoying life (enjoying food, sex, leisure etc.)" = "Self_SVS_15",
    "Accepting my portion in life (submitting to life's circumstances)" = "Self_SVS_16",
    "Respecting tradition (maintaining customs of one's family or culture)" = "Self_SVS_17",
    "Honouring of parents or elders (showing respect)" = "Self_SVS_18",
    "Obedience (following rules, meeting obligations)" = "Self_SVS_19",
    "Social order (having a stable society)" = "Self_SVS_20",
    "Family security (living in secure surroundings)" = "Self_SVS_21"
  )

DVlist <- c(
  "Protecting the environment (from destruction or pollution)",
  "Broadmindedness (being tolerant of different ideas or beliefs)",
  "Social justice (correcting injustice, protecting societies weak and vulnerable)",
  "Helpfulness (helping friends and family)",
  "Honesty (being genuine and sincere)",
  "Social power (having control over others, being dominant)",
  "Wealth (having material possessions, money)",
  "Success (achieving one's goals)",
  "Ambition (being hard-working, aspiring)",
  "Curiosity (being interested in everything, novelty, change)",
  "Freedom (forming own opinions, making own decisions)",
  "A varied life (filled with challenge, novelty and change)",
  "An exciting life (having stimulating experiences)",
  "Pleasure (fulfilling one's desires)",
  "Enjoying life (enjoying food, sex, leisure etc.)",
  "Accepting my portion in life (submitting to life's circumstances)",
  "Respecting tradition (maintaining customs of one's family or culture)",
  "Honouring of parents or elders (showing respect)",
  "Obedience (following rules, meeting obligations)",
  "Social order (having a stable society)",
  "Family security (living in secure surroundings)"
)

j <- 1
for (i in DVlist) {
  DV <- i
  plot <- ggplot(data, aes(x = , .data[[DV]], weight = WeightRegion)) +
    geom_bar(aes(y = (..count..) / sum(..count..)), fill = "blue") +
    scale_y_continuous(labels = percent, expand = c(0, 0)) +
    scale_x_continuous(
      labels = c("Opposed to My Values -1", "Not Important 0", "1", "2", "Moderately Important 3", "4", "5", "Very Important 6", "Most Important 7"),
      expand = c(0, 0), breaks = seq(-1, 7, 1), guide = guide_axis(angle = 90)
    ) +
    labs(
      title = DV,
      y = "Percentage of People in the UK Selecting Each Response",
      x = "Response"
    ) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
  print(plot)
  ggsave(paste("plothist", j, DV, ".png", sep = ""), width = 8, height = 8, dpi = 600)
  value <- as.matrix(data[DV])
  weight <- data$WeightRegion
  weightedvaluepcs <- tibble(value, weight) %>%
    complete(value = -1:7, fill = list(weight = 0)) %>%
    summarise(out = wpct(value, weight))
  print(paste("To put that graph into words, the percentage of people in the UK who put the value of ", DV,
    " as -1 (Opposed to My Values) was ", round(weightedvaluepcs$out[1] * 100, digits = 2),
    "%, as 0 (Not Important) was ", round(weightedvaluepcs$out[2] * 100, digits = 2),
    "%, as 1 on the scale was ", round(weightedvaluepcs$out[3] * 100, digits = 2),
    "%, as 2 on the scale was ", round(weightedvaluepcs$out[4] * 100, digits = 2),
    "%, as 3 (Moderately Important) ", round(weightedvaluepcs$out[5] * 100, digits = 2),
    "%, as 4 on the scale was ", round(weightedvaluepcs$out[6] * 100, digits = 2),
    "%, as 5 on the scale was ", round(weightedvaluepcs$out[7] * 100, digits = 2),
    "%, as 6 (Very Important) ", round(weightedvaluepcs$out[8] * 100, digits = 2),
    "%, and as 7 (Most Important) was ", round(weightedvaluepcs$out[9] * 100, digits = 2),
    "%.",
    sep = ""
  ))
  j <- j + 1
}

j <- 1
for (i in DVlist) {
  DV <- i
  plot <- ggplot(data, aes(y = , .data[[DV]], x = 1, weight = WeightRegion)) +
    geom_bar(
      stat = "summary",
      fun = "mean",
      fill = "blue"
    ) +
    scale_x_discrete(expand = c(-1, -1)) +
    scale_y_continuous(
      labels = c("Opposed to My Values -1", "Not Important 0", "1", "2", "Moderately Important 3", "4", "5", "Very Important 6", "Most Important 7"),
      limits = c(-1, 7), expand = c(0, 0), breaks = seq(-1, 7, 1)
    ) +
    theme_bw() +
    labs(
      title = DV,
      y = "Average (Mean) Response of People in the UK",
      x = DV
    ) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
  print(plot)
  ggsave(paste("plotbar", j, DV, ".png", sep = ""), width = 8, height = 8, dpi = 600)
  value <- data[DV]
  weight <- as.data.frame(data$WeightRegion)
  weightedmeanvalue <- round(weighted.mean(value, weight), digits = 2)
  print(paste("In other words, people in the UK on average put the value of ", DV, " as ", weightedmeanvalue, " important on the scale where -1 = Opposed to My Values, 0 = Not Important and +7 = Most Important", sep = ""))
  j <- j + 1
}
