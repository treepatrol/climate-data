library(ggplot2)
library(dplyr)

# Example data
df <- data.frame(
  x = c(rnorm(20), rnorm(20, mean = 2)),
  group = rep(c("A", "B"), each = 20)
)

# Create an offset per group
df <- df %>%
  mutate(y_offset = as.numeric(factor(group)))  # 1 for A, 2 for B, etc.

ggplot(df, aes(x = x)) +
  geom_rug(aes(y = y_offset, color = group), sides = "t", length = unit(0.1, "npc")) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme_minimal()
