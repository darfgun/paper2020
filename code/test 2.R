library(ggplot2)
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

p <- ggplot(cars, aes(speed, dist)) +
  geom_point()
# Add regression line
p + geom_smooth(method = lm, formula = y ~ poly(x, 3))

# loess method: local regression fitting
p + geom_smooth(method = "loess")
