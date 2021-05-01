df <- iris

plot.histogram <- function(df, var, xlab, ylab) {
  out <- ggplot(df, aes_string(x = var)) +
    geom_histogram(bins = 15, fill = "#42c2f5", color = "white", alpha = 0.7) +
    labs(x = xlab, y = ylab)
  return(out)
}

plot.scatter <- function(df, var.x, var.y, c.factor, xlab, ylab, lname) {
  out <- ggplot(df, aes_string(x = var.x, y = var.y, color = c.factor)) +
    geom_point() +
    labs(x = xlab, y = ylab) +
    scale_color_discrete(name = lname)
  return(out)
}