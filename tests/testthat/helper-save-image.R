save_plot <- function(plot, path) {
  ggplot2::ggsave(path, plot)
  return(path)
}
