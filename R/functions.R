make_example_data <- function() {
  data.frame(x = 1:10, y = (1:10)^2)
}

summarise_example <- function(data) {
  data.frame(mean_y = mean(data$y), n = nrow(data))
}
