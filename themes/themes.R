dodge_width <- 0.3
point_size <- 2.5
line_size <- 0.6
quasi_dot_size <- 0.1
stroke_size <- 0.6
spread_width <- 0.05

theme_rlstudy <- ggplot2::theme(
  axis.text.y = element_text(size = 5),
  axis.text.x = element_text(size = 5, hjust = 1,angle = 30),
  axis.title = element_text(size = 5),
  strip.text = element_text(size = 5, color = "black"),
  strip.background = element_blank(),
  plot.title = element_text(size = 7),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.line.x = element_line(),
  axis.line.y = element_line()
)



colorPalette <- c("#DD1E88FF", "#1C8EB3FF", "#8FC344FF") 

