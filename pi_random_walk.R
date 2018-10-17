library(tidyverse)

x <- str_replace(readLines("./pi1000000.txt"), "\\.", "")
df <- data.frame("pi" = as.vector(str_split(x, ""))[[1]], stringsAsFactors = FALSE)

df_walk <- df %>% 
  mutate(pos = row_number() - 1, ## assign position
         dig = as.numeric(pi)) %>%
  select(pos, dig) %>%
  mutate(angle_rad = 2 * pi / 10 * dig,  ## using current digit determine direction to move
         angle_deg = circular::deg(angle_rad), ## I just like to see number in degree...
         move_x = cos(angle_rad), ## how much to move in x direction
         move_y = sin(angle_rad), ## how much to move in y direction
         last_x = replace_na(lag(move_x), 0), ## position of last x, set origin as 0
         last_y = replace_na(lag(move_y), 0), ## position of last y, set origin as 0
         cumsum_x = cumsum(move_x), ## walking == adding up all steps in x
         cumsum_y = cumsum(move_y), ## walking == adding up all steps in y
         cumsum_x_lag = cumsum(last_x),
         cumsum_y_lag = cumsum(last_y)) 


n_steps <- 1000
## Random Walk of Pi

df_walk %>% 
  filter(pos < n_steps) %>%
  ggplot(aes(x = cumsum_x, y = cumsum_y, color = pos)) + 
  geom_segment(size = 0.5, aes(xend = cumsum_x_lag, yend = cumsum_y_lag)) +
  geom_point(size = 0.8) + 
  theme_void() +
  theme(panel.background = element_rect(fill = "#FFFFFF")) +
  scale_color_viridis_c(option = "viridis", guide = "none")
ggsave("/home/ulf/Bilder/randomPi_1000.png", width = 1920/96, height = 1080/96, dpi = 300)

library(magick)
pic <- image_read("/home/ulf/Bilder/randomPi_1000.png")
pic <- image_rotate(pic, 180)
pic <- image_flop(pic)

image_write(pic, path = "/home/ulf/Bilder/randomPi_1000_inverted.png", format = "png")
