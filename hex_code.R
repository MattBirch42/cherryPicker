library(hexSticker)
library(ggplot2)
library(dplyr)

# --- Step 1: Cherry positions ---
set.seed(123)
cherries <- data.frame(
  x = c(1, 2, 3, 1.5, 2.5, 1.2, 2.8),
  y = c(1, 1, 1, 2, 2, 2.5, 2.8)
)

# pick 3 cherries to highlight
highlight_ids <- c(2, 4, 7)
cherries <- cherries %>%
  mutate(highlight = ifelse(row_number() %in% highlight_ids, TRUE, FALSE))

# stems: one per cherry
stems <- cherries %>%
  mutate(xend = x, yend = y + 0.3)

# --- Step 2: Build cherries plot ---
p <- ggplot() +
  # glowing rectangle highlight
  annotate("rect",
           xmin = min(cherries$x[highlight_ids]) - 0.3,
           xmax = max(cherries$x[highlight_ids]) + 0.3,
           ymin = min(cherries$y[highlight_ids]) - 0.3,
           ymax = max(cherries$y[highlight_ids]) + 0.3,
           fill = "white", alpha = 0.4, color = "white", size = 1) +
  # stems
  geom_segment(data = stems,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "darkgreen", size = 1) +
  # cherries as circles
  geom_point(data = cherries,
             aes(x, y, fill = highlight),
             shape = 21, color = "black", size = 10, stroke = 1) +
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "orange")) +
  theme_void() +
  theme(legend.position = "none")

# --- Step 3: Wrap in hex sticker ---
sticker(
  subplot = p,
  package = "Cherry Picker",
  p_size = 16,
  p_color = "white",
  s_x = 1, s_y = 1.2, s_width = 1.8, s_height = 1.8,
  h_fill = "darkgreen",  # background
  h_color = "white",     # hex border
  filename = "cherry_picker_hex.png"
)