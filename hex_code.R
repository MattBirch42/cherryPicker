# ---- cherryPicker hex sticker (final balanced layout) ----
library(ggplot2)
library(dplyr)
library(hexSticker)

# --- Step 1: Define cherry positions ---
cherries <- tibble::tibble(
  x = c(
    0.7,
    0.7, # 2 red cherries (boxed)
    1.8,
    2.0,
    2.2,
    1.3,
    2.3,
    2.6,
    2.0,
    2.2,
    2.4,
    2.6
  ),
  y = c(1.2, 1.9, 1.8, 1.1, 1.3, 1.9, 1.6, 1.5, 2.0, 1.8, 2.4, 2)
)

highlight_ids <- c(1, 2)
cherries <- cherries %>%
  mutate(highlight = row_number() %in% highlight_ids)

stems <- cherries %>%
  mutate(xend = x, yend = y + 0.12)

# --- Step 2: Build the cherries plot ---
p <- ggplot() +
  # rectangle around the two red cherries
  annotate(
    "rect",
    xmin = min(cherries$x[highlight_ids]) - 0.15,
    xmax = max(cherries$x[highlight_ids]) + 0.15,
    ymin = min(cherries$y[highlight_ids]) - 0.15,
    ymax = max(cherries$y[highlight_ids]) + 0.15,
    fill = "white",
    alpha = 0.4,
    color = "white",
    linewidth = 1
  ) +
  # stems
  geom_segment(
    data = stems,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "darkgreen",
    linewidth = 0.7
  ) +
  # cherries
  geom_point(
    data = cherries,
    aes(x, y, fill = highlight),
    shape = 21,
    color = "black",
    size = 5.5,
    stroke = 0.8
  ) +
  scale_fill_manual(values = c("FALSE" = "gray60", "TRUE" = "red")) +
  coord_equal(xlim = c(0.4, 2.9), ylim = c(0.7, 2.8), expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

# --- Step 3: Wrap in hex sticker ---
sticker(
  subplot = p,
  package = "cherryPicker",
  p_size = 18,
  p_color = "white",
  p_family = "sans",
  p_y = 0.5,
  s_x = 1,
  s_y = 1.15,
  s_width = 1.8,
  s_height = 1.8,
  h_fill = "darkgreen",
  h_color = "white",
  filename = "cherryPicker_hex.png"
)

cat("✅ Hex sticker saved as 'cherryPicker_hex.png'\n")
