# tidyods sticker
# A script to create the package sticker, uncomment to run
# requires the hexSticker package to run


library(ggplot2)

# libreoffice palette
# Green 0  = #106802
# Green 1  = #18A303
# Green 2  = #43C330
# Green 3  = #92E285
# Green 4  = #CCF4C6
#
# scales::gradient_n_pal(c("#CCF4C6", "#ffffff"))(0.6)
# scales::gradient_n_pal(c("#CCF4C6", "#cccccc"))(0.6)
#
# Blue 3   = #63BBEE
# Orange 3 = #F09E6F
# Yellow 3 = #F5CE53

left_edge <- 1 + c(-0.7, 0.1)
right_edge <- 1 + c(-0.1, 0.7)
top_edge <- 1.1
bottom_edge <- 0.55

table_cells <- tibble::tibble(
  type = c(
    "Numeric", "Date", "String", "Numeric",
    "Numeric", "String", "Bool", "Numeric",
    "Bool", "Numeric", "Date", "String",
    rep("Header", 4)),
  row = sort(rep(1:4, 4), decreasing = TRUE),
  col = rep(1:4, 4),
  ymax = sort(rep((top_edge - ((top_edge - bottom_edge)/4) * 0:3), 4)),
  xmin = rep(left_edge[1] + ((right_edge[1]-left_edge[1])/4) * 0:3, 4),
) |>
  dplyr::mutate(
    ymin = dplyr::lag(ymax, n = 4, default = bottom_edge),
    xmax = rep(dplyr::lead(sort(unique(xmin)), n = 1, default = right_edge[1]), 4)
  )
)

table_borders <- tibble::tibble(
  type = c(rep("horizontal", 3), rep("vertical", 3)),
  x = c(rep(left_edge[1], 3), (left_edge[1] + ((right_edge[1]-left_edge[1])/4) * 1:3)),
  xend = c(rep(right_edge[1], 3), (left_edge[1] + ((right_edge[1]-left_edge[1])/4) * 1:3)),
  y = c((bottom_edge + ((top_edge - bottom_edge)/4) * 1:3), rep(bottom_edge, 3)),
  yend = c(bottom_edge + (((top_edge - bottom_edge)/4) * 1:3), rep(top_edge, 3)),
)

tidy_cells <- tibble::tibble(
  type = c("Header", "Bool", "Numeric", "Date", "String",
           "Numeric", "Numeric", "String", rep("Info", 8 * 3)),
  ymax = rep((top_edge - ((top_edge - bottom_edge)/8) * 0:7), 4),
  xmin = sort(rep(left_edge[2] + ((right_edge[2]-left_edge[2])/15 * 4) * 0:3, 8)),
) |>
  dplyr::mutate(
    ymin = rep(dplyr::lead(sort(unique(ymax), decreasing = TRUE), n = 1,
               default = bottom_edge), 4),
    xmax = dplyr::lead(xmin, n = 8, default = right_edge[2])
  )

tidy_borders <- tibble::tibble(
  type = c(rep("horizontal", 7), rep("vertical", 3)),
  x = c(rep(left_edge[2], 7), (left_edge[2] + ((right_edge[2]-left_edge[2])/4) * 1:3)),
  xend = c(rep(right_edge[2], 7), (left_edge[2] + ((right_edge[2]-left_edge[2])/4) * 1:3)),
  y = c((bottom_edge + ((top_edge - bottom_edge)/8) * 1:7), rep(bottom_edge, 3)),
  yend = c(bottom_edge + (((top_edge - bottom_edge)/8) * 1:7), rep(top_edge, 3)),
)

sheet_boxes <- tibble::tibble(
  ymax = top_edge,
  ymin = bottom_edge,
  xmin = left_edge,
  xmax = right_edge
)

tidyods_sticker <- ggplot(table_cells) +
  hexSticker::geom_hexagon(size = 2, fill = "#F5FDF4", color = "#18A303") +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type),
            colour = "#106802", size = 0.4) +
  geom_rect(data = tidy_cells,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type),
            colour = "#106802", size = 0.4) +
  geom_rect(data = sheet_boxes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, colour = "#106802", linetype = "solid", size = 0.4) +
  annotate(
    geom = "path",
    x = c(0.97, 1.03, 0.97),
    y = c(0.9, 0.825, 0.75),
    colour = "#18A303",
    lineend = "round",
    linejoin = "round",
    size = 1
  ) +
  annotate(
    geom = "text",
    label = "tidyods",
    x = 1,
    y = 1.27,
    vjust = 0,
    size = 8,
    colour = "#18A303",
    family = "Victor Mono",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Header" = "#18A303", "String" = "#63BBEE", "Numeric" = "#92E285",
               "Date" = "#F09E6F", "Bool" = "#F5CE53", "Info" = "#CDDCCA"),
    guide = guide_none()
  ) +
  hexSticker::theme_transparent() +
  hexSticker::theme_sticker(2)

ggsave("man/figures/tidyods_hex.svg", tidyods_sticker, width = 43.9,
       height = 50.8, units = "mm", bg = "transparent")

ggsave("man/figures/tidyods_hex.png", tidyods_sticker, width = 43.9,
       height = 50.8, units = "mm", bg = "transparent")
