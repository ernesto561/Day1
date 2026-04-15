library(dplyr)
library(ggplot2)
library(lubridate)
library(showtext)
library(geomtextpath)
library(glue)

font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 300) 

lluvia_horaria <- read.csv("procafe/proc_hourly.csv")
lluvia_horas <- lluvia_horaria |> 
  mutate(date_hour = ymd_hms(date_hour)) |>
  mutate(hora = hour(date_hour)) |>
  group_by(hora) |> 
  summarise(sum_hora = sum(hourly_rain, na.rm = TRUE)) |>
  mutate(pct = sum_hora / sum(sum_hora) * 100)

clr_bg      <- "#FAFAF6"
clr_text    <- "#1A1A18"
clr_text2   <- "#6B6B65"
clr_accent  <- "steelblue"
clr_accent2 <- "#3D4F9F"
clr_grid    <- "#E0E0D8"

max_pct <- ceiling(max(lluvia_horas$pct, na.rm = TRUE))

# Porcentaje acumulado horas > 18
pct_tarde <- lluvia_horas |> 
  filter(hora > 18) |> 
  pull(pct) |> 
  sum() |> 
  round(0)

arrow_segment_df <- tibble(
  x    = 18,
  y    = max_pct * 1.4,
  xend = 23.5,
  yend = max_pct * 1.4,
  label = glue("{pct_tarde}% de la lluvia")
)

p <- ggplot(lluvia_horas, aes(x = hora, y = pct)) +  
  geom_col(width = 0.75, fill = clr_accent) +
  geom_textsegment(
    data = arrow_segment_df, 
    mapping = aes(x = x, y = y, xend = xend, yend = yend, label = label),
    color = "cornflowerblue",
    family = "roboto",
    size = 6,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"), 
    inherit.aes = FALSE, linewidth = 0.5) + 
  scale_x_continuous(
    limits = c(-0.5, 23.5),
    expand = c(0, 0),
    breaks = c(0, 3, 6, 9, 12, 15, 18, 21),
    labels = c("0:00", "3:00", "6:00", "9:00", "12:00", "15:00", "18:00", "21:00")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, max_pct, 5),
    limits = c(0, max_pct),
    labels = scales::label_percent(scale = 1),
    oob    = scales::oob_keep
  ) +
  coord_radial(
    start         = -2 * pi * (0.5 / 24),
    end           =  2 * pi - 2 * pi * (0.5 / 24),
    inner.radius  = 0.25,
    rotate_angle  = FALSE,
    r.axis.inside = 3,
    clip          = "off"
  ) +
  guides(theta = "axis_textpath") +
  labs(
    title    = "El 40% de la lluvia total cae entre las 18:00 y medianoche",
    subtitle = "Porcentaje de lluvia por hora del día",
    caption  = "Fuente: MARN"
  ) +
  theme_void(base_family = "roboto", base_size = 14) +
  theme(
    legend.position  = "none",
    plot.background  = element_rect(fill = clr_bg, color = NA),
    panel.grid.major.x = element_line(color = clr_grid, linewidth = 0.5, linetype = "dashed"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray50", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.text.y = element_text(size = 8, color = clr_text2),
    plot.title.position = "plot",
    plot.title    = element_text(face = "bold", size = 36, hjust = 0),
    plot.subtitle = element_text(size = 25, hjust = 0, margin = margin(t = 10, b = 50)),
    plot.caption  = element_text(size = 12, hjust = 1, margin = margin(t = 30, b = 6.5)),
    plot.margin   = margin(t = 16, r = 60, b = 34, l = 60)
  )

ggsave("lluvia_horaria2.png", p, width = 8, height = 10, dpi = 300)