library(tidyverse)
library(janitor)
library(showtext)
library(ggtext)

font_add_google("Roboto", "roboto")
showtext_auto()

showtext_opts(dpi = 300) 

hogares <- read.csv("../../../censo_2024/Bases-Finales-CPV2024SV-CSV/Base de Datos de Hogares - CPV 2024 SV.csv")

recode_deptos <- c(
  "1" = "Ahuachapán",
  "2" = "Santa Ana",
  "3" = "Sonsonate",
  "4" = "Chalatenango",
  "5" = "La Libertad",
  "6" = "San Salvador",
  "7" = "Cuscatlán",
  "8" = "La Paz",
  "9" = "Cabañas",
  "10" = "San Vicente",
  "11" = "Usulután",
  "12" = "San Miguel",
  "13" = "Morazán",
  "14" = "La Unión"
)

codigos_distritos <- read.csv("catalogo-de-municipios-y-distritos.csv", 
                    fileEncoding = "latin1",
                    sep = ";") |> 
  clean_names()


ac_dep <- hogares |> 
  group_by(DEPTO, H02_5_HOG_AC) |> 
  summarise(n = n(), .groups = "drop") |> 
  drop_na() |> 
  mutate(
    DEPTO = recode(DEPTO, !!!recode_deptos),
    H02_5_HOG_AC = recode(H02_5_HOG_AC,
                          `1` = "Sí",
                          `2` = "No",
                          `9` = "NS/NR"
    ),
    H02_5_HOG_AC = factor(H02_5_HOG_AC,
                          levels = c("Sí", "No", "NS/NR")
    )
  ) |> 
  group_by(DEPTO) |> 
  mutate(prop = n / sum(n))

orden_deptos <- ac_dep |> 
  group_by(DEPTO) |> 
  mutate(prop = n / sum(n)) |> 
  filter(H02_5_HOG_AC == "Sí") |> 
  arrange(desc(prop)) |> 
  pull(DEPTO)

ac_dist <- hogares |> 
  group_by(DISTODESC, H02_5_HOG_AC) |> 
  summarise(n = n(), .groups = "drop") |> 
  drop_na() |> 
  mutate(
    DISTODESC = recode_values(DISTODESC, 
                              from = codigos_distritos$codigo_distritos, 
                              to = codigos_distritos$distritos),
    DISTODESC = recode(DISTODESC, 
                              "Santa Tecla antes: Nueva San Salvador" = "Santa Tecla"),
    H02_5_HOG_AC = recode(H02_5_HOG_AC, `1` = "Sí", `2` = "No", `9` = "NS/NR"),
    H02_5_HOG_AC = factor(H02_5_HOG_AC, levels = c("Sí", "No", "NS/NR"))
  ) |> 
  group_by(DISTODESC) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |>                     
  filter(H02_5_HOG_AC == "Sí") |> 
  slice_max(order_by = prop, n = 10) |>
  pull(DISTODESC) -> orden_dist


ac_dist_final <- hogares |> 
  group_by(DISTODESC, H02_5_HOG_AC) |> 
  summarise(n = n(), .groups = "drop") |> 
  drop_na() |> 
  mutate(
    DISTODESC = recode_values(DISTODESC, 
                              from = codigos_distritos$codigo_distritos, 
                              to = codigos_distritos$distritos),
    H02_5_HOG_AC = recode(H02_5_HOG_AC,
                          `1` = "Sí",
                          `2` = "No",
                          `9` = "NS/NR"
    ),
    H02_5_HOG_AC = factor(H02_5_HOG_AC,
                          levels = c("Sí", "No", "NS/NR"),
    ),
    DISTODESC = recode(DISTODESC, 
                       "Santa Tecla antes: Nueva San Salvador" = "Santa Tecla"),
  ) |> 
  filter(DISTODESC %in% orden_dist) |> 
  group_by(DISTODESC) |> 
  mutate(prop = n / sum(n))

pal <- c("Sí" = "#2E5A88", "No" = "#D97A5B", "NS/NR" = "#D3D3D3")

ggplot(ac_dep, aes(
  fill = H02_5_HOG_AC,
  y = factor(DEPTO, levels = rev(orden_deptos)),
  x = prop
)) +
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
  scale_fill_manual(values = pal) +
  scale_x_continuous(
    position = "top",
    expand = expansion(mult = c(0, 0.05)), 
    breaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), 
    labels = c("0", "20%", "40%", "60%", "80%", "100%")) +
  scale_y_discrete(expand = c(0,0)) +
  theme_minimal(base_size = 22, base_family = "roboto") + 
  labs(x = "Porcentaje de hogares", y = "Distrito",
       fill = "¿Posee aire acondicionado?", 
       title = "San Miguel tiene el mayor porcentaje de hogares con aire acondicionado") +
  theme(
    plot.title.position = "plot", 
    legend.location = "plot",
    legend.position = "top",
    legend.justification = "left", 
    legend.direction = "horizontal",
    legend.margin = margin(l = 0, b = 10),
    panel.grid = element_blank()
  )

ggsave("aire_acondicionado_censo_2024_depto.png", width = 13, height = 10, dpi = 300)


ggplot(ac_dist_final, aes(
  fill = H02_5_HOG_AC,
  y = factor(DISTODESC, levels = rev(orden_dist)),
  x = prop
)) +
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
  scale_fill_manual(values = pal) +
  scale_x_continuous(
    position = "top",
    expand = expansion(mult = c(0, 0.05)), 
    breaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), 
    labels = c("0", "20%", "40%", "60%", "80%", "100%")) +
  scale_y_discrete(expand = c(0,0)) +
  theme_minimal(base_size = 22, base_family = "roboto") + 
  labs(x = "Porcentaje de hogares", y = "Distrito",
       fill = "¿Posee aire acondicionado?", 
       title = "Antiguo Cuscatlán tiene el mayor <span style='color:#2E5A88;'>porcentaje de hogares con aire acondicionado</span>",
       subtitle = "Top 10 Distritos de El Salvador con mayor porcentaje de hogares con aire acondicionado.\nResultados del Censo de Población y Vivienda 2024. Fuente: Banco Central de Reserva",
       caption = "\n#30DayChartChallenge, Day 1\nPart-to-Whole\nMario Reyes") +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.title.position = "plot", 
    legend.location = "plot",
    legend.position = "top",
    legend.justification = "left", 
    legend.direction = "horizontal",
    legend.margin = margin(l = 0, b = 10),
    panel.grid = element_blank()
  )

ggsave("aire_acondicionado_censo_2024_dist.png", width = 14, height = 10, dpi = 300)