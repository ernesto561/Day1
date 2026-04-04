library(tidyverse)
library(ggmosaic)

vivienda <- read.csv("../../../censo_2024/Bases-Finales-CPV2024SV-CSV/Base de Datos de Viviendas - CPV 2024 SV.csv")

tenencia <- vivienda |> 
  group_by(V07_VIV_TENENCIA) |> 
  summarise(n = n(), .groups = "drop") |> 
  drop_na() |>
  mutate(prop = round(n / sum(n)*100, 0))

vivienda_mp <- vivienda %>%
  mutate(
    tenencia = factor(V07_VIV_TENENCIA),
    area = factor(AREA)
  ) |> 
  drop_na(tenencia)

ggplot(vivienda_mp) +
  geom_mosaic(aes(
    x = product(tenencia, area),
    fill = tenencia
  )) +
  geom_mosaic_text(aes(x = product(tenencia, area), 
                       label = after_stat(paste0(round(.wt/sum(.wt)*100, 1), "%")))) +
  labs(title = "Tenencia de vivienda por área")



# 1. Create the base mosaic plot
p <- ggplot(data = vivienda_mp) +
  geom_mosaic(aes(x = product(tenencia, area), fill = tenencia))

# 2. Extract calculated data to add percentage labels
plot_data <- ggplot_build(p)$data[[1]]

# 3. Add labels (example uses .wt which is the count)
p + geom_text(data = plot_data, 
              aes(x = (xmin + xmax) / 2, 
                  y = (ymin + ymax) / 2, 
                  label = scales::percent(prop))) # 'prop' is often the cell proportion