# install.packages(c("dplyr", "readr", "tidyr", "ggplot2"))

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# ============================================================
# 0. coord_radar(): coord_polar con líneas rectas entre ejes
#    (necesario para que el radar se vea como polígono y no
#    con lados curvos). Basado en el patrón estándar de ggplot.
# ============================================================
coord_radar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CoordRadar", CoordPolar,
          theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

# ============================================================
# 1. Datos SPI: solo los 5 pilares en formato largo
# ============================================================
# Busca data/spi_index.csv en el wd actual o en la carpeta de arriba
# (funciona igual si corres desde la raíz del proyecto o desde viz_functions/)
find_data <- function(rel = "data/spi_index.csv") {
  for (p in c(rel, file.path("..", rel))) {
    if (file.exists(p)) return(p)
  }
  stop("No encuentro ", rel, ". Abre el proyecto SPI_viz.Rproj o pon el ",
       "working directory en la carpeta 'spi_viz/'.")
}

spi_index <- read_csv(find_data("data/spi_index.csv"), show_col_types = FALSE)

pillar_cols <- c("SPI.INDEX.PIL1", "SPI.INDEX.PIL2", "SPI.INDEX.PIL3",
                 "SPI.INDEX.PIL4", "SPI.INDEX.PIL5")

# etiquetas cortas para los ejes del radar
pillar_short <- c("SPI.INDEX.PIL1" = "PIL1\nData Use",
                  "SPI.INDEX.PIL2" = "PIL2\nData Services",
                  "SPI.INDEX.PIL3" = "PIL3\nData Products",
                  "SPI.INDEX.PIL4" = "PIL4\nData Sources",
                  "SPI.INDEX.PIL5" = "PIL5\nData Infra.")

spi_pillars <- spi_index %>%
  select(country, iso3c, date, region, income, all_of(pillar_cols)) %>%
  pivot_longer(
    cols      = all_of(pillar_cols),
    names_to  = "pillar",
    values_to = "value"
  )

# ============================================================
# 2. Paleta oficial WB Data Viz Style Guide
#    https://worldbank.github.io/data-visualization-style-guide/colors
# ============================================================
wb_country     <- "#0071BC"   # selection1  -> país seleccionado
wb_reference   <- "#8A969F"   # reference   -> benchmark regional
wb_text        <- "#111111"   # text
wb_text_subtle <- "#666666"   # textSubtle
wb_grid        <- "#EBEEF4"   # grey100     -> líneas de grilla

# ============================================================
# 3. La función: create_pillar_performance(country, year)
#    Radar de los 5 pilares del país (relleno) vs. el promedio
#    de su región (línea punteada) para un año dado.
# ============================================================
create_pillar_performance <- function(country,
                                      year = 2024,
                                      data = spi_pillars) {

  # --- año disponible ---
  if (!year %in% data$date) {
    stop("Año no disponible: ", year, ". Usa uno de: ",
         paste(sort(unique(data$date)), collapse = ", "))
  }
  df_year <- data %>% filter(.data$date == !!year)

  # --- validar país ---
  if (!country %in% df_year$country) {
    stop("País no encontrado para ", year, ": ", country)
  }

  # --- región del país (se detecta sola) ---
  region <- df_year %>%
    filter(.data$country == !!country) %>%
    pull(region) %>%
    unique()
  region <- region[!is.na(region)][1]
  if (is.na(region) || length(region) == 0) {
    stop("No encontré la región para el país: ", country)
  }

  # --- serie del país ---
  country_df <- df_year %>%
    filter(.data$country == !!country) %>%
    transmute(pillar, value, serie = "country")

  # --- promedio regional por pilar (benchmark) ---
  region_df <- df_year %>%
    filter(.data$region == !!region) %>%
    group_by(pillar) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(serie = "region")

  plot_df <- bind_rows(country_df, region_df) %>%
    mutate(
      pillar = factor(pillar, levels = pillar_cols, labels = pillar_short[pillar_cols]),
      serie  = factor(serie, levels = c("country", "region"))
    )

  # etiquetas de leyenda con nombres reales
  serie_labels <- c(country = country,
                    region  = paste0(region, " (avg.)"))

  ggplot(plot_df, aes(x = pillar, y = value, group = serie)) +
    geom_polygon(aes(colour = serie, fill = serie, linetype = serie),
                 linewidth = 1, alpha = 0.15) +
    geom_point(aes(colour = serie), size = 2.2) +
    coord_radar() +
    scale_y_continuous(limits = c(0, 100),
                       breaks = c(20, 40, 60, 80, 100)) +
    scale_colour_manual(values = c(country = wb_country,
                                   region  = wb_reference),
                        labels = serie_labels, name = NULL) +
    scale_fill_manual(values = c(country = wb_country,
                                 region  = NA),
                      labels = serie_labels, name = NULL) +
    scale_linetype_manual(values = c(country = "solid",
                                     region  = "dashed"),
                          labels = serie_labels, name = NULL) +
    labs(
      title    = "Pillar performance",
      subtitle = paste0(country, " vs. promedio regional de ", region,
                        "  ·  ", year),
      x        = NULL,
      y        = NULL,
      caption  = "Source: World Bank Statistical Performance Indicators (SPI)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = wb_grid),
      axis.text.y      = element_text(colour = wb_text_subtle, size = 8),
      axis.text.x      = element_text(colour = wb_text, face = "bold"),
      plot.title       = element_text(face = "bold", colour = wb_text),
      plot.subtitle    = element_text(colour = wb_text_subtle),
      plot.caption     = element_text(colour = wb_text_subtle),
      legend.position  = "top"
    )
}

# ============================================================
# 4. Ejemplos
# ============================================================
create_pillar_performance("Chile")
# create_pillar_performance("Kenya", 2020)
# create_pillar_performance("Peru",  2024)
