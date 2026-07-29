# install.packages(c("dplyr", "readr", "tidyr", "ggplot2"))

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# ============================================================
# 1. Datos SPI: solo los 5 pilares en formato largo
# ============================================================
# Busca data/spi_index.csv en el wd actual o en la carpeta de arriba
# (funciona desde la raíz del proyecto o desde viz_functions/)
find_data <- function(rel = "data/spi_index.csv") {
  for (p in c(rel, file.path("..", rel))) {
    if (file.exists(p)) return(p)
  }
  stop("No encuentro ", rel, ". Abre SPI_viz.Rproj o pon el working ",
       "directory en la carpeta 'spi_viz/'.")
}

spi_index <- read_csv(find_data("data/spi_index.csv"), show_col_types = FALSE)

pillar_cols <- c("SPI.INDEX.PIL1", "SPI.INDEX.PIL2", "SPI.INDEX.PIL3",
                 "SPI.INDEX.PIL4", "SPI.INDEX.PIL5")

# etiquetas de una línea (horizontal, no hace falta cortar)
pillar_labels_1l <- c("SPI.INDEX.PIL1" = "PIL1: Data Use",
                      "SPI.INDEX.PIL2" = "PIL2: Data Services",
                      "SPI.INDEX.PIL3" = "PIL3: Data Products",
                      "SPI.INDEX.PIL4" = "PIL4: Data Sources",
                      "SPI.INDEX.PIL5" = "PIL5: Data Infrastructure")

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
wb_stem        <- "#CED4DE"   # noData grey -> tallo del lollipop
wb_text        <- "#111111"   # text
wb_text_subtle <- "#666666"   # textSubtle
wb_grid        <- "#EBEEF4"   # grey100     -> líneas de grilla

# ============================================================
# 3. La función: create_pillar_lollipop(country, year)
#    Barras/lollipop horizontales de los 5 pilares del país
#    (punto azul) con el promedio de su región como referencia
#    (rombo gris). Alternativa recomendada al radar.
# ============================================================
create_pillar_lollipop <- function(country,
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

  # --- valor del país por pilar ---
  country_df <- df_year %>%
    filter(.data$country == !!country) %>%
    select(pillar, country_val = value)

  # --- promedio regional por pilar (benchmark) ---
  region_df <- df_year %>%
    filter(.data$region == !!region) %>%
    group_by(pillar) %>%
    summarise(region_val = mean(value, na.rm = TRUE), .groups = "drop")

  plot_df <- country_df %>%
    left_join(region_df, by = "pillar") %>%
    mutate(
      pillar_lab = pillar_labels_1l[pillar],
      # ordena los pilares por el valor del país (mayor arriba)
      pillar_lab = reorder(pillar_lab, country_val)
    )

  # capa "larga" solo para construir la leyenda con los dos marcadores
  legend_df <- plot_df %>%
    tidyr::pivot_longer(c(country_val, region_val),
                        names_to = "serie", values_to = "value") %>%
    mutate(serie = factor(
      ifelse(serie == "country_val", "country", "region"),
      levels = c("country", "region")
    ))

  serie_labels <- c(country = country,
                    region  = paste0(region, " (avg.)"))

  ggplot(plot_df, aes(y = pillar_lab)) +
    # tallo del lollipop: de 0 al valor del país
    geom_segment(aes(x = 0, xend = country_val,
                     yend = pillar_lab),
                 colour = wb_stem, linewidth = 1.2) +
    # puntos país + región (con color mapeado -> genera leyenda)
    geom_point(data = legend_df,
               aes(x = value, colour = serie, shape = serie),
               size = 3.4) +
    # etiqueta con el valor del país
    geom_text(aes(x = country_val, label = round(country_val)),
              hjust = -0.5, size = 3.2, colour = wb_text) +
    scale_colour_manual(values = c(country = wb_country,
                                   region  = wb_reference),
                        labels = serie_labels, name = NULL) +
    scale_shape_manual(values = c(country = 19, region = 18),  # círculo / rombo
                       labels = serie_labels, name = NULL) +
    scale_x_continuous(limits = c(0, 105),
                       breaks = c(0, 20, 40, 60, 80, 100)) +
    labs(
      title    = "Pillar performance",
      subtitle = paste0(country, " vs. promedio regional de ", region,
                        "  ·  ", year),
      x        = "Score",
      y        = NULL,
      caption  = "Source: World Bank Statistical Performance Indicators (SPI)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = wb_grid),
      plot.title         = element_text(face = "bold", colour = wb_text),
      plot.subtitle      = element_text(colour = wb_text_subtle),
      plot.caption       = element_text(colour = wb_text_subtle),
      axis.text          = element_text(colour = wb_text),
      axis.title         = element_text(colour = wb_text_subtle),
      legend.position    = "top"
    )
}

# ============================================================
# 4. Ejemplos
# ============================================================
create_pillar_lollipop("Chile")
# create_pillar_lollipop("Kenya", 2020)
# create_pillar_lollipop("Peru",  2024)
