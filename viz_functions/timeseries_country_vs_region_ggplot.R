# install.packages(c("dplyr", "readr", "tidyr", "ggplot2"))

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# ============================================================
# 1. Datos SPI (formato largo: un pilar por fila)
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

spi_long <- spi_index %>%
  pivot_longer(
    # índice general + los 5 pilares
    cols      = c("SPI.INDEX", starts_with("SPI.INDEX.PIL")),
    names_to  = "pillar",
    values_to = "value"
  )

# etiquetas legibles de cada pilar (+ el índice general)
pillar_labels <- c(
  "SPI.INDEX"      = "SPI Overall Index",
  "SPI.INDEX.PIL1" = "PIL1: Data Use",
  "SPI.INDEX.PIL2" = "PIL2: Data Services",
  "SPI.INDEX.PIL3" = "PIL3: Data Products",
  "SPI.INDEX.PIL4" = "PIL4: Data Sources",
  "SPI.INDEX.PIL5" = "PIL5: Data Infrastructure"
)

# ============================================================
# 2. Paleta oficial WB Data Viz Style Guide
#    https://worldbank.github.io/data-visualization-style-guide/colors
# ============================================================
wb_country    <- "#0071BC"   # selection1  -> país seleccionado
wb_reference  <- "#8A969F"   # reference   -> benchmark regional
wb_text       <- "#111111"   # text
wb_text_subtle<- "#666666"   # textSubtle
wb_grid       <- "#EBEEF4"   # grey100     -> líneas de grilla

# ============================================================
# 3. La función: create_country_vs_region(country, pillar)
#    Compara un país (línea sólida) contra el promedio de su
#    región (línea punteada) para un pilar a lo largo del tiempo.
# ============================================================
create_country_vs_region <- function(country,
                                      pillar = "SPI.INDEX.PIL1",
                                      data   = spi_long) {

  # admite "PIL1" o el nombre completo "SPI.INDEX.PIL1"
  if (grepl("^PIL[1-5]$", pillar, ignore.case = TRUE)) {
    pillar <- paste0("SPI.INDEX.", toupper(pillar))
  }
  if (!pillar %in% data$pillar) {
    stop("Pilar no válido: ", pillar, ". Usa uno de: ",
         paste(unique(data$pillar), collapse = ", "))
  }

  pillar_label <- ifelse(pillar %in% names(pillar_labels),
                         pillar_labels[[pillar]], pillar)

  # región del país seleccionado (se detecta sola)
  region <- data %>%
    filter(.data$country == !!country) %>%
    pull(region) %>%
    unique()
  region <- region[!is.na(region)][1]
  if (is.na(region) || length(region) == 0) {
    stop("No encontré la región para el país: ", country)
  }

  # serie del país
  country_data <- data %>%
    filter(.data$country == !!country, .data$pillar == !!pillar) %>%
    arrange(date) %>%
    transmute(date, value, serie = "country")

  # promedio regional por año (benchmark)
  region_data <- data %>%
    filter(.data$region == !!region, .data$pillar == !!pillar) %>%
    group_by(date) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(date) %>%
    transmute(date, value, serie = "region")

  plot_df <- bind_rows(country_data, region_data) %>%
    mutate(serie = factor(serie, levels = c("country", "region")))

  # etiquetas de leyenda con nombres reales
  serie_labels <- c(country = country,
                    region  = paste0(region, " (avg.)"))

  ggplot(plot_df, aes(x = date, y = value,
                      colour = serie, linetype = serie)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_colour_manual(values = c(country = wb_country,
                                   region  = wb_reference),
                        labels = serie_labels, name = NULL) +
    scale_linetype_manual(values = c(country = "solid",
                                     region  = "dashed"),
                          labels = serie_labels, name = NULL) +
    scale_y_continuous(limits = c(0, 100)) +   # escala fija -> comparable
    labs(
      title    = pillar_label,
      subtitle = paste0(country, " vs. promedio regional de ", region),
      x        = NULL,
      y        = "Score",
      caption  = "Source: World Bank Statistical Performance Indicators (SPI)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = wb_grid),
      plot.title       = element_text(face = "bold", colour = wb_text),
      plot.subtitle    = element_text(colour = wb_text_subtle),
      plot.caption     = element_text(colour = wb_text_subtle),
      axis.text        = element_text(colour = wb_text_subtle),
      axis.title       = element_text(colour = wb_text),
      legend.position  = "top"
    )
}

# ============================================================
# 4. Ejemplos
# ============================================================
create_country_vs_region("Chile", "PIL1")
# create_country_vs_region("Kenya", "SPI.INDEX.PIL3")
# create_country_vs_region("Peru",  "SPI.INDEX")   # índice general
