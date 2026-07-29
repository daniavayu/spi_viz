# install.packages(c("dplyr", "readr", "ggplot2", "sf", "stringr", "ggiraph"))
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(stringr)
library(ggiraph)   # makes map interactive

# ------------------------------------------------------------
# Busca un archivo en data/ desde el wd actual o la carpeta de
# arriba (funciona desde la raíz del proyecto o desde viz_functions/)
# ------------------------------------------------------------
find_data <- function(rel) {
  for (p in c(rel, file.path("..", rel))) {
    if (file.exists(p)) return(p)
  }
  stop("No encuentro ", rel, ". Abre SPI_viz.Rproj o pon el working ",
       "directory en la carpeta 'spi_viz/'.")
}

# ============================================================
# 1. WB oficial boundaries
#    https://datacatalog.worldbank.org/search/dataset/0038272/
#    world-bank-official-boundaries  -> capa admin 0 (países)
#
#    Se lee DIRECTO desde el .zip con el sistema virtual de GDAL
#    (/vsizip/), así no hay que descomprimir ni preocuparse por
#    los archivos hermanos (.shx, .dbf, .prj).
# ============================================================
wb_boundaries_zip <- "data/World Bank Official Boundaries - Admin 0.zip"
wb_boundaries_shp <- "WB_GAD_ADM0.shp"   # archivo dentro del zip

load_wb_boundaries <- function(zip = wb_boundaries_zip,
                               shp = wb_boundaries_shp) {
  # ruta virtual: /vsizip/<ruta-absoluta-al-zip>/<archivo.shp>
  vsi_path <- file.path("/vsizip", normalizePath(find_data(zip), winslash = "/"), shp)
  b <- sf::st_read(vsi_path, quiet = TRUE)
  
  # detecta automáticamente la columna con el código ISO3
  iso_col <- names(b)[str_detect(
    names(b),
    regex("^(ISO_A3|WB_A3|ISO3|ISO_3|ISO_A3_EH)$", ignore_case = TRUE)
  )][1]
  if (is.na(iso_col)) {
    stop("No encontré columna ISO3. Columnas disponibles: ",
         paste(names(b), collapse = ", "))
  }

  # detecta la columna con el nombre del país (para el tooltip)
  name_col <- names(b)[str_detect(
    names(b),
    regex("^(NAM_0|WB_NAME|NAME_EN|NAME|ADMIN|COUNTRY)$", ignore_case = TRUE)
  )][1]

  b <- b %>%
    mutate(
      iso3    = toupper(as.character(.data[[iso_col]])),
      country = if (!is.na(name_col)) as.character(.data[[name_col]]) else iso3
    ) %>%
    filter(!is.na(iso3), iso3 != "-99")

  b %>% select(iso3, country, geometry)
}

world <- load_wb_boundaries()   # se carga una sola vez

# ============================================================
# 2. Datos SPI (índice principal)
# ============================================================
spi <- read_csv(find_data("data/spi_index.csv"), show_col_types = FALSE) %>%
  transmute(
    iso3  = toupper(trimws(iso3c)),
    year  = as.integer(date),
    value = as.numeric(SPI.INDEX)
  ) %>%
  filter(!is.na(iso3), nchar(iso3) == 3, !is.na(value))


# ============================================================
# 3. Paleta oficial WB Data Viz Style Guide
#    Escala secuencial "Bad to Good" (seq1..seq5): mayor SPI = mejor.
#    https://worldbank.github.io/data-visualization-style-guide/colors
# ============================================================
wb_seq_good   <- c("#FDF6DB", "#A1CBCF", "#5D99C2", "#2868A0", "#023B6F")  # seq1..seq5
wb_no_data    <- "#CED4DE"   # noData
wb_border     <- "#FFFFFF"   # contorno de países
wb_text       <- "#111111"   # text
wb_text_subtle<- "#666666"   # textSubtle

# ============================================================
# 4. La función: create_spimap(year, country = NULL)
# ============================================================
create_spimap <- function(year,
                          country     = NULL,       # ISO3, opcional (ej. "CHL")
                          interactive = TRUE,       # TRUE = tooltip al pasar el mouse
                          spi_data    = spi,
                          world_sf    = world) {
  
  yr  <- year
  df  <- spi_data %>% filter(.data$year == yr)
  
  # si se pide país(es): recorta el mapa y los datos a ellos (zoom)
  if (!is.null(country) && length(country) > 0) {
    sel      <- toupper(country)
    world_sf <- world_sf %>% filter(.data$iso3 %in% sel)
    df       <- df       %>% filter(.data$iso3 %in% sel)
    if (nrow(world_sf) == 0) stop("Ningún ISO3 coincide: ", paste(sel, collapse = ", "))
  }
  
  map_df <- world_sf %>%
    left_join(df, by = "iso3") %>%
    mutate(
      # texto que se muestra al pasar el mouse
      tooltip = paste0(
        country, " (", iso3, ")\n",
        "SPI Index: ",
        ifelse(is.na(value), "sin dato", format(round(value, 1), nsmall = 1))
      )
    )
  
  p <- ggplot(map_df) +
    geom_sf_interactive(
      aes(fill = value, tooltip = tooltip, data_id = iso3),
      color = wb_border, linewidth = 0.1
    ) +
    scale_fill_gradientn(
      colours  = wb_seq_good,
      na.value = wb_no_data,
      limits   = c(0, 100),                  # escala fija -> comparable entre años
      name     = "SPI Index"
    ) +
    coord_sf(crs = "+proj=natearth") +       # cambia a "+proj=robin" si prefieres
    labs(
      title   = paste0("SPI Index | ", yr),
      caption = "Source: World Bank Statistical Performance Indicators (SPI)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text   = element_blank(),
      axis.ticks  = element_blank(),
      panel.grid  = element_blank(),
      plot.title  = element_text(face = "bold", colour = wb_text),
      plot.caption = element_text(colour = wb_text_subtle),
      legend.title = element_text(colour = wb_text)
    )
  
  # estático (ggplot normal) o interactivo (HTML con tooltip)
  if (!interactive) return(p)
  
  girafe(
    ggobj = p,
    options = list(
      opts_hover(css = "stroke:#111111;stroke-width:0.8px;"),
      opts_tooltip(css = paste0(
        "background:#FFFFFF;color:#111111;border:1px solid #CED4DE;",
        "padding:6px 8px;border-radius:4px;font-family:sans-serif;font-size:12px;"
      ))
    )
  )
}

# ============================================================
# 4. Ejemplos
# ============================================================
create_spimap(2016)                       
create_spimap(2024, country = "CHL")   
#create_spimap(2024, country = c("CHL","ARG","PER","BOL"))  # región