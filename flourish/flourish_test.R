# ==========================================
# 1. Instalar paquete (solo la primera vez)
# ==========================================

install.packages("flourishcharts")

# Si necesitas la versión de GitHub:
# remotes::install_github("canva-public/flourish-charts", subdir = "R_package")


# ==========================================
# 2. Cargar librerías
# ==========================================

library(flourishcharts)
library(dplyr)
library(spiR)
library(htmlwidgets)


# ==========================================
# 3. Verificar API Key
# ==========================================

Sys.getenv("FLOURISH_API_KEY")


# ==========================================
# 4. Obtener datos frescos
# ==========================================

spi_index <- spi_index()


spi_index_2024 <- spi_index %>% 
  filter(date == 2024) %>% 
  select(country, iso3c, SPI.INDEX) %>% 
  rename(
    region = country,
    Economy = iso3c,
    SPI.INDEX = SPI.INDEX
  )


# Revisar datos
head(spi_index_2024)
names(spi_index_2024)


# ==========================================
# 5. Crear visualización basada en Flourish
# ==========================================

grafico <- flourish(
  data = spi_index_2024,
  base_visualisation_id = 26427135,
  api_key = Sys.getenv("FLOURISH_API_KEY")
)


# ==========================================
# 6. Guardar para abrir en navegador
# ==========================================

htmlwidgets::saveWidget(
  grafico,
  "flourish/flourish_test.html",
  selfcontained = TRUE
)

print(grafico)





# =========================================================
# PIP -> Flourish Automated Chart Update Pipeline
# =========================================================

library(httr2)
library(dplyr)
library(flourishcharts)
library(spiR)

# Config (edit these as needed)
FLOURISH_VIS_ID   <- 29585443
LOG_FILE          <- "flourish_update_log.txt"
PIP_POVLINE       <- 2.15   # international poverty line (USD/day)
PIP_COUNTRY       <- "all"  # or e.g. "KEN" for a single country
PIP_YEAR          <- "all"

# ---- 1. Pull latest data from the PIP API ----
get_pip_data <- function(country = PIP_COUNTRY, povline = PIP_POVLINE, year = PIP_YEAR) {
  req <- request("https://api.worldbank.org/pip/v1/pip") |>
    req_url_query(
      country = country,
      povline = povline,
      year    = year,
      format  = "json"
    )
  resp <- req_perform(req)
  df   <- resp_body_json(resp, simplifyVector = TRUE)
  if (is.null(df) || nrow(df) == 0) {
    stop("PIP API returned no data — aborting update.")
  }
  as_tibble(df)
}

# ---- 2. Validate the data before pushing (avoid publishing garbage) ----
validate_pip_data <- function(df) {
  required_cols <- c("country_name", "reporting_year", "headcount")  # adjust to actual PIP fields
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(paste("Missing expected columns:", paste(missing, collapse = ", ")))
  }
  if (nrow(df) < 1) {
    stop("Data frame is empty after retrieval.")
  }
  invisible(TRUE)
}

# ---- 3. Push the cleaned data to the existing Flourish visualization ----
update_flourish_chart <- function(df) {
  flourish(
    chart_type            = "heatmap",
    chart_description     = "PIP poverty data - auto-updated",
    base_visualisation_id = FLOURISH_VIS_ID,
    api_key               = Sys.getenv("API_Flourish"),
    data                  = df   # check ?flourish for the exact expected data argument/shape
  )
}

# ---- 4. Wrap everything with logging and error handling ----
refresh_chart_safe <- function() {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  tryCatch({
    df <- get_pip_data()
    validate_pip_data(df)
    update_flourish_chart(df)
    cat(sprintf("[%s] SUCCESS: Chart %s updated with %d rows.\n",
                timestamp, FLOURISH_VIS_ID, nrow(df)),
        file = LOG_FILE, append = TRUE)
  }, error = function(e) {
    cat(sprintf("[%s] ERROR: %s\n", timestamp, conditionMessage(e)),
        file = LOG_FILE, append = TRUE)
  })
}

# ---- 5. Run it ----
refresh_chart_safe()