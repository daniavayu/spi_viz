# Install once if needed:
# install.packages(c("shiny", "dplyr", "tidyr", "readr", "plotly", "scales"))

library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(scales)
library(spiR)

# Busca data/ desde el wd actual o la carpeta de arriba (raíz o experiments/)
find_data <- function(rel = "data/spi_index.csv") {
  for (p in c(rel, file.path("..", rel))) if (file.exists(p)) return(p)
  stop("No encuentro ", rel, ". Abre SPI_viz.Rproj o pon el wd en 'spi_viz/'.")
}

# 1) Load SPI data from local file
spi <- read_csv(find_data("data/spi_index.csv"), show_col_types = FALSE)

# 2) Prepare pillar data with simple average
pillar_cols <- c("SPI.INDEX.PIL1", "SPI.INDEX.PIL2", "SPI.INDEX.PIL3", "SPI.INDEX.PIL4", "SPI.INDEX.PIL5")
pillar_names <- c("SPI.INDEX.PIL1" = "PIL1", "SPI.INDEX.PIL2" = "PIL2", "SPI.INDEX.PIL3" = "PIL3", 
                  "SPI.INDEX.PIL4" = "PIL4", "SPI.INDEX.PIL5" = "PIL5")

spi_long <- spi %>%
  select(country, region, date, all_of(pillar_cols)) %>%
  pivot_longer(
    cols = all_of(pillar_cols),
    names_to = "pillar",
    values_to = "value"
  ) %>%
  mutate(
    date = as.integer(date),
    value = as.numeric(value),
    pillar_name = case_when(
      pillar == "SPI.INDEX.PIL1" ~ "PIL1",
      pillar == "SPI.INDEX.PIL2" ~ "PIL2",
      pillar == "SPI.INDEX.PIL3" ~ "PIL3",
      pillar == "SPI.INDEX.PIL4" ~ "PIL4",
      pillar == "SPI.INDEX.PIL5" ~ "PIL5"
    )
  ) %>%
  filter(!is.na(value), !is.na(region))

# 3) Calculate simple average by region and pillar
region_summary <- spi_long %>%
  group_by(region, date, pillar, pillar_name) %>%
  summarise(
    avg_value = mean(value, na.rm = TRUE),
    n_countries = n(),
    .groups = "drop"
  )

ui <- fluidPage(
  titlePanel("SPI Pillars Over Time - By Region (Simple Average)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "region_select",
        label = "Select Region",
        choices = sort(unique(region_summary$region)),
        selected = unique(region_summary$region)[1]
      ),
      checkboxGroupInput(
        inputId = "pillars",
        label = "Pillars to Display",
        choices = setNames(pillar_cols, pillar_names),
        selected = pillar_cols
      ),
      h4("Info"),
      textOutput("region_info"),
      textOutput("n_countries_info")
    ),
    mainPanel(
      plotlyOutput("timeseries_plot", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  
  filtered <- reactive({
    df <- region_summary %>%
      filter(region == input$region_select, pillar %in% input$pillars) %>%
      arrange(date)
    
    df
  })
  
  output$region_info <- renderText({
    df <- filtered()
    if (nrow(df) > 0) {
      paste0("Region: ", input$region_select)
    } else {
      "Region not found"
    }
  })
  
  output$n_countries_info <- renderText({
    df <- region_summary %>% 
      filter(region == input$region_select)
    if (nrow(df) > 0) {
      n_countries <- max(df$n_countries, na.rm = TRUE)
      paste0("Countries: ", n_countries)
    } else {
      ""
    }
  })
  
  output$timeseries_plot <- renderPlotly({
    df <- filtered()
    
    if (nrow(df) == 0) {
      return(
        plot_ly() %>%
          layout(title = "No data available for selected filters")
      )
    }
    
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")
    
    plot_ly(data = df, type = "scatter", mode = "lines+markers") %>%
      add_trace(
        x = ~date,
        y = ~avg_value,
        color = ~pillar_name,
        colors = colors,
        line = list(width = 2),
        marker = list(size = 6),
        hovertemplate = "<b>%{fullData.name}</b><br>Year: %{x}<br>Value: %{y:.2f}<extra></extra>"
      ) %>%
      layout(
        title = list(text = paste0("SPI Pillars: ", input$region_select, " (Simple Average)")),
        xaxis = list(title = "Year"),
        yaxis = list(title = "SPI Pillar Value (Simple Average)", range = c(0, 105)),
        hovermode = "x unified",
        legend = list(x = 0.02, y = 0.98),
        margin = list(l = 60, r = 20, b = 60, t = 100)
      )
  })
}

shinyApp(ui, server)
