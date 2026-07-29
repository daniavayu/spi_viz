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

# 2) Prepare pillar data
pillar_cols <- c("SPI.INDEX.PIL1", "SPI.INDEX.PIL2", "SPI.INDEX.PIL3", "SPI.INDEX.PIL4", "SPI.INDEX.PIL5")
pillar_names <- c("SPI.INDEX.PIL1" = "PIL1", "SPI.INDEX.PIL2" = "PIL2", "SPI.INDEX.PIL3" = "PIL3", 
                  "SPI.INDEX.PIL4" = "PIL4", "SPI.INDEX.PIL5" = "PIL5")

spi_long <- spi %>%
  select(country, iso3c, date, region, all_of(pillar_cols)) %>%
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
  filter(!is.na(value))

ui <- fluidPage(
  titlePanel("SPI Pillars Over Time - Single Country"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "region_select",
        label = "Filter by Region (optional)",
        choices = c("All", sort(unique(na.omit(spi_long$region)))),
        selected = "All"
      ),
      uiOutput("country_selector_ui"),
      checkboxGroupInput(
        inputId = "pillars",
        label = "Pillars to Display",
        choices = setNames(pillar_cols, pillar_names),
        selected = pillar_cols
      ),
      h4("Info"),
      textOutput("country_info"),
      textOutput("region_info")
    ),
    mainPanel(
      plotlyOutput("timeseries_plot", height = "700px")
    )
  )
)

server <- function(input, output, session) {

  # Dynamic country selector based on region
  output$country_selector_ui <- renderUI({
    countries <- if (input$region_select == "All") {
      sort(unique(spi_long$country))
    } else {
      spi_long %>%
        filter(region == input$region_select) %>%
        pull(country) %>%
        unique() %>%
        sort()
    }
    
    selectizeInput(
      inputId = "country_select",
      label = "Select Country",
      choices = countries,
      selected = head(countries, 1),
      options = list(maxOptions = 300)
    )
  })

  filtered <- reactive({
    df <- spi_long %>%
      filter(country == input$country_select, pillar %in% input$pillars) %>%
      arrange(date)
    
    df
  })
  
  output$country_info <- renderText({
    df <- spi_long %>% filter(country == input$country_select)
    if (nrow(df) > 0) {
      iso3c <- unique(df$iso3c)[1]
      paste0("ISO3: ", iso3c)
    } else {
      "Country not found"
    }
  })
  
  output$region_info <- renderText({
    df <- spi_long %>% filter(country == input$country_select)
    if (nrow(df) > 0) {
      region <- unique(df$region)[1]
      paste0("Region: ", region)
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
        y = ~value,
        color = ~pillar_name,
        colors = colors,
        line = list(width = 2),
        marker = list(size = 6),
        hovertemplate = "<b>%{fullData.name}</b><br>Year: %{x}<br>Value: %{y:.2f}<extra></extra>"
      ) %>%
      layout(
        title = list(text = paste0("SPI Pillars: ", input$country_select)),
        xaxis = list(title = "Year"),
        yaxis = list(title = "SPI Pillar Value", range = c(0, 105)),
        hovermode = "x unified",
        legend = list(x = 0.02, y = 0.98),
        margin = list(l = 60, r = 20, b = 60, t = 80)
      )
  })
}

shinyApp(ui, server)


