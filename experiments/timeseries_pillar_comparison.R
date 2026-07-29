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
pillar_names <- c("SPI.INDEX.PIL1" = "PIL1", 
                  "SPI.INDEX.PIL2" = "PIL2", 
                  "SPI.INDEX.PIL3" = "PIL3", 
                  "SPI.INDEX.PIL4" = "PIL4",
                  "SPI.INDEX.PIL5" = "PIL5")

spi_long <- spi %>%
  select(country, iso3c, date, region, all_of(pillar_cols)) %>%
  pivot_longer(
    cols = all_of(pillar_cols),
    names_to = "pillar",
    values_to = "value"
  ) %>%
  mutate(
    date = as.integer(date),
    value = as.numeric(value)
  ) %>%
  filter(!is.na(value))

ui <- fluidPage(
  titlePanel("SPI Pillar Comparison - Multiple Countries Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "pillar_select",
        label = "Select Pillar",
        choices = setNames(pillar_cols, pillar_names),
        selected = "SPI.INDEX.PIL1"
      ),
      h4("Filter by Region (optional)"),
      selectInput(
        inputId = "region_select",
        label = "Select Region",
        choices = c("All", sort(unique(spi_long$region))),
        selected = "All"
      ),
      h4("Select Countries to Compare"),
      uiOutput("country_selector"),
      p("Tip: Select multiple countries to compare their trajectories")
    ),
    mainPanel(
      plotlyOutput("comparison_plot", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamic country selector based on region
  output$country_selector <- renderUI({
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
      inputId = "countries_compare",
      label = "Countries",
      choices = countries,
      selected = head(countries, 1),
      multiple = TRUE,
      options = list(maxItems = 20, maxOptions = 300)
    )
  })
  
  filtered <- reactive({
    df <- spi_long %>%
      filter(pillar == input$pillar_select) %>%
      arrange(date)
    
    # Apply region filter if selected
    if (input$region_select != "All") {
      df <- df %>% filter(region == input$region_select)
    }
    
    # Filter by selected countries
    if (!is.null(input$countries_compare) && length(input$countries_compare) > 0) {
      df <- df %>% filter(country %in% input$countries_compare)
    }
    
    df
  })
  
  output$comparison_plot <- renderPlotly({
    df <- filtered()
    
    if (nrow(df) == 0) {
      return(
        plot_ly() %>%
          layout(title = "No data available for selected filters")
      )
    }
    
    # Get pillar name for title
    pillar_name <- names(pillar_names)[pillar_names == input$pillar_select] %>%
      unname()
    pillar_display <- pillar_names[input$pillar_select]
    
    plot_ly(data = df, type = "scatter", mode = "lines+markers") %>%
      add_trace(
        x = ~date,
        y = ~value,
        color = ~country,
        line = list(width = 2),
        marker = list(size = 6),
        hovertemplate = "<b>%{fullData.name}</b><br>Year: %{x}<br>Value: %{y:.2f}<extra></extra>"
      ) %>%
      layout(
        title = list(text = paste0(pillar_display, " - Country Comparison")),
        xaxis = list(title = "Year"),
        yaxis = list(title = "SPI Pillar Value", range = c(0, 105)),
        hovermode = "x unified",
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 1,
          xanchor = "left",
          yanchor = "top"
        ),
        margin = list(l = 60, r = 200, b = 60, t = 80)
      )
  })
}

shinyApp(ui, server)
