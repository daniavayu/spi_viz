library(shiny)
library(plotly)
library(dplyr)
library(tidyr)

# Busca data/ desde el wd actual o la carpeta de arriba (raíz o experiments/)
find_data <- function(rel = "data/spi_index.csv") {
  for (p in c(rel, file.path("..", rel))) if (file.exists(p)) return(p)
  stop("No encuentro ", rel, ". Abre SPI_viz.Rproj o pon el wd en 'spi_viz/'.")
}

# Load data
spi_index <- read.csv(find_data('data/spi_index.csv'))

# Transform to long format
spi_long <- spi_index %>%
  pivot_longer(
    cols = starts_with('SPI.INDEX.PIL'),
    names_to = 'pillar',
    values_to = 'value'
  ) %>%
  mutate(pillar_name = case_when(
    pillar == 'SPI.INDEX.PIL1' ~ 'PIL1: Data Use',
    pillar == 'SPI.INDEX.PIL2' ~ 'PIL2: Data Services',
    pillar == 'SPI.INDEX.PIL3' ~ 'PIL3: Data Products',
    pillar == 'SPI.INDEX.PIL4' ~ 'PIL4: Data Sources',
    pillar == 'SPI.INDEX.PIL5' ~ 'PIL5: Data Infrastructure',
    TRUE ~ pillar
  ))

# Get list of regions and countries
regions <- sort(unique(spi_index$region))
countries <- sort(unique(spi_index$country))

# Define UI
ui <- fluidPage(
  titlePanel("Country vs Region Comparison - Performance Benchmark"),
  
  sidebarLayout(
    sidebarPanel(
      # Country selector
      selectInput(
        'country_select',
        'Select Country:',
        choices = countries,
        selected = countries[1]
      ),
      
      # Region selector (dynamic based on country)
      uiOutput('region_selector'),
      
      hr(),
      
      # Pillar selector
      selectInput(
        'pillar_select',
        'Select Pillar:',
        choices = c(
          'PIL1: Data Use' = 'SPI.INDEX.PIL1',
          'PIL2: Data Services' = 'SPI.INDEX.PIL2',
          'PIL3: Data Products' = 'SPI.INDEX.PIL3',
          'PIL4: Data Sources' = 'SPI.INDEX.PIL4',
          'PIL5: Data Infrastructure' = 'SPI.INDEX.PIL5'
        ),
        selected = 'SPI.INDEX.PIL1'
      ),
      
      hr(),
      
      # Info panel
      htmlOutput('info_panel')
    ),
    
    mainPanel(
      plotlyOutput('country_vs_region_plot', height = '700px'),
      
      hr(),
      
      # Comparison table
      h4("Latest Year Comparison"),
      tableOutput('comparison_table')
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Update region selector based on selected country
  output$region_selector <- renderUI({
    if (is.null(input$country_select)) return(NULL)
    
    tryCatch({
      region_for_country <- unique(
        spi_index[spi_index$country == input$country_select, 'region']
      )
      
      if (is.na(region_for_country) || length(region_for_country) == 0) {
        return(selectInput('region_select', 'Region:', choices = "", selected = ""))
      }
      
      selectInput(
        'region_select',
        'Region (Auto):',
        choices = region_for_country,
        selected = region_for_country[1]
      )
    }, error = function(e) {
      selectInput('region_select', 'Region:', choices = "", selected = "")
    })
  })
  
  # Information panel
  output$info_panel <- renderUI({
    tryCatch({
      if (is.null(input$country_select)) return(NULL)
      
      country <- input$country_select
      
      # Get region for selected country
      region_data <- spi_index[spi_index$country == country, ]
      if (nrow(region_data) == 0) return(NULL)
      
      region <- region_data$region[1]
      iso3 <- region_data$iso3[1]
      
      HTML(paste(
        "<b>Country:</b>", country, "<br>",
        "<b>Region:</b>", region, "<br>",
        "<b>ISO3:</b>", ifelse(is.na(iso3), "N/A", iso3), "<br><br>",
        "<small>",
        "<i>Solid line = country</i><br>",
        "<i>Dashed line = regional average</i>",
        "</small>"
      ))
    }, error = function(e) {
      HTML("<p style='color: gray;'>Loading information...</p>")
    })
  })
  
  # Main plot
  output$country_vs_region_plot <- renderPlotly({
    if (is.null(input$country_select)) return(NULL)
    
    country <- input$country_select
    region <- input$region_select
    pillar <- input$pillar_select
    
    # Get pillar name
    pillar_names_map <- c(
      'SPI.INDEX.PIL1' = 'PIL1: Data Use',
      'SPI.INDEX.PIL2' = 'PIL2: Data Services',
      'SPI.INDEX.PIL3' = 'PIL3: Data Products',
      'SPI.INDEX.PIL4' = 'PIL4: Data Sources',
      'SPI.INDEX.PIL5' = 'PIL5: Data Infrastructure'
    )
    
    pillar_label <- pillar_names_map[pillar]
    pillar_color <- '#3498db'
    
    # Country data for selected pillar
    country_data <- spi_long %>%
      filter(country == !!country, pillar == !!pillar) %>%
      arrange(date)
    
    # Region data for selected pillar
    region_data <- spi_long %>%
      filter(region == !!region, pillar == !!pillar) %>%
      group_by(date) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
      arrange(date)
    
    # Create plot
    p <- plot_ly()
    
    # Solid line for country
    p <- p %>%
      add_trace(
        data = country_data,
        x = ~date,
        y = ~value,
        name = paste(pillar_label, "(Country)"),
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = pillar_color, width = 3),
        marker = list(size = 8),
        hovertemplate = '<b>%{fullData.name}</b><br>Year: %{x}<br>Score: %{y:.1f}<extra></extra>'
      )
    
    # Dashed line for region (benchmark)
    p <- p %>%
      add_trace(
        data = region_data,
        x = ~date,
        y = ~value,
        name = paste(pillar_label, "(Region)"),
        type = 'scatter',
        mode = 'lines+markers',
        line = list(
          color = pillar_color,
          width = 2,
          dash = 'dash'
        ),
        marker = list(size = 6),
        hovertemplate = '<b>%{fullData.name}</b><br>Year: %{x}<br>Score: %{y:.1f}<extra></extra>'
      )
    
    # Layout
    p <- p %>%
      layout(
        title = list(
          text = paste(
            pillar_label,
            '<br><sub style="font-size:12px">', country, ' vs Regional Average of', region,
            '</sub>'
          ),
          x = 0.5,
          xanchor = 'center'
        ),
        xaxis = list(title = 'Year', gridcolor = '#e5e5e5'),
        yaxis = list(
          title = 'Score',
          range = c(0, 105),
          gridcolor = '#e5e5e5'
        ),
        hovermode = 'x unified',
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = 'white',
        font = list(size = 12),
        legend = list(
          orientation = 'v',
          x = 1.02,
          y = 1,
          bgcolor = 'rgba(255, 255, 255, 0.8)',
          bordercolor = '#e5e5e5',
          borderwidth = 1
        ),
        margin = list(r = 200)
      )
    
    p
  })
  
  # Comparison table
  output$comparison_table <- renderTable({
    if (is.null(input$country_select)) return(NULL)
    
    country <- input$country_select
    region <- input$region_select
    pillar <- input$pillar_select
    
    # Latest year
    latest_year <- max(spi_long$date, na.rm = TRUE)
    
    # Get pillar name
    pillar_names_map <- c(
      'SPI.INDEX.PIL1' = 'PIL1: Data Use',
      'SPI.INDEX.PIL2' = 'PIL2: Data Services',
      'SPI.INDEX.PIL3' = 'PIL3: Data Products',
      'SPI.INDEX.PIL4' = 'PIL4: Data Sources',
      'SPI.INDEX.PIL5' = 'PIL5: Data Infrastructure'
    )
    
    # Country data
    country_val <- spi_long %>%
      filter(country == !!country, date == !!latest_year, pillar == !!pillar) %>%
      pull(value)
    
    # Region data
    region_val <- spi_long %>%
      filter(region == !!region, date == !!latest_year, pillar == !!pillar) %>%
      summarise(value = mean(value, na.rm = TRUE)) %>%
      pull(value)
    
    # Calculate differences
    abs_diff <- country_val - region_val
    pct_diff <- ifelse(region_val != 0, (abs_diff / region_val) * 100, 0)
    
    # Table
    data.frame(
      Concept = c(country, region, 'Difference (Absolute)', 'Difference (%)', 'Status'),
      Score = c(
        round(country_val, 1),
        round(region_val, 1),
        round(abs_diff, 1),
        round(pct_diff, 1),
        ifelse(country_val > region_val, '✓ Above', '✗ Below')
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# Run app
shinyApp(ui = ui, server = server)
