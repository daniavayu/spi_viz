# Install once if needed:
# install.packages(c("shiny", "dplyr", "tidyr", "readr", "plotly", "scales"))

library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(scales)
library(spiR)


spi_index <- spi_index() 
#spi_data <- spi_data()

#write_csv(spi_index, "spi_index.csv") 

# 1) Load SPI data from local file
spi <- read_csv("data/spi_index.csv", show_col_types = FALSE)

# 2) Identify column categories
index_cols <- names(spi)[grepl("^SPI\\.INDEX$", names(spi))]
pillar_cols <- names(spi)[grepl("^SPI\\.INDEX\\.PIL", names(spi))]
dimension_cols <- names(spi)[grepl("^SPI\\.DIM.*\\.INDEX$", names(spi))]
indicator_cols <- names(spi)[grepl("^SPI\\.D", names(spi)) & !grepl("\\.INDEX$", names(spi))]

# 3) Prepare data for each category
base_cols <- c("iso3c", "country", "date")

spi_long <- bind_rows(
	# Main Index
	spi %>% select(all_of(base_cols), all_of(index_cols)) %>%
		pivot_longer(cols = all_of(index_cols), names_to = "metric", values_to = "value") %>%
		mutate(category = "Index"),
	
	# Pillars
	spi %>% select(all_of(base_cols), all_of(pillar_cols)) %>%
		pivot_longer(cols = all_of(pillar_cols), names_to = "metric", values_to = "value") %>%
		mutate(category = "Pillar"),
	
	# Dimensions
	spi %>% select(all_of(base_cols), all_of(dimension_cols)) %>%
		pivot_longer(cols = all_of(dimension_cols), names_to = "metric", values_to = "value") %>%
		mutate(category = "Dimension"),
	
	# Indicators
	spi %>% select(all_of(base_cols), all_of(indicator_cols)) %>%
		pivot_longer(cols = all_of(indicator_cols), names_to = "metric", values_to = "value") %>%
		mutate(category = "Indicator")
) %>%
	mutate(
		date = as.integer(date),
		value = as.numeric(value),
		iso3c = toupper(trimws(iso3c))
	) %>%
	filter(!is.na(iso3c), nchar(iso3c) == 3, !is.na(value))

ui <- fluidPage(
	titlePanel("SPI Metrics Interactive Map"),
	sidebarLayout(
		sidebarPanel(
			selectInput(
				inputId = "category",
				label = "Metric Category",
				choices = sort(unique(spi_long$category)),
				selected = "Index"
			),
			uiOutput("metric_selector"),
			sliderInput(
				inputId = "year",
				label = "Year",
				min = min(spi_long$date, na.rm = TRUE),
				max = max(spi_long$date, na.rm = TRUE),
				value = max(spi_long$date, na.rm = TRUE),
				step = 1,
				sep = ""
			),
			selectizeInput(
				inputId = "countries",
				label = "Countries (optional filter)",
				choices = sort(unique(na.omit(spi_long$country))),
				selected = NULL,
				multiple = TRUE
			)
		),
		mainPanel(
			plotlyOutput("spi_map", height = "760px")
		)
	)
)

server <- function(input, output, session) {

	# Dynamic metric selector based on category
	output$metric_selector <- renderUI({
		metrics <- spi_long %>%
			filter(category == input$category) %>%
			pull(metric) %>%
			unique() %>%
			sort()
		
		selectInput(
			inputId = "metric",
			label = "Select Metric",
			choices = metrics,
			selected = metrics[1]
		)
	})

	filtered <- reactive({
		df <- spi_long %>%
			filter(category == input$category, metric == input$metric, date == input$year)

		if (!is.null(input$countries) && length(input$countries) > 0) {
			df <- df %>% filter(country %in% input$countries)
		}

		df
	})

	output$spi_map <- renderPlotly({
		df <- filtered()

		if (nrow(df) == 0) {
			return(
				plot_ly() %>%
					layout(title = "No data for selected filters")
			)
		}

		plot_ly(
			data = df,
			type = "choropleth",
			locations = ~iso3c,
			locationmode = "ISO-3",
			z = ~value,
			text = ~paste0(
				"<b>Country:</b> ", country,
				"<br><b>ISO3:</b> ", iso3c,
				"<br><b>Metric:</b> ", metric,
				"<br><b>Year:</b> ", date,
				"<br><b>Value:</b> ", ifelse(is.na(value), "NA", number(value, accuracy = 0.01))
			),
			hoverinfo = "text",
			colorscale = "YlGnBu",
			marker = list(line = list(color = "white", width = 0.2)),
			colorbar = list(title = "Value")
		) %>%
			layout(
				title = list(text = paste0(input$metric, " | Year: ", input$year)),
				geo = list(
					projection = list(type = "natural earth"),
					showframe = FALSE,
					showcoastlines = TRUE
				),
				margin = list(l = 0, r = 0, b = 0, t = 60)
			)
	})
}

shinyApp(ui, server)






