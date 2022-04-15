#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
	tags$style('.container-fluid {
                             background-color: #D6EAF8;
              }'),

	# Application title
	titlePanel("Unemployment during the great recession"),

	# Show a plot of the generated distribution
	mainPanel(
		fluidRow(
			column(width = 6,height=3,
				   wellPanel(plotOutput("distPlot"),style="background:white")),
			column(width = 6,height=3,
				   wellPanel(htmlOutput("plot2"),style="background:white")),
			column(width = 12,height=4,
				   wellPanel(h4("Total Unemployment rose drastically and Median house hold income saw a decrease"))),
			column(width = 12,height=3,
				   wellPanel(dygraphOutput("plot3"),style="background:white")),
			column(width = 12,height=4,
				   wellPanel(h4("Total job openings across the US saw a downturn"))),

		),
	)
)

# Define server logic required to draw a histogram
library(dygraphs)
library(datasets)
server <- function(input, output) {

	output$distPlot <- renderPlot({
		data_11<-read_excel("../Dataviz assignment datasets/unemp1.xlsx")
		data_11$Year<-dmy(data_11$Year)
		data_11
		library(viridis)
		data_11 %>%
			ggplot( aes(x=Year, y=`Observation Value`)) +
			geom_area() +
			scale_fill_viridis(discrete = FALSE) +
			theme(legend.position="none") +
			ggtitle("Total Unemployment rate of labour") +
			theme_ipsum() +
			theme(
				legend.position="none",
				panel.spacing = unit(0.1, "lines"),
				strip.text.x = element_text(size = 8)
			)
		})

	output$plot2 <- renderUI({
		data_12<-read_excel("../Dataviz assignment datasets/median_household_income.xlsx")
		data_12$Date<-dmy(data_12$Date)
		library(plotly)
		library(dplyr)
		library(readr)

		map_data = data_12

		map_data_graph <- plot_geo(map_data,
								   locationmode = 'USA-states',
								   frame = ~year(Date)) %>%
			add_trace(locations = ~State,
					  z = ~Income,
					  zmin = 0,
					  zmax = 100000,
					  color = ~Income,
					  colorscale = 'Electric') %>%
			layout(geo = list(scope = 'usa'),
				   title = "Median Household Income over the years")
		map_data_graph
		#export as html file
		#htmlwidgets::saveWidget(map_data_graph, file = "household_income_map.html")
	})
	output$plot3 <- renderDygraph({
		data_13<-read_excel("../Dataviz assignment datasets/Total_Job_Openings.xlsx")
		data_13$Year<-dmy(data_13$Year)
		col_2 <- xts(y = data_13$`Job Openings`, order.by = data_13$Year)
		p <- dygraph(col,main = "Total Job Openings across US - normalized",ylab="Job Openings") %>%
			dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#4C43A1") %>%
			dyRangeSelector() %>%
			dyCrosshair(direction = "vertical") %>%
			dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.8, hideOnMouseOut = FALSE)  %>%
			dyRoller(rollPeriod = 1)
		p
	})
}
# Run the application
shinyApp(ui = ui, server = server)
