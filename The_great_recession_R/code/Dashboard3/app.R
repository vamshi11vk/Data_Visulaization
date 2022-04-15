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
	titlePanel("Financial Sector during the great recession"),

	# Show a plot of the generated distribution
	mainPanel(
		fluidRow(
			column(width = 6,height=3,
				   wellPanel(plotOutput("distPlot"),style="background:white")),
			column(width = 6,height=3,
				   wellPanel(plotOutput("plot2"),style="background:white")),
			column(width = 12,height=4,
				   wellPanel(h4("Loan Losses sky rocketed throughout the nation, Federal funds almost dropped down to zero!"))),
			column(width = 12,height=3,
				   wellPanel(dygraphOutput("plot3"),style="background:white")),
			column(width = 12,height=4,
				   wellPanel(h4("Total Currency in circulation saw a never before high"))),

		),
	)
)

# Define server logic required to draw a histogram
library(dygraphs)
library(datasets)
server <- function(input, output) {

	output$distPlot <- renderPlot({
		data_8<-read_excel("../Dataviz assignment datasets/FEDFUNDS.xlsx")
		data_8$Date<-mdy(data_8$Date)
		data <- data_8
		barplot(main="Federal funds", height=data$Fedfunds, names=data$Date , density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="black"   )
	})

	output$plot2 <- renderPlot({
		data_9<-read_excel("../Dataviz assignment datasets/Net_Losses_Overall.xlsx")
		data_9$Date<-mdy(data_9$Date)
		data_9
		library(viridis)
		data_9 %>%
			ggplot( aes(x=Date, y=NetLoss, group=State, fill=State)) +
			geom_area() +
			scale_fill_viridis(discrete = TRUE) +
			theme(legend.position="none") +
			ggtitle("Net Loan Losses") +
			theme_ipsum() +
			theme(
				legend.position="none",
				panel.spacing = unit(0.1, "lines"),
				strip.text.x = element_text(size = 8)
			) +
			facet_wrap(~State, scale="free_y")
	})
	output$plot3 <- renderDygraph({
		data_10<-read_excel("../Dataviz assignment datasets/Currency in circulation.xlsx")
		data_10$Date<-mdy(data_10$Date)
		col_1<- xts(x = data_10$Currency, order.by = data_10$Date)
		p <- dygraph(col,main = "Currency Circulation in dollars",ylab="USD") %>%
			dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#000000") %>%
			dyRangeSelector() %>%
			dyCrosshair(direction = "vertical") %>%
			dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 1, hideOnMouseOut = FALSE)  %>%
			dyRoller(rollPeriod = 1)
		p
	})
}
# Run the application
shinyApp(ui = ui, server = server)
