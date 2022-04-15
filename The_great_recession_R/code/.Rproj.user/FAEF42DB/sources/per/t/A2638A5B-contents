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
	titlePanel("Housing market during the great recession"),

	# Show a plot of the generated distribution
	mainPanel(
		fluidRow(
			column(width = 6,height=3,
				   wellPanel(plotOutput("distPlot"),style="background:white")),
			column(width = 6,height=3,
				   wellPanel(plotOutput("plot2"),style="background:white")),
			column(width = 12,height=4,
				   wellPanel(h4("In the first graph we can see that the Home price index came down and the mortgage value Increased. The Second graph shows us the declining ownership across the states California, Florida, Massachusetts, Michigan and North Carolina"))),
			column(width = 10,height=4,
				   wellPanel(htmlOutput("hmi"),style="background:white")),
			column(width = 12,height=4,
				   wellPanel(h4("Further investigating on the home value, the above map depicts the behavior of home values across the US during the recession"))),

		),
		#imageOutput("gif1")
		#img(src="Banking.gif", align = "left",height='250px',width='900px'),


	)
)

# Define server logic required to draw a histogram
library(dygraphs)
library(datasets)
server <- function(input, output) {

	output$distPlot <- renderPlot({

		ggp <- ggplot(data_4)  +
			geom_bar(aes(x=`Observation Date`, y=`Composite Home Price Index`),stat="identity", fill="cyan",colour="#006000")+
			geom_line(aes(x=`Observation Date`, y=`Median Sale Price`),stat="identity",color="red",size=2)+
			labs(title= "Composite Home Price Index and Mortgage value",
				 x="Year",y="Number of Courses Sold")+
			scale_y_continuous(sec.axis=sec_axis(~.))
		ggp
	})

	output$plot2 <- renderPlot({
		data_6<-read_excel("../Dataviz assignment datasets/Homeownership_Rate_by_State_Percent.xlsx")
		data_6$Year<-as.Date(data_6$Year)
		library(gcookbook)
			ggplot(data_6, aes(x = Year, y = `Home ownsership Percentage`, colour = `Region Name`)) +
			geom_line()+
			ggtitle("Home ownership rate across 5 states")
	})
	output$gif1 <- renderImage({
		myAnimation<-
			ggplot(data_5, aes(Date, Values, size = Values, colour = BankNames)) +
			geom_point(alpha = 0.7, show.legend = FALSE) +
			scale_colour_manual(values = country_colors) +
			scale_size(range = c(2, 12)) +
			facet_wrap(~BankNames) +

			labs(title = 'Year: {frame_time}', x = 'Date', y = 'Values') +
			shadow_mark(alpha=1,size=0.5)+
			transition_time(Date) +
			ease_aes('linear')


		animate(myAnimation, duration = 5, fps = 20, width = 800, height = 800, renderer = gifski_renderer())
		anim_save("Banking.gif")
		list(src="Banking.gif",contentType="image/gif")
	},
	deleteFile = TRUE
	)
	output$hmi <- renderUI({
		library(plotly)
		library(dplyr)
		library(readr)

		map_data = data_7

		map_data_graph <- plot_geo(map_data,
								   locationmode = 'USA-states',
								   frame = ~year(Year)) %>%
			add_trace(locations = ~State,
					  z = ~value,
					  zmin = 0,
					  zmax = 600000,
					  color = ~value,
					  colorscale = 'Electric') %>%
			layout(geo = list(scope = 'usa'),
				   title = "Home value Index through the years 2005-2014")

		map_data_graph
		#export as html file
		#htmlwidgets::saveWidget(map_data_graph, file = "home_value_map.html")
	})
}
# Run the application
shinyApp(ui = ui, server = server)
