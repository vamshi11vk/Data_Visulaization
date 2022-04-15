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
    # Application title
    titlePanel("Stock Markets during the great recession"),
    h4("(Please wait while the gif renders)"),

        # Show a plot of the generated distribution
        mainPanel(
        	fluidRow(
        		column(width = 6,height=3,
        			   wellPanel(dygraphOutput("plot2"),style="background:white")),
        		column(width = 6,height=3,
        			   wellPanel(plotOutput("distPlot"),style="background:black")),
        		column(width = 12,height=4,
        						wellPanel(h4("S&P shares plummeted down by 38.5% in 2008 alone and after suffering huge losses, Lehman brothers declared bankruptcy on Sep 15 2008"))),

        	),

        	#img(src="Banking.gif", align = "left",height='250px',width='900px'),
        	column(width = 12,height=4,
        		   wellPanel(h4("Tycoons like Bank of America, JP Morgan, Morgan Stanley and Wells Fargo suffered huge losses")))

        ),
	imageOutput("gif1")
)

# Define server logic required to draw a histogram
library(dygraphs)
library(datasets)
server <- function(input, output) {

    output$distPlot <- renderPlot({

    	ggplot(data_2,aes(x=Date, y=High)) +
    		geom_bar(stat="identity") +
    		ggtitle("Lehman Brothers Stock Price") +
    		theme_ipsum() +
    		xlab("Year")
    	})
    output$plot2 <- renderDygraph({
    	data_1 <- read_excel("../Dataviz assignment datasets/Sheet1 (S&P_Stock_Price)_Sheet1.xlsx")
    	data_1$Date <- as.Date(data_1$Date)
    	col <- xts(x = data_1$High, order.by = data_1$Date)
    	p<-dygraph(col,main = "S&P Stock Prices",ylab="Average High") %>%
    		dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
    		dyRangeSelector() %>%
    		dyCrosshair(direction = "vertical") %>%
    		dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
    		dyRoller(rollPeriod = 1)
		p
    })
    output$gif1 <- renderImage({
    	myAnimation<-
    		ggplot(data_5, aes(Date, Values, size = Values, colour = BankNames)) +
    		geom_point(alpha = 0.7, show.legend = FALSE) +
    		#scale_colour_manual(values = country_colors) +
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
}

# Run the application
shinyApp(ui = ui, server = server)
