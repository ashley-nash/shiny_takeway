library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(shinyjs)
library(DT)
library(leaflet)

#Read all data
db_sale <- read_csv("data.csv")
db_geo <- read_csv("GeoLabel.csv")
#Format date
db_sale$日期 <- as.Date(as.character(db_sale$日期),"%Y%m%d")
#Stitching longitude and latitude into a new table
db_sale=db_sale %>% left_join(db_geo , by=c("网格编号"="label"))
#Column name should be in English
colnames(db_sale) <- c('day','t_time','dat','zlnum','cfnum','ddnum','lng','lat')


mapPanel <- tabPanel("cfnumPanel",                     
					fluidRow(
					  column(width=4,selectInput("selecta", label = h3("Selection date"), choices = as.character(unique(db_sale$day)), selected = '2017-09-01')),
					  column(width=4,selectInput("selectb", label = h3("Select t_time"), choices = as.character(unique(db_sale$t_time)), selected = '0'))
					 ),
					h4('Number of departures per hour at each station'), 
					fluidRow(
					  #--map1
					  column(width=12,leafletOutput("cfnum"))
					 ),
					h4('Number of arrivals per hour at each station'),					 
					fluidRow(
					  #--map2
					  column(width=12,leafletOutput("ddnum"))
					 )					 
							  )
plot_ly <- tabPanel("plot_ly",                     
					fluidRow(
					  column(width=4,selectInput("selectc", label = h3("Selection date"), choices = as.character(unique(db_sale$day)), selected = '2017-09-01')),
					  column(width=4,selectInput("selectd", label = h3("Base station"), choices = as.character(unique(db_sale$dat)), selected = '13'))
					 ),
					h4('Daily traffic change of each station'), 
					fluidRow(
					  column(width=12,plotlyOutput(outputId = "ptnum"))
					 )					 
							  )

dataPanel <- tabPanel("dataPanel",                     
					fluidRow(
					  column(width=4,selectInput("selecte", label = h3("Selection date"), choices = as.character(unique(db_sale$day)), selected = '2017-09-01')),
					  column(width=4,selectInput("selectf", label = h3("Base station"), choices = as.character(unique(db_sale$dat)), selected = '13'))
					 ),
					h4('Download required data results according to conditions'), 
					fluidRow(
					  column(width=12,dataTableOutput('datanum'))
					 )					 
							  )

## UI side
ui <- navbarPage("Talk Way",theme = shinythemes::shinytheme("united"),
                 mapPanel     
				,plot_ly
				,dataPanel
)

## Server side
server <- function(input, output) {
# map
output$cfnum <- renderLeaflet({
	      tb=filter(db_sale ,day == input$selecta & t_time== input$selectb ) #tb=filter(db_sale ,day == '2017-09-01' & t_time=='0' )	  
		  leaflet(data = tb) %>% addTiles() %>%
		  addMarkers(~lng, ~lat, popup = ~as.character(cfnum), label = ~as.character(cfnum))  
  }) 
  

output$ddnum <- renderLeaflet({
	      tb=filter(db_sale ,day == input$selecta & t_time== input$selectb ) #tb=filter(db_sale ,day == '2017-09-01' & t_time=='0' )	  
		  leaflet(data = tb) %>% addTiles() %>%
		  addMarkers(~lng, ~lat, popup = ~as.character(ddnum), label = ~as.character(ddnum))  
  }) 
  
##Draw a trend chart (select a date to show 24-hour trend)
output$ptnum <- renderPlotly({
		tx= filter(db_sale ,day == input$selectc & dat== input$selectd )
		p <- plot_ly(data=tx, x = ~t_time) %>%
			add_lines(y = ~zlnum, name = "Resident number") %>%
			add_lines(y = ~cfnum, name = "Departure number") %>%
			add_lines(y = ~ddnum, name = "Master arrivals") %>%	
			layout(
			  title = "Number of base stations changes",
			  xaxis = list(title = "Hours"),
			  yaxis = list(title = "Population flow"))  
			  
		p
					 })

## Data table download pdf format

output$datanum <- DT::renderDataTable({
                    tx= filter(db_sale ,day == input$selecte & dat== input$selectf )
					datatable(
					  tx, extensions = 'Buttons', options = list(
						dom = 'Bfrtip',
						buttons = c('pdf', 'print')
					  )
					)									
					 })

}

# Run the application 
shinyApp(ui = ui, server = server)