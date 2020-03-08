library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(shinyjs)
library(DT)
library(leaflet)

#读取数据
db_sale <- read_csv("data.csv")
db_geo <- read_csv("GeoLabel.csv")
#格式化日期
db_sale$日期 <- as.Date(as.character(db_sale$日期),"%Y%m%d")
#拼接经纬度成新表
db_sale=db_sale %>% left_join(db_geo , by=c("网格编号"="label"))

# 列名称要格式成英文
colnames(db_sale) <- c('day','t_time','dat','zlnum','cfnum','ddnum','lng','lat')




mapPanel <- tabPanel("cfnumPanel",                     
					fluidRow(
					  #--选择日期按钮
					  column(width=4,selectInput("selecta", label = h3("日期选择"), choices = as.character(unique(db_sale$day)), selected = '2017-09-01')),
					  #--选择时间点按钮
					  column(width=4,selectInput("selectb", label = h3("时刻选择"), choices = as.character(unique(db_sale$t_time)), selected = '0'))
					 ),
					h4('各站点每小时的出发流量'), 
					fluidRow(
					  #--地图1
					  column(width=12,leafletOutput("cfnum"))
					 ),
					h4('各站点每小时的到达流量'),					 
					fluidRow(
					  #--地图2
					  column(width=12,leafletOutput("ddnum"))
					 )					 
							  )
plot_ly <- tabPanel("plot_ly",                     
					fluidRow(
					  #--选择日期按钮
					  column(width=4,selectInput("selectc", label = h3("日期选择"), choices = as.character(unique(db_sale$day)), selected = '2017-09-01')),
					  #--选择站点按钮
					  column(width=4,selectInput("selectd", label = h3("基站选择"), choices = as.character(unique(db_sale$dat)), selected = '13'))
					 ),
					h4('各站点每天流量变化'), 
					fluidRow(
					  column(width=12,plotlyOutput(outputId = "ptnum"))
					 )					 
							  )

dataPanel <- tabPanel("dataPanel",                     
					fluidRow(
					  #--选择日期按钮
					  column(width=4,selectInput("selecte", label = h3("日期选择"), choices = as.character(unique(db_sale$day)), selected = '2017-09-01')),
					  #--选择站点按钮
					  column(width=4,selectInput("selectf", label = h3("基站选择"), choices = as.character(unique(db_sale$dat)), selected = '13'))
					 ),
					h4('根据条件下载需要的数据结果'), 
					fluidRow(
					  column(width=12,dataTableOutput('datanum'))
					 )					 
							  )

## UI端
ui <- navbarPage("Talk Way",theme = shinythemes::shinytheme("united"),
                 mapPanel    #地图     -- 筛选框选择日期 和 时间点
				,plot_ly
				,dataPanel
)

## 服务器端
server <- function(input, output) {
# 地图
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
  
##画走势图(选择日期，显示24小时走势)
output$ptnum <- renderPlotly({
		tx= filter(db_sale ,day == input$selectc & dat== input$selectd )
		p <- plot_ly(data=tx, x = ~t_time) %>%
			add_lines(y = ~zlnum, name = "驻留人数") %>%
			add_lines(y = ~cfnum, name = "出发人数") %>%
			add_lines(y = ~ddnum, name = "到达人数") %>%	
			layout(
			  title = "各网格人数变化",
			  xaxis = list(title = "小时数"),
			  yaxis = list(title = "人口流量数"))  
			  
		p
					 })

## 数据表下载 pdf格式

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