library(shiny)
library(tidyverse)
library(plotly)
library(shinyjs)

db_sale <- read_csv("data.csv")
db_geo <- read_csv("GeoLabel.csv")

gapminder %<>% mutate_at(c("year", "country"), as.factor)

gapminder_years <- levels(gapminder$year)
gapminder_countries <- levels(gapminder$country) %>% str_sort()

headerRow <- div(id="header", useShinyjs(),
                selectInput("selYear", 
                         label="Select the Year", 
                         multiple = TRUE,
                         choices=gapminder_years,
                         selected=head(gapminder_years,3)),
                selectInput("selCountry", 
                            label="Select the Coutry", 
                            multiple = TRUE,
                            choices=gapminder_countries,
                            selected=head(gapminder_countries,2))
)

dataPanel <- tabPanel("Data",
                      tableOutput("dataTable")
             )

plotPanel <- tabPanel("Plot",
                      fluidRow(
                        column(width = 8,
                               plotOutput("plotData",
                                          hover = hoverOpts(id = "plot_hover", delayType = "throttle"))
                        ),
                        column(width = 4,
                               h2("Info"),
                               div("Country: ",
                                   textOutput("txtCountry", inline = TRUE)
                               ),
                               div("Year: ",
                                   textOutput("txtYear", inline = TRUE)
                               ),
                               div("Population: ",
                                   textOutput("txtPop", inline = TRUE)
                               )
                               # verbatimTextOutput("plot_hoverinfo")
                        )
                      ) # fluidRow
          ) # tabPanel

plotlyPanel <- tabPanel("Plotly",
                        plotly::plotlyOutput("plotlyData")
            )

mapPanel <- tabPanel("Map",
                     tableOutput("mapTable")
)

ui <- navbarPage("take-away",
                 dataPanel,
                 plotPanel,
                 plotlyPanel,
                 mapPanel,
                 id = "navBar",
                 header = headerRow
)

server <- function(input, output) {
  
  observe(if(input$navBar=="Map") {
    cat(file=stderr(), input$navBar, "\n")
    shinyjs::hide("header")
  } else {
    cat(file=stderr(), input$navBar, "\n")
    shinyjs::show("header")
  })

  gapminder_filtered <- reactive({
    req(input$selCountry)
    req(input$selYear)
    gapminder %>% filter(year %in% input$selYear, 
                         country %in% input$selCountry)
    })
  
  output$dataTable <- renderTable({
    gapminder_filtered()
  })
  
  output$mapTable <- renderTable({
    head(esDB)
  })
  
  output$plotData <- renderPlot({
    gapminder_filtered() %>%
      ggplot(aes(x=country, y=pop, fill=year)) +
      geom_bar(stat="identity", position=position_dodge())
  })
  
  output$plotlyData <- plotly::renderPlotly({
    gapminder_filtered() %>%
      ggplot(aes(x=country, y=pop, fill=year)) +
      geom_bar(stat="identity", position=position_dodge())
  })
  
#  output$plot_hoverinfo <- renderPrint({
#    cat("Hover (throttled):\n")
#    str(input$plot_hover)
#  })
  
  countryIndex <- reactive({
    req(input$plot_hover$x)
    round(input$plot_hover$x)
    })
  countryName <- reactive({
    req(countryIndex() > 0 & countryIndex() <= length(input$selCountry))
    str_sort(input$selCountry)[countryIndex()]
    })
  output$txtCountry <- renderText(countryName())
  yearIndex <- reactive({
    req(input$plot_hover$x)
    ceiling((input$plot_hover$x - round(input$plot_hover$x) + 0.5) * length(input$selYear))
  })
  yearName <- reactive({
    req(yearIndex() > 0 & yearIndex() <= length(input$selYear))
    str_sort(input$selYear)[yearIndex()]
  })
  output$txtYear <- renderText(yearName())
  output$txtPop <- renderText(
    gapminder_filtered() %>% filter(year == yearName(), country == countryName()) %>% pull(pop)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)