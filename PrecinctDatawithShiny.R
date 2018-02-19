#pull in datasets
library(readr)
library(leaflet)
library(magrittr)
library(shiny)
library(dplyr)

##This can mostly be generalized for any set of political data. 
##The difficulty lies in finding longitude/latitude coordinates and keying by hand.
District25_NoPaperBallot <- read_csv("district25.csv")

#shiny implementation

#colorlist for counties
colbycty <- colorFactor(topo.colors(5), District25_NoPaperBallot$COUNTY)

inline = function (x) {
  tags$div(style="display:inline-block; vertical-align:top; width: 100px;", x)
}
#ui page
ui <- fluidPage(
  tags$style(type = "text/css", "html, body{width:100%;height:100%}"),
  
  sidebarPanel(
    ###DIFFERENCE
    h5("Please don't clear these values--highlight them and type a new one. No value will cause the app to quit."),
    h5("Republican - Democratic Votes"),
    inline(numericInput("diff_lb","Lower",value=min(District25_NoPaperBallot$Difference))),
    inline(numericInput("diff_ub","Upper",value=max(District25_NoPaperBallot$Difference))),
    
    ###TOTAL VOTES 14
    h5("Total Votes - 2014"),
    inline(numericInput("t14_lb","Lower",value=min(District25_NoPaperBallot$TotalVotes))),
    inline(numericInput("t14_ub","Upper",value=max(District25_NoPaperBallot$TotalVotes))),
    
    ###REG VOTERS - 16
    h5("Registered Voters - 2016"),
    inline(numericInput("r16_lb","Lower",value=min(District25_NoPaperBallot$RegVoters))),
    inline(numericInput("r16_ub","Upper",value=max(District25_NoPaperBallot$RegVoters))),
    
    ### % DEMS
    h5("Average % Democrat"),
    inline(numericInput("pd_lb","Lower",value=round(100 * min(District25_NoPaperBallot$AvgDem), 2))),
    inline(numericInput("pd_ub","Upper",value=round(100 * max(District25_NoPaperBallot$AvgDem), 2)))
  ),
  mainPanel(
    tabsetPanel(type="tabs",
      tabPanel("Map",
        h5("Hovering over each point shows the precinct, clicking shows the county."),
        leafletOutput("map")
      ),
      tabPanel("Analysis",
        includeHTML("wmunday-shinymd.html")
        #includeMarkdown("wmunday-shinymd.Rmd")
      )
    )
  )
)
#create server
server <- function(input, output, session) {
  subsetData1 <- reactive({
    District25_NoPaperBallot %>% 
      #multiple filters to best update map
      filter(District25_NoPaperBallot$Difference >= input$diff_lb &
               District25_NoPaperBallot$Difference <= input$diff_ub &
               District25_NoPaperBallot$TotalVotes >= input$t14_lb &
               District25_NoPaperBallot$TotalVotes <= input$t14_ub &
               District25_NoPaperBallot$RegVoters >= input$r16_lb &
               District25_NoPaperBallot$RegVoters <= input$r16_ub &
               round(100 * District25_NoPaperBallot$AvgDem, 2) >= input$pd_lb &
               round(100 * District25_NoPaperBallot$AvgDem, 2) <= input$pd_ub
      )
  })
  #initial map
  output$map <- renderLeaflet({
    leaflet(District25_NoPaperBallot) %>%
      setView(lng =-87, lat=36.1490 , zoom = 8) %>%
      addTiles() %>%
      addCircleMarkers(~ District25_NoPaperBallot$Longitude, ~District25_NoPaperBallot$Latitude, 
                       popup = as.character(District25_NoPaperBallot$COUNTY),
                       label = ~as.character(District25_NoPaperBallot$PRECINCT),
                       radius = 5,
                       color = ~colbycty(COUNTY)) %>%
      addLegend("topright", pal = colbycty, values=~COUNTY,
                title = "Counties")
  })
  observe({
    #leafletProxy to update with subsetData
    leafletProxy("map", data = subsetData1()) %>%
      
      clearMarkers() %>%
      clearShapes() %>%
      clearPopups() %>%
      
      addCircleMarkers( lat = ~Latitude,
                        lng = ~Longitude,
                        popup = ~COUNTY,
                        label = ~PRECINCT,
                        radius = 5,
                        color = ~colbycty(COUNTY))
  })
}

#run Shiny app
shinyApp(ui = ui, server = server)