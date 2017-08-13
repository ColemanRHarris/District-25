#pull in datasets
library(readxl)
library(leaflet)
library(magrittr)
library(shiny)
library(dplyr)

Precinct_List_Raw <- read_excel("Precinct List_Raw.xlsx")
TN_SD25_DPI <- read_excel("TN_SD25_DPI.xlsx")

#Create TotalVotes
Precinct_List_Raw[c("TotalVotes")] <- NA
Precinct_List_Raw$TotalVotes <- Precinct_List_Raw$PVTALLY1 + Precinct_List_Raw$PVTALLY2

#Pick useful columns and drop the rest
keep <- c("COUNTY", "PRCTSEQ", "PRECINCT", "PVTALLY1", "PVTALLY2", "Difference", "TotalVotes")
District25 <- Precinct_List_Raw[,keep,drop = FALSE]
District25_NoPaperBallot <- District25[-c(66:66),]

#add in data from TN_SD25_DPI
District25_NoPaperBallot[c("AvgDem")] <- TN_SD25_DPI$`AVG DEM`
District25_NoPaperBallot[c("AvgStateDem")] <- TN_SD25_DPI$`AVG STATE DEM`
District25_NoPaperBallot[c("RegVoters")] <- TN_SD25_DPI$`REGISTERED VOTER COUNT`
District25_NoPaperBallot[c("2016Expect")] <- TN_SD25_DPI$`2016 EXPECTED VOTE`
District25_NoPaperBallot[c("2016Expect%")] <- TN_SD25_DPI$`2016 EXPECTED VOTE %`

#key in each longitude and latitude for each precinct
Longitude <- c(-87.052233, -86.960140, -87.020061,
               -87.166610, -87.048989, -87.1351204,
               -87.042969, -87.094263, -87.37539, 
               -87.4301357, -87.4382766, -87.5195883,
               -87.4818407, -87.4460456, -87.2340729,
               -87.2749529, -87.3128148, -87.2833271,
               -87.3503384, -87.3943309, -87.3840187, 
               -87.3378917, -87.3564782, -87.2113892,
               -87.2324469, -87.4859976, -87.4751573,
               -87.4293047, -87.3215497, -87.3127987,
               -87.275385, -87.2620258, -87.4337671,
               -87.2792923, -87.4671762, -87.4859976,
               -87.600134, -87.5056047, -87.6378325,
               -87.9691873, -87.8096727, -87.7780637,
               -87.7942147, -87.9656927, -87.7356197,
               -87.6546323, -87.647143, -86.689389,
               -86.673303, -86.805105, -86.703421,
               -86.764416, -86.804914, -86.967121,
               -86.895555, -86.986416, -87.1215729,
               -86.8332913, -86.8652826, -86.764441,
               -86.882602, -86.856092, -86.904870,
               -87.0495737, -86.8978913)
District25_NoPaperBallot[c("Longitude")] <- Longitude

Latitude <- c(36.288577, 36.327801, 36.390989,
              36.341120, 36.368195, 36.278034,
              36.096244, 36.090248, 36.1812404, 
              36.172196, 36.0619829, 36.090652,
              36.114645, 36.2337379, 36.2552526,
              36.1062167, 36.0523131, 35.9857656,
              36.0333782, 36.0758519, 36.0862552, 
              36.0847296, 36.0586284, 36.1047576,
              36.037708,  35.8615263, 35.9123723,
              35.8355713, 35.9254472, 35.9290819,
              35.932968, 35.8180516, 35.8117338,
              35.7256528, 35.7775688, 35.8615263,
              35.6585538, 35.6813693, 35.7887965,
              36.0347965, 36.1016943, 36.0827731, 
              36.086132, 36.0107573, 36.194728, 
              36.1104869, 36.109711, 36.583496,
              36.474058, 36.441641, 36.474875,
              36.396575, 36.419222, 36.441376,
              36.423937, 36.556200, 36.5159228,
              36.5415252, 36.5565918, 36.520045,
              36.519984, 36.473194, 36.505669,
              36.4623593, 36.4992284)
District25_NoPaperBallot[c("Latitude")] <- Latitude
#shiny implementation

#ui page
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body{width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  
  #first slider with difference values
  absolutePanel(
    bottom = 10,
    right = 10,
    sliderInput(
      "slider1",
      "Difference in Votes, 2014 (Repub. - Dem.)",
      min = min(District25_NoPaperBallot$Difference),
      max = max(District25_NoPaperBallot$Difference),
      value = c(min(District25_NoPaperBallot$Difference),
                max(District25_NoPaperBallot$Difference))
    )
  ),
  
  #totalVotes - 2014
  absolutePanel(
    id = "slider1",
    bottom = 95,
    right = 10,
    sliderInput(
      "slider2",
      "Total Precinct Votes - 2014",
      min = min(District25_NoPaperBallot$TotalVotes),
      max = max(District25_NoPaperBallot$TotalVotes),
      value = c(min(District25_NoPaperBallot$TotalVotes),
                max(District25_NoPaperBallot$TotalVotes))
    )
  ),
  #registered voters - 2016
  absolutePanel(
    bottom = 180,
    right = 10,
    sliderInput(
      "slider3",
      "Registered Voters - 2016",
      min = min(District25_NoPaperBallot$RegVoters),
      max = max(District25_NoPaperBallot$RegVoters),
      value = c(min(District25_NoPaperBallot$RegVoters),
                max(District25_NoPaperBallot$RegVoters))
    )
  ),
  #number of votes - 2016
  absolutePanel(
    bottom = 265,
    right = 10,
    sliderInput(
      "slider4",
      "Number of Expected Votes  - 2016",
      min = min(District25_NoPaperBallot$`2016Expect`),
      max = max(District25_NoPaperBallot$`2016Expect`),
      value = c(min(District25_NoPaperBallot$`2016Expect`),
                max(District25_NoPaperBallot$`2016Expect`))
    )
  ),
  #avgDem %
  absolutePanel(
    bottom = 350,
    right = 10,
    sliderInput(
      "slider5",
      "Average % Democrat  - 2016",
      min = round(100 * min(District25_NoPaperBallot$AvgDem), 2),
      max = round(100 * max(District25_NoPaperBallot$AvgDem), 2),
      value = c(round(100 * min(District25_NoPaperBallot$AvgDem), 2),
                round(100 * max(District25_NoPaperBallot$AvgDem), 2))
    )
  ),
  #title of sliders
  absolutePanel(
    bottom = 450,
    right = 55,
    "Senate District 25 Election Data"
  )
)
#create server
server <- function(input, output, session) {
  subsetData1 <- reactive({
    District25_NoPaperBallot %>% 
      #multiple filters to best update map
      filter(District25_NoPaperBallot$Difference >= input$slider1[1] &
               District25_NoPaperBallot$Difference <= input$slider1[2] &
               District25_NoPaperBallot$TotalVotes >= input$slider2[1] &
               District25_NoPaperBallot$TotalVotes <= input$slider2[2] &
               District25_NoPaperBallot$RegVoters >= input$slider3[1] &
               District25_NoPaperBallot$RegVoters <= input$slider3[2] &
               District25_NoPaperBallot$`2016Expect` >= input$slider4[1] &
               District25_NoPaperBallot$`2016Expect` <= input$slider4[2] &
               round(100 * District25_NoPaperBallot$AvgDem, 2) >= input$slider5[1] &
               round(100 * District25_NoPaperBallot$AvgDem, 2) <= input$slider5[2]
               )
  })
  #initial map
  output$map <- renderLeaflet({
    leaflet(District25_NoPaperBallot) %>%
      setView(lng =-87, lat=36.1490 , zoom = 9) %>%
      addTiles() %>%
      addCircleMarkers(~ District25_NoPaperBallot$Longitude, ~District25_NoPaperBallot$Latitude, 
                 popup = as.character(District25_NoPaperBallot$COUNTY),
                 label = ~as.character(District25_NoPaperBallot$PRECINCT),
                 radius = 5)
  })
  observe({
    #leafletProxy to update with subsetData
    leafletProxy("map", data = subsetData1()) %>%
      
      clearMarkers() %>%
      clearShapes() %>%
      clearPopups() %>%
      
      addCircleMarkers( lat = ~Latitude,
                        lng = ~Longitude,
                        popup = as.character(District25_NoPaperBallot$COUNTY),
                        label = ~as.character(District25_NoPaperBallot$PRECINCT),
                        radius = 5)

  })
}

#run Shiny app
shinyApp(ui = ui, server = server)