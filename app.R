library(rgdal)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(DT)

# load data
dat <- readr::read_csv("data/2022 NCDP Disaster Mitigation State Bill Tracking_Public.csv") |> 
  janitor::clean_names()

# us map data
us_spdf <- rgdal::readOGR( 
  dsn= "data/cb_2018_us_state_20m", 
  verbose=FALSE
)

# my_choices <- sort(unique(dat$state))

# User interface ----
ui <- fluidPage(
  
  # title
  titlePanel("2022 NCDP Disaster Mitigation State Bill Tracking"),
  
  # input panel
  fluidRow(
    column(4,
      column(6,

      #checkboxInput("all", "Select All/None", value = F),
      
      # choose state
      checkboxGroupInput("state", 
                  label = "Choose a state",
                  choices = sort(unique(dat$state)),
                  selected = NULL,
                  inline = F)
      ),
 
    column(6,
      # choose ncdp categories     
      checkboxGroupInput("ncdp_categories", 
                         label = "Choose a category",
                         choices = sort(unique(dat$ncdp_categories)),
                         selected = sort(unique(dat$ncdp_categories)))
    )
    ),
    
    mainPanel(
      leafletOutput("us_map"), # output map
      DTOutput('us_table') # output table
      )
  )
)

# Server logic ----
server <- function(input, output, session) {

  # add State: to actual state names
  mytext <- paste(
    "State: ", us_spdf@data$NAME,"<br/>", 
    sep="") %>%
    lapply(htmltools::HTML)
  
  # leaflet map using US states shapefile downloaded online
  output$us_map <- renderLeaflet({
    leaflet(us_spdf) %>%
      addTiles()  %>%
      setView(lat=10, lng=0 , zoom=2) %>%
      addPolygons(
        stroke=TRUE,
        fillOpacity = 0.5,
        color="white",
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addPolylines(stroke=TRUE, weight = 2, color= "skyblue")
  })
  
  # add a leaflet proxy
  proxy <- leafletProxy("us_map")
  
  # interactive control of the map
  observe({
    # 
    # updateCheckboxGroupInput(
    #   session, "state", choices = my_choices,
    #   selected = if(input$all) my_choices
    # )
    
    if(length(input$state) > 0){
    
      #get the selected polygon and extract the label point
      selected_polygon <- subset(us_spdf, us_spdf$NAME %in% input$state)

      polygon_labelPt <- selected_polygon@polygons[[1]]@labpt

      #remove any previously highlighted polygon
      proxy %>% clearGroup("highlighted_polygon")

      #center the view on the polygon
      proxy %>% setView(lng = polygon_labelPt[1],
                        lat=polygon_labelPt[2],
                        zoom = 4)

      #add a slightly thicker red polygon on top of the selected one
      proxy %>% addPolylines(stroke=TRUE,
                             weight = 4,
                             color="red",
                             data=selected_polygon,
                             group="highlighted_polygon")
    } else {
      proxy %>% clearGroup("highlighted_polygon")
    }
  }
  )
  
  # render table based on selections of states and categories
  output$us_table <- renderDT(
    dat |> filter(state %in% input$state,
                  ncdp_categories %in% input$ncdp_categories),
    options = list(pageLength = 5)
    )
  
}

# Run app ----
shinyApp(ui, server)