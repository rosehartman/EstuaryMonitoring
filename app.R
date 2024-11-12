library(tidyverse)
library(shiny)
library(leaflet)
require(shinyWidgets)
require(rlang)
require(DT)
library(sf)
library(leafpop)


# Load initial data

load("estuaries.Rdata")


Program_info = select(estuaries, `Program Name`, Estuary, `Type of monitoring`, `Agency/Entity`,
                      `Contact`, `Program link`, `Project description notes`, `Start year`, `Program Status`)

Parameters<-c("Continuous WQ","Discrete WQ","Contaminants","Water Level",
              "Nutrients", "Sediment Properties", "Suspended Sediments/Turbidity",
              "Birds",
              "Phytoplankton/chlorophyll", "HABs", "Macroalgae", "Fish", "Invertebrates", "Vegetation", "SAV/FAV")

estua = unique(estuaries$Estuary)

# Define UI for application that draws a histogram
ui <- navbarPage("California Estuaries monitoring", id="nav",
                 
                 tabPanel("Info", 
                          
                          # Write introductory text
                          
                          a(shiny::icon("reply"), "CA Estuary Monitoring Workgroup Homepage", href="https://mywaterquality.ca.gov/monitoring_council/estuary_workgroup/"),
                          tags$div(tags$h2("Information"), 
                                   tags$p(tags$b("Please contact Rosemary hartman ", 
                                                 tags$a('(Rosemary.Hartman@water.ca.gov)', 
                                                        href="mailto:Rosemary.Hartman@water.ca.gov?subject=Monitoring%20Shiny%20App"), 
                                                 " with any questions.")),
                                   tags$p("This app displays locations of monitoring programs in estuaries across California."),
                                   a(shiny::icon("github"), "App code is available here", href="https://github.com/rosehartman/EstuaryMonitoring")),
                          tags$h3("Survey details"),
                          
                          # Include table of info
                          
                          dataTableOutput("Program_info")
                 ),                
                 # Second tab for map
                 
                 tabPanel("Interactive map", value="map",
                          
                          # Output map
                          
                          leafletOutput("mapplot", width="100%", height="100vh"),
                          
                          # Include panel with user inputs
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        pickerInput("Parameters", "Parameters of interest (only stations with samples of these parameters will be retained)", choices=Parameters, 
                                                    selected = Parameters, multiple = T, options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3")),
                                        pickerInput("Estuaries", "Estuary", choices=estua,
                                                    selected = estua, multiple = T, 
                                                    options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3")),
                                             prettySwitch("Years", "Inspect sampling effort for each year?", status = "success", fill = TRUE, bigger=TRUE),
                                        conditionalPanel(condition="input.Years",
                                                         sliderInput("Year", "Select year",
                                                                     min= 1959, max=2024, value=2018, step=1, sep="",
                                                                     animate=TRUE)),
  
                          
                 )),
                 # First tab for general info
                 

                 
                 # Add custom css to control appearance
                 
                 tags$style(HTML("

.selected {background-color:white !important;}
.selected {color:black !important;}
.dropdown-menu.inner {background-color:#D6D6D6 !important;}
td:first-child {
  font-weight: 900;
}
"))           
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Survey info table
  output$Program_info<-renderDataTable({
    datatable(Program_info, rownames=F, escape = FALSE, options=list(paging=FALSE))
  })
  
  # Update the survey input with just the surveys that sample the user-selected parameters
  
  observeEvent(input$Parameters, {
    req(input$nav=="map")
    
    if(is.null(input$Parameters)){
      Parameters<--c("Continuous WQ","Discrete WQ","Contaminants","Water Level",
                     "Nutrients", "Sediment Properties", "Suspended Sediments/Turbidity",
                     "Birds",
                     "Phytoplankton", "Zooplankton", "Water_quality", "Fish")
    } else{
      Parameters<-input$Parameters
    }
    
    Sources<-estuaries%>%
      filter(if_any(all_of(Parameters), ~.x>0))%>%
      pull(`Program Name`)%>%
      unique()
    
  })
  
  
  Data<-reactive({
    
    out<-estuary%>%
      dplyr::filter(Estuary%in%input$Estuaries & if_any(all_of(input$Parameters)))
    return(out)
  })
  
  # Create the dataset used for plotting, including all the info for the clickable popup and filtered to the chosen year if users are choosing years
  
  
  # Create the color palette for monitoring surveys
  
  pal_survey<-reactive({
    req(input$nav=="map", Data())
    
    Sources<-unique(Data()$`Type of monitoring`)

      colorFactor(c("#8DD3C7", "#FFFFB3", "#BEBADA"), Sources)
  })

  


  # Create the base map (these components don't change)
  
  mapplot<-reactive({
    req(input$nav=="map")
    leaflet()%>%
      addProviderTiles("Esri.WorldGrayCanvas")%>%
      fitBounds(min(estuary$Longitude, na.rm=T), min(estuary$Latitude), max(estuary$Longitude), max(estuary$Latitude))
  })
  
  # Create the reactive components of the map that are responsive to user inputs
  
  observe({

    data<-Data()
    
    mapplot<-leafletProxy("mapplot", session, data = data)%>%
      clearMarkers() %>%
      clearControls()%>%
      addCircleMarkers(., weight = 1, lng = ~Longitude, lat = ~Latitude,
                         fillColor = ~pal_survey()(data$`Type of monitoring`), 
                       color="black", fillOpacity = 0.7, 
                       
                       # popup=paste( data$Estuary, "\n", "<a href='"
                       #   , data$`Program link`
                       #   , "' target='_blank'>"
                       #   , data$`Program Name`, "</a>")
                       popup = popupTable(data)
                       ) %>%
          addLegend(., "topleft", pal = pal_survey(), values = ~data$`Type of monitoring`, opacity=1, title="Type of monitoring")
      
    
  })
  
  # Output the map
  
  output$mapplot <- renderLeaflet({
    mapplot()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
