# Load Packages ---------------------------------------------------------------
library(dplyr)
library(tidycensus)
library(ggplot2)
library(mapdeck)
library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(maps)
library(plotly)
library(DT)
library(tigris)
library(tidyverse)
library(tidycensus)
library(readxl)
library(collapsibleTree)
library(shinycssloaders)
library(leaflet)
library(leaflet.extras)
library(rvest)
library(sf)
library(shinydashboard)
library(shinydashboardPlus)
library(tidygeocoder)
library(janitor)
library(ggplot2)
library(magrittr)
options(tigris_use_cache = TRUE)
library(ggwordcloud)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(readr)
library(stringr)
library(mapdata)
library(htmlwidgets)
library(leafpop)
library(lattice)
library(htmltools)
library(leaflegend)
library(ggplotify)
library(grid)
library(gridExtra)
library(ggpubr)
library(lubridate)
library(shinyWidgets)
library(viridis)


prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")



readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

blocks<-c("Block Group 1, Census Tract 6112.05, Loudoun County, Virginia", "Block Group 2, Census Tract 6112.05, Loudoun County, Virginia", "Block Group 2, Census Tract 6112.04, Loudoun County, Virginia", "Block Group 2, Census Tract 6115.02, Loudoun County, Virginia","Block Group 3, Census Tract 6115.02, Loudoun County, Virginia", "Block Group 1, Census Tract 6113, Loudoun County, Virginia","Block Group 2, Census Tract 6113, Loudoun County, Virginia","Block Group 3, Census Tract 6113, Loudoun County, Virginia", "Block Group 1, Census Tract 6114, Loudoun County, Virginia","Block Group 2, Census Tract 6114, Loudoun County, Virginia","Block Group 3, Census Tract 6114, Loudoun County, Virginia","Block Group 1, Census Tract 6117.01, Loudoun County, Virginia","Block Group 2, Census Tract 6117.01, Loudoun County, Virginia", "Block Group 1, Census Tract 6116.02, Loudoun County, Virginia","Block Group 2, Census Tract 6116.02, Loudoun County, Virginia","Block Group 1, Census Tract 6116.01, Loudoun County, Virginia", "Block Group 2, Census Tract 6116.01, Loudoun County, Virginia")

va20_2 <- get_acs(geography = "block group",
                  variables = c(hispanic = "B03002_012"),
                  state = "VA",
                  year = 2020,
                  geometry = TRUE) %>%
  filter(NAME %in% blocks)

blocks_CDP<-c("Block Group 2, Census Tract 6115.02, Loudoun County, Virginia","Block Group 3, Census Tract 6115.02, Loudoun County, Virginia", "Block Group 1, Census Tract 6113, Loudoun County, Virginia","Block Group 2, Census Tract 6113, Loudoun County, Virginia","Block Group 3, Census Tract 6113, Loudoun County, Virginia", "Block Group 1, Census Tract 6114, Loudoun County, Virginia","Block Group 2, Census Tract 6114, Loudoun County, Virginia","Block Group 3, Census Tract 6114, Loudoun County, Virginia","Block Group 1, Census Tract 6117.01, Loudoun County, Virginia","Block Group 2, Census Tract 6117.01, Loudoun County, Virginia", "Block Group 1, Census Tract 6116.02, Loudoun County, Virginia","Block Group 2, Census Tract 6116.02, Loudoun County, Virginia","Block Group 1, Census Tract 6116.01, Loudoun County, Virginia","Block Group 2, Census Tract 6116.01, Loudoun County, Virginia")

va_20_CDP <- va20_2 %>% filter (NAME %in% blocks_CDP)

map <- read_excel(paste0(getwd(), "/data/school_locations.xlsx"))

map$Longitude <- as.numeric(map$Longitude)
map$Latitude <- as.numeric(map$Latitude)


map1<-leaflet(data = map) %>% addTiles() %>%
  addPolygons(data = va20_2,
              color="yellow",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5) %>%
  addMarkers(~Longitude, ~Latitude, popup = ~as.character(Address), label = ~as.character(Address)) %>% addPolygons(data = va_20_CDP,
                                                                                                                    color="red",
                                                                                                                    weight = 0.5,
                                                                                                                    smoothFactor = 0.2,
                                                                                                                    fillOpacity = 0.5) %>%
  addMarkers(~Longitude, ~Latitude, popup = ~as.character(Address), label = ~as.character(School)) 




# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }
           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }
            var mytype = getUrlParam('type','Empty');
            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");
                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }
           var x = document.getElementsByClassName('navbar-brand');
           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/node/451\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';
             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';
             //changeLinks('economic');
           }
           "
ui <- navbarPage(title = "Loudoun County Project 2022",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 
                 # main tab -----------------------------------------------------------
                 tabPanel("Project Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Evaulating Family Needs in Community Schools in Loudoun"),
                                      #h2("") ,
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Polytechnic Institute and State University"),
                                      #h4("[updat this]"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("The Setting")),
                                          
                                          p("The Community schools are centers for neighborhood enrichment, uniting families, educators and community partners to provide world class education, enrichment, health services, and other opportunities to succeed in school and life. Loudoun County atarted the community school initiative back in 2016. The first community school was Sterling Elementary School. Throughout the past several years, there's been a big community effort to build key connections in Loudoun County’s six Title 1 elementary schools, which serve under-resourced neighborhoods in Sterling. It’s called the Community Schools Initiative and its goal is to provide additional resources for Sugarland, Sterling, Rolling Ridge, Sully, Guilford, and Forest Grove elementary schools."),
                                          #p("During the 2018 – 2019 school year, the Community school model provided the families with clothes, shoes, and other basic supplies 538 times; enabled 135 families to receive weekend meals throughout the school year; supported 6 academic programs for 323 students; and provided 9 after-school enrichment programs for 373 students. Funds have provided these Community Schools with additional resources, such as full-time parent liaisons, a full-time social worker, and programs that keep families engaged in their child’s education. The Community Schools initiative focuses on bolstering these schools in six areas: academies, health and social services, youth and community engagement, building stronger families, and healthier communities."),
                                          
                                   ),
                                   column(4,
                                          h2(strong("Project Background")),
                                          
                                          p("During the 2018 – 2019 school year, the Community school model provided the families with clothes, shoes, and other basic supplies 538 times; enabled 135 families to receive weekend meals throughout the school year; supported 6 academic programs for 323 students; and provided 9 after-school enrichment programs for 373 students. Funds have provided these Community Schools with additional resources, such as full-time parent liaisons, a full-time social worker, and programs that keep families engaged in their child’s education. The Community Schools initiative focuses on bolstering these schools in six areas: academies, health and social services, youth and community engagement, building stronger families, and healthier communities."),
                                   ),
                                   
                                   column(4,
                                          h2(strong("Project Goals")),
                                          p("Identifying service gaps in the Community Schools"),
                                          #p(""),
                                          #p("")
                                   )
                          ),
                          #fluidRow(align = "center",
                          # p(tags$small(em('Last updated: August 2021'))))
                 ),
                 
                 ## Sterling Area--------------------------------------------
                 tabPanel("Sterling Area", value = "overview",
                          fluidRow(style = "margin: 6px;",
                                   p("", style = "padding-top:10px;"),
                                   column(12, align = "center",h4(strong("Map of Sterling")),
                                          p("This map shows the Sterling area and the 6 schools."),
                                          br("")
                                          
                                          
                                          
                                   )),
                          
                          fluidPage(
                            column(12, align = "center", leafletOutput("map1", width = "60%")
                                   #fluidRow(align = "center",
                                   #    p(tags$small(em('Last updated: August 2021'))))
                            ) 
                          )
                 ), 
                 navbarMenu("Sociodemographics" , 
                            tabPanel("Sterling", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Sterling"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              #column(4, 
                                              #      h4(strong("Education")),
                                              #     p("These are demographics"),
                                              #) ,
                                              column(12, 
                                                     h4(strong("Sterling, CDP")),
                                                     selectInput("demosdrop", "Select Variable:", width = "60%", choices = c(
                                                       "Age" = "age",
                                                       "Gender" = "gender",
                                                       "Race/ethnicity" = "race", 
                                                       "Educational Attainment" = "edu",
                                                       "Poverty by Age and Sex" = "pov", 
                                                       "Marital Status" = "mar",
                                                       "Family Income" = "faminc",
                                                       "Property Value" = "propval"
                                                     ),
                                                     ), 
                                                     withSpinner(plotOutput("ageplot", height = "500px", width = "60%")),
                                                     
                                              ),
                                              # column(12, 
                                              #      h4("References: "), 
                                              #     p(tags$small("[1] Groundwater: Groundwater sustainability. (2021). Retrieved July 27, 2021, from https://www.ngwa.org/what-is-groundwater/groundwater-issues/groundwater-sustainability")) ,
                                              #     p("", style = "padding-top:10px;")) 
                                     )), 
                            tabPanel("Community Schools", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Demographics of Community Schools"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              #column(4, 
                                              #      h4(strong("Education")),
                                              #     p("These are demographics"),
                                              #  ) ,
                                              column(12, 
                                                     h4(strong("Community Schools")),
                                                     selectInput("schooldrop", "Select Variable:", width = "60%", choices = c(
                                                       "Gender" = "cgender",
                                                       "Race/Ethnicity" ="crace", 
                                                       "Hispanic Population" = "chispanic",
                                                       "No. of teacher" = "teacher",
                                                       "Enrollment" = "enrol", 
                                                       "Absences By Quarter" = "absense", 
                                                       "Chronic Absenteeism" = "chronic"
                                                     ),
                                                     ), 
                                                     withSpinner(plotOutput("ocuplot", height = "500px")),
                                                     
                                              ),
                                              # column(12, 
                                              #       h4("References: "), 
                                              #       p(tags$small("[1] Groundwater: Groundwater sustainability. (2021). Retrieved July 27, 2021, from https://www.ngwa.org/what-is-groundwater/groundwater-issues/groundwater-sustainability")) ,
                                              #      p("", style = "padding-top:10px;")) 
                                     )), 
                            
                            
                            
                 ), 
                 
)


# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  # Render map 
  output$map1 <- renderLeaflet({
    map1
  })
  
  ageVar <- reactive({
    input$demosdrop
  })
  
  output$ageplot <- renderPlot({
    if (ageVar() == "age") {
      
      age
    }
    else if (ageVar() == "faminc") {
      income
    }
    else if(ageVar() == "pov"){
      pov
    }
    
  })
}

#sociodemo tabset ----------------------------------------------------

shinyApp(ui = ui, server = server)
  