library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(maps)
library(plotly)
library(DT)
library(dplyr)
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
options(tigris_use_cache = TRUE)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Sterling",
             tabName = "sterling"
             ),    
    menuItem("Demographics of Sterling",             
             tabName = "demographics"
              ),
    menuItem("Community Schools",
             tabName = "schools"
    ),    
    menuItem("Availability of Services",             
             tabName = "services"), 
    menuItem("Service Gaps",             
             tabName = "gaps")
    
    ))

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "sterling"),    
    tabItem(tabName = "demographics", tabBox(
      title = "Sterling, CDP",
      tabPanel("Age"),
      tabPanel("Gender"), 
      tabPanel("Race/Ethnicity"),
      tabPanel("gaps")
    )),
    tabItem(tabName = "schools"),    
    tabItem(tabName = "services") 
  )
)

ui <- dashboardPage(
    skin="yellow-light",
    header = dashboardHeader(),
    sidebar = sidebar,
    body = body
)

server <- function(input, output) {}

shinyApp(ui, server)
