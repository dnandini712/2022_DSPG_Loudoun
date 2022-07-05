#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(tidycensus)
library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)
library(DT)
library(shinydashboard)
library(readxl)
library(shinyjs)
library(rworldmap)
library(sp)
library(sf)
library(spatialEco)
library(FRK)
library(rgdal)
library(shinycssloaders)
library(shinythemes)
library(stringr)
library(viridis)
options(tigris_use_cache = TRUE)

#data --------------------------------------

readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

sterling <- read_excel(paste0(getwd(),"/data/sterlingdata.xlsx"),skip=1,col_names=TRUE)
subset_sterling <- sterling[6:18,c(1, 10:13)]
subset_sterling
sterling$Percent...12[6:18]
age_estimate <- sterling$Percent...12[6:18]
age_estimate <- gsub("%","",age_estimate)
age_percent <- as.numeric(age_estimate)
age_cat <- (subset_sterling$Label)
#this is how to subset the data
#turn categories into factors 
age<-ggplot(subset_sterling,aes(x=age_cat,y=age_percent, fill=age_cat))+geom_col()+theme(axis.text.x=element_blank(), axis.title.x = element_blank(), axis.text.y = element_blank(),axis.ticks.y=element_blank())+scale_x_discrete(limits=age_cat)+labs(caption= "Source: S0101 ACS 5-year data 2016-2020",y="Percent",)+ coord_polar()+guides(fill = guide_legend(title = "Age Group")) + geom_text(aes(label = age_percent, y = age_percent), size = 3, position = position_stack(vjust = 0.8))


#logo code ----------------------------------
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

#user interface-----------------------------------
ui <- navbarPage(title = "DSPG 2022",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown{z-index:10000}')),
                 useShinyjs(),
                 
              tabPanel("Overview",value = "overview"),
                 fluidRow(style = "margin: 6px;",
                          align = "center",
                          h1(strong("Identifying Service Gaps for Families Attending Loudoun County Community Schools")),
                          h4("Data Science for the Public Good Program"),
                          h4("Virginia Tech"),
                         ),
                  fluidRow(style="margin:6px;",
                               column(4,
                                 h2(strong("Background")),
                                 p(a(href = "https://www.loudoun.gov/", strong("Loudoun County:",target = "_blank"),"Loudon County is a county that is a part of the 5-million-acre Northern part of Virginia that was granted by King Charles II in 1649. Over the next few decades, this land would be broken into four counties: Westmoreland, Stafford, Prince William, and Fairfax. In 1757, Fairfax would be divided into more counties, one being named Loudon County. Loudon County was named for John Campbell. John Campbell was a Scottish nobleman and served as the commander-in-chief for all British armed forces in North America, and was the fourth earl of Loudon. From 1756 to 1759, he was the titular governor of Virginia."),
                                   
                                   p(a(href="https://en.wikipedia.org/wiki/Sterling,_Virginia", strong("Sterling, VA:",target = "_blank"), "Sterling, Virginia is a census-designated place (CDP) in Loudoun County, Virginia. However, the “Sterling, Virginia” mailing address applies to a much wider region including other localities such as Arcola, Cascades, Dulles, Countryside, and Sugarland Run. Sugarland Run in a portion of Sterling with zip code 20164.")),
                                  
                                   p(a(href="https://loudouneducationfoundation.org/community-schools-blog/?utm_content=buffer43559&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer", strong("What are Community Schools?",target = "_blank"),"Center for neighborhood enrichment, uniting families, educators and community partners to provide world class education, enrichment, health services, and other opportunities to succeed in school and life. "))
                                 )
                          ),
                         column(4,
                                h2(strong("Our Work")),
                                p("Our research team worked with...")),
                         column(4,
                                h2(strong("Dashboard Aims")),
                                p("The aim of our dashboard")),
                fluidRow(align = "center",
                         p(tags$small(em('Last updated: August 2022')))
              ),
         tabPanel("Sociodemographics",value="sociodemographics",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Floyd County Residents' Sociodemographic Characteristics"), align = "center"),
                                   p("", style = "padding-top:10px;"))),

                 navbarMenu("Services",
                            tabPanel("Health and Social Services"),
                            tabPanel("Mental Health"),
                            tabPanel("Youth Development"),
                            tabPanel("Family Engagement")),
                navbarMenu("Gaps",
                          tabPanel("Overall"),
                          tabPanel("School-Wise")),
                tabPanel("Analysis"),
                tabPanel("Data"),
                tabPanel("Team"))

server <- function(input,output){}
shinyApp(ui=ui,server=server)