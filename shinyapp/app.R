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


# data -----------------------------------------------------------

# Sterling Map -----------------------------------------------------


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


#----------Gender---------------------------------------------

labelsG = c("Male", "Female")
valuesG = c(15282, 14989)
gender <- plot_ly(type='pie', labels=labelsG, values=valuesG, 
                textinfo='label+percent',
                insidetextorientation='radial', marker = list(colors = c('20AFCC', 'F56D4F'))) %>% layout(title ='', legend=list(title=list(text='')))
gender

#---------Age pie chart---------------------------------------

this.year = 2020
vars <- load_variables(year = 2020,
                       dataset = "acs5",
                       cache = TRUE)
dim(vars)

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

#-----------Race/Ethnicity--------------------

library(plotly)

labelsR = c("White", "Black", "Am.Indian", "Asian","Hawaiian","Other")
valuesR = c(18138, 3132, 418, 5313, 144, 4570)

race <- plot_ly(type='pie', labels=labelsR, values=valuesR, 
                textinfo='label+percent',
                insidetextorientation='radial') %>% layout(title ='', legend=list(title=list(text='Race/Ethnicity')))

#-------income--------------------------------
medianin <- read_excel(paste0(getwd(),"/data/incomemedian.xlsx"))

subset_medianin <- medianin[4:13, 1:5]
subset_medianin
mi_pop <- subset_medianin$...4
mi_cat <- subset_medianin$...1


mi_cat.fac <- factor(mi_cat, levels = c(mi_cat))
subset_medianin$new_pop<-gsub("%$","",subset_medianin$...4)
pop_nop <- subset_medianin$new_pop
pop_num <- as.numeric(pop_nop)
income <- ggplot(subset_medianin,aes(x=mi_cat.fac,y=pop_num, fill=mi_cat.fac))+geom_col(stat="identity")+theme(axis.text.x=element_blank(),axis.ticks.y=element_blank(),axis.title.x = element_blank()+scale_x_discrete(limits=mi_cat.fac),axis.text.y=element_blank())+labs(caption= "Source: S1901 ACS 5-year data 2016-2020",x="Income", y="Percent") + coord_polar() + guides(fill = guide_legend(title = "Income Level ($)")) + geom_text(aes(label=pop_num,y=pop_num), size = 3, position = position_stack(vjust = 1.1))

#---------Property Value---------------------------------

dfpv <- read_excel(paste0(getwd(), "/data/Property_Value.xlsx"), col_names = TRUE)

figpv <- dfpv %>% plot_ly(labels = ~`HOUSING OCCUPANCY`, values = ~dfpv$count, sort = FALSE, direction = "clockwise")
figpv <- figpv %>% add_pie(hole = 0.5)
property <- figpv %>% layout(title = "", showlegend = T,
                             xaxis = list(showgrid = TRUE, zeroline = FALSE, showticklabels = TRUE),
                             yaxis = list(showgrid = TRUE, zeroline = FALSE, showticklabels = TRUE))

#------------Housing Occupancy---------------------------

lbls.HOUSING = c("Owners", "Renters")
slices.HOUSING = c(6839, 2412)

housing <- plot_ly(type='pie', labels=lbls.HOUSING, values=slices.HOUSING, 
                   textinfo='label+percent',
                   insidetextorientation='radial') %>% layout(title ='', legend=list(title=list(text='Occupants')))

#-------------Commuter Time------------------------------

labelsCT = c("Less than 10 minutes","10 to 14 minutes","15 to 19 minutes","20 to 24 minutes", "25 to 29 minutes", "30 to 34 minutes", "35 to 44 minutes", "45 to 59 minutes", "60 or more minutes")
valuesCT = c(5.99, 18.0, 16.9, 13.2, 4.8, 13.1, 7.09, 10.2, 10.8)

commutertime <- plot_ly(type='funnelarea', labels=labelsCT, values=valuesCT, sort = FALSE, direction = "",
                        textinfo='percent',
                        insidetextorientation='radial') %>% layout(title ='', showlegend=TRUE, legend=list(x=1, y=0.5))
#------------------poverty-------------------------------

poverty_as<- read_excel(paste0(getwd(),"/data/povertybyageandsexnewss.xlsx"), 
           sheet = "Data")

subset_poverty_as <- poverty_as[3:28, 1:4]
subset_poverty_as
#subset_poverty_as$Estimate[1:26]
povas_pop <- subset_poverty_as$Estimate
povas_pop <- as.numeric(povas_pop)
povas_cat <- subset_poverty_as$Label

ggplot(subset_poverty_as, aes(x=povas_cat,y=povas_pop,fill=Sex)) + 
  geom_bar(stat = "identity",position=position_dodge(),width=.6)   + coord_flip() + theme(axis.text.x=element_text(angle=90)) +scale_fill_manual(values=c("tomato2", "darkblue"))+labs(title="Poverty by Age and Sex in Sterling, VA",subtitle="2020",caption= "Source: ACS data 2016-2020",x="Age",y="Estimated Population") +scale_x_discrete(limits=c("Under 5 years", "5 years","6 to 11 years","12 to 14 years","15 years","16 to 17 years","18 to 24 years","25 to 34 years","35 to 44 years","45 to 54 years","55 to 64 years","65 to 74 years","75 years and older")) 


pov <- ggplot(subset_poverty_as,aes(x=povas_cat,y=povas_pop, fill=Sex))+geom_col(position="dodge",width=1.5)+theme(axis.text.x=element_text(angle=90, size=7.5, face="bold"),axis.title.x = element_blank())+scale_x_discrete(limits=povas_cat[1:length(povas_cat)/2])+labs(caption= "Source: B17001 ACS 5-year data 2016-2020",x="Age",y="Total Population")+ scale_fill_discrete(name = "", labels = c("Female", "Male")) 

#race by school ----------------------------

races <- read_excel(paste0(getwd(),"/data/racedems.xlsx"))

race_subset <- races[(1:153),c(1,5,6,7)]
School <- race_subset$`School Name`
raceS <- race_subset$Race
total <- race_subset$`Full Time Count (All Grades)`
raceehtn <- ggplot(race_subset, aes(x=raceS,y=total,fill=School))+ geom_col()+labs(x="Race/Ethnicity",y="Number of Students",caption = "Source: VDOE Fall Membership Report 2016-2020") + theme(plot.caption.position = "plot",plot.caption = element_text(hjust = 1),axis.text.x=element_text(angle=90)) + scale_fill_brewer(palette = "Set1") + scale_fill_discrete(name = "")
raceehtn<-ggplotly(raceehtn)

#attendance --------------

attendance <- read_excel(paste0(getwd(),"/data/absencerate.xlsx"))
att_per <- attendance$`Absence Rate`
att_rate <- att_per*100
quarter <- attendance$`School Quarter`
School <- attendance$`School Name`
attend <- ggplot(attendance,aes(x=quarter,y=att_rate,group=School,color=School))+geom_point()+geom_line() +labs(caption= "Source: LCPS Dashboard 2021-2022",x="Quarter",y="Percentage") + theme(plot.caption.position = "plot",plot.caption = element_text(hjust = 1)) + scale_fill_brewer(palette = "Set1")

                                                                                                                                                                                                

#------------------employment-----------------
emp <- read_excel(paste0(getwd(),"/data/Employmentsterling.xlsx"),skip=2,col_names=TRUE)
subset_emp <- emp[c(29:33),]
percNum <- c()
for(i in 1:5){
  percNum <- c(percNum, as.numeric(sub("%", "", subset_emp[i, "...4"])))
}
subset_emp[, "...4"] <- percNum
employment <- ggplot(subset_emp, aes(x = `EMPLOYMENT STATUS`, y = (...4), fill = `EMPLOYMENT STATUS`)) + geom_bar(position = "stack", stat="identity", width = 0.5)+ theme(axis.text.x = element_text(angle=90), legend.position="none") + labs(x = "Occupations", y = "Percentages", caption = " Source : DP03 ACS 5-yr data 2016-2020", title = "Work Occupations") + coord_flip()


employment<- ggplotly(employment)
#---------------------health insurance----------------------------


health <- read_excel(paste0(getwd(),"/data/Employmentsterling.xlsx"),skip=2,col_names=TRUE)

subset_health <- health[c(103:105),]
healthin <- ggplot(subset_health, aes(x = `EMPLOYMENT STATUS`, y = ...4, fill = `EMPLOYMENT STATUS`)) + geom_bar(position = "stack", stat="identity", width = 0.5) + labs(x = "Insurance Type", y= "Percentage", caption = " Source : DP03 ACS 5 -yr data 2016-2020", titlel = "Distribution of Health Insurance") + guides(fill = guide_legend(title = "Health Insurance Type"))+ theme(axis.text.x = element_text(angle=0))+ coord_flip()
#race by school ----------------------------

races <- read_excel(paste0(getwd(),"/data/racedems.xlsx"))

race_subset <- races[(1:153),c(1,5,6,7)]
School <- race_subset$`School Name`
raceS <- race_subset$Race
total <- race_subset$`Full Time Count (All Grades)`
raceehtn <- ggplot(race_subset, aes(x=raceS,y=total,fill=School))+ geom_col()+labs(x="Race/Ethnicity",y="Number of Students",caption = "Source: VDOE Fall Membership Report 2016-2020") + theme(plot.caption.position = "plot",plot.caption = element_text(hjust = 1),axis.text.x=element_text(angle=90)) + scale_fill_brewer(palette = "Set1") + scale_fill_discrete(name = "")
raceehtn<-ggplotly(raceehtn)

#attendance --------------

attendance <- read_excel(paste0(getwd(),"/data/absencerate.xlsx"))
att_per <- attendance$`Absence Rate`
att_rate <- att_per*100
quarter <- attendance$`School Quarter`
School <- attendance$`School Name`
attend <- ggplot(attendance,aes(x=quarter,y=att_rate,group=School,color=School))+geom_point()+geom_line() +labs(caption= "Source: LCPS Dashboard 2021-2022",x="Quarter",y="Percentage") + theme(plot.caption.position = "plot",plot.caption = element_text(hjust = 1)) + scale_fill_brewer(palette = "Set1")

#---------------Number of Teachers/Staff--------------------------

Schools <- c("Sterling", "Sugarland", "Rolling Ridge", "Forest Grove", "Guilford", "Sully")
Teachers <- c(32, 52, 66, 55, 59, 35)
Staff <- c(49, 22, 27, 20, 29, 19)
dataSTAFF <- data.frame(Schools, Teachers, Staff)

figSTM <- plot_ly(dataSTAFF, x = ~Schools, y = ~Teachers, type = 'bar', name = 'Teachers', marker = list(color = 'rgb(255, 2, 2 )'))
figSTM <- figSTM %>% add_trace(y = ~Staff, name = 'Staff', marker = list(color = 'rgb(20, 252, 241 )'))
cteacher <- figSTM %>% layout(yaxis = list(title = 'Total Number'), barmode = 'stack')

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


#plotly vs plot variable----

var <- c("ageplot1","ageplot2")
# user -------------------------------------------------------------
ui <- navbarPage(title = "DSPG-LivDiv 2022",
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
                 tabPanel("Sociodemographics of Sterling",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Sterling"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              #column(4, 
                                              #      h4(strong("Education")),
                                              #     p("These are demographics"),
                                              #) ,
                                              column(8, 
                                                     h4(strong("Sterling, CDP")),
                                                     selectInput("demosdrop", "Select Variable:", width = "60%", choices = c(
                                                       "Gender" = "gender",
                                                       "Age" = "age",
                                                       "Race/ethnicity" = "race", 
                                                       "Educational Attainment" = "edu",
                                                       "Family Income" = "faminc",
                                                       "Property Value" = "property",
                                                       "Housing Occupancy" = "housing",
                                                       "Employment" = "employment",
                                                       "Work Occupation" = "workoccu",
                                                       "Commuter Time" = "commutertime",
                                                       "Commuter Mode" = "commmode",
                                                       "Poverty by Age and Sex" = "pov", 
                                                       "Health Coverage" = "health"
                                                       ),
                                                     ),
                                                     
                                                     withSpinner(plotlyOutput("ageplot2", height = "500px", width ="60%")),
                                                     withSpinner(plotOutput("ageplot1", height = "500px", width = "60%")),
                                                     
                                                       #if ( == "age"){
                                                         #withSpinner(plotOutput("ageplot1", height = "500px", width = "60%"))
                                                       #} else {
                                                         #withSpinner(plotlyOutput("ageplot2", height = "500px", width ="60%"))
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
                                                       "Race/Ethnicity" ="raceehtn", 
                                                       "Hispanic Population" = "chispanic",
                                                       "No. of teacher/Staff" = "cteacher",
                                                       "Enrollment" = "cenrol", 
                                                       "Absences By Quarter" = "attend", 
                                                       "Chronic Absenteeism" = "chronic"
                                                     ),
                                                     ), 
                                                     withSpinner(plotlyOutput("ocuplot", height = "500px", width = "60%")),
                                                     
                                              ),
                                              # column(12, 
                                              #       h4("References: "), 
                                              #       p(tags$small("[1] Groundwater: Groundwater sustainability. (2021). Retrieved July 27, 2021, from https://www.ngwa.org/what-is-groundwater/groundwater-issues/groundwater-sustainability")) ,
                                              #      p("", style = "padding-top:10px;")) 
                                     )), 
                            
                            
                            
                 )
                 
                 
# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  # Render map 
  output$map1 <- renderLeaflet({
    map1
  })
  
  Var <- reactive({
    input$demosdrop
  })

  
  output$ageplot1 <- renderPlot({
    if (Var() == "age") {
      
      age
    }
    else if (Var() == "faminc") {
     income
    }
    else if (Var() == "health") {
      
     healthin 
    }
    
  })
  
  output$ageplot2 <- renderPlotly({
    if (Var() == "pov") {
      
      pov
    }
    
    else if (Var() == "gender") {
      gender
    }
    else if (Var() == "race") {
    
      race
    }
    
    else if (Var() == "property") {
      
      property
    }
    
    else if (Var() == "housing") {
      
      housing
    }
    
    else if (Var() == "commutertime") {
      
      commutertime
    }
    else if (Var() == "employment"){
      employment
    }
  })


#School Demos

Var2 <- reactive({
    input$schooldrop
  }) 
  
output$ocuplot <- renderPlotly({
  
  if(Var2() == "raceehtn"){
    raceehtn 
  }
  
  else if(Var2() == "attend"){
    attend 
  }
  
  else if (Var2() == "cteacher") {
    
    cteacher
  }
})
}  

  
  #sociodemo tabset ----------------------------------------------------

shinyApp(ui = ui, server = server)