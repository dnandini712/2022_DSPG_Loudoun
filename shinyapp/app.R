#==========DSPG 2022============LOUDOUN

#Load Packages ---------------------------------------------------------------
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
library(traveltime)
library(nycflights13)
library(osmdata)
library(purrr)
library(mapview)
library(osrm)
library(rmapzen)
library(scales)
library(ggwordcloud)
library(wordcloud2)


prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")


# data -----------------------------------------------------------

# Sterling Map -----------------------------------------------------


readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

blocks<-c("Block Group 1, Census Tract 6112.05, Loudoun County, Virginia",
          "Block Group 2, Census Tract 6112.05, Loudoun County, Virginia",
          "Block Group 3, Census Tract 6112.05, Loudoun County, Virginia",
          "Block Group 2, Census Tract 6112.04, Loudoun County, Virginia",
          "Block Group 3, Census Tract 6112.04, Loudoun County, Virginia",
          "Block Group 2, Census Tract 6115.02, Loudoun County, Virginia",
          "Block Group 3, Census Tract 6115.02, Loudoun County, Virginia",
          "Block Group 1, Census Tract 6113, Loudoun County, Virginia",
          "Block Group 2, Census Tract 6113, Loudoun County, Virginia",
          "Block Group 3, Census Tract 6113, Loudoun County, Virginia",
          "Block Group 1, Census Tract 6114, Loudoun County, Virginia",
          "Block Group 2, Census Tract 6114, Loudoun County, Virginia",
          "Block Group 3, Census Tract 6114, Loudoun County, Virginia",
          "Block Group 1, Census Tract 6117.01, Loudoun County, Virginia",
          "Block Group 2, Census Tract 6117.01, Loudoun County, Virginia",
          "Block Group 1, Census Tract 6116.02, Loudoun County, Virginia",
          "Block Group 2, Census Tract 6116.02, Loudoun County, Virginia",
          "Block Group 1, Census Tract 6116.01, Loudoun County, Virginia",
          "Block Group 2, Census Tract 6116.01, Loudoun County, Virginia", 
          "Block Group 3, Census Tract 6112.02, Loudoun County, Virginia")
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


popups <- lapply(
  paste("<strong>Name: </strong>",
        str_to_title(map$Name),
        "<br />",
        "<strong>Address:</strong>",
        map$Address ,
        "<br />",
        "<a href = ",map$Website, "> Website </a>",
        "<br />"),
  
  
  htmltools::HTML
)


map1<-leaflet(data = map) %>% addTiles() %>%
  addPolygons(data = va20_2,
              color="yellow",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% addPolygons(data = va_20_CDP,color="red",weight = 0.5,smoothFactor = 0.2,fillOpacity = 0.5) %>%
  addMarkers(~Longitude, ~Latitude, popup = popups, label = ~as.character(Name))


#----------Gender---------------------------------------------

labelsG = c("Male", "Female")
valuesG = c(15282, 14989)
gender <- plot_ly(type='pie', labels=labelsG, values=valuesG, 
                  textinfo='label+percent',
                  insidetextorientation='radial', hoverinfo = 'text', text = ~paste('Total:', valuesG), marker = list(colors = c('20AFCC', 'F56D4F'))) %>% layout(title ='Gender', legend=list(title=list(text='')))
gender

#---------Age chart---------------------------------------


sterling1 <- read_excel(paste0(getwd(),"/data/sterlingdata.xlsx"),skip=1,col_names=TRUE)
subset_sterling1 <- sterling1[6:18,c(1, 10:13)]
age_estimate1 <- sterling1$Percent...12[6:18]
age_estimate1 <- gsub("%","",age_estimate1)
age_percent1 <- as.numeric(age_estimate1)
age_cat1 <- (subset_sterling1$Label)
#this is how to subset the data
#turn categories into factors 
Age1 <- age_cat1
Percent1 <- age_percent1

agesterling<- plot_ly(subset_sterling1,x=~Age1, y=~Percent1,type = "bar", color = ~Age1, hoverinfo = "text",text = ~paste("Age:",Age1,"<br>","Percent:",Percent1,"%" )) %>% layout(title = "Age Distribution",xaxis = list(title=""))


#-----------Race/Ethnicity--------------------

labelsR = c("White", "Black", "Am.Indian", "Asian","Hawaiian","Other")
valuesR = c(18138, 3132, 418, 5313, 144, 4570)

race <- plot_ly(type='pie', labels=labelsR, values=valuesR, 
                textinfo='label+percent',
                hoverinfo = 'text', 
                text = ~paste('Total:', valuesR),
                insidetextorientation='radial') %>% layout(title ='Race/Ethnicity Composition', legend=list(title=list(text='Select Race')))

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
income <- plot_ly(subset_medianin,x=~mi_cat.fac,y=~pop_num,color = ~mi_cat.fac,type = "bar", hoverinfo = "text",text = ~paste("Income Level:",mi_cat.fac,"<br>","Percent:",pop_num,"%")) %>% layout(title = "Median Income Distribution",xaxis = list(title="") ,yaxis= list(title = "Percent"))
#---------Property Value---------------------------------

dfpv <- read_excel(paste0(getwd(), "/data/Property_Value.xlsx"), col_names = TRUE)
Numberpv=c(58,6,46,204,1137,4653,709,26)

figpv <- dfpv %>% plot_ly(labels = ~`HOUSING OCCUPANCY`, values = ~dfpv$count, sort = FALSE, direction = "counterclockwise", marker = list(line = list(width = 1, pull = 3)), hoverinfo = 'text', text = ~paste('Number of Property Values:', Numberpv), textinfo = "percent")
figpv <- figpv %>% add_pie(hole = 0.5, domain = list(x = c(0.25, 0.9), y = c(0.75, 0.6)))
property <- figpv %>% layout(title = "Residential Property Value", showlegend = TRUE, 
                             legend=list(title=list(text='Select Value')),
                             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#------------Housing Occupancy---------------------------

lbls.HOUSING = c("Owners", "Renters")
slices.HOUSING = c(6839, 2412)

housing <- plot_ly(type='pie', labels=lbls.HOUSING, values=slices.HOUSING, 
                   textinfo='label+percent',
                   insidetextorientation='radial') %>% layout(title ='', legend=list(title=list(text='Occupants')))

#-------------Commuter Time------------------------------

labelsCT = c("Less than 10 minutes","10 to 14 minutes","15 to 19 minutes","20 to 24 minutes", "25 to 29 minutes", "30 to 34 minutes", "35 to 44 minutes", "45 to 59 minutes", "60 or more minutes")
valuesCT = c(6, 18, 17, 13, 5, 13, 7, 10, 11)
valuestotalCT = c(989,2968,2787,2177,792,2160,1154,1649,1813.9)

commutertime <- plot_ly(type='funnelarea', labels=labelsCT, values=valuesCT, sort = FALSE, direction = "",
                        textinfo='percent',
                        hoverinfo = 'text', text = ~paste('Total Number of Commuters:', valuestotalCT),
                        insidetextorientation='radial') %>% layout(title ='Commuter Time to Work', showlegend=TRUE, legend=list(title=list(text='Select Value')), legend=list(x=1, y=0.5))

#-----------------Commuter mode--------------------------
my_colors <- c("#CA001B", "#1D28B0", "#D71DA4", "#00A3AD", "#FF8200", "#753BBD", "#00B5E2", "#008578", "#EB6FBD", "#FE5000", "#6CC24A", "#D9D9D6", "#AD0C27", "#950078")
#subset_sterling$`...2` <- gsub(",", "", subset_sterling$`...2`)
slices.Races <- c(75.0, 14.9, 1.8, 0.5, 3.5, 4.3)
lbls.Races <- c("Drove Alone 75.0%", "Carpooled 14.9%", "Public Transport 1.8%", "Walked 0.4%","Other means 3.5%","Worked from home 4.3%")
pie(slices.Races, labels = lbls.Races, main="", sub = "Source: DP03 ACS data 2016-2020")

library(plotly)
labelsR = c("Drove Alone", "Carpooled", "Public Transport", "Walked","Other means","Worked from home")
valuesR = c(12922, 2574, 308, 77, 609, 741)
perc <- round(valuesR / sum(valuesR)*100, 1)
commutermode <- plot_ly(type='pie', labels=~labelsR, values=~valuesR, hoverinfo = "none", 
                text = ~paste0(labelsR, "\n", perc, "%"), 
                textinfo='text') %>% layout(title ='', legend=list(title=list(text='')), hoverinfo = "none")
#------------------poverty-------------------------------

poverty_as<- read_excel(paste0(getwd(),"/data/povertybyageandsexnewss.xlsx"), 
                        sheet = "Data")

subset_poverty_as <- poverty_as[3:28, 1:4]
subset_poverty_as
#subset_poverty_as$Estimate[1:26]
povas_pop <- subset_poverty_as$Estimate
povas_pop <- as.numeric(povas_pop)
povas_cat <- subset_poverty_as$Label

Total <- povas_pop

cat <- as.character(povas_cat)

pov <- plot_ly(subset_poverty_as, x = ~cat, y = ~Total, color = ~Sex, type = "bar", hoverinfo = "text",text = ~paste("Age:",cat,"<br>","Total:",Total,"<br>","Sex:",Sex)) %>% layout(title = "Poverty by Age and Sex",xaxis = list(title="",barmode = "group", categoryorder = "array", categoryarray = ~cat))
#--------gender by school-------------------------------------------------


genders <- data.frame(Sex=rep(c("Male", "Female"), each=6),
                      School=c("Sugarland","Rolling Ridge","Guilford","Sterling","Sully","Forest Grove"),
                      Total=c(268, 273, 278, 237,221, 282, 255, 259, 272, 200, 217, 278),
                      Percentage = c(51.2, 51.3, 50.5, 54.2, 50.5, 50.4, 48.8, 48.7, 49.5, 45.8, 49.5, 49.6)
                      )



genders<- ggplot(data=genders, aes(x=School, y=Total, fill=Sex,  width=0.9)) +
  geom_bar(stat="identity", position="stack", hoverinfo = "text", aes(text = paste("Percentage :",Percentage,"%\n", "Total :", Total))) +
  scale_fill_manual(values = c('#F56D4F', "#20AFCC")) + labs(y="Total Students", x="", fill="")+ggtitle("Gender by Schools") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

genders <-ggplotly(genders, tooltip = c("text"))
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
sterling <- read_excel(paste0(getwd(),"/data/Employmentsterling.xlsx"),skip=2,col_names=TRUE)
subset_sterling <- sterling[(4:5), c(1:2,4)]
employed <- as.numeric(sub("%", "", subset_sterling[1, "...4"]))
unemployed <- as.numeric(sub("%", "", subset_sterling[2, "...4"]))
subset_sterling[, "...4"] <- c(employed, unemployed)


labor <- ggplot(subset_sterling, aes(x = `EMPLOYMENT STATUS`, y = ...4, fill = `EMPLOYMENT STATUS`)) + 
  
  geom_bar(position = "stack", stat="identity", width = 0.25, aes(text = paste0("Percentage: ",...4, "%\n", "Total: ",...2))) + 
  
  labs(x = "", y= "Percentage", caption = " Source : DP03 ACS 5-yr data 2016-2020", title = "Employment Status")+ guides(fill = guide_legend(title = ""))+ scale_y_continuous(limits = c(0,85))
employment <- ggplotly(labor, tooltip = c("text"))


#-------------------work occupation----------------------

subset_sterling <- sterling[c(29:33),]
subset_sterling$`EMPLOYMENT STATUS` <- gsub(" occupations", "", subset_sterling$`EMPLOYMENT STATUS`)
subset_sterling$`...2` <- gsub(",", "", subset_sterling$`...2`)
percNum <- c()
for(i in 1:5){
  percNum <- c(percNum, as.numeric(sub("%", "", subset_sterling[i, "...4"])))
}
subset_sterling[, "...4"] <- percNum
occupation <- ggplot(subset_sterling, aes(x = reorder(`EMPLOYMENT STATUS`,...4), y = (...4), fill = `EMPLOYMENT STATUS`)) + geom_bar(position = "stack", stat="identity", width = 0.5, aes(text = paste0("Percentage: ",...4, "%\n", "Total: ", ...2)))+ theme(axis.text.x = element_text(angle=0), legend.position="none") + labs(x = "", y = "Percentages", caption = " Source : DP03 ACS 5-yr data 2016-2020", title = "Employment by Sector") + coord_flip()

occuplot <- ggplotly(occupation, tooltip = c("text"))


#------------------education-------------------------

#sterling <- read_excel(paste0(getwd(),"/data/Loudouncountyeducation.xlsx"),skip=2,col_names=TRUE)
df2 <- data.frame(
  levels=c("Less than 9th grade",
           "9th to 12th grade, no diploma","High School graduate",
           "Some college or no degree","Associate's degree",
           "Bachelor's Degree",
           "Graduate or professional"),
  Total=c(2193, 1943, 4050, 3094, 1396, 4706, 2402))

df2$levels <- factor(df2$levels, levels = df2$levels)
p<-ggplot(df2,aes(levels, Total, )) + geom_col(fill = "skyblue") + theme(axis.text.x = element_text(angle=0)) +labs(x = "", y = "Total Population", caption = " Source : S1501 ACS 5-yr data 2016-2020", title = "Population 25 and Over Educational Attainment by Degree") + coord_flip()

education <- ggplotly(p, tooltip = c("", "Total"))
#---------------------health insurance----------------------------


sterling <- read_excel(paste0(getwd(),"/data/Employmentsterling.xlsx"),skip=2,col_names=TRUE)
subset_sterling <- sterling[c(103:105),]
subset_sterling$...4 <- as.numeric(gsub("%", "", subset_sterling$...4) )
subset_sterling$`EMPLOYMENT STATUS` <- reorder(subset_sterling$`EMPLOYMENT STATUS`, subset_sterling$...4)
health <- ggplot(subset_sterling, aes(x =`EMPLOYMENT STATUS`,y = (subset_sterling$...4), fill = `EMPLOYMENT STATUS`)) + geom_bar(position = "stack", stat="identity", width = 0.5, aes(text = paste0(...4, "%"))) + labs(x = "Insurance Type", y= "Percentage", caption = " Source : DP03 ACS 5 -yr data 2016-2020", titlel = "Distribution of Health Insurance") + guides(fill = guide_legend(title = ""))+ theme(axis.text.y = element_text(angle=0), axis.ticks.y= element_blank())+ coord_flip()#


healthin <- ggplotly(health, tooltip = c("text"))


#-------------------------------race by school ----------------------------

races <- read_excel(paste0(getwd(),"/data/racedems.xlsx"))

race_subset <- races[(1:153),c(1,5,6,7)]
School <- race_subset$`School Name`
raceS <- race_subset$Race
total <- race_subset$`Full Time Count (All Grades)`
raceehtn <- ggplot(race_subset, aes(x=raceS,y=total,fill=School))+ geom_col()+labs(x="Race/Ethnicity",y="Number of Students",caption = "Source: VDOE Fall Membership Report 2016-2020") + theme(plot.caption.position = "plot",plot.caption = element_text(hjust = 1),axis.text.x=element_text(angle=90)) + scale_fill_brewer(palette = "Set1") + scale_fill_discrete(name = "")
raceehtn<-ggplotly(raceehtn)

#-----------hispanic population-----------------

map$Longitude <- as.numeric(map$Longitude)
map$Latitude <- as.numeric(map$Latitude)

total <- merge(va20_2,map)

#specify the bin breaks
mybins <- c(0,300,600,900,1200,1500,1800,2100)
#specify the default color
mypalette <- colorBin(palette="inferno", domain=va20_2$estimate, na.color="transparent", bins=mybins)

hispanicschool <- leaflet(data = total) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~mypalette(va20_2$`estimate`),
    stroke=TRUE,
    weight = 1,
    smoothFactor = 0.2,
    opacity = 1.0,
    fillOpacity = 0.7, 
    label=paste("County: ",va20_2$GEOID, ", Value: ",va20_2$estimate),
    highlightOptions = highlightOptions(color = "white",
                                        weight = 2,
                                        bringToFront = TRUE)) %>%
  addLegend(pal=mypalette, position = "bottomright",
            values = ~va20_2$estimate,
            opacity = 0.5, title = "Hispanic Population") %>%
  addMarkers( ~Longitude, ~Latitude, popup = ~as.character(Address), label = ~as.character(School), labelOptions = TRUE)
#-----------enrollment-----------------

enrollment <- read_excel(paste0(getwd(),"/data/Enrollment16-20.xlsx"))
enr_total <- enrollment$Total
School <- enrollment$Schools
Year <- enrollment$Year
enroll <- plot_ly(enrollment, x = ~Year,y = ~Total, color = ~School, type = 'scatter',mode = 'lines', hoverinfo="text", text = ~paste("Total:", Total)) %>% layout(title= " Total Enrollment by Schools", xaxis = list(title = ""))

#-------------------attendance --------------

att_per <- attendance$`Absence Rate`
Percent <- att_per*100
Quarter <- attendance$`School Quarter`
School <- attendance$`School Name`
ggplot(attendance,aes(x=quarter,y=att_rate,group=School,color=School))+geom_point()+geom_line() +labs(title = "Student Absences by 2020-2021 Quarter",caption= "Source: LCPS Dashboard 2021-2022",x="Quarter",y="Percentage") + theme(plot.caption.position = "plot",
                                                                                                                                                                                                                                      plot.caption = element_text(hjust = 1)) + scale_fill_brewer(palette = "Set1")
attend <- plot_ly(attendance,x = ~Quarter, y = ~Percent, color  = ~School, type = 'scatter',mode = 'lines',hoverinfo = "text",text = ~paste("School:",School,"<br>","Percent:",Percent)) %>% layout(title = "Student Absences by 2020-2021 Quarter",xaxis = list(title = ""))
#---------------Number of Teachers/Staff--------------------------

Schools <- c("Sterling", "Sugarland", "Rolling Ridge", "Forest Grove", "Guilford", "Sully")
Teachers <- c(32, 52, 66, 55, 59, 35)
Staff <- c(49, 22, 27, 20, 29, 19)
dataSTAFF <- data.frame(Schools, Teachers, Staff)

figSTM <- plot_ly(dataSTAFF, x = ~Schools, y = ~Teachers, type = 'bar', name = 'Teachers', marker = list(color = 'rgb(255, 2, 2 )'), textinfo = "total",
                  hoverinfo = 'text')
#text = ~paste('Total:',))

figSTM <- figSTM %>% add_trace(y = ~Staff, name = 'Staff', marker = list(color = 'rgb(253, 151, 12 )'))
cteacher <- figSTM %>% layout(title = "Teachers/Staff by Schools", yaxis = list(title = 'Total Educators'), xaxis = list(title = ''), barmode = 'stack')

#--------Chronic absenteeism------------------

chronic <- data.frame(sex=rep(c("Missed less than 10%"), each=6),
                      School=c("Sugarland","Rolling Ridge","Guilford","Sterling","Sully","Forest Grove"),
                      Percent=c(11.1, 10.1, 6.7, 5.8, 9.7,7.9))

chronic<- ggplot(data=chronic, aes(x=School, y=Percent, fill=School,  width=0.8)) +
  geom_bar(stat="identity",hoverinfo = "text", aes(text = paste("School :",School,"\n", "Percent :", Percent, "%")))  + labs(y="", x="", fill="")+ggtitle("Chronic Absenteeism by Schools") 

chronic<-ggplotly(chronic, tooltip = c("text"))

#--------------free or not free resources ---------------------------------------

costs <- read_excel(paste0(getwd(),"/data/resourcecost.xlsx"))
foods <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"),sheet="Food")
clothes <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"),sheet = "Clothing")
counseling <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"), sheet = "Counseling")
dental <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"), sheet = "Dental Care")
vision <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"), sheet = "Vision Care")
medical <-  read_excel(paste0(getwd(),"/data/healthsep.xlsx"), sheet = "Medical Services")
speech <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"), sheet = "Speech and Hearing")
physical <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"), sheet = "Physical Therapy")
#---------------map_health and isochrones-----------------------------------------

YourAPIKey <- "73ad15c60d8fe57014b574b4fc428ec0"
YourAppId <- "afe3f1af"

traveltime10 <- traveltime_map(appId=YourAppId,
                               apiKey=YourAPIKey,
                               location=c(39.009006,-77.4029155),
                               traveltime=600,
                               type="driving",
                               departure="2022-08-09T08:00:00+01:00")
# ... and within 60 minutes?
traveltime20 <- traveltime_map(appId=YourAppId,
                               apiKey=YourAPIKey,
                               location=c(39.009006,-77.4029155),
                               traveltime=1200,
                               type="driving",
                               departure="2022-08-09T08:00:00+01:00")
traveltime45 <- traveltime_map(appId = YourAppId,
                               apiKey = YourAPIKey,
                               location = c(39.009006,-77.4029155),
                               traveltime= 2700,
                               type = "driving",
                               departure = "2022-08-09T08:00:00+01:00")
map<- read_excel(paste0(getwd(),"/data/school_locations.xlsx"))

subset_map <- map[1,c(1,4,5)]

healthsep <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"))
popups <- lapply(
  paste("<strong>Name: </strong>",
        str_to_title(healthsep$Name),
        "<br />",
        "<strong>Description:</strong>",
        healthsep$Description ,
        "<br />",
        "<strong>Serves:</strong>",
        healthsep$Serves, 
        "<br />",
        "<strong>Hours:</strong>",
        healthsep$Hours,
        "<br />",
        "<strong>Language:</strong>",
        healthsep$Language,
        "<br />",
        "<strong>Address:</strong>",
        healthsep$Address,
        "<a href = ",healthsep$Website, "> Website </a>",
        "<br />"),
  
  htmltools::HTML
)

foods <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"),sheet="Food")
clothes <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"),sheet = "Clothing")
counseling <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"), sheet = "Counseling")
dental <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"), sheet = "Dental Care")
vision <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"), sheet = "Vision Care")
medical <-  read_excel(paste0(getwd(),"/data/healthsep.xlsx"), sheet = "Medical Services")
speech <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"), sheet = "Speech and Hearing")
physical <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"), sheet = "Physical Therapy")


blocks<-c("Block Group 1, Census Tract 6112.05, Loudoun County, Virginia", "Block Group 2, Census Tract 6112.05, Loudoun County, Virginia", "Block Group 2, Census Tract 6112.04, Loudoun County, Virginia", "Block Group 2, Census Tract 6115.02, Loudoun County, Virginia","Block Group 3, Census Tract 6115.02, Loudoun County, Virginia", "Block Group 1, Census Tract 6113, Loudoun County, Virginia","Block Group 2, Census Tract 6113, Loudoun County, Virginia","Block Group 3, Census Tract 6113, Loudoun County, Virginia", "Block Group 1, Census Tract 6114, Loudoun County, Virginia","Block Group 2, Census Tract 6114, Loudoun County, Virginia","Block Group 3, Census Tract 6114, Loudoun County, Virginia","Block Group 1, Census Tract 6117.01, Loudoun County, Virginia","Block Group 2, Census Tract 6117.01, Loudoun County, Virginia", "Block Group 1, Census Tract 6116.02, Loudoun County, Virginia","Block Group 2, Census Tract 6116.02, Loudoun County, Virginia","Block Group 1, Census Tract 6116.01, Loudoun County, Virginia", "Block Group 2, Census Tract 6116.01, Loudoun County, Virginia")
va20_2 <- get_acs(geography = "block group",
                  variables = c(hispanic = "B03002_012"),
                  state = "VA",
                  year = 2020,
                  geometry = TRUE) %>%
  filter(NAME %in% blocks)
leaflet(data = foods) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = va20_2,
              color="#5f308f",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% 
  addPolygons(data=traveltime20, color= "#21618C",opacity = 1,weight=2,fillColor = "white", fillOpacity = .1) %>% addPolygons(data=traveltime10,color="green",opacity=1,weight=2,fillColor = "white",fillOpacity = .1) %>%     addPolygons(data=traveltime45,color="#D98880",opacity = 1,weight = 2,fillColor = "white",fillOpacity = .1) %>%
  setView(-77.4029155,39.009006, zoom = 11)%>%
  addCircleMarkers(data=foods,~Longitude,~Latitude,popup=~popups,label=~as.character(Name),color="#2e850c",group = "Food",weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1)%>%
  addCircleMarkers(data=clothes,~Longitude,~Latitude,popup=~popups,label=~as.character(Name),color=         "#0a31a5",group="Clothing",weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1)  %>%
  addCircleMarkers(data=counseling,~Longitude,~Latitude,popup =~popups,label=~as.character(Name),color="#ec1c1c",group = "Counseling", weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1) %>%
  addCircleMarkers(data=dental,~Longitude,~Latitude,popup = ~popups,label=~as.character(Name),color="#ffd600",group = "Medical Services", weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1) %>%
  addCircleMarkers(data=vision,~Longitude,~Latitude,popup =~popups,label=~as.character(Name),color="#ffd600",group = "Medical Services", weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1) %>%
  addCircleMarkers(data=medical,~Longitude,~Latitude,popup = ~popups,label=~as.character(Name),color="#ffd600",group = "Medical Services", weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1) %>%
  addCircleMarkers(data=speech,~Longitude,~Latitude,popup =~popups,label=~as.character(Name),color="#ffd600",group = "Medical Services", weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1) %>%
  addCircleMarkers(data=physical,~Longitude,~Latitude,popup = ~popups,label=~as.character(Name),color="#ffd600",group = "Medical Services", weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1) %>%
  addLayersControl(overlayGroups = c("Food", "Clothing", "Counseling","Medical Services"),options = layersControlOptions(collapsed = FALSE)) %>% addMarkers(data=subset_map,~Longitude,~Latitude,popup = ~as.character(School)) %>% addLegend("bottomright",colors=c("green","#21618C","#D98880"),labels=c("10 minutes","20 minutes","45 minutes"),title = "Travel Time") -> map_health



#--------------youth development map ----------------

youth <- read_excel(paste0(getwd(),"/data/Sterling_Youth_Development 3.xlsx"))
popups <- lapply(
  paste("<strong>Name: </strong>",
        str_to_title(youth$Name),
        "<br />",
        "<strong>Description:</strong>",
        youth$Description ,
        "<br />",
        "<strong>Hours:</strong>",
        youth$Hours, 
        "<br />",
        "<strong>Address:</strong>",
        youth$Address,
        "<a href = ",youth$Website, "> Website </a>",
        "<br />"),
  
  
  htmltools::HTML
)

pal <- colorFactor(c("red","blue","green","orange","purple"),domain = c("Activity","Athletics","Resource","Club","After School Program"))

leaflet(data = youth) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = va20_2,
              color="#5f308f",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% 
  addPolygons(data=traveltime20, color= "#21618C",opacity = 1,weight=2,fillColor = "white", fillOpacity = .1) %>% addPolygons(data=traveltime10,color="green",opacity=1,weight=2,fillColor = "white",fillOpacity = .1) %>%     addPolygons(data=traveltime45,color="#D98880",opacity = 1,weight = 2,fillColor = "white",fillOpacity = .1) %>%
  setView(-77.4029155,39.009006, zoom = 11)%>%
  addCircleMarkers(data=youth,~Longitude,~Latitude,popup=~popups,label=~as.character(Name),color= ~pal(Type),weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1,group = ~Type)%>%
  addLayersControl(overlayGroups = ~Type,options= layersControlOptions(collapsed = FALSE)) %>%
  addMarkers(data=subset_map,~Longitude,~Latitude,popup = ~as.character(School)) %>% addLegend("bottomright",colors=c("green","#21618C","#D98880"),labels=c("10 minutes","20 minutes","45 minutes"),title = "Travel Time") -> map_youth


#---------mental health resources map------------------------

ment <- read_excel(paste0(getwd(),"/data/mentalhealthres.xlsx"),sheet = "Mental")

popups <- lapply(
  paste("<strong>Name: </strong>",
        str_to_title(ment$Name),
        "<br />",
        "<strong>Description:</strong>",
        ment$Description ,
        "<br />",
        "<strong>Hours:</strong>",
        ment$Hours, 
        "<br />",
        "<strong>Address:</strong>",
        ment$Address,
        "<a href = ",ment$Website, "> Website </a>",
        "<br />"),
  
  
  htmltools::HTML
)

pal <- colorFactor(c("red", "blue", "green"), domain = c("Family Therapy", "Family Counseling", "Bereavement"))

leaflet(data = ment) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = va20_2,
              color="#5f308f",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% 
  addPolygons(data=traveltime20, color= "#21618C",opacity = 1,weight=2,fillColor = "white", fillOpacity = .1) %>% addPolygons(data=traveltime10,color="green",opacity=1,weight=2,fillColor = "white",fillOpacity = .1) %>%     addPolygons(data=traveltime45,color="#D98880",opacity = 1,weight = 2,fillColor = "white",fillOpacity = .1) %>%
  setView(-77.4029155,39.009006, zoom = 11)%>%
  addCircleMarkers(data=ment,~Longitude,~Latitude,popup=~popups,label=~as.character(Name),group=~Resources,color=~pal(Resources),weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1)%>%
  addLayersControl(overlayGroups = ~Resources,options = layersControlOptions(collapsed = FALSE)) %>% 
  addMarkers(data=subset_map,~Longitude,~Latitude,popup = ~as.character(School)) %>% addLegend("bottomright",colors=c("green","#21618C","#D98880"),labels=c("10 minutes","20 minutes","45 minutes"),title = "Travel Time") -> map_mental



#-------------word clouds--------------------
#------------cloud_1-------------------------

#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


text1 <- "Internet needs were very overwhelming in our community, virtual presence of students created difficulties with individual telecounseling, student engagement in school an ongoing concern time to provide for the basic needs; finding mental health providers for elementary aged students (this was a huge challenge); medical care for undocumented and uninsured Parent involvement internet access; monetary stressors - rent, food, hygiene and cleanliness supplies needs and health (COVID) and mental health needs Maintaining the same level of connectedness with families in the DL model as those attending in person.parent engagement, parent attendance, finding medical care for undocumented and uninsured, finding housing assistance for undocumented Our challenge is the overall transiency of our student population turnover; community mental health and medical supports; undocumented and uninsured medical care parent involvement Youth development activities We would like to continue to diversify our community partner list community mental health and medical supports, undocumented and uninsured medical care"

#docs <- Corpus(VectorSource(text))


Clean_String <- function(string){
  # Lowercase
  temp <- tolower(string)
  # Remove everything that is not a number or letter (may want to keep more 
  # stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  temp <- stringr::str_replace_all(temp,";", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}

clean1 <- Clean_String(text1)

docs1 <- Corpus(VectorSource(clean1))

docs1 <- tm_map(docs1, removeWords, c("to", "in", "and", "the", "we", "of", "an", "is", "like", "for", "those", "were", "was", "list", "our", "with", "would", "very", "huge","this","same","ongoing","overall", "finding", "continue"))


dtm1 <- TermDocumentMatrix(docs1) 
matrix1 <- as.matrix(dtm1) 
words1 <- sort(rowSums(matrix1),decreasing=TRUE) 
df1 <- data.frame(word = names(words1),freq=words1)



set.seed(1234) # for reproducibility 
cloud1 <- wordcloud2(df1, size=0.5)

#---------Cloud 2-----------------------

text2 <- "Many families attended our family outreach and engagement initiatives, teaching families how to operate Schoology.  Families are now very adept at using technology to help their students and support teachers and instruction
student attendance and active engagement; virtual home visits
Reduction of gaggle reports
Virtual Home Visits and United Mental Health screening; hot spots, 1-1 technology
Staying connected with families and working to empower families to be active in the school community. We offered support to families throughout the year and were responsive to needs as they arose. We worked together collaboratively.
We have the Principal of the Year, School is a warm and welcoming place - Principal and VP tell students every day that they are loved and wanted, staff have risen to the occasion to offer quality instruction in this challenging time Excellent teachers and supportive and caring administration, cohesive working environment. positive climate, PEP, Parent Liaison always actively engaged and helping all families with many needs
family involvement; welcoming environment
Students feel safe and supported at school, equity is promoted throughout the school building, teamwork among staff 
UMHT initiatives (MTSS tiered support)
Collaboration and a warm atmosphere. Care for our community
vision, teamwork, the people who work here "

clean2 <- Clean_String(text2)

docs2 <- Corpus(VectorSource(clean2))

docs2 <- tm_map(docs2, removeWords, c("to", "in", "and", "the", "we", "of", "an", "is", "like", "for", "those", "were", "was", "list", "our", "with", "would", "very", "huge","this","same","ongoing","overall", "finding", "hot"))


dtm2 <- TermDocumentMatrix(docs2) 
matrix2 <- as.matrix(dtm2) 
words2 <- sort(rowSums(matrix2),decreasing=TRUE) 
df2 <- data.frame(word = names(words2),freq=words2)


cloud2 <- wordcloud2(df2, size=0.5)

#---------------cloud3------------------

text3 <- "Continue working with our excellent business partners and community agencies.  Utilize the support of many agencies and local people interested in supporting our students and families.   Provide in-person assistance to families and students
seek resources for the areas listed above; streamline supports for the most needy
Increase parent involvement, more youth development opportunities, and resuming after school clubs and programs 
Continuation of services provided pre-COVID and expansion of Youth Development Activities
I would like to increase opportunities for after-school youth development programs at Rolling Ridge. We also hope to continue to offer virtual opportunities in addition to our in-person programs to offer working parents flexibility. As always, we will seek feedback from families to better meet their needs.
community partnerships, return to PEP, engage parents in meaningful and timely ways, create opportunities for stakeholder input 4 times per year.  "

clean3 <- Clean_String(text3)

docs3 <- Corpus(VectorSource(clean3))

docs3 <- tm_map(docs3, removeWords, c("to", "in", "and", "the", "we", "of", "an", "is", "like", "for", "those", "were", "was", "list", "our", "with", "would", "very", "umht", "hot", "to", "in", "and", "the", "we", "of", "an", "is", "like", "for", "those", "were", "was", "list", "our", "with", "would", "very", "huge","this","same","ongoing","overall", "finding", "their", "from", "always"))


dtm3 <- TermDocumentMatrix(docs3) 
matrix3 <- as.matrix(dtm3) 
words3 <- sort(rowSums(matrix3),decreasing=TRUE) 
df3 <- data.frame(word = names(words3),freq=words3)

cloud3<- wordcloud2(df3, size=0.5)


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

#--------------Teacher/Staff Climate Surveys------------------------
newsurveydata <- read_excel(paste0(getwd(), "/data/NewSurveyData.xlsx"),skip=0,col_names=TRUE)
subsetnewsurveydataSTAFF <- newsurveydata[3:8,c(1,2:8)]

#Teacher & Staff Survey Q1 - Staff Collegiality
staffquestion1 <- subsetnewsurveydataSTAFF[1:6,1:2]
question1 <- staffquestion1$SCHOOLS
staffquestion1percentage <- staffquestion1$`Question 1`
staffquestion1percentage <- as.numeric(staffquestion1percentage)
staffquestion1percentage <- staffquestion1percentage*100
one <- ggplot(staffquestion1,aes(x=question1,y=staffquestion1percentage,fill=question1)) +geom_col()+labs(title="Staff Collegiality",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion1percentage, y = staffquestion1percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(one)

#Teacher & Staff Survey Q2 - Academic Environment
staffquestion2 <- subsetnewsurveydataSTAFF[1:6,c(1,3)]
question2 <- staffquestion2$SCHOOLS
staffquestion2percentage <- staffquestion2$`Question 2`
staffquestion2percentage <- as.numeric(staffquestion2percentage)
staffquestion2percentage <- staffquestion2percentage*100
two <- ggplot(staffquestion2,aes(x=question2,y=staffquestion2percentage,fill=question2)) +geom_col()+labs(title="Academic Environment",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion2percentage, y = staffquestion2percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(two)

#Teacher & Staff Survey Q3 - School Leadership
staffquestion3 <- subsetnewsurveydataSTAFF[1:6,c(1,4)]
question3 <- staffquestion3$SCHOOLS
staffquestion3percentage <- staffquestion3$`Question 3`
staffquestion3percentage <- as.numeric(staffquestion3percentage)
staffquestion3percentage <- staffquestion3percentage*100
three <- ggplot(staffquestion3,aes(x=question3,y=staffquestion3percentage,fill=question3)) +geom_col()+labs(title="School Leadership",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion3percentage, y = staffquestion3percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(three)

#Teacher & Staff Survey Q4 - Managing Student Behavior
staffquestion4 <- subsetnewsurveydataSTAFF[1:6,c(1,5)]
question4 <- staffquestion4$SCHOOLS
staffquestion4percentage <- staffquestion4$`Question 4`
staffquestion4percentage <- as.numeric(staffquestion4percentage)
staffquestion4percentage <- staffquestion4percentage*100
four <- ggplot(staffquestion4,aes(x=question4,y=staffquestion4percentage,fill=question4)) +geom_col()+labs(title="Managing Student Behavior",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion4percentage, y = staffquestion4percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(four)

#Teacher & Staff Survey Q5 - School Parent Communication
staffquestion5 <- subsetnewsurveydataSTAFF[1:6,c(1,6)]
question5 <- staffquestion5$SCHOOLS
staffquestion5percentage <- staffquestion5$`Question 5`
staffquestion5percentage <- as.numeric(staffquestion5percentage)
staffquestion5percentage <- staffquestion5percentage*100
five <- ggplot(staffquestion5,aes(x=question5,y=staffquestion5percentage,fill=question5)) +geom_col()+labs(title="School Parent Communication",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion5percentage, y = staffquestion5percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(five)

#Teacher & Staff Survey Q6 - Workplace Environment
staffquestion6 <- subsetnewsurveydataSTAFF[1:6,c(1,7)]
question6 <- staffquestion6$SCHOOLS
staffquestion6percentage <- staffquestion6$`Question 6`
staffquestion6percentage <- as.numeric(staffquestion6percentage)
staffquestion6percentage <- staffquestion6percentage*100
six <- ggplot(staffquestion6,aes(x=question6,y=staffquestion6percentage,fill=question6)) +geom_col()+labs(title="Workplace Environment",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion6percentage, y = staffquestion6percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(six)

#Teacher & Staff Survey Q7 - Instructional Practices 
staffquestion7 <- subsetnewsurveydataSTAFF[1:6,c(1,8)]
question7 <- staffquestion7$SCHOOLS
staffquestion7percentage <- staffquestion7$`Question 7`
staffquestion7percentage <- as.numeric(staffquestion7percentage)
staffquestion7percentage <- staffquestion7percentage*100
seven <- ggplot(staffquestion7,aes(x=question7,y=staffquestion7percentage,fill=question7)) +geom_col()+labs(title="Instructional Practices",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion7percentage, y = staffquestion7percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(seven)

#--------------Parent Climate Surveys--------------------------------
newsurveydata <- read_excel(paste0(getwd(), "/data/NewSurveyData.xlsx"),skip=0,col_names=TRUE)
subsetnewsurveydataPARENT <- newsurveydata[19:24,c(1,2:5)]

#Parent Survey Q1 - Academic Support 
parentquestion1 <- subsetnewsurveydataPARENT[1:6,1:2]
question8 <- parentquestion1$SCHOOLS
parentquestion1percentage <- parentquestion1$`Question 1`
parentquestion1percentage <- as.numeric(parentquestion1percentage)
parentquestion1percentage <- parentquestion1percentage*100
eight <- ggplot(parentquestion1,aes(x=question8,y=parentquestion1percentage,fill=question8)) +geom_col()+labs(title="Academic Support",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = parentquestion1percentage, y = parentquestion1percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(eight)

#Parent Survey Q2 - Communications
parentquestion2 <- subsetnewsurveydataPARENT[1:6,c(1,3)]
question9 <- parentquestion2$SCHOOLS
parentquestion2percentage <- parentquestion2$`Question 2`
parentquestion2percentage <- as.numeric(parentquestion2percentage)
parentquestion2percentage <- parentquestion2percentage*100
nine <- ggplot(parentquestion2,aes(x=question9,y=parentquestion2percentage,fill=question9)) +geom_col()+labs(title="Communications",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = parentquestion2percentage, y = parentquestion2percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(nine)

#Parent Survey Q3 - Relationships
parentquestion3 <- subsetnewsurveydataPARENT[1:6,c(1,4)]
question10 <- parentquestion3$SCHOOLS
parentquestion3percentage <- parentquestion3$`Question 3`
parentquestion3percentage <- as.numeric(parentquestion3percentage)
parentquestion3percentage <- parentquestion3percentage*100
ten <- ggplot(parentquestion3,aes(x=question10,y=parentquestion3percentage,fill=question10)) +geom_col()+labs(title="Relationships",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = parentquestion3percentage, y = parentquestion3percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(ten)

#Parent Survey Q4 - Instructions
parentquestion4 <- subsetnewsurveydataPARENT[1:6,c(1,5)]
question11 <- parentquestion4$SCHOOLS
parentquestion4percentage <- parentquestion4$`Question 4`
parentquestion4percentage <- as.numeric(parentquestion4percentage)
parentquestion4percentage <- parentquestion4percentage*100
eleven <- ggplot(parentquestion4,aes(x=question11,y=parentquestion4percentage,fill=question11)) +geom_col()+labs(title="Instructions",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = parentquestion4percentage, y = parentquestion4percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(eleven)

#--------------Student Climate Surveys-------------------------------
newsurveydata <- read_excel(paste0(getwd(), "/data/NewSurveyData.xlsx"),skip=0,col_names=TRUE)
subsetnewsurveydataSTUDENT <- newsurveydata[11:16,c(1,2:6)]

#Student Survey Q1 - Student Engagement
studentquestion1 <- subsetnewsurveydataSTUDENT[1:6,1:2]
question12 <- studentquestion1$SCHOOLS
studentquestion1percentage <- studentquestion1$`Question 1`
studentquestion1percentage <- as.numeric(studentquestion1percentage)
studentquestion1percentage <- studentquestion1percentage*100
twelve <- ggplot(studentquestion1,aes(x=question12,y=studentquestion1percentage,fill=question12)) +geom_col()+labs(title="Student Engagement",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = studentquestion1percentage, y = studentquestion1percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(twelve)

#Student Survey Q2 - Teacher Relationship
studentquestion2 <- subsetnewsurveydataSTUDENT[1:6,c(1,3)]
question13 <- studentquestion2$SCHOOLS
studentquestion2percentage <- studentquestion2$`Question 2`
studentquestion2percentage <- as.numeric(studentquestion2percentage)
studentquestion2percentage <- studentquestion2percentage*100
thirteen <- ggplot(studentquestion2,aes(x=question13,y=studentquestion2percentage,fill=question13)) +geom_col()+labs(title="Teacher Relationship",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = studentquestion2percentage, y = studentquestion2percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(thirteen)

#Student Survey Q3 - Social-Emotional Wellbeing
studentquestion3 <- subsetnewsurveydataSTUDENT[1:6,c(1,4)]
question14 <- studentquestion3$SCHOOLS
studentquestion3percentage <- studentquestion3$`Question 3`
studentquestion3percentage <- as.numeric(studentquestion3percentage)
studentquestion3percentage <- studentquestion3percentage*100
fourteen <- ggplot(studentquestion3,aes(x=question14,y=studentquestion3percentage,fill=question14)) +geom_col()+labs(title="Social-Emotional Wellbeing",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = studentquestion3percentage, y = studentquestion3percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(fourteen)

#Student Survey Q4 - Student Behavior
studentquestion4 <- subsetnewsurveydataSTUDENT[1:6,c(1,5)]
question15 <- studentquestion4$SCHOOLS
studentquestion4percentage <- studentquestion4$`Question 4`
studentquestion4percentage <- as.numeric(studentquestion4percentage)
studentquestion4percentage <- studentquestion4percentage*100
fifteen <- ggplot(studentquestion4,aes(x=question15,y=studentquestion4percentage,fill=question15)) +geom_col()+labs(title="Student Behavior",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = studentquestion4percentage, y = studentquestion4percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(fifteen)

#Student Survey Q5 - Bullying
studentquestion5 <- subsetnewsurveydataSTUDENT[1:6,c(1,6)]
question16 <- studentquestion5$SCHOOLS
studentquestion5percentage <- studentquestion5$`Question 5`
studentquestion5percentage <- as.numeric(studentquestion5percentage)
studentquestion5percentage <- studentquestion5percentage*100
sixteen <- ggplot(studentquestion5,aes(x=question16,y=studentquestion5percentage,fill=question16)) +geom_col()+labs(title="Bullying",x="",y="Percent") + scale_fill_discrete(name = "") + geom_text(aes(label = studentquestion5percentage, y = studentquestion5percentage), size = 3, position = position_stack(vjust = 1.02))
ggplotly(sixteen)

# user -------------------------------------------------------------
ui <- navbarPage(title = "DSPG",
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
                                   h1(strong("Illustrating Potential Opportunities for Community Schools in Loudoun County"),
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
                                          h2(strong("The Setting")),align = "justify",
                                          
                                          p("                     
The Community schools are centers for neighborhood enrichment, uniting families, educators and community partners to provide world class education, enrichment, health services, and other opportunities to succeed in school and life. Loudoun County atarted the community school initiative back in 2016. The first community school was Sterling Elementary School. Throughout the past several years, there's been a big community effort to build key connections in Loudoun County’s six Title 1 elementary schools, which serve under-resourced neighborhoods in Sterling. It’s called the Community Schools Initiative and its goal is to provide additional resources for Sugarland, Sterling, Rolling Ridge, Sully, Guilford, and Forest Grove elementary schools.
"),
                                          #p("During the 2018 – 2019 school year, the Community school model provided the families with clothes, shoes, and other basic supplies 538 times; enabled 135 families to receive weekend meals throughout the school year; supported 6 academic programs for 323 students; and provided 9 after-school enrichment programs for 373 students. Funds have provided these Community Schools with additional resources, such as full-time parent liaisons, a full-time social worker, and programs that keep families engaged in their child’s education. The Community Schools initiative focuses on bolstering these schools in six areas: academies, health and social services, youth and community engagement, building stronger families, and healthier communities."),
                                          
                                   ),
                                   column(4,
                                          h2(strong("Project Background")), align = "justify",
                                          
                                          p("During the 2018 – 2019 school year, the Community school model provided the families with clothes, shoes, and other basic supplies 538 times; enabled 135 families to receive weekend meals throughout the school year; supported 6 academic programs for 323 students; and provided 9 after-school enrichment programs for 373 students. Funds have provided these Community Schools with additional resources, such as full-time parent liaisons, a full-time social worker, and programs that keep families engaged in their child’s education. The Community Schools initiative focuses on bolstering these schools in six areas: academies, health and social services, youth and community engagement, building stronger families, and healthier communities."),
                                   ),
                                   
                                   column(4,
                                          h2(strong("Project Goals")),
                                          p("Identifying possible opportunities in services in the Community Schools"),
                                          #p(""),
                                          #p("")
                                   )
                          ),
                          #fluidRow(align = "center",
                          # p(tags$small(em('Last updated: August 2021'))))
                 ),
                 
                 ## Sterling Area--------------------------------------------
                 tabPanel("Community School Initiative", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   p("", style = "padding-top:10px;"),
                                   column(12, align = "center", h1(strong("Sterling’s Elementary Community Schools"))),
                          ),
                          
                          fluidPage(style = "margin: 12px;",
                                    
                                    column(6, align = "justify", h3(strong("What are Community Schools?")),
                                           
                                           p("Community Schools is a term that describes schools that brings educators, families, community partners, and local government together to address the comprehensive needs of students, families, and communities. The U.S. Department of Education states that the primary purpose of community schools is to “provide comprehensive academic, social, and health services for students, student's family members, and community members that will result in improved educational outcomes for children. Community Schools are expanding across the United States – in 2020, around 10,000 schools have been transformed into community schools (Quinn & Blank, 2020). This is also evident in Virginia, where", a(href = " https://www.cisofva.org/", strong("Community Schools"), target = "_blank"), "served over 58,000 students in 2021, an increase from over 45,000 students in 2019."),
                                           
                                           h3(strong("The Sterling Region")),
                                           
                                           p("The", a(href = "https://www.lcps.org/Page/236420s",strong("Community School Initiative"), target = "_blank"), "in the Loudoun County Public School started in 2016 with Sterling Elementary, a Title 1 school, due to the generous support of 100WomenStrong. According to the Virginia Department of Education, Title 1 schools are provided “financial assistance through state educational agencies to school divisions and public schools with high numbers or percentages of children from low-income families to help ensure that all children meet challenging state academic content and achievement standards.”"),
                                           p("The Community School program in Loudoun focuses on four key areas to promote academic achievement: "),
                                           strong(tags$ol(
                                             tags$li(("Health and Social Services" ), style = "font-size:18px;"),
                                             tags$li(("Mental Health"), style = "font-size:18px;"),
                                             tags$li(("Family Engagement"), style = "font-size:18px;"),
                                             tags$li(("Youth Development Opportunities"), style = "font-size:18px;")
                                             
                                           )),
                                           p("Over the past several years, the Community School initiative has grown to include six Title 1 elementary schools from the Sterling area of Loudoun County in 2022."),
                                           p("The interactive map shows the location of the six elementary schools in Sterling. Most schools are in the Sterling Census Designated Place (shaded orange). Sugarland Elementary, however, falls in the Greater Sterling region. We estimated, shown in yellow, by selecting the respective blocks assigned by the US Census Bureau."),
                                    ),
                                    
                                    
                                    
                                    column(6, align ="right", leafletOutput("map1", width = "90%", height = 500)
                                           #fluidRow(align = "center",
                                           #    p(tags$small(em('Last updated: August 2021'))))
                                    ),
                                    
                                    
                          ),
                          
                          fluidRow(style = "margin: 12px;",
                                   column(8, h3(strong("When did schools join the Community Schools Initiative?"))),
                                   column(12, align ="center", 
                                          img(src='sterlingmascot.png', width = "60%", height = 250)
                                          
                                          
                                   ), 
                                   column(12, 
                                          h4("References: "),
                                          p("[1] U.S Department of Education, Office of Elementary and Secondary Education. Full-Service Community Schools Program (FSCS). Retrieved from:", a(href =  "https://oese.ed.gov/offices/office-of-discretionary-grants-support-services/school-choice-improvement-programs/full-service-community-schools-program-fscs/", "https://oese.ed.gov/offices/office-of-discretionary-grants-support-services/school-choice-improvement-programs/full-service-community-schools-program-fscs/"), style = "font-size:12px;"),
                                          p("[2] Quinn, J., & Blank, M. J. (2020). Twenty years, ten lessons: Community schools as an equitable school improvement strategy.", em("Voices in Urban Education (VUE)."), style = "font-size:12px;")),
                                   
                          ),
                 ), 
                 
                 
                 tabPanel("Sterling Sociodemographics",
                          fluidRow(style = "margin: 4px;",
                                   h1(strong("Sterling"), align = "center"),
                                   p("", style = "padding-top:10px;"), 
                                   column(12, 
                                          h4(strong("Sterling Residents' Characteristics")),
                                          
                                          tabsetPanel(
                                            
                                            tabPanel("Demographic",
                                                     fluidRow(style = "margin: 4px;",
                                                              p("", style = "padding-top:10px;"),
                                                              column(7, align = "left",
                                                                     selectInput("demos1drop", "Select Socioeconomic Characteristic:", width = "100%", choices = c(
                                                                       "Gender" = "gender",
                                                                       "Age" = "age",
                                                                       "Race/ethnicity" = "race"
                                                                       
                                                                     ),
                                                                     ),   
                                                                     br(""),
                                                                     withSpinner(plotlyOutput("demo1", height = "500px", width ="100%")),
                                                                     column(12, align = "right",
                                                                     p("Source: American Community 2019 5-Year Estimates", style = "font-size:12px;"),
                                                                     p("*Note: Data is nill for missing bars", style = "font-size:12px;")
                                                                     )
                                                              ),
                                                              
                                                              column(5, 
                                                                     
                                                                     p("To access the possible opportunities within the six title 1 community schools in Sterling, VA, it is important to understand the neighborhood and area in which these schools are located. By looking at the graph, it appears to be almost an equal split of male’s and females in Sterling, VA. The total population in sterling is 30,271. 15,282 members of the population are male, while the remaining 14,989 are female. There are only 293 more males than females so that gives us almost a one to one ratio - so that’s good.", style = "padding-top:15px;font-size: 14px;"),
                                                                     p("So, once the team looked at the gender distribution, we were curious to look at the age distribution of the individuals of Sterling. The largest age group in Sterling are Adults (ages 35 to 44) which is represented by the blue green block, and then the older millennials (25 to 34) which is represented by the light green block. The age groups are fitting for the high median income within the area. ", style = "padding-top:15px;font-size: 14px;"),
                                                                     p("
Another important determinant that might impact someone’s availability of opportunities is their race or ethnicity. So, our logical next step was to look at the ethnicity/race distribution of the people of Sterling. Collecting the Race and Ethnicity demographic proved to be a little challenging at first. While the team was observing the data, we kept noticing that the number of individuals in each race population kept exceeding the total population of Sterling. For example, when we would add up all of the white, hispanic, asian, Hawaiian, and  African American population’s, the total number would exceed 30,271, the population of Sterling, VA. We later learned that this is because the American Community Survey does not recognize hispanic as a race. To the American Community Survey, Hispanic is an ethnicity so People can identify as white or asian and still be of hispanic decent, or they could select “other” as there is a separate category for them to mark hispanic. Knowing that disclaimer, we had to create a separate visualization for the hispanic population so we could best represent them. As you can see, majority of the Sterling population is white. When you look at the ethnicity visualization, almost half of the sterling CDP population identifies as being hispanic or latino.
", style = "padding-top:15px;font-size: 14px;")
                                                                     
                                                                     )
                                                              
                                                              
                                                              )),
                                            
                                            tabPanel("Income",
                                                     fluidRow(style = "margin: 4px;",
                                                              p("", style = "padding-top:10px;"),
                                                              column(7, align = "left",
                                                                     selectInput("demos2drop", "Select Socioeconomic Characteristic:", width = "100%", choices = c(
                                                                       "Educational Attainment" = "edu",
                                                                       "Family Income" = "faminc",
                                                                       "Poverty Status" = "pov", 
                                                                       "Health Coverage" = "health"
                                                                     ),
                                                                     ),     
                                                                     br(""),
                                                                     withSpinner(plotlyOutput("demo2", height = "500px", width ="100%")),
                                                                     
                                                              ),
                                                              
                                                              column(5, align = "justify",
                                                              p("Now that we looked at the ethnic groups, we wanted to look at the educational attainment levels. The data was collected from the individuals who are ages 25 and over. It seems that a large number of the population are educated with that of a bachelors agree or higher, which is fitting for the county’s high median income. 
so a reasonable population has achieved high education - how much is Sterling making? Thus, we looked at family income levels. This data was collected looking at family households in the last 12 months of the ACS collected data (2020). It has also been adjusted for inflation. Interestingly enough, the largest income bracket lies within the range of $100,000 to $149,999 which is represented by the dark purple area however we do acknowledge that this may not apply our families of interest.
", style = "padding-top:15px;font-size: 14px;"),
                                                              p("So we wondered if the high-income levels also affect the housing market so this can help us to understand housing needs and availability of the children’s families. Looking at this visualization, we see that this area has a high property value with a large percentage of properties being worth $300,000 to $499,999, shown by the blue part of the circle. This is important to take into consideration considering our targeted title1 area. Once we looked at the high property values, we were intrigued to find out how many occupants own their homes. As expected, majority housing residents are home owners, while a little over a quarter housing residents are renters. This could be because of families high incomes.", style = "padding-top:15px;font-size: 14px;"),  
                                                              p("Next, we visualized the data for employment in the sterling area. Pink is used to show the Employed Percentage whereas Aqua blue shows the unemployed percentage of the population. One thing to note is that this percentage is from within the labor force not the total population of the area. One key take away from this graph is that the majority of sterling is employed at a remarkably
high rate higher than the national average. 71.8% employment rate.", style = "padding-top:15px;font-size: 14px;"), 
                                                              p("The next best thing to do was to look at what groups are being employed so we saw that the majority of them are being employed in the management, business, and science industry with a total number of 6,380 individuals. The service industry comes in second place at around 25% which makes it upto 4,122 individuals. The graph shows that one of the smallest occupations is the production industry which suggests that sterling is not an industrial area but has more corporate jobs to offer as management and business are one of the highest employable sectors in sterling. Using ACS American community survey data of the sterling cdp census designated place we found that 75% commuters drive on their own while only a quarter prefer other modes of commute. Notably less than 2 % of commuters take public transportation, this may be something we want to further research.", style = "padding-top:15px;font-size: 14px;"), 
                                                              
                                                              p("The visualization for healthcare tells us that private health insurance is the most popular type of health insurance. Private health insurance mainly consists of insurance plans provided through the employer. Coming in second place is the public insurance type which mainly consists of low-cost government backed programs such as medicare, medicaid, blue cross blue shield and Virginia Cover. However, there is a chunk of population of about 16.5% that does not have any kind of health insurance this might be an area where we can research more to figure out the possible opportunities in the community.", style = "padding-top:15px;font-size: 14px;"),
                                                              
                                                              )
                                                              
                                                              )),
                                            tabPanel("Occupation/Work",
                                                     fluidRow(style = "margin: 4px;",
                                                              p("", style = "padding-top:10px;"),
                                                              column(7, align = "left",
                                                                     selectInput("demos3drop", "Select Socioeconomic Characteristic:", width = "100%", choices = c(
                                                                       "Employment" = "employment",
                                                                       "Work Occupation" = "workoccu",
                                                                       "Commuter Time" = "commutertime",
                                                                       "Commuter Mode" = "commutermode"
                                                                     ),
                                                                     ),         
                                                                     br(""),
                                                                     withSpinner(plotlyOutput("demo3", height = "500px", width ="100%")),
                                                              ))),
                                            
                                            
                                            
                                          )),
                                   
                                   )
                 ),
                 
                 
                 navbarMenu("Community Schools",
                            tabPanel("Demographics", 
                                     fluidRow(style = "margin: 6px;",
                                              
                                              h1(strong("Demographics of Community Schools"), align = "center"),
                                              column(6, 
                                              #column(4, 
                                              #      h4(strong("Education")),
                                              #     p("These are demographics"),
                                              #  ) ,
                                              
                                                     h4(strong("Community Schools")),
                                                     selectInput("schooldrop", "Select Variable:", width = "100%", choices = c(
                                                       "Gender" = "cgender",
                                                       "Race/Ethnicity" ="raceehtn", 
                                                       "Hispanic Population" = "chispanic",
                                                       "Educators" = "cteacher",
                                                       "Enrollment" = "cenrol", 
                                                       "Absences" = "attend", 
                                                       "Chronic Absenteeism" = "chronic"
                                                     ),
                                                     ), 
                                                     withSpinner(plotlyOutput("ocuplot1", height = "500px", width = "100%")),
                                                     withSpinner(leafletOutput("ocuplot2", height = "500px", width = "60%")),
                                              ),
                                              
                                              
                                              
                                              
                                              
                                              
                                              # column(12, 
                                              #       h4("References: "), 
                                              #       p(tags$small("[1] Groundwater: Groundwater sustainability. (2021). Retrieved July 27, 2021, from https://www.ngwa.org/what-is-groundwater/groundwater-issues/groundwater-sustainability")) ,
                                              #      p("", style = "padding-top:10px;")) 
                                              
                                              
                                              #fluidRow(style = "margin: 12px;",
                                              #h1(strong("Analysis"), align = "center"),
                                              #p("", style = "padding-top:15px;font-size: 35px;"), 
                                              
                                              column(6,
                                                     h2(strong("Analysis")), align = "justify",
                                                     p("After understanding the demographics of the areas that feed into the community schools, next we began to look at the demographics of our specific populations, the 6 schools. For this, we used data from the Virginia Department of Education as well as the Loudoun County Public Schools dashboard and staff directory.", style = "padding-top:15px;font-size: 14px;"),
                                                     p("To further understand our population, we wanted to compare the race and ethnicity demographics we visualized from the Sterling CDP and our 6 community schools.   In this graph, we visualized data from all 6 schools together and found that overall, Hispanic students, represented by the light purple bar, make up the greatest percentage of students which differs from the general make-up of the Sterling CDP where White people made up the majority of residents. After seeing this, we wanted to look at the breakdown of the Hispanic population within the greater Sterling area.   Using data from the American Community Survey of Greater Sterling between the years 2016 to 2020, we found that the area where Rolling Ridge is located, represented by the light yellow area of the map, has the largest population of Hispanic identifying people. This is followed closely by Sterling Elementary, the area of the map shaded mustard yellow,  and Forest Grove Elementary, the dark orange, lower area of the map.   This information will help us to identify possible opportunities within the schools and neighborhoods specifically surrounding language services.", style = "padding-top:15px;font-size: 14px;"),
                                                     p("Once we felt we understood the demographics of those attending the schools, we switched our focus to the data within the schools themselves. Beginning with the number of teachers and staff employed at each school, we used data from the 2022 school directory, which revealed that at most of the schools, there are more teachers than staff except for Sterling Elementary, which has a larger amount of staff than teachers which may suggest possible possible opportunities in service.    Also notable was the lower total number of staff and teachers employed at Sully Elementary, seen in the last bar on the graph, which led us to visualize the total enrollment for each of the schools to better understand these differences.
 
Using data from the Virginia Department of Education’s Fall Membership Reports for the years 2016 to 2020,   we found that Guilford, seen in the bottom left graph,   Sugarland, the top right graph,   and Rolling Ridge, the bottom right graph,  all maintained a total enrollment of between 550 and 600 students with only slight variations between years. For Forest Grove, the top left graph,   enrollment remained steady between 2016 and 2020 hovering right at 575 students.   On the other hand, Sterling Elementary, the top right graph,   had an enrollment of 450 to 500 students with a slight decline from 2016 to 2020   and Sully Elementary, has only between 400 to 475 students enrolled.   Since the implementation of the Community School Initiative, there has been an increase in enrollment at Sully and Guilford,   while we are not saying that this program is the cause of this increase, it was noticed and may be worth looking into further. Additionally, the differences in enrollment between schools may suggest possible opportunities to look into surrounding youth engagement resources.
", style = "padding-top:15px;font-size: 14px;"),
                                                     p("To further breakdown the enrollment statistics, we used the LCPS Dashboard data to visualize the student’s absences by quarter at each school during the 2021 to 2022 school year.   While Rolling Ridge, the green line,   Sugarland, the dark blue line,   and Guilford, the yellow line,   all saw spikes in absences during quarter two,   it was Sully, the pink line,   Sterling, the light blue line,   and Forest Grove, the orange line,   that had a steady increase in absences over the 4 quarters. However it is important to note that this data came from the school year during the COVID-19 pandemic.
To determine if this issue was chronic,   we used Virginia Department of Education data from prior to the pandemic on chronic absenteeism.   This is defined as the percentage of students who miss more than 10% of total classes throughout the year.   This data revealed to us that Sugarland and Rolling Ridge Elementary continued to have a high number of absences prior to the pandemic, suggesting that this may be an area to look for possible service gaps.
", style = "padding-top:15px;font-size: 14px;"),
                                                     
                                                     
                                                     
                                              )
                                     )), 
                            
                            tabPanel("Climate Survey Reports",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              
                                              tabsetPanel(
                                                tabPanel("Parent Climate Survey",
                                                         fluidRow(style = "margin: 2px;",
                                                                  p("", style = "padding-top:10px;"),
                                                                  column(6, align = "center",h4(strong("Parent Survey")),
                                                                         p("Parent Climate Survey Results From The 2019 to 2020 School Year"),
                                                                         selectInput("surveydrop", "Select Survey Question", width = "60%", choices = c(
                                                                           "Parent Survey",
                                                                           "Student Survey",
                                                                           "Teacher/Staff Survey"
                                                                         ),
                                                                         ),
                                                                         br("")
                                                                         
                                                                         
                                                                  ),
                                                                  column(6, align = "center",h4(strong("Parent Survey")),
                                                                         p("Parent Climate Survey Results From The 2019 to 2020 School Year"),
                                                                         selectInput("surveydrop", "Select Survey Question", width = "60%", choices = c(
                                                                           "Parent Survey",
                                                                           "Student Survey",
                                                                           "Teacher/Staff Survey"
                                                                         ),
                                                                         ),
                                                                         br("")
                                                                         
                                                                         
                                                                  ),
                                                                  column(6, align = "center",h4(strong("Parent Survey")),
                                                                         p("Parent Climate Survey Results From The 2019 to 2020 School Year"),
                                                                         selectInput("surveydrop", "Select Survey Question", width = "60%", choices = c(
                                                                           "Parent Survey",
                                                                           "Student Survey",
                                                                           "Teacher/Staff Survey"
                                                                         ),
                                                                         ),
                                                                         br("")
                                                                         
                                                                         
                                                                  ),
                                                                  column(6, align = "center",h4(strong("Parent Survey")),
                                                                         p("Parent Climate Survey Results From The 2019 to 2020 School Year"),
                                                                         selectInput("surveydrop", "Select Survey Question", width = "60%", choices = c(
                                                                           "Parent Survey",
                                                                           "Student Survey",
                                                                           "Teacher/Staff Survey"
                                                                         ),
                                                                         ),
                                                                         br("")
                                                                         
                                                                         
                                                                  ),
                                                         )
                                                ),
                                                tabPanel("Student Climate Survey",
                                                         p("", style = "padding-top:10px;"),
                                                         column(6, align = "center",h4(strong("Student Survey")),
                                                                p("Student Climate Survey Results From The 2019 to 2020 School Year"),
                                                                selectInput("surveydrop", "Select Survey Question", width = "60%", choices = c(
                                                                  "Parent Survey",
                                                                  "Student Survey",
                                                                  "Teacher/Staff Survey"
                                                                ),
                                                                ),
                                                                br("")
                                                                
                                                                
                                                         ),
                                                         column(6, align = "center",h4(strong("Student Survey")),
                                                                p("Student Climate Survey Results From The 2019 to 2020 School Year"),
                                                                selectInput("surveydrop", "Select Survey Question", width = "60%", choices = c(
                                                                  "Parent Survey",
                                                                  "Student Survey",
                                                                  "Teacher/Staff Survey"
                                                                ),
                                                                ),
                                                                br("")
                                                                
                                                                
                                                         ),
                                                         column(6, align = "center",h4(strong("Student Survey")),
                                                                p("Student Climate Survey Results From The 2019 to 2020 School Year"),
                                                                selectInput("surveydrop", "Select Survey Question", width = "60%", choices = c(
                                                                  "Parent Survey",
                                                                  "Student Survey",
                                                                  "Teacher/Staff Survey"
                                                                ),
                                                                ),
                                                                br("")
                                                                
                                                                
                                                         ),
                                                         column(6, align = "center",h4(strong("Student Survey")),
                                                                p("Student Climate Survey Results From The 2019 to 2020 School Year"),
                                                                selectInput("surveydrop", "Select Survey Question", width = "60%", choices = c(
                                                                  "Parent Survey",
                                                                  "Student Survey",
                                                                  "Teacher/Staff Survey"
                                                                ),
                                                                ),
                                                                br("")
                                                                
                                                                
                                                         ),
                                                ), 
                                                
                                                tabPanel("Teacher/Staff Climate Survey",
                                                         p("", style = "padding-top:10px;"),
                                                         column(6, align = "center",h4(strong("Teacher/Staff Survey")),
                                                                p("Teacher/Staff Climate Survey Results From The 2019 to 2020 School Year"),
                                                                selectInput("surveydrop", "Select Survey Question", width = "60%", choices = c(
                                                                  "Parent Survey",
                                                                  "Student Survey",
                                                                  "Teacher/Staff Survey"
                                                                ),
                                                                ),
                                                                br("")
                                                                
                                                                
                                                                
                                                         ),
                                                         column(6, align = "center",h4(strong("Teacher/Staff Survey")),
                                                                p("Teacher/Staff Climate Survey Results From The 2019 to 2020 School Year"),
                                                                selectInput("surveydrop", "Select Survey Question", width = "60%", choices = c(
                                                                  "Parent Survey",
                                                                  "Student Survey",
                                                                  "Teacher/Staff Survey"
                                                                ),
                                                                ),
                                                                br("")
                                                                
                                                                
                                                                
                                                         ),
                                                         column(6, align = "center",h4(strong("Teacher/Staff Survey")),
                                                                p("Teacher/Staff Climate Survey Results From The 2019 to 2020 School Year"),
                                                                selectInput("surveydrop", "Select Survey Question", width = "60%", choices = c(
                                                                  "Parent Survey",
                                                                  "Student Survey",
                                                                  "Teacher/Staff Survey"
                                                                ),
                                                                ),
                                                                br("")
                                                                
                                                                
                                                                
                                                         ),
                                                         column(6, align = "center",h4(strong("Teacher/Staff Survey")),
                                                                p("Teacher/Staff Climate Survey Results From The 2019 to 2020 School Year"),
                                                                selectInput("surveydrop", "Select Survey Question", width = "60%", choices = c(
                                                                  "Parent Survey",
                                                                  "Student Survey",
                                                                  "Teacher/Staff Survey"
                                                                ),
                                                                ),
                                                                br("")
                                                                
                                                                
                                                                
                                                         )))
                                     ),
                                     
                            ),
                            
                            tabPanel("School Reports",
                                     fluidPage(style = "margin: 2px;",
                                               p(h4("Representatives' responses"), style = "padding-top:5px;"),
                                               
                                               tabsetPanel(
                                                 tabPanel("Challenges and Weaknesses",
                                                          fluidRow(style = "margin: 2px;",
                                                                   p("", style = "padding-top:10px;"),
                                                                   column(12, align = "center",
                                                                          wordcloud2Output("cloud1")
                                                                   )
                                                                   
                                                          )),
                                                 tabPanel("Strengths and Successes",
                                                          p("", style = "padding-top:10px;"),
                                                          column(12, align = "center",
                                                                 wordcloud2Output("cloud2")
                                                          ),
                                                          
                                                 ), 
                                                 
                                                 tabPanel("Future Goals",
                                                          p("", style = "padding-top:10px;"),
                                                          column(12, align = "center",
                                                                 wordcloud2Output("cloud3")
                                                          ),
                                                          
                                                 )
                                                 
                                                 
                                                 
                                                 
                                               )))
                            
                            #tabPanel(h4("Weaknesses and biggest challenges")),
                            
                            
                            
                            
                            
                 ),
                 
                 navbarMenu("Availability of Resources",
                            tabPanel("Health and Social Services",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h4(strong("Health and Social Services")),
                                                     p(""),
                                                     br("")
                                                     
                                                     
                                                     
                                                     
                                                     
                                                     
                                              )),
                                     
                                     fluidPage(style = "margin: 2px;", 
                                               column(12, 
                                                      leafletOutput("map_health", width = "100%")
                                                      #fluidRow(align = "center",
                                                      #    p(tags$small(em('Last updated: August 2021'))))
                                               )
                                     )),
                            
                            
                            
                            tabPanel("Mental Health",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h4(strong("")),
                                                     p(""),
                                                     br("")
                                                     
                                                     
                                                     
                                              )),
                                     fluidPage(style = "margin: 2px;", 
                                               column(12, 
                                                      leafletOutput("map_mental", width = "100%")
                                                      #fluidRow(align = "center",
                                                      #    p(tags$small(em('Last updated: August 2021'))))
                                               )
                                     )
                                     
                            ),
                            tabPanel("Family Engagement",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h4(strong("")),
                                                     p(""),
                                                     br("")
                                                     
                                                     
                                                     
                                              )),
                                     
                                     
                                     
                                     
                            ),
                            tabPanel("Youth Development Opportunities",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h4(strong("Youth Development Opportunities")),
                                                     p(""),
                                                     br("")
                                                     
                                                     
                                                     
                                              )
                                              
                                     ),
                                     
                                     
                                     
                                     fluidPage(style = "margin: 2px;", 
                                               column(12, 
                                                      leafletOutput("map_youth", width = "100%")
                                                      #fluidRow(align = "center",
                                                      #    p(tags$small(em('Last updated: August 2021'))))
                                               )
                                     )),
                            
                 ),
                 
                 tabPanel("Service possible opportunities",
                          fluidRow(style = "margin: 6px;",
                                   p("", style = "padding-top:10px;"),
                                   column(12, align = "center",h4(strong("Service possible opportunities")),
                                          p(""),
                                          br("")
                                          
                                          
                                          
                                   )),
                          
                 ),
                 
                 tabPanel("Analysis",
                          fluidRow(style = "margin: 6px;",
                                   p("", style = "padding-top:10px;"),
                                   column(12, align = "center",h4(strong("")),
                                          p(""),
                                          br("")
                                          
                                          
                                          
                                   )),
                          
                 ),
                 
                 tabPanel("Meet the Team",
                          fluidRow(style = "margin: 6px;",
                                   p("", style = "padding-top:10px;"),
                                   column(12, align = "center",h4(strong("")),
                                          p(""),
                                          br("")
                                          
                                          
                                          
                                   )),
                          
                 )
                 
)


# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  # Render map 
  output$map1 <- renderLeaflet({
    map1
  })
  
  output$map_health <- renderLeaflet({
    map_health
  })
  
  output$map_youth <- renderLeaflet({
    map_youth
  })
  
  output$map_mental <- renderLeaflet({
    map_mental
  })
  
  output$cloud2 <- renderWordcloud2(
    cloud2
  )
  
  output$cloud3 <- renderWordcloud2(
    cloud3
  )
  
  
  Var <- reactive({
    input$demosdrop
  })
  
  
  Var3 <- reactive({
    input$demos1drop
  })
  
  output$demo1 <- renderPlotly({
    
    if (Var3() == "gender") {
      
      gender
      
    }
    
    else if (Var3() == "race") {
      
      race
    }
    
    else if (Var3() == "age") {
      
      agesterling
    }
    
  })
  
  Var4 <- reactive({
    input$demos2drop
  })
  
  output$demo2  <- renderPlotly({
    
    if (Var4() == "edu") {
      
      education
      
    }
    
    else if (Var4() == "faminc") {
      
      income
    }
    
    else if (Var4() == "pov") {
      
      pov
    }
    
    else if (Var4() == "health") {
      
      healthin
      
      
    }
    
  })
  
  Var5 <- reactive({
    input$demos3drop
  })
  
  output$demo3  <- renderPlotly({
    
    if (Var5() == "commutertime") {
      
      commutertime
      
    }
    
    else if (Var5() == "commutermode") {
      
      commutermode
    }
    
    else if (Var5() == "workoccu") {
      
      occuplot
    }
    
    else if (Var5() == "employment") {
      
      employment
      
      
    }
    
    
  })
  
  
  
  #School Demos
  Var2 <- reactive({
    input$schooldrop
  }) 
  
  output$ocuplot1 <- renderPlotly({
    
    if(Var2() == "raceehtn"){
      raceehtn 
    }
    else if(Var2() == "cgender"){
      genders 
    }
    
    else if(Var2() == "attend"){
      attend 
    }
    
    else if (Var2() == "cteacher") {
      
      cteacher
      
    }
    else if (Var2() == "chronic") {
      
      chronic
      
      
    }
    else if (Var2() == "cenrol") {
      enroll
    }
  })
  
  
  output$ocuplot2 <- renderLeaflet({
    if (Var2() == "chispanic") {
      
      hispanicschool
    }
    
    
  })
  
  
  #---------word clouds-----------------
  
  output$cloud1 <- renderWordcloud2(
    cloud1
  )
  
  
}
shinyApp(ui = ui, server = server)


