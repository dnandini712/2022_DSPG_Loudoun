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

poverty_as1<- read_excel(paste0(getwd(),"/data/povertybyageandsexnewss.xlsx"), 
                        sheet = "Data")

subset_poverty_as1 <- poverty_as1[3:28, 1:4]

#subset_poverty_as$Estimate[1:26]
povas_pop1 <- subset_poverty_as1$Estimate
povas_pop1 <- as.numeric(povas_pop1)
povas_cat1 <- subset_poverty_as1$Label

Total1 <- povas_pop1

cat1 <- as.character(povas_cat1)

pov <- plot_ly(subset_poverty_as1, x = cat1, y = Total1, color = ~Sex, type = "bar", hoverinfo = "text",text = ~paste("Age:",cat1,"<br>","Total:",Total1,"<br>","Sex:",Sex)) %>% layout(title = "Poverty by Age and Sex",xaxis = list(title="",barmode = "group", categoryorder = "array", categoryarray = cat1))
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
nineteensub <- race_subset[(73:96),(1:4)]
Race <- nineteensub$Race
Total <- nineteensub$`Full Time Count (All Grades)`
School <- nineteensub$`School Name`
racenine <- ggplot(nineteensub,aes(x=School,y=Total,fill=Race))+ geom_col(position = "dodge")+labs(title="Race/Ethnicity Demographics for 2019-2020",y="Total",x = "",caption = "Source: VDOE Fall Membership Report 2016-2020") + theme(plot.caption.position = "plot",
                                                                                                                                                                                                                                         plot.caption = element_text(hjust = 1)) + guides(fill=guide_legend(title="Race/Ethnicity"))
racenine <- ggplotly(racenine)

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
    label=paste("Total Hispanic Population: ",va20_2$estimate),
    highlightOptions = highlightOptions(color = "white",
                                        weight = 2,
                                        bringToFront = TRUE)) %>%
  addLegend(pal=mypalette, position = "bottomright",
            values = ~va20_2$estimate,
            opacity = 0.5, title = "Hispanic Population") %>%
  addMarkers( ~Longitude, ~Latitude, popup = popups, label = ~as.character(School), labelOptions = FALSE) 
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

#----------------------family engagement map-------------------

familyengage <- read_excel(paste0(getwd(),"/data/ListOfResources.xlsx"),sheet = "Family Engagement")

popups <- lapply(
  paste("<strong>Name: </strong>",
        str_to_title(familyengage$Name),
        "<br />",
        "<strong>Description:</strong>",
        familyengage$Description ,
        "<br />",
        "<strong>Hours:</strong>",
        familyengage$Hours, 
        "<br />",
        "<strong>Address:</strong>",
        familyengage$Address,
        "<br />",
        "<a href = ",familyengage$Website, "> Website </a>",
        "<br />",
        "<strong>Serves:</strong>",
        familyengage$Serves),
  
  
  htmltools::HTML
)

pal <- colorFactor(c("red", "blue", "green", "orange","purple", "#2e850c"), domain = c("Housing", "Holiday Help", "Education", "Essentials supply", "Employment help", "Other"))

leaflet(data = familyengage) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = va20_2,
              color="#5f308f",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% 
  addPolygons(data=traveltime20, color= "#21618C",opacity = 1,weight=2,fillColor = "white", fillOpacity = .1) %>% addPolygons(data=traveltime10,color="green",opacity=1,weight=2,fillColor = "white",fillOpacity = .1) %>%     addPolygons(data=traveltime45,color="#D98880",opacity = 1,weight = 2,fillColor = "white",fillOpacity = .1) %>%
  setView(-77.4029155,39.009006, zoom = 11)%>%
  addCircleMarkers(data=familyengage,~Longitude,~Latitude,popup=~popups,label=~as.character(Name),group=~Resources,color=~pal(Resources),weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1)%>%
  addLayersControl(overlayGroups = ~Resources,options = layersControlOptions(collapsed = FALSE)) %>% 
  addMarkers(data=subset_map,~Longitude,~Latitude,popup = ~as.character(School)) %>% addLegend("bottomright",colors=c("green","#21618C","#D98880"),labels=c("10 minutes","20 minutes","45 minutes"),title = "Travel Time") -> map_family



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

docs1 <- tm_map(docs1, removeWords, c("to", "challenge", "concern", "level", "aged", "created", "elementary", "basic", "in", "and", "the", "we", "of", "an", "is", "like", "for", "those", "were", "was", "list", "our", "with", "would", "very", "huge","this","same","ongoing","overall", "finding", "continue"))


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

docs2 <- tm_map(docs2, removeWords, c("day", "now", "help", "pep", "people", "arose", "to", "risen", "offer", "offered", "warm", "spots", "their", "every", "they", "tell", "that", "who", "are", "all", "many", "gaggle", "here", "always", "among", "mtss", "umht", "how", "feel", "adept", "in", "and", "the", "we", "of", "an", "is", "like", "for", "those", "were", "was", "list", "our", "with", "would", "very", "huge","this","same","ongoing","overall", "finding", "hot"))


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

docs3 <- tm_map(docs3, removeWords, c("will", "covid", "areas", "listed", "input", "to", "per", "pre", "utilize", "most", "also", "more", "many", "ways", "local", "pep", "times", "ridge", "year", "needy", "people", "after", "person", "in", "and", "the", "we", "of", "an", "is", "like", "for", "those", "were", "was", "list", "our", "with", "would", "very", "umht", "hot", "to", "in", "and", "the", "we", "of", "an", "is", "like", "for", "those", "were", "was", "list", "our", "with", "would", "very", "huge","this","same","ongoing","overall", "finding", "their", "from", "always"))


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
subsetnewsurveydataSTAFF <- newsurveydata[3:8,c(1,2:5,7:8)]

#Teacher & Staff Survey Q1 - Staff Collegiality
staffquestion1 <- subsetnewsurveydataSTAFF[1:6,1:2]
question1 <- staffquestion1$SCHOOLS
staffquestion1percentage <- staffquestion1$`Question 1`
staffquestion1percentage <- as.numeric(staffquestion1percentage)
staffquestion1percentage <- staffquestion1percentage*100
one <- ggplot(staffquestion1,aes(x=question1,y=staffquestion1percentage,fill=question1, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",staffquestion1$SCHOOLS)))+labs(title="Staff Collegiality",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion1percentage, y = staffquestion1percentage), size = 3, position = position_stack(vjust = 1.02))
teacherandstaffanswer1 <- ggplotly(one, tooltip = c("text"))

#Teacher & Staff Survey Q2 - Academic Environment
staffquestion2 <- subsetnewsurveydataSTAFF[1:6,c(1,3)]
question2 <- staffquestion2$SCHOOLS
staffquestion2percentage <- staffquestion2$`Question 2`
staffquestion2percentage <- as.numeric(staffquestion2percentage)
staffquestion2percentage <- staffquestion2percentage*100
two <- ggplot(staffquestion2,aes(x=question2,y=staffquestion2percentage,fill=question2, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",staffquestion2$SCHOOLS)))+labs(title="Academic Environment",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion2percentage, y = staffquestion2percentage), size = 3, position = position_stack(vjust = 1.02))
teacherandstaffanswer2 <- ggplotly(two, tooltip = c("text"))

#Teacher & Staff Survey Q3 - School Leadership
staffquestion3 <- subsetnewsurveydataSTAFF[1:6,c(1,4)]
question3 <- staffquestion3$SCHOOLS
staffquestion3percentage <- staffquestion3$`Question 3`
staffquestion3percentage <- as.numeric(staffquestion3percentage)
staffquestion3percentage <- staffquestion3percentage*100
three <- ggplot(staffquestion3,aes(x=question3,y=staffquestion3percentage,fill=question3, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",staffquestion3$SCHOOLS)))+labs(title="School Leadership",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion3percentage, y = staffquestion3percentage), size = 3, position = position_stack(vjust = 1.02))
teacherandstaffanswer3 <- ggplotly(three, tooltip = c("text"))

#Teacher & Staff Survey Q4 - Managing Student Behavior
staffquestion4 <- subsetnewsurveydataSTAFF[1:6,c(1,5)]
question4 <- staffquestion4$SCHOOLS
staffquestion4percentage <- staffquestion4$`Question 4`
staffquestion4percentage <- as.numeric(staffquestion4percentage)
staffquestion4percentage <- staffquestion4percentage*100
four <- ggplot(staffquestion4,aes(x=question4,y=staffquestion4percentage,fill=question4, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",staffquestion4$SCHOOLS)))+labs(title="Managing Student Behavior",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion4percentage, y = staffquestion4percentage), size = 3, position = position_stack(vjust = 1.02))
teacherandstaffanswer4 <- ggplotly(four, tooltip = c("text"))

#Teacher & Staff Survey Q6 - Workplace Environment
staffquestion6 <- subsetnewsurveydataSTAFF[1:6,c(1,6)]
question6 <- staffquestion6$SCHOOLS
staffquestion6percentage <- staffquestion6$`Question 6`
staffquestion6percentage <- as.numeric(staffquestion6percentage)
staffquestion6percentage <- staffquestion6percentage*100
six <- ggplot(staffquestion6,aes(x=question6,y=staffquestion6percentage,fill=question6, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",staffquestion6$SCHOOLS)))+labs(title="Workplace Environment",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion6percentage, y = staffquestion6percentage), size = 3, position = position_stack(vjust = 1.02))
teacherandstaffanswer6 <- ggplotly(six, tooltip = c("text"))

#Teacher & Staff Survey Q7 - Instructional Practices 
staffquestion7 <- subsetnewsurveydataSTAFF[1:6,c(1,7)]
question7 <- staffquestion7$SCHOOLS
staffquestion7percentage <- staffquestion7$`Question 7`
staffquestion7percentage <- as.numeric(staffquestion7percentage)
staffquestion7percentage <- staffquestion7percentage*100
seven <- ggplot(staffquestion7,aes(x=question7,y=staffquestion7percentage,fill=question7, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",staffquestion7$SCHOOLS)))+labs(title="Instructional Environment",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion7percentage, y = staffquestion7percentage), size = 3, position = position_stack(vjust = 1.02))
teacherandstaffanswer7 <- ggplotly(seven, tooltip = c("text"))

#--------------Parent Climate Surveys--------------------------------
newsurveydata <- read_excel(paste0(getwd(), "/data/NewSurveyData.xlsx"),skip=0,col_names=TRUE)
subsetnewsurveydataPARENT <- newsurveydata[19:24,c(1,2:5)]

#Parent Survey Q1 - Academic Support 
parentquestion1 <- subsetnewsurveydataPARENT[1:6,1:2]
question8 <- parentquestion1$SCHOOLS
parentquestion1percentage <- parentquestion1$`Question 1`
parentquestion1percentage <- as.numeric(parentquestion1percentage)
parentquestion1percentage <- parentquestion1percentage*100
eight <- ggplot(parentquestion1,aes(x=question8,y=parentquestion1percentage,fill=question8, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",parentquestion1$SCHOOLS)))+labs(title="Academic Support",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = parentquestion1percentage, y = parentquestion1percentage), size = 3, position = position_stack(vjust = 1.02))
parentanswer1 <- ggplotly(eight, tooltip = c("text"))

#Parent Survey Q2 - Communications
parentquestion2 <- subsetnewsurveydataPARENT[1:6,c(1,3)]
question9 <- parentquestion2$SCHOOLS
parentquestion2percentage <- parentquestion2$`Question 2`
parentquestion2percentage <- as.numeric(parentquestion2percentage)
parentquestion2percentage <- parentquestion2percentage*100
nine <- ggplot(parentquestion2,aes(x=question9,y=parentquestion2percentage,fill=question9, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",parentquestion2$SCHOOLS)))+labs(title="Communications",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = parentquestion2percentage, y = parentquestion2percentage), size = 3, position = position_stack(vjust = 1.02))
parentanswer2 <- ggplotly(nine, tooltip = c("text"))

#Parent Survey Q3 - Relationships
parentquestion3 <- subsetnewsurveydataPARENT[1:6,c(1,4)]
question10 <- parentquestion3$SCHOOLS
parentquestion3percentage <- parentquestion3$`Question 3`
parentquestion3percentage <- as.numeric(parentquestion3percentage)
parentquestion3percentage <- parentquestion3percentage*100
ten <- ggplot(parentquestion3,aes(x=question10,y=parentquestion3percentage,fill=question10, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",parentquestion3$SCHOOLS)))+labs(title="Relationships",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = parentquestion3percentage, y = parentquestion3percentage), size = 3, position = position_stack(vjust = 1.02))
parentanswer3 <- ggplotly(ten, tooltip = c("text"))

#Parent Survey Q4 - Instructions
parentquestion4 <- subsetnewsurveydataPARENT[1:6,c(1,5)]
question11 <- parentquestion4$SCHOOLS
parentquestion4percentage <- parentquestion4$`Question 4`
parentquestion4percentage <- as.numeric(parentquestion4percentage)
parentquestion4percentage <- parentquestion4percentage*100
eleven <- ggplot(parentquestion4,aes(x=question11,y=parentquestion4percentage,fill=question11, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",parentquestion4$SCHOOLS)))+labs(title="Instructions",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = parentquestion4percentage, y = parentquestion4percentage), size = 3, position = position_stack(vjust = 1.02))
parentanswer4 <- ggplotly(eleven, tooltip = c("text"))

#--------------Student Climate Surveys-------------------------------
newsurveydata <- read_excel(paste0(getwd(), "/data/NewSurveyData.xlsx"),skip=0,col_names=TRUE)
subsetnewsurveydataSTUDENT <- newsurveydata[11:16,c(1,2:4,6)]

#Student Survey Q1 - Student Engagement
studentquestion1 <- subsetnewsurveydataSTUDENT[1:6,1:2]
question12 <- studentquestion1$SCHOOLS
studentquestion1percentage <- studentquestion1$`Question 1`
studentquestion1percentage <- as.numeric(studentquestion1percentage)
studentquestion1percentage <- studentquestion1percentage*100
twelve <- ggplot(studentquestion1,aes(x=question12,y=studentquestion1percentage,fill=question12, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",studentquestion1$SCHOOLS)))+labs(title="Student Engagement",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = studentquestion1percentage, y = studentquestion1percentage), size = 3, position = position_stack(vjust = 1.02))
studentanswer1 <- ggplotly(twelve, tooltip = c("text"))

#Student Survey Q2 - Teacher Relationship
studentquestion2 <- subsetnewsurveydataSTUDENT[1:6,c(1,3)]
question13 <- studentquestion2$SCHOOLS
studentquestion2percentage <- studentquestion2$`Question 2`
studentquestion2percentage <- as.numeric(studentquestion2percentage)
studentquestion2percentage <- studentquestion2percentage*100
thirteen <- ggplot(studentquestion2,aes(x=question13,y=studentquestion2percentage,fill=question13, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",studentquestion2$SCHOOLS)))+labs(title="Teacher Relationship",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = studentquestion2percentage, y = studentquestion2percentage), size = 3, position = position_stack(vjust = 1.02))
studentanswer2 <- ggplotly(thirteen, tooltip = c("text"))

#Student Survey Q3 - Social-Emotional Wellbeing
studentquestion3 <- subsetnewsurveydataSTUDENT[1:6,c(1,4)]
question14 <- studentquestion3$SCHOOLS
studentquestion3percentage <- studentquestion3$`Question 3`
studentquestion3percentage <- as.numeric(studentquestion3percentage)
studentquestion3percentage <- studentquestion3percentage*100
fourteen <- ggplot(studentquestion3,aes(x=question14,y=studentquestion3percentage,fill=question14, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",studentquestion3$SCHOOLS)))+labs(title="Social-Emotional Wellbeing",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = studentquestion3percentage, y = studentquestion3percentage), size = 3, position = position_stack(vjust = 1.02))
studentanswer3 <- ggplotly(fourteen, tooltip = c("text"))

#Student Survey Q5 - Bullying
studentquestion5 <- subsetnewsurveydataSTUDENT[1:6,c(1,5)]
question16 <- studentquestion5$SCHOOLS
studentquestion5percentage <- studentquestion5$`Question 5`
studentquestion5percentage <- as.numeric(studentquestion5percentage)
studentquestion5percentage <- studentquestion5percentage*100
sixteen <- ggplot(studentquestion5,aes(x=question16,y=studentquestion5percentage,fill=question16, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",studentquestion5$SCHOOLS)))+labs(title="Bullying",x="",y="percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = studentquestion5percentage, y = studentquestion5percentage), size = 3, position = position_stack(vjust = 1.02))
studentanswer5 <- ggplotly(sixteen, tooltip = c("text"))

# user interface-------------------------------------------------------------
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
                                          
                                          p("Loudoun County",a(href = "https://www.loudoun.gov/Loudoun County"), target = "_blank", "is located in the Northern Neck of Virginia 
                                          within the D.C. metropolitan area. With a population of 413,538 and a median income of $147,111 in 2020, Loudoun County is 
                                          fast growing and has remained the richest county with a population over 65,000 in the United States. The bulk of the 
                                          population resides in the Eastern half of Loudoun County with many high-tech companies located in and around the region 
                                          while Western Loudoun retains many of its rural roots. Loudoun County Public Schools claims the top employer with 
                                          over 10,000 employees followed by Verizon and the Loudoun County Government."),
                                          p("Sterling",a(href = "https://en.wikipedia.org/wiki/Sterling,_Virginia Sterling"), target = "_blank", "Virginia, located in the 
                                              Eastern most section of Loudoun County, had a population of 30,872 in 2020. Sterling is situated next to Washington Dulles 
                                              International Airport and is home to many federal agencies and tech companies allowing for rapid growth."),
                                          #p("During the 2018 – 2019 school year, the Community school model provided the families with clothes, shoes, and other basic supplies 538 times; enabled 135 families to receive weekend meals throughout the school year; supported 6 academic programs for 323 students; and provided 9 after-school enrichment programs for 373 students. Funds have provided these Community Schools with additional resources, such as full-time parent liaisons, a full-time social worker, and programs that keep families engaged in their child’s education. The Community Schools initiative focuses on bolstering these schools in six areas: academies, health and social services, youth and community engagement, building stronger families, and healthier communities."),
                                          
                                   ),
                                   column(4,
                                          h2(strong("Project Background")), align = "justify",
                                          
                                          p("The Supervisor of Outreach Services for the Department of Education in the Loudoun County Public School
                                            System as well as the Virginia Cooperative Extension seek to understand areas of opportunity to further
                                            assist low-income families within Sterling."),
                                          p("We will use publicly available data including the American Community Survey and Virginia Department of Education to 
                                            provide our stakeholders with a comprehensive understanding of the factors impacting the Sterling area. We focus on 
                                            sociodemographic indicators, community school characteristics, and resource proximity to support our analysis."),
                                   ),  
                                   
                                   column(4,
                                          h2(strong("Project Goals")),
                                          p("Our team seeks to design an interactive dashboard that visualizes the resources and services available to the students and families
                                            involved in the Loudoun County Community School Initiative. This dashboard will allow stakeholders to understand the main needs of 
                                            the community and provide insights into potential opportunities for improvement to increase the quality of life for those impacted 
                                            by the Community School Initiative."),
                                          p("Our dashboard will contain our findings and analysis, which will allow both our stakeholders and all those interested to understand this 
                                            information in a comprehensive and dynamic manner"), 
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
                 
                 
                 tabPanel("Sociodemographics",
                          fluidRow(style = "margin: 4px;",
                                   h1(strong("Sterling Sociodemographics"), align = "center"),
                                   p("", style = "padding-top:10px;"), 
                                   column(12, 
                                          h2(strong("Sterling Residents' Characteristics")),
                                          column(12, align = "left",
                                                 h3(strong("Who Makes Up Sterling, Virginia?")), 
                                                 p("We used the American Community Survey (ACS) 5-year data to understand the socioeconomic demographics of the Sterling Census Designated Place (CDP) from the years 2016 to 2020. The ACS data is a survey collected by the U.S. Census Bureau which gathers sociodemographic information on American households including age, gender, race/ethnicity, income, and employment. ")
                                          ),
                                          tabsetPanel(
                                            
                                            tabPanel("Demographic",
                                                     fluidRow(style = "margin: 4px;",
                                                              p("", style = "padding-top:10px;"),
                                                              column(7, align = "left",
                                                                     selectInput("demos1drop", "Select Socioeconomic Characteristic:", width = "100%", choices = c(
                                                                       "Gender" = "gender",
                                                                       "Age" = "age",
                                                                       "Race/Ethnicity" = "race"
                                                                       
                                                                     ),
                                                                     ),   
                                                                     br(""),
                                                                     withSpinner(plotlyOutput("demo1", height = "500px", width ="100%")),
                                                                     column(12, align = "right",
                                                                            p("Source: American Community 2019 5-Year Estimates", style = "font-size:12px;"),
                                                                           
                                                                     )
                                                              ),
                                                              
                                                              column(5, 
                                                                     
                                                                     p("Within Sterling, the largest age group are adults aged 35 to 44 years old, closely followed by 
                                                                       25 to 34 years old, and 45 to 54 years old. About 27% of the population is under the age of 20 
                                                                       with the largest group being those under 5.", style = "padding-top:15px;font-size: 14px;"),
                                                                     p("From 2016-2020, those identifying as White made up just over half of the Sterling residents 
                                                                       followed Asian and Other which may include those who identify as Hispanic as the ACS does not
                                                                       include Hispanic as a race.", 
                                                                       style = "padding-top:15px;font-size: 14px;"),
                                                              
                                                              
                                                     ))),
                                            
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
                                                                     column(12,align = "right",
                                                                     p("Source: American Community 2019 5-Year Estimates", style = "font-size:12px;"),
                                                                     p("*Note: Data is nill for missing bars", style = "font-size:12px;"))
                                                                     
                                                              ),
                                                              
                                                              column(5, align = "justify",
                                                                     p("For the Sterling residents, the majority have attained a high school degree or 
                                                                       equivalent with the largest group having attained a bachelor’s degree. ", style = "padding-top:15px;font-size: 14px;"),
                                                                     p("For families in the past 12 months, the largest income level is the $100,000 to $149,999 bracket 
                                                                       followed closely by both the $50,000 to $74,999 bracket and $150,000 to $199,999. It should be taken
                                                                       into consideration however that our data is slightly skewed as the areas of the Sterling CPD includes
                                                                       those that are highly affluent, outweigh those located in the areas with the schools designated as 
                                                                       Title 1. ", style = "padding-top:15px;font-size: 14px;"),  
                                                                     p("Expanding on that, females ages 18 to 24 years old face the highest level of poverty by sex and age 
                                                                       overall followed by females ages 35 to 44 years old and males ages 25 to 34 years old. The ACS lacks 
                                                                       data on males 12 to 14, 15, and 34 to 44 years as well as females ages 16 to 17 years. From the data 
                                                                       that is available, females tend to have higher levels of poverty than males especially from years 6 to 11. 
                                                                       While most Sterling residents have private health insurance, 23% are on public insurance such as Medicare 
                                                                       and Medicaid, and 16.5% of residents have no health insurance at all. ", 
                                                                       style = "padding-top:15px;font-size: 14px;"), 
                                                                     
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
                                                                     column(12,align = "right",
                                                                            p("Source: American Community 2019 5-Year Estimates", style = "font-size:12px;"),
                                                                          )),
                                                                     column(5, align = "justify",
                                                                            p("Within Sterling, the majority of residents are employed at 71.8% while only 4.7% 
                                                                              of the residents are unemployed. The labor force of Sterling has the largest number
                                                                              of the population working in the management, business, science and art sector followed
                                                                              by the service sector and sales and office sector. Only 20% of residents in Sterling 
                                                                              work in the natural resources, construction, and maintenance field as well as the 
                                                                              production, transportation, and material moving field. ",  style = "padding-top:15px;font-size: 14px;"),
                                                                            p("For Sterling residents who commute to work, over half have a commute that is less than 30 
                                                                              minutes and 75% of said commuters drove alone. Notably, only 2.8% of commuters utilized public 
                                                                              transportation.",style = "padding-top:15px;font-size: 14px;"),
                                                                            ),
                                                                     
                                                                      )
                                                              ),
                                            
                                            
                                            
                                          )),
                                   
                          )
                 ),
                 
                 
                 navbarMenu("Community Schools",
                            tabPanel("Demographics", 
                                     fluidRow(style = "margin: 6px;",
                                              column(12, 
                                                     h1(strong("Elementary Students in Community Schools Characteristics"), align = "center"),
                                                     h2(strong("What Do Community Schools Look Like?"), align = "left"),
                                                     p("Community schools are schools that are available in low-income areas that provide resources and accommodation for the students and families who attend their schools. These schools not only focus on students learning, but may provide free meals, health care services, tutoring, and counseling services, to those in need. In Sterling, there are six Title 1 Community Schools. Those schools are Forest Grove Elementary, Guilford Elementary, Rolling Ridge Elementary, Sterling Elementary, Sugarland Elementary, and Sully Elementary. "),
                                                     
                                                     
                                                     tabsetPanel(
                                                       
                                                       tabPanel("Demographics",
                                                                fluidRow(style = "margin: 4px;",
                                                                         
                                                                         column(7, align = "left",
                                                                                selectInput("schooldrop1", "Select Demographic:", width = "100%", choices = c(
                                                                                  "Gender" = "cgender",
                                                                                  "Race/Ethnicity" = "racenine",
                                                                                  "Hispanic Population" = "chispanic"
                                                                                  
                                                                                ),
                                                                                ),
                                                                                
                                                                                withSpinner(plotlyOutput("ocuplot1", height = "500px", width = "100%")),
                                                                                withSpinner(leafletOutput("ocuplot3", height = "500px", width = "60%")),
                                                                                column(12,align = "right",
                                                                                       p("Source: Virginia Department of Education, Loudoun County Public Schools Dashboard and Staff directory", style = "font-size:12px;"),
                                                                                       )
                                                                         ),
                                                                         
                                                                         column(5, align = "justify",
                                                                                p("To understand the population of the six elementary schools, we looked at the demographics and 
                                                                                  compared them to one another. The race and ethnicity demographics in 2019-2020 revealed that overall, 
                                                                                  Hispanic students, made up the greatest percentage of students attending the six elementary schools 
                                                                                  in Sterling, which differs from the general make-up of the Sterling area where White residents made
                                                                                  up the majority of residents. ", style = "padding-top:15px;font-size: 14px;"),
                                                                                p("To break this down, we mapped the schools, and collected the total Hispanic population between the 
                                                                                  years 2016 to 2020. We found that the area where Rolling Ridge is located has the largest population
                                                                                  of Hispanic identifying people. This is followed closely by Sterling Elementary and Forest Grove 
                                                                                  Elementary.", 
                                                                                  style = "padding-top:15px;font-size: 14px;"),
                                                                                
                                                                                
                                                                         )
                                                                         
                                                                )
                                                       ),
                                                       tabPanel("Education",
                                                                fluidRow(style = "margin: 4px;",
                                                                         column(7, align = "left",
                                                                                selectInput("schooldrop2", "Select Characteristic:", width = "100%", choices = c(
                                                                                  "Educators" = "cteacher",
                                                                                  "Enrollment" = "cenrol", 
                                                                                  "Absences" = "attend", 
                                                                                  "Chronic Absenteeism" = "chronic"
                                                                                  
                                                                                ),
                                                                                ),
                                                                                
                                                                                withSpinner(plotlyOutput("ocuplot2", height = "500px", width = "100%")),
                                                                                column(12,align = "right",
                                                                                       p("Source: Virginia Department of Education, Loudoun County Public Schools Dashboard and Staff directory", style = "font-size:12px;"),
                                                                                )
                                                                                
                                                                         ),
                                                                         
                                                                         column(5, align = "justify",
                                                                                p("", style = "padding-top:15px;font-size: 14px;"),
                                                                                p("To further breakdown the enrollment statistics, we used the LCPS Dashboard data to visualize the student’s absences by quarter at each school during the 2021 to 2022 school year.   While Rolling Ridge, the green line,   Sugarland, the dark blue line,   and Guilford, the yellow line,   all saw spikes in absences during quarter two,   it was Sully, the pink line,   Sterling, the light blue line,   and Forest Grove, the orange line,   that had a steady increase in absences over the 4 quarters. However it is important to note that this data came from the school year during the COVID-19 pandemic.
To determine if this issue was chronic,   we used Virginia Department of Education data from prior to the pandemic on chronic absenteeism.   This is defined as the percentage of students who miss more than 10% of total classes throughout the year.   This data revealed to us that Sugarland and Rolling Ridge Elementary continued to have a high number of absences prior to the pandemic, suggesting that this may be an area to look for possible service gaps.
", style = "padding-top:15px;font-size: 14px;"),
                                                                                
                                                                                
                                                                         )
                                                                         
                                                                )
                                                       )
                                                       
                                                     )),
                                              
                                              
                                              
                                              
                                     )), 
                            
                            tabPanel("Climate Survey Reports",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              
                                              
                                              
                                              tabsetPanel(
                                                tabPanel("Parent Climate Survey",
                                                         fluidRow(style = "margin: 2px;",
                                                                  fluidRow(style = "margin: 2px;",
                                                                           align = "center",
                                                                           # br("", style = "padding-top:2px;"),
                                                                           # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                                                           br(""),
                                                                           h1(strong("Parent Climate Survey"),
                                                                              #h2("") ,
                                                                              h4("Parent Climate Survey Results From The 2019 to 2020 School Year"),
                                                                              h4(""),
                                                                              #h4("[updat this]"),
                                                                              br()
                                                                           )
                                                                  ),
                                                                  p("", style = "padding-top:10px;"),
                                                                  column(6, align = "center",h4(strong("")),
                                                                         p(""),
                                                                         selectInput("surveydrop1", "Select Survey Question", width = "60%", choices = c(
                                                                           "Academic Support" = "parentanswer1",
                                                                           "Communications" = "parentanswer2",
                                                                           "Relationships" = "parentanswer3",
                                                                           "Instructions" = "parentanswer4"
                                                                         ),
                                                                         ),
                                                                         withSpinner(plotlyOutput("survo1", height = "275px", width ="100%")),
                                                                         
                                                                         
                                                                         
                                                                  ),
                                                                  column(6, align = "center",h4(strong("")),
                                                                         p(""),
                                                                         selectInput("surveydrop2", "Select Survey Question", width = "60%", choices = c(
                                                                           "Academic Support" = "parentanswer1",
                                                                           "Communications" = "parentanswer2",
                                                                           "Relationships" = "parentanswer3",
                                                                           "Instructions" = "parentanswer4"
                                                                         ),
                                                                         ),
                                                                         withSpinner(plotlyOutput("survo2", height = "275px", width ="100%")),
                                                                         
                                                                         
                                                                         
                                                                  ),
                                                                  column(6, align = "center",h4(strong("")),
                                                                         p(""),
                                                                         selectInput("surveydrop3", "Select Survey Question", width = "60%", choices = c(
                                                                           "Academic Support" = "parentanswer1",
                                                                           "Communications" = "parentanswer2",
                                                                           "Relationships" = "parentanswer3",
                                                                           "Instructions" = "parentanswer4"
                                                                         ),
                                                                         ),
                                                                         withSpinner(plotlyOutput("survo3", height = "275px", width ="100%")),
                                                                         
                                                                         
                                                                         
                                                                  ),
                                                                  column(6, align = "center",h4(strong("")),
                                                                         p(""),
                                                                         selectInput("surveydrop4", "Select Survey Question", width = "60%", choices = c(
                                                                           "Academic Support" = "parentanswer1",
                                                                           "Communications" = "parentanswer2",
                                                                           "Relationships" = "parentanswer3",
                                                                           "Instructions" = "parentanswer4"
                                                                         ),
                                                                         ),
                                                                         withSpinner(plotlyOutput("survo4", height = "275px", width ="100%")),
                                                                         
                                                                         
                                                                         
                                                                  ),
                                                         )
                                                ),
                                                tabPanel("Student Climate Survey",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 2px;",
                                                                  align = "center",
                                                                  # br("", style = "padding-top:2px;"),
                                                                  # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                                                  br(""),
                                                                  h1(strong("Student Climate Survey"),
                                                                     #h2("") ,
                                                                     h4("Student Climate Survey Results From The 2019 to 2020 School Year"),
                                                                     h4(""),
                                                                     #h4("[updat this]"),
                                                                     br()
                                                                  )
                                                         ),
                                                         column(6, align = "center",h4(strong("")),
                                                                p(""),
                                                                selectInput("surveydrop5", "Select Survey Question", width = "60%", choices = c(
                                                                  "Student Engagement" = "studentanswer1",
                                                                  "Teacher Relationship" = "studentanswer2",
                                                                  "Social-Emotional Wellbeing" = "studentanswer3",
                                                                  "Bullying" = "studentanswer5"
                                                                ),
                                                                ),
                                                                withSpinner(plotlyOutput("survo5", height = "275px", width ="100%")),
                                                                
                                                                
                                                                
                                                         ),
                                                         column(6, align = "center",h4(strong("")),
                                                                p(""),
                                                                selectInput("surveydrop6", "Select Survey Question", width = "60%", choices = c(
                                                                  "Student Engagement" = "studentanswer1",
                                                                  "Teacher Relationship" = "studentanswer2",
                                                                  "Social-Emotional Wellbeing" = "studentanswer3",
                                                                  "Bullying" = "studentanswer5"
                                                                ),
                                                                ),
                                                                withSpinner(plotlyOutput("survo6", height = "275px", width ="100%")),
                                                                
                                                                
                                                                
                                                         ),
                                                         column(6, align = "center",h4(strong("")),
                                                                p(""),
                                                                selectInput("surveydrop7", "Select Survey Question", width = "60%", choices = c(
                                                                  "Student Engagement" = "studentanswer1",
                                                                  "Teacher Relationship" = "studentanswer2",
                                                                  "Social-Emotional Wellbeing" = "studentanswer3",
                                                                  "Bullying" = "studentanswer5"
                                                                ),
                                                                ),
                                                                withSpinner(plotlyOutput("survo7", height = "275px", width ="100%")),
                                                                
                                                                
                                                                
                                                         ),
                                                         column(6, align = "center",h4(strong()),
                                                                p(""),
                                                                selectInput("surveydrop8", "Select Survey Question", width = "60%", choices = c(
                                                                  "Student Engagement" = "studentanswer1",
                                                                  "Teacher Relationship" = "studentanswer2",
                                                                  "Social-Emotional Wellbeing" = "studentanswer3",
                                                                  "Bullying" = "studentanswer5"
                                                                ),
                                                                ),
                                                                withSpinner(plotlyOutput("survo8", height = "275px", width ="100%")),
                                                                
                                                                
                                                                
                                                         ),
                                                ), 
                                                
                                                tabPanel("Teacher/Staff Climate Survey",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 2px;",
                                                                  align = "center",
                                                                  # br("", style = "padding-top:2px;"),
                                                                  # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                                                  br(""),
                                                                  h1(strong("Teacher/Staff Climate Survey"),
                                                                     #h2("") ,
                                                                     h4("Teacher/Staff Climate Survey Results From The 2019 to 2020 School Year"),
                                                                     h4(""),
                                                                     #h4("[updat this]"),
                                                                     br()
                                                                  )
                                                         ),
                                                         column(6, align = "center",h4(strong("")),
                                                                p(""),
                                                                selectInput("surveydrop9", "Select Survey Question", width = "60%", choices = c(
                                                                  "Staff Collegiality" = "teacherandstaffanswer1",
                                                                  "Academic Environment" = "teacherandstaffanswer2",
                                                                  "School Leadership" = "teacherandstaffanswer3",
                                                                  "Managing Student Behavior" = "teacherandstaffanswer4",
                                                                  "Workplace Environment" = "teacherandstaffanswer6",
                                                                  "Instructional Environment" = "teacherandstaffanswer7"
                                                                ),
                                                                ),
                                                                withSpinner(plotlyOutput("survo9", height = "275px", width ="100%")),
                                                                
                                                                
                                                                
                                                                
                                                         ),
                                                         column(6, align = "center",h4(strong("")),
                                                                p(""),
                                                                selectInput("surveydrop10", "Select Survey Question", width = "60%", choices = c(
                                                                  "Staff Collegiality" = "teacherandstaffanswer1",
                                                                  "Academic Environment" = "teacherandstaffanswer2",
                                                                  "School Leadership" = "teacherandstaffanswer3",
                                                                  "Managing Student Behavior" = "teacherandstaffanswer4",
                                                                  "Workplace Environment" = "teacherandstaffanswer6",
                                                                  "Instructional Environment" = "teacherandstaffanswer7"
                                                                ),
                                                                ),
                                                                withSpinner(plotlyOutput("survo10", height = "275px", width ="100%")),
                                                                
                                                                
                                                                
                                                                
                                                         ),
                                                         column(6, align = "center",h4(strong("")),
                                                                p(""),
                                                                selectInput("surveydrop11", "Select Survey Question", width = "60%", choices = c(
                                                                  "Staff Collegiality" = "teacherandstaffanswer1",
                                                                  "Academic Environment" = "teacherandstaffanswer2",
                                                                  "School Leadership" = "teacherandstaffanswer3",
                                                                  "Managing Student Behavior" = "teacherandstaffanswer4",
                                                                  "Workplace Environment" = "teacherandstaffanswer6",
                                                                  "Instructional Environment" = "teacherandstaffanswer7"
                                                                ),
                                                                ),
                                                                withSpinner(plotlyOutput("survo11", height = "275px", width ="100%")),
                                                                
                                                                
                                                                
                                                                
                                                         ),
                                                         column(6, align = "center",h4(strong("")),
                                                                p(""),
                                                                selectInput("surveydrop12", "Select Survey Question", width = "60%", choices = c(
                                                                  "Staff Collegiality" = "teacherandstaffanswer1",
                                                                  "Academic Environment" = "teacherandstaffanswer2",
                                                                  "School Leadership" = "teacherandstaffanswer3",
                                                                  "Managing Student Behavior" = "teacherandstaffanswer4",
                                                                  "Workplace Environment" = "teacherandstaffanswer6",
                                                                  "Instructional Environment" = "teacherandstaffanswer7"
                                                                ),
                                                                ),
                                                                withSpinner(plotlyOutput("survo12", height = "275px", width ="100%")),
                                                                
                                                                
                                                                
                                                                
                                                         ))),
                                              # column(12, 
                                              # h4("References: "),
                                              # p("[1] U.S Department of Education, Office of Elementary and Secondary Education. Full-Service Community Schools Program (FSCS). Retrieved from:", a(href =  "https://oese.ed.gov/offices/office-of-discretionary-grants-support-services/school-choice-improvement-programs/full-service-community-schools-program-fscs/", "https://oese.ed.gov/offices/office-of-discretionary-grants-support-services/school-choice-improvement-programs/full-service-community-schools-program-fscs/"), style = "font-size:12px;"),
                                              # p("[2] Quinn, J., & Blank, M. J. (2020). Twenty years, ten lessons: Community schools as an equitable school improvement strategy.", em("Voices in Urban Education (VUE)."), style = "font-size:12px;")),
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
                 
                 navbarMenu("Availabile Resources",
                            tabPanel("Health and Social Services",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h1(strong("Health and Social Services")),
                                                     p(""),
                                                     h3(strong("Overview"), align = "left"), 
                                                     p(("To understand the services available to those located in Sterling, we used data from our stakeholders as well as publicly available data to provide a general idea of the resources available within the Sterling area. "), align = "left"),
                                                     
                                                     
                                                     
                                                     
                                                     
                                                     
                                              )),
                                     
                                     fluidPage(style = "margin: 2px;", 
                                               column(6, 
                                                      leafletOutput("map_health", width = "100%", height = 600)
                                                      #fluidRow(align = "center",
                                                      #    p(tags$small(em('Last updated: August 2021'))))
                                               ), 
                                               column(6, 
                                                      h3("Services"), 
                                                      
                                                      
                                                      )
                                     )),
                            
                            
                            
                            tabPanel("Mental Health",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h4(strong("Mental Health")),
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
                                              column(12, align = "center",h4(strong("Family Engagement")),
                                                     p(""),
                                                     br("")
                                                     
                                                     
                                                     
                                              )),
                                     
                                     fluidPage(style = "margin: 2px;", 
                                               column(12, 
                                                      leafletOutput("map_family", width = "100%")
                                                      #fluidRow(align = "center",
                                                      #    p(tags$small(em('Last updated: August 2021'))))
                                               )
                                     )
                                     
                                     
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
                 
                 tabPanel("Opportunities",
                          fluidRow(style = "margin: 6px;",
                                   p("", style = "padding-top:10px;"),
                                   column(12, align = "center",h4(strong("Possible Service Opportunities")),
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
  
  output$map_family <- renderLeaflet({
    map_family
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
    input$schooldrop1
  }) 
  
  output$ocuplot1 <- renderPlotly({
    
    if(Var2() == "racenine"){
      racenine 
    }
    else if(Var2() == "cgender"){
      genders 
    }
    
  })
  
  VarSchool <- reactive({
    
    input$schooldrop2
    
  })
  
  output$ocuplot2<- renderPlotly({
    
    if(VarSchool() == "attend"){
      attend 
    }
    
    else if (VarSchool() == "cteacher") {
      
      cteacher
      
    }
    else if (VarSchool() == "chronic") {
      
      chronic
      
      
    }
    else if (VarSchool() == "cenrol") {
      enroll
      
    }
    
  })
  
  
  
  output$ocuplot3 <- renderLeaflet({
    if (Var2() == "chispanic") {
      
      hispanicschool
    }
    
    
  })
  
  #------------Climate Surveys-----------
  
  Answer1 <- reactive({
    input$surveydrop1
  })
  
  output$survo1 <- renderPlotly({
    
    if (Answer1() == "parentanswer1") {
      
      parentanswer1
      
    }
    
    else if (Answer1() == "parentanswer2") {
      
      parentanswer2
    }
    
    else if (Answer1() == "parentanswer3") {
      
      parentanswer3
    }
    
    else if (Answer1() == "parentanswer4") {
      parentanswer4
    }
  })
  
  Answer2 <- reactive({
    input$surveydrop2
  })
  
  output$survo2 <- renderPlotly({
    
    if (Answer2() == "parentanswer1") {
      
      parentanswer1
      
    }
    
    else if (Answer2() == "parentanswer2") {
      
      parentanswer2
    }
    
    else if (Answer2() == "parentanswer3") {
      
      parentanswer3
    }
    
    else if (Answer2() == "parentanswer4") {
      parentanswer4
    }
  })
  
  Answer3 <- reactive({
    input$surveydrop3
  })
  
  output$survo3 <- renderPlotly({
    
    if (Answer3() == "parentanswer1") {
      
      parentanswer1
      
    }
    
    else if (Answer3() == "parentanswer2") {
      
      parentanswer2
    }
    
    else if (Answer3() == "parentanswer3") {
      
      parentanswer3
    }
    
    else if (Answer3() == "parentanswer4") {
      parentanswer4
    }
  })
  
  Answer4 <- reactive({
    input$surveydrop4
  })
  
  output$survo4 <- renderPlotly({
    
    if (Answer4() == "parentanswer1") {
      
      parentanswer1
      
    }
    
    else if (Answer4() == "parentanswer2") {
      
      parentanswer2
    }
    
    else if (Answer4() == "parentanswer3") {
      
      parentanswer3
    }
    
    else if (Answer4() == "parentanswer4") {
      parentanswer4
    }
  })
  
  Answer5 <- reactive({
    input$surveydrop5
  }) 
  
  output$survo5 <- renderPlotly({
    
    if (Answer5() == "studentanswer1") {
      
      studentanswer1
      
    }
    
    else if (Answer5() == "studentanswer2") {
      
      studentanswer2
    }
    
    else if (Answer5() == "studentanswer3") {
      
      studentanswer3
    }
    
    else if (Answer5() == "studentanswer5") {
      studentanswer5
    }
  })
  
  Answer6 <- reactive({
    input$surveydrop6
  }) 
  
  output$survo6 <- renderPlotly({
    
    if (Answer6() == "studentanswer1") {
      
      studentanswer1
      
    }
    
    else if (Answer6() == "studentanswer2") {
      
      studentanswer2
    }
    
    else if (Answer6() == "studentanswer3") {
      
      studentanswer3
    }
    
    else if (Answer6() == "studentanswer5") {
      studentanswer5
    }
  })
  
  Answer7 <- reactive({
    input$surveydrop7
  }) 
  
  output$survo7 <- renderPlotly({
    
    if (Answer7() == "studentanswer1") {
      
      studentanswer1
      
    }
    
    else if (Answer7() == "studentanswer2") {
      
      studentanswer2
    }
    
    else if (Answer7() == "studentanswer3") {
      
      studentanswer3
    }
    
    else if (Answer7() == "studentanswer5") {
      studentanswer5
    }
  })
  
  Answer8 <- reactive({
    input$surveydrop8
  }) 
  
  output$survo8 <- renderPlotly({
    
    if (Answer8() == "studentanswer1") {
      
      studentanswer1
      
    }
    
    else if (Answer8() == "studentanswer2") {
      
      studentanswer2
    }
    
    else if (Answer8() == "studentanswer3") {
      
      studentanswer3
    }
    
    else if (Answer8() == "studentanswer5") {
      studentanswer5
    }
  })
  
  Answer9 <- reactive({
    input$surveydrop9
  })
  
  output$survo9 <- renderPlotly({
    
    if (Answer9() == "teacherandstaffanswer1") {
      
      teacherandstaffanswer1
      
    }
    
    else if (Answer9() == "teacherandstaffanswer2") {
      
      teacherandstaffanswer2
    }
    
    else if (Answer9() == "teacherandstaffanswer3") {
      
      teacherandstaffanswer3
    }
    
    else if (Answer9() == "teacherandstaffanswer4") {
      teacherandstaffanswer4
    }
    
    else if (Answer9() == "teacherandstaffanswer6") {
      teacherandstaffanswer6
    }
    
    else if (Answer9() == "teacherandstaffanswer7") {
      teacherandstaffanswer7
    }
  })
  
  Answer10 <- reactive({
    input$surveydrop10
  })
  
  output$survo10 <- renderPlotly({
    
    if (Answer10() == "teacherandstaffanswer1") {
      
      teacherandstaffanswer1
      
    }
    
    else if (Answer10() == "teacherandstaffanswer2") {
      
      teacherandstaffanswer2
    }
    
    else if (Answer10() == "teacherandstaffanswer3") {
      
      teacherandstaffanswer3
    }
    
    else if (Answer10() == "teacherandstaffanswer4") {
      teacherandstaffanswer4
    }
    
    else if (Answer10() == "teacherandstaffanswer6") {
      teacherandstaffanswer6
    }
    
    else if (Answer10() == "teacherandstaffanswer7") {
      teacherandstaffanswer7
    }
  })
  
  Answer11 <- reactive({
    input$surveydrop11
  })
  
  output$survo11 <- renderPlotly({
    
    if (Answer11() == "teacherandstaffanswer1") {
      
      teacherandstaffanswer1
      
    }
    
    else if (Answer11() == "teacherandstaffanswer2") {
      
      teacherandstaffanswer2
    }
    
    else if (Answer11() == "teacherandstaffanswer3") {
      
      teacherandstaffanswer3
    }
    
    else if (Answer11() == "teacherandstaffanswer4") {
      teacherandstaffanswer4
    }
    
    else if (Answer11() == "teacherandstaffanswer6") {
      teacherandstaffanswer6
    }
    
    else if (Answer11() == "teacherandstaffanswer7") {
      teacherandstaffanswer7
    }
  })
  
  Answer12 <- reactive({
    input$surveydrop12
  })
  
  output$survo12 <- renderPlotly({
    
    if (Answer12() == "teacherandstaffanswer1") {
      
      teacherandstaffanswer1
      
    }
    
    else if (Answer12() == "teacherandstaffanswer2") {
      
      teacherandstaffanswer2
    }
    
    else if (Answer12() == "teacherandstaffanswer3") {
      
      teacherandstaffanswer3
    }
    
    else if (Answer12() == "teacherandstaffanswer4") {
      teacherandstaffanswer4
    }
    
    else if (Answer12() == "teacherandstaffanswer6") {
      teacherandstaffanswer6
    }
    
    else if (Answer12() == "teacherandstaffanswer7") {
      teacherandstaffanswer7
    }
  })
  
  
  #---------word clouds-----------------
  
  output$cloud1 <- renderWordcloud2(
    cloud1
  )
  
  
}
shinyApp(ui = ui, server = server)




