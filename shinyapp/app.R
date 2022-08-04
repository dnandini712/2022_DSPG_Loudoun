#==========DSPG 2022============LOUDOUN========================================#

#This dashboard is arranged in the following way:
#1. Loading Packages which are required--------
#2. Loading the data and making the visualizations-----------this is according to order in the shinyapp 
#3. JSCODE 
#4. USER INTERFACE    (Search for 'XXX Tab' and it will take you to the UI for that tab)
#5. Server (not in order of the shinyapp but the OutputIds from the UI will help to locate the code in server. )

#For this repo, all the visualizations are made beforehand and in the server these graphs are just called. 
#Nothing is calculated in the server. 

#For the isochrones to run: install the following packages 
#install.packages("remotes")
#remotes::install_github("tlorusso/traveltimeR")


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
library(collapsibleTree)

#---------------------------------------------------------------

prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")


#Data -----------------------------------------------------------

#The Initiative Tab Data ------------------------------------------

#Sterling’s Elementary Community Schools-----------------------------------------------------

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

va20_2 <- readRDS(paste0(getwd(),"/data/va20_2.RDS" )) %>%
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

#Sociodemographics Tab Data
#Demographic Subtab Data

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

agesterling<- plot_ly(subset_sterling1,x=~Age1, y=~Percent1,type = "bar", color = ~Age1, hoverinfo = "text",text = ~paste("Percent:",Percent1,"%","<br>","Age:",Age1 )) %>% layout(title = "Age Distribution",xaxis = list(title=""), yaxis = list(title = "Percentage"))


#-----------Race/Ethnicity--------------------

labelsR = c("White", "Black", "Am.Indian", "Asian","Hawaiian","Other")
valuesRACEPIE = c(18138, 3132, 418, 5313, 144, 4570)

race <- plot_ly(type='pie', labels=labelsR, values=valuesRACEPIE, 
                textinfo='label+percent',
                hoverinfo = 'text', 
                text = ~paste('Total:', valuesRACEPIE),
                
                
                insidetextorientation='radial') %>% layout(title ='Race/Ethnicity Composition 2019', legend=list(title=list(text='Select Race')))

#------------Race/Ethnicity - Hispanic Percentage-------------

labelsHispanicPIE = c('Hispanic or Latino','Not Hispanic or Latino')
valuesHispanicPIE = c(12472, 17799)

HispanicPercentagePIE <- plot_ly(type='pie', labels=labelsHispanicPIE, values=valuesHispanicPIE, 
                                 textinfo='label+percent',
                                 insidetextorientation='radial',
                                 hoverinfo = 'text', 
                                 
                                 
                                 text = ~paste('Total Population:', valuesHispanicPIE)) %>% layout(title ='Hispanic Population In Sterling 2019', legend=list(title=list(text='')))






#Income Sub tab----

#------------------educational attainment-------------------------

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

#-------family income--------------------------------
medianin <- read_excel(paste0(getwd(),"/data/incomemedian.xlsx"))

subset_medianin <- medianin[4:13, 1:5]
subset_medianin
mi_pop <- subset_medianin$...4
mi_cat <- subset_medianin$...1


mi_cat.fac <- factor(mi_cat, levels = c(mi_cat))
subset_medianin$new_pop<-gsub("%$","",subset_medianin$...4)
pop_nop <- subset_medianin$new_pop
pop_num <- as.numeric(pop_nop)
income <- plot_ly(subset_medianin,x=~mi_cat.fac,y=~pop_num,color = ~mi_cat.fac,type = "bar", hoverinfo = "text",text = ~paste("Percent:",pop_num,"%","<br>","Income Level:",mi_cat.fac)) %>% layout(title = "Median Income Distribution",xaxis = list(title="") ,yaxis= list(title = "Percentage"))


#------------Housing Occupancy---------------------------

lbls.HOUSING = c("Owners", "Renters")
slices.HOUSING = c(6839, 2412)

housing <- plot_ly(type='pie', labels=lbls.HOUSING, values=slices.HOUSING, 
                   textinfo='label+percent',
                   insidetextorientation='radial') %>% layout(title ='Housing Occupancy', legend=list(title=list(text='Occupants')))


#---------Property Value---------------------------------

dfpv <- read_excel(paste0(getwd(), "/data/Property_Value.xlsx"), col_names = TRUE)
Numberpv=c(58,6,46,204,1137,4653,709,26)

figpv <- dfpv %>% plot_ly(labels = ~`HOUSING OCCUPANCY`, values = ~dfpv$count, sort = FALSE, direction = "counterclockwise", marker = list(line = list(width = 1, pull = 3)), hoverinfo = 'text', text = ~paste('Number of Property Values:', Numberpv), textinfo = "percent")
figpv <- figpv %>% add_pie(hole = 0.5, domain = list(x = c(0.25,1), y = c(0,0.9)))
property <- figpv %>% layout(title = "Residential Property Value (PV)", showlegend = TRUE, 
                             legend=list(title=list(text='Select Value')),
                             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#-------------Property Value  - Prop. Value Comparison---------------------

propcomparison <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = 378700,
  title = list(text = "Sterling & Loudoun's Median PV"),
  type = "indicator",
  mode = "gauge+number+delta",
  delta = list(reference = 534600),
  gauge = list(
    axis =list(range = list(NULL, 600000)),
    steps = list(
      list(range = c(0, 250000), color = "lightgray"),
      list(range = c(250000, 350000), color = "gray"),
      list(range = c(350000, 500000), color = "yellow"),
      list(range = c(500000, 600000), color = "red")),
    threshold = list(
      line = list(color = "black", width = 4),
      thickness = 0.75,
      value = 534600))) 
propcomparison <- propcomparison %>%
  layout(margin = list(l=20,r=30))


#------------------poverty status-------------------------------

poverty_as1<- read_excel(paste0(getwd(),"/data/povertybyageandsexnewss.xlsx"), 
                         sheet = "Data")

subset_poverty_as1 <- poverty_as1[3:28, 1:4]

#subset_poverty_as$Estimate[1:26]
povas_pop1 <- subset_poverty_as1$Estimate
povas_pop1 <- as.numeric(povas_pop1)
povas_cat1 <- subset_poverty_as1$Label

Total1 <- povas_pop1

cat1 <- as.character(povas_cat1)

pov <- plot_ly(subset_poverty_as1, x = cat1, y = Total1, color = ~Sex, type = "bar", hoverinfo = "text",text = ~paste("Total:",Total1,"<br>","Age:",cat1,"<br>","Sex:",Sex)) %>% layout(title = "Poverty by Age and Sex",xaxis = list(title="",barmode = "group", categoryorder = "array", categoryarray = cat1))

#---------------------Health Coverage----------------------------


sterling <- read_excel(paste0(getwd(),"/data/Employmentsterling.xlsx"),skip=2,col_names=TRUE)
subset_sterling <- sterling[c(103:105),]
subset_sterling$...4 <- as.numeric(gsub("%", "", subset_sterling$...4) )
subset_sterling$`EMPLOYMENT STATUS` <- reorder(subset_sterling$`EMPLOYMENT STATUS`, subset_sterling$...4)
health <- ggplot(subset_sterling, aes(x =`EMPLOYMENT STATUS`,y = (subset_sterling$...4), fill = `EMPLOYMENT STATUS`)) + geom_bar(position = "stack", stat="identity", width = 0.5, aes(text = paste0(...4, "%"))) + labs(x = "Insurance Type", y= "Percentage", caption = " Source : DP03 ACS 5 -yr data 2016-2020") + ggtitle("Distribution of Health Insurance") + guides(fill = guide_legend(title = ""))+ theme(axis.text.y = element_text(angle=0), axis.ticks.y= element_blank())+ coord_flip()#


healthin <- ggplotly(health, tooltip = c("text"))

#Occupation/Work Subtab------------------------

#------------------Employment-----------------
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

#-------------Commuter Time------------------------------

labelsCT = c("Less than 10 minutes","10 to 14 minutes","15 to 19 minutes","20 to 24 minutes", "25 to 29 minutes", "30 to 34 minutes", "35 to 44 minutes", "45 to 59 minutes", "60 or more minutes")
valuesCT = c(6, 18, 17, 13, 5, 13, 7, 10, 11)
valuestotalCT = c(989,2968,2787,2177,792,2160,1154,1649,1813.9)

commutertime <- plot_ly(type='funnelarea', labels=labelsCT, values=valuesCT, sort = FALSE, direction = "",
                        textinfo='percent',
                        hoverinfo = 'text', text = ~paste('Total Number of Commuters:', valuestotalCT),
                        insidetextorientation='radial') %>% layout(title ='Commuter Time to Work', showlegend=TRUE, legend=list(title=list(text='')), legend=list(x=1, y=0.5))


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
                        textinfo='text') %>% layout(title ='Mode of Transportation to Work', legend=list(title=list(text='')), hoverinfo = "none")


#SCHOOLS TAB

#Demographics sub tab 


#---------gender by school-------------------------------------------------


genders <- data.frame(Sex=rep(c("Male", "Female"), each=6),
                      School=c("Sugarland","Rolling Ridge","Guilford","Sterling","Sully","Forest Grove"),
                      Total=c(251, 266, 254, 208,233, 253, 221, 245, 276, 178, 215, 251),
                      Percentage = c(53.2, 52.1, 47.9, 53.9, 52, 50.2, 46.8, 47.9, 52.1, 46.1, 48, 49.8)
)


genders<- ggplot(data=genders, aes(x=School, y=Total, fill = forcats::fct_rev(Sex), group=Sex, width=0.9)) +
  geom_bar(stat="identity", position=position_stack(reverse = TRUE), hoverinfo = "text", aes(text = paste("Percentage :",Percentage,"%\n", "Total :", Total))) +
  scale_fill_manual(values = c('#20AFCC','#F56D4F')) + labs(y="Total Students", x="", fill="")+ggtitle("Gender by Schools for 2021-2022") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

genders <-ggplotly(genders, tooltip = c("text"))



#-------------------------------Race/Ethnicity -  race by school ----------------------------

races <- read_excel(paste0(getwd(),"/data/racedems.xlsx"))

race_subset <- races[(1:153),c(1,5,6,11)]
nineteensub <- race_subset[(1:24),(1:4)]
Race <- nineteensub$Race
Percentage <- nineteensub$Percentage
School <- nineteensub$`School Name`
racenine <- ggplot(nineteensub,aes(x=School,y=Percentage,fill=Race, group=Race))+ geom_col(position = "dodge",aes(text = paste0("Percentage:",Percentage, "%", "\n","Race:",Race,"\n","School:",School)))+labs(title="Race/Ethnicity Demographics for 2019-2020",y="Percentage",x = "",caption = "Source: VDOE Fall Membership Report 2016-2020") + theme(plot.caption.position = "plot",
                                                                                                                                                                                                                                                                                                                                                          plot.caption = element_text(hjust = 1)) + guides(fill=guide_legend(title="Race/Ethnicity"))
racenine <- ggplotly(racenine,tooltip=c("text"))

#-----------Race/Ethnicity - hispanic population-----------------

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
  addMarkers( ~Longitude, ~Latitude, popup = popups, label = ~as.character(Name), labelOptions = FALSE) 
#English Learners (EL)-------------

generaldata <- read_excel(paste0(getwd(),"/data/generaldata.xlsx"),skip=0,col_names=TRUE)
subset_englishlearnerstatus <- generaldata[3:6,1:2]

xELS <- subset_englishlearnerstatus$`School Year`
yELS <- subset_englishlearnerstatus$Percentage
dataELS <- data.frame(xELS, yELS)

figELS <- plot_ly(dataELS, 
                  x = ~xELS, 
                  y = ~yELS,
                  type = 'scatter', 
                  mode = 'lines',
                  fill = 'tozeroy',
                  fillcolor = 'rgba(114,186,59,0.5)',
                  line = list(color = 'rgb(114,186,59)'),
                  text = ~paste("Year:", subset_englishlearnerstatus$`School Year`, "<br>", "Percentage:", subset_englishlearnerstatus$Percentage,"%"),
                  hoverinfo = 'text')

figELS <- figELS %>% layout(
  title = "English Learners(ELs)",
  yaxis = list(
    title = "Percentage",
    zerolinewidth =60,
    standoff = 25,
    range = list(60,75),
    tickvals = list(60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85),
    zeroline = F
  ),
  xaxis = list(
    title = "", 
    zeroline = T, 
    tickangle = 40,
    zerolinewidth = 60,
    standoff = 25,
    showgrid = T
  )
)


#------------------------------IEP Status------------------------------------

generaldata <- read_excel(paste0(getwd(),"/data/generaldata.xlsx"),skip=0,col_names=TRUE)
subset_IEPstatus <- generaldata[11:14,1:2]

xIEP <- subset_IEPstatus$`School Year`
yIEP <- subset_IEPstatus$Percentage
dataIEP <- data.frame(xIEP, yIEP)

figIEP <- plot_ly(dataIEP, 
                  x = ~xIEP, 
                  y = ~yIEP,
                  type = 'scatter', 
                  mode = 'lines',
                  fill = 'tozeroy',
                  fillcolor = 'rgba(114,186,59,0.5)',
                  line = list(color = 'rgb(114,186,59)'),
                  text = ~paste("Year:", subset_IEPstatus$`School Year`, "<br>", "Percentage:", subset_IEPstatus$Percentage,"%"),
                  hoverinfo = 'text')

figIEP <- figIEP %>% layout(
  title = "Individual Education Plan (IEP) Status",
  yaxis = list(
    title = "Percentage",
    range = list(8,13),
    tickvals = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
    zeroline = F
  ),
  xaxis = list(
    title = "", 
    zeroline = T, 
    tickangle = 40, 
    zerolinewidth = 60,
    standoff = 25,
    showgrid = T
  )
)


#-------------------------Homelessness-------------------------------------------

generaldata <- read_excel(paste0(getwd(),"/data/generaldata.xlsx"),skip=0,col_names=TRUE)
subset_homeless <- generaldata[27:30,1:2]

xHOME <- subset_homeless$`School Year`
yHOME <- subset_homeless$Percentage
dataHOME <- data.frame(xHOME, yHOME)

figHOME <- plot_ly(dataHOME, 
                   x = ~xHOME, 
                   y = ~yHOME,
                   type = 'scatter', 
                   mode = 'lines',
                   fill = 'tozeroy',
                   fillcolor = 'rgba(114,186,59,0.5)',
                   line = list(color = 'rgb(114,186,59)'),
                   text = ~paste("Year:", subset_homeless$`School Year`, "<br>", "Percentage:", subset_homeless$Percentage, "%"))

figHOME <- figHOME %>% layout(
  title = "Students Facing Homelessness",
  yaxis = list(
    title = "Percentage",
    range = list(6,17),
    tickvals = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
    zeroline = TRUE
  ),
  xaxis = list(
    title = "", 
    zeroline = T, 
    tickangle = 40,
    zerolinewidth = 60,
    standoff = 25,
    showgrid = T
  )
)

#Usage of School Programs sub tab 

#-------------------------Free and Reduced-Price Breakfast Data------------------------------------

breakfast_data <- read_excel(paste0(getwd(),"/data/Breakfast.xlsx"),skip=0,col_names=TRUE)
breakfast <- plot_ly(breakfast_data, x = ~Year, y = ~Percent, color = ~School, type = 'scatter', mode = 'bars', hoverinfo = "text", text = ~paste("School:", School, "<br>", "Percentage: ", Percent, "%"))%>% layout(title = "Free and Reduced-Price Breakfast Participation  ", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#--------------------------Free And Reduced-Price Lunch----------------------------

generaldata <- read_excel(paste0(getwd(),"/data/generaldata.xlsx"),skip=0,col_names=TRUE)
subset_freereducedlunch <- generaldata[19:22,1:2]

xFRL <- subset_freereducedlunch$`School Year`
yFRL <- subset_freereducedlunch$Percentage
dataFRL <- data.frame(xFRL, yFRL)

figFRL <- plot_ly(dataFRL, 
                  x = ~xFRL, 
                  y = ~yFRL,
                  type = 'scatter', 
                  mode = 'lines',
                  fill = 'tozeroy',
                  fillcolor = 'rgba(114,186,59,0.5)',
                  line = list(color = 'rgb(114,186,59)'),
                  text = ~paste("Year:", subset_freereducedlunch$`School Year`, "<br>",  "Percentage:", subset_freereducedlunch$Percentage, "%"),
                  hoverinfo = 'text')

figFRL <- figFRL %>% layout(
  title = "Free and Reduced-Price Lunch Overall Participation",
  yaxis = list(
    title = "Percentage",
    range = list(65,77),
    tickvals = list(65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80),
    zeroline = F
  ),
  xaxis = list(
    title = "", 
    zeroline = T, 
    tickangle = 40,
    zerolinewidth = 60,
    standoff = 25,
    showgrid = T
  )
)


#----------------------------Free And Reduced-Price Lunch by Schools-------------------------------------

eligablelunch_data <- read_excel(paste0(getwd(),"/data/LunchParticipation.xlsx"),skip=1,col_names=TRUE)

eligablelunch_data <- plot_ly(eligablelunch_data, x = ~Year, y = ~Percent, color = ~School, type = 'scatter', mode = 'bars', hoverinfo = "text", text = ~paste("School:", School, "<br>", "Percentage: ", Percent, "%"))%>% layout(title = "Free and Reduced-Price Lunch Participation by Schools", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(50,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

# weekend meals --------------------------------

healthscrape <- read_excel(paste0(getwd(),"/data/manualscrappingdata.xlsx"))
subset_healthscrape <- healthscrape[2:5,c(2,5)]
Totalw <- subset_healthscrape$...5
Yearw <- subset_healthscrape$...2
plot_ly(data = subset_healthscrape, x = ~Yearw, y = ~Totalw, type = "scatter",mode="line",hoverinfo = "text",text = ~paste("Year:",Yearw,"Total:",Totalw)) %>% layout(yaxis = list(tickvals = list(100,200,300,400,500,600,700,800,900)),title = "Families Who Received Weekend Meals") -> weekendmeals

#clothing and basic supplies---
subset_healthscrape2 <- healthscrape[c(2,4),c(2,4)]
Year2 <- subset_healthscrape2$...2
Total2 <- subset_healthscrape2$...4
plot_ly(data = subset_healthscrape2,x = ~Year2,y = ~Total2,type = "bar", hoverinfo = "text", text = ~paste("Year:",Year2,"Total:",Total2)) %>% layout(yaxis = list(tickvals = list(400,450,500,550,600,650,700,750,800,850,900),title = "Total"),title = "Families Received Help with Clothing, Shoes, and Basic Supplies",xaxis = list(title = "Year")) -> basicsupplies

#Performance sub tab 

# Size sub sub tab 


#-------------------enrollment-----------------

enrollment <- read_excel(paste0(getwd(),"/data/Enrollment16-20.xlsx"))
enr_total <- enrollment$Total
School <- enrollment$Schools
Year <- enrollment$Year
enroll <- plot_ly(enrollment, x = ~Year,y = ~Total, color = ~School, type = 'scatter',mode = 'lines', hoverinfo="text", text = ~paste("Total:", Total, "<br>", "School:",School)) %>% layout(title= "Enrollment", xaxis = list(title = ""), yaxis = list(title = "Total Students"), legend=list(title=list(text='Select School')))


#---------------Educators--------------------------

Schools <- c("Sterling", "Sugarland", "Rolling Ridge", "Forest Grove", "Guilford", "Sully")
Teachers <- c(32, 52, 66, 55, 59, 35)
Staff <- c(49, 22, 27, 20, 29, 19)
dataSTAFF <- data.frame(Schools, Teachers, Staff)

figSTM <- plot_ly(dataSTAFF, x = ~Schools, y = ~Teachers, type = 'bar', name = 'Teachers', marker = list(color = 'rgb(255, 2, 2 )'), textinfo = "total",
                  hoverinfo = 'text')
#text = ~paste('Total:',))

figSTM <- figSTM %>% add_trace(y = ~Staff, name = 'Staff', marker = list(color = 'rgb(253, 151, 12 )'))
cteacher <- figSTM %>% layout(title = "Total Teachers and Staff 2021-2022", yaxis = list(title = 'Total Educators'), xaxis = list(title = ''), barmode = 'stack')

#Absences
#-------------------Absences --------------

attendance <- read_excel(paste0(getwd(),"/data/absencerate.xlsx"))
att_per <- attendance$`Absence Rate`
Percent <- att_per*100
Quarter <- attendance$`School Quarter`
School <- attendance$`School Name`
ggplot(attendance,aes(x=quarter,y=att_rate,group=School,color=School))+geom_point()+geom_line() +labs(title = "Student Absences by 2020-2021 Quarter",caption= "Source: LCPS Dashboard 2021-2022",x="Quarter",y="Percentage") + theme(plot.caption.position = "plot",
                                                                                                                                                                                                                                      plot.caption = element_text(hjust = 1)) + scale_fill_brewer(palette = "Set1")
attend <- plot_ly(attendance,x = ~Quarter, y = ~Percent, color  = ~School, type = 'scatter',mode = 'lines',hoverinfo = "text",text = ~paste("Percent:",Percent, "%","<br>","School:",School)) %>% layout(title = "Student Absences by 2020-2021 Quarter", legend=list(title=list(text='Select School')), yaxis = list(title = "Percentage"), xaxis = list(title = ""))



##--------Chronic absenteeism------------------

chronic <- data.frame(sex=rep(c("Missed less than 10%"), each=6),
                      School=c("Sugarland","Rolling Ridge","Guilford","Sterling","Sully","Forest Grove"),
                      Percent=c(11.1, 10.1, 6.7, 5.8, 9.7,7.9))

chronic<- ggplot(data=chronic, aes(x=School, y=Percent, fill=School,  width=0.8)) +
  geom_bar(stat="identity",hoverinfo = "text", aes(text = paste("School :",School,"\n", "Percent :", Percent, "%")))  + labs(y="", x="", fill="")+ggtitle("Chronic Absenteeism by Schools for 2018-2019") 

chronic1<-ggplotly(chronic, tooltip = c("text"))


absentieesm <- read_excel(paste0(getwd(),"/data/Chronicabseetism.xlsx"),skip=0,col_names=TRUE)

absentieesm %>% filter(Subgroup == "All Students") -> absentieesm

chronic <- plot_ly(absentieesm, x = ~Year, y = ~`Percent above 10`, color = ~School, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent above 10`, "%", "<br>", "School:",School))%>% layout(title = "Chronic Absenteeism", xaxis = list(title = ""), yaxis = list(title="Percentage"))



#-----------------Assessment sub sub tab -------------------------
#----------------all students-------------------------------
#-------------------students by race----------------------
#------------------Mathematics-------------------------------


#assessment <- read_excel(paste0(getwd(),"/data/Assessments.xlsx"),skip=0,col_names=TRUE)

assessment <- read_excel(paste0(getwd(),"/data/assessmentrace.xlsx"),skip=0,col_names=TRUE)

assessment %>% filter(School == "Forest Grove" & Subject == "Mathematics") %>% filter(Subgroup == "Hispanic"| Subgroup == "White" |Subgroup == "Black"| Subgroup == "Asian") -> assessmentraceForestGrove

forestgroverace <- plot_ly(assessmentraceForestGrove, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))%>% add_annotations(
  x=.85,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
)

#----------------Sugarland------------------------------------------------

#assessment <- read_excel(paste0(getwd(),"/data/Assessments.xlsx"),skip=0,col_names=TRUE)

assessment %>% filter(School == "Sugarland" & Subject == "Mathematics") %>% filter(Subgroup == "Hispanic"| Subgroup == "White" |Subgroup == "Black"| Subgroup == "Asian") -> assessmentraceSugarland

sugarlandrace <- plot_ly(assessmentraceSugarland, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))%>% add_annotations(
  x=.85,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
)

#-------------------Guilford------------------------------------

#assessment <- read_excel(paste0(getwd(),"/data/Assessments.xlsx"),skip=0,col_names=TRUE)

assessment %>% filter(School == "Guilford" & Subject == "Mathematics") %>% filter(Subgroup == "Hispanic"| Subgroup == "White" |Subgroup == "Black"| Subgroup == "Asian") -> assessmentraceGuilford

guilfordrace <- plot_ly(assessmentraceGuilford, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))%>% add_annotations(
  x=.7,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
)

#-------------------------------Rolling Ridge------------------

#assessment <- read_excel(paste0(getwd(),"/data/Assessments.xlsx"),skip=0,col_names=TRUE)

assessment %>% filter(School == "Rolling Ridge" & Subject == "Mathematics") %>% filter(Subgroup == "Hispanic"| Subgroup == "White" |Subgroup == "Black"| Subgroup == "Asian") -> assessmentraceRollingRidge

rrrace <- plot_ly(assessmentraceRollingRidge, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#-----------------------------Sterling--------------------

#assessment <- read_excel(paste0(getwd(),"/data/Assessments.xlsx"),skip=0,col_names=TRUE)

assessment %>% filter(School == "Sterling" & Subject == "Mathematics") %>% filter(Subgroup == "Hispanic"| Subgroup == "White" |Subgroup == "Black"| Subgroup == "Asian") -> assessmentraceSterling

sterlingrace <- plot_ly(assessmentraceSterling, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))%>% add_annotations(
  x=.85,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
)

#----------------------Sully--------------------------

#assessment <- read_excel(paste0(getwd(),"/data/Assessments.xlsx"),skip=0,col_names=TRUE)

assessment %>% filter(School == "Sully" & Subject == "Mathematics") %>% filter(Subgroup == "Hispanic"| Subgroup == "White" |Subgroup == "Black"| Subgroup == "Asian") -> assessmentraceSully

sullyrace <- plot_ly(assessmentraceSully, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
)) %>% add_annotations(
  x=-0.1,
  y=2,
  xref = "x",
  yref = "y",
  text = ">",
  #xanchor = 'left',
  showarrow = F
) %>% add_annotations(
  x=0.85,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
)

#------------------English------------------

#-----------------Forest Grove-------------------------------------------
#assessment <- read_excel(paste0(getwd(),"/data/Assessments.xlsx"),skip=0,col_names=TRUE)

assessment <- read_excel(paste0(getwd(),"/data/assessmentrace.xlsx"),skip=0,col_names=TRUE)

assessment %>% filter(School == "Forest Grove" & Subject == "English Reading") %>% filter(Subgroup == "Hispanic"| Subgroup == "White" |Subgroup == "Black"| Subgroup == "Asian") -> assessmentraceForestGroveeng

forestgroveraceeng <- plot_ly(assessmentraceForestGroveeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#----------------Sugarland------------------------------------------------

#assessment <- read_excel(paste0(getwd(),"/data/Assessments.xlsx"),skip=0,col_names=TRUE)

assessment %>% filter(School == "Sugarland" & Subject == "English Reading") %>% filter(Subgroup == "Hispanic"| Subgroup == "White" |Subgroup == "Black"| Subgroup == "Asian") -> assessmentraceSugarlandeng

sugarlandraceeng <- plot_ly(assessmentraceSugarlandeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))%>% add_annotations(
  x=0.85,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
)

#-------------------Guilford------------------------------------

#assessment <- read_excel(paste0(getwd(),"/data/Assessments.xlsx"),skip=0,col_names=TRUE)

assessment %>% filter(School == "Guilford" & Subject == "English Reading") %>% filter(Subgroup == "Hispanic"| Subgroup == "White" |Subgroup == "Black"| Subgroup == "Asian") -> assessmentraceGuilfordeng

guilfordraceeng <- plot_ly(assessmentraceGuilfordeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#-------------------------------Rolling Ridge------------------

#assessment <- read_excel(paste0(getwd(),"/data/Assessments.xlsx"),skip=0,col_names=TRUE)

assessment %>% filter(School == "Rolling Ridge" & Subject == "English Reading") %>% filter(Subgroup == "Hispanic"| Subgroup == "White" |Subgroup == "Black"| Subgroup == "Asian") -> assessmentraceRollingRidgeeng

rrraceeng <- plot_ly(assessmentraceRollingRidgeeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#-----------------------------Sterling--------------------

#assessment <- read_excel(paste0(getwd(),"/data/Assessments.xlsx"),skip=0,col_names=TRUE)

assessment %>% filter(School == "Sterling" & Subject == "English Reading") %>% filter(Subgroup == "Hispanic"| Subgroup == "White" |Subgroup == "Black"| Subgroup == "Asian") -> assessmentraceSterlingeng

sterlingraceeng <- plot_ly(assessmentraceSterlingeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))%>% add_annotations(
  x=.85,
  y=2,
  xref = "x",
  yref = "y",
  text = ">",
  xanchor = 'left',
  showarrow = F
)

#----------------------Sully--------------------------

#assessment <- read_excel(paste0(getwd(),"/data/Assessments.xlsx"),skip=0,col_names=TRUE)

assessment %>% filter(School == "Sully" & Subject == "English Reading") %>% filter(Subgroup == "Hispanic"| Subgroup == "White" |Subgroup == "Black"| Subgroup == "Asian") -> assessmentraceSullyeng

sullyraceeng <- plot_ly(assessmentraceSullyeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
)) %>% add_annotations(
  x=-0.1,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  #xanchor = 'left',
  showarrow = F
) %>% add_annotations(
  x=0.85,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
)



#--------assessment by School and Gender-----------------

#-------------Mathematics-------------------------------
assessment <- read_excel(paste0(getwd(),"/data/assessmentgender.xlsx"),skip=0,col_names=TRUE)
assessment %>% filter(School == "Forest Grove" & Subject == "Mathematics") %>% filter(Subgroup == "Male"| Subgroup == "Female") -> assessmentgenderForestGrove

forestgrovegender <- plot_ly(assessmentgenderForestGrove, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#----------------Sugarland------------------------------------------------

assessment %>% filter(School == "Sugarland" & Subject == "Mathematics") %>% filter(Subgroup == "Male"| Subgroup == "Female") -> assessmentgenderSugarland

sugarlandgender <- plot_ly(assessmentgenderSugarland, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#-------------------Guilford------------------------------------

assessment %>% filter(School == "Guilford" & Subject == "Mathematics") %>% filter(Subgroup == "Male"| Subgroup == "Female") -> assessmentgenderGuilford

guilfordgender <- plot_ly(assessmentgenderGuilford, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#-------------------------------Rolling Ridge------------------

assessment %>% filter(School == "Rolling Ridge" & Subject == "Mathematics") %>% filter(Subgroup == "Male"| Subgroup == "Female") -> assessmentgenderRollingRidge

rrgender <- plot_ly(assessmentgenderRollingRidge, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#-----------------------------Sterling--------------------

assessment %>% filter(School == "Sterling" & Subject == "Mathematics") %>% filter(Subgroup == "Male"| Subgroup == "Female") -> assessmentgenderSterling

sterlinggender <- plot_ly(assessmentgenderSterling, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#----------------------Sully--------------------------

assessment %>% filter(School == "Sully" & Subject == "Mathematics") %>% filter(Subgroup == "Male"| Subgroup == "Female") -> assessmentgenderSully

sullygender <- plot_ly(assessmentgenderSully, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#------------------English------------------

#-----------------Forest Grove-------------------------------------------

assessment %>% filter(School == "Forest Grove" & Subject == "English Reading") %>% filter(Subgroup == "Male"| Subgroup == "Female") -> assessmentgenderForestGroveeng

forestgrovegendereng <- plot_ly(assessmentgenderForestGroveeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#----------------Sugarland------------------------------------------------


assessment %>% filter(School == "Sugarland" & Subject == "English Reading") %>% filter(Subgroup == "Male"| Subgroup == "Female") -> assessmentgenderSugarlandeng

sugarlandgendereng <- plot_ly(assessmentgenderSugarlandeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#-------------------Guilford------------------------------------

assessment %>% filter(School == "Guilford" & Subject == "English Reading") %>% filter(Subgroup == "Male"| Subgroup == "Female") -> assessmentgenderGuilfordeng

guilfordgendereng <- plot_ly(assessmentgenderGuilfordeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#-------------------------------Rolling Ridge------------------

assessment %>% filter(School == "Rolling Ridge" & Subject == "English Reading") %>% filter(Subgroup == "Male"| Subgroup == "Female") -> assessmentgenderRollingRidgeeng

rrgendereng <- plot_ly(assessmentgenderRollingRidgeeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#-----------------------------Sterling--------------------


assessment %>% filter(School == "Sterling" & Subject == "English Reading") %>% filter(Subgroup == "Male"| Subgroup == "Female") -> assessmentgenderSterlingeng

sterlinggendereng <- plot_ly(assessmentgenderSterlingeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#----------------------Sully--------------------------


assessment %>% filter(School == "Sully" & Subject == "English Reading") %>% filter(Subgroup == "Male"| Subgroup == "Female") -> assessmentgenderSullyeng

sullygendereng <- plot_ly(assessmentgenderSullyeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))


#--------performance / assessment by Other Subgroups All Students, Homeless, Disabilities-----------------


#-------------Mathematics-------------------------------

#Forest Grove

assessment <- read_excel(paste0(getwd(),"/data/assessmentsubgroup.xlsx"),skip=0,col_names=TRUE)

assessment %>% filter(School == "Forest Grove" & Subject == "Mathematics") %>% filter(Subgroup == "All Students"|Subgroup == "Homeless"|Subgroup == "Students with Disabilities"|Subgroup == "Economically Disadvantaged"|Subgroup == "English Learners")-> assessmentallForestGrove

forestgroveall <- plot_ly(assessmentallForestGrove, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#----------------Sugarland------------------------------------------------

assessment %>% filter(School == "Sugarland" & Subject == "Mathematics") %>% filter(Subgroup == "All Students"|Subgroup == "Homeless"|Subgroup == "Students with Disabilities"|Subgroup == "Economically Disadvantaged"|Subgroup == "English Learners") -> assessmentallSugarland

sugarlandall <- plot_ly(assessmentallSugarland, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#-------------------Guilford------------------------------------

assessment %>% filter(School == "Guilford" & Subject == "Mathematics") %>% filter(Subgroup == "All Students"|Subgroup == "Homeless"|Subgroup == "Students with Disabilities"|Subgroup == "Economically Disadvantaged"|Subgroup == "English Learners") -> assessmentallGuilford

guilfordall <- plot_ly(assessmentallGuilford, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#-------------------------------Rolling Ridge------------------

assessment %>% filter(School == "Rolling Ridge" & Subject == "Mathematics") %>% filter(Subgroup == "All Students"|Subgroup == "Homeless"|Subgroup == "Students with Disabilities"|Subgroup == "Economically Disadvantaged"|Subgroup == "English Learners") -> assessmentallRollingRidge

rrall <- plot_ly(assessmentallRollingRidge, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
)) %>% add_annotations(
  x=0.1,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
) %>% add_annotations(
  x=0.99,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
)

#-----------------------------Sterling--------------------

assessment %>% filter(School == "Sterling" & Subject == "Mathematics") %>% filter(Subgroup == "All Students"|Subgroup == "Homeless"|Subgroup == "Students with Disabilities"|Subgroup == "Economically Disadvantaged"|Subgroup == "English Learners") -> assessmentallSterling

sterlingall <- plot_ly(assessmentallSterling, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))%>% add_annotations(
  x=1.3,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
)

#----------------------Sully--------------------------

assessment %>% filter(School == "Sully" & Subject == "Mathematics") %>% filter(Subgroup == "All Students"|Subgroup == "Homeless"|Subgroup == "Students with Disabilities"|Subgroup == "Economically Disadvantaged"|Subgroup == "English Learners") -> assessmentallSully

sullyall <- plot_ly(assessmentallSully, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "Mathematics Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#------------------English------------------

#-----------------Forest Grove-------------------------------------------

assessment %>% filter(School == "Forest Grove" & Subject == "English Reading") %>% filter(Subgroup == "All Students"|Subgroup == "Homeless"|Subgroup == "Students with Disabilities"|Subgroup == "Economically Disadvantaged"|Subgroup == "English Learners") -> assessmentallForestGroveeng

forestgrovealleng <- plot_ly(assessmentallForestGroveeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))%>% add_annotations(
  x=1.0,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
)

#----------------Sugarland------------------------------------------------


assessment %>% filter(School == "Sugarland" & Subject == "English Reading") %>% filter(Subgroup == "All Students"|Subgroup == "Homeless"|Subgroup == "Students with Disabilities"|Subgroup == "Economically Disadvantaged"|Subgroup == "English Learners") -> assessmentallSugarlandeng

sugarlandalleng <- plot_ly(assessmentallSugarlandeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
)) %>% add_annotations(
  x=1.3,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
)

#-------------------Guilford------------------------------------

assessment %>% filter(School == "Guilford" & Subject == "English Reading") %>% filter(Subgroup == "All Students"|Subgroup == "Homeless"|Subgroup == "Students with Disabilities"|Subgroup == "Economically Disadvantaged"|Subgroup == "English Learners") -> assessmentallGuilfordeng

guilfordalleng <- plot_ly(assessmentallGuilfordeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))

#-------------------------------Rolling Ridge------------------

assessment %>% filter(School == "Rolling Ridge" & Subject == "English Reading") %>% filter(Subgroup == "All Students"|Subgroup == "Homeless"|Subgroup == "Students with Disabilities"|Subgroup == "Economically Disadvantaged"|Subgroup == "English Learners") -> assessmentallRollingRidgeeng

rralleng <- plot_ly(assessmentallRollingRidgeeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
)) %>% add_annotations(
  x=0.1,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
) %>% add_annotations(
  x=0.99,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
)

#-----------------------------Sterling--------------------


assessment %>% filter(School == "Sterling" & Subject == "English Reading") %>% filter(Subgroup == "All Students"|Subgroup == "Homeless"|Subgroup == "Students with Disabilities"|Subgroup == "Economically Disadvantaged"|Subgroup == "English Learners") -> assessmentallSterlingeng

sterlingalleng <- plot_ly(assessmentallSterlingeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))%>% add_annotations(
  x=1.3,
  y=2,
  xref = "x",
  yref = "y",
  text = "<",
  xanchor = 'left',
  showarrow = F
)

#----------------------Sully--------------------------


assessment %>% filter(School == "Sully" & Subject == "English Reading") %>% filter(Subgroup == "All Students"|Subgroup == "Homeless"|Subgroup == "Students with Disabilities"|Subgroup == "Economically Disadvantaged"|Subgroup == "English Learners") -> assessmentallSullyeng

sullyalleng <- plot_ly(assessmentallSullyeng, x = ~Year, y = ~`Percent Pass`, color = ~Subgroup, type = 'bar', mode = 'stack', hoverinfo = "text", text = ~paste("Percentage: ", `Percent Pass`, "%", "<br>", "School: ", School))%>% layout(title = "English Reading Pass Rate", xaxis = list(title = ""), yaxis = list(
  title = "Percentage",
  #zerolinewidth =60,
  #standoff = 25,
  range = list(0,90),
  tickvals = list(0,10,20,30,40,50,60,70,80,90)
  #zeroline = F
))


#Climate Survey sub tab 

#--------------Teacher/Staff Climate Surveys------------------------
newsurveydata <- read_excel(paste0(getwd(), "/data/NewSurveyData.xlsx"),skip=0,col_names=TRUE)
subsetnewsurveydataSTAFF <- newsurveydata[3:8,c(1,2:5,7:8)]

#Teacher & Staff Survey Q1 - Staff Collegiality
staffquestion1 <- subsetnewsurveydataSTAFF[1:6,1:2]
question1 <- staffquestion1$SCHOOLS
staffquestion1percentage <- staffquestion1$`Question 1`
staffquestion1percentage <- as.numeric(staffquestion1percentage)
staffquestion1percentage <- staffquestion1percentage*100
one <- ggplot(staffquestion1,aes(x=question1,y=staffquestion1percentage,fill=question1, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",staffquestion1$SCHOOLS)))+labs(title="Staff Collegiality",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion1percentage, y = staffquestion1percentage), size = 3, position = position_stack(vjust = 1.02))
teacherandstaffanswer1 <- ggplotly(one, tooltip = c("text"))

#Teacher & Staff Survey Q2 - Academic Environment
staffquestion2 <- subsetnewsurveydataSTAFF[1:6,c(1,3)]
question2 <- staffquestion2$SCHOOLS
staffquestion2percentage <- staffquestion2$`Question 2`
staffquestion2percentage <- as.numeric(staffquestion2percentage)
staffquestion2percentage <- staffquestion2percentage*100
two <- ggplot(staffquestion2,aes(x=question2,y=staffquestion2percentage,fill=question2, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",staffquestion2$SCHOOLS)))+labs(title="Academic Environment",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion2percentage, y = staffquestion2percentage), size = 3, position = position_stack(vjust = 1.02))
teacherandstaffanswer2 <- ggplotly(two, tooltip = c("text"))

#Teacher & Staff Survey Q3 - School Leadership
staffquestion3 <- subsetnewsurveydataSTAFF[1:6,c(1,4)]
question3 <- staffquestion3$SCHOOLS
staffquestion3percentage <- staffquestion3$`Question 3`
staffquestion3percentage <- as.numeric(staffquestion3percentage)
staffquestion3percentage <- staffquestion3percentage*100
three <- ggplot(staffquestion3,aes(x=question3,y=staffquestion3percentage,fill=question3, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",staffquestion3$SCHOOLS)))+labs(title="School Leadership",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion3percentage, y = staffquestion3percentage), size = 3, position = position_stack(vjust = 1.02))
teacherandstaffanswer3 <- ggplotly(three, tooltip = c("text"))

#Teacher & Staff Survey Q4 - Managing Student Behavior
staffquestion4 <- subsetnewsurveydataSTAFF[1:6,c(1,5)]
question4 <- staffquestion4$SCHOOLS
staffquestion4percentage <- staffquestion4$`Question 4`
staffquestion4percentage <- as.numeric(staffquestion4percentage)
staffquestion4percentage <- staffquestion4percentage*100
four <- ggplot(staffquestion4,aes(x=question4,y=staffquestion4percentage,fill=question4, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",staffquestion4$SCHOOLS)))+labs(title="Managing Student Behavior",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion4percentage, y = staffquestion4percentage), size = 3, position = position_stack(vjust = 1.02))
teacherandstaffanswer4 <- ggplotly(four, tooltip = c("text"))

#Teacher & Staff Survey Q6 - Workplace Environment
staffquestion6 <- subsetnewsurveydataSTAFF[1:6,c(1,6)]
question6 <- staffquestion6$SCHOOLS
staffquestion6percentage <- staffquestion6$`Question 6`
staffquestion6percentage <- as.numeric(staffquestion6percentage)
staffquestion6percentage <- staffquestion6percentage*100
six <- ggplot(staffquestion6,aes(x=question6,y=staffquestion6percentage,fill=question6, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",staffquestion6$SCHOOLS)))+labs(title="Workplace Environment",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion6percentage, y = staffquestion6percentage), size = 3, position = position_stack(vjust = 1.02))
teacherandstaffanswer6 <- ggplotly(six, tooltip = c("text"))

#Teacher & Staff Survey Q7 - Instructional Practices 
staffquestion7 <- subsetnewsurveydataSTAFF[1:6,c(1,7)]
question7 <- staffquestion7$SCHOOLS
staffquestion7percentage <- staffquestion7$`Question 7`
staffquestion7percentage <- as.numeric(staffquestion7percentage)
staffquestion7percentage <- staffquestion7percentage*100
seven <- ggplot(staffquestion7,aes(x=question7,y=staffquestion7percentage,fill=question7, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",staffquestion7$SCHOOLS)))+labs(title="Instructional Environment",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = staffquestion7percentage, y = staffquestion7percentage), size = 3, position = position_stack(vjust = 1.02))
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
eight <- ggplot(parentquestion1,aes(x=question8,y=parentquestion1percentage,fill=question8, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",parentquestion1$SCHOOLS)))+labs(title="Academic Support",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = parentquestion1percentage, y = parentquestion1percentage), size = 3, position = position_stack(vjust = 1.02))
parentanswer1 <- ggplotly(eight, tooltip = c("text"))

#Parent Survey Q2 - Communications
parentquestion2 <- subsetnewsurveydataPARENT[1:6,c(1,3)]
question9 <- parentquestion2$SCHOOLS
parentquestion2percentage <- parentquestion2$`Question 2`
parentquestion2percentage <- as.numeric(parentquestion2percentage)
parentquestion2percentage <- parentquestion2percentage*100
nine <- ggplot(parentquestion2,aes(x=question9,y=parentquestion2percentage,fill=question9, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",parentquestion2$SCHOOLS)))+labs(title="Communications",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = parentquestion2percentage, y = parentquestion2percentage), size = 3, position = position_stack(vjust = 1.02))
parentanswer2 <- ggplotly(nine, tooltip = c("text"))

#Parent Survey Q3 - Relationships
parentquestion3 <- subsetnewsurveydataPARENT[1:6,c(1,4)]
question10 <- parentquestion3$SCHOOLS
parentquestion3percentage <- parentquestion3$`Question 3`
parentquestion3percentage <- as.numeric(parentquestion3percentage)
parentquestion3percentage <- parentquestion3percentage*100
ten <- ggplot(parentquestion3,aes(x=question10,y=parentquestion3percentage,fill=question10, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",parentquestion3$SCHOOLS)))+labs(title="Relationships",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = parentquestion3percentage, y = parentquestion3percentage), size = 3, position = position_stack(vjust = 1.02))
parentanswer3 <- ggplotly(ten, tooltip = c("text"))

#Parent Survey Q4 - Instructions
parentquestion4 <- subsetnewsurveydataPARENT[1:6,c(1,5)]
question11 <- parentquestion4$SCHOOLS
parentquestion4percentage <- parentquestion4$`Question 4`
parentquestion4percentage <- as.numeric(parentquestion4percentage)
parentquestion4percentage <- parentquestion4percentage*100
eleven <- ggplot(parentquestion4,aes(x=question11,y=parentquestion4percentage,fill=question11, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",parentquestion4$SCHOOLS)))+labs(title="Instructions",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = parentquestion4percentage, y = parentquestion4percentage), size = 3, position = position_stack(vjust = 1.02))
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
twelve <- ggplot(studentquestion1,aes(x=question12,y=studentquestion1percentage,fill=question12, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",studentquestion1$SCHOOLS)))+labs(title="Student Engagement",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = studentquestion1percentage, y = studentquestion1percentage), size = 3, position = position_stack(vjust = 1.02))
studentanswer1 <- ggplotly(twelve, tooltip = c("text"))

#Student Survey Q2 - Student-Teacher Relationship
studentquestion2 <- subsetnewsurveydataSTUDENT[1:6,c(1,3)]
question13 <- studentquestion2$SCHOOLS
studentquestion2percentage <- studentquestion2$`Question 2`
studentquestion2percentage <- as.numeric(studentquestion2percentage)
studentquestion2percentage <- studentquestion2percentage*100
thirteen <- ggplot(studentquestion2,aes(x=question13,y=studentquestion2percentage,fill=question13, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",studentquestion2$SCHOOLS)))+labs(title="Student-Teacher Relationship",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = studentquestion2percentage, y = studentquestion2percentage), size = 3, position = position_stack(vjust = 1.02))
studentanswer2 <- ggplotly(thirteen, tooltip = c("text"))

#Student Survey Q3 - Social-Emotional Wellbeing
studentquestion3 <- subsetnewsurveydataSTUDENT[1:6,c(1,4)]
question14 <- studentquestion3$SCHOOLS
studentquestion3percentage <- studentquestion3$`Question 3`
studentquestion3percentage <- as.numeric(studentquestion3percentage)
studentquestion3percentage <- studentquestion3percentage*100
fourteen <- ggplot(studentquestion3,aes(x=question14,y=studentquestion3percentage,fill=question14, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",studentquestion3$SCHOOLS)))+labs(title="Social-Emotional Wellbeing",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = studentquestion3percentage, y = studentquestion3percentage), size = 3, position = position_stack(vjust = 1.02))
studentanswer3 <- ggplotly(fourteen, tooltip = c("text"))

#Student Survey Q5 - Bullying
studentquestion5 <- subsetnewsurveydataSTUDENT[1:6,c(1,5)]
question16 <- studentquestion5$SCHOOLS
studentquestion5percentage <- studentquestion5$`Question 5`
studentquestion5percentage <- as.numeric(studentquestion5percentage)
studentquestion5percentage <- studentquestion5percentage*100
sixteen <- ggplot(studentquestion5,aes(x=question16,y=studentquestion5percentage,fill=question16, width = 0.70)) +geom_col(hoverinfo = "text", aes(text = paste("",studentquestion5$SCHOOLS)))+labs(title="Bullying",x="",y="Percentage") + scale_fill_discrete(name = "") + geom_text(aes(label = studentquestion5percentage, y = studentquestion5percentage), size = 3, position = position_stack(vjust = 1.02))
studentanswer5 <- ggplotly(sixteen, tooltip = c("text"))

#Representatives'Reports 

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

docs1 <- tm_map(docs1, removeWords, c("to", "challenge", "concern", "level", "aged", "created", "elementary", "basic", "in", "and", "the", "we", "of", "an", "is", "like", "for", "those", "were", "was", "list", "our", "with", "would", "very", "huge","this","same","ongoing","overall", "finding", "continue", "provide"))


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

docs2 <- tm_map(docs2, removeWords, c("day", "now", "help", "pep", "people", "arose", "to", "risen", "offer", "offered", "warm", "spots", "their", "every", "they", "tell", "that", "who", "are", "all", "many", "here", "always", "among", "mtss", "umht", "how", "feel", "adept", "in", "and", "the", "we", "of", "an", "is", "like", "for", "those", "were", "was", "list", "our", "with", "would", "very", "huge","this","same","ongoing","overall", "finding", "hot","wanted", "tiered", "using", "staying", "excellent", "worked", "actively", "throughout"))


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

docs3 <- tm_map(docs3, removeWords, c("better","rolling","return", "above", "will", "covid", "areas", "listed", "input", "to", "per", "pre", "utilize", "most", "also", "more", "many", "ways", "local", "pep", "times", "ridge", "year", "needy", "people", "after", "person", "in", "and", "the", "we", "of", "an", "is", "like", "for", "those", "were", "was", "list", "our", "with", "would", "very", "umht", "hot", "to", "in", "and", "the", "we", "of", "an", "is", "like", "for", "those", "were", "was", "list", "our", "with", "would", "very", "huge","this","same","ongoing","overall", "finding", "their", "from", "always"))


dtm3 <- TermDocumentMatrix(docs3) 
matrix3 <- as.matrix(dtm3) 
words3 <- sort(rowSums(matrix3),decreasing=TRUE) 
df3 <- data.frame(word = names(words3),freq=words3)

cloud3<- wordcloud2(df3, size=0.5)





#----------------------Collapsible Tree - Key Partners and Programs--------------------

Tree <- read_excel(paste0(getwd(),"/data/treedata.xlsx")) 

Tree %>% collapsibleTree(hierarchy = c("Four Pillars", "Name", "Key Partners"),
                         root="Pillar",
                         attribute = "Pillar",
                         width=1800,
                         zoomable=T, 
                         collapsed = T, nodeSize = 'leafCount',
                         
                         fill = c(
                           # The root
                           rep("white", 1),
                           # Unique Pillars
                           rep("firebrick", length(unique(Tree$`Four Pillars`))),
                           # Unique Names of schools
                           rep("steelblue", 24),
                           rep("orange", 71)
                           
                         ))-> tree1
#Resources Tab 

#--------------free or not free resources ---------------------------------------

costs <- read_excel(paste0(getwd(),"/data/resourcecost.xlsx"))
foods <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"))
#---------------map_health and isochrones-----------------------------------------

#install.packages("remotes")
#remotes::install_github("tlorusso/traveltimeR")

YourAPIKey <- "32f6ed99d0636fe05d01a5ff5a99c6e7"
YourAppId <- "190b7348"

traveltime10 <- read_sf("data/iso_10_sterling.shp")
traveltime20 <- read_sf("data/iso_20_sterling.shp")
traveltime45 <- read_sf("data/iso_45_sterling.shp")
# traveltime10 <- traveltime_map(appId=YourAppId,
#                                apiKey=YourAPIKey,
#                                location=c(39.009006,-77.4029155),
#                                traveltime=600,
#                                type="driving",
#                                departure="2022-08-09T08:00:00+01:00")
# # ... and within 60 minutes?
# traveltime20 <- traveltime_map(appId=YourAppId,
#                                apiKey=YourAPIKey,
#                                location=c(39.009006,-77.4029155),
#                                traveltime=1200,
#                                type="driving",
#                                departure="2022-08-09T08:00:00+01:00")
# traveltime45 <- traveltime_map(appId = YourAppId,
#                                apiKey = YourAPIKey,
#                                location = c(39.009006,-77.4029155),
#                                traveltime= 2700,
#                                type = "driving",
#                                departure = "2022-08-09T08:00:00+01:00")
map<- read_excel(paste0(getwd(),"/data/school_locations.xlsx"))

subset_map <- map[1,c(1,4,5)]

healthsep <- read_excel(paste0(getwd(),"/data/healthsep.xlsx"))
popups <- lapply(
  paste("<strong>Name: </strong>",
        str_to_title(healthsep$Name1),
        "<br />",
        "<strong>Description:</strong>",
        healthsep$Description1 ,
        "<br />",
        "<strong>Serves:</strong>",
        healthsep$Serves1, 
        "<br />",
        "<strong>Hours:</strong>",
        healthsep$Hours1,
        "<br />",
        "<strong>Language:</strong>",
        healthsep$Language,
        "<br />",
        "<strong>Address:</strong>",
        healthsep$Address1,
        "<a href = ",healthsep$Website1, "> Website </a>",
        "<br />"),
  
  htmltools::HTML
)

popup <- lapply(
  paste("<strong>Name: </strong>",
        str_to_title(costs$Name),
        "<br />",
        "<strong>Description:</strong>",
        costs$Description ,
        "<br />",
        "<strong>Serves:</strong>",
        costs$Serves, 
        "<br />",
        "<strong>Hours:</strong>",
        costs$Hours,
        "<br />",
        "<strong>Language:</strong>",
        costs$Language,
        "<br />",
        "<strong>Address:</strong>",
        costs$Address,
        "<a href = ",costs$Website, "> Website </a>",
        "<br />"),
  
  htmltools::HTML
)

healthfree <- read_excel(paste0(getwd(), "/data/resourcecost.xlsx"),sheet = "Health Free")



pal <- colorFactor(c("#91003f", "#005824", "#d7301f","#CC6677","#DDCC77","#88419d"), domain = c("Food Pantry", "Clothing", "Counseling","Medical Services","Vision Care","Dental Care"))
pal1 <- colorFactor(c("#91003f","#005824","#d7301f","#88419d","#DDCC77","#CC6677","#AA4499","#882255"),domain = c("Food Pantry","Clothing","Counseling","Dental Care","Vision Care","Medical Services","Speech and Hearing Services","Physical Therapy"))


leaflet(data = costs) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = va20_2,
              color="#5f308f",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% 
  addPolygons(data=traveltime20, color= "#21618C",opacity = 1,weight=2,fillColor = "white", fillOpacity = .1) %>% addPolygons(data=traveltime10,color="green",opacity=1,weight=2,fillColor = "white",fillOpacity = .1) %>%     addPolygons(data=traveltime45,color="#D98880",opacity = 1,weight = 2,fillColor = "white",fillOpacity = .1) %>%
  setView(-77.4029155,39.009006, zoom = 11)%>%
  addCircleMarkers(data=costs,~Longitude,~Latitude,popup = ~popup, label = ~as.character(Name),group = ~Resource,color = ~pal(Resource),weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1) %>%
  addLayersControl(overlayGroups = c("Food Pantry", "Clothing", "Counseling","Medical Services","Vision Care","Dental Care"),options = layersControlOptions(collapsed = FALSE)) %>% 
  addMarkers(data=subset_map,~Longitude,~Latitude,popup = ~as.character("Sterling Elementary")) %>% addLegend("bottomright",colors=c("green","#21618C","#D98880"),labels=c("10 minutes","20 minutes","45 minutes"),title = "Travel Time")%>%
  setView(-77.4029155,39.009006, zoom = 11) -> health_free



leaflet(data = foods) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = va20_2,
              color="#5f308f",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% 
  addPolygons(data=traveltime20, color= "#21618C",opacity = 1,weight=2,fillColor = "white", fillOpacity = .1) %>% addPolygons(data=traveltime10,color="green",opacity=1,weight=2,fillColor = "white",fillOpacity = .1) %>%     addPolygons(data=traveltime45,color="#D98880",opacity = 1,weight = 2,fillColor = "white",fillOpacity = .1) %>%
  setView(-77.4029155,39.009006, zoom = 11)%>%
  addCircleMarkers(data=foods,~Longitude1,~Latitude1,popup=~popups,label=~as.character(Name1),color= ~pal1(Resource1),group = ~Resource1,weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1)%>%
  addLayersControl(overlayGroups = ~Resource1,options = layersControlOptions(collapsed = FALSE)) %>% addMarkers(data=subset_map,~Longitude,~Latitude,popup = ~as.character("Sterling Elementary")) %>% addLegend("bottomright",colors=c("green","#21618C","#D98880"),labels=c("10 minutes","20 minutes","45 minutes"),title = "Travel Time") -> health_all
#--------------youth development map --------------------------

youth <- read_excel(paste0(getwd(),"/data/Sterling_Youth_Development 3.xlsx"))
popups3 <- lapply(
  paste("<strong>Name: </strong>",
        str_to_title(youth$Name3),
        "<br />",
        "<strong>Description:</strong>",
        youth$Description3 ,
        "<br />",
        "<strong>Hours:</strong>",
        youth$Hours3, 
        "<br />",
        "<strong>Address:</strong>",
        youth$Address3,
        "<a href = ",youth$Website3, "> Website </a>",
        "<br />"),
  
  
  htmltools::HTML
)

pal3 <- colorFactor(c("red","blue","green","orange","purple"),domain = c("Activity","Athletics","Other","Club","After School Program"))

leaflet(data = youth) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = va20_2,
              color="#5f308f",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% 
  addPolygons(data=traveltime20, color= "#21618C",opacity = 1,weight=2,fillColor = "white", fillOpacity = .1) %>% addPolygons(data=traveltime10,color="green",opacity=1,weight=2,fillColor = "white",fillOpacity = .1) %>%     addPolygons(data=traveltime45,color="#D98880",opacity = 1,weight = 2,fillColor = "white",fillOpacity = .1) %>%
  setView(-77.4029155,39.009006, zoom = 11)%>%
  addCircleMarkers(data=youth,~Longitude3,~Latitude3,popup=~popups3,label=~as.character(Name3),color= ~pal3(Type),weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1,group = ~Type)%>%
  addLayersControl(overlayGroups = ~Type,options= layersControlOptions(collapsed = FALSE)) %>%
  addMarkers(data=subset_map,~Longitude,~Latitude,popup = ~as.character("Sterling Elementary")) %>% addLegend("bottomright",colors=c("green","#21618C","#D98880"),labels=c("10 minutes","20 minutes","45 minutes"),title = "Travel Time") -> map_youth


youthfree <- read_excel(paste0(getwd(),"/data/resourcecost.xlsx"), sheet = "Youth Free")

popups4 <- lapply(
  paste("<strong>Name: </strong>",
        str_to_title(youthfree$Name4),
        "<br />",
        "<strong>Description:</strong>",
        youthfree$Description4 ,
        "<br />",
        "<strong>Hours:</strong>",
        youthfree$Hours4, 
        "<br />",
        "<strong>Address:</strong>",
        youthfree$Address4,
        "<a href = ",youthfree$Website4, "> Website </a>",
        "<br />"),
  htmltools::HTML
)

pal3 <- colorFactor(c("red","blue","green","orange","purple"),domain = c("Activity","Athletics","Other","Club","After School Program"))

leaflet(data = youthfree) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = va20_2,
              color="#5f308f",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% 
  addPolygons(data=traveltime20, color= "#21618C",opacity = 1,weight=2,fillColor = "white", fillOpacity = .1) %>% addPolygons(data=traveltime10,color="green",opacity=1,weight=2,fillColor = "white",fillOpacity = .1) %>%     addPolygons(data=traveltime45,color="#D98880",opacity = 1,weight = 2,fillColor = "white",fillOpacity = .1) %>%
  setView(-77.4029155,39.009006, zoom = 11)%>%
  addCircleMarkers(data=youthfree,~Longitude4,~Latitude4,popup=~popups4,label=~as.character(Name4),color= ~pal3(Type),weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1,group = ~Type)%>%
  addLayersControl(overlayGroups = ~Type,options= layersControlOptions(collapsed = FALSE)) %>%
  addMarkers(data=subset_map,~Longitude,~Latitude,popup = ~as.character("Sterling Elementary")) %>% addLegend("bottomright",colors=c("green","#21618C","#D98880"),labels=c("10 minutes","20 minutes","45 minutes"),title = "Travel Time") -> youth_free


#---------mental health resources map------------------------

ment <- read_excel(paste0(getwd(),"/data/mentalhealthres.xlsx"),sheet = "Mental")

popups2 <- lapply(
  paste("<strong>Name: </strong>",
        str_to_title(ment$Name2),
        "<br />",
        "<strong>Description:</strong>",
        ment$Description2 ,
        "<br />",
        "<strong>Hours:</strong>",
        ment$Hours2, 
        "<br />",
        "<strong>Address:</strong>",
        ment$Address2,
        "<a href = ",ment$Website2, "> Website </a>",
        "<br />"),
  
  
  htmltools::HTML
)

pal2 <- colorFactor(c("red", "blue", "green"), domain = c("Family Therapy", "Family Counseling", "Bereavement"))

leaflet(data = ment) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = va20_2,
              color="#5f308f",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% 
  addPolygons(data=traveltime20, color= "#21618C",opacity = 1,weight=2,fillColor = "white", fillOpacity = .1) %>% addPolygons(data=traveltime10,color="green",opacity=1,weight=2,fillColor = "white",fillOpacity = .1) %>%     addPolygons(data=traveltime45,color="#D98880",opacity = 1,weight = 2,fillColor = "white",fillOpacity = .1) %>%
  setView(-77.4029155,39.009006, zoom = 11)%>%
  addCircleMarkers(data=ment,~Longitude2,~Latitude2,popup=~popups2,label=~as.character(Name2),group=~Resources2,color=~pal2(Resources2),weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1)%>%
  addLayersControl(overlayGroups = ~Resources2,options = layersControlOptions(collapsed = FALSE)) %>% 
  addMarkers(data=subset_map,~Longitude,~Latitude,popup = ~as.character("Sterling Elementary")) %>% addLegend("bottomright",colors=c("green","#21618C","#D98880"),labels=c("10 minutes","20 minutes","45 minutes"),title = "Travel Time") -> map_mental

mentfree <- read_excel(paste0(getwd(),"/data/resourcecost.xlsx"), sheet = "Mental Free")

popups5 <- lapply(
  paste("<strong>Name: </strong>",
        str_to_title(mentfree$Name5),
        "<br />",
        "<strong>Description:</strong>",
        mentfree$Description5 ,
        "<br />",
        "<strong>Hours:</strong>",
        mentfree$Hours5, 
        "<br />",
        "<strong>Address:</strong>",
        mentfree$Address5,
        "<a href = ",mentfree$Website5, "> Website </a>",
        "<br />"),
  
  
  htmltools::HTML
)

leaflet(data = mentfree) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = va20_2,
              color="#5f308f",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% 
  addPolygons(data=traveltime20, color= "#21618C",opacity = 1,weight=2,fillColor = "white", fillOpacity = .1) %>% addPolygons(data=traveltime10,color="green",opacity=1,weight=2,fillColor = "white",fillOpacity = .1) %>%     addPolygons(data=traveltime45,color="#D98880",opacity = 1,weight = 2,fillColor = "white",fillOpacity = .1) %>%
  setView(-77.4029155,39.009006, zoom = 11)%>%
  addCircleMarkers(data = mentfree, ~Longitude5,~Latitude5,popup = ~popups5,label = ~as.character(Name5),group = ~Resource5,color = ~pal2(Resource5),weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1)%>%
  addLayersControl(overlayGroups = ~Resource5,options = layersControlOptions(collapsed = FALSE)) %>% 
  addMarkers(data=subset_map,~Longitude,~Latitude,popup = ~as.character("Sterling Elementary")) %>% addLegend("bottomright",colors=c("green","#21618C","#D98880"),labels=c("10 minutes","20 minutes","45 minutes"),title = "Travel Time") -> mental_free


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

pal8 <- colorFactor(c("red", "blue", "green", "orange","purple", "#2e850c"), domain = c("Housing", "Holiday Help", "Education", "Essentials supply", "Employment help", "Other"))

leaflet(data = familyengage) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = va20_2,
              color="#5f308f",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% 
  addPolygons(data=traveltime20, color= "#21618C",opacity = 1,weight=2,fillColor = "white", fillOpacity = .1) %>% addPolygons(data=traveltime10,color="green",opacity=1,weight=2,fillColor = "white",fillOpacity = .1) %>%     addPolygons(data=traveltime45,color="#D98880",opacity = 1,weight = 2,fillColor = "white",fillOpacity = .1) %>%
  setView(-77.4029155,39.009006, zoom = 11)%>%
  addCircleMarkers(data=familyengage,~Longitude,~Latitude,popup=~popups,label=~as.character(Name),group=~Resources,color=~pal8(Resources),weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1)%>%
  addLayersControl(overlayGroups = ~Resources,options = layersControlOptions(collapsed = FALSE)) %>% 
  addMarkers(data=subset_map,~Longitude,~Latitude,popup = ~as.character("Sterling Elementary")) %>% addLegend("bottomright",colors=c("green","#21618C","#D98880"),labels=c("10 minutes","20 minutes","45 minutes"),title = "Travel Time") -> map_family

famfree <-  read_excel(paste0(getwd(),"/data/resourcecost.xlsx"), sheet = "Family Free")

popups9 <- lapply(
  paste("<strong>Name: </strong>",
        str_to_title(famfree$Name8),
        "<br />",
        "<strong>Description:</strong>",
        famfree$Description8 ,
        "<br />",
        "<strong>Hours:</strong>",
        famfree$Hours8, 
        "<br />",
        "<strong>Address:</strong>",
        famfree$Address8,
        "<br />",
        "<a href = ",famfree$Website8, "> Website </a>",
        "<br />",
        "<strong>Serves:</strong>",
        famfree$Serves8),
  
  
  htmltools::HTML
)

pal8 <- colorFactor(c("red", "blue", "green", "orange","purple", "#2e850c"), domain = c("Education", "Employment help","Essentials Supply","Housing", "Holiday Help","Other"))

leaflet(data = famfree) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = va20_2,
              color="#5f308f",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% 
  addPolygons(data=traveltime20, color= "#21618C",opacity = 1,weight=2,fillColor = "white", fillOpacity = .1) %>% addPolygons(data=traveltime10,color="green",opacity=1,weight=2,fillColor = "white",fillOpacity = .1) %>%     addPolygons(data=traveltime45,color="#D98880",opacity = 1,weight = 2,fillColor = "white",fillOpacity = .1) %>%
  setView(-77.4029155,39.009006, zoom = 11)%>%
  addCircleMarkers(data=famfree,~Longitude8,~Latitude8,popup=~popups9,label=~as.character(Name8),group=~Resource8,color=~pal8(Resource8),weight = 7, radius=7, 
                   stroke = F, fillOpacity = 1)%>%
  addLayersControl(overlayGroups = ~Resource8,options = layersControlOptions(collapsed = FALSE)) %>% 
  addMarkers(data=subset_map,~Longitude,~Latitude,popup = ~as.character("Sterling Elementary")) %>% addLegend("bottomright",colors=c("green","#21618C","#D98880"),labels=c("10 minutes","20 minutes","45 minutes"),title = "Travel Time") -> fam_free


#List of Resources ----------------------------
list <- read_excel(paste0(getwd(),"/data/allresources.xlsx")) 

#Recommendations Tab
#-----------------Suggestions Tree-----------------------------

Treesuggestions <- read_excel(paste0(getwd(),"/data/treedata_communityschoolsrecommendations.xlsx")) 

Treesuggestions %>% collapsibleTree(hierarchy = c("Four Pillars", "Resource", "Type of Help", "Action Plans"),
                                    root="Pillar",
                                    attribute = "Pillar",
                                    width=1000,
                                    zoomable=T, 
                                    collapsed = T, nodeSize = 'leafCount',
                                    
                                    fill = c(
                                      # The root
                                      rep("white", 1),
                                      # Unique Pillars
                                      rep("firebrick", length(unique(Treesuggestions$`Four Pillars`))),
                                      # Unique Names of schools
                                      rep("steelblue", length(unique(Treesuggestions$`Resource`))),
                                      rep("orange", length(unique(Treesuggestions$`Type of Help`))),
                                      rep("purple", 95)
                                      
                                    ))-> tree2



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


# user interface-------------------------------------------------------------
ui <- navbarPage(title = "DSPG",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 
                 # Overview tab -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
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
                                   column(3,
                                          h2(strong("The Setting")),align = "center",
                                          
                                          p(a(href = "https://www.loudoun.gov/", strong("Loudoun County"), target = "_blank"), "is located in the northern part of the state of Virginia. It lies along Virginia’s state line, where Virginia meets West Virginia and Maryland. It is also part of the Washington Metropolitan Statistical Area, the sixth largest metropolitan area in the United States. Loudoun County is among the top three most populated county in Virginia with an estimated", a(href = "https://data.census.gov/cedsci/profile?g=0500000US51107", strong("population"), target = "_blank"), "of 420,959.", align = "justify"),
                                          p(a(href = "https://www.loudoun.gov/174/History", strong("Loudoun County"), target = "_blank"), "was traditionally a rural county heavily dependent on agriculture as a primary livelihood. However, this changed in the early 1960s with the construction of the Dulles International Airport. This resulted in an economic boom and rapid growth in the county. Additionally, the metropolitan Washington, D.C. area began growing simultaneously. These two factors led to a significant increase in Loudoun’s population in the 1970s and heavy suburbanization throughout the 1990s. Due to the full-fledged service economy, Loudoun County hosts several world headquarters of high-tech companies, including Verizon Business and Telos Corporation. Notably, the largest", a(href = "https://biz.loudoun.gov/information-center/major-employers/", strong("employer"), target = "_blank"), "is the government sector, with Loudoun County Public Schools being the number one sector with over 10,000 individuals. These factors led to Loudoun being one of the wealthiest county in the United States, with a median average household income of $147,111 as of the 2020 American Community Survey.", align = "justify"),
                                          p("Despite Loudoun county’s wealth, some areas could benefit from improving their economic conditions to be on par with the county. For example, the", a(href = "https://www.livehealthyloudoun.org/indicators/index/view?indicatorId=8483&localeId=202605", strong("Sterling"), target = "_blank"), "region (our area of interest) had 6.9% in 2018 of the households that are below the federal poverty level. While this is substantially low compared to Virginia’s rate of 10%, it is quite high compared to Loudoun’s low rate of 3.2%.", align = "justify"),
                                          
                                   ),
                                   column(6,
                                          h2(strong("Background")), align = "center",
                                          h4(strong("")),
                                          p("Loudoun County Public Schools", a(href = "https://www.lcps.org/loudoun", strong("(LCPS)"), target = "_blank"), "is the third largest school division in Virginia, serving over 80,000 students in 97 facilities. With over 18 high schools, 17 middle schools, 60 elementary schools, and 2 educational centers there are considerable variations in the needs of these schools. For instance, 6 of the 60", a(href = "https://www.lcps.org/Page/834", strong("elementary schools"), target = "_blank"), "in Loudoun are Title 1 Schools. According to the U.S Department of Education", a(href = "https://www2.ed.gov/programs/titleiparta/index.html", strong("Title 1 Schools"), target = "_blank"), "are provided federal funds to school divisions and public schools with high numbers or percentages of children from low-income families to help ensure that all children meet challenging state academic content and achievement standards. Notably, all six of these Title 1 elementary schools are located in the Sterling area in Loudoun county. ", align = "justify"), 
                                          
                                          h4(strong("Sterling")), 
                                          p("The six Title 1 schools in Sterling are Sterling Elementary, Sugarland Elementary, Sully Elementary, Guilford Elementary, Rolling Ridge Elementary, and Forest Grove Elementary. To provide additional resources to these schools, LCPS started a Community Initiative Program (CIP) in 2015. This program is a partnership between school and community resources that focus on academics, health and social services, youth and community development, and community engagement to help improve student learning, strong families, and healthier communities. In 2015, the CIP was supported at one Title one school, Sterling Elementary.  In 2018, the program was expanded to all six Title one elementary schools as part of their Community School Initiative (CSI) with the addition of one full-time social worker to attain a 1 social worker per every 2 schools’ ratio.  Since 2018, LCPS has increased staffing to include one full time social worker, school nurse, and Family Liaison at each Community School in addition to other initiatives designed to support students and families within the community", align = "justify"),
                                          
                                          h4(strong("What is the project question?")),
                                          p("Potential partners of Loudoun County Public Schools may be eager to provide services to the Community Schools; however, a lack of data makes it unclear what resources would be most beneficial for this region. Scraping data and visualizing it would help our stakeholders to find potential improvement opportunities that can help improve the lives of the students in these targeted elementary schools.", align = "justify"),
                                          fluidRow(style = "margin: 12px;",
                                                   column(12, align ="center", 
                                                          img(src='lcps_com_school_initiative.png', width = 210, height = 210)
                                                          
                                                          
                                                   ),
                                          ),
                                   ),  
                                   
                                   column(3,
                                          h2(strong("Project Goals")), align = "center",
                                          p("Our team seeks to design an interactive dashboard that visualizes the resources and services available to the students and families involved in the Loudoun County Community School Initiative. This dashboard will allow stakeholders to understand the primary needs of the community and provide insights into potential opportunities for improvement to increase the quality of life for those impacted by the Community School Initiative. ", align = "justify"),
                                          p("We will use publicly available data, including the American Community Survey and the Virginia Department of Education, to provide our stakeholders with a comprehensive understanding of the factors impacting families in the Sterling area. We focus on sociodemographic indicators such as median income and employment and education indicators such as enrollment and grades to support our analysis. We will also map available services and resource locations relating to distance and travel time. These maps will be broken down into four key areas related to the Community School Program Health and Social Services, Mental Health, Family Engagement, and Youth Development to analyze potential opportunities for service expansion. ", align = "justify"), 
                                          p("Our dashboard will contain our findings and analysis, allowing our stakeholders, Supervisor of Outreach, LCPS, and other interested individuals to understand this information comprehensively and dynamically. This dashboard will also enable the LCPS to leverage target points for further community collaboration and partnerships. For those interested in further research, this repository will help investigate possible underlying factors leading to these differences in accessibility. ", align = "justify"),
                                          #p("")
                                   )
                          ),
                          #fluidRow(align = "center",
                          # p(tags$small(em('Last updated: August 2021'))))
                 ),
                 
                 ## Community Schools Tab--------------------------------------------
                 tabPanel("The Initiative", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   p("", style = "padding-top:10px;"),
                                   column(12, align = "center", h1(strong("Sterling’s Elementary Community Schools"))),
                          ),
                          
                          fluidPage(style = "margin: 12px;",
                                    
                                    column(6, align = "justify", h3(strong("What are Community Schools?")),
                                           
                                           p("Community Schools is a term that describes schools that brings educators, families, community partners, and local government together to address the comprehensive needs of students, families, and communities. The U.S. Department of Education states that the primary purpose of community schools is to “provide comprehensive academic, social, and health services for students, student's family members, and community members that will result in improved educational outcomes for children. Community Schools are expanding across the United States – in 2020, around 10,000 schools have been transformed into community schools (Quinn & Blank, 2020). This is also evident in Virginia, where", a(href = " https://www.cisofva.org/", strong("Community Schools"), target = "_blank"), "served over 58,000 students in 2021, an increase from over 45,000 students in 2019."),
                                           
                                           h3(strong("The Sterling Region")),
                                           
                                           p("The", a(href = "https://www.lcps.org/Page/236420",strong("Community School Initiative"), target = "_blank"), "(CSI) in the Loudoun County Public Schools started in 2015 with Sterling Elementary, a Title 1 school, due to the generous support of 100WomenStrong. According to the Virginia Department of Education, Title 1 schools are provided “financial assistance through state educational agencies to school divisions and public schools with high numbers or percentages of children from low-income families to help ensure that all children meet challenging state academic content and achievement standards.” Title I funds originiate as federal funds."),
                                           br(),
                                           p("The Community School program in Loudoun focuses on four key areas to promote academic achievement: "),
                                           strong(tags$ol(
                                             tags$li(("Health and Social Services" ), style = "font-size:18px;"),
                                             tags$li(("Mental Health"), style = "font-size:18px;"),
                                             tags$li(("Family Engagement"), style = "font-size:18px;"),
                                             tags$li(("Youth Development Opportunities"), style = "font-size:18px;")
                                             
                                           )),
                                           p("Over the past several years, the CSI has expanded to all six Title one elementary with the addition of one full-time social worker to attain a 1 social worker per every 2 schools’ ratio.  Since 2018, LCPS has increased staffing to include one full time social worker, school nurse, and Family Liaison at each Community School in addition to other initiatives designed to support students and families within the community. The interactive map shows the location of the six elementary schools in Sterling. Most schools are in the Sterling Census Designated Place (shaded orange). Sugarland Elementary, however, falls in the Greater Sterling region. The yellow area roughly mimics the Sugarland Elementary school zone using Census Blocks guidelines."),
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
                                          img(src='sterlingmascot.png', width = "50%", height = "20%")
                                          
                                          
                                   ), 
                                   column(12, 
                                          h4("References: "),
                                          p("[1] U.S Department of Education, Office of Elementary and Secondary Education. Full-Service Community Schools Program (FSCS). Retrieved from:", a(href =  "https://oese.ed.gov/offices/office-of-discretionary-grants-support-services/school-choice-improvement-programs/full-service-community-schools-program-fscs/", "https://oese.ed.gov/offices/office-of-discretionary-grants-support-services/school-choice-improvement-programs/full-service-community-schools-program-fscs/"), style = "font-size:12px;"),
                                          p("[2] Quinn, J., & Blank, M. J. (2020). Twenty years, ten lessons: Community schools as an equitable school improvement strategy.", em("Voices in Urban Education (VUE)."), style = "font-size:12px;")),
                                   
                          ),
                 ), 
                 
                 #---------------Sociodemographics Tab------------------------
                 tabPanel("Sociodemographics",
                          fluidRow(style = "margin: 4px;",
                                   h1(strong("Sterling Sociodemographics"), align = "center"),
                                   p("", style = "padding-top:10px;"), 
                                   
                                   
                                   
                                   column(7, 
                                          tabsetPanel(
                                            
                                            tabPanel("Demographic",
                                                     fluidRow(style = "margin: 4px;",
                                                              p("", style = "padding-top:10px;"),
                                                              column(12, align = "left",
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
                                                                            withSpinner(plotlyOutput("demoHispanicPIE", height = "500px", width = "100%")),
                                                                     )
                                                              ),
                                                     )),
                                            
                                            
                                            tabPanel("Income",
                                                     fluidRow(style = "margin: 4px;",
                                                              p("", style = "padding-top:10px;"),
                                                              column(12, align = "left",
                                                                     selectInput("demos2drop", "Select Socioeconomic Characteristic:", width = "100%", choices = c(
                                                                       "Educational Attainment" = "edu",
                                                                       "Family Income" = "faminc",
                                                                       "Housing Occupancy" = "housing",
                                                                       "Property Value" = "property",
                                                                       "Poverty Status" = "pov", 
                                                                       "Health Coverage" = "health"
                                                                     ),
                                                                     ),     
                                                                     br(""),
                                                                     withSpinner(plotlyOutput("demo2", height = "500px", width ="100%")),
                                                                     fluidRow(column(3,),
                                                                              (column(9,
                                                                                      
                                                                                      withSpinner(plotlyOutput("PropComp", height = "450%", width = "66%"))
                                                                              ))
                                                                     ),
                                                                     
                                                                     column(12, align = "right",
                                                                            p("Source: American Community 2019 5-Year Estimates", style = "font-size:12px;"),
                                                                     ),
                                                                     
                                                              ),
                                                              
                                                     )),
                                            tabPanel("Occupation/Work",
                                                     fluidRow(style = "margin: 4px;",
                                                              p("", style = "padding-top:10px;"),
                                                              column(12, align = "left",
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
                                                              
                                                              
                                                              
                                                     )
                                            ),
                                            
                                            
                                            
                                          )),
                                   
                                   column(5, align = "justify",
                                          br(""),
                                          br(""),
                                          br(""),
                                          h4(strong("Who Makes Up Sterling, Virginia?")), 
                                          
                                          p("We used the American Community Survey (ACS) 5-year 2016 - 2020 data to understand the socioeconomic demographics of the Sterling Census Designated Place (CDP). The ACS is administered by the U.S. Census Bureau which gathers information on American households, including sociodemographic variables such as gender, race/ethnicity, income, and employment. "),
                                          br(),
                                          p("Sterling’s population consists of slightly more males (50.5%) than females, with a median age of 34.7 years. Young and middle-aged adults (ages 25 to 44) account for the largest age group in Sterling. On average, the majority of the population identified as White between 2016-2020, followed by Asian as the second most common race. The area appears diverse, with almost half of the residents (41%) identifying as Hispanic or Latino. This is considerably larger than Loudoun county’s 14% Hispanic population.  "),
                                          
                                          p("Many in the Sterling community are well-educated and have a college degree. Most residents ages 25 and older have attained a high school diploma, with the largest group having earned a bachelor’s degree. This education level may contribute to the high-income level in the region. The largest median income group for families in Sterling earn $100,000 to $149,999, followed closely by both $50,000 to $74,999 bracket and $150,000 to $199,999.  There is also considerable investment in homeownership as approximately 75% of families own their home. Additionally, 68% residential property is valued between $300,000 to $499,9999. Note that there may be some considerable variation of wealth in this region as some areas may be highly affluent which may outweigh those located in less affluent areas such as those with Title 1 schools. For instance, the average residential property value may seem high, however, it is $156,000 less than the average residential property value in Loudoun County.  ", style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                          
                                          p("Given the possible variation in income, we examine the differences in poverty levels by groups – females tend to have a higher poverty level regardless of age. Specifically, females ages 18 to 24 face the highest poverty level, followed by females ages 35 to 44.  ", style = "padding-top:15px;font-size: 14px;"),
                                          
                                          p("While majority of Sterling’s population is employed (approximately 71%), there is a noticeable gap in the residents' health insurance. About 17% have no health insurance, which is higher than Loudoun county's 5.5%. This may point to possible opportunities provided by Community Schools as these families will be less routine screening and will delay treatment until the condition is more advanced and more costly and challenging to treat. ",style = "padding-top:15px;font-size: 14px;"),
                                          
                                          p("The labor force of Sterling primarily works in management, business, science, and art, followed by the service sector. Over half of those who commute to work have a commute time less than 30 minutes, and 75% of said commuters drive alone. Notably, only 1.8% of commuters utilized public transportation. ",style = "padding-top:15px;font-size: 14px;")
                                          
                                          
                                          
                                   )
                                   
                                   
                                   
                                   
                                   
                          )
                 ),
                 
                 
                 # ---------------Schools Tab------------------------
                 navbarMenu("Schools",
                            tabPanel("Demographics", 
                                     fluidRow(style = "margin: 6px;",
                                              column(12, 
                                                     h1(strong("Student Characteristics in Elementary Community Schools"), align = "center")),
                                              
                                              column(7, align = "left",
                                                     tabsetPanel(
                                                       tabPanel("Gender",
                                                                fluidRow(style = "margin: 4px;",
                                                                         withSpinner(plotlyOutput("cgender", height = "500px", width = "100%")),
                                                                         br(""),
                                                                         
                                                                         column(12,align = "right",
                                                                                p("Source: Virginia Department of Education, Loudoun County Public Schools Dashboard and Staff directory", style = "font-size:12px;"),
                                                                         ),
                                                                         
                                                                         
                                                                         
                                                                         
                                                                         
                                                                )),
                                                       
                                                       tabPanel("Race/Ethnicity",
                                                                fluidRow(style = "margin: 4px;",
                                                                         withSpinner(plotlyOutput("racenine", height = "500px", width = "100%")),
                                                                         br(""),
                                                                         fluidRow( 
                                                                           column(1,),
                                                                           
                                                                           column(11, align = "left",
                                                                                  withSpinner(leafletOutput("hispanicschool", height = "400px", width = "70%"))),
                                                                         ),
                                                                         column(12,align = "right",
                                                                                p("Source: Virginia Department of Education, Loudoun County Public Schools Dashboard and Staff directory, American Community 2019 5-Year Estimates", style = "font-size:12px;"),
                                                                         )
                                                                ),
                                                                
                                                                
                                                       ),      tabPanel("English Learners (ELs)",
                                                                        br(),
                                                                        fluidRow(style = "margin: 4px;",
                                                                                 withSpinner(plotlyOutput("figELS", height = "500px", width = "100%")),
                                                                                 br(""),
                                                                                 
                                                                                 column(12,align = "right",
                                                                                        p("Source: Virginia Department of Education, Loudoun County Public Schools Dashboard and Staff directory", style = "font-size:12px;"),
                                                                                 ),
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                        )),
                                                       
                                                       tabPanel("Individualized Educational Plan (IEP)",
                                                                br(),
                                                                fluidRow(style = "margin: 4px;",
                                                                         withSpinner(plotlyOutput("figIEP", height = "500px", width = "100%")),
                                                                         br(""),
                                                                         
                                                                         column(12,align = "right",
                                                                                p("Source: Virginia Department of Education, Loudoun County Public Schools Dashboard and Staff directory", style = "font-size:12px;"),
                                                                         ),
                                                                         
                                                                         
                                                                         
                                                                         
                                                                         
                                                                )),
                                                       
                                                       tabPanel("Homelessness",
                                                                br(),
                                                                fluidRow(style = "margin: 4px;",
                                                                         withSpinner(plotlyOutput("figHOME", height = "500px", width = "100%")),
                                                                         br(""),
                                                                         
                                                                         column(12,align = "right",
                                                                                p("Source: Virginia Department of Education, Loudoun County Public Schools Dashboard and Staff directory", style = "font-size:12px;"),
                                                                         ),
                                                                         
                                                                         
                                                                         
                                                                         
                                                                         
                                                                )),
                                                     )
                                              ), 
                                              br(""),
                                              br(""),
                                              br(""),
                                              column(5, align = "justify",
                                                     
                                                     h4(strong("What Do Community Schools Look Like?"), align = "left"),
                                                     p("Community schools are schools that are available in low-income areas that provide resources and accommodation for the students and families who attend their schools. These schools not only focus on students learning, but may provide free meals, health care services, tutoring, and counseling services, to those in need through partnerships with community, business, and governmental organizations. In Sterling, there are six Title 1 Community Schools. Those schools are Forest Grove Elementary, Guilford Elementary, Rolling Ridge Elementary, Sterling Elementary, Sugarland Elementary, and Sully Elementary."),
                                                     p("To understand the population of the six elementary schools, we looked at the demographics and compared them to one another. For the figure titled “Gender”, we can see the total number of students in each school as well as the gender split. Sterling Elementary has the lowest number of students. Guilford is the only school which has more females than male while Forest Grove like the Sterling area for the gender ratio has an almost equal split. Rolling Ridge and Sully have a similar trend: the female students are about 48% of the total. Sugarland and Rolling Ridge have slightly lesser females than them. Sterling Elementary has the most uneven distribution where there are 86 females for every 100 male students. ", style = "padding-top:15px;font-size: 14px;"),
                                                     p("The race and ethnicity demographics in 2019-2020 revealed that overall, Hispanic students, made up the greatest percentage of students attending the six elementary schools in Sterling, which differs from the general make-up of the Sterling area where White residents made up the majority of residents. There is a huge difference between Guilford and Forest Grove in terms of the ethnic groups of students, given that they both have similar number of total students: Guilford has higher Hispanic students while Forest Grove has a lot more White and Hispanic students. ",  style = "padding-top:15px;font-size: 14px;"),
                                                     p("The differences might be due to the Hispanic population density in the areas where these schools are located. Hence, we mapped the schools, and collected the total Hispanic population between the years 2016 to 2020. We found that the area where Rolling Ridge is located has the largest population of Hispanic identifying people. This is followed closely by Sterling Elementary and Forest Grove Elementary.",  style = "padding-top:15px;font-size: 14px;"),
                                                     p("The high Hispanic population may explain the large percentage of English learners in these Community Schools. English Learners (EL) are students learning English in schools who generally come from diverse backgrounds and non-English speaking homes. The percentage of Els increases from 67% in 2018-2019 to a high of 70% in 2020-2021.  ", style = "padding-top:15px;font-size: 14px;"),
                                                     p("The Department of Education defines Individualized Educational Plan (IEP) as an individualized plan created by teachers, parents, school administrators, and service personnel to improve educational results for a child with a disability. From 2018-2019, approximately 11% of Sterling’s Community Schools students had an individualized educational plan. There is also a significant portion of students who are facing homelessness in these schools. 13% of students facing homelessness attended Community Schools in Sterling in 2018-2019. This number increased to over 15% in 2019-2020, with a noticeable two-year decline in 2020 -2021 and 2021-2022. ", style = "padding-top:15px;font-size: 14px;")
                                              ),
                                              
                                              
                                              
                                              
                                              
                                              
                                              
                                     )), 
                            tabPanel("Usage of School Programs",
                                     column(12, 
                                            h1(strong("Utilization of Current School Programs"), align = "center")),
                                     column(6, align = "left",
                                            selectInput("generalDATA", "Select Data:", width = "100%", choices = c(
                                              # "English Learner Statu" = "figELS",
                                              # "IEP Status" = "figIEP",
                                              # "Homeless" = "figHOME",
                                              
                                              
                                              "Free and Reduced-Price Breakfast" = "breakfast",
                                              "Overall Free and Reduced-Price Lunch" = "figFRL",
                                              "Free and Reduced-Price Lunch by Schools" = "eligablelunch_data",
                                              "Weekend Meals" = "weekendmeals",
                                              "Clothing and Basic Supplies" = "families"
                                              
                                            ),
                                            ),
                                            
                                            withSpinner(plotlyOutput("generaldatafilledlinegraphs", height = "500px", width = "100%")),
                                            p("Source: Community Schools Annual Reports", style = "font-size:12px;", align = "right"),
                                     ),
                                     column(6, align = "justify",
                                            br(),
                                            
                                            p("The School breakfast program is a federally assisted meal program that provides nutritious breakfast meals to students. The percentage of students participating in the Free and Reduced Breakfast Program is consistent across all years for Guilford, Sugarland, Sterling, and Rolling Ridge at about 80%, 65%, 45%, and 30%, respectively. Since joining the Community School Initiative, student participation in the Breakfast program increased for Forest Grove and Sully to 80% and 70%, respectively, in 2019-2020 and 2020-2021."),
                                            p("The trends are similar for the Free and Reduced-Price program which is a U.S. Department of Agriculture assisted meal program that provides nutritionally balanced lunches. Participating schools receive USDA cash subsidies and donated food items for each meal that meets Federal nutrition standards and meal patterns.  On average, over 70% of students in Sterling’s community schools participate in this program. While participation rates are consistent across years, there are differences across schools ranging from as high as 85% in Sully and a low of 60% in Forest Grove Elementary. "),
                                            p("Sterling’s Community Schools have increasingly assisted families with weekend meals growing from 135 families in 2018 to 808 in 2021. There is also an increase in families helped with clothing, shoes, and other essential supplies. In 2018, 538 families received clothing assistance from community schools, increasing to 832 families in 2020.")
                                            
                                     )),
                            
                            tabPanel("Performance", 
                                     fluidRow(style = "margin: 6px;",
                                              column(12, 
                                                     h1(strong("Education in Community Schools"), align = "center")),
                                              
                                              column(6, align = "left",
                                                     tabsetPanel(
                                                       tabPanel("Size",
                                                                br(),
                                                                selectInput("schooldrop2", "Select Characteristic:", width = "100%", choices = c(
                                                                  "Enrollment" = "cenrol",
                                                                  "Educators" = "cteacher"
                                                                  
                                                                ),
                                                                ),
                                                                
                                                                withSpinner(plotlyOutput("ocuplot2", height = "500px", width = "100%")),
                                                                img(src = "stratiohyphen.png", class = "topimage", width = "80%", height ="80%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                                                
                                                                column(12,align = "right",
                                                                       p("Source: Virginia Department of Education, Loudoun County Public Schools Dashboard and Staff directory", style = "font-size:12px;"),
                                                                ),
                                                                
                                                       ),
                                                       
                                                       tabPanel("Absences",
                                                                br(),
                                                                selectInput("schooldrop3", "Select Characteristic:", width = "100%", choices = c(
                                                                  
                                                                  "Absences" = "attend", 
                                                                  "Chronic Absenteeism" = "chronic"
                                                                  
                                                                  
                                                                ),
                                                                ),
                                                                
                                                                withSpinner(plotlyOutput("ocuplot3", height = "500px", width = "100%")),
                                                                
                                                                
                                                                
                                                                column(12,align = "right",
                                                                       p("Source: Virginia Department of Education, Loudoun County Public Schools Dashboard and Staff directory", style = "font-size:12px;"),
                                                                ),
                                                                
                                                       ),
                                                       
                                                       tabPanel( "Assessment",
                                                                 br(),
                                                                 fluidRow(
                                                                   column(12,align = "left",
                                                                          column(6,
                                                                                 radioButtons(
                                                                                   "category_subject",
                                                                                   label = "Select:",
                                                                                   choices = c("Mathematics", "English Reading"),
                                                                                 )),
                                                                          column(6,
                                                                                 radioButtons(
                                                                                   "category_subgroup",
                                                                                   label = "Select:",
                                                                                   choices = c("Race", "Gender", "Other Subgroups"),
                                                                                 )),
                                                                          
                                                                          selectInput("schoolgradesdrop", "Select School:", width = "100%", choices = c(
                                                                            "Forest Grove" = "forestgroverace",
                                                                            "Guilford" = "guilfordrace",
                                                                            "Rolling Ridge" = "rollingrace",
                                                                            "Sterling" = "sterlingrace",
                                                                            "Sugarland" = "sugarlandrace",
                                                                            "Sully" = "sullyrace"
                                                                            
                                                                          ),
                                                                          ),
                                                                          
                                                                          withSpinner(withSpinner(plotlyOutput("schoolgrades", height = "500px", width = "100%"))),
                                                                          
                                                                          #br(""),
                                                                          #withSpinner(plotlyOutput("grades_english", height = "500px", width = "100%")),
                                                                          #br(""),
                                                                          
                                                                          #br(""),
                                                                          p(strong("*Note: < represents either: i) fewer than 10 students in the group or ii) fewer than 20 students in the group and the pass rate is less than 50%", style = "font-size:14px;")),
                                                                          p(strong("*Note: > represents either: i) fewer than 10 students in the group or ii) fewer than 20 students in the group and the pass rate is greater than 50%", style = "font-size:14px;")),
                                                                          p("Source: Virginia Department of Education, Loudoun County Public Schools Dashboard and Staff directory", style = "font-size:12px;"),
                                                                   )
                                                                   
                                                                   
                                                                 )      
                                                                 
                                                                 
                                                                 
                                                                 
                                                       ),
                                                       
                                                     )),
                                              
                                              column(6, align = "justify",
                                                     h4(strong("How are students performing in Community Schools?")),
                                                     
                                                     
                                                     p("Due to the COVID-19 pandemic, changes in modality and hardships may have impacted students' behavior and exam performance in 2020-2021. As such, we cannot draw significant conclusions about changes over time. However, we include information from 2020-2021 as this may highlight possible opportunities for Community Schools' in assisting an already vulnerable population.  ", style = "padding-top:15px;font-size: 14px;"),
                                                     
                                                     p("Sterling ES has enrolled a consistent number of students, approximately 575, from 2016 to 2020. Interestingly, the schools with the lowest number of students - Guilford, Sully, and Sugarland - have increased their enrollment since joining the Community Schools Initiative. There are slight differences in the number of educators across schools, although the student population ranged from 461 to 585 in 2019-2020. Rolling Ridge has the lowest student to teacher ratio, whereas Sterling Elementary is the only school with a greater proportion of staff than teachers. Sully ES has the highest student-to-teacher ratio, with 14 students per teacher, even though it has the lowest number of students. ", style = "padding-top:15px;font-size: 14px;"),
                                                     p("We utilize data from the Loudoun County Public Schools Dashboard and the Virginia Department of Education to analyze students’ behavior. Sully ES had the largest increase in absence rate across the year. There are also increases across all schools in the second and fourth quarters of the 2020-2021 school year. We acknowledge that this rate may be skewed due to the Covid-19 pandemic. As such, we analyze chronic absenteeism in the prior school year.  ", style = "padding-top:15px;font-size: 14px;"),
                                                     p("Chronic Absenteeism is defined by the Virginia Department of Education as the percentage of students missing 10% or more of the academic year for any reason, including excused absences, unexcused absences, and suspensions. In 2018-2019, before the pandemic, the average chronic absenteeism was about 9% across all schools. Sterling and Guilford had the lowest rate, with about 6% of students missing 1 out of 10 classes. Sugarland, Rolling Ridge, and Sully had the highest chronic absenteeism rate, above 10%. Interestingly, Forest Grove and Sterling’s rates were consistent after the Covid-19 pandemic, whereas there were increases in chronic absenteeism for other schools. Surprisingly, chronic absenteeism at Forest Grove reduced slightly during the pandemic, moving from approximately 8% in the 2018-2019 school year to 5% in 2020-2021. Rolling Ridge had the highest rate in 2020-2021, with 17.7% of students missing 1 out of 10 classes.",style = "padding-top:15px;font-size: 14px;"),
                                                     p("We collect and visualize Standard of Learning (SOL) Mathematics and English Reading passing rates for the 2018-2019 and 2020-2021 school years. Performance statistics can be disaggregated into subgroups such as race, gender, and other characteristics. Students tend to perform better in Mathematics than in English, with the average pass rate for Mathematics being around 75% for all students, whereas the pass rate for English is approximately 60% in 2018-2019. Due to the additional hardships students who are economically disadvantaged, facing homelessness, or are differently-abled, it is no suprise that these students pass rates are lower in both Mathematics and English than their peers."),
                                                     br(),
                                                     p(strong(em("Mathematics"))),
                                                     p("Generally, White, and Asian students perform better than their Black and Hispanic counterparts. On average, Rolling Ridge, Sterling, Sugarland, and Sully had lower pass rates (below 80%) among all race/ethnic groups compared to Forest Grove, Guilford, and Rolling Ridge (above 80%) in 2018-2018. There is a decline in pass rates for all students in the academic year 2020-2021; however, the reduction is steeper for Black and Hispanic students.",style = "padding-top:15px;font-size: 14px;"),
                                                     p("Males tend to have higher pass rates than females regardless of year or school. In general, homeless and differently-abled students have lower pass rates than their counterparts in 2018-2018. The pass rates for these students (homeless and differently-abled) also differ across schools. For instance, these students have a lower rate in Sully, Sterling, and Guilford compared to their peers in other ES. Moreover, there was a decline in pass rates for these students suggesting possible opportunities to provide resources for these already vulnerable students. ",style = "padding-top:15px;font-size: 14px;"),
                                                     br(),
                                                     p(strong(em("English Reading"))),
                                                     p("The trends in English Reading performance are similar to those in Mathematics. On average, Hispanic students in all schools perform poorer than the other racial subgroups. White and Asian students typically have a higher pass rate usually over 80%. Surprisingly, the pass rate in English for Asian and White students remained steady or fallen marginally during the pandemic whereas Hispanic students seems to have a substantial negative impact by the pandemic.  ",style = "padding-top:15px;font-size: 14px;"),
                                                     
                                              )
                                     )),
                            
                            
                            
                            
                            #------------Climate Survey Subtab-----------------
                            tabPanel("Climate Survey",
                                     fluidRow(style = "margin: 4px;",
                                              h1(strong("Climate Survey 2019-2020"), align = "center"),
                                              p("Loudoun County Public Schools (LCSPS) surveyed students, parents, and teachers/staff in February 2020 to assess their perceptions about the climate of schools and factors that influence student achievement, as well as the implementation of key initiatives. Questions included student engagement, relationship between teachers and students, bullying, and social-emotional wellbeing. LCPS uses these surveys to identify strengths and weaknesses that can guide efforts to improve student learning and the school's environment. The survey was administered online to parents, administrators (other than principals), and students (grades 3-8).", style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                              p("The graphs below present some key indices from the school climate surveys. Each index comprises of a series of questions that are averaged for an overall score. Higher scores indicate a more favorable school climate. Graphs are visualized so one can select multiple indices for comparison. ", style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                              p("", style = "padding-top:10px;"),
                                              
                                              
                                              
                                              tabsetPanel(
                                                tabPanel("Student Climate Survey",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 2px;",
                                                                  align = "center",
                                                                  # br("", style = "padding-top:2px;"),
                                                                  # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                                                  br(""),
                                                                  h2(strong("Student Perception"),
                                                                     column(12, align="justify",
                                                                            p("We present four key indices based on students’ responses:", tags$b("1. Student Engagement"), "which measures whether students believe their decisions are important and they belong;", tags$b("2. Student-Teacher Relationship"), "- Students engagement with teachers;", tags$b("3. Social-Emotional Wellbeing"), "-Students believe they can discuss and work through their emotions; and", tags$b("4. Bullying"), "- Students perception about bullying occurrence. Higher scores mean less bullying.", style = "padding-top:15px;font-size: 14px;"),
                                                                     ),
                                                                     
                                                                  )
                                                         ),
                                                         column(6, align = "center",h4(strong("")),
                                                                p(""),
                                                                selectInput("surveydrop5", "Select Survey Index:", width = "60%", choices = c(
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
                                                                selectInput("surveydrop6", "Select Survey Index:", width = "60%", choices = c(
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
                                                                selectInput("surveydrop7", "Select Survey Index:", width = "60%", choices = c(
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
                                                                selectInput("surveydrop8", "Select Survey Index:", width = "60%", choices = c(
                                                                  "Student Engagement" = "studentanswer1",
                                                                  "Teacher Relationship" = "studentanswer2",
                                                                  "Social-Emotional Wellbeing" = "studentanswer3",
                                                                  "Bullying" = "studentanswer5"
                                                                ),
                                                                ),
                                                                withSpinner(plotlyOutput("survo8", height = "275px", width ="100%")),
                                                                
                                                                
                                                                
                                                         ),
                                                         column(12, align = "justify",
                                                                column(6, align = "justify",
                                                                       br(),
                                                                       p("Most students feel like they belong at their schools and help their class to make decisions at school. There are some differences across schools as on average over 80% of students in Forest Grove, Sugarland, and Sully believe these statements whereas only 70% have these beliefs in Guilford, Rolling Ridge, and Sterling.", align = "justify"),
                                                                       p("Student-Teacher Relationship measures “Teachers and other adults at this school treat me with respect” and “There are teachers or adults at this school I could talk with if I need help with something.” This index is important as students, especially at a young age, should feel comfortable confiding in and talking to their teachers. Interestingly, only 72% of students on average in Sterling indicate a good student-teacher relationship. This is significantly lower than all other elementary community schools, with an overwhelming majority of students having an excellent relationship with teachers. This significant difference suggests a potential opportunity for the Community School program to help Sterling’s student-teacher relationship. ", style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                ), 
                                                                column(6, align= "justify",
                                                                       p("There are some differences in students' perceptions about their social-emotional well-being across schools. Social-emotional measures students ability to understand and manage their emotions. It includes questions such as", dQuote("I work out disagreements with other students by talking with them"), "and", dQuote("I can control myself when I am upset."), "On average, less than 80% of Guilford, Rolling Ridge, and Sterling students indicate good social-emotional well-being. This may play a role in students' perception of bullying. ", style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                       p("The bullying category includes questions such as", dQuote("I have stopped other people from bullying when I have seen it at school"), "and", dQuote("I have been bullied by students at school this year.(Disagree)."), "The index has been recorded so that a higher score indicates less bullying. These scores are significantly lower than other categories. On average, just a little over 50% of students across all schools believe that bullying is not a problem, meaning at least half of the student population believes it is a problem. Students' perception on bullying and social-emotional well-being suggests community schools can implement more services and resources in the Mental Health pillar to help address these concerns. ", style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                       
                                                                ))
                                                         
                                                         
                                                ), 
                                                
                                                tabPanel("Parent Climate Survey",
                                                         fluidRow(style = "margin: 2px;",
                                                                  fluidRow(style = "margin: 2px;",
                                                                           align = "center",
                                                                           # br("", style = "padding-top:2px;"),
                                                                           # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                                                           br(""),
                                                                           h2(strong("Parent Perception"),
                                                                              column(12, align = "justify",
                                                                                     p("The four indices below are based on parents’ perception on:", tags$b("1. Academic Support"), "– Academic expectations and individualized instruction;", tags$b("2. Communications"), "– School and teacher communication to parents;", tags$b("3. Relationships"), "– Welcoming environment, social-emotional support, respect for students; and", tags$b("4. Instructions"), "– Measures of LCPS initiatives that helps foster deep learning and thinking. ", style = "padding-top:15px;font-size: 14px;"),
                                                                              ),
                                                                              #h4(""),
                                                                              #h4("[updat this]"),
                                                                              br()
                                                                           )
                                                                  ),
                                                                  p("", style = "padding-top:10px;"),
                                                                  column(6, align = "center",h4(strong("")),
                                                                         p(""),
                                                                         selectInput("surveydrop1", "Select Survey Index:", width = "60%", choices = c(
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
                                                                         selectInput("surveydrop2", "Select Survey Index:", width = "60%", choices = c(
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
                                                                         selectInput("surveydrop3", "Select Survey Index:", width = "60%", choices = c(
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
                                                                         selectInput("surveydrop4", "Select Survey Index:", width = "60%", choices = c(
                                                                           "Academic Support" = "parentanswer1",
                                                                           "Communications" = "parentanswer2",
                                                                           "Relationships" = "parentanswer3",
                                                                           "Instructions" = "parentanswer4"
                                                                         ),
                                                                         ),
                                                                         withSpinner(plotlyOutput("survo4", height = "275px", width ="100%")),
                                                                         
                                                                         
                                                                         
                                                                  ),
                                                                  column(12, align = "justify",
                                                                         br(),
                                                                         column(6, align = "justify",
                                                                                p("Academic support measures parents’ beliefs on whether", dQuote("Teachers at this school care about how well my child does in school,"), dQuote("High expectations for academic achievement are evident in my child’s classrooms;"), dQuote("My child’s school emphasizes critical thinking through authentic challenging problem solving;"), dQuote("My child’s teachers provide help when my child needs it;"), dQuote("I am satisfied that my child is receiving a quality education at this school;"), dQuote("The school seeks ways to improve my child’s learning;"), dQuote("I am satisfied with how much my child is learning at school;"), "and", dQuote("The school encourages my student to take academic risks."), style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                                p("Communications consists of responses on", dQuote("This school effectively communicates important information to parents;"), dQuote("I am taken seriously by school staff when I have concerns;"), dQuote("Teachers provide me with feedback on my child’s progress including suggestions for improvement;"), dQuote("The school provides a variety of ways for parents to become involved;"), "and", dQuote("My child’s teachers clearly tell me what my child is expected to learn."), style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                                p("Relationships measures whether", dQuote("A welcoming environment exists in my child’s school;"), dQuote("This school respects diversity and welcomes all cultures;"), dQuote("This school supports an inclusive environment;"), dQuote("The administration is responsive to parents and children;"), dQuote("My child’s teachers care about my child;"), dQuote("My child feels respected at this school;"), "and", dQuote("This school provides multiple opportunities for family engagement in school activities"), style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                         ),
                                                                         column(6, align = "justify",
                                                                                p("Instruction measurement comprises of the following questions:", dQuote("I have noticed my child taking what he/she learns in one lesson and using that learning in new situations;"), dQuote("My child explains his/her ideas in ways that are understood by others;"), dQuote("My child is able to communicate his/her ideas to different audiences;"), dQuote("My child is able work with and learn from other people to solve problems together;"), dQuote("My child respects other peoples' perspectives and ideas;"), dQuote("My child has engaged in solving problems that impact his/her community;"), dQuote("My child creates new ideas or strategies that provide solutions to challenging problems;"), dQuote("My child asks questions and thinks in creative ways;"), dQuote("My child analyzes information and identifies patterns and connections to solve problems;"), "and", dQuote("My child engages in projects to solve authentic challenging problems"), style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                                p("Parents' perceptions across schools appear to be consistent across most categories. On average, over 90% of parents from all schools believe that:", tags$b("1."), "schools have high academic expectations and individualized instruction;", tags$b("2."), "there is a welcoming environment for students and families, and", tags$b("3."), "their child is engaging in deep learning and critical thinking. Communication is the only category with are slight differences in parents’ responses. Parents from Forest Grove and Sugarland rank their schools’ communication process lower than other schools; however, it is still significantly high at 85% and 86%, respectively. ", style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                         )),
                                                                  
                                                                  
                                                                  
                                                         )
                                                ),
                                                
                                                tabPanel("Teacher/Staff Climate Survey",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 2px;",
                                                                  align = "center",
                                                                  # br("", style = "padding-top:2px;"),
                                                                  # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                                                  br(""),
                                                                  h2(strong("Teacher/Staff Perception"),
                                                                     column(12, align = "justify",
                                                                            p("We present six key indices based on teachers/staff responses:", tags$b("1. Staff Collegiality"), "– Teacher and staff relationship;", tags$b("2. Academic Environment"), "– Teacher support and student effort;", tags$b("3. School Leadership"), "– Support from School Administrators;", tags$b("4. Managing Student Behavior"), "– Student knows how to conduct themselves;", tags$b("5. Workplace Environment"), "– Support and material availability; and", tags$b("6. Instructional Behavior"), "– Supporting students learning needs and goals.", style = "padding-top:15px;font-size: 14px;"),
                                                                     ),
                                                                  ),
                                                         ),
                                                         column(6, align = "center",h4(strong("")),
                                                                p(""),
                                                                selectInput("surveydrop9", "Select Survey Index:", width = "60%", choices = c(
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
                                                                selectInput("surveydrop10", "Select Survey Index:", width = "60%", choices = c(
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
                                                                selectInput("surveydrop11", "Select Survey Index:", width = "60%", choices = c(
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
                                                                selectInput("surveydrop12", "Select Survey Index:", width = "60%", choices = c(
                                                                  "Staff Collegiality" = "teacherandstaffanswer1",
                                                                  "Academic Environment" = "teacherandstaffanswer2",
                                                                  "School Leadership" = "teacherandstaffanswer3",
                                                                  "Managing Student Behavior" = "teacherandstaffanswer4",
                                                                  "Workplace Environment" = "teacherandstaffanswer6",
                                                                  "Instructional Environment" = "teacherandstaffanswer7"
                                                                ),
                                                                ),
                                                                withSpinner(plotlyOutput("survo12", height = "275px", width ="100%")),
                                                                
                                                                
                                                                
                                                                
                                                         ),
                                                         column(12, align = "justify",
                                                                br(),
                                                                column(6, align = "justify",
                                                                       p("Staff Collegiality comprises of the following questions:", dQuote("Teachers and other adults at this school support one another to meet the needs of all students"), "and", dQuote("Teachers and other adults at this school have taught me things that have helped me do my job better"), style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                       p("Academic Environment comprises of the following questions:", dQuote("Teachers and other adults at this school provide students the support they need to succeed"), "and", dQuote("Students put forth the effort required to learn the material"), style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                       p("School Leadership comprises of the following questions:", dQuote("I feel comfortable raising issues and concerns that are important to me with school administrators;"), dQuote("This school’s administrators support the professional development of staff;"), dQuote("This school’s administrators support teachers’ efforts to maintain discipline in the classrooms;"), "and", dQuote("This school's administrators set high expectations for all students."), style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                       p("Managing Student Behavior comprises of the following questions:", dQuote("Students know how this school defines inappropriate behavior;"), dQuote("Students know there are consequences for breaking school rules;"), dQuote("Teachers and other adults at this school consistently enforce rules for student behavior;"), dQuote("Students are acknowledged for positive behavior;"), dQuote("There are supports to help a student who consistently misbehaves develop positive behavior;"), "and", dQuote("We use data to evaluate and, if needed, adjust this school’s student conduct policies."), style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                ),
                                                                column(6, align = "justify",
                                                                       p("Workplace Environment comprises of the following questions:", dQuote("The physical environment of my workspace supports my work responsibilities;"), dQuote("My school provides me with sufficient access to appropriate supplies and materials;"), "and", dQuote("I have the support I need to incorporate technology into my work responsibilities."), style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                       p("Instructional Environment comprises of the following questions:", dQuote("The physical environment of my classroom supports my teaching and my students’ learning;"), dQuote("My school provides me with sufficient access to appropriate instructional materials;"), "and", dQuote("I have the support I need to incorporate technology into my instruction."), style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                       p("Overall, educators seem to have a favorable view of the schools' environment. On average, over 90% of educators believe that the academic, instructional, and workplace environment was supportive and positive. Notably, although high, only 80% of educators thought positively about their schools' environment. Staff collegiality is also high, with almost all educators in Guilford, Rolling Ridge, Sterling, Sugarland, and Sully believing they have good support from fellow staff. ", style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                       p("There are some slight variations in beliefs about school leadership across schools. Over 85% of educators in Guilford, Rolling Ridge, Sterling, Sugarland, and Sully believe that school administrators are supportive, set high standards, and allow staff to raise issues and concerns, only 78% in Forest Grove agreed with the statements. There are also differences in managing student behavior which ranges from as high as 89% to a low of 75%.", style = "padding-top:15px;font-size: 14px;", align = "justify"),
                                                                       
                                                                ),
                                                                br(""),
                                                                br(""),
                                                                br(""),
                                                         ))),
                                              # column(12, 
                                              # h4("References: "),
                                              # p("[1] U.S Department of Education, Office of Elementary and Secondary Education. Full-Service Community Schools Program (FSCS). Retrieved from:", a(href =  "https://oese.ed.gov/offices/office-of-discretionary-grants-support-services/school-choice-improvement-programs/full-service-community-schools-program-fscs/", "https://oese.ed.gov/offices/office-of-discretionary-grants-support-services/school-choice-improvement-programs/full-service-community-schools-program-fscs/"), style = "font-size:12px;"),
                                              # p("[2] Quinn, J., & Blank, M. J. (2020). Twenty years, ten lessons: Community schools as an equitable school improvement strategy.", em("Voices in Urban Education (VUE)."), style = "font-size:12px;")),
                                     ),
                                     
                            ),
                            
                            tabPanel("Representatives' Reports",
                                     column(12, align = "center",
                                            p(h1(strong("Elementary Community School Representatives ")), style = "padding-top:5px;"),
                                     ),
                                     tabsetPanel(
                                       tabPanel("Perception",
                                                fluidPage(style = "margin: 2px;",
                                                          fluidRow(
                                                            
                                                            column(5, align = "left",
                                                                   radioButtons(
                                                                     "category",
                                                                     
                                                                     label = "Select:",
                                                                     choices = c("Challenges and Weaknesses", "Strengths and Successes", "Future Goals"),
                                                                   ))),
                                                          
                                                          column(5, align = "left",
                                                                 wordcloud2Output("wordcloud")
                                                          ),
                                                          
                                                          column(7, align = "justify",
                                                                 h4(strong("School Representative Perception ")),
                                                                 br(),
                                                                 p("Loudoun County Public Schools surveyed each school representative in 2020-2021 to obtain information on the state of the Elementary Community Schools. Questions ranged from strengths, weaknesses, and utilization of different programs. We present word clouds to highlight representatives’ major responses.  The bigger the word appears, the more often it is mentioned in the representative’s response. Hovering over the word will show the number of times the school representatives used the term in their response. "),
                                                                 h4(strong("Challenges and Weaknesses Faced")),
                                                                 p("Challenges and Weaknesses consist of responses to two questions: 'What are your school's biggest challenges?' and 'What are your school's biggest weaknesses?'. The word cloud suggests that the biggest challenges for Elementary Community Schools in Sterling are mental health and ensuring care for the undocumented and uninsured, especially for Forest Grove and Sterling. Parent involvement is also a significant concern for representatives in Sugarland."),
                                                                 h4(strong("Strengths and Successes")), 
                                                                 p("Responses suggest that schools generally do well in the Family Engagement pillar, except for Sugarland. There is also a strong sense of community and teamwork. Teachers and staff at these schools also feel valued. If there is a negative word, for example, 'gaggle' - it means there has been a decrease in that area in one or many schools, depending on the size of the word: so in this case, one school, Sugarland saw a reduction of gaggle reports. ('Gaggle is a student surveillance application, in which student work and behavior are scrutinized for indicators of violence or a mental health crisis, and profanity and sexuality are policed.')"),
                                                                 h4(strong("Future Goals")),
                                                                 p("Future Goals primarily focus on creating more opportunities to engage and offer to students and parents. Representatives would like to focus on program development as the schools continue working with the students and their families. Another major goal for all schools is the partnership creation and development with community members to help expand programs. This is evident by numerous terms such as agencies, stakeholder, partners, partnerships, involvement, community, meaningful services addition, and assistance.")
                                                          )
                                                )),
                                       
                                       tabPanel("Partners",
                                                column(9, 
                                                       
                                                       collapsibleTreeOutput("tree1",height = "600px", width = "100%") 
                                                       
                                                ),
                                                
                                                column(3, align = "justify",
                                                       br(),
                                                       h4(strong("Community Schools Programs Key Partners ")),
                                                       br(),
                                                       p("Community Schools Representatives were asked about key partners each school utilized for help with services in Health and Social Services, Mental Health, Family Engagement, and Youth Development. The interactive tree shows these key partners and programs for each school in 2020-2021 to assist in comparative analysis by schools to help find partnership possibilities for future activities. The node's size depends on the number of entries. Thus, the bigger circles indicate more school partnerships. For example, Sterling Elementary has the largest number of partners for Youth Development activities, as indicated by the more sizeable node (blue circle). Note, one should take caution when making conclusions as the tree is not exhaustive. There were a few schools with missing information, e.g., Forest Grove had no information on their critical partners for the Youth Development Pillar. ")
                                                )
                                                
                                                
                                       )
                                       
                                       
                                     ))
                            
                            
                            
                            #tabPanel(h4("Weaknesses and biggest challenges")),
                            
                            
                            
                            
                            
                 ),
                 #----------------------------Resources Tab-----------------------------
                 navbarMenu("Resources",
                            tabPanel("Health and Social Services",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h1(strong("Health and Social Services")),
                                                     p(""),
                                                     
                                              )),
                                     
                                     fluidPage(style = "margin: 2px;", 
                                               radioButtons(
                                                 inputId = "health_category",
                                                 label = "Select:",
                                                 choices = c("All Services", "Free Services"),
                                               ),
                                               column(6, 
                                                      leafletOutput("map_health", width = "100%", height = 600)
                                                      #fluidRow(align = "center",
                                                      #    p(tags$small(em('Last updated: August 2021'))))
                                               ), 
                                               column(6, 
                                                      h3(strong("Overview"), align = "justify"), 
                                                      p(("We present interactive maps to better understand the services available to students and families in the six community schools. The legend on the top right corner is interactive, allowing the user to filter by desired resource groups. Each colored marker provides a pop-up with the Name of the services, detailed Description, Language, Address, and Website link.   "), align = "justify"),
                                                      h4("Travel Distance"),
                                                      p(("We also include the driving distances to services from Sterling Elementary School (the blue-tipped marker). Sterling Elementary School is the center point on our map as it is located in the middle of Sterling, CDP. Driving distances for 10 minutes, 20 minutes, and 45 minutes are shown on the map using the green, blue, and red boundaries, respectively. Service markers within these boundaries on the map represent different services available within the respective driving distances."),align = "justify"),
                                                      h4(strong("Health and Social Services Availability")), 
                                                      p(("A key pillar essential to ensuring students thrive in school is access to quality health and social services. It is difficult for students to focus on academic needs if their non-academic needs are not met. Thus, providing nutritious food, weather-appropriate clothing, and medical care such as dental, vision, and preventative care can improve a student's performance. For many, barriers to these services are often a result of expense, transportation, and time availability, making it vital to provide access to these resources for all members of a community. "),align = "justify"),
                                                      p(("Due to Sterling's unique location within Loudoun County and its proximity to Washington, D.C., Sterling residents have access to numerous health and social services. However, the number and accessibility of services decrease for residents that require free or reduced-cost services, with many options falling outside of a ten-minute drive. For instance, a wide variety of free food pantries are available within a ten-minute drive of Sterling Elementary. However, beyond that, access to medical care and clothing is not as readily open, with many resources falling within the 20- and 45-minute boundaries. It should be noted that although there may be resources in neighboring Fairfax County, close to Sterling, often these services are unavailable for out-of-county residents. "),align = "justify"),
                                                      
                                               )
                                     )),
                            
                            
                            
                            tabPanel("Mental Health", 
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h1(strong("Mental Health Services")),
                                                     p(""),
                                                     
                                                     
                                                     
                                                     
                                                     
                                                     
                                                     
                                              )),
                                     p(""),
                                     br(""),
                                     
                                     
                                     
                                     
                                     fluidPage(style = "margin: 2px;", 
                                               radioButtons(
                                                 inputId = "mental_category",
                                                 label = "Select:",
                                                 choices = c("All Services", "Free Services"),
                                               ),
                                               column(6, 
                                                      leafletOutput("map_mental", width = "100%", height = 600)
                                                      #fluidRow(align = "center",
                                                      #    p(tags$small(em('Last updated: August 2021'))))
                                               ), 
                                               
                                               column(6, 
                                                      h3(strong("Overview"), align = "left"), 
                                                      p(("We present interactive maps to better understand the services available to students and families in the six community schools. The legend on the top right corner is interactive, allowing the user to filter by desired resource groups. Each colored marker provides a pop-up with the Name of the services, detailed Description, Language, Address, and Website link.  "), align = "justify"),
                                                      h4(("Travel Distance")),
                                                      p(("We also include the driving distances to services from Sterling Elementary School (the blue-tipped marker). Sterling Elementary School is the center point on our map as it is located in the middle of Sterling, CDP. Driving distances for 10 minutes, 20 minutes, and 45 minutes are shown on the map using the green, blue, and red boundaries, respectively. Service markers within these boundaries on the map represent different services available within the respective driving distances.  "), align = "justify"),
                                                      h4(strong("Mental Health Availability")), 
                                                      p(("Mental health services can improve behavior, attendance, performance, and one’s overall wellbeing. Mental health is important to perform well in school. It is equally important as physical health. If Mental health is not given attention it can lead to behavioral issues. A person cannot function well with poor mental health. Providing the correct mental health resource in a timely manner can improve a student’s performance drastically. Furthermore, mental health is not only important for a student but also for everyone living in an area. Parents also equally require mental health checkups so that they can take care of their children properly and make sure that their children are living in a healthy environment at home.  "), align = "justify"),
                                                      p("In order to understand the availability of mental health services in and around the Sterling area we used publicly available data to plot the resources on the map. We further divide Mental Health into four categories: Anger Management, Bereavement, Family Counseling, and Family Therapy. There are only two mental health services available in a 10-minute driving radius. There are only three available anger management services in a 45-minute radius of the Sterling area. However, there are numerous options of Family therapy including family counseling, relationship counseling, and children counseling. The residents of the Sterling area should take advantage of these resources provided around their area.  ", align = "justify")
                                               )
                                               
                                               
                                     )
                                     
                            ),
                            tabPanel("Family Engagement",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h1(strong("Family Engagement Resources")),
                                                     p(""),
                                                     br("")
                                                     
                                                     
                                                     
                                              )),
                                     
                                     fluidPage(style = "margin: 2px;", 
                                               radioButtons(
                                                 inputId = "family_category",
                                                 label = "Select:",
                                                 choices = c("All Services", "Free Services"),
                                               ),
                                               column(6, 
                                                      leafletOutput("map_family", width = "100%",height =600)
                                               ),
                                               column(6, 
                                                      h3(strong("Overview"), align = "left"), 
                                                      p(("We present interactive maps to better understand the services available to students and families in the six community schools. The legend on the top right corner is interactive, allowing the user to filter by desired resource groups. Each colored marker provides a pop-up with the Name of the services, detailed Description, Language, Address, and Website link.  "), align = "justify"),
                                                      h4(("Travel Distance")),
                                                      p(("We also include the driving distances to services from Sterling Elementary School (the blue-tipped marker). Sterling Elementary School is the center point on our map as it is located in the middle of Sterling, CDP. Driving distances for 10 minutes, 20 minutes, and 45 minutes are shown on the map using the green, blue, and red boundaries, respectively. Service markers within these boundaries on the map represent different services available within the respective driving distances.  "), align = "justify"),
                                                      h4(strong("Family Engagement Resources")), 
                                                      p((""), align = "justify"),
                                                      p("Engaging family members such as parents, siblings, grandparents, and neighbors can help create a neighborhood with goals and strategies to ensure student success. These individuals can work together to help monitor students' progress and provide early intervention guidance if necessary. Informed and increase family engagement can improve attendance rates and academic achievement. Additionally, providing adults with educational opportunities can spur students' performance. ", align = "justify"),
                                                      p("As compared to the other resources in this group, services which provide various classes to enrich skillsets and help the family members to find employment are lesser in number and are mostly above 20-minute drive time (Select Education and Employment Help). Resources which provide emergency housing facilities and help with paying rent and utilities are also mostly around the Leesburg area which is just a bit longer than a 20-minute drive. INMED Family Homelessness Prevention And Intervention Program is a major resource inside the Sterling area which rescues families on the edge of homelessness. ", align = "justify"),
                                                      p("Another noteworthy organization which provides multiple resources to women who are victims of domestic violence is LAWS (Loudoun Abused Womens’ Shelter). There are some resources in Ashburn and Leesburg which help families during holidays which are grouped under “Holiday Help”. An important service provider is the Tin Cup Fund which works towards fulfilling the needs of the communities in various ways and have partnerships with organizations like the Cornerstones, Embry Rucker Homeless Shelter, Habitat for Humanity, Women Giving Back and Loudoun Cares, among others. NOVA Diaper Bank is another organization which share partnerships with many of these groups: it distributes diapers (primarily) and also other basic needs for childcare with several drop off locations in and around Sterling. ", align = "justify")
                                                      
                                               )
                                               
                                               
                                     )
                                     
                            ),
                            
                            
                            
                            tabPanel("Youth Development",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h1(strong("Youth Development Opportunities")),
                                                     p(""),
                                                     
                                                     
                                                     
                                                     
                                              )
                                              
                                     ),
                                     
                                     
                                     
                                     fluidPage(style = "margin: 2px;", 
                                               radioButtons(
                                                 inputId = "youth_category",
                                                 label = "Select:",
                                                 choices = c("All Services", "Free Services"),
                                               ),
                                               column(6, 
                                                      leafletOutput("map_youth", width = "100%", height = 600)
                                               ),
                                               column(6, 
                                                      h3(strong("Overview"), align = "left"), 
                                                      p(("We present interactive maps to better understand the services available to students and families in the six community schools. The legend on the top right corner is interactive, allowing the user to filter by desired resource groups. Each colored marker provides a pop-up with the Name of the services, detailed Description, Language, Address, and Website link.  "), align = "justify"),
                                                      h4(("Travel Distance")),
                                                      p(("We also include the driving distances to services from Sterling Elementary School (the blue-tipped marker). Sterling Elementary School is the center point on our map as it is located in the middle of Sterling, CDP. Driving distances for 10 minutes, 20 minutes, and 45 minutes are shown on the map using the green, blue, and red boundaries, respectively. Service markers within these boundaries on the map represent different services available within the respective driving distances.  "), align = "justify"),
                                                      h4(strong("Youth Development Resources")), 
                                                      p(("Students from low-income communities tend to have limited access to activities outside of school which can widen the achievement gap. Services or programs where students can develop social, emotional, physical, and academic skills can improve a student’s performance and behavior. These include athletic events, academic and non-academic clubs, as well as after school programs and family resources. While going through the available resources within Sterling, we chose resources that are given through the Loudoun County school system, and free or reduced cost resources that are available nearby. When you look at the map, there are nine plots (youth development opportunities) that fall within our Sterling defined area.  "), align = "justify"),
                                                      p(("Most of those resources are after school related. CASA is a licensed after-school program that provides students with activities and a fun environment while their parents are working. CASA is in two schools, while serving others. The YMCA is in the 4 other schools. They offer activities and support in homework, sports, fitness, and so much more. A resource that is available within our Sterling defined area is the Sterling Library. The library is a great resource for the students and families. They provide clubs, conversation groups, book clubs, art classes, and more. The Inova Healthy Plate Club is a club that is also located within our Sterling defined area. They provide cooking classes for healthy eating throughout the week. For the athletic and sport lovers, the Sterling Soccer is another resource available within our Sterling defined area. Sterling Soccer provides opportunities to play at a variety of competitive levels, while providing a safe and healthy soccer environment for the youth. "),align = "justify"),
                                               )),
                                     
                            ),
                            tabPanel("List of All Resources", 
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h1(strong("All Services")),
                                                     DT::dataTableOutput("resourcetable"),
                                                     p(""),
                                                     
                                              )),
                                     
                            )),
                 #----------------Recommendations Tab-----------------------
                 
                 navbarMenu("Recommendations",
                            tabPanel("Potential Improvement",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h1(strong("Recommendations for Improvement Opportunities")),
                                                     p(""),
                                                     br("")
                                                     
                                                     
                                                     
                                              )),
                                     fluidPage(style = "margin: 2px;", align = "justify",
                                               
                                               column(7,
                                                      h2(strong("Opportunities Within Pillars")),
                                                      p("We present sociodemographic characteristics and performance statistics visualization to understand the student population in the six Elementary Community Schools in Sterling, Loudoun. Information on students, educators, parents, and school representatives' perceptions was also included in our analysis to understand better these schools' environments and their strengths and challenges. Using these visualizations, we analyze the findings to recommend the following potential opportunities for improvement for each pillar of the Community School Program: "),
                                                      h4(strong("Health and Social Services")),
                                                      p("Community Schools address not only the needs of students but also those of family members. One recommendation is to increase the number of resources and services available to families in Health and Social Services. About 40% of Sterling residents are either uninsured or have public health insurance highlighting the need for affordable, inclusive medical care. This appears to be an important opportunity as Community Schools Liaisons highlighted “health,” “medial,” and “uninsured” as significant challenges and weaknesses facing families at these schools. Moreover, our maps show that most of these medical, dental, and vision services are within a 20-minute distance of Sterling ES (midpoint), which may prove difficult for families with unreliable transportation. The number of these resources drastically reduces if we examine only those that are free. As such, providing annual mobile medical, dental, and vision services can play a significant role for families at these community schools, which may also improve students' health and performance. Community schools could also partner with medical providers to help perform for free or at a reduced cost for preventative and restorative services.  "),
                                                      h4(strong("Mental Health")),
                                                      p("Our research shows a lack of mental health resources within a 10-minute drive for students and the community. There are also some schools where many students perceive bullying as a problem and a low level of social-emotional well-being. Thus, at such a tender age, it would be helpful to include more individual counseling and referral systems to helps students learn emotion management, communication, and self-discipline. Schools can also try to find community organizations and individuals who can provide such counseling services at no or reduced cost to families. This may strengthen families to implement early-intervention strategies to help students who display disruptive behavior and help with their emotional well-being. Engaging families may also help schools adjust any chronic absenteeism that is occurring during the semester."),
                                                      h4(strong("Youth Development")),
                                                      p("Students from low-income families tend to have less access to enrichment opportunities beyond the traditional classroom. Increasing after-school programs, such as athletic clubs, could provide opportunities for youth to build communication skills while remaining healthy and actively learning. Students and parents could also benefit from these programs as students expand their learning and interests while decreasing the time parents must juggle between work and childcare. The 2020-2021 academic school year saw a drop in academic performances for all six community schools. This was especially severe for Hispanic and Black students, that had a dramatic decline. Although this was following the COVID-19 pandemic, this could be an opportunity to provide extra tutoring hours at the local library or a study hall session during the school day to help alleviate the disruptions of online learning. "),
                                                      h4(strong("Family Engagement")),
                                                      p("There is a significantly large number of Hispanic students attending community schools. These students will require additional needs, such as multi-language resources for both students and parents, to ensure a clear line of communication for all involved in the learning process. Additionally, providing more opportunities for parent feedback forums and family events at the school may increase parents’ feelings of positive relationships and communication with the school as some of these parents are undocumented, as suggested by the Community School liaison.  "),
                                                      p("In our research, we found that there are few families engagement resources available within a 10-minute drive of Sterling Elementary, a possible opportunity to help reduce travel time for parents could be to host a resource fair at the schools allowing parents easy access to these resources. Community School Liaisons also indicate that “rent,” “internet,” “housing,” and “food” are also significant challenges for families at these community schools. This is also essential as some students have a large population of students who experience homelessness. Thus, helping families within these areas would be beneficial. ")
                                               ),
                                               column(5, align= "justify",
                                                      h2(strong("Individual School Key Takeaways")),
                                                      p("The following takeaways are conclusions from the school reports that each school representative submitted in the year 2020-2021 and the various visualizations in the previous tabs from the data collected from Virginia Department of Education and Loudoun Dashboard. "),
                                                      h4(strong("Forest Grove")),
                                                      tags$div(
                                                        tags$ul(
                                                          tags$li("Few key partners to support their four pillars"),
                                                          tags$li("School representatives view family engagement as a major challenge "),
                                                          tags$li("Decrease in enrollment rate "),
                                                          tags$li("Decrease in chronic absenteeism "),
                                                          tags$li("Strengthen Mental Health resources – meetings with Unified Mental Health Team"))),
                                                      h4(strong("Guilford")),
                                                      tags$div(
                                                        tags$ul(
                                                          tags$li("Few partners to support youth development pillar "),
                                                          tags$li("Community internet needs reported as a major challenge "),
                                                          tags$li("Good support in Health and Social Services and Family Engagement pillars "),
                                                          tags$li("Parents feel adept at using technology to aid in learning "),
                                                          tags$li("High proportion of Hispanic students  "),
                                                        ),
                                                      ),
                                                      h4(strong("Rolling Ridge")),
                                                      tags$div(
                                                        tags$ul(
                                                          tags$li("Increases in Mental Health resources"),
                                                          tags$li("High proportion of Hispanic students "),
                                                          tags$li("School reports to be well connected with families"),
                                                          tags$li("Lowest student to teacher ratio"),
                                                        )),
                                                      h4(strong("Sterling")),
                                                      tags$div(
                                                        tags$ul(
                                                          tags$li("Mental Health resources are a major challenge as reported by School representative "),
                                                          tags$li("Several partners in Health and Social Services and Youth Development pillar "),
                                                          tags$li("Maintained levels of chronic absenteeism during pandemic"),
                                                        )),
                                                      h4(strong("Sugarland")),
                                                      tags$div(
                                                        tags$ul(
                                                          tags$li("Representatives view Family Engagement as a key challenge "),
                                                          tags$li("Highest levels of chronic absenteeism  "),
                                                          tags$li("Appears to have least number of key partners"),
                                                        )),
                                                      h4(strong("Sully")),
                                                      tags$div(
                                                        tags$ul(
                                                          tags$li("Low performance on Math and English standardized exams "),
                                                          tags$li("Highest student to teacher ratio with the lowest enrollment "),
                                                          tags$li("Large proportion of Hispanic students "),
                                                        )),
                                               ))),
                            tabPanel("NYC Community Schools",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h1(strong("NYC Community Schools Program")),
                                                     p(""),
                                                     br("")
                                                     
                                                     
                                                     
                                              )),
                                     column(9, align = "left", 
                                            
                                            collapsibleTreeOutput("tree2",height = "600px", width = "300%") 
                                            
                                     ),
                                     column(3, align = "justify",
                                            br(),
                                            h4(strong("NYC Community School Program Pillars")),
                                            br(),
                                            p("New York City (NYC) initiated its Community Schools Program in Fall 2014 with 45 schools. It now has over 300 Community Schools across every district in NYC, funded through city, state, and federal dollars. Although New York City has a larger population and more available resources than Loudoun County, the NYC's Community Schools Program can be used to obtain ideas for different programs and initiatives the LCPS may implement for their Community School Initiative. The interactive tree visualizes each pillar's programs and partnerships in the NYC Community Schools.")
                                     )
                                     
                                     
                                     
                            )
                 ),
                 #----------------Data Tab------------------------------------------
                 tabPanel("Data ", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data Sources"), align = "center"),
                                   p("", style = "padding-top:10px;"), 
                                   column(6,
                                          img(src = 'data-acs.png', style = "display: inline; float: left;", width = "250px"),
                                          p("We retrieve ",strong("American Community Survey (ACS)")," data to examine demographic and socioeconomic characteristics of our target population. 
                                            ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets.
                                            We used the most recently available 1-year/5-year estimates, to characterize Loudoun County’s transition aged youths by age, race,
                                            gender, educational attainment, health insurance coverage, and poverty level. ", style = "padding-top:20px;", align = "justify"),
                                          br(), 
                                          br(),
                                          br(),
                                          
                                          img(src = 'VDOEimage.png', style = "display: inline; float: left;", width = "200px"),
                                          p(strong("Virginia Department of Education:"), "The Virginia Department of Education records the information for all Virginia schools county wise. We graphed the demographics such as like enrollment ,chronic absenteeism,  absences and number of educators for the 6 Sterling Community Schools for the years 2016 – 2020. ", style = "padding-top:20px;", align = "justify"), 
                                          br(), 
                                          br(), 
                                          br(), 
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          img(src = 'Loudounimage.png', style = "display: inline; float: left;", width = "200px", height=190),
                                          p("The ", strong("Loudoun County Outreach Services:"), " Holds records of those who use their provided services based on number of persons, percent of student population and year. We graphed several demographics including health coverage, family income, and listed various resources onto maps. ", style = "padding-top:20px;", align = "justify"),
                                          p("", style = "padding-top:10px;")) ,
                                   column(6,
                                          img(src = 'data-LCPS.png', style = "display: inline; float: left;", width = "160px"),
                                          p("The ", strong("Loudoun County Public Schools : "), "Loudoun County Public Schools holds the data for each individual school in its division. It reports the number of students and families who used their provided resources and different demographics such as gender, race/ethnicity, absences, social worker services, and free and reduced lunch. ", style = "padding-top:20px;", align = "justify"),
                                          br(), 
                                          br(), 
                                          br(), 
                                          br(),
                                          br(),
                                          
                                          img(src = 'data-traveltime.png', style = "display: inline; float: left;", width = "200px"),
                                          p(strong("TravelTime")," Travel Time Application Programming Interface (API) aggregates data from OpenStreetMap, transport timetables and speed profiles to generate isochrones. An isochrone is a shape covering all locations that can be reached within the same timeframe given a start location, departure time, and a mode of transportation. We used the Travel Time API to produce isochrones of 10- to 45-minute drive times interval from Sterling Elementary which is the Central point for our 6 community schools.", style = "padding-top:20px;",  align = "justify"),
                                          
                                          
                                   )
                                   
                          )
                          
                 ),
                 #---------------Meet the Team Tab-------------------------           
                 tabPanel("Meet the Team", value = "team",
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h1(strong("Team"), align = "center"),
                                   br(),
                                   h4(strong("VT Data Science for the Public Good"), align = "center"),
                                   p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program offered by the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural and Applied Economics'), 
                                     "In its third year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges 
                                     around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to 
                                     determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, 
                                     how to apply, and our annual symposium, please visit", 
                                     a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official VT DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(4, align = "center",
                                          h4(strong("DSPG Graduate Fellow Member")),
                                          img(src = "Nandini Das.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "200px"),
                                          
                                          p("", style = "padding-top:10px;"), 
                                          p(a(href = 'https://www.linkedin.com/in/nandini-das-390577104/', 'Nandini Das', target = '_blank'), "(Virginia Tech, PhD. Candidate in Economics Department);"),
                                          
                                          p("", style = "padding-top:10px;") 
                                   ),
                                   column(4, align = "center",
                                          h4(strong("DSPG Undergraduate Members")),
                                          img(src = "Amanda Ljuba.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "200px"),
                                          p(a(href = 'https://www.linkedin.com/in/amanda-ljuba-9824551b9', 'Amanda Ljuba', target = '_blank'), "(Virginia Tech, Undergraduate in Sociology with a concentration in Social Inequality);"),
                                          img(src = "Jontayvion Osborne.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "200px"),
                                          p(a(href = 'https://www.linkedin.com/in/jontayvion-osborne-a3b7961a7', 'Jontayvion Osborne', target = '_blank'), "Austin Peay State University, Undergraduate in Business Management and Minor in Marketing) ;"),
                                          img(src = "Chaudhry Abdullah Rizwan.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "200px"),
                                          p(a(href = 'https://www.linkedin.com/in/chaudhry-abdullah-rizwan-a1641522b/', 'Chaudhry Abdullah Rizwan', target = '_blank'), "(Virginia Tech, Undergraduate in Computational Modeling and Data Analytics and Economics, Minors in Computer Science and Mathematics)."),
                                          
                                   ),
                                   column(4, align = "center",
                                          h4(strong("Faculty Advisor")),
                                          img(src = "faculty-chanita.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "200px"),
                                          
                                          p("", style = "padding-top:10px;"), 
                                          p(a(href = "https://aaec.vt.edu/people/faculty/holmes-chanita.html", 'Chanita Holmes', target = '_blank'), "(Virgina Tech, Research Assistant Professor)") , 
                                          
                                          p("", style = "padding-top:10px;")
                                   )) ,
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("Project Stakeholders")),
                                   p(a(href = 'https://www.lcps.org/outreachservices', 'Sarah Eaton', target = '_blank'), "(Supervisor, Outreach Services Loudoun);"),
                                   p(a(href = 'https://loudoun.ext.vt.edu/staff/Vermaak-Stuart.html', 'Stuart Vermaak', target = '_blank'), "(Virginia Cooperative Extension, Loudoun County at Virginia Tech)."),
                                   p("", style = "padding-top:10px;"),
                                   h4(strong("Acknowledgments")) ,
                                   p("We would like to thank Loudoun officials for providing us with data for our project. "),
                                   p("", style = "padding-top:10px;")
                          )
                 )
                 
)


#---------------------- server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  # Render map 
  output$map1 <- renderLeaflet({
    map1
  })
  
  

  
  output$basicsupplies <- renderPlotly({
    basicsupplies
  })
  
  
  Var <- reactive({
    input$demosdrop
  })
  
  
  
  
  output$demoHispanicPIE <- renderPlotly({
    if (Var3() == "race") {
      HispanicPercentagePIE
    }
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
    
    else if (Var4() == "property") {
      
      property
      
      
    }
    
    else {
      
      housing
      
      
    }
    
  })
  
  output$PropComp <- renderPlotly({
    if (Var4() == "property") {
      propcomparison
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
  
  
  #---------performance graphs--------------
  
  Varperf <- reactive({
    input$gradesdrop
  })
  
  category_subject <- reactive({
    input$category_subject
  })
  
  output$grades  <- renderPlotly({
    
    if(category_subject() == "Mathematics") {
      
      if (Varperf() == "allstudentsgrades") {
        
        math_all 
        
      }
      
      else if (Varperf() == "blackgrades") {
        
        math_black
        
      }
      
      else if (Varperf() == "whitegrades") {
        
        math_white
        
      }
      
      
      else if (Varperf() == "asiangrades") {
        
        math_asian
      }
      
      else if (Varperf() == "disabilitiesgrades") {
        
        math_dis
        
        
      }
      
      else if (Varperf() == "hispanicgrades") {
        
        math_hispanic
        
        
      }
      
      else if (Varperf() == "malegrades") {
        
        math_male
        
        
      }
      
      
      else if (Varperf() == "femalegrades") {
        
        math_female
        
        
      }
      
      else if (Varperf() == "homelessgrades") {
        
        math_homeless
        
        
      }
    } else  {
      if  (Varperf() == "allstudentsgrades") {
        
        english_all 
        
      }
      
      else if (Varperf() == "whitegrades") {
        
        english_white
        
      }
      
      else if (Varperf() == "blackgrades") {
        
        english_black
        
      }
      
      else if (Varperf() == "asiangrades") {
        
        english_asian
      }
      
      else if (Varperf() == "disabilitiesgrades") {
        
        english_dis
        
        
      }
      
      else if (Varperf() == "hispanicgrades") {
        
        english_hispanic
        
        
      }
      
      else if (Varperf() == "malegrades") {
        
        english_male
        
        
      }
      
      
      else if (Varperf() == "femalegrades") {
        
        english_female
        
        
      }
      
      else if (Varperf() == "homelessgrades") {
        
        english_homeless
        
        
      }}
    
    
  })
  
  
  
  
  #School Demos
  Var2 <- reactive({
    input$schooldrop1
  }) 
  
  output$cgender <- renderPlotly({
    
    genders 
    
    
  })
  
  output$figHOME <- renderPlotly({
    
    figHOME
    
    
  })
  
  
  output$figIEP <- renderPlotly({
    
    figIEP
    
    
  })
  
  
  output$figELS <- renderPlotly({
    
    figELS
    
    
  })
  
  
  
  output$racenine <- renderPlotly({
    
    racenine 
    
    
  })
  
  output$breakfast <- renderPlotly({
    
    breakfast 
    
    
  })
  
  VarSchool <- reactive({
    
    input$schooldrop2
    
  })
  
  output$ocuplot2<- renderPlotly({
    
    
    if (VarSchool() == "cteacher") {
      
      cteacher
      
    }
    
    else if (VarSchool() == "cenrol") {
      enroll
      
    }
    
  })
  
  VarSchool3 <- reactive({
    
    input$schooldrop3
    
  })
  
  output$ocuplot3<- renderPlotly({
    
    
    if (VarSchool3() == "chronic") {
      
      chronic
      
      
    }
    else if (VarSchool3() == "attend") {
      attend
      
    }
    
  })
  
  
  categoryyear<- reactive({
    input$categoryyear
  }) 
  
  
  Varsuspend <- reactive({
    input$schoolsuspend
  }) 
  
  output$schoolsuspendall <- renderPlotly({
    
    if (categoryyear() == "2018") {
      if(Varsuspend() == "susforest"){
        
        susforest18
      }
      
      
      else if (Varsuspend() == "susguilford") {
        
        susguilford18
        
      }
      
      else if (Varsuspend() == "susrolling") {
        
        susrolling18
      }
      
      else if (Varsuspend() == "susterling") {
        sussterling18
      }
      
      else if (Varsuspend() == "sussugarland"){
        sussugarland18
      }
      
      else if (Varsuspend() == "sussully"){
        sussully18
      }
    } else {
      
      {
        if(Varsuspend() == "susforest"){
          
          susforest19
        }
        
        
        else if (Varsuspend() == "susguilford") {
          
          susguilford19
          
        }
        
        else if (Varsuspend() == "susrolling") {
          
          susrolling19
        }
        
        else if (Varsuspend() == "susterling") {
          sussterling19
        }
        
        else if (Varsuspend() == "sussugarland"){
          sussugarland19
        }
        
        else if (Varsuspend() == "sussully"){
          sussully19
        }
      }
      
      
    }
  })
  
  
  
  #  observeEvent(c(input$schooldrop2, input$ocuplot2, teacherstudentratio), {
  #    req(input$schooldrop2)
  #   if (VarSchool() == "tsratio") {
  #      hide("ocuplot2")
  #   } else {
  #      show("ocuplot2")
  #     hide(teacherstudentratio)
  #    
  #    }
  # 
  # })
  #  
  
  
  
  
  output$hispanicschool <- renderLeaflet({
    
    
    hispanicschool
    
    
    
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
  
  category <- reactive({
    input$category
  })
  
  output$wordcloud <- renderWordcloud2({
    
    if (category() == "Challenges and Weaknesses") {
      
      cloud1
      
    }
    
    else if(category() == "Strengths and Successes") {
      
      cloud2
    }
    
    else if(category() == "Future Goals") {
      
      cloud3
    }
    
  })
  
  
  
  
  
  output$math_all<- renderPlotly({
    math_all
  })
  
  output$english_all<- renderPlotly({
    english_all
  })
  
  output$science_all<- renderPlotly({
    science_all
  })
  
  gendad <- reactive({
    input$generalDATA
  })
  
  output$generaldatafilledlinegraphs <- renderPlotly({
    if (gendad() == "figELS") {
      figELS
    }
    
    else if (gendad() == "figIEP") {
      figIEP
    }
    
    else if (gendad() == "figFRL") {
      figFRL
    }
    
    
    else if (gendad() == "weekendmeals") {
      weekendmeals
    }
    
    else if (gendad() == "families") {
      basicsupplies
    }
    
    else if (gendad() == "breakfast") {
      breakfast
    }
    
    else if (gendad() == "eligablelunch_data") {
      eligablelunch_data
    }
  })
  
  output$map_health <- renderLeaflet({
    if(input$health_category == "Free Services"){
      # call some leaflet plot already made for the free services
      health_free
    }
    else{
      health_all
    }
    
    
  })
  
  output$map_youth <- renderLeaflet({
    if(input$youth_category == "Free Services"){
      youth_free
    }
    else{
      map_youth
    }
    
    
  })
  
  output$map_mental <- renderLeaflet({
    if(input$mental_category == "Free Services"){
      mental_free
    }
    else{
      map_mental
    }
    
  })
  
  output$map_family <- renderLeaflet({
    if(input$family_category == "Free Services"){
      fam_free
    }
    else{
      map_family
    }
  })
  
  output$resourcetable = DT::renderDataTable({
    datatable(list, filter = 'top')
  })
  
  
  output$tree1 <- renderCollapsibleTree({
    
    tree1
    
  })
  
  output$tree2 <- renderCollapsibleTree({
    
    tree2
    
  })
  
  category_subject <- reactive({
    input$category_subject
  })
  
  category_subgroup <- reactive({
    input$category_subgroup
  })
  
  
  Vargrade <- reactive({
    input$schoolgradesdrop
  }) 
  
  output$schoolgrades <- renderPlotly({
    
    if (category_subgroup() == "Race") {
      
      if (category_subject() == "Mathematics") {
        
        if (Vargrade() == "forestgroverace") {
          
          forestgroverace
          
        }
        
        else if (Vargrade() == "guilfordrace") {
          
          guilfordrace
        }
        
        else if (Vargrade() == "rollingrace") {
          
          rrrace
        }
        
        else if (Vargrade() == "sterlingrace") {
          sterlingrace
        }
        
        else if (Vargrade() == "sugarlandrace") {
          sugarlandrace
        }
        
        else if (Vargrade() == "sullyrace") {
          sullyrace
        }
      } else if (category_subject() == "English Reading") {
        
        if (Vargrade() == "forestgroverace") {
          
          forestgroveraceeng
          
        }
        
        else if (Vargrade() == "guilfordrace") {
          
          guilfordraceeng
        }
        
        else if (Vargrade() == "rollingrace") {
          
          rrraceeng
        }
        
        else if (Vargrade() == "sterlingrace") {
          sterlingraceeng
        }
        
        else if (Vargrade() == "sugarlandrace") {
          sugarlandraceeng
        }
        
        else if (Vargrade() == "sullyrace") {
          sullyraceeng
        }
      } 
    } else if (category_subgroup() == "Gender") {
      
      if (category_subject() == "Mathematics") {
        
        if (Vargrade() == "forestgroverace") {
          
          forestgrovegender
          
        }
        
        else if (Vargrade() == "guilfordrace") {
          
          guilfordgender
        }
        
        else if (Vargrade() == "rollingrace") {
          
          rrgender
        }
        
        else if (Vargrade() == "sterlingrace") {
          sterlinggender
        }
        
        else if (Vargrade() == "sugarlandrace") {
          sugarlandgender
        }
        
        else if (Vargrade() == "sullyrace") {
          sullygender
        }
      } else {
        
        if (Vargrade() == "forestgroverace") {
          
          forestgrovegendereng
          
        }
        
        else if (Vargrade() == "guilfordrace") {
          
          guilfordgendereng
        }
        
        else if (Vargrade() == "rollingrace") {
          
          rrgendereng
        }
        
        else if (Vargrade() == "sterlingrace") {
          sterlinggendereng
        }
        
        else if (Vargrade() == "sugarlandrace") {
          sugarlandgendereng
        }
        
        else if (Vargrade() == "sullyrace") {
          sullygendereng
        }
      }
    }  else {
      
      if (category_subject() == "Mathematics") {
        
        if (Vargrade() == "forestgroverace") {
          
          forestgroveall
          
        }
        
        else if (Vargrade() == "guilfordrace") {
          
          guilfordall
        }
        
        else if (Vargrade() == "rollingrace") {
          
          rrall
        }
        
        else if (Vargrade() == "sterlingrace") {
          sterlingall
        }
        
        else if (Vargrade() == "sugarlandrace") {
          sugarlandall
        }
        
        else if (Vargrade() == "sullyrace") {
          sullyall
        }
      } else {
        
        if (Vargrade() == "forestgroverace") {
          
          forestgrovealleng
          
        }
        
        else if (Vargrade() == "guilfordrace") {
          
          guilfordalleng
        }
        
        else if (Vargrade() == "rollingrace") {
          
          rralleng
        }
        
        else if (Vargrade() == "sterlingrace") {
          sterlingalleng
        }
        
        else if (Vargrade() == "sugarlandrace") {
          sugarlandalleng
        }
        
        else if (Vargrade() == "sullyrace") {
          sullyalleng
        }
      }
    }
    
  })
  
  
}
shinyApp(ui = ui, server = server)


