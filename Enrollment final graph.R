
library(readxl)
library(dplyr)
library(ggplot2)
library(tidycensus)
library(viridis)
subset_sterling <- sterling[c(29:33),]
percNum <- c()
for(i in 1:5){
  percNum <- c(percNum, as.numeric(sub("%", "", subset_sterling[i, "...4"])))
}
subset_sterling[, "...4"] <- percNum
ggplot(subset_sterling, aes(x = `EMPLOYMENT STATUS`, y = (...4), fill = `EMPLOYMENT STATUS`)) + geom_bar(position = "stack", stat="identity", width = 0.5)+ theme(axis.text.x = element_text(angle=90), legend.position="none") + labs(x = "Occupations", y = "percentages", caption = " Source : DP03 ACS 5-yr data 2016-2020", title = "Work Occupations") + coord_flip()
