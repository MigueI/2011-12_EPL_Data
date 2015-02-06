# BACKGROUND STUFF
# Verify that R version 3.1.2 is being used.  Should be True if not follow steps
# 1-3 here: http://reed.edu/data-at-reed/software/R/r_studio.html
R.Version()$major == '3' & R.Version()$minor == '1.2'

# Install necessary packages
pkg <- c("devtools", "rvest", "ggthemes",  "dplyr", "ggplot2",
         "ggmap", "RColorBrewer", "htmlwidgets", "RCurl", "scales")

new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)
}
# Install rvest and leaflet packages
if (!require("rvest")) {
  devtools::install_github("hadley/rvest")
}
if (!require("leaflet")) {
  devtools::install_github("rstudio/leaflet")
}

# Update all packages
update.packages(checkBuilt=TRUE, ask=FALSE)

# Load all packages
library(rvest)
library(leaflet)
library(devtools)
library(htmlwidgets)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggmap)
library(RColorBrewer)
library(magrittr)
library(RCurl)
library(scales)

#---IMPORT DATA---

#Main Data
mdata<-read.csv("/Users/Miguel/Desktop/Projects/MC2012Data.csv")
#Note that not all columns are visibile from the view window.

#Let's start by simplifying our data set. First, pick a few key variabiles. 
sdata <- mdata %>% select(Player.Surname, Player.Forename, Team, Time.Played, Goals, Position.Id)

#Scrape Wikipedia for season ending information:
webpage <- html("http://en.wikipedia.org/wiki/2011%E2%80%9312_Premier_League")
# Look at third of all HTML tables
wp_data <- webpage %>% html_nodes("table") %>% .[[5]] %>% html_table(,fill=TRUE)
rank <- wp_data %>% select(Pos, Team)
rank[[1,2]] <- "Manchester City"
rank[[18,2]] <- "Bolton Wanderers"
rank[[19,2]] <- "Blackburn Rovers"
rank[[20,2]] <- "Wolverhampton Wanderers"
  
#This function changes a number to a name. We'll use it later.
posi <- function(arg) {
  if(arg == 1) {
    return("Goalie")
  } else if(arg == 2) {
    return("Defense")
  } else if(arg == 4) {
    return("Midfield")
  } else if(arg == 6) {
    return("Striker")
  } 
}

#---ALL TEAMS---

alldata <- sdata %>% arrange(Team, Player.Surname) 

#Now we want to look at the total minutes each player played over the course of the season. 

alldata %<>% group_by(Team, Player.Surname, Player.Forename, Position.Id) %>% 
  summarise(tottime = sum(Time.Played), totgoals = sum(Goals))
mutate(alldata, Rank = 0)

#Add rank into our dataset.
for(i in seq(1:nrow(alldata))) {
  for(j in seq(1:nrow(rank))) {
    ifelse(alldata$Team[[i]] == rank$Team[[j]], alldata$Rank[[i]] <- rank$Pos[[j]],"") 
  }
}

alldata %<>% group_by(Rank) %>% arrange(Rank)

#Replace number with Name for position id
for(i in seq(1:nrow(alldata))) {
  alldata$Position.Id[i] = posi(alldata$Position.Id[i])
}

#The plot!
ggplot(alldata, aes(x = tottime, y = totgoals, color = desc(Rank))) + 
  geom_point(size = 3) + facet_wrap(~Position.Id) + xlab("Total Minutes Played") + 
  ylab("Total Goals Scored") + 
  ggtitle("Goals Scored vs. Minutes Played\nOver 2012-2013 EPL Season") +
  scale_colour_gradient("Rank at End\nof Season", high = "orange", low = "blue") +
  theme_classic()
