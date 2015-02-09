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
# Install rvest package
if (!require("rvest")) {
  devtools::install_github("hadley/rvest")
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
mdata<-read.csv("/Users/Miguel/Desktop/Projects/2011-12_EPL_Data/MC2012Data.csv")
#Note that not all columns are visibile from the view window.

#Let's start by simplifying our data set. First, pick a few key variabiles. 
sdata <- mdata %>% select(Player.Surname, Player.Forename, Team, Time.Played, Goals, Position.Id)

#Scrape Wikipedia for season ending information:
webpage <- html("http://en.wikipedia.org/wiki/2011%E2%80%9312_Premier_League")

#Look at fifth of all HTML tables on the page. 
#NOTE: Import might go wrong if Wikipedia page changes. 
wp_data <- webpage %>% html_nodes("table") %>% .[[5]] %>% html_table(,fill=TRUE)
wp_data[[1,2]] <- "Manchester City"
wp_data[[18,2]] <- "Bolton Wanderers"
wp_data[[19,2]] <- "Blackburn Rovers"
wp_data[[20,2]] <- "Wolverhampton Wanderers"
rank <- wp_data %>% select(Pos, Team)
  
#This function changes a position id number to a name. We'll use it later.
posi <- function(arg) {
  if(arg == 1) {
    return("Goalie")
  } else if(arg == 2) {
    return("Defender")
  } else if(arg == 4) {
    return("Midfielder")
  } else if(arg == 6) {
    return("Striker")
  } 
}

#---GOALS vs MINUTES---

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
  geom_point(size = 2) + facet_wrap(~Position.Id) + xlab("Total Minutes Played") + 
  ylab("Total Goals Scored") + 
  ggtitle("Goals Scored vs. Minutes Played\nOver 2011-2012 EPL Season") +
  scale_colour_gradient("Rank at End\nof Season", high = "orange", low = "blue") +
  theme_classic()

#First off: the goalkeeper who scored was Tim Howard, off of a clearance and with some help
#from the wind. The top scoring striker was Robin Van Persie with 35 goals.
#The trend from the three "field" positions is pretty clear: the longer you play, 
#the more you tend to score. However, the rate of scoring is different depending on your position.
#As a defender, you only score a couple of goals if you play A LOT. As a forward, it's your job
#to score goals, so we see the slope is greater. 

#---PASSING PERCENTAGE---
sdata <- mdata %>% select(Player.Surname, Player.Forename, Team,  
                          Total.Successful.Passes.All, Total.Unsuccessful.Passes.All)
alldata <- sdata %>% arrange(Team, Player.Surname) 
alldata %<>% group_by(Team) %>% 
  summarise(succ_pass = sum(Total.Successful.Passes.All), fail_pass = sum(Total.Unsuccessful.Passes.All))
alldata <- mutate(alldata, pass_perc = succ_pass/(succ_pass+fail_pass)*100)
alldata <- mutate(alldata, pass_total = (succ_pass+fail_pass))

#Add rank
for(i in seq(1:nrow(alldata))) {
  for(j in seq(1:nrow(rank))) {
    ifelse(alldata$Team[[i]] == rank$Team[[j]], alldata$Rank[[i]] <- rank$Pos[[j]],"") 
  }
}
alldata %<>% group_by(Rank) %>% arrange(Rank)


#Plot
ggplot(alldata, aes(x =pass_total, y = pass_perc, color = Rank, label = Rank)) + 
  geom_point(size = 4) + ylab("Successful Passes (%)") + xlab("Total Passes Attempted") +
  ggtitle("Total Passes vs. Successful Passes in 2011-12 EPL") + 
  scale_colour_gradient("Rank") +
  theme_classic() + geom_text(aes(label=Rank),hjust=0, vjust=2) + geom_smooth(method=lm)

#I guess this is intuitive: what we might be seeing is that teams that make more successful passes will, 
#by definition, hold on to the ball longer. This allows them to make more passes, which explains 
#why the two variables increase linearly. Then, if a team holds the ball for longer and makes more passes,
#it's easy to imagine that they will have create more goal scoring opportunites AND prevent their 
#opponents from creating scoring opportunities. Both of these factors will make a team more likely 
#to win games, and thus end up in a higher position at the end of the season.

