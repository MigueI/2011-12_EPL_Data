#---BACKGROUND STUFF---
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
mdata <- tbl_df(mdata)
#Note that not all columns are visibile from the view window.

#Let's start by simplifying our data set. First, pick a few key variabiles. 
sdata <- mdata %>% select(Player.Surname, Player.Forename, Team,  Total.Successful.Passes.All, Total.Unsuccessful.Passes.All)

#Scrape Wikipedia for season ending information:
webpage <- html("http://en.wikipedia.org/wiki/2011%E2%80%9312_Premier_League")
# Look at third of all HTML tables
wp_data <- webpage %>% html_nodes("table") %>% .[[4]] %>% html_table(,fill=TRUE)
wp_data[[1,2]] <- "Manchester City"
wp_data[[18,2]] <- "Bolton Wanderers"
wp_data[[19,2]] <- "Blackburn Rovers"
wp_data[[20,2]] <- "Wolverhampton Wanderers"
rank <- wp_data %>% select(Pos, Team)

#---Percentage Passes---

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
ggplot(alldata, aes(x = desc(Rank), y = pass_perc, color = pass_total)) + 
  geom_point() + ylab("Successful Passes (%)")
