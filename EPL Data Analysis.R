# BACKGROUND STUFF
# Verify that R version 3.1.2 is being used.  Should be True if not follow steps
# 1-3 here: http://reed.edu/data-at-reed/software/R/r_studio.html
R.Version()$major == '3' & R.Version()$minor == '1.2'

# Install necessary packages
pkg <- c("devtools", "rvest", "ggthemes",  "dplyr", "ggplot2",
         "ggmap", "RColorBrewer", "htmlwidgets", "RCurl", "scales")

# Install rvest package
if (!require("rvest")) {
  devtools::install_github("hadley/rvest")
}

# Update all packages
update.packages(checkBuilt=TRUE, ask=FALSE)

# Load all packages
library(rvest)
library(tidyr)
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
rank <- wp_data %>% select(Pos, Team, Pts, W, L, D, GF, GA, GD)
  
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

alldata <- sdata %>% group_by(Team) %>%
  summarize(succ_pass = sum(Total.Successful.Passes.All), 
            fail_pass = sum(Total.Unsuccessful.Passes.All))
alldata <- mutate(alldata, pass_perc = succ_pass/(succ_pass+fail_pass)*100)
alldata <- mutate(alldata, pass_total = (succ_pass+fail_pass))

#Add rank 
alldata <- left_join(alldata, rank, by = "Team")

#Plot
ggplot(alldata, aes(x =pass_total, y = pass_perc, color = Pos, label = Pos)) + 
  geom_point(size = 4) + 
  ylab("Successful Passes (%)") + 
  xlab("Total Passes Attempted") +
  ggtitle("Total Passes vs. Successful Passes in 2011-12 EPL") + 
  scale_colour_gradient("Rank") +
  theme_classic() + 
  geom_text(aes(label=Pos),hjust=0, vjust=2) + 
  geom_smooth(method=lm)

#I guess this is intuitive: what we might be seeing is that teams that make more successful passes will, 
#by definition, hold on to the ball longer. This allows them to make more passes, which explains 
#why the two variables increase linearly. Then, if a team holds the ball for longer and makes more passes,
#it's easy to imagine that they will have create more goal scoring opportunites AND prevent their 
#opponents from creating scoring opportunities. Both of these factors will make a team more likely 
#to win games, and thus end up in a higher position at the end of the season.

#---PREDICTIVE MODEL---
# Can we make a model that will predict the better teams? We'll try to use percents, since applying 
# a model mid-season will make percentages more applicable than a total count.

# Data
teamdata <- mdata %>% 
  group_by(Team) %>% 
  summarize(tot_first_goals = sum(First.Goal), 
            def_to = sum(Unsuccessful.Passes.Defensive.third),
            def_ok = sum(Successful.Passes.Defensive.third),
            KP = sum(Key.Passes), 
            shotson = sum(Shots.On.Target.inc.goals),
            shotsoff = sum(Shots.Off.Target.inc.woodwork),
            bad_touch = sum(Unsuccessful.Ball.Touch),
            disp = sum(Dispossessed)) %>%
  mutate(def_to_perc = def_to/(def_to+def_ok)) %>%
  mutate(first_goals_per_game = tot_first_goals/38) 

alldata <- left_join(alldata, teamdata, by="Team") %>%
  mutate(pts_per_game = Pts/38) %>%
  mutate(Lpg = L/38) %>%
  mutate(Kpg = KP/38) %>%
  mutate(Spg = (shotson+shotsoff)/38) %>%
  mutate(Dpg = disp/38)

# Shots per Game
ggplot(alldata, aes(Spg, pts_per_game)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# Passing Percentage
ggplot(alldata, aes(pass_perc, pts_per_game)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# Number of Dispossessions per game.
ggplot(alldata, aes(Dpg, pts_per_game)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# Number of Key Passes per Game
ggplot(alldata, aes(Kpg, pts_per_game)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# Simple Linear Model
model <- lm(pts_per_game ~ pass_perc + Kpg + Spg, data=alldata)
b <- coefficients(model)
b

# Test it with stats from the 2013-2014 season. How does our model predict the teams will stack up?
# We need to find data for each teams passing percentage and their number of losses per game. 

# Getting this data is tough, and scraping websites wont work well since some try to prevent that, 
# which means I'll need to manually insert it. 

# Add key passes data.
next.season <- rank$Team %>% as.data.frame()
names(next.season)[1] <- "Team"
rank$Team[15] <- "Crystal Palace"
rank$Team[17] <- "West Ham"
rank$Team[18] <- "Hull"
rank$Team[19] <- "Cardiff"
rank$Team[20] <- "Norwich"
next.season$Kpg <- c(14.4, 10.3, 10.7, 11.7, 11.2, 
                     13.5, 11.1, 13.1, 8.5, 9.6, 
                     10.2, 9.1, 9.4, 8.7, 8.2, 
                     8.8, 8.4, 8, 7.8, 9.1)

# Add pass percentage data.
next.season$pass_perc <- c(85.5, 84.0, 85.5, 81.1, 79.4, 
                           82.7, 83.2, 84.1, 78.0, 77.1, 
                           84.8, 75.9, 77.5, 77.3, 70.2, 
                           73.2, 73.1, 76.1, 75.4, 75.9)


# Add shots per game to data.
next.season$Spg <- c(17.7, 13.8, 13.8, 15.4, 15.2,
                           18.2, 14.8, 17.1, 11.3, 12.8,
                           13, 12.3, 12.9, 11.3, 10.9, 
                           11.3, 11.1, 11.3, 11, 12.3)

# Add dispossessions per game to data.
next.season$Dpg <- c(10.8, 13, 9.6, 15.8, 11.8,
                     10.3, 12.4, 11.2, 12.7, 13.5,
                     10.5, 12.9, 10.8, 12.5, 9.5, 
                     9.5, 9.8, 11, 12.9, 12.9)


# Apply model: 
next.season$pts_pred <- predict(model, next.season)*38

# How does this compare with actual results?
next.season$pts_actual <- c(86, 64, 79, 69, 49,
                            82, 72, 84, 50, 36, 
                            42, 33, 38, 50, 45,
                            38, 40, 37, 30, 33) 

# Arrange data for plot.
next.season <- gather(next.season, "pts_type", "pts", 6:7)

# Sort values for x-axis of plot.
next.season$Team <- factor(next.season$Team, 
                         levels = subset(next.season$Team, 
                                         next.season$pts_type == "pts_pred")[order(next.season$pts)])

ggplot(next.season, aes(Team, pts, col= pts_type)) + 
  geom_point(size = 3) +
  theme(axis.text.x=element_text(angle=45, hjust=1))  +
  xlab("Team") +
  ylab("Points") +
  ggtitle("Actual Points vs. Point Predictions\n For 2013-2014 EPL Season")
  
# Looks like we still have a bit of work to do! At least we seem to be predicting the top guys, 
# for the most part. The middle guys are really giving us a lot of trouble though...

# Also, we could use the difference between actual and predicted results as an R-squared estimate.