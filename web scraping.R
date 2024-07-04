library(tidyverse)
library(rvest)
library(readxl)

#all tourney dates the Wednesday before tourney starts
tourney_dates <- c("2001-03-14", "2002-03-13", "2003-03-19", "2004-03-17",
                   "2005-03-16", "2006-03-15", "2007-03-14", "2008-03-19", "2009-03-18",
                   "2010-03-17", "2011-03-16", "2012-03-14", "2013-03-20", "2014-03-19",
                   "2015-03-18", "2016-03-16", "2017-03-15", "2018-03-13", "2019-03-20",
                   "2021-03-17", "2022-03-16", "2023-03-15", "2024-03-20")

#######################################################
###### FG % ####################################
##########################################################

#reading in the URl for year 2000
url <-  "https://www.teamrankings.com/ncaa-basketball/stat/shooting-pct?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

#call the table and set the year to 2000
df = df[[1]]
df$Rank = 2000

#loop through 2001 - 2023 URL's and assign year to each set
for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/shooting-pct?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

#write csv
write.csv(df, "field_goal_percentage.csv")


#######################################################
###### 3 point % ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/three-point-pct?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/three-point-pct?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "three_point_percentage.csv")


#######################################################
###### Free throw  % ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/free-throw-pct?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/free-throw-pct?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "freethrow_percentage.csv")


#######################################################
###### total rebounds per game ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/total-rebounds-per-game?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/total-rebounds-per-game?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "total_rebounds_pergame.csv")


#######################################################
###### assists per game ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/assists-per-game?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/assists-per-game?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "assists_pergame.csv")


#######################################################
###### steals per game ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/steals-per-game?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/steals-per-game?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "steals_pergame.csv")


#######################################################
###### blocks per game ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/blocks-per-game?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/blocks-per-game?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "blocks_pergame.csv")


#######################################################
###### turnovers per game ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/turnovers-per-game?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/turnovers-per-game?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "turnovers_pergame.csv")


#######################################################
###### Personal fouls per game ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/personal-fouls-per-game?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/personal-fouls-per-game?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "fouls_pergame.csv")


#######################################################
###### points per game ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/points-per-game?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/points-per-game?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "points_pergame.csv")

#######################################################
###### opponent fg% ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/opponent-shooting-pct?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/opponent-shooting-pct?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "opp_field_goal_percentage.csv")


#######################################################
###### opponent points per game ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/opponent-points-per-game?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/opponent-points-per-game?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "opp_points_pergame.csv")


#######################################################
###### win/loss percentage ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/win-pct-all-games?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/win-pct-all-games?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "win_loss_percentage.csv")


#######################################################
###### pace of play ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/possessions-per-game?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/possessions-per-game?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "pace.csv")


#######################################################
###### offensive efficiency - unadj ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/offensive-efficiency?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/offensive-efficiency?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "offensive_efficiency.csv")



#######################################################
###### defensive efficiency - unadj  ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/defensive-efficiency?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/defensive-efficiency?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "defensive_efficiency.csv")


#######################################################
###### effective fg%  ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/effective-field-goal-pct?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/effective-field-goal-pct?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "effective_field_goal_percentage.csv")


#######################################################
###### offensive rebounds ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/offensive-rebounds-per-game?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/offensive-rebounds-per-game?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "offensive_rebounds_pergame.csv")

#######################################################
###### avg MOV ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/average-scoring-margin?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/average-scoring-margin?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "avg_MOV.csv")

#######################################################
###### extra scoring chances PG####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/extra-chances-per-game?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/extra-chances-per-game?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "extraScoringChancesPG.csv")

#######################################################
###### games played ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/games-played?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/games-played?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "gamesPlayed.csv")


#######################################################
###### RPI_Total ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncb/rpi/?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncb/rpi/?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "rpi_total.csv")


#######################################################
###### RPI SOS ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/rpi-ranking/sos-rpi-rating-by-team?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/rpi-ranking/sos-rpi-rating-by-team?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "rpi_sos.csv")


#######################################################
###### Opp TO_pg ####################################
##########################################################

url <-  "https://www.teamrankings.com/ncaa-basketball/stat/opponent-turnovers-per-game?date=2000-03-15"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

df = df[[1]]
df$Rank = 2000


for (i in tourney_dates) {
  url1 <-  "https://www.teamrankings.com/ncaa-basketball/stat/opponent-turnovers-per-game?date="
  url <- paste(url1, i, sep = "")
  #print(url)
  df2 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  #print(df2[[1]])
  df2[[1]]$Rank = year(i)
  
  df <- rbind(df, setNames(df2[[1]] ,names(df)))
  #print(df)
}

write.csv(df, "oppTOpg.csv")



####################################################################
########## joining pre-tourney scores with A/B table ##############
##################################################################
full_doc <- read_excel("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/march_madness.xlsx", sheet = 9)
matches <- read_excel("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/march_madness.xlsx", sheet = 3)


##############################################
############## FG Perc
################################################
fgperc <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/field_goal_percentage.csv")

#assigning key/value pairs for team names
fgperc <- left_join(fgperc, matches, by = c('Team' = 'Key'))

#trimming table
fgperc <- fgperc %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(FGPerc = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, fgperc, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, fgperc, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## 3 point Perc
################################################
threepointperc <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/three_point_percentage.csv")

#assigning key/value pairs for team names
threepointperc <- left_join(threepointperc, matches, by = c('Team' = 'Key'))

#trimming table
threepointperc <- threepointperc %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(ThreePointPerc = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, threepointperc, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, threepointperc, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## free throw perc
################################################
freethrowperc <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/freethrow_percentage.csv")

#assigning key/value pairs for team names
freethrowperc <- left_join(freethrowperc, matches, by = c('Team' = 'Key'))

#trimming table
freethrowperc <- freethrowperc %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(FreeThrowPerc = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, freethrowperc, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, freethrowperc, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## total rebounds
################################################
totalrebounds <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/total_rebounds_pergame.csv")

#assigning key/value pairs for team names
totalrebounds <- left_join(totalrebounds, matches, by = c('Team' = 'Key'))

#trimming table
totalrebounds <- totalrebounds %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(TotalRebounds = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, totalrebounds, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, totalrebounds, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## assists per game
################################################
assistspg <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/assists_pergame.csv")

#assigning key/value pairs for team names
assistspg <- left_join(assistspg, matches, by = c('Team' = 'Key'))

#trimming table
assistspg <- assistspg %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(AssistsPG = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, assistspg, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, assistspg, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## steals per game
################################################
stealspg <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/steals_pergame.csv")

#assigning key/value pairs for team names
stealspg <- left_join(stealspg, matches, by = c('Team' = 'Key'))

#trimming table
stealspg <- stealspg %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(StealsPG = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, stealspg, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, stealspg, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## blocks per game
################################################
blockspg <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/blocks_pergame.csv")

#assigning key/value pairs for team names
blockspg <- left_join(blockspg, matches, by = c('Team' = 'Key'))

#trimming table
blockspg <- blockspg %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(BlocksPG = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, blockspg, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, blockspg, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## turnovers per game
################################################
turnoverspg <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/turnovers_pergame.csv")

#assigning key/value pairs for team names
turnoverspg <- left_join(turnoverspg, matches, by = c('Team' = 'Key'))

#trimming table
turnoverspg <- turnoverspg %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(TurnoversPG = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, turnoverspg, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, turnoverspg, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## fouls per game
################################################
personalfoulsPG <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/fouls_pergame.csv")

#assigning key/value pairs for team names
personalfoulsPG <- left_join(personalfoulsPG, matches, by = c('Team' = 'Key'))

#trimming table
personalfoulsPG <- personalfoulsPG %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(PFpergame = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, personalfoulsPG, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, personalfoulsPG, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## points per game
################################################
ppg <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/points_pergame.csv")

#assigning key/value pairs for team names
ppg <- left_join(ppg, matches, by = c('Team' = 'Key'))

#trimming table
ppg <- ppg %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(PointsPG = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, ppg, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, ppg, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## opponent fg percent per game
################################################
oppfgpct <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/opp_field_goal_percentage.csv")

#assigning key/value pairs for team names
oppfgpct <- left_join(oppfgpct, matches, by = c('Team' = 'Key'))

#trimming table
oppfgpct <- oppfgpct %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(OppFGPerc = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, oppfgpct, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, oppfgpct, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## opponents points per game
################################################
opp_ppg <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/opp_points_pergame.csv")

#assigning key/value pairs for team names
opp_ppg <- left_join(opp_ppg, matches, by = c('Team' = 'Key'))

#trimming table
opp_ppg <- opp_ppg %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(Opp_PPG = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, opp_ppg, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, opp_ppg, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## offensive efficiency - unadj
################################################
off_eff <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/offensive_efficiency.csv")

#assigning key/value pairs for team names
off_eff <- left_join(off_eff, matches, by = c('Team' = 'Key'))

#trimming table
off_eff <- off_eff %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(Off_Eff = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, off_eff, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, off_eff, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## defnesive efficiency - unadj
################################################
def_eff <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/defensive_efficiency.csv")

#assigning key/value pairs for team names
def_eff <- left_join(def_eff, matches, by = c('Team' = 'Key'))

#trimming table
def_eff <- def_eff %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(Def_Eff = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, def_eff, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, def_eff, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## win loss percentage
################################################
winlossperc <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/win_loss_percentage.csv")

#assigning key/value pairs for team names
winlossperc <- left_join(winlossperc, matches, by = c('Team' = 'Key'))

#trimming table
winlossperc <- winlossperc %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(WinLossPercentage = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, winlossperc, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, winlossperc, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## pace
################################################
posspg <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/pace.csv")

#assigning key/value pairs for team names
posspg <- left_join(posspg, matches, by = c('Team' = 'Key'))

#trimming table
posspg <- posspg %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(PacePG = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, posspg, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, posspg, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## effective fg%
################################################
efgperc <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/effective_field_goal_percentage.csv")

#assigning key/value pairs for team names
efgperc <- left_join(efgperc, matches, by = c('Team' = 'Key'))

#trimming table
efgperc <- efgperc %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(eFGPerc = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, efgperc, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, efgperc, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## offensive rebounds
################################################
oreb <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/offensive_rebounds_pergame.csv")

#assigning key/value pairs for team names
oreb <- left_join(oreb, matches, by = c('Team' = 'Key'))

#trimming table
oreb <- oreb %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(OReb = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, oreb, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, oreb, by = c('Year' = 'Rank', 'TeamB' = 'Value'))

##############################################
############## avg MOV
################################################
avgMOV <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/avg_MOV.csv")

#assigning key/value pairs for team names
avgMOV <- left_join(avgMOV, matches, by = c('Team' = 'Key'))

#trimming table
avgMOV <- avgMOV %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(avgMOV = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, avgMOV, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, avgMOV, by = c('Year' = 'Rank', 'TeamB' = 'Value'))


##############################################
############## extra scoring chances PG
################################################
xtraScorepg <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/extraScoringChancesPG.csv")

#assigning key/value pairs for team names
xtraScorepg <- left_join(xtraScorepg, matches, by = c('Team' = 'Key'))

#trimming table
xtraScorepg <- xtraScorepg %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(xtraScorepg = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, xtraScorepg, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, xtraScorepg, by = c('Year' = 'Rank', 'TeamB' = 'Value'))


##############################################
############## games Played
################################################
gamesPlayedpg <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/gamesPlayed.csv")

#assigning key/value pairs for team names
gamesPlayedpg <- left_join(gamesPlayedpg, matches, by = c('Team' = 'Key'))

#trimming table
gamesPlayedpg <- gamesPlayedpg %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(gamesPlayedpg = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, gamesPlayedpg, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, gamesPlayedpg, by = c('Year' = 'Rank', 'TeamB' = 'Value'))


##############################################
############## rpi
################################################
rpi_total <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/rpi_total.csv")

#subbing out the win/loss records fromthe column
rpi_total$Team <- sub(" \\([^()]*\\)$", "", rpi_total$Team)

#assigning key/value pairs for team names
rpi_total <- left_join(rpi_total, matches, by = c('Team' = 'Key'))

#trimming table
rpi_total <- rpi_total %>%
  dplyr::select(Rank, Value, Rating) %>%
  rename(rpi_total = Rating)

#for whatever reason, team rankings is missing these specific values, so I am manually imputing them from another website
rpi_total[303, "rpi_total"] = "0.572" #arkansas 2000
rpi_total[302, "rpi_total"] = "0.599" #auburn 2000
rpi_total[299, "rpi_total"] = "0.611" #florida 2000
rpi_total[300, "rpi_total"] = "0.638" #kentucky 2000
rpi_total[298, "rpi_total"] = "0.619" #lsu 2000
rpi_total[297, "rpi_total"] = "0.622" #oklahoma 2000
rpi_total[301, "rpi_total"] = "0.620" #tennessee 2000
rpi_total[1601, "rpi_total"] = "0.640" #uconn 2004
rpi_total[1602, "rpi_total"] = "0.622" #kansas 2004
rpi_total[1603, "rpi_total"] = "0.601" #nevada 2004
rpi_total[1604, "rpi_total"] = "0.581" #utep 2004
rpi_total[1607, "rpi_total"] = "0.564" #pacific 2004
rpi_total[1614, "rpi_total"] = "0.522" #vermont 2004
rpi_total[1619, "rpi_total"] = "0.458" #alabama st 2004


#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, rpi_total, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, rpi_total, by = c('Year' = 'Rank', 'TeamB' = 'Value'))


##############################################
############## effective fg%
################################################
oppTO <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/oppTOpg.csv")

#assigning key/value pairs for team names
oppTO <- left_join(oppTO, matches, by = c('Team' = 'Key'))

#trimming table
oppTO <- oppTO %>%
  dplyr::select(Rank, Value, X1999) %>%
  rename(oppTO = X1999)

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, oppTO, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, oppTO, by = c('Year' = 'Rank', 'TeamB' = 'Value'))


##############################################
############## rpi_sos
################################################
rpi_sos <- read.csv("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/rpi_sos.csv")

rpi_sos$Team <- sub(" \\([^()]*\\)$", "", rpi_sos$Team)

#assigning key/value pairs for team names
rpi_sos <- left_join(rpi_sos, matches, by = c('Team' = 'Key'))

#trimming table
rpi_sos <- rpi_sos %>%
  dplyr::select(Rank, Value, Rating) %>%
  rename(rpi_sos = Rating)

#for whatever reason, team rankings is missing these specific values, so I am manually imputing them from another website
rpi_sos[303, "rpi_sos"] = "0.571" #arkansas 2000
rpi_sos[302, "rpi_sos"] = "0.562" #auburn 2000
rpi_sos[299, "rpi_sos"] = "0.556" #florida 2000
rpi_sos[300, "rpi_sos"] = "0.617" #kentucky 2000
rpi_sos[298, "rpi_sos"] = "0.546" #lsu 2000
rpi_sos[297, "rpi_sos"] = "0.559" #oklahoma 2000
rpi_sos[301, "rpi_sos"] = "0.562" #tennessee 2000
rpi_sos[1601, "rpi_sos"] = "0.581" #uconn 2004
rpi_sos[1602, "rpi_sos"] = "0.591" #kansas 2004
rpi_sos[1603, "rpi_sos"] = "0.557" #nevada 2004
rpi_sos[1604, "rpi_sos"] = "0.522" #utep 2004
rpi_sos[1607, "rpi_sos"] = "0.496" #pacific 2004
rpi_sos[1614, "rpi_sos"] = "0.452" #vermont 2004
rpi_sos[1619, "rpi_sos"] = "0.433" #alabama st 2004

#pull in Team A /Team B stats pre tourney
full_doc <- left_join(full_doc, rpi_sos, by = c('Year' = 'Rank', 'TeamA' = 'Value'))
full_doc <- left_join(full_doc, rpi_sos, by = c('Year' = 'Rank', 'TeamB' = 'Value'))


####################################################################
###### Write march madness to new csv #########
#################################################################

write.csv(full_doc, "test.csv")


####################################################################
###### create additional columns #########
#################################################################

#### week 6 top 12 AP Poll
full_doc <- read_excel("~/Data Science/Random Stuff/March Madness/UPDATED MARCH MADNESS/march_madness.xlsx")

#confirm thre is 12 & the champ
ap12_2000 <- c("Stanford", "UConn", "Arizona", "Cincinnati", "Michigan State", "UNC", "Auburn", "Florida", "Syracuse", "Duke", "Tennessee", "Kansas")
ap12_2001 <- c("Duke", "Michigan State", "Stanford", "Tennessee", "Illinois", "Wake Forest", "Florida", "Seton Hall", "Kansas", "Arizona", "UConn", "Syracuse")
ap12_2002 <- c("Duke", "Kansas", "Florida", "Virginia", "Oklahoma State", "Kentucky", "Illinois", "Maryland", "Iowa", "Missouri", "Boston College", "Oklahoma")
ap12_2003 <- c("Alabama", "Pitt", "Duke", "Arizona", "Oklahoma", "UConn", "Illinois", "Notre Dame", "Texas", "Indiana", "Oregon", "Florida")
ap12_2004 <- c("Stanford", "Kentucky", "Gonzaga", "Oklahoma State", "St. Joseph's", "Duke", "UConn", "Mississippi State", "Pitt", "Wisconsin", "Cincinnati", "Texas")
ap12_2005 <- c("Illinois", "Kansas", "Oklahoma State", "UNC", "Wake Forest", "Duke", "Syracuse", "Kentucky", "Georgia Tech", "Pitt", "UConn", "Washington")
ap12_2006 <- c("Duke", "UConn", "Villanova", "Memphis", "Florida", "Illinois", "Oklahoma", "Gonzaga", "Washington", "Michigan State", "Louisville", "UCLA")
ap12_2007 <- c("UCLA", "UNC", "Ohio State", "Wisconsin", "Florida", "Duke", "Pitt", "Wichita State", "Arizona", "Alabama", "Kansas", "LSU")
ap12_2008 <- c("UNC", "Memphis", "Kansas", "Texas", "Georgetown", "Duke", "Washington State", "UCLA", "Michigan State", "Marquette", "Pitt", "Tennessee")
ap12_2009 <- c("UNC", "UConn", "Pitt", "Oklahoma", "Duke", "Wake Forest", "Gonzaga", "Notre Dame", "Texas", "Purdue", "Michigan State", "Georgetown")
ap12_2010 <- c("Kansas", "Texas", "Kentucky", "Purdue", "Syracuse", "West Virginia", "Duke", "Villanova", "Michigan State", "UNC", "UConn", "Kansas State")
ap12_2011 <- c("Duke", "Ohio State", "Kansas", "UConn", "Syracuse", "Pitt", "San Diego State", "Villanova", "Missouri", "Georgetown", "Kansas State", "Michigan State")
ap12_2012 <- c("Syracuse", "Ohio State", "Kentucky", "Louisville", "UNC", "Baylor", "Duke", "UConn", "Missouri", "Marquette", "Florida", "Kansas")
ap12_2013 <- c("Duke", "Michigan", "Syracuse", "Arizona", "Louisville", "Indiana", "Ohio State", "Florida", "Kansas", "Illinois", "Cincinnati", "Missouri")
ap12_2014 <- c("Arizona", "Syracuse", "Ohio State", "Wisconsin", "Michigan State", "Louisville", "Oklahoma State", "Duke", "Villanova", "UConn", "Wichita State", "Baylor")
ap12_2015 <- c("Kentucky", "Duke", "Arizona", "Louisville", "Virginia", "Wisconsin", "Villanova", "Gonzaga", "Texas", "Kansas", "Wichita State", "Iowa State")
ap12_2016 <- c("Michigan State", "Kansas", "Oklahoma", "Maryland", "Virginia", "Xavier", "UNC", "Arizona", "Butler", "Providence", "Iowa State", "Kentucky")
ap12_2017 <- c("Villanova", "UCLA", "Kansas", "Baylor", "Duke", "Kentucky", "Gonzaga", "UNC", "Creighton", "Louisville", "West Virginia", "Virginia")
ap12_2018 <- c("Villanova", "Michigan State", "Arizona State", "Duke", "UNC", "Miami (FL)", "Kentucky", "Texas A&M", "Xavier", "West Virginia", "Wichita State", "Gonzaga")
ap12_2019 <- c("Kansas", "Duke", "Tennessee", "Michigan", "Virginia", "Nevada", "Auburn", "Gonzaga", "UNC", "Michigan State", "Florida State", "Texas Tech")
ap12_2021 <- c("Gonzaga", "Baylor", "Villanova", "Texas", "Iowa", "Kansas", "Creighton", "Wisconsin", "Tennessee", "Michigan", "Houston", "Illinois")
ap12_2022 <- c("Baylor", "Duke", "Purdue", "Gonzaga", "UCLA", "Arizona", "Kansas", "USC", "Iowa State", "Alabama", "Michigan State", "Auburn")
ap12_2023 <- c("Purdue", "UConn", "Houston", "Kansas", "Arizona", "Virginia", "Texas", "Tennessee", "Alabama", "Arkansas", "Gonzaga", "Baylor")


full_doc$x2000 <- ifelse(((full_doc$Year == 2000) & ((full_doc$TeamA %in% ap12_2000) | (full_doc$TeamB %in% ap12_2000))), 1, 0)
full_doc$x2001 <- ifelse(((full_doc$Year == 2001) & ((full_doc$TeamA %in% ap12_2001) | (full_doc$TeamB %in% ap12_2001))), 1, 0)
full_doc$x2002 <- ifelse(((full_doc$Year == 2002) & ((full_doc$TeamA %in% ap12_2002) | (full_doc$TeamB %in% ap12_2002))), 1, 0)
full_doc$x2003 <- ifelse(((full_doc$Year == 2003) & ((full_doc$TeamA %in% ap12_2003) | (full_doc$TeamB %in% ap12_2003))), 1, 0)
full_doc$x2004 <- ifelse(((full_doc$Year == 2004) & ((full_doc$TeamA %in% ap12_2004) | (full_doc$TeamB %in% ap12_2004))), 1, 0)
full_doc$x2005 <- ifelse(((full_doc$Year == 2005) & ((full_doc$TeamA %in% ap12_2005) | (full_doc$TeamB %in% ap12_2005))), 1, 0)
full_doc$x2006 <- ifelse(((full_doc$Year == 2006) & ((full_doc$TeamA %in% ap12_2006) | (full_doc$TeamB %in% ap12_2006))), 1, 0)
full_doc$x2007 <- ifelse(((full_doc$Year == 2007) & ((full_doc$TeamA %in% ap12_2007) | (full_doc$TeamB %in% ap12_2007))), 1, 0)
full_doc$x2008 <- ifelse(((full_doc$Year == 2008) & ((full_doc$TeamA %in% ap12_2008) | (full_doc$TeamB %in% ap12_2008))), 1, 0)
full_doc$x2009 <- ifelse(((full_doc$Year == 2009) & ((full_doc$TeamA %in% ap12_2009) | (full_doc$TeamB %in% ap12_2009))), 1, 0)
full_doc$x2010 <- ifelse(((full_doc$Year == 2010) & ((full_doc$TeamA %in% ap12_2010) | (full_doc$TeamB %in% ap12_2010))), 1, 0)
full_doc$x2011 <- ifelse(((full_doc$Year == 2011) & ((full_doc$TeamA %in% ap12_2011) | (full_doc$TeamB %in% ap12_2011))), 1, 0)
full_doc$x2012 <- ifelse(((full_doc$Year == 2012) & ((full_doc$TeamA %in% ap12_2012) | (full_doc$TeamB %in% ap12_2012))), 1, 0)
full_doc$x2013 <- ifelse(((full_doc$Year == 2013) & ((full_doc$TeamA %in% ap12_2013) | (full_doc$TeamB %in% ap12_2013))), 1, 0)
full_doc$x2014 <- ifelse(((full_doc$Year == 2014) & ((full_doc$TeamA %in% ap12_2014) | (full_doc$TeamB %in% ap12_2014))), 1, 0)
full_doc$x2015 <- ifelse(((full_doc$Year == 2015) & ((full_doc$TeamA %in% ap12_2015) | (full_doc$TeamB %in% ap12_2015))), 1, 0)
full_doc$x2016 <- ifelse(((full_doc$Year == 2016) & ((full_doc$TeamA %in% ap12_2016) | (full_doc$TeamB %in% ap12_2016))), 1, 0)
full_doc$x2017 <- ifelse(((full_doc$Year == 2017) & ((full_doc$TeamA %in% ap12_2017) | (full_doc$TeamB %in% ap12_2017))), 1, 0)
full_doc$x2018 <- ifelse(((full_doc$Year == 2018) & ((full_doc$TeamA %in% ap12_2018) | (full_doc$TeamB %in% ap12_2018))), 1, 0)
full_doc$x2019 <- ifelse(((full_doc$Year == 2019) & ((full_doc$TeamA %in% ap12_2019) | (full_doc$TeamB %in% ap12_2019))), 1, 0)
full_doc$x2021 <- ifelse(((full_doc$Year == 2021) & ((full_doc$TeamA %in% ap12_2021) | (full_doc$TeamB %in% ap12_2021))), 1, 0)
full_doc$x2022 <- ifelse(((full_doc$Year == 2022) & ((full_doc$TeamA %in% ap12_2022) | (full_doc$TeamB %in% ap12_2022))), 1, 0)
full_doc$x2023 <- ifelse(((full_doc$Year == 2023) & ((full_doc$TeamA %in% ap12_2023) | (full_doc$TeamB %in% ap12_2023))), 1, 0)

write.csv(full_doc, "test.csv")
