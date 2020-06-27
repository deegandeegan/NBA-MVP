# Load Packages
library(rvest)
library(tidyverse)
library(ggrepel)


# remove all of the objects that are stored in your global environment 
rm(list = ls())

# Clear console
cat("\014")  

set.seed(4321)

#############################################
' Web Scrapping'
#############################################

# Which years you want to gather
years <- c(2000:2019)

# empty list for the URLs of each year
urls <- list()

# Create for loop to get the links of each web page
for (i in 1:length(years)){
  url = paste0("https://www.basketball-reference.com/awards/awards_",years[i],".html#mvp")
  urls[[i]] = url
}

# empty tbl list
MVP_tbl <- list()

# Which year you're starting with
years <- 2000

# For the 'for loop' below
j <- 1
for (j in seq_along(urls)) {
  MVP_tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table()
  MVP_tbl[[j]]$Year = years
  j = j+1
  years = years+1
}

# Create a data frame from each element in this list
NBA <-  plyr::ldply(MVP_tbl, data.frame)

# Make the first row the Column Names
colnames(NBA) <- as.character(NBA[1,])

# Change last Column's name
colnames(NBA)[21] <- c('Year')

# Create a data frame of standings per year of the EAST
years <- c(2000:2019)
urls <- list()

# Create for loop to get the links of each web page
for (i in 1:length(years)){
  url = paste0("https://www.basketball-reference.com/leagues/NBA_",years[i],"_standings.html#all_confs_standings_E")
  urls[[i]] = url
}

# Create a tbl for East standings
East_tbl <- list()
years <- 2000
j <- 1
for (j in seq_along(urls)) {
  East_tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table()
  East_tbl[[j]]$Year = years
  j = j+1
  years = years+1
}

# Create a data frame from each element in this list
East_NBA_standings <- plyr::ldply(East_tbl, data.frame)

# colnames(NBA_standings) <- as.character(NBA_standings[1,])
# NBA_standings = NBA_standings[-1,]
colnames(East_NBA_standings)[9] <- c('Year')


# Create a data frame of standings per year of the WEST
years <- c(2000:2019)
urls <- list()

# Create for loop to get the links of each web page
for (i in 1:length(years)){
  url = paste0("https://www.basketball-reference.com/leagues/NBA_",years[i],"_standings.html#all_confs_standings_E")
  urls[[i]] = url
}

# Tbl for west standings starting from 2000
West_tbl <- list()
years <- 2000
j <- 1
for (j in seq_along(urls)) {
  West_tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table()
  West_tbl[[j]]$Year = years
  j = j+1
  years = years+1
}

# Create a data frame from each element in this list
West_NBA_standings <- plyr::ldply(West_tbl, data.frame)

# Rename the 1st column
colnames(East_NBA_standings)[1] <- "Record"
colnames(West_NBA_standings)[1] <- "Record"

# Combine both East & West standings
standings <- rbind(East_NBA_standings, West_NBA_standings)

# Filter out any row that has the word 'Division' in the Record column
standings <- standings %>%
  filter(!grepl("Division", Record))

# To remove * "\\*" to remove ), ")" and to do both in the same time throw a | in the middle
standings$Record <-  gsub("\\*|)", "" , standings$Record)

# Seperate Record into Team & Seed
standings <- standings %>%
  separate(Record, c("Team", "Seed"), "[(]")

# Remove extra space after team names
standings$Team <- str_trim(standings$Team)

# Which years you want to gather
years <- c(2000:2019)

# empty list for URLs of the years
adv_urls <- list()

# Create for loop to get the links of each web page
for (i in 1:length(years)){
  url = paste0("https://www.basketball-reference.com/leagues/NBA_",years[i],"_advanced.html")
  adv_urls[[i]] = url
}

# empty tbl list
adv_tbl <- list()

# Which year you're starting with
years <- 2000

# For the 'for loop' below
j <- 1
for (j in seq_along(adv_urls)) {
  adv_tbl[[j]] = adv_urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    #.[1] %>%
    html_table()
  adv_tbl[[j]]$Year = years
  j = j+1
  years = years+1
}

# Create a data frame from each element in this list
advanced <- plyr::ldply(adv_tbl, data.frame)

# Select Columns - Player, PER, TS., Year
adv <- advanced[, c(2, 8, 9, 30)]

# Remove text
adv$Player <- gsub("[*]", "", adv$Player)
adv$PER <-as.numeric(adv$PER)
adv$TS. <- as.numeric(adv$TS.)

# filter out inputted errors
adv <- adv %>%
  filter(PER > 0 & PER < 40 , TS. > 0)


# There are certain players who played on a few teams in one year, so we need to avg there PER & TS.
adv_avg <- aggregate(adv[, 2:3], list(adv$Player, adv$Year), mean)

names(adv_avg)[1:2] <- c("Player", "Year")

#############################################
' Data Preparation '
#############################################


# Filter out all rows containing the word "Rank" in column Rank
NBA <- NBA %>%
  filter(!grepl("Rank", Rank))

# Remove the T in the Rank Column
NBA$Rank <- str_remove(NBA$Rank, "T")

# Columns that need to be changed from character type to numeric
i <- c(5:20)

# Apply as.numeric() to columns 5 through 20 in mvp data frame
NBA[ , i] <- apply(NBA[ , i], 2,function(x) as.numeric(as.character(x)))

# Load the dataframe names - 36 NBA team names w/ abbreviations 
names <- read.csv("names.csv", sep = ",", stringsAsFactors = F)
str(names)

# Combine Names df with standings on Team columns
standings_v2 <- merge(standings, names, by.x = "Team", by.y = "Team")
standings_v3 <- standings_v2[,c(2, 3, 4, 10, 11)]

# Merge NBA df with newly created df "standings_v3"
all <- merge(NBA, standings_v3, by.x = c("Tm", "Year"), by.y = c("Tm", "Year"))

# Arange DF by Year & Rank columns
all <- all  %>%
  arrange(Year, Rank)

# left join all & adv dfs on columns Player and Year. You are using the left_join because the only thing missing from the
# all df is the PER and TS. However, you only need it for the rows in the all df 
all <- left_join(all, adv_avg, by = c("Player", "Year"))

# Change 'WS/48' to just WS48
colnames(all)[21] <- "WS48"

# Mutate columns in 'all' to numeric type
str(all)
all[,c(3, 5, 22, 23, 24, 25, 26)] <- lapply(all[,c(3, 5, 22, 23, 24, 25, 26)], as.numeric)

# Create a df that ranks where the MVPS ended up each year. The Rankings range from 1 - 5 because it's ranking the top 5 MVP canidates of each year
big_5 <- all %>%
  group_by(Year) %>%
  filter(Rank < 6) %>%
  mutate(Points_Rank = order(order(PTS, decreasing = T))) %>%
  mutate(Rebounds_Rank = order(order(TRB, decreasing = T))) %>%
  mutate(Assists_Rank = order(order(AST, decreasing = T))) %>%
  mutate(Steals_Rank = order(order(STL, decreasing = T))) %>%
  mutate(Blocks_Rank = order(order(BLK, decreasing = T))) %>%
  mutate(Per_Rank = order(order(PER, decreasing = T)))  %>%
  mutate(TS_Rank = order(order(TS., decreasing = T)))%>%
  mutate(WS_Rank = order(order(WS, decreasing = T)))%>%
  mutate(Wins_Rank = order(order(W, decreasing = T)))


# Turn into df
big_5 <- as.data.frame(big_5)

# If a column is a interger mutate it into a numeric
big_5 <- mutate_if(big_5, is.integer, as.numeric)
big_5$Sum <- rowSums(big_5[,27:34], na.rm=TRUE)

# Create a df that ranks where the MVPS ended up compared to top 5 players of from 2000-2020
bigger_5 <- big_5 %>%
  mutate(All_Points_Rank = order(order(PTS, decreasing = T))) %>%
  mutate(All_Rebounds_Rank = order(order(TRB, decreasing = T))) %>%
  mutate(All_Assists_Rank = order(order(AST, decreasing = T))) %>%
  mutate(All_Steals_Rank = order(order(STL, decreasing = T))) %>%
  mutate(All_Blocks_Rank = order(order(BLK, decreasing = T))) %>%
  mutate(All_Per_Rank = order(order(PER, decreasing = T)))  %>%
  mutate(All_TS_Rank = order(order(TS., decreasing = T)))%>%
  mutate(All_WS_Rank = order(order(WS, decreasing = T)))%>%
  mutate(All_Wins_Rank = order(order(W, decreasing = T)))


# Turn into df
bigger_5 <- as.data.frame(bigger_5)

# If a column is a interger mutate it into a numeric
bigger_5 <- mutate_if(bigger_5, is.integer, as.numeric)
bigger_5$All_Sum <- rowSums(bigger_5[,37:43], na.rm=TRUE)

# Load 2019-20 MVP canidates
twenty <- read.csv("2020_v2.csv", sep = ",")

# Create Test data set to predict on
test_data <- twenty %>%
  filter(Rk < 6) %>%
  mutate(Points_Rank = order(order(PTS, decreasing = T))) %>%
  mutate(Rebounds_Rank = order(order(TRB, decreasing = T))) %>%
  mutate(Assists_Rank = order(order(AST, decreasing = T))) %>%
  mutate(Steals_Rank = order(order(STL, decreasing = T))) %>%
  mutate(Blocks_Rank = order(order(BLK, decreasing = T))) %>%
  mutate(Per_Rank = order(order(PER, decreasing = T)))  %>%
  mutate(TS_Rank = order(order(TS., decreasing = T)))%>%
  mutate(WS_Rank = order(order(WS, decreasing = T)))%>%
  mutate(Wins_Rank = order(order(W, decreasing = T)))

# If a column is a interger mutate it into a numeric
test_data <- mutate_if(test_data, is.integer, as.numeric)
test_data$Sum <- rowSums(test_data[,25:33], na.rm=TRUE)

# Create a df of the mvps
first <- all %>%
  filter(Rank == "1")

# Modifiy the type of last 5 columns
first[, 22:26]<- sapply(first[, 22:26], as.numeric)

# Modify Year column to factor
first$Year <- as.factor(first$Year)

# Create a df that ranks the MVPS against eachother
king <- first %>%
  mutate(Points_Rank = order(order(PTS, decreasing = T))) %>%
  mutate(Rebounds_Rank = order(order(TRB, decreasing = T))) %>%
  mutate(Assists_Rank = order(order(AST, decreasing = T))) %>%
  mutate(Steals_Rank = order(order(STL, decreasing = T))) %>%
  mutate(Blocks_Rank = order(order(BLK, decreasing = T)))%>%
  mutate(TS_Rank = order(order(TS., decreasing = T)))%>%
  mutate(WS_Rank = order(order(WS, decreasing = T))) %>%
  mutate(PER_Rank = order(order(PER, decreasing = T)))%>%
  mutate(Wins_Rank = order(order(W, decreasing = T)))

king <- as.data.frame(king)
# If there are columns that are int type then mutate them to be numeric type
king <- mutate_if(king, is.integer, as.numeric)

# Add a sum column
king$Sum <- rowSums(king[,c(27:35)], na.rm=TRUE)
