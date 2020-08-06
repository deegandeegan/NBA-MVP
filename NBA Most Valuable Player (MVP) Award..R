# Load Packages
library(rvest)
library(tidyverse)
library(ggrepel)
library(RColorBrewer)
# Machine Learning packages
library(caret)
library(nnet)
library(mlbench)
library(leapp)
library(GGally)
# Classification
library(AppliedPredictiveModeling)


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
  filter(PER > 0 & PER < 40, TS. > 0)


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


####################################################
' Have to reload all.csv due to changes in basketball reference website'
###################################################

all <- read.csv("all.csv", sep = ",")

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

# This is to see what makes an MVP
legend <- big_5 %>%
  filter(Seed == 1 &  Per_Rank == 1 & (Rebounds_Rank == 1 | Assists_Rank == 1 | Steals_Rank == 1 | TS_Rank == 1))

# This is to see what makes an MVP
legend2 <- big_5 %>%
  filter((WS_Rank == 1 | Points_Rank ==1 | Rebounds_Rank == 1 ) & Seed == 1 )

legend3 <- big_5 %>%
  filter(Points_Rank == 1 & Seed == 1 )

# Need to make year a factor (not continuous) for the boxplots to work
all$Year <- as.factor(all$Year)


# How Many Different MVPs have there been?
length(unique(first$Player))

# make a DF with average from each year
all$Age <- as.numeric(as.character(all$Age))

# This creates a year by year for each statistic
avg <- all %>%
  group_by(Year) %>%
  summarise_at(vars(-Player, Tm, Rank), funs(mean(., na.rm=TRUE)))

# The average taken from each mvp for all statistics
mvp_avg <- first %>%
  summarise_all(funs(mean))

# Below is an example of what the code does above, but for all the statistics not just PER
mean(first$`PER`)



# How many is a three part analysis. The first is creating the how_many df, which ranks each player in stats by their year.
# The second part filters to only show MVPS, titled 'hm;
# Thir third part is titled 'answer', which is counting how many time the MVP was first in each statistic
how_many <- all %>%
  group_by(Year) %>%
  mutate(All_Points_Rank = order(order(PTS, decreasing = T))) %>%
  mutate(All_Rebounds_Rank = order(order(TRB, decreasing = T))) %>%
  mutate(All_Assists_Rank = order(order(AST, decreasing = T))) %>%
  mutate(All_Steals_Rank = order(order(STL, decreasing = T))) %>%
  mutate(All_Blocks_Rank = order(order(BLK, decreasing = T))) %>%
  mutate(All_Per_Rank = order(order(PER, decreasing = T)))  %>%
  mutate(All_TS_Rank = order(order(TS., decreasing = T)))%>%
  mutate(All_WS_Rank = order(order(WS, decreasing = T)))%>%
  mutate(All_Wins_Rank = order(order(W, decreasing = T)))


# Count how many times the MVP was first in each cateorgoery
hm <- how_many %>%
  filter(Rank == 1)

# Part 3
answer <- plyr::ldply(hm[,27:35], function(c) sum(c== 1))
colnames(answer) <- c("Stat", "MVPs")

# Below is an example of one statistic which 'answer' calculates. It shows which 12 MVPs lead their season of MVP canidates in WS 
example_answer <- how_many %>%
  filter(Rank == 1 & All_WS_Rank == 1)


# MVPs with a 1 seed and lead in PER
legend4 <- how_many %>%
  filter(Seed == 1 & All_Per_Rank == 1 & (All_Points_Rank == 1 |  All_WS_Rank == 1))

# Players who won MVP with the 1 seed but NOT the best PER
anti_leg <-how_many %>%
  filter(Rank == 1 & Seed == 1 & All_Per_Rank != 1)



#############################################
'Data Visualization'
#############################################

# Define the number of colors you want
nb.cols <- 20
mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(nb.cols)

# 'My_theme' will be used in multiple graphs, so it's best to create it to minimise code
My_theme <- theme(axis.text = element_text(face = "bold", size = 14),
                  axis.title.y = element_text(size = 15),
                  panel.background = element_blank())

#change Year from num type to factor
all$Year <- as.factor(all$Year)

# Points by Season
s1 <- ggplot(all, aes(y=PTS, x= Year, fill = Year)) + 
  geom_boxplot()+
  # A line that shows the average of the data. Seemed to clutter the visualization 
  # geom_hline(yintercept = mean(all$PTS), color="blue") + 
  theme_classic() +
  scale_fill_manual(values = mycolors) +
  ylab("Points Per Game")+
  scale_y_continuous(limits = c(0,40)) +
  My_theme  +
  geom_label_repel(
    all %>% filter(PTS > 35 | PTS < 10),
    mapping = aes(label = Player))

s1 + theme(legend.position = "none") + 
  theme(axis.title.x = element_blank()) +
  labs(title = "PPG by Season",
       subtitle = "Includes all MVP candidates",
       caption = "Data source: Basketball Reference")

# https://www.r-graph-gallery.com/275-add-text-labels-with-ggplot2.html



# Rebounds Per Game
s2 <- ggplot(all, aes(x = Year, y = TRB, fill = Year)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = mycolors) +
  ylab("Rebounds Per Game") +
  
  # This was created above S1 
  My_theme +
  # labels extreme outliers
  geom_label_repel(
    # Anyone who avg more than 14 rebounds per game
    all %>% filter(TRB > 14),
    mapping = aes(label = Player))



s2 + theme(legend.position = "none") + 
  theme(axis.title.x = element_blank()) +
  labs(title = "RPG by Season",
       subtitle = "Includes all MVP candidates",
       caption = "Data source: Basketball Reference")


# Assists Summary
s3 <- ggplot(all, aes(x = Year, y = AST, fill = Year)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = mycolors)+
  ylab("Assists Per Game") +
  My_theme +
  geom_label_repel(
    all %>% filter(AST > 11),
    mapping = aes(label = Player))

s3 + theme(legend.position = "none") + 
  theme(axis.title.x = element_blank()) +
  labs(title = "APG by Season",
       subtitle = "Includes all MVP candidates",
       caption = "Data source: Basketball Reference")




# PER for each player
s4 <- ggplot(all, aes(x = Year, y = PER, fill = Year)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = mycolors)+
  ylab("Player Efficiency Rating") +
  My_theme +
  geom_label_repel(
    all %>% filter(PER > 31 | PER < 17),
    mapping = aes(label = Player))

s4 + theme(legend.position = "none") + 
  theme(axis.title.x = element_blank()) +
  labs(title = "PER by Season",
       subtitle = "Includes all MVP candidates",
       caption = "Data source: Basketball Reference")

# Add labels to rownames so that thye appear in graphs below
rownames(first) <- make.names(first$Player, unique=T)


f1 <- ggplot(first, aes(x = Year, y = PTS, group = 1))+
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Points Per Game") +
  My_theme  +
  geom_label_repel(
    first %>% filter(PTS > 11),
    mapping = aes(label = Player))


f1 + theme(legend.position = "none") + 
  theme(axis.title.x = element_blank()) +
  labs(title = "MVP Points Per Game by Year",
       caption = "Data source: Basketball Reference")



f2 <- ggplot(first, aes(x = Year, y = PER, group = 1))+
  geom_point() +
  geom_line() +
  ylab("PER") +
  My_theme +
  geom_label_repel(
    first %>% filter(PTS > 11),
    mapping = aes(label = Player))


f2 + theme(legend.position = "none") + 
  theme(axis.title.x = element_blank()) +
  labs(title = "MVP PER by Season",
       caption = "Data source: Basketball Reference")


# Ranking MVPS
big_first <- big_5 %>%
  filter(Rank == 1)



# df with selected columns
k2 <- king[,c(2, 4, 12, 13, 14, 15, 16, 20, 23, 25, 26)]
k2$Year <- as.numeric(as.character(k2$Year))

# Create a "long" df out of the k2 df
king_long <- k2 %>%
  pivot_longer(-c(Year, Player), names_to = "Stat", values_to = "Value")

# Define the number of colors you want
nb.cols <- 20
mycolors_v2 <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)

# Create a bar plot for each statistic in the king_long data set
ggplot(king_long, aes(x = Year, y = Value))  +
  geom_bar(stat = "identity", aes(fill = factor(Player))) + 
  facet_wrap(facets = ~reorder(Stat, -Value), scales = "free") +
  scale_fill_manual(values = mycolors_v2) + 
  labs(fill = "MVP by Season") +
  theme(axis.text.y = element_text(face = "bold", size = 12)) +
  My_theme


############################################################
'THIS IS FOR THE 2ND BLOG AKA THE MACHINE LEARNING PART !!!'
############################################################

#############################################
'2020 Predictions using Machine Learing'
#############################################
# Machine Learning packages
library(caret)
library(nnet)
# library(mlbench)
# library(Boruta)
# library(leapp)
library(GGally)
library(tidyverse)
# RF
library(rattle)
#ANN
library(NeuralNetTools)
# Classification
library(AppliedPredictiveModeling)
## Loading DMwr to balance the unbalanced class
library(DMwR)
# rpart plot
library(rattle)
library(rpart)
library(neuralnet)
library(ggrepel)




'We are reloading the big_5.csv in case there are users that simply want to run the ML portion of this project. 
Big_five data should be in your Global Environment if you have ran everything before this'

# Load Data
five <- read.csv("big_5.csv", sep = ",")
head(five, 15)

# replace NAs with 0
five[is.na(five)] <- 0

# load test data set. This is the data set with the canidates from 2020
test_data2020 <- read.csv("test.csv", sep = ",")


#############################################
'Binary Classification'
#############################################

# The target variable is "Rank"
c_model <- five[,c(2, 4, 12, 13, 14, 15, 16, 20,21,  22, 23, 25, 26 , 36, 3)]

# Turn Rank into factor class
c_model$Rank <- as.factor(c_model$Rank)

# Add a column "Results" with yes or no answer
c_model <- c_model %>%
  mutate(Results = ifelse(Rank == 1, "MVP", "Not_MVP")) #%>%

# Make Results column a factor type
c_model$Results <- as.factor(c_model$Results)

# Create binary c_model
c_model2 <- c_model %>%
  # Include all columns besides Rank
  select(-Rank)

set.seed(4321)
# create a single 75/25% split of the Results data:
trainIndex <- createDataPartition(c_model2$Results, p = .75, list = FALSE, times = 1)

# Training and Testing data
Train_C2 <- c_model2[ trainIndex,]
Test_C2  <- c_model2[-trainIndex,]


set.seed(4321)
# Logisitic - binary
c_logistic_model2 <- glm(Results ~ ., data = Train_C2[,3:15], family = binomial)

set.seed(4321)
# decision tree model
c_tree_model2 <- rpart(Results ~ ., data = Train_C2[,3:15])

set.seed(4321)
# ANN model
c_ann_model2 <- neuralnet(Results ~ ., data = Train_C2[,3:15], hidden = c(2,1))

# Apply model to make prediction on Testing set
logistic_pred_2 <-ifelse(round(predict(c_logistic_model2, Test_C2[,-15], type = "response")) == 0,"MVP","Not_MVP")
tree_pred_2 <-predict(c_tree_model2, Test_C2[,-15], type = "class") 
ann_pred_2 <-ifelse(round(predict(c_ann_model2, Test_C2[,-15], type ="t") [,2]) == 0,"MVP","Not_MVP")

# Model performance (Displays confusion matrix and statistics)
logistic_results_2 <- confusionMatrix(factor(logistic_pred_2), Test_C2$Results,  positive = "MVP")
tree_results_2 <- confusionMatrix(tree_pred_2, Test_C2$Results, positive = "MVP")
ann_results_2 <- confusionMatrix(factor(ann_pred_2), Test_C2$Results,  positive = "MVP")

# Results on binary classification
logistic_results_2
tree_results_2
ann_results_2



'Now doing the UPGRADED BINARY CLASSIFICATION MODELS'

# SMOTE
over_2 <- SMOTE(Results ~., Train_C2, perc.over = 400, perc.under = 125,  k = 5)
over_2$Results <- factor(over_2$Results, levels = c("MVP", "Not_MVP"))

# Check target variable's distribution
table(over_2$Results)

# fit control
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

set.seed(4321)
# logistic
upgraded_c_logistic_model2 <- train(Results ~ ., data = over_2[,3:15], 
                                    method = "glmboost", 
                                    trControl = fitControl)

set.seed(4321)
## Decision Tree
upgraded_c_tree_model2 <- train(Results ~ ., data = over_2[,3:15],
                                method = "rpart",
                                tuneLength = 10,
                                trControl = fitControl)

# decay is the weight decay, and there are three tuning values. size is the number of hidden units.
nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))
set.seed(4321)
## ANN model
upgraded_c_ann_model2 <- train(Results ~ ., data = over_2[,3:15],
                               method = "nnet",
                               trControl = fitControl,
                               preProcess = c('center', 'scale'),
                               tuneGrid = nnetGrid)

## Make prediction
upgraded_logistic_pred_2 <-predict(upgraded_c_logistic_model2, Test_C2[,-15])
upgraded_tree_pred_2 <-predict(upgraded_c_tree_model2, Test_C2[,-15])
upgraded_ann_pred_2 <-predict(upgraded_c_ann_model2, Test_C2[,-15]) 


# Results
upgraded_logistic_results_2 <- confusionMatrix(upgraded_logistic_pred_2, Test_C2$Results)
upgraded_tree_results_2 <- confusionMatrix(upgraded_tree_pred_2, Test_C2$Results,  positive = "MVP")
upgraded_ann_results_2 <- confusionMatrix(upgraded_ann_pred_2, Test_C2$Results,  positive = "MVP")


# Results of updated binary classification model
upgraded_logistic_results_2
upgraded_tree_results_2
upgraded_ann_results_2


logistic_results_2
upgraded_logistic_results_2


tree_results_2
upgraded_tree_results_2

ann_results_2
upgraded_ann_results_2





#############################################
'Multiclass Classification'
#############################################

set.seed(4321)
trainIndex <- createDataPartition(c_model$Rank, p = .75, list = FALSE, times = 1)
# Include everything besides the "Results" Column
c5 <- c_model[,1:15]

# Training and Testing data
Train_C5 <- c5[ trainIndex,]
Test_C5  <- c5[-trainIndex,]

# Build Training model
set.seed(4321)
c_log_model5 <- multinom(Rank ~ ., data = Train_C5[,3:15])

set.seed(4321)
c_tree_model5 <- rpart(Rank ~ ., data = Train_C5[,3:15])

set.seed(4321)
c_ann_model5 <- nnet(Rank ~  ., data = Train_C5[,3:15], size = 1)

# Apply model to make prediction on Testing set
log_pred_5 <-predict(c_log_model5, Test_C5[,-15], type = "class")
tree_pred_5 <-predict(c_tree_model5, Test_C5[,-15], type = "class")
ann_pred_5 <- predict(c_ann_model5, Test_C5[,-15], type = "class")

# modify ann to factor
ann_pred_5 <- factor(ann_pred_5, levels = c_ann_model5$lev)
str(ann_pred_5)

# Model performance (Displays confusion matrix and statistics)
log_results_5 <- confusionMatrix(log_pred_5, Test_C5$Rank)
tree_results_5 <- confusionMatrix(tree_pred_5, Test_C5$Rank)
ann_results_5 <- confusionMatrix(ann_pred_5, Test_C5$Rank)

log_results_5
tree_results_5
ann_results_5


# Upgraded multiclass Classification
set.seed(4321)
# create a single 75/25% split of the c_model data:
trainIndex <- createDataPartition(c5$Rank, p = .75, list = FALSE, times = 1)

Train_C5 <- c5[ trainIndex,]
Test_C5  <- c5[-trainIndex,]


# SMOTE
set.seed(4321)
over_5 <- SMOTE(Rank ~., Train_C5, perc.over = 200,perc.under = 420, k = 5)

# Check target variable's distribution
table(over_5$Rank)

set.seed(4321)
# Build Training model
upgraded_c_log_model5 <- train(Rank ~  ., data = over_5[,3:15],
                               method = 'multinom',
                               trControl = fitControl)

set.seed(4321)
upgraded_c_tree_model5 <- train(Rank ~ ., data = over_5[,3:15],
                                method = "ctree",
                                trControl = fitControl)

# decay is the weight decay, and there are three tuning values. size is the number of hidden units.
nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))
set.seed(4321)
# ANN model
upgraded_c_ann_model5 <- train(Rank ~  ., data = over_5[,3:15],
                               method = "nnet",
                               preProcess = c('center', 'scale'),
                               tuneGrid = nnetGrid,
                               trControl = fitControl)



# Apply model to make prediction on Testing set
upgraded_log_pred_5 <-predict(upgraded_c_log_model5, Test_C5[,-15])
upgraded_tree_pred_5 <-predict(upgraded_c_tree_model5, Test_C5[,-15])
upgraded_ann_pred_5 <-predict(upgraded_c_ann_model5, Test_C5[,-15]) 

# Model performance (Displays confusion matrix and statistics)
upgraded_log_results_5 <- confusionMatrix(upgraded_log_pred_5, Test_C5$Rank)
upgraded_tree_results_5 <- confusionMatrix(upgraded_tree_pred_5, Test_C5$Rank)
upgraded_ann_results_5 <- confusionMatrix(upgraded_ann_pred_5, Test_C5$Rank)

# See updated results
log_results_5
upgraded_log_results_5

tree_results_5
upgraded_tree_results_5

ann_results_5
upgraded_ann_results_5


#############################################
'Regression Tasks'
############################################


# Target variable is "Pts Won"
r_data <- five[,c(2, 4, 12, 13, 14, 15, 16, 20,21,  22, 23, 25, 26 , 36, 7)]
colnames(r_data)[15] <- c("Points_Won")

# create a single 75%/25% split of the NBA data:
trainIndex <- createDataPartition(r_data$Points_Won, p = .75, list = FALSE, times = 1)

Train_r <- r_data[ trainIndex,]
Test_r  <- r_data[-trainIndex,]

set.seed(4321)
# create MLR model
r_mlr_model <- lm(Points_Won ~ WS + W, data = Train_r)

set.seed(4321)
# Decision Tree model
r_tree_model <- rpart(Points_Won ~., data = Train_r[,3:15])

set.seed(4321)
# ANN model
# You need to add ' linout = TRUE' to make it a regression model or else you'll only get 1 for an output
r_ann_model <- nnet(Points_Won ~ ., data = Train_r[,3:15], size = 65, linout= TRUE)


# Creating a dataframe with all our results
reg_results <- NULL
reg_results$Year <- Test_r$Year
reg_results$Player <- Test_r$Player
reg_results$pred_mlr <- predict(r_mlr_model, Test_r[-15])
reg_results$pred_tree <- predict(r_tree_model, Test_r[, -15])
reg_results$pred_ann <- predict(r_ann_model, Test_r[, -15])
reg_results <-as.data.frame(reg_results)
reg_results$Actual <- Test_r$Points_Won

reg_results

# Round data frame
reg_results$pred_mlr <- round(reg_results$pred_mlr, 0)
reg_results$pred_tree <- round(reg_results$pred_tree, 0)
reg_results$pred_ann <- round(reg_results$pred_ann, 0)


'          Upgraded Regression Models    ' 
set.seed(4321)
# create a single 75%/25% split of the NBA data:
trainIndex <- createDataPartition(r_data$Points_Won, p = .75, list = FALSE, times = 1)

Train_r <- r_data[ trainIndex,]
Test_r  <- r_data[-trainIndex,]

# fit control
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# Make a custom tuning grid
tuneGrid <- expand.grid(alpha = 0:1, lambda = seq(0.0001, 1,
                                                  length = 10))

set.seed(4321)
upgraded_r_mlr_model <- train(Points_Won ~., data = Train_r[,3:15],
                              method = "glmnet",
                              trControl = fitControl,
                              tuneGrid = tuneGrid)

set.seed(4321)
# Decision Tree model
upgraded_r_tree_model <- train(Points_Won ~., data = Train_r[,3:15],
                               method = "rpart",
                               trControl = fitControl)


# Make a custom tuning grid
nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.0001, to = 0.5, by = 0.1))

set.seed(4321)
# ANN model
# You need to add ' linout = 1' to make it a regression model or else you'll only get 1 for an output
upgraded_r_ann_model <- train(Points_Won ~ ., data = Train_r[,3:15],
                              method = "nnet",
                              trControl = fitControl,
                              linout = 1,
                              tuneGrid = nnetGrid,
                              preProcess = c('center', 'scale'))


# Creating a dataframe for all our upgraded results
upgraded_reg_results <- NULL
upgraded_reg_results$Year <- Test_r$Year
upgraded_reg_results$Player <- Test_r$Player
upgraded_reg_results$pred_mlr <- predict(upgraded_r_mlr_model, Test_r[-15])
upgraded_reg_results$pred_ann <- predict(r_ann_model, Test_r[, -15])
upgraded_reg_results$pred_tree <- predict(r_tree_model, Test_r[, -15])
upgraded_reg_results <-as.data.frame(upgraded_reg_results)
upgraded_reg_results$Actual <- Test_r$Points_Won


# MLR Results
MLR_Results <- data.frame(
  Basic = postResample(reg_results$pred_mlr, reg_results$Actual),
  Upgraded = postResample(upgraded_reg_results$pred_mlr, upgraded_reg_results$Actual)
)


# Trees Results
Tree_Results <- data.frame(
  Basic = postResample(reg_results$pred_tree, reg_results$Actual),
  Upgraded = postResample(upgraded_reg_results$pred_tree, upgraded_reg_results$Actual)
)


# ANN Results
ANN_Results <- data.frame(
  Basic = postResample(reg_results$pred_ann, reg_results$Actual),
  Upgraded = postResample(upgraded_reg_results$pred_ann, upgraded_reg_results$Actual)
)




###################################################################
' Predicting on 2020 Data'
###################################################################

# Create testing data set with the 2020 candiates stats
Final_Test <- test_data2020[,c("Year","Player","PTS","TRB","AST","STL", "BLK","WS","WS48" ,"Seed","W","PER", "TS.","Sum")]


final_binary <- data.frame(probs = predict(upgraded_c_ann_model2, Final_Test, type="prob"))
final_multi <- data.frame(probs = predict(upgraded_c_log_model5, Final_Test, type="prob"))
final_regression <- predict(upgraded_r_mlr_model, Final_Test)



#############################################
'Data Viz'
#############################################
reg_long <- upgraded_reg_results %>%
  pivot_longer(-c(Year, Player, Actual), names_to = "Model", values_to = "Prediction")

reg_long <- reg_long[,c (1, 2,4, 5, 3)]

# Graph to show our estimates of our upgraded regression models
p <- ggplot(reg_long, aes(x = Actual, y = Prediction, color = Model, fill = Model)) + 
  geom_point( aes(x = Actual, y = Actual))  +
  geom_line(size=1, alpha = 0.7) +
  theme_classic() + 
  xlim(0, 1250) +
  ylim(0, 1250) 

p


# Create long verison of c_model2 
c_long <- c_model %>% 
  pivot_longer(-c(Year, Player, Results, Rank), names_to = "Statistic", values_to = "Value")

# You can switch 'y=value' to x=value to switch graph look. Just personal preference 
ggplot(c_long, aes(y=Value, fill = Results, color = Results)) + 
  geom_boxplot(col=4, alpha = .55) +
  facet_wrap(~Statistic, scales = "free") +
  theme_classic()


# You can switch 'y=value' to x=value to switch graph look. Just personal preference 
ggplot(c_long, aes(y=Value, fill = Rank, color = Rank)) + 
  geom_boxplot(col=4, alpha = .55) +
  facet_wrap(~Statistic, scales = "free") +
  theme_classic()


# bar plot of the normal binary data
bar <- ggplot(c_model2, aes(x = Results, fill = Results)) + 
  geom_bar() +
  theme_classic() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
bar

# b2
bar2 <- ggplot(over_2, aes(x = Results, fill = Results)) + 
  geom_bar() +
  theme_classic() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

bar2

# b5
bar5 <- ggplot(over_5, aes(x = Rank, fill = Rank)) + 
  geom_bar() +
  theme_classic()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)

bar5

# Trees !!
library(rpart.plot)
# Binary
rpart.plot(c_tree_model2, main = "Basic Binary")
rpart.plot(upgraded_c_tree_model2$finalModel, main = "Upgraded Binary")

# Multiclass
fancyRpartPlot(c_tree_model5, cex = .75, main = "Basic Multiclass")
plot(upgraded_c_tree_model5$finalModel, cex = .75,  main = "Upgraded Multiclass")

# Regression
rpart.plot(r_tree_model, main = "Basic Regression")
rpart.plot(upgraded_r_tree_model$finalModel, main = "Upgraded Regression")




binary_predicted.data <- data.frame(
  Player = paste(Test_C2$Player, Test_C2$Year),
  probability.of.mvp = logistic_pred_2,
  mvp = Test_C2$Results
)


binary_predicted.data <- binary_predicted.data[
  order(binary_predicted.data$probability.of.mvp, decreasing= FALSE),]
binary_predicted.data$rank <- 1:nrow(binary_predicted.data)



## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
logistic_training_plot2 <- ggplot(data=binary_predicted.data, aes(x=rank, y=probability.of.mvp)) +
  geom_point(aes(color=mvp), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of winning MVP") 


logistic_training_plot2 + 
  geom_label_repel(mapping = aes(label = Player), nudge_y = .4) +
  theme_classic() +
  labs(title = "Basic Logistic Regression Predictions")


' UPGRADED BINARY LOGISTIC PLOT SORRY FOR THE LONG VARIABLE NAMES'

up_binary_predicted.data <- data.frame(
  Player = paste(Test_C2$Player, Test_C2$Year),
  probability.of.mvp = upgraded_logistic_pred_2,
  mvp = Test_C2$Results
)


up_binary_predicted.data <- up_binary_predicted.data[
  order(up_binary_predicted.data$probability.of.mvp, decreasing= FALSE),]
up_binary_predicted.data$rank <- 1:nrow(up_binary_predicted.data)



## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
up_logistic_training_plot2 <- ggplot(data = up_binary_predicted.data, aes(x = rank, y = probability.of.mvp)) +
  geom_point(aes(color=mvp), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of winning MVP") +
  labs(title = "Upgraded Logistic Regression Predictions")


up_logistic_training_plot2 + 
  geom_label_repel(mapping = aes(label = Player), nudge_y = .4) +
  theme_classic()








