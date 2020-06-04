library(dplyr)
library(rvest)
library(RCurl)
library(dplyr)
library(httr)
library(xml2)
library(stringr)
library(purrr)
library(lubridate)

setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/2018_2019/')

players <- read.csv('Anaheim Ducks 201810030 .csv',sep =';', dec = ".", header = TRUE, skip = 1)

players <- players[,-c(1,2)]  

goal <- read.csv('Anaheim Ducks goalie_stats_ 201811060 .csv', sep = ";", dec = ".", header = TRUE,skip = 1)

goal <- goal[,-c(1,2)]


scores <- read.table('/home/sebastien//Bureau/Projet_NHL/Season_2018_2019.txt', sep = ",", header = TRUE)


Date <- vector()
Team <- vector()
Result <- vector()

for (i in 1:dim(scores)[1]) {
  
  #print(scores$Date[i])
  
  Date <- append(Date,as.character(scores$Date[i]))
  Team <- append(Team,as.character(scores$Home[i]))
  
  if (scores$G.1[i]< scores$G[i]) {
    
    Result <- append(Result,'Lost')
    
  }
  
  else if(scores$G.1[i] == scores$G[i]){
    
    Result <- append(Result,'Draw')
  }
  
  else{
    
    Result <- append(Result,'Win')
    
  }
  
  
}

test <- data.frame(Date,Team,Result)


df <- read.csv('/home/sebastien//Bureau/Projet_NHL/Players_2018.csv', header = TRUE, sep = ',') # Season 2017_2018

manip <- df$Player %>% as.character()

test_tag <-manip[1]

multiple_test_tag <- strsplit(test_tag,"_")[1]
final_tag <- multiple_test_tag[[1]][2]


player_tags_2018 <- vector()

player_names_2018 <- vector()

for (i in 1:length(manip)) {
  
  current_player <- manip[i]
  
  multiple_test_tag <- strsplit(current_player,"_")[1]
  final_tag <- multiple_test_tag[[1]][2]
  
  final_tag_name <- multiple_test_tag[[1]][1]
  
  player_tags_2018 <- append(player_tags_2018,final_tag)
  player_names_2018 <- append(player_names_2018,final_tag_name)
}

player_first_letter_2018 <- vector()

for (i in 1:length(player_tags_2018)) {
  
  current_player_first_lettre <- player_tags_2018[i]
  
  # print(current_player_first_lettre)
  
  player_first_letter_2018 <- append(player_first_letter_2018,substr(current_player_first_lettre,1,1))
  
  
  
}


# https://www.hockey-reference.com/players/d/dowdni01/gamelog/2019

# Base : https://www.hockey-reference.com/players/d/ --> d exemple fait reference au nom du joueur

# Player tag : dowdni01 --> 4 premieres lettres du nom + 2 premieres lettres du prenom + 01 ou 02 (voir juste premiere lettre ou 3 lettres si De La Rose prenom dans certains cas)
# autres cas de figure : DeAngello --> deang (5lettres)
# Suite : gamelog

# Season : 2019

url_list = c()

for (k in 1:length(player_tags_2018)) {
  
  url = paste("https://www.hockey-reference.com/players/",player_first_letter_2018[k],"/",player_tags_2018[k],"/gamelog/2018",sep="")
  url_list <- rbind(url_list,url)
  print(k)
  
}

############################### Capture Table #############################################

setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/2017_2018/')

for (i in 1:length(url_list)) {
  


pagina <- read_html(url_list[i], as.data.frame=T, stringsAsFactors = TRUE)



try(pagina %>%
  html_nodes("h2") %>% .[[1]] %>%
  html_text() -> x_title)

try(pagina %>% html_nodes("table") %>%.[[1]] %>%  html_table(fill=T) -> x)

try(write.table(x, file = as.character(paste(player_names_2018[i],"player_stats_","2017_2018",".csv")), sep = ';', col.names = TRUE))

}
  ################################################################################################





#########################################################################################
#########################################################################################

df <- read.csv('/home/sebastien//Bureau/Projet_NHL/Players_2017.csv', header = TRUE, sep = ',') # Season 2016_2017

manip <- df$Player %>% as.character()

test_tag <-manip[1]

multiple_test_tag <- strsplit(test_tag,"_")[1]
final_tag <- multiple_test_tag[[1]][2]


player_tags_2017 <- vector()

player_names_2017 <- vector()

for (i in 1:length(manip)) {
  
  current_player <- manip[i]
  
  multiple_test_tag <- strsplit(current_player,"_")[1]
  final_tag <- multiple_test_tag[[1]][2]
  
  final_tag_name <- multiple_test_tag[[1]][1]
  
  player_tags_2017 <- append(player_tags_2017,final_tag)
  player_names_2017 <- append(player_names_2017,final_tag_name)
}

player_first_letter_2017 <- vector()

for (i in 1:length(player_tags_2017)) {
  
  current_player_first_lettre <- player_tags_2017[i]
  
  # print(current_player_first_lettre)
  
  player_first_letter_2017 <- append(player_first_letter_2017,substr(current_player_first_lettre,1,1))
  
  
  
}

url_list = c()

for (k in 1:length(player_tags_2017)) {
  
  url = paste("https://www.hockey-reference.com/players/",player_first_letter_2017[k],"/",player_tags_2017[k],"/gamelog/2017",sep="")
  url_list <- rbind(url_list,url)
  print(k)
  
}

############################### Capture Table #############################################

setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/2016_2017/')

for (i in 1:length(url_list)) {
  
  
  
  pagina <- read_html(url_list[i], as.data.frame=T, stringsAsFactors = TRUE)
  
  
  
  try(pagina %>%
        html_nodes("h2") %>% .[[1]] %>%
        html_text() -> x_title)
  
  try(pagina %>% html_nodes("table") %>%.[[1]] %>%  html_table(fill=T) -> x)
  
  try(write.table(x, file = as.character(paste(player_names_2017[i],"player_stats_","2016_2017",".csv")), sep = ';', col.names = TRUE))
  
}
  ################################################################################################


#########################################################################################
#########################################################################################

df <- read.csv('/home/sebastien//Bureau/Projet_NHL/Players_2019.csv', header = TRUE, sep = ',') # Season 2018_2019

manip <- df$Player %>% as.character()

test_tag <-manip[1]

multiple_test_tag <- strsplit(test_tag,"_")[1]
final_tag <- multiple_test_tag[[1]][2]


player_tags_2019 <- vector()

player_names_2019 <- vector()

for (i in 1:length(manip)) {
  
  current_player <- manip[i]
  
  multiple_test_tag <- strsplit(current_player,"_")[1]
  final_tag <- multiple_test_tag[[1]][2]
  
  final_tag_name <- multiple_test_tag[[1]][1]
  
  player_tags_2019 <- append(player_tags_2019,final_tag)
  player_names_2019 <- append(player_names_2019,final_tag_name)
}

player_first_letter_2019 <- vector()

for (i in 1:length(player_tags_2019)) {
  
  current_player_first_lettre <- player_tags_2019[i]
  
  # print(current_player_first_lettre)
  
  player_first_letter_2019 <- append(player_first_letter_2019,substr(current_player_first_lettre,1,1))
  
  
  
}

url_list = c()

for (k in 1:length(player_tags_2019)) {
  
  url = paste("https://www.hockey-reference.com/players/",player_first_letter_2019[k],"/",player_tags_2019[k],"/gamelog/2019",sep="")
  url_list <- rbind(url_list,url)
  print(k)
  
}

############################### Capture Table #############################################

setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/2018_2019/')

for (i in 1:length(url_list)) {
  
  
  
  pagina <- read_html(url_list[i], as.data.frame=T, stringsAsFactors = TRUE)
  
  
  
  try(pagina %>%
        html_nodes("h2") %>% .[[1]] %>%
        html_text() -> x_title)
  
  try(pagina %>% html_nodes("table") %>%.[[1]] %>%  html_table(fill=T) -> x)
  
  try(write.table(x, file = as.character(paste(player_names_2018[i],"player_stats_","2018_2019",".csv")), sep = ';', col.names = TRUE))
  
}
################################################################################################


df <- read.csv('/home/sebastien//Bureau/Projet_NHL/Goalies_2017.csv', header = TRUE, sep = ',') # Season 2018_2019


manip <- df$Player %>% as.character()

test_tag <-manip[1]

multiple_test_tag <- strsplit(test_tag,"_")[1]
final_tag <- multiple_test_tag[[1]][2]


player_tags_2017 <- vector()

player_names_2017 <- vector()

for (i in 1:length(manip)) {
  
  current_player <- manip[i]
  
  multiple_test_tag <- strsplit(current_player,"_")[1]
  final_tag <- multiple_test_tag[[1]][2]
  
  final_tag_name <- multiple_test_tag[[1]][1]
  
  player_tags_2017 <- append(player_tags_2017,final_tag)
  player_names_2017 <- append(player_names_2017,final_tag_name)
}

player_first_letter_2017 <- vector()

for (i in 1:length(player_tags_2017)) {
  
  current_player_first_lettre <- player_tags_2017[i]
  
  # print(current_player_first_lettre)
  
  player_first_letter_2017 <- append(player_first_letter_2017,substr(current_player_first_lettre,1,1))
  
  
  
}



url_list = c()

for (k in 1:length(player_tags_2017)) {
  
  url = paste("https://www.hockey-reference.com/players/",player_first_letter_2017[k],"/",player_tags_2017[k],"/gamelog/2017",sep="")
  url_list <- rbind(url_list,url)
  print(k)
  
}

############################### Capture Table #############################################

setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/Goalies_2016_2017/')

for (i in 1:length(url_list)) {
  
  print(url_list[i])
  
  
  
  pagina <- read_html(url_list[i], as.data.frame=T, stringsAsFactors = TRUE)
  
  
  
  try(pagina %>%
        html_nodes("h2") %>% .[[1]] %>%
        html_text() -> x_title)
  
  try(pagina %>% html_nodes("table") %>%.[[1]] %>%  html_table(fill=T) -> x)
  
  try(write.table(x, file = as.character(paste(player_names_2017[i],"player_stats_","2016_2017",".csv")), sep = ';', col.names = TRUE))
  
}
################################################################################################


################################################################################################


df <- read.csv('/home/sebastien//Bureau/Projet_NHL/Goalies_2018.csv', header = TRUE, sep = ',') # Season 2018_2019


manip <- df$Player %>% as.character()

test_tag <-manip[1]

multiple_test_tag <- strsplit(test_tag,"_")[1]
final_tag <- multiple_test_tag[[1]][2]


player_tags_2018 <- vector()

player_names_2018 <- vector()

for (i in 1:length(manip)) {
  
  current_player <- manip[i]
  
  multiple_test_tag <- strsplit(current_player,"_")[1]
  final_tag <- multiple_test_tag[[1]][2]
  
  final_tag_name <- multiple_test_tag[[1]][1]
  
  player_tags_2018 <- append(player_tags_2018,final_tag)
  player_names_2018 <- append(player_names_2018,final_tag_name)
}

player_first_letter_2018 <- vector()

for (i in 1:length(player_tags_2018)) {
  
  current_player_first_lettre <- player_tags_2018[i]
  
  # print(current_player_first_lettre)
  
  player_first_letter_2018 <- append(player_first_letter_2018,substr(current_player_first_lettre,1,1))
  
  
  
}



url_list = c()

for (k in 1:length(player_tags_2018)) {
  
  url = paste("https://www.hockey-reference.com/players/",player_first_letter_2018[k],"/",player_tags_2018[k],"/gamelog/2018",sep="")
  url_list <- rbind(url_list,url)
  print(k)
  
}

############################### Capture Table #############################################

setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/Goalies_2017_2018/')

for (i in 1:length(url_list)) {
  
  print(url_list[i])
  
  
  
  pagina <- read_html(url_list[i], as.data.frame=T, stringsAsFactors = TRUE)
  
  
  
  try(pagina %>%
        html_nodes("h2") %>% .[[1]] %>%
        html_text() -> x_title)
  
  try(pagina %>% html_nodes("table") %>%.[[1]] %>%  html_table(fill=T) -> x)
  
  try(write.table(x, file = as.character(paste(player_names_2017[i],"player_stats_","2017_2018",".csv")), sep = ';', col.names = TRUE))
  
}
################################################################################################



################################################################################################


df <- read.csv('/home/sebastien//Bureau/Projet_NHL/Goalies_2019.csv', header = TRUE, sep = ',') # Season 2018_2019


manip <- df$Player %>% as.character()

test_tag <-manip[1]

multiple_test_tag <- strsplit(test_tag,"_")[1]
final_tag <- multiple_test_tag[[1]][2]


player_tags_2019 <- vector()

player_names_2019 <- vector()

for (i in 1:length(manip)) {
  
  current_player <- manip[i]
  
  multiple_test_tag <- strsplit(current_player,"_")[1]
  final_tag <- multiple_test_tag[[1]][2]
  
  final_tag_name <- multiple_test_tag[[1]][1]
  
  player_tags_2019 <- append(player_tags_2019,final_tag)
  player_names_2019 <- append(player_names_2019,final_tag_name)
}

player_first_letter_2019 <- vector()

for (i in 1:length(player_tags_2019)) {
  
  current_player_first_lettre <- player_tags_2019[i]
  
  # print(current_player_first_lettre)
  
  player_first_letter_2019 <- append(player_first_letter_2019,substr(current_player_first_lettre,1,1))
  
  
  
}



url_list = c()

for (k in 1:length(player_tags_2019)) {
  
  url = paste("https://www.hockey-reference.com/players/",player_first_letter_2019[k],"/",player_tags_2019[k],"/gamelog/2017",sep="")
  url_list <- rbind(url_list,url)
  print(k)
  
}

############################### Capture Table #############################################

setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/Goalies_2018_2019/')

for (i in 1:length(url_list)) {
  
  print(url_list[i])
  
  
  
  pagina <- read_html(url_list[i], as.data.frame=T, stringsAsFactors = TRUE)
  
  
  
  try(pagina %>%
        html_nodes("h2") %>% .[[1]] %>%
        html_text() -> x_title)
  
  try(pagina %>% html_nodes("table") %>%.[[1]] %>%  html_table(fill=T) -> x)
  
  try(write.table(x, file = as.character(paste(player_names_2017[i],"player_stats_","2018_2019",".csv")), sep = ';', col.names = TRUE))
  
}
################################################################################################


############################### Formating Table #############################################

setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/2016_2017/')

Age_format <- function(age){
  
  cut <- age %>% strsplit(split = "-") %>% unlist()
  
  formated_age <- (cut[1] %>% as.numeric()) *365 + cut[2] %>% as.numeric()
  return(formated_age)
  
}


test <- read.csv("Aaron Ness player_stats_ 2016_2017 .csv", header = TRUE, sep = ";", skip = 1,stringsAsFactors=FALSE)

test<-test[!(test$Rk=="Rk"),]

test <- test[,-1]

colnames(test) <- c("Rank","Date","Season_Game","Age","Team","Visitor","Opp","Result","Goals","Assists","PTS","+/-","PIM","EVG","PPG","SHG","GWG","EVA","PPA","SHA","SOG","S%","SHFT","TOI","HIT","BLK","FOW","FOL","FO")


test$Rank <- as.numeric(test$Rank)
test$Date <- as.Date(test$Date)
test$Season_Game <- as.numeric(test$Season_Game)
test$Goals <- as.numeric(test$Goals)
test$Assists <- as.numeric(test$Assists)
test$PTS <- as.numeric(test$PTS)
test[,12] <- as.numeric(test[,12])
test$PIM <- as.numeric(test$PIM)

test$EVG <- as.numeric(test$EVG)
test$PPG <- as.numeric(test$PPG)
test$SHG <- as.numeric(test$SHG)
test$GWG <- as.numeric(test$GWG)
test$EVA <- as.numeric(test$EVA)
test$SHA <- as.numeric(test$SHA)

test$SOG <- as.numeric(test$SOG)

test$`S%` <- as.numeric(test$`S%`)
test$SHFT <- as.numeric(test$SHFT)
test$TOI <- as.numeric(test$TOI)
test$HIT <- as.numeric(test$HIT)

test$BLK <- as.numeric(test$BLK)
test$FOW <- as.numeric(test$FOW)
test$FOL <- as.numeric(test$FOL)

test$FO <- as.numeric(test$FO)

format_player <- function(df){
  
  df<-df[!(df$Rk=="Rk"),]
  
  df <- df[,-1]
  
  colnames(df) <- c("Rank","Date","Season_Game","Age","Team","Visitor","Opp","Result","Goals","Assists","PTS","+/-","PIM","EVG","PPG","SHG","GWG","EVA","PPA","SHA","SOG","S%","SHFT","TOI","HIT","BLK","FOW","FOL","FO")
  
  
  df$Rank <- as.numeric(df$Rank)
  df$Date <- as.Date(df$Date)
  df$Season_Game <- as.numeric(df$Season_Game)
  df$Goals <- as.numeric(df$Goals)
  df$Assists <- as.numeric(df$Assists)
  df$PTS <- as.numeric(df$PTS)
  df[,12] <- as.numeric(df[,12])
  df$PIM <- as.numeric(df$PIM)
  
  df$EVG <- as.numeric(df$EVG)
  df$PPG <- as.numeric(df$PPG)
  df$SHG <- as.numeric(df$SHG)
  df$GWG <- as.numeric(df$GWG)
  df$EVA <- as.numeric(df$EVA)
  df$SHA <- as.numeric(df$SHA)
  
  df$SOG <- as.numeric(df$SOG)
  
  df$`S%` <- as.numeric(df$`S%`)
  df$SHFT <- as.numeric(df$SHFT)
  df$TOI <- df$TOI%>%lubridate::ms() %>%period_to_seconds()
  df$HIT <- as.numeric(df$HIT)
  
  df$BLK <- as.numeric(df$BLK)
  df$FOW <- as.numeric(df$FOW)
  df$FOL <- as.numeric(df$FOL)
  
  df$FO <- as.numeric(df$FO)
  
  for (i in 1:dim(df)[1]) {
    df$Age[i] <- Age_format(df$Age[i])
    
  }
  
  return(df)
  
}

goalie <- read.csv("Aaron Dell player_stats_ 2016_2017 .csv", sep = ';', dec = ".", skip = 1, stringsAsFactors = FALSE)


format_goalie <- function(df){
  
  df <- df[,-c(1,9)]
  colnames(df) <- c("Rk","Date","Season_Game","Age","Tm","Visitor","Opp","Result","GA","SA","SV","SV.","SO","PIM","TOI")
  df<-df[!(df$Rk=="Rk"),]
  
  df$Date <- as.Date(df$Date)
  
  df$TOI <- df$TOI%>%lubridate::ms() %>%period_to_seconds()
  
  for (i in 1:dim(df)[1]) {
    df$Age[i] <- Age_format(df$Age[i])
    
  }
  return(df)
  
}

X <- format_goalie(goalie)


########################################################
########################################################


setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/2016_2017/')

files <- list.files()

for (i in 1:length(files)) {
  
  df <- read.csv(files[i], header = TRUE, sep = ";", skip = 1,stringsAsFactors=FALSE)
  print('ok')
  
  df <- format_player(df)
  print('ok')
  
  write.csv(df, file = sprintf(files[i]))
  
}


setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/2017_2018/')

files <- list.files()

for (i in 1:length(files)) {
  
  df <- read.csv(files[i], header = TRUE, sep = ";", skip = 1,stringsAsFactors=FALSE)
  print('ok')
  
  df <- format_player(df)
  print('ok')
  
  write.csv(df, file = sprintf(files[i]))
  
}


setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/2018_2019/')

files <- list.files()

for (i in 1:length(files)) {
  
  df <- read.csv(files[i], header = TRUE, sep = ";", skip = 1,stringsAsFactors=FALSE)
  print('ok')
  
  df <- format_player(df)
  print('ok')
  
  write.csv(df, file = sprintf(files[i]))
  
}



setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/Goalies_2016_2017/')

files <- list.files()

for (i in 1:length(files)) {
  
  df <- read.csv(files[i], header = TRUE, sep = ";", skip = 1,stringsAsFactors=FALSE)
  print('ok')
  
  df <- format_goalie(df)
  print('ok')
  
  write.csv(df, file = sprintf(files[i]))
  
}



setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/Goalies_2017_2018/')

files <- list.files()

for (i in 1:length(files)) {
  
  df <- read.csv(files[i], header = TRUE, sep = ";", skip = 1,stringsAsFactors=FALSE)
  print('ok')
  
  df <- format_goalie(df)
  print('ok')
  
  write.csv(df, file = sprintf(files[i]))
  
}


#################################################################################################
#################################################################################################
                                            # Stacking Data #
#################################################################################################
#################################################################################################


setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/2016_2017/')


files <- list.files()

base <- read.csv(files[1], sep =",", header = T)

for (i in 2:length(files)) {
  
  df <- read.csv(files[i],sep=",",header =T)
  base <- rbind(base,df)
  rm(df)  
  
}

setwd('/home/sebastien/Bureau/Projet_NHL/NHL_stats/Individual_stats/')

write.csv(base,"stacked_players_2016_2017.csv")



#################################################################################################
#################################################################################################
                                        # PCA Analysis #
#################################################################################################
#################################################################################################

library(dplyr)
library(stringr)
base <- read.csv('stacked_players_2016_2017.csv', header = TRUE,sep = ",")
data_set_PCA <- base %>% select(Season_Game,Age,Goals,Assists,PTS,X,PIM,EVG,PPG,SHG,GWG,EVA,PPA,SHA,SOG,,SHFT,TOI,HIT,BLK,FOW,FOL)

res_pca <- PCA(data_set_PCA, scale.unit = TRUE, ncp = 5, graph = TRUE)

base_classif <- base %>% select(Season_Game,Age,Goals,Assists,PTS,X,PIM,EVG,PPG,SHG,GWG,EVA,PPA,SHA,SOG,,SHFT,TOI,HIT,BLK,FOW,FOL, Result)
base_classif$Result <- as.character(base_classif$Result)

pattern <-c('L-SO')

replacement <-c('L')

base_classif$Result <- str_replace_all(base_classif$Result,pattern,replacement)


pattern <-c('L-OT')

replacement <-c('L')

base_classif$Result <- str_replace_all(base_classif$Result,pattern,replacement)

pattern <-c('W-SO')

replacement <-c('W')

base_classif$Result <- str_replace_all(base_classif$Result,pattern,replacement)


pattern <-c('W-OT')

replacement <-c('W')

base_classif$Result <- str_replace_all(base_classif$Result,pattern,replacement)
library('e1071')



# Random sample indexes
train_index <- sample(1:nrow(base_classif), 0.8 * nrow(base_classif))
test_index <- setdiff(1:nrow(base_classif), train_index)

# Build X_train, y_train, X_test, y_test
X_train <- base_classif[train_index,] %>% select(-Result)
y_train <- base_classif[train_index,] %>% select(Result)

X_test <- base_classif[test_index,] %>% select(-Result)
y_test <- base_classif[test_index,] %>% select(Result)

X_train <- base_classif[train_index,]
  
X_test <- base_classif[test_index,]

X_train$Result <- X_train$Result %>% as.factor() %>% as.numeric()

set.seed(1000) #pour obtenir à chaque lancement le même résultat
obj <- tune(svm, Result ~ ., data = X_train, ranges =
              list(kernel=c('linear','polynomial','radial', 'sigmoid'), cost =
                     c(0.1,0.5,1.0,2.0,10)), tunecontrol = tune.control(sampling="cross"))
