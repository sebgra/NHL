library(rvest)
library(RCurl)
library(dplyr)
library(httr)
library(xml2)
library(stringr)
library(purrr)

setwd("/home/sebastien/Bureau/")

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

rm(list = ls())
##### Variables ####
team_names = c("ANA","ARI","BOS","BUF","CAR","CGY","CHI","CBJ","COL","DAL","DET","EDM","FLA","LAK","MIN","MTL","NSH","NJD","NYI","NYR","OTT","PHI","PHX","PIT","SJS","STL","TBL","TOR","VAN","VGK","WPG","WSH")

S2018 = read.table("Season_2016_2017.txt", header = T, sep = ",")

S2018 = read.table("Season_2017_2018.txt", header = T, sep = ",")

S2018 = read.table("Season_2018_2019.txt", header = T, sep = ",")

dates = S2018 %>% select(Date) %>% as.data.frame()

dates[] <- lapply(dates, gsub, pattern='-', replacement='')

dates = unique(dates)

url_list = c()

for (j in 1:dim(dates)[1]) {
  print(j)
  
  for (k in 1:length(team_names)) {
    
    url = paste("https://www.hockey-reference.com/boxscores/",dates[j,],"0",team_names[k],".html",sep="")
    url_list <- rbind(url_list,url)
    print(k)
    
  }
  
}

url_rafine <- c()

for (i in 1:length(url_list)) {
  print(i)
  
  resp <- httr::GET(url_list[i], httr::timeout(60))
  if(resp$status_code==200) {
    
    url_rafine <- append(url_rafine,url_list[i])
    
  }
  
}


html <- xml2::read_html(url_rafine[1])
txt <- rvest::html_text(rvest::html_nodes(html))

A <- url_rafine[1] %>% read_html() %>% html_nodes(xpath = '//*[@id="all_CGY_skaters"]/div[2]') %>% html_table()

resp <- httr::GET(url_address, httr::timeout(60)) 
if(resp$status_code==200) {
  html <- xml2::read_html(resp)
  txt <- rvest::html_text(rvest::html_nodes(html)) # or similar
  # save the results somewhere or do your operations..
}

tes <-readRDS("url_S2017.rds")

setwd("C:/Users/sebas/Desktop/Projet_NHL/NHL_stats/2018_2019/")

for (i in 1:length(url_rafine)) {
  print(i)
  

url <- url_rafine[i]

date <- numextract(url)

pagina <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)

pagina %>%
  html_nodes("h2") %>% .[[3]] %>%
  html_text() -> x_title

pagina %>% html_nodes("table") %>%.[[3]] %>%  html_table(fill=T) -> x



write.table(x, file = as.character(paste(x_title,"player_stats_",date,".csv")), sep = ';', col.names = TRUE) #


pagina %>%
  html_nodes("h2") %>% .[[4]] %>%
  html_text() -> xx_title

pagina %>% html_nodes("table") %>%.[[4]] %>%  html_table(fill=T) -> xx



write.table(xx, file = as.character(paste(x_title,"goalie_stats_",date,".csv")), sep = ';', col.names = TRUE) #

pagina %>%
  html_nodes("h2") %>% .[[5]] %>%
  html_text() -> y_title

pagina %>% html_nodes("table") %>%.[[5]] %>%  html_table(fill=T) -> y

write.table(y, file = as.character(paste(y_title,"player_stats_",date,".csv")), sep = ';', col.names = TRUE) #

pagina %>%
  html_nodes("h2") %>% .[[6]] %>%
  html_text() -> yy_title

pagina %>% html_nodes("table") %>%.[[6]] %>%  html_table(fill=T) -> yy



write.table(yy, file = as.character(paste(y_title,"goalie_stats_",date,".csv")), sep = ';', col.names = TRUE) #


}


filenames <- list.files("C:/Users/sebas/Desktop/Projet_NHL/NHL_stats/2016_2017/", pattern="*.csv", full.names=TRUE)

ldf <- lapply(filenames, read.csv, header=FALSE, sep=";")

for (i in 1:length(ldf)) {
  
  ldf[[i]] <- ldf[[i]][,-1]
  
  names(ldf[[i]]) <- as.matrix(ldf[[i]][2, ])
  ldf[[i]] <- ldf[[i]][-c(1,2), ]
  ldf[[i]] <- ldf[[i]][,-1]

}

noms <- filenames

for (i in 1:length(filenames)) {
  
  noms[i] <- str_replace(str_split_fixed(noms[i],"/",8)[8]," .csv","")
  
}

names(ldf) <- noms

grep("player.*Anaheim|Anaheim.*player", noms) 


test <- ldf[grep("player.*Anaheim|Anaheim.*player", noms)]

df <- do.call(rbind.data.frame, test)

teams <- c("Anaheim", "Arizona","Boston","Buffalo","Calgary","Carolina","Chicago","Colorado","Colombus","Dallas","Detroit","Edmonton","Florida","Los Angeles","Minnesota","Montreal","Nashville","New Jersey","New York Islanders","New York Rangers","Ottawa","Philadelphia","Pittsburgh","San Jose","St. Louis","Tampa Bay","Toronto","Vancouver","Vegas","Washington","Winnipeg")

setwd("C:/Users/sebas/Desktop/Projet_NHL/NHL_stats/2016_2017/Full_season/")

for (i in 1:length(teams)) {
  
  condition_player <- paste0("player.*",teams[i],"|",teams[i],".*player")
  condition_goalie <- paste0("goalie.*",teams[i],"|",teams[i],".*goalie")
  
  df_player <-  ldf[grep(condition_player, noms)]
  df_goalie <- ldf[grep(condition_goalie, noms)]
  
  df_player <- do.call(rbind.data.frame, df_player)
  df_goalie <- do.call(rbind.data.frame, df_goalie)
  
  # colnames(df_player) <- c(" ", colnames(df_player))
  # colnames(df_goalie) <- c(" ", colnames(df_goalie))
  
  write.table(df_goalie,file = as.character(paste0(teams[i],"_goalie_season_stats_2016_2017.csv")), sep = ';', col.names = TRUE, row.names = F)
  write.table(df_player,file = as.character(paste0(teams[i],"_players_season_stats_2016_2017.csv")), sep = ';', col.names = TRUE, row.names = F)
  
  
}

SJ <- read.csv(file = "San Jose_goalie_season_stats_2016_2017.csv", header = T, sep = ";")

DS <- read.csv(file = "Dallas_goalie_season_stats_2016_2017.csv", header = T, sep = ";")



# https://www.hockey-reference.com/players/d/dowdni01/gamelog/2019

# Base : https://www.hockey-reference.com/players/d/ --> d exemple fait reference au nom du joueur

# Player tag : dowdni01 --> 4 premieres lettres du nom + 2 premieres lettres du prenom + 01 ou 02 (voir juste premiere lettre ou 3 lettres si De La Rose prenom dans certains cas)
 # autres cas de figure : DeAngello --> deang (5lettres)
# Suite : gamelog

# Season : 2019

