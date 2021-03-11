library(stringr)
library(dplyr)
library(magrittr)
library(readr)
library(httr)
library(jsonlite)
library(RSQLite)
library(tidyr)
library(data.table)
library(purrr)

csvs <- c("iateyourpie - game stats on Twitch in 2015 - SullyGnome.csv",
          "iateyourpie - game stats on Twitch in 2016 - SullyGnome.csv",
          "iateyourpie - game stats on Twitch in 2017 - SullyGnome.csv",
          "iateyourpie - game stats on Twitch in 2018 - SullyGnome.csv",
          "iateyourpie - game stats on Twitch in 2019 - SullyGnome.csv",
          "iateyourpie - game stats on Twitch in 2020 - SullyGnome.csv",
          "spikevegeta - game stats on Twitch in 2015 - SullyGnome.csv",
          "spikevegeta - game stats on Twitch in 2016 - SullyGnome.csv",
          "spikevegeta - game stats on Twitch in 2017 - SullyGnome.csv",
          "spikevegeta - game stats on Twitch in 2018 - SullyGnome.csv",
          "spikevegeta - game stats on Twitch in 2019 - SullyGnome.csv",
          "spikevegeta - game stats on Twitch in 2020 - SullyGnome.csv")

tablenames <- c("pie2015","pie2016","pie2017","pie2018","pie2019","pie2020",
                "spike2015","spike2016","spike2017",
                "spike2018","spike2019","spike2020")

#dbname should be a database.
#tablename is a string
#filename is a string leading to a csv file from sullygnome.

#This is unfortunately EXTREMELY ugly since the way the URLs handle "'s" is
#inconsistent. It's pretty much a 50/50 whether "Mario's" will turn into
#"marios" or "mario-s" in the URL. &s and +s are also a nightmare.
#This could absolutely be cleaned up a bit but that's a non-trivial matter
#and a project for another day.
construct_yearly_time <- function(dbname,tablename,file){
  read_csv(file) %>%
    select(Game,`Stream time`) %>%
    `colnames<-` (c("game","minutes")) %>%
    mutate(game = str_extract(game,"[^|]+"),
           game = str_replace(game,"é","e"), #Pokemon plz
           game = sapply(game,pokemon_checker),
           url = str_to_lower(game),
           url = str_replace_all(url,"&","and"),
           url = str_replace_all(url,"\\+","plus"),
           url = str_remove_all(url,"[^([:alnum:]-?|[:space:]|\'|$)]"),
           url = str_replace_all(url,"([:space:]+|\')","-"),
           url = str_replace(url,"chip-s","chips"), #Why is Chip's Challenge 2's URL different
           url = str_replace(url,"freddy-s","freddys"), #Why is this different too
           url = str_replace(url,"igi-s-man","igis-man"), #Luigi's Mansion
           url = str_replace(url,"known-s-bat","knowns-bat"), #PUBG
           url = str_replace(url,"let-s-go-pika","lets-go-pika"),
           url = str_replace(url,"pooh-s-home","poohs-home"),
           url = str_replace(url,"ter-s-aren","ters-aren"),
           url = str_replace(url,"shi-s-wool","shis-wool"),
           url = str_replace(url,"shi-s-saf","shis-saf"),
           url = str_replace(url,"o-hare","ohare"),
           url = str_replace(url,"4-it-s-abo","4-its-abo"),
           url = str_replace(url,"evil-director-s-cut","evil-directors-cut"),
           url = str_replace(url,"link-s-awakening","links-awakening"), #Whywhywhy. 
           url = str_replace(url,"links-awakening-dx","link-s-awakening-dx"),
           url = str_replace(url,"duelists-of-the-roses","duelists-of-the-roses-fa82a70b-8784-4e43-babc-9ba06b3ba75d"), #I don't even know
           url = str_replace(url,"--","-"),
           url = str_replace(url,"ing-harmony","ing-harmony--1"), #Danganronpa 3. I really don't understand.
           url = str_replace(url,"fate-of-atlantis","fate-of-atlantis--1"), #Still don't understand.
           url = str_replace(url,"mike-tyson-s-punch-out","punch-out--2"), #sigh
           url = str_replace(url,"hd-15","hd-1-dot-5"),
           url = str_replace(url,"25-remix","2-dot-5-remix"),
           url = str_replace(url,"mario-and-luigi","mario-luigi"), #Whyyyyyyyyyyy
           url = str_replace(url,"mario-luigi-superstar-saga-plus","mario-and-luigi-superstar-saga-plus"),
           url = str_replace(url,"mario-luigi-paper-jam","mario-and-luigi-paper-jam"),
           url = str_replace(url,"man-and-bass","man-bass"),
           url = str_c("https://www.igdb.com/games/",url),
           game_id = sapply(url,search_game_id)) %>%
    filter(game_id >= 0) %>%
    select(game_id,game,minutes,url) %>%
    dbWriteTable(db,tablename,.,overwrite=T)  
}


#Checks if the game is a main-line Pokemon game with multiple versions.
#They will be written in a form such as "Pokemon Red/Blue, which are two games
#and have two separate listings in the database. This removes everything after
#the / to grab one entry.
pokemon_checker <- function(string){
  if(str_detect(string,"Pokemon ([:alnum:]|[:space:])+/")){
    return(str_extract(string,"Pokemon ([:alnum:]|[:space:])+"))
  }
  else{
    return(string)
  }
}

#Takes the name of a game as provided from the sullygnome csv, searches for it
#on igdb, and either returns the id number of the best match or -1 if no game
#match was found.
search_game_id <- function(name){
  tryCatch({POST(str_c(api_url,end_games),
                 add_headers(`Client-ID`=client_id,
                             Authorization=str_c("Bearer ",access_token)),
                 body=str_c('fields id,name; where url="',name,'";')) %>%
      use_series("content") %>%
      rawToChar() %>%
      fromJSON() %>%
      as_tibble() %>%
      magrittr::extract(1,1) %>%
      unlist() %>%
      return()
  },
  #Some "games" on Twitch aren't actual games, such as "The Game Awards" or "Just Chatting"
  error = function(e) return(-1)) 
} 


#This can 10000% be apply family'd but I couldn't get it for some reason. No biggie.
for(i in 1:length(csvs)){
  construct_yearly_time(db,tablenames[i],csvs[i])  
}


POST(str_c(api_url,end_games),
     add_headers(`Client-ID`=client_id,
                 Authorization=str_c("Bearer ",access_token)),
     body='fields id,name,url; where id = 69;') %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble() %>%
  View()

#********************************************************************************

#List of all the games to look up as well as the columns to grab 
db <- dbConnect(SQLite(),dbname="MOMAM.sqlite")

query <- dbGetQuery(db,"SELECT game_id FROM pie2015 UNION
                     SELECT game_id FROM pie2016 UNION
                     SELECT game_id FROM pie2017 UNION
                     SELECT game_id FROM pie2018 UNION
                     SELECT game_id FROM pie2019 UNION
                     SELECT game_id FROM pie2020 UNION
                     SELECT game_id FROM spike2015 UNION
                     SELECT game_id FROM spike2016 UNION
                     SELECT game_id FROM spike2017 UNION
                     SELECT game_id FROM spike2018 UNION
                     SELECT game_id FROM spike2019 UNION
                     SELECT game_id FROM spike2020") %>%
  as.character() %>%
  str_remove("c") %>%
  str_c('fields id,name,game_engines,genres,platforms,player_perspectives,themes,age_ratings,franchises,involved_companies,keywords; where id = ',.,'; limit 500;')

#End points with necessary information from the API.
api_url <- "https://api.igdb.com/v4"
end_games <- "/games"
end_categories <- "/categories"
end_genres <- "/genres"
end_age <- "/age_ratings"
end_company <- "/involved_companies"
end_engines <- "/game_engines"
end_platforms <- "/platforms"
end_franchises <- "/franchises"
end_themes <- "/themes"
end_keywords <- "/keywords"
end_perspectives <- "/player_perspectives"


client_id <- get_client_id()
access_token <- get_access_token()

#Grabs the above query and returns it at a tibble.
#Note that the result has lists within the dataframe that will need to be
#dealt with.
game_info <- POST(str_c(api_url,end_games),
     add_headers(`Client-ID`=client_id,
                 Authorization=str_c("Bearer ",access_token)),
     body=query) %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble()


lookup_id_names <- function(url=api_url,endpoint,info=game_info,clientid=client_id,accesstoken=access_token,column,database,table){
  
  #ids of the categories that will be looked up
  ids <- info %>%
    pluck(column) %>%
    unlist() %>%
    unique() %>%
    sort()
  
  #Each API query is capped at 500 results, so the queries need to be broken up into
  #batches of 500 in the event that more results need to be searched.
  for(i in 1:ceiling(length(ids)/500)){
    start <- ((i-1)*500+1)
    end <- i*500
    
    query <- ids[start:end] %>%
      unique() %>% #Multiple NA values appear when end is not a multiple of 500. This reduces that to 1.
      as_tibble() %>%
      as.character() %>%
      str_remove("c") %>% #String is currently of the form c(A, \nB, ..., NA)
      str_remove_all("\\n") %>%
      str_remove(", NA") %>%
      str_c('fields id,slug; where id = ',.,'; limit 500;')
    
    tryCatch({
      df <- POST(str_c(url,endpoint),
                 add_headers(`Client-ID`=clientid,
                             Authorization=str_c("Bearer ",accesstoken)),
                 body=query) %>%
        use_series("content") %>%
        rawToChar() %>%
        fromJSON() %>%
        as_tibble() %>%
        arrange(id)
    },error=function(e){
      message("Query returned no results.")
      stop()
    })
    
    #Only append for values 500+
    if(i > 1){
      dbWriteTable(database,table,df,append=T)
    }
    else{
      dbWriteTable(database,table,df,overwrite=T)
    }
  }
}

lookup_id_names(endpoint=end_engines,column="game_engines",database=db,table="games_engines")
lookup_id_names(endpoint=end_keywords,column = "keywords",database = db,table="games_keywords")
lookup_id_names(endpoint=end_themes,column = "themes",database = db,table="games_themes")
lookup_id_names(endpoint=end_franchises,column = "franchises",database = db,table="games_franchises")
lookup_id_names(endpoint=end_perspectives,column = "player_perspectives",database = db,table="games_perspectives")
lookup_id_names(endpoint=end_genres,column = "genres",database = db,table="games_genres")
lookup_id_names(endpoint=end_platforms,column = "platforms",database = db,table="games_platforms")
lookup_id_names(endpoint=end_age,column = "age_ratings",database = db,table="games_ratings")

#Company is done separately from the automated function since it's a mutli-step
#process. We take the involved companies, filter down to the developers, and then
#execute another search to retrieve the relevant company names.
involved_companies <- game_info$involved_companies %>%
  unlist() %>%
  unique() %>%
  sort() %>%
  as_tibble() %>%
  as.character() %>%
  str_remove("c") %>%
  str_c('fields id,company,developer; where id = ',.,' & developer = true; limit 500;') %>%
  POST(str_c(api_url,end_company),
       add_headers(`Client-ID`=client_id,
                   Authorization=str_c("Bearer ",access_token)),
       body=.) %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble()

company_names <- involved_companies %>%
  pluck("company") %>%
  unlist() %>%
  sort() %>%
  as_tibble() %>%
  as.character() %>%
  str_remove("c") %>%
  str_c('fields id,slug; where id = ',.,'; limit 500;') %>%
  POST(str_c(api_url,"/companies"),
       add_headers(`Client-ID`=client_id,
                   Authorization=str_c("Bearer ",access_token)),
       body=.) %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble()


dbWriteTable(db,"games_involved_companies",involved_companies,overwrite=T)
dbWriteTable(db,"games_companies",company_names,overwrite=T)

#And finally, age ratings. It's actually faster to just hard code these in using
#the igdb API docs https://api-docs.igdb.com/#age-rating
ratings <- tibble(name=c("Three","Seven","Twelve","Sixten","Eighteen",
                         "RP","EC","E","E10","T","M","AO"),
                  value=c(1,2,3,4,5,6,7,8,9,10,11,12))

##############################################################################

POST(str_c(api_url,end_games),
     add_headers(`Client-ID`=client_id,
                 Authorization=str_c("Bearer ",access_token)),
     body="fields *; where id = (49, 12349, 29, 329, 15, 943, 13, 403);") %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble() %>%
  View()



engine_query <- game_info$game_engines %>%
  unlist() %>%
  unique() %>%
  sort() %>%
  as_tibble() %>%
  as.character() %>%
  str_remove("c") %>%
  str_c('fields id,name; where id = ',.,'; limit 500;')


POST(str_c(api_url,end_engines),
     add_headers(`Client-ID`=client_id,
                 Authorization=str_c("Bearer ",access_token)),
     body=engine_query) %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble() %>%
  dbWriteTable(db,"games_engines",.)




#Game engines
game_info <- game_info %>%
  mutate(game_engines = as.character(game_engines)) %>%
  mutate(game_engines = case_when(game_engines=="NULL"~"",
                                  T~game_engines)) %>%
  select(game_engines) %>%
  unlist() %>%
  str_remove_all("(c\\(|\\))") %>%
  str_replace_all(":",", ") %>%
  str_replace_all(", ","-") %>%
  as.factor() %>%
  mSplit() %>%
  as_tibble() %>%
  setnames(.,names(.),paste0("engine_",names(.))) %>%
  cbind(game_info,.)


#Genres.
game_info <- game_info %>%
  mutate(genres = as.character(genres)) %>%
  mutate(genres = case_when(genres=="NULL"~"",
                            T~genres)) %>%
  select(genres) %>%
  unlist() %>%
  str_remove_all("(c\\(|\\))") %>%
  str_replace_all(":",", ") %>%
  str_replace_all(", ","-") %>%
  mSplit() %>%
  as_tibble() %>%
  setnames(.,names(.),paste0("genre_",names(.))) %>%
  cbind(game_info,.)

#Involved Companies.
game_info <- game_info %>%
  mutate(involved_companies = as.character(involved_companies)) %>%
  mutate(involved_companies = case_when(involved_companies=="NULL"~"",
                            T~involved_companies)) %>%
  select(involved_companies) %>%
  unlist() %>%
  str_remove_all("(c\\(|\\))") %>%
  str_replace_all(":",", ") %>%
  str_replace_all(", ","-") %>%
  mSplit() %>%
  as_tibble() %>%
  setnames(.,names(.),paste0("company_",names(.))) %>%
  cbind(game_info,.)

game_info %>% View()


#Keywords
game_info <- game_info %>%
  mutate(keywords = as.character(keywords)) %>%
  mutate(keywords = case_when(keywords=="NULL"~"",
                                        T~keywords)) %>%
  select(keywords) %>%
  unlist() %>%
  str_remove_all("(c\\(|\\))") %>%
  str_replace_all(":",", ") %>%
  str_replace_all(", ","-") %>%
  mSplit() %>%
  as_tibble() %>%
  setnames(.,names(.),paste0("keyword_",names(.))) %>%
  cbind(game_info,.)



#Player Perspectives
game_info <- game_info %>%
  mutate(player_perspectives = as.character(player_perspectives)) %>%
  mutate(player_perspectives = case_when(player_perspectives=="NULL"~"",
                              T~player_perspectives)) %>%
  select(player_perspectives) %>%
  unlist() %>%
  str_remove_all("(c\\(|\\))") %>%
  str_replace_all(":",", ") %>%
  str_replace_all(", ","-") %>%
  mSplit() %>%
  as_tibble() %>%
  setnames(.,names(.),paste0("perspective_",names(.))) %>%
  cbind(game_info,.)



#Tags
game_info <- game_info %>%
  mutate(tags = as.character(tags)) %>%
  mutate(tags = case_when(tags=="NULL"~"",
                                         T~tags)) %>%
  select(tags) %>%
  unlist() %>%
  str_remove_all("(c\\(|\\))") %>%
  str_replace_all(":",", ") %>%
  str_replace_all(", ","-") %>%
  mSplit() %>%
  as_tibble() %>%
  setnames(.,names(.),paste0("tag_",names(.))) %>%
  cbind(game_info,.)


#Themes
game_info <- game_info %>%
  mutate(themes = as.character(themes)) %>%
  mutate(themes = case_when(themes=="NULL"~"",
                          T~themes)) %>%
  select(themes) %>%
  unlist() %>%
  str_remove_all("(c\\(|\\))") %>%
  str_replace_all(":",", ") %>%
  str_replace_all(", ","-") %>%
  mSplit() %>%
  as_tibble() %>%
  setnames(.,names(.),paste0("theme_",names(.))) %>%
  cbind(game_info,.)


#Age Ratings
game_info <- game_info %>%
  mutate(age_ratings = as.character(age_ratings)) %>%
  mutate(age_ratings = case_when(age_ratings=="NULL"~"",
                            T~age_ratings)) %>%
  select(age_ratings) %>%
  unlist() %>%
  str_remove_all("(c\\(|\\))") %>%
  str_replace_all(":",", ") %>%
  str_replace_all(", ","-") %>%
  mSplit() %>%
  as_tibble() %>%
  setnames(.,names(.),paste0("rating_",names(.))) %>%
  cbind(game_info,.)

#Franchises
game_info <- game_info %>%
  mutate(franchises = as.character(franchises)) %>%
  mutate(franchises = case_when(franchises=="NULL"~"",
                                 T~franchises)) %>%
  select(franchises) %>%
  unlist() %>%
  str_remove_all("(c\\(|\\))") %>%
  str_replace_all(":",", ") %>%
  str_replace_all(", ","-") %>%
  mSplit() %>%
  as_tibble() %>%
  setnames(.,names(.),paste0("franchise_",names(.))) %>%
  cbind(game_info,.)


game_info <- game_info[,!(colnames(game_info) %in% c("game_engines","genres","involved_companies","keywords","player_perspectives","tags","themes","age_ratings","franchises"))]

dbWriteTable(db,"games_info",game_info,overwrite=T)


mSplit <- function(vec) {
  if (!is.character(vec))
    vec <- as.character(vec)
  L <- strsplit(vec, "-")
  ids <- unlist(lapply(seq_along(L), function(i) rep(i, length(L[[i]])) ))
  U <- sort(unique(unlist(L)))
  M <- matrix(0, nrow = length(vec), 
              ncol = length(U), 
              dimnames = list(NULL, U))
  M[cbind(ids, match(unlist(L), U))] <- 1L
  M
}









  
  



POST(str_c(api_url,end_themes),
     add_headers(`Client-ID`=client_id,
                 Authorization=str_c("Bearer ",access_token)),
     body="fields id,name; limit 500;") %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble() %>%
  View()



POST(str_c(api_url,end_games),
     add_headers(`Client-ID`=client_id,
                 Authorization=str_c("Bearer ",access_token)),
     body='fields name,id; search "The Legend of Zelda: Ocarina of Time / Master Quest";') %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble() %>%
  View()


get_genres <- function(client_id,access_token){
  POST(str_c(api_url,end_genres),
       add_headers(`Client-ID`=client_id,
                   Authorization=str_c("Bearer ",access_token)),
       body="fields id,name; limit 100;") %>%
    use_series("content") %>%
    rawToChar() %>%
    fromJSON() %>%
    as_tibble() %>%
    arrange(id) %>%
    return()
}
get_platforms <- function(client_id,access_token){
  POST(str_c(api_url,end_platforms),
       add_headers(`Client-ID`=client_id,
                   Authorization=str_c("Bearer ",access_token)),
       body="fields id,name; limit 200;") %>%
    use_series("content") %>%
    rawToChar() %>%
    fromJSON() %>%
    as_tibble() %>%
    arrange(id) %>%
    return()  
}
get_themes <- function(client_id,access_token){
  POST(str_c(api_url,end_themes),
       add_headers(`Client-ID`=client_id,
                   Authorization=str_c("Bearer ",access_token)),
       body="fields id,name; limit 100;") %>%
    use_series("content") %>%
    rawToChar() %>%
    fromJSON() %>%
    as_tibble() %>%
    arrange(id) %>%
    return()
}
genres <- get_genres(client_id,access_token)
platforms <- get_platforms(client_id,access_token)
themes <- get_themes(client_id,access_token)



db <- dbConnect(SQLite(),dbname="MOMAM.sqlite")

dbWriteTable(db,"games_genres",genres,overwrite=T)
dbWriteTable(db,"games_platforms",platforms,overwrite=T)
dbWriteTable(db,"games_ratings",ratings,overwrite=T)
dbWriteTable(db,"games_themes",themes,overwrite=T)


dbDisconnect(db)
