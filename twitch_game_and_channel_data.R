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
library(stringi)

db <- dbConnect(SQLite(),dbname="MOMAM.sqlite")

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

dbWriteTable(db,"games_name",select(game_info,id,name),overwrite=T)

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
ratings <- tibble(id=c(1,2,3,4,5,6,7,8,9,10,11,12),
                  slug=c("three","seven","twelve","sixten","eighteen",
                         "rp","ec","e","e10","t","m","ao"))

dbWriteTable(db,"games_ratings",ratings,overwrite=T)

##############################################################################

#Helper function that will be used when converting categorical columns to
#dummy variables.
#This is used since some columns are actually a list of multiple categorical
#values. Traditional dummy variable encoding function, unfortunately, expect
#each column to contain a single category and nothing else.
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

#Takes in a data frame with an id column and a column with a list of ids.
#column_prefix will be used for the new column names.
#table_with_info is the db table that contains id and slug info for every
#id in the column of interest.
categorical_to_dummy <- function(df,id_column,data_column,column_prefix,db,table_to_write,table_with_info){
  table <- dbGetQuery(db,str_c("SELECT * FROM ",table_with_info)) %>%
    as_tibble() %>%
    mutate(id = as.character(id))
  
  id_string <- table$id
  slug_string <- table$slug
  
  dummy_df <- df %>%
    mutate(new_column = as.character(eval(as.name(paste(data_column)))),
           new_column = case_when(new_column=="NULL"~"",
                                  T~new_column)) %>%  
    select(new_column) %>%
    unlist() %>%
    stri_replace_all_regex("\\b" %s+% id_string %s+% "\\b",slug_string,vectorize_all=F) %>%
    str_replace_all("-","_") %>%
    str_remove_all("(c\\(|\\))") %>%
    str_replace_all(":",", ") %>%
    str_replace_all(", ","-") %>%
    as.factor() %>%
    mSplit() %>%
    as_tibble() %>%
    setnames(.,names(.),paste0(column_prefix,names(.)))
  
  df_to_write <- df %>%
    pluck(id_column) %>%
    cbind(dummy_df) %>%
    setnames(.,names(.)[1],"id") #First column would be named "." otherwise.
  
  #The max number of columns for a sqlite table is (by default) 999.
  #This stores the tables in batches if there are more than 999 categories.
  if(ncol(df_to_write)>999){
    for(i in 1:ceiling(ncol(df_to_write)/999)){
      to_append <- str_c("_",as.character(i))
      
      #The first column will be the id column
      if(i == 1){
        start <- 2
        end <- 999
      }
      #The final subset may have less than 999 columns.
      else if(i == ceiling(ncol(df_to_write)/999)){
        message("Attempting to write a table with more than 999 columns. The values will be split across multiple tables.")
        start <- start + 998
        end <- ncol(df_to_write)
      }
      else{
        start <- start + 998
        end <- end + 998
      }
      
      #Always keep id column
      df_subset <- df_to_write[,c(1,start:end)]
      
      dbWriteTable(db,str_c(table_to_write,to_append),df_subset,overwrite=T)
      
    }
  }
  else{
    dbWriteTable(db,table_to_write,df_to_write,overwrite=T)  
  }
  
}

#Could mapply this, but I think this actually ends up being easier to read.
categorical_to_dummy(game_info,"id","game_engines","engine_",db,"data_engines","games_engines")
categorical_to_dummy(game_info,"id","genres","genre_",db,"data_genres","games_genres")
categorical_to_dummy(game_info,"id","keywords","keyword_",db,"data_keywords","games_keywords")
categorical_to_dummy(game_info,"id","platforms","platform_",db,"data_platforms","games_platforms")
categorical_to_dummy(game_info,"id","player_perspectives","perspective_",db,"data_perspectives","games_perspectives")
categorical_to_dummy(game_info,"id","themes","theme_",db,"data_themes","games_themes")
categorical_to_dummy(game_info,"id","age_ratings","rating_",db,"data_ratings","games_ratings")
categorical_to_dummy(game_info,"id","franchises","franchise_",db,"data_franchises","games_franchises")

#Companies once again require special treatment.
company_table <- dbGetQuery(db,"SELECT i.id, c.slug
           FROM games_involved_companies as i
           LEFT JOIN games_companies as c
           ON i.company = c.id") %>%
  as_tibble()

id_string <- company_table$id
slug_string <- company_table$slug

#Involved companies includes not just developers, but publishers and distributors as well.
#However, the developer has a much larger impact on who will win a particular game
#than the publisher does, meaning any publishers can be ignored.
#The normal stringi_replace_all function won't successfully replace all company ids
#Because some of those ids are for publishing companies.
game_info %>%
  mutate(new_column = as.character(involved_companies),
         new_column = case_when(new_column=="NULL"~"",
                                T~new_column)) %>%  
  select(new_column) %>%
  unlist() %>%
  stri_replace_all_regex("\\b" %s+% id_string %s+% "\\b",slug_string,vectorize_all=F) %>%
  str_replace_all("-","_") %>% #We're going to be removing all -'s later.
  str_remove_all("(c\\(|\\))") %>%
  str_replace_all(":",", ") %>%
  str_replace_all(", ","-") %>%
  str_remove_all("[0-9]{2,}") %>% #Remove ids for companies that are not developers 
  str_replace_all("-{2,}","-") %>% #Clean up the debris left behind by removing the publisher ids.
  str_remove_all("^-|-$") %>% #Remove -s at the beginning and end of string.
  as.factor() %>%
  mSplit() %>%
  as_tibble() %>%
  setnames(.,names(.),paste0("company_",names(.))) %>%
  cbind(game_info$id,.) %>%
  #This *could* be done in the previous setnames call, but it'd be ugly.
  setnames(.,names(.)[1],"id") %>% 
  dbWriteTable(db,"data_companies",.,overwrite=T)


#Determines how much time (in minutes) each streamer played a certain piece of metadata
#(genre, console, etc) for a give year. For example, calculate how much time
#pie spent playing adventure games in 2015.
get_annual_playtime <- function(database=db,db_table_year,data_table,columns){
  dbGetQuery(database,str_c("SELECT p.game,p.minutes, d.*
               FROM ",db_table_year," AS p
               LEFT JOIN ",data_table," AS d ON p.game_id = d.id")) %>%
    mutate_at(vars(starts_with(columns)),funs(.*minutes)) %>%
    summarise_at(vars(starts_with(columns)),sum) %>%
    dbWriteTable(db,str_c(db_table_year,str_extract(data_table,"_[:alnum:]+")),.,overwrite=T) 
}

#Faster and neater to mapply this than to write out the get_annual_playtime function 72 times.
mapply(get_annual_playtime,
       db_table_year=c(rep("pie2015",6),rep("pie2016",6),rep("pie2017",6),
                       rep("pie2018",6),rep("pie2019",6),rep("pie2020",6),
                       rep("spike2015",6),rep("spike2016",6),rep("spike2017",6),
                       rep("spike2018",6),rep("spike2019",6),rep("spike2020",6)),
       data_table=rep(c("data_themes","data_franchises","data_companies",
                        "data_genres","data_perspectives","data_platforms"),12),
       columns=rep(c("theme","franchise","compan","genre","perspective","platform"),12))



#Next step: Get cumulative play time for each category (should be fast.)

dbDisconnect(db)
