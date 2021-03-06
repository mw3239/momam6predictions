#The purpose of this file is to 1) Visualize predictions across various models
#and 2) Track how well each model is performing by updating the scores after
#each round.

library(stringr)
library(purrr)
library(ggplot2)
library(forcats) #for releveling factors
library(reshape2)
library(dplyr)
library(magrittr)
library(readr)


pie_prob <- read_csv("final_model_predictions_momam6.csv")


visualize <- function(pie_prob){
  pie_prob %>%
    magrittr::extract(seq(2,ncol(pie_prob)-3)) %>%
    melt() %>%
    mutate(variable = as.character(variable)) %>%
    mutate(variable = case_when(variable=="round_1"~"MP3",
                                variable=="round_2"~"Pikmin 3",
                                variable=="round_3"~"Ape Esc",
                                variable=="round_4"~"BfBB",
                                variable=="round_5"~"YGO:DotR",
                                variable=="round_6"~"PokeShip",
                                variable=="round_7"~"PokeTCG",
                                variable=="round_8"~"Rocket",
                                variable=="round_9"~"M:ZM",
                                variable=="round_10"~"MM2MOMAM",
                                variable=="round_11"~"SMB35",
                                variable=="round_12"~"BotW",
                                variable=="round_13"~"SADX",
                                variable=="round_14"~"MK8Sub",
                                variable=="round_15"~"MK:DD",
                                variable=="round_16"~"BeatEm",
                                variable=="round_17"~"MG:TT",
                                variable=="round_18"~"MP3CPU",
                                variable=="round_19"~"SM3DWBF",
                                variable=="round_20"~"Risk 2",
                                variable=="round_21"~"LMGCN",
                                variable=="round_22"~"Elmo",
                                variable=="round_23"~"OoTSub",
                                variable=="round_24"~"CVNES",
                                variable=="round_25"~"WL4",
                                variable=="round_26"~"MvsDK",
                                variable=="round_27"~"SMRPG",
                                variable=="round_28"~"SMAS",
                                variable=="round_29"~"GoldPig",
                                variable=="round_30"~"Undertale",
                                variable=="round_31"~"PS2RR",
                                T~variable)) %>%
    mutate(variable = fct_relevel(variable,"MP3","Pikmin 3","Ape Esc","BfBB",
                                  "YGO:DotR","PokeShip","PokeTCG","Rocket","M:ZM",
                                  "MM2MOMAM","SMB35","BotW","SADX","MK8Sub",
                                  "MK:DD","BeatEm","MG:TT","MP3CPU","SM3DWBF",
                                  "Risk 2","LMGCN","Elmo","OoTSub","CVNES",
                                  "WL4","MvsDK","SMRPG","SMAS","GoldPig",
                                  "Undertale","PS2RR")) %>%
    ggplot(aes(x=variable,y=value)) +
    geom_boxplot(aes(fill=variable),
                 outlier.alpha = .8,
                 outlier.size = .8,
                 outlier.stroke = 1.2) +
    xlab("Game") +
    ylab("Pie's Predicted Chance of Winning") +
    ggtitle("MOMAM6 Predictions with Various Models") +
    scale_y_continuous(expand=c(0,0)) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.text.x = element_text(angle=69,vjust=.5,hjust=.25))
}

update_scores <- function(df,winner,round){
  column <- str_c("round_",round)
  
  df %>%
    mutate(correct = case_when((winner=="pie" & df[column] > .5)~correct+1,
                               (winner=="spike" & df[column] < .5)~correct+1,
                               T~correct),
           score = case_when(winner=="pie"~score+eval(as.name(paste(column))),
                             winner=="spike"~score+(1-eval(as.name(paste(column)))))) %>%
    return()
}


pie_prob %>%
  count(correct) %>%
  arrange(desc(correct))

visualize(pie_prob)

pie_prob <- update_scores(pie_prob,"pie",1)
pie_prob <- update_scores(pie_prob,"spike",2)
pie_prob <- update_scores(pie_prob,"spike",3)
pie_prob <- update_scores(pie_prob,"pie",4)

pie_prob %>%
  select(method,round_4,round_5,correct,score,road2momam6_acc) %>%
  arrange(desc(correct),desc(score),desc(road2momam6_acc)) %>%
  View()


write_csv(pie_prob,"final_model_predictions_momam6.csv")