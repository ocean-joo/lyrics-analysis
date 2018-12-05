library(rJava)
library(memoise)
library(KoNLP)
library(dplyr)
library(ggplot2)
library(stringr)
#require(devtools)
#install_github("lchiffon/wordcloud2", force=TRUE)
library(wordcloud2)

library(extrafont)
font_import()
theme_set(theme_gray(base_family="AppleGothic"))

useSejongDic()

red_velvet <- readLines("data/RedVelvet.txt")
twice <- readLines("data/Twice.txt")
fx <- readLines("data/Fx.txt")

file2df <- function(input){
  input <- str_replace_all(input, "\\W", " ")
  nouns <- extractNoun(input)
  wordCount <- table(unlist(nouns))
  df_word <- as.data.frame(wordCount, stringsAsFactors=F)
  df_word <- rename(df_word, word=Var1, freq=Freq)
  df_word <- filter(df_word, (nchar(word) >= 2)&(as.numeric(freq) >= 3)) %>% 
    arrange(desc(freq))
  return (df_word)
}

file2df_noEng <- function(input) {
  
  input <- str_replace_all(input, "\\W", " ")
  input <- str_replace_all(input, "[a-zA-Z0-9]", "")
  
  input <- trimws(input)
  nouns <- extractNoun(input)
  wordCount <- table(unlist(nouns))
  df_word <- as.data.frame(wordCount, stringsAsFactors=F)
  df_word <- rename(df_word, word=Var1, freq=Freq)
  df_word <- filter(df_word, nchar(word) >= 2) %>% 
    arrange(desc(freq))
  return(df_word)
}
file2df_onlyEng <- function(input) {
  
  input <- str_replace_all(input, "\\W", " ")
  input <- str_replace_all(input, "[^a-zA-Z ]", "")
  
  input <- trimws(input)
  nouns <- extractNoun(input)
  wordCount <- table(unlist(nouns))
  df_word <- as.data.frame(wordCount, stringsAsFactors=F)
  df_word <- rename(df_word, word=Var1, freq=Freq)
  df_word <- filter(df_word, nchar(word) >= 2) %>% 
    arrange(desc(freq))
  return(df_word)
}

drawTop20 <- function(input) {
  input <- str_replace_all(input, "\\W", " ")
  input <- trimws(input)
  nouns <- extractNoun(input)
  wordCount <- table(unlist(nouns))
  df_word <- as.data.frame(wordCount, stringsAsFactors=F)
  df_word <- rename(df_word, word=Var1, freq=Freq)
  df_word <- filter(df_word, nchar(word) >= 2) %>% 
    arrange(desc(freq)) %>% 
    head(20)
  order <- arrange(df_word, freq)$word
  ggplot(data=df_word, aes(x=word, y=freq)) +
    geom_col() +
    coord_flip() + 
    scale_x_discrete(limit=order) + 
    geom_text(aes(label=freq), hjust=-0.3)
}

drawTop20(red_velvet)
drawTop20(twice)
drawTop20(fx)

red_velvet_df <- file2df(red_velvet)
red_velvet_no_eng <- file2df_noEng(red_velvet)
red_velvet_only_eng <- file2df_onlyEng(red_velvet)

twice_df <- file2df(twice)
twice_no_eng <- file2df_noEng(twice)
twice_only_eng <- file2df_onlyEng(twice)

fx_df <- file2df(fx)
fx_no_eng <- file2df_noEng(fx)
fx_only_eng <- file2df_onlyEng(fx)

#red_velvet_color <- c("#aedaf5", "#fae882", "#d8c7ed", "#cee872", "#f5cab5", "#c5e8fc", "#98c9fa")
red_velvet_color <- c("#d63333", "#e8d3d7", "#d43f56", "#e6aaa3", "#d6372f", "#8f2621")
twice_color <- c("#fccfa6", "#fdb3a5", "#fe97a4", "#fe7ba3", "#ff5fa2")
fx_color <- c("#8171eb", "#c734cf", "#68157a", "#401394", "#6d89de", "#85038f", "#9128c9")
  
set.seed(9999)

wordcloud2(red_velvet_df,
           figPath="data/redvelvet_logo.jpeg",
           color=rep(red_velvet_color,100),
           size=0.7)

wordcloud2(red_velvet_no_eng, 
           figPath="data/redvelvet_logo.jpeg", 
           color=rep(red_velvet_color,120))

wordcloud2(red_velvet_only_eng, 
           figPath="data/redvelvet_logo.jpeg", 
           color=rep(red_velvet_color,120))

wordcloud2(twice_df, 
           figPath="data/twice_logo.jpg", 
           color=rep(twice_color,100))

wordcloud2(twice_no_eng, 
           figPath="data/twice_logo.jpg", 
           color=rep(twice_color,100))

wordcloud2(twice_only_eng, 
           figPath="data/twice_logo.jpg", 
           color=rep(twice_color,100))

wordcloud2(fx_df, 
           figPath="data/fx_logo.png", 
           color=rep(fx_color,80))

wordcloud2(fx_no_eng, 
           figPath="data/fx_logo.png", 
           color=rep(fx_color,80),
           size=1.2)

wordcloud2(fx_only_eng, 
           figPath="data/fx_logo.png", 
           color=rep(fx_color,80),
           size=1)



