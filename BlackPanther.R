
#----------------------------------------------------------------------------
#  Using Unicode pattern matching to extract emojis from tweets
#----------------------------------------------------------------------------
# install.packages('DataCombine')
library(tidyverse)
library(DataCombine)

emoticons <- read.csv("emoji_dictionary.csv", header = T)
emojis=emoticons %>% mutate(Name_Number=paste0(Name,Number))
BlackPanther_df=read.csv("tweets.cleaned_EN-#blackpanther_0218-0218_2018-02-25_20-22-50_n16951.csv", header = T)

emojireplace <- FindReplace(data = BlackPanther_df, Var = "text", 
                            replaceData = emojis,
                            from = "R_Encoding", to = "Name_Number", 
                            exact = FALSE)


library(stringr)

emoji_final<-data.frame("emoji1",1)
names(emoji_final)<-c("EMOJI","Count")
for(i in emojis$Name_Number){
  a=sum(str_count( emojireplace$text,i))
  de<-data.frame(i,a)
  names(de)<-c("EMOJI","Count")
  emoji_final <- rbind(emoji_final, de)
}
emoji_final=emoji_final%>%  filter(!EMOJI=='emoji1')

#Top 10
emoji_final_clean_top10=emoji_final%>%  arrange(desc(Count))%>% 
  mutate(rank= as.numeric(row.names(emoji_final)))%>%
  mutate(dens=1000*Count/nrow(emoji_final))%>%
  mutate(Codepoint=EMOJI)%>%
  filter(!rank>=11)

emoji_final_clean_top10 <- FindReplace(data = emoji_final_clean_top10, Var = "EMOJI", 
                                 replaceData = emojis,
                                 from = "Name_Number", to = "Name", 
                                 exact = FALSE)
emoji_final_clean_top10 <- FindReplace(data = emoji_final_clean_top10, Var = "Codepoint", 
                                 replaceData = emojis,
                                 from = "Name_Number", to = "Codepoint", 
                                 exact = FALSE)
emoji_final_clean_top10

#Top 20
emoji_final_clean_top20=emoji_final%>%  arrange(desc(Count))%>% 
  mutate(rank= as.numeric(row.names(emoji_final)))%>%
  mutate(dens=1000*Count/nrow(emoji_final))%>%
  mutate(Codepoint=EMOJI)%>%
  filter(!rank>=21)

emoji_final_clean_top20 <- FindReplace(data = emoji_final_clean_top20, Var = "EMOJI", 
                                       replaceData = emojis,
                                       from = "Name_Number", to = "Name", 
                                       exact = FALSE)
emoji_final_clean_top20 <- FindReplace(data = emoji_final_clean_top20, Var = "Codepoint", 
                                       replaceData = emojis,
                                       from = "Name_Number", to = "Codepoint", 
                                       exact = FALSE)
emoji_final_clean_top20


#All
emoji_final_clean=emoji_final%>%  arrange(desc(Count))%>% 
  mutate(rank= as.numeric(row.names(emoji_final)))%>%
  mutate(dens=1000*Count/nrow(emoji_final))%>%
  mutate(Codepoint=EMOJI)%>%
  filter(!Count==0)


emoji_final_clean <- FindReplace(data = emoji_final_clean, Var = "EMOJI", 
                                       replaceData = emojis,
                                       from = "Name_Number", to = "Name", 
                                       exact = FALSE)
emoji_final_clean<- FindReplace(data = emoji_final_clean, Var = "Codepoint", 
                                       replaceData = emojis,
                                       from = "Name_Number", to = "Codepoint", 
                                       exact = FALSE)

emoji_final_clean
#----------------------------------------------------------------------------
#  Group favorites and retweets by emojis
#----------------------------------------------------------------------------
library(tidyverse)
library(DataCombine)

emoji_final_with_Favorites_Retweet<-data.frame("emoji1",1,2,3)
names(emoji_final_with_Favorites_Retweet)<-c("EMOJI","Count",'Retweets','Favorites')
for(i in emojis$Name_Number){
  a=sum(str_count( emojireplace$text,i))
  b=grepl(pattern = i,x = emojireplace$text,fixed='TURE')
  Emoji_Retweets=sum(emojireplace$retweets[b])
  Emoji_Favorites=sum(emojireplace$favorites[b])
  de<-data.frame(i,a,Emoji_Retweets,Emoji_Favorites)
  names(de)<-c("EMOJI","Count",'Retweets','Favorites')
  emoji_final_with_Favorites_Retweet <- rbind(emoji_final_with_Favorites_Retweet, de)
}
emoji_final_with_Favorites_Retweet=emoji_final_with_Favorites_Retweet%>%  filter(!EMOJI=='emoji1')



emoji_final_withFR_top20=emoji_final_with_Favorites_Retweet%>%  arrange(desc(Count))%>% 
  mutate(rank= as.numeric(row.names(emoji_final_with_Favorites_Retweet)))%>%
  mutate(dens=1000*Count/nrow(emoji_final_with_Favorites_Retweet))%>%
  mutate(Codepoint=EMOJI)%>%
  filter(!rank>=21)

emoji_final_withFR_top20 <- FindReplace(data = emoji_final_withFR_top20, Var = "EMOJI", 
                                       replaceData = emojis,
                                       from = "Name_Number", to = "Name", 
                                       exact = FALSE)
emoji_final_withFR_top20 <- FindReplace(data = emoji_final_withFR_top20, Var = "Codepoint", 
                                       replaceData = emojis,
                                       from = "Name_Number", to = "Codepoint", 
                                       exact = FALSE)
emoji_final_withFR_top20

#----------------------------------------------------------------------------
# Part 3 📊: Visualizing emojis(basic)
#----------------------------------------------------------------------------

#Top 20
df.plot <- emoji_final_clean_top20; 
# Eliminate the space
df.plot$EMOJI=gsub(' ','',df.plot$EMOJI)
xlab= 'Rank'; 
ylab <- 'Overall Density in Tweets';

setwd('D:/工作/PRIMOJIS/Download  all emojis pictures by using Python - 副本/AlanChi_emojis_all picture');
imgs <- lapply(paste0(df.plot$Codepoint, '.png'), png::readPNG)
g <- lapply(imgs, grid::rasterGrob);
k <- 0.20 * (10/nrow(df.plot)) * max(df.plot$dens); 
df.plot$xsize <- k; df.plot$ysize <- k; 
#df.plot$ysize <- k * (df.plot$dens / max(df.plot$dens));
df.plot$ysize <- 0.55*k;
g1 <- ggplot(data = df.plot, aes(x = rank, y = dens)) +
  geom_bar(stat = 'identity', fill = '#86C166') +
  xlab(xlab) + ylab(ylab)+
  mapply(function(x, y, i) {
    annotation_custom(g[[i]], xmin = x-0.5*df.plot$xsize[i], xmax = x+0.5*df.plot$xsize[i], 
                      ymin = y-0.5*df.plot$ysize[i], ymax = y+0.5*df.plot$ysize[i])},
    df.plot$rank, df.plot$dens, seq_len(nrow(df.plot))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(df.plot), 1), labels = seq(1, nrow(df.plot), 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(df.plot$dens))) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
g1

#----------------------------------------------------------------------------
# Word frequncy in One emojis (FACEWITHTEARSOFJOY)
#----------------------------------------------------------------------------

emojireplace
# find the location of certain strings


library(stringr)

emoji_final<-data.frame("emoji1",1)
names(emoji_final)<-c("EMOJI","Count")
for(i in emojis$Name_Number){
  a=sum(str_count( emojireplace$text,i))
  de<-data.frame(i,a)
  names(de)<-c("EMOJI","Count")
  emoji_final <- rbind(emoji_final, de)
}
emoji_final=emoji_final%>%filter(!EMOJI=='emoji1')



emoji_final<-data.frame("emoji1",1,'TEXT')
names(emoji_final)<-c("EMOJI","Count",'TEXT')
for(i in emojis$Name_Number){
  a=sum(str_count( emojireplace$text,i))
  de<-data.frame(i,a)
  names(de)<-c("EMOJI","Count")
  emoji_final <- rbind(emoji_final, de)
}
emoji_final=emoji_final%>%  filter(!EMOJI=='emoji1')


## find rows with emojis of FACEWITHTEARSOFJOY
a=grepl(pattern = "FACEWITHTEARSOFJOY 3",x = emojireplace$text,fixed='TURE')
Emoji_facewithtearsofjot=emojireplace$text[a]
Emoji_facewithtearsofjot

#install.packages('RColorBrewer')
# install.packages('wordcloud')
library(RColorBrewer)
library(wordcloud)
library(tidyverse)

#Data cleansing , eliminate the ' ' and punctuation

txt = tolower(Emoji_facewithtearsofjot)
txtList = lapply(txt, strsplit," ")
txtChar = unlist(txtList)
txtChar = gsub("\\.|,|\\!|:|;|\\?","",txtChar) #clean symbol(.,!:;?)
txtChar = txtChar[txtChar!=""]
data = as.data.frame(table(txtChar))
colnames(data) = c("Word","freq")
ordFreq = data[order(data$freq,decreasing=T),]

#Filter the meaningless word(stopwprds) & the emoji name & numbers
df_stopwords = read.delim('C:/Users/skyof/Desktop/PRISMOJI/#BlackPanther/BlackPanther/Stopwords/scikitlearn.txt',header = F)
names(df_stopwords)<-c("Word")
antiWord = data.frame(df_stopwords,stringsAsFactors=F)
antiWord_emoji=data.frame(tolower(emojis$Name),stringsAsFactors=F)
# Eliminate the space
antiWord_emoji$Word=gsub(' ','',antiWord_emoji$Word)
names(antiWord_emoji)<-c("Word")


result1= anti_join(ordFreq,antiWord,by="Word") %>% 
  arrange(desc(freq))


result=anti_join(result1,antiWord_emoji,by="Word")%>%
  filter(!grepl('[0-9]',Word))%>%
  filter(!Word=='#blackpanther')
result = result[1:50,]
head(result,20)

wordcloud(words=result$Word,freq=result$freq,scale=c(3,.5),col=rainbow(length(result$freq)))



## --------------------------------------------------------
#Word frequncy in All emojis
#------------------------------------------------------------
library(RColorBrewer)
library(wordcloud)
txt = tolower(emojireplace$text)
txtList = lapply(txt, strsplit," ")
txtChar = unlist(txtList)
txtChar = gsub("\\.|,|\\!|:|;|\\?","",txtChar) #clean symbol(.,!:;?)
txtChar = txtChar[txtChar!=""]
data = as.data.frame(table(txtChar))
colnames(data) = c("Word","freq")
ordFreq = data[order(data$freq,decreasing=T),]

#Filter the meaningless word(stopwprds) & the emoji name & numbers
df_stopwords = read.delim('C:/Users/skyof/Desktop/PRISMOJI/#BlackPanther/BlackPanther/Stopwords/scikitlearn.txt',header = F)
names(df_stopwords)<-c("Word")
antiWord = data.frame(df_stopwords,stringsAsFactors=F)
antiWord_emoji=data.frame(tolower(emojis$Name),stringsAsFactors=F)
# Eliminate the space

antiWord_emoji$Word=gsub(' ','',antiWord_emoji$Word)

names(antiWord_emoji)<-c("Word")

result1= anti_join(ordFreq,antiWord,by="Word") %>% 
  arrange(desc(freq))
result=anti_join(result1,antiWord_emoji,by="Word")%>%
  filter(!grepl('[0-9]',Word))%>%
  filter(!Word=='#blackpanther')
result = result[1:50,]
head(result,20)
wordcloud(words=result$Word,freq=result$freq,scale=c(3,.5),col=rainbow(length(result$freq)))

