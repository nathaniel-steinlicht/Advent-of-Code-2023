#Day 2
#Part 1
aoc23day2 <- read.delim2("file path here",
                         header=FALSE)
games<-unlist(regmatches(aoc23day2$V1,gregexpr("Game \\d{,2}",aoc23day2$V1)))
game.results<-gsub(".{,6}\\d: ","",aoc23day2$V1)
game.df<-data.frame()
for(i in 1:length(games)){
  game.split<-unlist(strsplit(game.results[i],";"))
  for(j in 1:length(game.split)){
    green<-as.numeric(gsub(" green","",regmatches(game.split[j],gregexpr("\\d{1,} green",game.split[j]))))
    red<-as.numeric(gsub(" red","",regmatches(game.split[j],gregexpr("\\d{1,} red",game.split[j]))))
    blue<-as.numeric(gsub(" blue","",regmatches(game.split[j],gregexpr("\\d{1,} blue",game.split[j]))))
    game.df<-rbind(game.df,data.frame(green,red,blue,i))
  }
}
game.df$green<-ifelse(is.na(game.df$green),0,game.df$green)
game.df$red<-ifelse(is.na(game.df$red),0,game.df$red)
game.df$blue<-ifelse(is.na(game.df$blue),0,game.df$blue)
game.df$valid<-ifelse(game.df$green<=13 & game.df$red<=12 &game.df$blue<=14,0,1)
valid.games<-aggregate(game.df$valid,by=list(ID=game.df$i),sum)
sum(subset(valid.games,x==0)$ID)
#Part 2
#find the maximum number of cubes need to play all games
#multiple the amount for each color together
#sum the power of these steps
game.power<-NULL
for(k in 1:length(games)){
  game.k<-subset(game.df,i==k)
  green.max<-max(game.k$green)
  red.max<-max(game.k$red)
  blue.max<-max(game.k$blue)
  game.power<-append(game.power,c(green.max*red.max*blue.max))
}
sum(game.power)
