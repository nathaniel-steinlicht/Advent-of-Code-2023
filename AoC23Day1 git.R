#Day 1
#Part 1
aoc23day1 <- read.table("file path here", 
                        quote="\"", comment.char="")
digits<-gsub("\\D","",aoc23day1$V1)
digits.split<-strsplit(digits,"")
sum(as.numeric(
  unlist(
    lapply(1:length(digits.split),function(x) paste0(digits.split[[x]][1],
                                                     digits.split[[x]][length(digits.split[[x]])])))))
#Part 2
m<-gregexpr("(?=(one|two|three|four|five|six|seven|eight|nine|\\d))", aoc23day1$V1, perl=T)
digits.v2<-NULL
for(i in 1:length(m)){
  attr(m[[i]],"match.length")<-attr(m[[i]],"capture.length")
  first<-regmatches(aoc23day1$V1[i],m[i])[[1]][1]
  last<-tail(unlist(regmatches(aoc23day1$V1[i],m[i])),n=1)
  digits.v2<-append(digits.v2,paste0(first,last))
}
numbers<-c("one","two","three","four","five","six","seven","eight","nine")
for(i in 1:length(numbers)){
  digits.v2<-gsub(numbers[i],i,digits.v2)
}
sum(as.numeric(digits.v2))

