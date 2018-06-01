library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
################################檔案載入與設定#################################


###############################################################################

score_range=1:100

sample(score_range,100,replace=FALSE)
student.score <- sample(score_range,50,replace=TRUE)
#sample(score_range,50),預設replace是FALSE

transcript <- tibble(score=student.score)
mean(transcript$score)
ggplot(transcript,aes(score))+geom_histogram(stat="bin",bins=10, fill="white",colour="black")+geom_line(color="red",stat="bin",bins=10)

###############################################################################

student.score<-round(rnorm(50,mean=80,sd=10),0)
transcript$score<-ifelse(student.score>100,100,student.score)
mean(transcript$score)
ggplot(transcript,aes(score))+geom_histogram(stat="bin",bins=10, fill="white",colour="black")+geom_line(color="red",stat="bin",bins=10)






