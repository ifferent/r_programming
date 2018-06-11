library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
################################檔案載入與設定#################################
taxrelation.table_taipei<-read_csv("D:/my_code/R_code/presentation_sample_ppt/ch5/taipei.csv",col_names=TRUE)
taxrelation.table_kaohsiung<-read_csv("D:/my_code/R_code/presentation_sample_ppt/ch5/kaohsiung.csv",col_names=TRUE)
taxrelation.table_kaohsiung2<-read_csv("D:/my_code/R_code/presentation_sample_ppt/ch5/kaohsiung_2.csv",col_names=TRUE)
taxrelation.table_newtpi<-read_csv("D:/my_code/R_code/presentation_sample_ppt/ch5/new_tpi.csv",col_names=TRUE)
taxrelation.table_taichung<-read_csv("D:/my_code/R_code/presentation_sample_ppt/ch5/taichung.csv",col_names=TRUE)
taxrelation.table_taichung2<-read_csv("D:/my_code/R_code/presentation_sample_ppt/ch5/taichung_2.csv",col_names=TRUE)
taxrelation.table_tainan<-read_csv("D:/my_code/R_code/presentation_sample_ppt/ch5/tainan.csv",col_names=TRUE)


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

###############################################################################

student_score.chinese<-round(rnorm(100,mean=80,sd=10),0)
student_score.math<-round(rnorm(100,mean=60,sd=30),0)
student_score.history<-round(rnorm(100,mean=90,sd=5),0)
student_score.geo<-round(rnorm(100,mean=70,sd=20),0)

transcript <- tibble("座號"=1:100,"國文"=student_score.chinese,"數學"=student_score.math,"歷史"=student_score.history,"地理"=student_score.geo)
transcript$國文<-ifelse(student_score.chinese>100,100,student_score.chinese)
transcript$數學<-ifelse(student_score.math>100,100,student_score.math)
transcript$數學<-ifelse(student_score.math<0,0,student_score.math)
transcript$歷史<-ifelse(student_score.history>100,100,student_score.history)
transcript$地理<-ifelse(student_score.geo>100,100,student_score.geo)

mean(transcript$國文)
mean(transcript$數學)
mean(transcript$歷史)
mean(transcript$地理)

ggplot(transcript,aes(國文))+geom_histogram(stat="bin",bins=10, fill="white",colour="black")+geom_line(color="red",stat="bin",bins=10)
ggplot(transcript,aes(數學))+geom_histogram(stat="bin",bins=10, fill="white",colour="black")+geom_line(color="red",stat="bin",bins=10)
ggplot(transcript,aes(歷史))+geom_histogram(stat="bin",bins=10, fill="white",colour="black")+geom_line(color="red",stat="bin",bins=10)
ggplot(transcript,aes(地理))+geom_histogram(stat="bin",bins=10, fill="white",colour="black")+geom_line(color="red",stat="bin",bins=10)

write_excel_csv(transcript,"成績單_new.csv")

###############################################################################
taxrelation.reg_taipei<-select(taxrelation.table_taipei,"公司家數","營業稅年度總稅收")
taxrelation.reg_kaohsiung<-select(taxrelation.table_kaohsiung,"公司家數","營業稅年度總稅收")
taxrelation.reg_kaohsiung2<-select(taxrelation.table_kaohsiung2,"公司家數","營業稅年度總稅收")
taxrelation.reg_newtpi<-select(taxrelation.table_newtpi,"公司家數","營業稅年度總稅收")
taxrelation.reg_taichung<-select(taxrelation.table_taichung,"公司家數","營業稅年度總稅收")
taxrelation.reg_taichung2<-select(taxrelation.table_taichung2,"公司家數","營業稅年度總稅收")
taxrelation.reg_tainan<-select(taxrelation.table_tainan,"公司家數","營業稅年度總稅收")

###############################################################################

ggplot(taxrelation.table_taipei,aes(x=公司家數,y=營業稅年度總稅收))+geom_point()+stat_smooth()+ggtitle("台北市營業稅稅收與公司家數關係")+theme(plot.title=element_text(hjust=0.5))

ggplot(taxrelation.table_kaohsiung,aes(x=公司家數,y=營業稅年度總稅收))+geom_point()+stat_smooth()+ggtitle("高雄市營業稅稅收與公司家數關係")+theme(plot.title=element_text(hjust=0.5))
ggplot(taxrelation.table_kaohsiung2,aes(x=公司家數,y=營業稅年度總稅收))+geom_point()+stat_smooth()+ggtitle("高雄市營業稅稅收與公司家數關係(局部放大)")+theme(plot.title=element_text(hjust=0.5))

ggplot(taxrelation.table_newtpi,aes(x=公司家數,y=營業稅年度總稅收))+geom_point()+stat_smooth()+ggtitle("新北市營業稅稅收與公司家數關係")+theme(plot.title=element_text(hjust=0.5))

ggplot(taxrelation.table_taichung,aes(x=公司家數,y=營業稅年度總稅收))+geom_point()+stat_smooth()+ggtitle("台中市營業稅稅收與公司家數關係")+theme(plot.title=element_text(hjust=0.5))
ggplot(taxrelation.table_tainan,aes(x=公司家數,y=營業稅年度總稅收))+geom_point()+stat_smooth()+ggtitle("台南市營業稅稅收與公司家數關係(局部放大)")+theme(plot.title=element_text(hjust=0.5))

###############################################################################

lm(data=taxrelation.reg_taipei,營業稅年度總稅收~公司家數^2)
lm(data=taxrelation.reg_kaohsiung,營業稅年度總稅收~公司家數^3)
lm(data=taxrelation.reg_newtpi,營業稅年度總稅收~公司家數^2)
lm(data=taxrelation.reg_taichung,營業稅年度總稅收~公司家數^4)
lm(data=taxrelation.reg_taichung,營業稅年度總稅收~公司家數^2)




