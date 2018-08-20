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
name<-c("Alice","Bob","Roddy","Eddie","William",
        "Howard","Rose","Mary","Daisy","Jack")
hight<-sample(150:185,10,replace=T)

data.frame (
  c(1:10),
  2,
  3:12,
  name
)

data.frame (
  c(1:9),
  2,
  3:12,
  name
)#序列資料不等長，無法自動產生，但是單一資料可以自動產生

tibble(
  c(1:10),
  2,
  3:12,
  name
)

tibble(
  profile.name  = name,
  profile.age   = c(23,22,20,18,24,25,23,20,21,22),
  profile.hight = hight,
  profile.sex   = c("F","M","M","M","M","M","F","F","F","M")   
)

tibble(
  `@@`       = name,
  `A~G~E`    = c(23,22,20,18,24,25,23,20,21,22),
  `  T . T ` = hight,
  `5000`     = c("F","M","M","M","M","M","F","F","F","M")   
)#可以使用``符號使特殊符號、空格、數字等成為資料欄的名稱

df_exp<-tibble(
  profile.name  = name,
  profile.age   = age,
  profile.hight = hight,
  profile.sex   = sex,
  #與data.frame的最大差別
  magic_index   = profile.hight/(profile.age*ifelse(profile.sex=="M",1,2)) 
)

out_exp<-tribble(
          ~profile.name, ~profile.age, ~profile.hight, ~profile.sex, ~magic_index,
        #--------------/-------------/---------------/-------------/---------------------
               "Alice",           23,       hight[1],          "女", df_exp$magic_index[1],
                 "Bob",           22,       hight[2],          "男", df_exp$magic_index[2],
               "Roddy",           20,       hight[3],          "男", df_exp$magic_index[3],
               "Eddie",           18,       hight[4],          "男", df_exp$magic_index[4],
             "William",           24,       hight[5],          "男", df_exp$magic_index[5],
              "Howard",           25,       hight[6],          "男", df_exp$magic_index[6],
                "Rose",           23,       hight[7],          "女", df_exp$magic_index[7],
                "Mary",           20,       hight[8],          "女", df_exp$magic_index[8],
               "Daisy",           21,       hight[9],          "女", df_exp$magic_index[9],
                "Jack",           22,      hight[10],          "男", df_exp$magic_index[10]
)#增加程式可讀性

###############################################################################
output_path<-c("output/ch5/")
write.csv(out_exp, paste(output_path, "use_base_writ_csv.csv"), 
          row.names=F, fileEncoding="UTF-8")

write.csv(out_exp, paste(output_path, "use_base_writ_csv_noutf8.csv"), 
          row.names=F)
read_csv(paste(output_path, "use_base_writ_csv_noutf8.csv"),col_names=TRUE)
#採用預設的編碼，雖然用excel並不會亂碼，但用這個檔案再次給R讀取就會出現悲劇

write.csv(out_exp,paste(output_path, "use_base_writ_csv2.csv"), 
          row.names=T, fileEncoding="UTF-8")

write_csv(out_exp,paste(output_path, "use_readr_writ_csv.csv"))
write_excel_csv(out_exp,paste(output_path, "use_readr_writ_excel_csv.csv"))

###############################################################################

#1
pois.lambda <- 7
dpois(0,pois.lambda)
#2
1-dpois(0,pois.lambda)-dpois(1,pois.lambda)

1-ppois(1,pois.lambda) # 1 - P[X ≤ 1]

ppois(1, pois.lambda, lower.tail=F) # P[X > 1]

#3
pois.lambda_per30sec <- pois.lambda/2
ppois(0, pois.lambda_per30sec, lower.tail=F ) # P[X > 0]

#4
ppois(4, pois.lambda, lower.tail=F) # P[X > 4]

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

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

score_range=1:100

sample(score_range,100,replace=FALSE)
student.score <- sample(score_range,50,replace=TRUE)
#sample(score_range,50),預設replace是FALSE

transcript <- tibble(score=student.score)
mean(transcript$score)
ggplot(transcript,aes(score))+geom_histogram(stat="bin",bins=10, fill="white",colour="black")+geom_line(color="red",stat="bin",bins=10)

student.score<-round(rnorm(50,mean=80,sd=10),0)
transcript$score<-ifelse(student.score>100,100,student.score)
mean(transcript$score)
ggplot(transcript,aes(score))+geom_histogram(stat="bin",bins=10, fill="white",colour="black")+geom_line(color="red",stat="bin",bins=10)

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




