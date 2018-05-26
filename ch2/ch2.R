library("readr")
library("dplyr")

############################################檔案載入與設定################################################

ch2sample.exp1_path<-"ch2/sample_data/Ch2_exp_table1.csv"

##########################################################################################################

mych2.csv<-read_csv(ch2sample.exp1_path)
#mych2.csv<-read_csv("D:/my_code/R_code/presentation_sample_ppt/ch2/Ch2_exp_table1.csv")

mych2.csv2<-read.csv(ch2sample.exp1_path,header=TRUE)
#mych2.csv2<-read.csv("D:/my_code/R_code/presentation_sample_ppt/ch2/Ch2_exp_table1.csv",header=TRUE)

mych2.csv3<-read.csv(ch2sample.exp1_path,header=FALSE)
#mych2.csv3<-read.csv("D:/my_code/R_code/presentation_sample_ppt/ch2/Ch2_exp_table1.csv",header=FALSE)

filter(mych2.csv, month==12, day==25)

filter(mych2.csv, month==2, dep_delay<=0)

our.view<-filter(mych2.csv2, month==12, day==25)#這是使用R預設的函數讀取的表格
head(our.view)

select(mych2.csv,month,day,arr_time)

select(mych2.csv,year:dep_time,air_time:time_hour)

summarize(mych2.csv, delay=mean(dep_delay, na.rm=TRUE))  

by_day<-group_by(mych2.csv, day)#每年每月的同一天
sum.by_day<-summarize(by_day, delay=mean(dep_delay, na.rm=TRUE))  

by_day_mon<-group_by(mych2.csv, month, day)#每年的同一天同一月
sum.by_day_mon<-summarize(by_day_mon, delay=mean(dep_delay, na.rm=TRUE))  

by_day_mon_year<-group_by(mych2.csv, year ,month, day)
sum.by_day_mon_year<-summarize(by_day_mon_year, delay=mean(dep_delay, na.rm=TRUE))  







