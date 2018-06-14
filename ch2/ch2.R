###############################################################################
###############################################################################
####                                                                       ####
#### 完成日期: 2018-06-14                                                  ####
#### 作者：Roddy Hung                                                      ####
#### 版本：V1.1                                                            ####
####                                                                       ####
#### 第2章範例程式:                                                        ####
####    1.比較套件base與readr讀取原始資料的方式                            ####
####    2.利用套件dplyr過濾與挑選資料(表格)                                ####
####    3.怎樣將摘要一個表格                                               ####
####                                                                       ####
###############################################################################
###############################################################################

source("common/check_package.R")#檢查是否有未安裝的套件

###############################################################################
####                                                                       ####
#### 載入套件相關使用函數參考:                                             ####
#### readr: read_csv                                                       ####
#### dplyr: filter, seleclt, summarize, group_by                           ####
####                                                                       ####
###############################################################################

library("readr")#載入readr套件
library("dplyr")#載入dplyr套件

################################檔案載入與設定#################################

ch2sample.exp1_path<-"ch2/sample_data/Ch2_exp_table1.csv"

mych2.csv<-read_csv(ch2sample.exp1_path)
#檔案完整路徑如下：
#mych2.csv<-read_csv("D:/my_code/R_code/presentation_sample_ppt/ch2/Ch2_exp_table1.csv")

mych2.csv2<-read.csv(ch2sample.exp1_path,header=TRUE)#這是使用R預設utils套件的函數讀取的表格
#檔案完整路徑如下：
#mych2.csv2<-read.csv("D:/my_code/R_code/presentation_sample_ppt/ch2/Ch2_exp_table1.csv",header=TRUE)

mych2.csv3<-read.csv(ch2sample.exp1_path,header=FALSE)#第一列變數包含在表格元素內
#檔案完整路徑如下：
#mych2.csv3<-read.csv("D:/my_code/R_code/presentation_sample_ppt/ch2/Ch2_exp_table1.csv",header=FALSE)

###############################################################################

filter(mych2.csv, month==12, day==25)

filter(mych2.csv, month==2, dep_delay<=0)

our.view<-filter(mych2.csv2, month==12, day==25)
head(our.view)

select(mych2.csv,month,day,arr_time)
#這是使用R預設base套件的cbind函數讀取的表格
temp_view <- cbind(mych2.csv$month, mych2.csv$day, mych2.csv$arr_time)
head(temp_view)

select(mych2.csv,year:dep_time,air_time:time_hour)
#cbind沒辦法這樣使用cbind(mych2.csv$year:mych2.csv$dep_time, mych2.csv$air_time:mych2.csv$time_hour)

summarize(mych2.csv, delay=mean(dep_delay, na.rm=TRUE))  
#引數na.rm有沒有要包含缺失值 

by_day<-group_by(mych2.csv, day)#每年每月的同一天
sum.by_day<-summarize(by_day, delay=mean(dep_delay, na.rm=TRUE))  

by_day_mon<-group_by(mych2.csv, month, day)#每年的同一天同一月
sum.by_day_mon<-summarize(by_day_mon, delay=mean(dep_delay, na.rm=TRUE))  

by_day_mon_year<-group_by(mych2.csv, year ,month, day)
sum.by_day_mon_year<-summarize(by_day_mon_year, delay=mean(dep_delay, na.rm=TRUE))  

summary(mych2.csv)#比較一下R預設base套件的summary函數的摘要表格







