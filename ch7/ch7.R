###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
###                                                                       ###
### 完成日期: 2018-10-14                                                  ###
### 作者：Roddy Hung                                                      ###
### 版本：V0.1                                                            ###
###                                                                       ###
### 第7章範例程式:                                                        ###
###                                                                       ###
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

source("common/check_package.R")#檢查是否有未安裝的套件
source("common/function.R",encoding="utf-8") #將公用自訂函數載起來

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(readr)
library(lubridate)
library(zoo)
library(xts)
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
################################檔案載入與設定#################################

ch7sample.exp1_path="ch7/sample_data/日期時間表格範例.csv"
ch7sample.exp2_path="ch7/sample_data/中華電信股價(日).csv"
ch7sample.exp3_path="ch7/sample_data/中華電信股價(單日).csv"
ch7sample.exp4_path="ch7/sample_data/CPI_總指數.csv"
ch7sample.exp5_path="ch7/sample_data/CPI_房租.csv"
ch7sample.exp6_path="ch7/sample_data/CPI_娛樂費用.csv"

ch7sample.exp1<-read_csv(ch7sample.exp1_path, col_names=TRUE)
ch7sample.exp2<-read_csv(ch7sample.exp2_path, col_names=TRUE)
ch7sample.exp3<-read_csv(ch7sample.exp3_path, col_names=TRUE)
ch7sample.exp4<-read_csv(ch7sample.exp4_path, col_names=TRUE)
ch7sample.exp5<-read_csv(ch7sample.exp5_path, col_names=TRUE)
ch7sample.exp6<-read_csv(ch7sample.exp6_path, col_names=TRUE)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
########## 時間與日期的建立與處理 ##########

today()
now()

ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")

hm("13:04")
hms("03:13,04")
hms("03,13,04")

ymd_hms("2017 01 31 22 25 08")
mdy_hms("may 01 2017 9 25 08")
ymd_hms("2017-10-20,22:25:08")

mutate(
    select(ch7sample.exp1, "年", "月", "日", "時", "分"),
    "整合時間"=make_datetime(年, 月, 日, 時, 分)
)

exp_date_time<-transmute(
    select(ch7sample.exp1, "年", "月", "日", "時", "分"),
    "整合時間"=make_datetime(年, 月, 日, 時, 分)
)

year(exp_date_time$"整合時間")
month(exp_date_time$"整合時間")
month(exp_date_time$"整合時間", label=T)
day(exp_date_time$"整合時間")
mday(exp_date_time$"整合時間")
yday(exp_date_time$"整合時間")
wday(exp_date_time$"整合時間")
wday(exp_date_time$"整合時間",label=T)

year(exp_date_time$"整合時間"[13:19])<-2015
month(exp_date_time$"整合時間"[2:8])<-c(5,12,1,7,9,10,4)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
########## 時間與日期的算數 ##########

exp_date_time$"整合時間"[20] - exp_date_time$"整合時間"[1]
diff_time<-exp_date_time$"整合時間"[20] - exp_date_time$"整合時間"[1]
exp_date_time$"整合時間"[2] - exp_date_time$"整合時間"[5]

as.duration(diff_time)#差值換算

ymd("2018-10-15")+ddays(5)
ymd("2018-10-15")+ddays(18)

ymd("2018-10-15")+dyears(10)

ymd_hms("2018-10-15 13:22:34")+dminutes(18)
ymd_hms("2018-10-15 13:22:34")+dhours(18)

hms("18:22:34")+dminutes(18) #沒辦法直接加時間

hms("18:22:34")+minutes(18)
hms("18:22:34")+hours(18)


###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
########## 使用zoo ##########

dates<-as.Date(c("2018-09-05", "2018-09-06", "2018-09-07", 
                 "2018-09-08", "2018-09-09", "2018-09-12", 
                 "2018-09-13", "2018-09-14", "2018-09-15", "2018-09-16"))
cht_daily.zoo<-zoo(ch7sample.exp2, dates)

dates_2<-as.Date(c("2018-09-05"))+0:9
cht_daily_2.zoo<-zoo(ch7sample.exp2, dates_2)

dates_3<-as.Date(c("2018-09-05"))+c(0:4,7:11)
cht_daily_3.zoo<-zoo(ch7sample.exp2, dates_3)

dates_4<-as.Date(c("2018-09-05"))+0:19
cht_daily_4.zoo<-zoo(ch7sample.exp2, dates_4)

time_zoo<-ymd_hms(c("2018-10-12 9:00:00","2018-10-12 9:05:00","2018-10-12 9:10:00",
                    "2018-10-12 9:15:00","2018-10-12 9:20:00","2018-10-12 9:25:00",
                    "2018-10-12 9:30:00","2018-10-12 9:35:00","2018-10-12 9:40:00",
                    "2018-10-12 9:45:00"))
cht.time_zoo<-zoo(ch7sample.exp3, time_zoo)

time_zoo2<-ymd_hms("2018-10-12 9:00:00") + dminutes(seq(0,45,5))
cht.time_zoo2<-zoo(ch7sample.exp3, time_zoo2)

lag(cht_daily_4.zoo,k=+1,na.pad=T)
lag(cht_daily_4.zoo,k=+1,na.pad=T)

diff(cht_daily.zoo, differences = 1)
diff(cht_daily.zoo, differences = 2)
#(2018-09-05 - 2018-09-07), (2018-09-06 - 2018-09-08) 
diff(cht_daily.zoo, lag = 2, differences = 1)

time_zoo3<-ymd_hms("2018-10-12 9:45:00") + dminutes(seq(5,50,5))
cht.time_zoo3<-zoo(ch7sample.exp3, time_zoo3)

merge(cht.time_zoo2,cht.time_zoo3)
merge(cht.time_zoo2,cht.time_zoo3,all=T)

cpi_time<-ymd("1982-01-01")+months(0:434)
"cpi_總指數"<-zoo(ch7sample.exp4,cpi_time)
"cpi_房租"<-zoo(ch7sample.exp5,cpi_time)
"cpi_娛樂費用"<-zoo(ch7sample.exp6,cpi_time)

merge(cpi_總指數,cpi_房租,cpi_娛樂費用)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
########## 使用xts ##########

#dates<-as.Date(c("2018-09-05", "2018-09-06", "2018-09-07", 
#                 "2018-09-08", "2018-09-09", "2018-09-12", 
#                 "2018-09-13", "2018-09-14", "2018-09-15", "2018-09-16"))
cht_daily.xts<-xts(ch7sample.exp2, dates)

#dates_2<-as.Date(c("2018-09-05"))+0:10
cht_daily_2.xts<-xts(ch7sample.exp2, dates_2)

#dates_3<-as.Date(c("2018-09-05"))+c(0:4,7:11)
cht_daily_3.xts<-xts(ch7sample.exp2, dates_3)

#dates_4<-as.Date(c("2018-09-05"))+0:19
cht_daily_4.xts<-xts(ch7sample.exp2, dates_4) #較嚴格

#time_zoo<-ymd_hms(c("2018-10-12 9:00:00","2018-10-12 9:05:00","2018-10-12 9:10:00",
#                    "2018-10-12 9:15:00","2018-10-12 9:20:00","2018-10-12 9:25:00",
#                    "2018-10-12 9:30:00","2018-10-12 9:35:00","2018-10-12 9:40:00",
#                    "2018-10-12 9:45:00"))
cht.time_xts <- xts(ch7sample.exp3, time_zoo) #較嚴格，會檢查時區

#time_zoo2<-ymd_hms("2018-10-12 9:00:00") + dminutes(seq(0,45,5))
cht.time_xts2 <- xts(ch7sample.exp3, time_zoo2)

