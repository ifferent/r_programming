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

ch7sample.exp1<-read_csv(ch7sample.exp1_path, col_names=TRUE)
ch7sample.exp2<-read_csv(ch7sample.exp2_path, col_names=TRUE)
ch7sample.exp3<-read_csv(ch7sample.exp3_path, col_names=TRUE)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 時間與日期的建立與處理
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
#### 時間與日期的算數

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
