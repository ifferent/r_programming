###############################################################################
###############################################################################
####                                                                       ####
#### 完成日期: 2018-07-05                                                  ####
#### 作者：Roddy Hung                                                      ####
#### 版本：V4.0                                                            ####
####                                                                       ####
#### 第4章範例程式:                                                        ####
####    1.基礎繪圖(圖形)文法的概念                                         ####
####    2.美學映射的概念                                                   ####
####        i.基本變數映射設定                                             ####
####       ii.變數映射到fill、color或形狀等的概念                          ####
####      iii.表格轉換成聚集的形式                                         ####
####       iv.複式長條圖                                                   ####
####    3.長條圖                                                           ####
####        i.堆疊長條圖(引數position的使用)                               ####
####       ii.顏色設定                                                     ####
####      iii.外框大小和長條圖寬度的設定                                   ####
####    4.直方圖                                                           ####
####        i.統計轉換的概念                                               ####
####    5.線圖                                                             ####
####    6.散佈圖                                                           ####
####                                                                       ####
###############################################################################
###############################################################################

source("common/check_package.R")#檢查是否有未安裝的套件
source("common/function.R",encoding="utf-8") #將公用自訂函數載起來

###############################################################################
####                                                                       ####
#### 載入套件相關使用函數參考:                                             ####
#### readr: read_csv                                                       ####
#### dplyr: filter,select,rename,mutate                                    ####
#### ggplot2: 略                                                           ####
#### tidyr: gather,spread                                                  ####
#### lubridate: make_date                                                  ####
####                                                                       ####
###############################################################################

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

############################################檔案載入與設定################################################

ch4sample.exp1_path="presentation_sample_ppt/sample_data/family/最近一年內曾因家庭緣故影響工作之情形－按無法加班或無法延長工時分(年齡).csv"
ch4sample.exp1_1_path="presentation_sample_ppt/sample_data/family/最近一年內曾因家庭緣故影響工作之情形－按中斷工作或上班時臨時趕回家分(年齡).csv"
#ch4sample.exp1_2_path="presentation_sample_ppt/sample_data/family/最近一年內曾因家庭緣故影響工作之情形－按無法加班或無法延長工時分(ggplot2).csv"
ch4sample.exp2_path="presentation_sample_ppt/sample_data/myself/ch3_exp1.csv"
ch4sample.exp3_path="presentation_sample_ppt/sample_data/myself/ch3_exp2.csv"
ch4sample.exp4_path="presentation_sample_ppt/sample_data/myself/ch3_exp3.csv"
ch4sample.exp5_path="presentation_sample_ppt/sample_data/消費者物價基本分類暨項目群指數/消費者物價基本分類暨項目群指數.csv"

ch4sample.exp1<-read_csv(ch4sample.exp1_path,col_names = TRUE)
ch4sample.exp1_1<-read_csv(ch4sample.exp1_1_path,col_names=TRUE)
#ch4sample.exp1_2<-read_csv(ch4sample.exp1_2_path,col_names=TRUE)
ch4sample.exp2<-read_csv(ch4sample.exp2_path,col_names=TRUE)
ch4sample.exp3<-read_csv(ch4sample.exp3_path,col_names=TRUE)
ch4sample.exp4<-read_csv(ch4sample.exp4_path,col_names=TRUE)
ch4sample.exp5<-read_csv(ch4sample.exp5_path,col_names=TRUE)

##########################################################################################################
age_range<-c("20－24歲","25－29歲","30－34歲","35－39歲","40－44歲","45－49歲","50－54歲","55－59歲","60－64歲","65歲及以上")
freq_tag<-c("經常","有時","極少","從不")

ggplot(ch4sample.exp1,aes(x=項目別,y=經常))+geom_bar(stat="identity")
ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=項目別))+geom_bar(stat="identity")
ch4sample.exp1_gatable<-gather(ch4sample.exp1,freq_tag,key="頻率",value="計數") #將表格轉成聚集形式
ch4sample.exp1_gatable<-rename(ch4sample.exp1_gatable, "年齡區間"="項目別")

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=計數,fill=頻率))+geom_bar(position="dodge",stat="identity")
ch4sample.exp1_gatable$頻率<-factor(ch4sample.exp1_gatable$頻率,levels=freq_tag)#改變頻率順序
ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=計數,fill=頻率))+geom_bar(position="dodge",stat="identity")

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=計數,fill=頻率))+geom_bar(stat="identity")#沒有position="dodge"

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=計數,fill=頻率))+geom_bar(position="dodge", colour="black"
      ,stat="identity")#colour="black"=>長條圖加黑色邊框

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=計數,fill=頻率))+geom_bar(position="dodge", colour="black"
      ,stat="identity") + scale_fill_brewer(palette="Accent")#scale_fill_brewer(palette="Accent")=>長條圖圖色(調色盤)

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=計數,fill=頻率)) + geom_bar(position="dodge", colour="black"
       ,stat="identity") + scale_fill_brewer(palette="Accent") + xlab("Age Range")

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=計數,fill=頻率)) + geom_bar(position="dodge", colour="black"
       ,stat="identity", size=1, width=1)#size=1, width=1=>外框粗細，與長條圖區間寬度

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=計數,fill=頻率))

######################### Line Graphs #########################################
item.gather<-c("年月","總指數","米類及其製品","肉類","蔬菜")
item.seq<-c("總指數","米類及其製品","肉類","蔬菜")

ch4sample.exp5<-mutate(ch4sample.exp5, "年月" = make_date(年,月))

ggplot(ch4sample.exp5,aes(x=年月,y=總指數))+geom_line()

ch4sample.exp5_gatable<-select(ch4sample.exp5, item.gather)
ch4sample.exp5_gatable<-gather(ch4sample.exp5_gatable,item.seq,key="品項",value="年增率") #將表格轉成聚集形式
ch4sample.exp5_gatable$品項<-factor(ch4sample.exp5_gatable$品項,levels=item.seq)#改變頻率順序

ggplot(ch4sample.exp5_gatable,aes(x=年月,y=年增率,colour=品項))+geom_line()+scale_colour_brewer(palette="Set2")

ggplot(ch4sample.exp5_gatable,aes(x=年月,y=年增率,group=品項))+geom_line(colour="blue",linetype="dashed")

###################################################################################

ggplot(ch4sample.exp5,aes(x=年,y=總指數))+geom_point()

#ggplot(ch4sample.exp5_gatable,aes(x=年,y=年增率,colour=品項))+geom_point()


