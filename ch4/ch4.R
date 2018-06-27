library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

############################################檔案載入與設定################################################

ch4sample.exp1_path="ch4/sample_data/最近一年內曾因家庭緣故影響工作之情形－按無法加班或無法延長工時分(年齡).csv"
ch4sample.exp1_1_path="ch4/sample_data/最近一年內曾因家庭緣故影響工作之情形－按中斷工作或上班時臨時趕回家分(年齡).csv"
ch4sample.exp2_path="ch4/sample_data/消費者物價基本分類暨項目群指數.csv"
ch4sample.exp3_path="ch4/sample_data/台灣太陽光電發電量統計表.csv"

ch4sample.exp1<-read_csv(ch4sample.exp1_path,col_names = TRUE)
ch4sample.exp1_1<-read_csv(ch4sample.exp1_1_path,col_names=TRUE)
ch4sample.exp2<-read_csv(ch4sample.exp2_path,col_names=TRUE)
ch4sample.exp3<-read_csv(ch4sample.exp3_path,col_names=TRUE)

##########################################################################################################

ggplot(ch4sample.exp1,aes(x=項目別,y=經常)) + geom_bar(stat="identity")
ggplot(ch4sample.exp1,aes(x=項目別,y=經常)) + stat_identity(geom="bar")


ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=項目別)) + geom_bar(stat="identity")
ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=有時)) + geom_bar(stat="identity")

###############################################################################

freq_tag<-c("經常","有時","極少","從不")

ch4sample.exp1_gatable<-gather(ch4sample.exp1,freq_tag,key="頻率",value="人數") #將表格轉成聚集形式
ch4sample.exp1_gatable<-rename(ch4sample.exp1_gatable, "年齡區間"="項目別")

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=人數,fill=頻率)) +
    geom_bar(position="dodge",stat="identity")

ggplot(ch4sample.exp1_gatable,aes(x=頻率,y=人數,fill=年齡區間)) +
  geom_bar(position="dodge",stat="identity")

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=人數,fill=頻率)) +
    geom_bar(position="dodge",stat="identity") + 
        guides(fill=guide_legend(reverse = TRUE)) #改變legend(頻率)順序
#ch4sample.exp1_gatable$頻率<-factor(ch4sample.exp1_gatable$頻率,levels=freq_tag)#改變頻率順序另一個方法

###############################################################################

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=人數,fill=頻率)) +
    geom_bar(stat="identity",position="stack")#position="stack"

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=人數,fill=頻率)) +
    geom_bar(stat="identity")#default position="stack"

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=人數,fill=頻率)) +
    geom_bar(stat="identity",position="fill")#position="fill"

ggplot(ch4sample.exp1,aes(x=項目別,y=有時,colour=項目別)) +
    geom_bar(stat="identity",fill="white")

ggplot(ch4sample.exp1,aes(x=項目別,y=有時,colour=有時)) +
    geom_bar(stat="identity",fill="white")

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=人數,colour=頻率)) +
    geom_bar(stat="identity", position="dodge", fill="white")

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=人數,colour=頻率)) +
    geom_bar(stat="identity", position="dodge", fill="white", size=2)

ggplot(ch4sample.exp1,aes(x=項目別,y=有時,colour=有時)) +
    geom_bar(stat="identity",fill="white",width=2)

######################### Line Graphs #########################################
item.gather<-c("年月","總指數","米類及其製品","肉類","蔬菜")
item.seq<-c("總指數","米類及其製品","肉類","蔬菜")

ch4sample.exp2<-mutate(ch4sample.exp2, "年月" = make_date(年,月))

ggplot(ch4sample.exp2,aes(x=年月,y=總指數))+geom_line()

ch4sample.exp2_gatable<-select(ch4sample.exp2, item.gather)
ch4sample.exp2_gatable<-gather(ch4sample.exp2_gatable,item.seq,key="品項",value="年增率") #將表格轉成聚集形式
ch4sample.exp2_gatable$品項<-factor(ch4sample.exp2_gatable$品項,levels=item.seq)#改變頻率順序

ggplot(ch4sample.exp2_gatable,aes(x=年月,y=年增率,colour=品項))+geom_line()+scale_colour_brewer(palette="Set2")

ggplot(ch4sample.exp2_gatable,aes(x=年月,y=年增率,group=品項))+geom_line(colour="blue",linetype="dashed")

###################################################################################

ggplot(ch4sample.exp2,aes(x=年,y=總指數))+geom_point()
