###############################################################################
###############################################################################
####                                                                       ####
#### 完成日期: 2018-07-13                                                  ####
#### 作者：Roddy Hung                                                      ####
#### 版本：V4.8                                                            ####
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
####    7.Facets                                                           ####
####    8.Scale                                                            ####
####    9.文字輔助標示與版面配置                                           ####
####   10.座標軸設定                                                       ####
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

ch4sample.exp1_path="ch4/sample_data/最近一年內曾因家庭緣故影響工作之情形－按無法加班或無法延長工時分(年齡).csv"
ch4sample.exp1_1_path="ch4/sample_data/最近一年內曾因家庭緣故影響工作之情形－按中斷工作或上班時臨時趕回家分(年齡).csv"
ch4sample.exp2_path="ch4/sample_data/成績單.csv"
ch4sample.exp3_path="ch4/sample_data/消費者物價基本分類暨項目群指數.csv"
ch4sample.exp4_path="ch4/sample_data/台灣太陽光電發電量統計表.csv"

ch4sample.exp1<-read_csv(ch4sample.exp1_path,col_names = TRUE)
ch4sample.exp1_1<-read_csv(ch4sample.exp1_1_path,col_names=TRUE)
ch4sample.exp2<-read_csv(ch4sample.exp2_path,col_names=TRUE)
ch4sample.exp3<-read_csv(ch4sample.exp3_path,col_names=TRUE)
ch4sample.exp4<-read_csv(ch4sample.exp4_path,col_names=TRUE)

############################ aesthetic mapping ################################

ggplot(ch4sample.exp1,aes(x=項目別,y=經常)) + geom_bar(stat="identity")

ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=項目別)) + geom_bar(stat="identity")
ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=有時)) + geom_bar(stat="identity")

############################## bar graphic ####################################

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
#ch4sample.exp1_gatable$頻率<-factor(ch4sample.exp1_gatable$頻率,levels=freq_tag)#改變lengend(頻率)順序另一個方法

############################# fill & colour ###################################
exp1.fill<-ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=人數,fill=頻率))
exp1.colour<-ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=人數,colour=頻率))

exp1.fill + geom_bar(stat="identity",position="stack")#position="stack"

exp1.fill + geom_bar(stat="identity")#default position="stack"

exp1.fill + geom_bar(stat="identity",position="fill")#position="fill"

ggplot(ch4sample.exp1,aes(x=項目別,y=有時,colour=項目別)) + 
    geom_bar(stat="identity",fill="white")

ggplot(ch4sample.exp1,aes(x=項目別,y=有時,colour=有時)) +
    geom_bar(stat="identity",fill="white")

exp1.colour + geom_bar(stat="identity", position="dodge", fill="white")

exp1.colour + geom_bar(stat="identity", position="dodge", fill="white", size=2)

ggplot(ch4sample.exp1,aes(x=項目別,y=有時,colour=有時)) +
    geom_bar(stat="identity",fill="white",width=2)
########################### Histogram #########################################
subject_tag<-c("國文","數學","歷史","地理")
ch4sample.exp2_gatable<-gather(ch4sample.exp2,subject_tag,key="科目",value="分數") #將表格轉成聚集形式

scope.his<-ggplot(ch4sample.exp2,aes(x=國文))
stat_trans_1<-ggplot(ch4sample.exp2,aes(x=國文))
stat_trans_2<-ggplot(ch4sample.exp2_gatable,aes(x=分數,fill=科目))

scope.his + geom_histogram(fill="white", colour="black")
scope.his + geom_histogram(fill="white", colour="black", binwidth=10)

ggplot(ch4sample.exp2_gatable,aes(x=分數,fill=科目)) +
  geom_histogram(position="identity", binwidth=15, alpha=0.3)

stat_trans_1 + geom_bar(stat="bin", bins=5, fill="white", colour="black")

stat_trans_1 + stat_bin(geom="bar", bins=5, fill="white", colour="black")

stat_trans_2 + geom_bar(stat="bin", bins=8, position="identity", alpha=0.3)

stat_trans_2 + stat_bin(geom="bar", bins=8, position="identity", alpha=0.3)

######################### Line Graphs #########################################
item.gather<-c("年月","總指數","米類及其製品","肉類","蔬菜")
item.seq<-c("總指數","米類及其製品","肉類","蔬菜")

ch4sample.exp3<-mutate(ch4sample.exp3, "年月" = make_date(年,月))

ggplot(ch4sample.exp3,aes(x=年月,y=總指數))+geom_line()

ch4sample.exp3_gatable<-select(ch4sample.exp3, item.gather)
ch4sample.exp3_gatable<-gather(ch4sample.exp3_gatable,item.seq,key="品項",value="年增率") #將表格轉成聚集形式

ggplot(ch4sample.exp3_gatable,aes(x=年月,y=年增率,colour=品項)) +
    geom_line() +
        guides(colour=guide_legend(reverse = TRUE)) #改變legend(品項)順序
#ch4sample.exp3_gatable$品項<-factor(ch4sample.exp3_gatable$品項,levels=item.seq)#改變legend(品項)順序另一個方法

ggplot(ch4sample.exp3_gatable,aes(x=年月,y=年增率,colour=品項,linetype=品項)) +
    geom_line() +
        guides(colour=guide_legend(reverse = TRUE), linetype=guide_legend(reverse = TRUE))

ggplot(ch4sample.exp3_gatable,aes(x=年月,y=年增率,group=品項)) +
    geom_line(colour="blue",linetype="dashed")


############################# Scatter Graphs ######################################

ch4sample.exp4<-mutate(ch4sample.exp4, "年月" = make_date(年度,月份))
#將表格轉成分散形式來表示"平均單位裝置容量每日發電量"與"各光電站的關係"
ch4sample.exp4_sprtable.perday<-spread(select(ch4sample.exp4, "年月", "光電站名稱", "平均單位裝置容量每日發電量"),key="光電站名稱", value="平均單位裝置容量每日發電量")
#將表格轉成分散形式來表示"發電量(度)"與"各光電站的關係"
ch4sample.exp4_sprtable.total<-spread(select(ch4sample.exp4, "年月", "光電站名稱", "發電量(度)"),key="光電站名稱", value="發電量(度)")

ch4sample.exp4_ponhu<-filter(ch4sample.exp4, 光電站名稱== "澎湖光電")
ggplot(ch4sample.exp4_ponhu,aes(x=年月, y=平均單位裝置容量每日發電量)) +
    geom_point()#使用filter來將表格抽離
ggplot(ch4sample.exp4_sprtable.perday,aes(x=年月, y=澎湖光電)) +
    geom_point()#使用分散表示法，這個的優點是可以在美學映射中較好知道程式要講甚麼
ggplot(ch4sample.exp4_ponhu,aes(x=年月, y=`發電量(度)`)) +
    geom_point()#使用filter來將表格抽離
ggplot(ch4sample.exp4_sprtable.total,aes(x=年月, y=澎湖光電)) +
    geom_point()#使用分散表示法

###################################################################################

ggplot(ch4sample.exp4,aes(x=年月, y=平均單位裝置容量每日發電量, colour=光電站名稱)) +
    geom_point()

ch4sample.exp4$年月<-factor(ch4sample.exp4$年月)
ggplot(ch4sample.exp4,aes(x=年月, y=平均單位裝置容量每日發電量, colour=光電站名稱,group=1)) +
    geom_point()

ggplot(ch4sample.exp4,aes(x=年月, y=平均單位裝置容量每日發電量, colour=光電站名稱, shape=光電站名稱)) +
    geom_point()

###################################################################################

ggplot(ch4sample.exp4_sprtable.perday, aes(x=年月, y=澎湖光電)) +
    geom_line() +
        geom_point(size=4,shape=22,fill="pink")

ggplot(ch4sample.exp4_sprtable.perday) +
    geom_line(aes(x=年月, y=澎湖光電))+
        geom_line(aes(x=年月, y=七美光電)) +
            geom_point(aes(x=年月, y=澎湖光電)) +
                geom_point(aes(x=年月, y=七美光電),size=4,shape=22,fill="pink")

ggplot(ch4sample.exp4_sprtable.perday, aes(x=年月, y=澎湖光電)) +
    geom_line()+
        geom_line(aes(x=年月, y=七美光電)) +
            geom_point() +
                geom_point(aes(x=年月, y=七美光電),size=4,shape=22,fill="pink")

################################## Facets #####################################

#橫軸顯示
ggplot(ch4sample.exp2_gatable,aes(x=學號,y=分數,colour=科目)) +
    geom_point() +
        facet_grid(科目~.)

#縱軸顯示
ggplot(ch4sample.exp2_gatable,aes(x=學號,y=分數,colour=科目)) +
    geom_point() +
        facet_grid(.~科目)

#非aesthetic映射的變數
ggplot(ch4sample.exp2_gatable,aes(x=學號,y=分數,colour=科目)) +
    geom_point() +
        facet_grid(.~性別)

#雙變數
ggplot(ch4sample.exp2_gatable,aes(x=學號,y=分數,colour=科目)) +
    geom_point() +
        facet_grid(班級~性別)

#雙變數以上
ggplot(ch4sample.exp2_gatable,aes(x=學號,y=分數,colour=科目)) +
    geom_point() +
        facet_grid(班級+性別~科目)

#grid & wrap的比較
ggplot(ch4sample.exp4,aes(x=光電站名稱, y=平均單位裝置容量每日發電量, colour=光電站名稱)) +
    geom_point() +
        geom_line() +
            facet_grid(.~月份) + 
                scale_x_discrete(labels=NULL, breaks=NULL) +
                    scale_y_continuous(breaks=seq(0.5,5.2,0.5))

ggplot(ch4sample.exp4,aes(x=光電站名稱, y=平均單位裝置容量每日發電量, colour=光電站名稱)) +
    geom_point() +
        geom_line() +
            facet_wrap(~月份) + 
                scale_x_discrete(labels=NULL, breaks=NULL) +
                    scale_y_continuous(breaks=seq(0.5,5.5,0.5))

#使用wrap更改row和column
ggplot(ch4sample.exp4,aes(x=光電站名稱, y=平均單位裝置容量每日發電量, colour=光電站名稱)) +
    geom_point() +
        geom_line() +
            facet_wrap(~月份,nrow=4) + 
                scale_x_discrete(labels=NULL, breaks=NULL) +
                    scale_y_continuous(breaks=seq(0.5,5.5,0.5))

ggplot(ch4sample.exp4,aes(x=光電站名稱, y=平均單位裝置容量每日發電量, colour=光電站名稱)) +
    geom_point() +
        geom_line() +
            facet_wrap(~月份,ncol=5,nrow=3) +
                scale_x_discrete(labels=NULL, breaks=NULL) +
                    scale_y_continuous(breaks=seq(0.5,5.5,0.5))


ch4sample.exp2_matrix<-m2gg.scope(ch4sample.exp2) #轉換表格符合散佈矩矩陣圖

#改變排列順序為"國文","數學","歷史","地理"
ch4sample.exp2_matrix$科目1<-factor(ch4sample.exp2_matrix$科目1,levels=c("國文","數學","歷史","地理"))
ch4sample.exp2_matrix$科目2<-factor(ch4sample.exp2_matrix$科目2,levels=c("國文","數學","歷史","地理"))

ggplot(ch4sample.exp2_matrix,aes(x=分數1,y=分數2)) +
    geom_point() +
        facet_grid(科目1~科目2)

ggplot(ch4sample.exp2_matrix,aes(x=分數1,y=性別,colour=班級)) +
    geom_point() +
        facet_grid(科目1~科目2)

ggplot(ch4sample.exp2_matrix,aes(x=分數1,y=性別,colour=班級)) +
    geom_point() +
        facet_grid(科目1~科目2+性別)

ggplot(ch4sample.exp2_matrix,aes(x=分數1,y=分數2,colour=性別)) +
    geom_point() +
        facet_wrap(科目1~科目2)

ggplot(ch4sample.exp2_matrix,aes(x=分數1,y=分數2,colour=性別)) +
    geom_point() +
        facet_wrap(性別~科目2)

################################## Scales #####################################

ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=項目別)) + 
    geom_bar(stat="identity") #離散變數
ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=有時)) + 
    geom_bar(stat="identity") #連續變數 

ggplot(ch4sample.exp4,aes(x=年月, y=平均單位裝置容量每日發電量, colour=光電站名稱)) +
    geom_point() +
    scale_colour_brewer(palette = "Set3")

#gradient是連續變數，所以使用在離散變數上會出現錯誤
ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=項目別)) + 
    geom_bar(stat="identity") +
        scale_fill_gradient(low="#00ff00", high="#ff99cc") 
ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=有時)) + 
    geom_bar(stat="identity") +
        scale_fill_gradient(low="#00ff00", high="#ff99cc")

ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=項目別,colour=項目別)) + 
    geom_bar(stat="identity") +
        scale_fill_hue(h=c(0,360),c=50,l=80)

ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=項目別)) + 
    geom_bar(stat="identity") +
        scale_fill_grey(start=0.2,end=0.8)

ggplot(ch4sample.exp4,aes(x=光電站名稱, y=平均單位裝置容量每日發電量, colour=光電站名稱)) +
    geom_point() +
        scale_colour_hue(breaks=c("七美光電","澎湖光電","金門光電")) + 
            scale_y_continuous(limits=c(2,2.5),breaks=seq(2,2.5,0.01))+
            scale_x_discrete(limits=c("七美光電","澎湖光電","金門光電"))

ggplot(ch4sample.exp4,aes(x=光電站名稱, y=平均單位裝置容量每日發電量, colour=光電站名稱)) +
    geom_point() +
        scale_colour_hue(h=c(0,360),c=50,l=80,breaks=c("七美光電","澎湖光電","金門光電")) + 
        scale_x_discrete(limits=c("七美光電","澎湖光電","金門光電"), breaks=c("七美光電","澎湖光電"))
#反轉y軸
ggplot(ch4sample.exp3_gatable,aes(x=年月,y=年增率,colour=品項)) +
    geom_line() +
        scale_y_reverse()

#y軸以log10為刻度
ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=人數,fill=頻率)) +
    geom_bar(position="dodge",stat="identity") +
        guides(colour=guide_legend(reverse = TRUE)) +
            scale_y_log10()

ggplot(ch4sample.exp3_gatable,aes(x=年月,y=年增率,colour=品項,linetype=品項)) +
        geom_line() +
            guides(colour=guide_legend(reverse = TRUE),
                   linetype=guide_legend(reverse = TRUE)) +
                scale_linetype_manual(values=c(5:8))
             
ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=人數,fill=頻率)) +
    geom_bar(position="dodge",stat="identity") + 
        scale_fill_manual(values=c("magenta2","#7e9f68","chocolate4","cyan2"))

ggplot(ch4sample.exp4,aes(x=年月, y=平均單位裝置容量每日發電量, colour=光電站名稱, shape=光電站名稱)) +
    geom_point() +
        scale_shape_manual(values=c(0:15))

ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=有時)) + 
    geom_bar(stat="identity") +
        scale_fill_gradient(name="統計人數",labels=seq(0,350,30),
                            breaks=seq(0,350,30),low="#00ff00", high="#ff99cc")

#缺失值設定
item.gather.traffic<-c("年月","總指數","交通工具","油料費","汽車","機車")
item.seq.traffic<-c("總指數","交通工具","油料費","汽車","機車")

ch4sample.exp3_gatable.traffic<-select(ch4sample.exp3, item.gather.traffic)
ch4sample.exp3_gatable.traffic<-gather(ch4sample.exp3_gatable.traffic,item.seq.traffic,key="品項",value="年增率") #將表格轉成聚集形式
ch4sample.exp3_gatable.traffic$品項<-factor(ch4sample.exp3_gatable.traffic$品項,levels=item.seq.traffic)

ggplot(ch4sample.exp3_gatable.traffic,aes(x=年月,y=年增率,colour=品項)) +
    geom_line() +
        scale_x_date(limits=c(as.Date("2010-01-01","%Y-%m-%d"),as.Date("2018-01-01","%Y-%m-%d"))) +
        scale_y_continuous(na.value = 0,limits=c(-10,10))

############################# Annotation & Layout #############################

ggplot(ch4sample.exp3,aes(x=年月,y=總指數)) +
    geom_line(colour="#ec8092") +
        labs(
              title = "消費者物價基本分類暨項目群指數",
              subtitle = "總指數",
              y="年增率",
              caption = "主計處"
            )


data_we_want<-filter(group_by(ch4sample.exp4,光電站名稱),
                     (光電站名稱=="七美光電")|(光電站名稱=="澎湖光電")|(光電站名稱=="金門光電"))         

ggplot(ch4sample.exp4,aes(x=年月, y=平均單位裝置容量每日發電量)) +
    geom_point(aes(colour=光電站名稱),data=data_we_want) +
        geom_text(aes(label=光電站名稱,colour=光電站名稱), data=data_we_want, show.legend=F)

ggplot(ch4sample.exp4,aes(x=年月, y=平均單位裝置容量每日發電量)) +
    geom_point(aes(colour=光電站名稱)) +
        geom_text(aes(label=光電站名稱,colour=光電站名稱),
                  data=data_we_want, show.legend=F, nudge_y=0.1) +
        scale_colour_hue(limits=c("七美光電","澎湖光電","金門光電"),
                         breaks=c("七美光電","澎湖光電","金門光電"))

ggplot(ch4sample.exp4,aes(x=年月, y=平均單位裝置容量每日發電量)) +
      geom_point(aes(colour=光電站名稱),data=data_we_want) +
      geom_label(aes(label=光電站名稱,colour=光電站名稱),
                 data=data_we_want,show.legend=F,nudge_y=0.1,alpha=0.5)

ggplot(ch4sample.exp3_gatable,aes(x=年月,y=年增率,colour=品項)) +
     geom_line() +
         guides(colour=guide_legend(reverse = TRUE)) +
             annotate(
                      "rect", xmin=as.Date("2010-01-01","%Y-%m-%d"),
                      xmax=as.Date("2016-12-01","%Y-%m-%d"),
                      ymin=-50, ymax=85, fill="#cc00ff", alpha=0.3
                     ) +
             annotate(
                      "segment", x=as.Date("2008-05-01","%Y-%m-%d"),
                      xend=as.Date("2016-02-01","%Y-%m-%d"),
                      y=90, yend=80, color="#006600", 
                      alpha=0.8, size=1.2, arrow=arrow()
                     )

ggplot(ch4sample.exp1_gatable,aes(x=年齡區間,y=人數,fill=頻率)) +
      geom_bar(position="dodge",stat="identity") +
          guides(colour=guide_legend(reverse = TRUE)) +
          theme(
                legend.position = "top",
                legend.background = element_rect(colour="green",size=1.2),
                legend.text = element_text(colour="blue"),
                panel.grid.major = element_line(colour="red"),
                panel.grid.minor = element_line(colour="red",
                                                linetype="dashed", size=0.3)
               )

ggplot(ch4sample.exp2_matrix,aes(x=分數1,y=分數2)) +
        geom_point() +
            facet_grid(科目1~科目2) +
                labs(title="三班成績散佈矩陣圖") +
                    theme(
                          strip.background = element_rect(fill="pink2"),
                          strip.text = element_text(size=14,face="bold"),
                          strip.text.y = element_text(angle=0),
                          plot.title = element_text(color="#cc3300",
                                                    size=22, hjust=0.5)
                         )

############################# Coordinate System ###############################

ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=項目別)) + 
    geom_bar(stat="identity") + 
        coord_flip()

#改為極座標
ggplot(ch4sample.exp1,aes(x=項目別,y=有時,fill=項目別)) + 
    geom_bar(stat="identity",width=1) + 
        coord_polar(theta = "x")

#改為圓餅圖
y.breaks <- cumsum(rev(ch4sample.exp1$有時)) - rev(ch4sample.exp1$有時)/2
y.labels <- paste(scales::percent(rev(ch4sample.exp1$有時)/sum(rev(ch4sample.exp1$有時))))

ggplot(ch4sample.exp1,aes(x=1,y=有時,fill=項目別)) + 
    geom_bar(stat="identity") + 
        coord_polar(theta = "y") + 
            scale_y_continuous(breaks=y.breaks,labels=y.labels)

#心臟線
x<-seq(0,2*pi,2*pi/1000)
heart_line<-tibble(
        theta = x,
        cardioid = 2*(0.25-cos(theta))
      )

ggplot(heart_line,aes(x=theta,y=cardioid)) + 
    geom_line() + 
        coord_polar()

#玫瑰線
rose_line<-tibble(
    theta = x,
    rose = cos(9*theta)
    )

ggplot(rose_line,aes(x=theta,y=rose)) + 
    geom_line() + 
        coord_polar()

############################### Other Graphic #################################
ggplot(ch4sample.exp4,aes(x=光電站名稱, y=平均單位裝置容量每日發電量, fill=光電站名稱)) + 
    geom_boxplot()
ggplot(ch4sample.exp3_gatable,aes(x=品項,y=年增率,colour=品項)) + 
    geom_boxplot() +
        guides(colour=guide_legend(reverse = TRUE))



