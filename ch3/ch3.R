###############################################################################
###############################################################################
####                                                                       ####
#### 完成日期: 2018-06-29                                                  ####
#### 作者：Roddy Hung                                                      ####
#### 版本：V3.2                                                            ####
####                                                                       ####
#### 第3章範例程式:                                                        ####
####    1.繪畫長條圖                                                       ####
####    2.繪畫直方圖                                                       ####
####    3.繪畫散佈圖                                                       ####
####    4.繪圖參數的設定                                                   ####
####    5.低階繪圖函數的使用                                               ####
####    6.繪畫圓形圖                                                       ####
####    7.繪畫扇形圖                                                       ####
####    8.繪畫盒鬚圖                                                       ####
####                                                                       ####
###############################################################################
###############################################################################

source("common/check_package.R")#檢查是否有未安裝的套件

###############################################################################
####                                                                       ####
#### 載入套件相關使用函數參考:                                             ####
#### readr: read_csv                                                       ####
#### dplyr: seleclt                                                        ####
#### plotrix: fan.plot                                                     ####
####                                                                       ####
###############################################################################

library(readr)
library(dplyr)
library(plotrix)

################################檔案載入與設定#################################

ch3sample.exp1_path="ch3/sample_data/最近一年內曾因家庭緣故影響工作之情形－按無法加班或無法延長工時分(年齡).csv"
ch3sample.exp1_1_path="ch3/sample_data/最近一年內曾因家庭緣故影響工作之情形－按中斷工作或上班時臨時趕回家分(年齡).csv"
ch3sample.exp2_path="ch3/sample_data/成績單.csv"
ch3sample.exp3_path="ch3/sample_data/產品銷售額.csv"
#ch3sample.exp4_path="ch3/sample_data/台灣太陽光電發電量統計表.csv"

ch3sample.exp1<-read_csv(ch3sample.exp1_path,col_names = FALSE)
ch3sample.exp1_nocolname<-read_csv(ch3sample.exp1_1_path,col_names=TRUE)
ch3sample.exp1_1<-read_csv(ch3sample.exp1_1_path,col_names=TRUE)
ch3sample.exp2<-read_csv(ch3sample.exp2_path,col_names=TRUE)
ch3sample.exp3<-read_csv(ch3sample.exp3_path,col_names=TRUE)

###############################################################################

age_range<-c("20－24歲","25－29歲","30－34歲","35－39歲","40－44歲","45－49歲","50－54歲","55－59歲","60－64歲","65歲及以上")
freq_tag<-c("經常","有時","極少","從不")
barplot(ch3sample.exp1_nocolname$"經常")
barplot(ch3sample.exp1_nocolname$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形(經常)", names.arg=age_range)

barplot(ch3sample.exp1_nocolname$"有時",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形(有時)", names.arg=age_range)

barplot(ch3sample.exp1_nocolname$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形(極少)", names.arg=age_range)

barplot(ch3sample.exp1_nocolname$"從不",xlab="年齡區間",ylab="計數",horiz=TRUE,main="主計處─因家庭緣故影響無法加班或延長工時之情形(從不)", names.arg=age_range)#放橫的

barplot(ch3sample.exp1_nocolname$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形(經常)", names.arg=age_range, legend=ch3sample.exp1_nocolname$"項目", col=c("gray","green","blue","red","cyan","yellow","pink","magenta","black","aliceblue"))#legend & color 的使用

#############################選出我們需要的列#################################
bind_table<-cbind(ch3sample.exp1_nocolname$"經常",ch3sample.exp1_nocolname$"有時",ch3sample.exp1_nocolname$"極少",ch3sample.exp1_nocolname$"從不")#使用cbind
#也可以使用bind_table2<-ch3sample.exp1_nocolname[,2:5]

bind_table<-select(ch3sample.exp1_nocolname, "經常", "有時", "極少", "從不")#使用select

barplot(t(bind_table),xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形", names.arg=age_range, legend=freq_tag, col=c("gray","green","red","blue"),beside=TRUE) #複式長條圖
#barplot(t(bind_table2),xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形", names.arg=age_range, legend=freq_tag, col=c("gray","green","red","blue"),beside=TRUE) #複式長條圖

barplot(t(bind_table),xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形", names.arg=age_range, legend=freq_tag, col=c("gray","green","red","blue"),beside=FALSE) #堆疊長條圖

###############################################################################
hist(ch3sample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額",breaks=20,col=c("gray","green","red","blue"))#直方圖

hist(ch3sample.exp2$數學,xlab="數學",ylab="計數",main="數學成績",breaks=5)#直方圖

###############################################################################

plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額")#散佈圖

plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額",pch=3)
plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額",pch=6)
plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額",pch=9)
plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額",pch=11)
plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額",pch=12)
plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額",pch=18)
plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額",pch=21)
plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額",pch='*')
plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額",pch='%')

plot(select(ch3sample.exp2,"國文","數學","歷史","地理"))
plot(ch3sample.exp2[,2:5])
plot(select(ch3sample.exp2,"國文","數學","歷史","地理"),xlim=c(0,100),ylim=c(0,100))
plot(ch3sample.exp2[,2:5],xlim=c(0,100),ylim=c(0,100))

###############################################################################

plot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='l',col="red")
plot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='h',col="blue")
plot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='b',col="yellow")
plot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='s',col="green")
plot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='l',col="pink",lwd=3)
plot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='l',col="black",lwd=5)
plot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='l',col="gray30",lwd=2,lty=2)
plot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='l',col="gray50",lwd=5,lty=3,xlim=c(3,5))
plot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='l',col="gray80",lwd=7,lty=4,xlim=c(3,5),ylim=c(550,650))

###############################################################################
par(mfrow=c(2,2))
barplot(ch3sample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)
barplot(ch3sample.exp1_1$"有時",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(有時)", names.arg=age_range)
barplot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)", names.arg=age_range)
barplot(ch3sample.exp1_1$"從不",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(從不)", names.arg=age_range)

par(mfrow=c(4,1))
barplot(ch3sample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)
barplot(ch3sample.exp1_1$"有時",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(有時)", names.arg=age_range)
barplot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)", names.arg=age_range)
barplot(ch3sample.exp1_1$"從不",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(從不)", names.arg=age_range)

par(mfrow=c(1,4))
barplot(ch3sample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)
barplot(ch3sample.exp1_1$"有時",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(有時)", names.arg=age_range)
barplot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)", names.arg=age_range)
barplot(ch3sample.exp1_1$"從不",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(從不)", names.arg=age_range)

par(mfrow=c(3,2))#超過會將前次的圖蓋掉並重新開始
barplot(ch3sample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)
barplot(ch3sample.exp1_1$"有時",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(有時)", names.arg=age_range)
barplot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)", names.arg=age_range)
barplot(ch3sample.exp1_1$"從不",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(從不)", names.arg=age_range)
barplot(ch3sample.exp1_nocolname$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形(經常)", names.arg=age_range)
barplot(ch3sample.exp1_nocolname$"有時",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形(有時)", names.arg=age_range)
barplot(ch3sample.exp1_nocolname$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形(極少)", names.arg=age_range)
barplot(ch3sample.exp1_nocolname$"從不",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形(從不)", names.arg=age_range)

par(mar=c(1,4,4,2)) #(下,左,右,上)
barplot(ch3sample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)
par(mar=c(5,4,10,2)) #(下,左,右,上)
barplot(ch3sample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)
par(mar=c(5,15,9.2,20)) #(下,左,右,上)
barplot(ch3sample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)
par(mai=c(1,1.5,2,2.5)) #(下,左,右,上)
barplot(ch3sample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)

par(mar=c(5,10,9.2,10), mfrow=c(3,2)) #(下,左,右,上)
barplot(ch3sample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)
barplot(ch3sample.exp1_1$"有時",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(有時)", names.arg=age_range)
barplot(ch3sample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)", names.arg=age_range)
barplot(ch3sample.exp1_1$"從不",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(從不)", names.arg=age_range)

par(bg="pink")
barplot(ch3sample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)

par(cex=2)
plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額")

par(cex=1.3, cex.main=0.3)
plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額")

par(cex=1.3, cex.axis=2)
plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額")

par(cex=1.3, cex.lab=2)
plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額")

par(col="red",col.axis="green3",col.main="blue", col.lab="chocolate")
plot(ch3sample.exp3$銷售額,xlab="日",ylab="銷售額",main="公司銷售額")

#引數cex的應用=>氣泡圖
scope_ratio <- ifelse(ch3sample.exp2$數學>=60,ch3sample.exp2$數學/15,ch3sample.exp2$數學/25)#大於等於60分除以15，小於60分除以25，拉大比例
plot(x=ch3sample.exp2$座號,y=ch3sample.exp2$數學,xlab="數學",ylab="計數",main="數學成績",cex=scope_ratio,col="pink",pch=19)
points(x=ch3sample.exp2$座號,y=ch3sample.exp2$數學,cex=scope_ratio)

###############################################################################

plot(ch3sample.exp2[,2:3],xlim=c(0,100),ylim=c(0,100),xlab="",ylab="",main="多樣成績分佈")
points(select(ch3sample.exp2,"數學","歷史"),pch=5,col="red")
points(select(ch3sample.exp2,"數學","地理"),pch=8,col="blue")

grid(lwd=2,col="gray50")

p<-hist(ch3sample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額",breaks=20,col="gray")#直方圖
lines(y=p$counts,x=p$mids,col="red")

plot(5,5,type="n",xlim=c(0,10),ylim=c(0,10))
x<-0:20
abline(a=x,b=1) #y=x
abline(a=x+10,b=-1,col="red")#y=-x+10
abline(v=1:5,col="green")
abline(h=1:5,col="blue")

###############################################################################

ch3sample.exp1.percent<-prop.table(ch3sample.exp1_nocolname$"極少")*100
age_percent<-paste(age_range," ",round(ch3sample.exp1.percent,2),"%",sep="")
pie(ch3sample.exp1.percent,xlab="年齡區間",labels=age_percent,main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",init.angle=0)

fan.plot(round(ch3sample.exp1.percent),labels=age_percent,main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",ticks=200,max.span=9*pi/10)

boxplot(ch3sample.exp3$銷售額)#盒鬚圖
boxplot(ch3sample.exp2[,2:5])#多筆資料的盒鬚圖

###############################################################################










