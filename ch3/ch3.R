library(readr)
library(dplyr)
library(plotrix)

############################################檔案載入與設定################################################

mysample.exp1_path="ch3/sample_data/最近一年內曾因家庭緣故影響工作之情形－按無法加班或無法延長工時分(年齡).csv"
mysample.exp1_1_path="ch3/sample_data/最近一年內曾因家庭緣故影響工作之情形－按中斷工作或上班時臨時趕回家分(年齡).csv"
mysample.exp2_path="ch3/sample_data/成績單.csv"
mysample.exp3_path="ch3/sample_data/產品銷售額.csv"

mysample.exp1<-read_csv(mysample.exp1_path,col_names = FALSE)
mysample.exp1_nocolname<-read_csv(mysample.exp1_1_path,col_names=TRUE)
mysample.exp1_1<-read_csv(mysample.exp1_1_path,col_names=TRUE)
mysample.exp2<-read_csv(mysample.exp2_path,col_names=TRUE)
mysample.exp3<-read_csv(mysample.exp3_path,col_names=TRUE)

###############################################################################

age_range<-c("20－24歲","25－29歲","30－34歲","35－39歲","40－44歲","45－49歲","50－54歲","55－59歲","60－64歲","65歲及以上")
freq_tag<-c("經常","有時","極少","從不")
barplot(mysample.exp1_nocolname$"經常")
barplot(mysample.exp1_nocolname$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形(經常)", names.arg=age_range)

barplot(mysample.exp1_nocolname$"有時",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形(有時)", names.arg=age_range)

barplot(mysample.exp1_nocolname$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形(極少)", names.arg=age_range)

barplot(mysample.exp1_nocolname$"從不",xlab="年齡區間",ylab="計數",horiz=TRUE,main="主計處─因家庭緣故影響無法加班或延長工時之情形(從不)", names.arg=age_range)#放橫的

barplot(mysample.exp1_nocolname$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形(經常)", names.arg=age_range, legend=mysample.exp1_nocolname$"項目", col=c("gray","green","blue","red","cyan","yellow","pink","magenta","black","aliceblue"))#legend & color 的使用

#######################選出我們需要的列#######################
bind_table<-cbind(mysample.exp1_nocolname$"經常",mysample.exp1_nocolname$"有時",mysample.exp1_nocolname$"極少",mysample.exp1_nocolname$"從不")#使用cbind

bind_table2<-select(mysample.exp1_nocolname, "經常", "有時", "極少", "從不")#使用select

barplot(t(bind_table),xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形", names.arg=age_range, legend=freq_tag, col=c("gray","green","red","blue"),beside=TRUE) #複式長條圖

barplot(t(bind_table2),xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形", names.arg=age_range, legend=freq_tag, col=c("gray","green","red","blue"),beside=TRUE) #複式長條圖

barplot(t(bind_table),xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故影響無法加班或延長工時之情形", names.arg=age_range, legend=freq_tag, col=c("gray","green","red","blue"),beside=FALSE) #堆疊長條圖

#####################################################################
hist(mysample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額",breaks=20,col=c("gray","green","red","blue"))

hist(mysample.exp2$數學,xlab="數學",ylab="計數",main="數學成績",breaks=5)

###############################################################################
par(mfrow=c(2,2))
barplot(mysample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)
barplot(mysample.exp1_1$"有時",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(有時)", names.arg=age_range)
barplot(mysample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)", names.arg=age_range)
barplot(mysample.exp1_1$"從不",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(從不)", names.arg=age_range)

par(mfrow=c(4,1))
barplot(mysample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)
barplot(mysample.exp1_1$"有時",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(有時)", names.arg=age_range)
barplot(mysample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)", names.arg=age_range)
barplot(mysample.exp1_1$"從不",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(從不)", names.arg=age_range)

par(mfrow=c(1,4))
barplot(mysample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)
barplot(mysample.exp1_1$"有時",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(有時)", names.arg=age_range)
barplot(mysample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)", names.arg=age_range)
barplot(mysample.exp1_1$"從不",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(從不)", names.arg=age_range)

par(mfrow=c(3,2))
barplot(mysample.exp1_1$"經常",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(經常)", names.arg=age_range)
barplot(mysample.exp1_1$"有時",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(有時)", names.arg=age_range)
barplot(mysample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)", names.arg=age_range)
barplot(mysample.exp1_1$"從不",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(從不)", names.arg=age_range)


hist(mysample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額",breaks=50)

##########################################################################################################
par(mfrow=c(1,1))
mysample.exp1.percent<-prop.table(mysample.exp1_nocolname$"極少")*100
age_percent<-paste(age_range," ",round(mysample.exp1.percent,2),"%",sep="")
pie(mysample.exp1.percent,xlab="年齡區間",labels=age_percent,main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",init.angle=0)

fan.plot(round(mysample.exp1.percent),labels=age_percent,main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",ticks=500)

##########################################################################################################

boxplot(mysample.exp3$銷售額)

#########################################################################################################

plot(mysample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額")#散佈圖
par(mfrow=c(3,3))
plot(mysample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額",pch=3)
plot(mysample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額",pch=6)
plot(mysample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額",pch=9)
plot(mysample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額",pch=11)
plot(mysample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額",pch=12)
plot(mysample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額",pch=18)
plot(mysample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額",pch=19)
plot(mysample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額",pch='*')
plot(mysample.exp3$銷售額,xlab="銷售額",ylab="計數",main="公司銷售額",pch='%')


#########################################################################################################

plot(mysample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='l',col="red")
plot(mysample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='h',col="blue")
plot(mysample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='b',col="yellow")
plot(mysample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='s',col="green")
plot(mysample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='l',col="pink",lwd=3)
plot(mysample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='l',col="black",lwd=5)
plot(mysample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='l',col="gray30",lwd=2,lty=2)
plot(mysample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='l',col="gray50",lwd=5,lty=3,xlim=c(3,5))
plot(mysample.exp1_1$"極少",xlab="年齡區間",ylab="計數",main="主計處─因家庭緣故中斷工作或上班時臨時趕回家(極少)",type='l',col="gray80",lwd=7,lty=4,xlim=c(3,5),ylim=c(550,650))











