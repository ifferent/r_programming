###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
###                                                                       ###
### 完成日期: 2018-09-25                                                  ###
### 作者：Roddy Hung                                                      ###
### 版本：V0.5                                                            ###
###                                                                       ###
### 第6章範例程式:                                                        ###
###    1.
###                                                                       ###
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

source("common/check_package.R")#檢查是否有未安裝的套件
source("common/function.R",encoding="utf-8") #將公用自訂函數載起來

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
###                                                                       ###
### 載入套件相關使用函數參考:                                             ###
### readr: read_csv                                                       ###
### dplyr: filter,select,rename,mutate                                    ###
### ggplot2: 略                                                           ###
### tibble: tibble, tirbble                                               ###
###                                                                       ###
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(agricolae)
library(ggplot2)

################################檔案載入與設定#################################

ch6sample.exp1_path="ch6/sample_data/基金投報率.csv"
ch6sample.exp2_path="ch6/sample_data/產品製程改善.csv"
ch6sample.exp3_path="ch6/sample_data/披薩店銷售額.csv"
ch6sample.exp4_path="ch6/sample_data/貨運公司資料.csv"
ch6sample.exp5_path="ch6/sample_data/維修紀錄.csv"
ch6sample.exp6_path="ch6/sample_data/北市國營業稅資料.csv"
ch6sample.exp7_path="ch6/sample_data/某公司銷售預測.csv"
ch6sample.exp8_path="ch6/sample_data/百貨公司信用卡.csv"

ch6sample.exp1<-read_csv(ch6sample.exp1_path, col_names=TRUE)
ch6sample.exp2<-read_csv(ch6sample.exp2_path, col_names=TRUE)
ch6sample.exp3<-read_csv(ch6sample.exp3_path, col_names=TRUE)
ch6sample.exp4<-read_csv(ch6sample.exp4_path, col_names=TRUE)
ch6sample.exp5<-read_csv(ch6sample.exp5_path, col_names=TRUE)
ch6sample.exp6<-read_csv(ch6sample.exp6_path, col_names=TRUE)
ch6sample.exp7<-read_csv(ch6sample.exp7_path, col_names=TRUE)
ch6sample.exp8<-read_csv(ch6sample.exp8_path, col_names=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########單一母體變異數推論##########
ch6sample.exp1
quarter<-c("第一季","第二季","第三季","第四季")
quarter_return<-gather(ch6sample.exp1,quarter,key="季",value="投報率")
mean(quarter_return$投報率)
sd(quarter_return$投報率)
v<-var(quarter_return$投報率)
n<-length(quarter_return$投報率)
up_interval<-sqrt(((n-1)*v)/qchisq(0.025, df=n-1, lower.tail=F))
down_interval<-sqrt(((n-1)*v)/qchisq(0.025, df=n-1))
#  變異數區間: 4.67<=6.008<=8.428 
ggplot(quarter_return,aes(x=季,y=投報率,fill=季)) +
    geom_boxplot()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########單一母體變異數檢定##########
#變異數20ppm =>0.00002
sample_var<-0.000029
sigma_var <-0.00002
n<-30
chi_sat<-((n-1)*sample_var)/sigma_var
pchisq(chi_sat,df=n-1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########兩母體變異數檢定##########
mach1<-ch6sample.exp2$機器1
mach2<-ch6sample.exp2$機器2
mach1_var<-var(ch6sample.exp2$機器1);mach1_var
mach2_var<-var(ch6sample.exp2$機器2,na.rm=T);mach2_var

var.test(mach1,mach2,alternative="two.sided")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########變異數分析(ANOVA)##########
Method_A<-sample(rnorm(1000,mean=62,sd=15),30)
Method_B<-sample(rnorm(1000,mean=60,sd=21),30)
Method_C<-sample(rnorm(1000,mean=68,sd=23),30)
Method_D<-sample(rnorm(1000,mean=59,sd=18),30)

method_est1<-tibble(
    "A"=Method_A,
    "B"=Method_B,
    "C"=Method_C,
    "D"=Method_D
)

method_est<-gather(method_est1, names(method_est1), key="方法", value="組裝時間")

method_est.aov<-aov(組裝時間~方法,data=method_est);method_est.aov
summary(method_est.aov)
ggplot(method_est,aes(x=方法,y=組裝時間,fill=方法)) +
    geom_boxplot()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########多重比較程序##########
LSD_resault<-LSD.test(method_est.aov, "方法", 
                      DFerror=116, MSerror=356.1); LSD_resault

pairwise.t.test(method_est$"組裝時間", method_est$"方法")#兩兩均值比較
TukeyHSD(method_est.aov)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########繪製Q-Q 圖##########
#@@@ base 系統
par(mfrow=c(2,2), cex=0.8, cex.main=0.7)
qqnorm(method_est1$A, main="Method A",col="red")
qqline(method_est1$A,col="red")
qqnorm(method_est1$B, main="Method B",col="blue")
qqline(method_est1$B,col="blue")
qqnorm(method_est1$C, main="Method C",col="green")
qqline(method_est1$C,col="green")
qqnorm(method_est1$D, main="Method D")
qqline(method_est1$D)

#@@@ ggplot2 系統
ggplot(method_est1,aes(sample=A)) + 
    geom_qq() + geom_qq_line(colour="blue", size=0.8)

ggplot(method_est.aov,aes(sample=組裝時間,colour=方法)) + 
    geom_qq() + geom_qq_line(size=0.8) +
    facet_wrap(~方法)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########簡單線性迴歸##########
reg_y<-ch6sample.exp3$`銷售額(季)`
reg_x<-ch6sample.exp3$`學生人數(千人)`
regress_data<-lm(reg_y~reg_x);regress_data
summary(regress_data)

#@@@ 信賴區間
predict(regress_data,interval="confidence",level=0.95)

#@@@ 預測
pred_data<- tibble(
    reg_x=c(3,5,7,11,20)
)
predict(regress_data,data.frame(reg_x=10),interval="confidence",level=0.95)
predict(regress_data,newdata=pred_data,interval="prediction",level=0.95)

#@@@ base 系統
par(mfrow=c(1,2))
plot(reg_y~reg_x,data=ch6sample.exp3)
abline(regress_data,col="red")
plot(regress_data,which=1)

#@@@ ggplot2 系統
ggplot(ch6sample.exp3,aes(x=`學生人數(千人)`,y=`銷售額(季)`)) + 
    geom_point(size=2,colour="blue") +
    geom_abline(intercept=regress_data$coefficients[1],
                slope=regress_data$coefficients[2],colour="red")

ggplot(ch6sample.exp3,aes(x=`學生人數(千人)`,y=`銷售額(季)`)) + 
    geom_point(size=2,colour="blue") +
    geom_smooth(method=lm, formula=y~x, colour="red")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########複迴歸##########
dri_y<-ch6sample.exp4$行駛時間
dri_x1<-ch6sample.exp4$行駛哩程數
dri_x2<-ch6sample.exp4$送貨批數
    
dri_data<-lm(dri_y~dri_x1+dri_x2);dri_data
summary(dri_data)
anova(dri_data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########虛擬變數##########
ch6sample.exp5$維修類型<-ifelse(ch6sample.exp5$"維修類型"=="機電型", 1, 0)
fix_recoder_y<-ch6sample.exp5$"維修所需時間"
fix_recoder_x1<-ch6sample.exp5$`距離上次叫修時間(月)`
fix_recoder_x2<-ch6sample.exp5$"維修類型"
fix_recoder_data<-lm(fix_recoder_y~fix_recoder_x1+fix_recoder_x2);fix_recoder_data

ggplot(ch6sample.exp5,aes(x=`距離上次叫修時間(月)`,y=維修所需時間,colour=factor(維修類型))) + 
    geom_point(size=2, position="jitter") +
    geom_abline(intercept=fix_recoder_data$coefficients[1],
                slope=fix_recoder_data$coefficients[2],colour="red") +
    geom_abline(intercept=fix_recoder_data$coefficients[1]+fix_recoder_data$coefficients[3],
                slope=fix_recoder_data$coefficients[2],colour="green") +
    scale_colour_discrete(name="維修類型", labels=c("機械型","機電型"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########非線性變數##########
taipei_x<-ch6sample.exp6$`公司家數(千家)`
taipei_y<-ch6sample.exp6$`營業稅年度總稅收(百萬元)`
taipei_busi_tax<-lm(taipei_y~taipei_x+I(taipei_x^2))
summary(taipei_busi_tax)
pre_data<-data.frame(taipei_x=c(169:173))
predict(taipei_busi_tax,newdata=pre_data,interval="prediction")

plot(taipei_y~taipei_x,data=ch6sample.exp6)
curve(taipei_busi_tax,col="red")

ggplot(ch6sample.exp6,aes(x=`公司家數(千家)`,y=`營業稅年度總稅收(百萬元)`,colour=factor(year)))+
  geom_point()+geom_smooth(colour="red",method=lm,formula=y~x+I(x^2))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########逐步迴歸##########
y<-ch6sample.exp7$業務總銷售額
x1<-ch6sample.exp7$業務服務時間
x2<-ch6sample.exp7$市場潛力
x3<-ch6sample.exp7$廣告費用
x4<-ch6sample.exp7$市場佔有率
x5<-ch6sample.exp7$市場占有率變化
x6<-ch6sample.exp7$業務的戶數z
x7<-ch6sample.exp7$工作負荷

model_origin<-lm(y~x1+x2+x3+x4+x5+x6+x7, data=ch6sample.exp7)
model2<-step(model_origin)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########羅吉斯迴歸(logistic)##########z
logi_y<-ch6sample.exp8$折價卷z
logi_x1<-ch6sample.exp8$`年度開銷(千元)`
logi_x2<-ch6sample.exp8$百貨公司信用卡

logi_resault<-glm(logi_y~logi_x1+logi_x2, data=ch6sample.exp8, family=binomial)
predic_data<-data.frame(logi_x1=2,logi_x2=0)
predict.glm(logi_resault,type="response",newdata=predic_data)



