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

ch6sample.exp1<-read_csv(ch6sample.exp1_path, col_names=TRUE)
ch6sample.exp2<-read_csv(ch6sample.exp2_path, col_names=TRUE)

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





