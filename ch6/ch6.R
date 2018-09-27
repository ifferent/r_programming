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

ch6sample.exp1<-read_csv(ch6sample.exp1_path, col_names=TRUE)

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

