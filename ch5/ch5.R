###############################################################################
###############################################################################
####                                                                       ####
#### 完成日期: 2018-08-28                                                  ####
#### 作者：Roddy Hung                                                      ####
#### 版本：V2.3                                                            ####
####                                                                       ####
#### 第4章範例程式:                                                        ####
####    1.建立資料框                                                       ####
####    2.將資料框輸出為檔案                                               ####
####    3.機率分配                                                         ####
####    4.檢定                                                             ####
####        i.點估計                                                       ####
####       ii.區間估計                                                     ####
####      iii.假設檢定                                                     ####
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
#### tibble: tibble, tirbble                                               ####
####                                                                       ####
###############################################################################

library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
################################檔案載入與設定#################################
ch5sample.exp1_path="ch5/sample_data/信用卡卡款.csv"

ch5sample.exp1<-read_csv(ch5sample.exp1_path,col_names=TRUE)

###############################################################################
name<-c("Alice","Bob","Roddy","Eddie","William",
        "Howard","Rose","Mary","Daisy","Jack")
hight<-sample(150:185,10,replace=T)

data.frame (
    c(1:10),
    2,
    3:12,
    name
)

data.frame (
    c(1:9),
    2,
    3:12,
    name
)#序列資料不等長，無法自動產生，但是單一資料可以自動產生

tibble(
    c(1:10),
    2,
    3:12,
    name
)

tibble(
    profile.name  = name,
    profile.age   = c(23,22,20,18,24,25,23,20,21,22),
    profile.hight = hight,
    profile.sex   = c("F","M","M","M","M","M","F","F","F","M")   
)

tibble(
    `@@`       = name,
    `A~G~E`    = c(23,22,20,18,24,25,23,20,21,22),
    `  T . T ` = hight,
    `5000`     = c("F","M","M","M","M","M","F","F","F","M")   
)#可以使用``符號使特殊符號、空格、數字等成為資料欄的名稱

df_exp<-tibble(
    profile.name  = name,
    profile.age   = age,
    profile.hight = hight,
    profile.sex   = sex,
    #與data.frame的最大差別
    magic_index   = profile.hight/(profile.age*ifelse(profile.sex=="M",1,2)) 
)

out_exp<-tribble(
    ~profile.name, ~profile.age, ~profile.hight, ~profile.sex, ~magic_index,
    #--------------/-------------/---------------/-------------/---------------------
    "Alice",        23,           hight[1],       "女",         df_exp$magic_index[1],
    "Bob",          22,           hight[2],       "男",         df_exp$magic_index[2],
    "Roddy",        20,           hight[3],       "男",         df_exp$magic_index[3],
    "Eddie",        18,           hight[4],       "男",         df_exp$magic_index[4],
    "William",      24,           hight[5],       "男",         df_exp$magic_index[5],
    "Howard",       25,           hight[6],       "男",         df_exp$magic_index[6],
    "Rose",         23,           hight[7],       "女",         df_exp$magic_index[7],
    "Mary",         20,           hight[8],       "女",         df_exp$magic_index[8],
    "Daisy",        21,           hight[9],       "女",         df_exp$magic_index[9],
    "Jack",         22,           hight[10],      "男",         df_exp$magic_index[10]
)#增加程式可讀性

###############################################################################
output_path<-c("output/ch5/")
write.csv(out_exp, paste(output_path, "use_base_writ_csv.csv"), 
          row.names=F, fileEncoding="UTF-8")

write.csv(out_exp, paste(output_path, "use_base_writ_csv_noutf8.csv"), 
          row.names=F)
read_csv(paste(output_path, "use_base_writ_csv_noutf8.csv"),col_names=TRUE)
#採用預設的編碼，雖然用excel並不會亂碼，但用這個檔案再次給R讀取就會出現悲劇

write.csv(out_exp,paste(output_path, "use_base_writ_csv2.csv"), 
          row.names=T, fileEncoding="UTF-8")

write_csv(out_exp,paste(output_path, "use_readr_writ_csv.csv"))
write_excel_csv(out_exp,paste(output_path, "use_readr_writ_excel_csv.csv"))

###############################################################################

#1
pois.lambda <- 7
dpois(0,pois.lambda)
#2
1-dpois(0,pois.lambda)-dpois(1,pois.lambda)

1-ppois(1,pois.lambda) # 1 - P[X ≤ 1]

ppois(1, pois.lambda, lower.tail=F) # P[X > 1]

#3
pois.lambda_per30sec <- pois.lambda/2
ppois(0, pois.lambda_per30sec, lower.tail=F ) # P[X > 0]

#4
ppois(4, pois.lambda, lower.tail=F) # P[X > 4]

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

transcript <- tibble(
    "座號"=1:100,
    #round函數主要用來取整數
    "國文"=round(rnorm(100, mean=80, sd=10), 0),
    "數學"=round(rnorm(100, mean=60, sd=30), 0),
    "歷史"=round(rnorm(100, mean=90, sd=5),  0),
    "地理"=round(rnorm(100, mean=70, sd=20), 0)
)

#讓分數不要超過100分或低於0分(負分)
transcript$國文<-ifelse(transcript$國文>100, 100, transcript$國文)
transcript$數學<-ifelse(transcript$數學>100, 100, transcript$數學)
transcript$數學<-ifelse(transcript$數學<  0,   0, transcript$數學)
transcript$歷史<-ifelse(transcript$歷史>100, 100, transcript$歷史)
transcript$地理<-ifelse(transcript$地理>100, 100, transcript$地理)

mean(transcript$國文)
mean(transcript$數學)
mean(transcript$歷史)
mean(transcript$地理)

ggplot(transcript,aes(國文)) +
    geom_histogram(stat="bin",bins=10, fill="white",colour="black") +
    geom_line(color="red",stat="bin",bins=20)

ggplot(transcript,aes(數學)) +
    geom_histogram(stat="bin",bins=10, fill="white",colour="black") +
    geom_line(color="red",stat="bin",bins=20)

ggplot(transcript,aes(歷史)) +
    geom_histogram(stat="bin",bins=10, fill="white",colour="black") +
    geom_line(color="red",stat="bin",bins=20)

ggplot(transcript,aes(地理)) +
    geom_histogram(stat="bin",bins=10, fill="white",colour="black") +
    geom_line(color="red",stat="bin",bins=20)

write_excel_csv(transcript,paste(output_path,"成績單.csv"))

###############################################################################

df <- data.frame(
    #x.
    x.uni  = 100:160,
    x.norm = -10:10,
    x.f    = 0:10, 
    x.pois = 0:30
)

df.base <- data.frame(
    #x.
    "均勻分配"       = c(100,160),
    "常態分配"       = c(-10,10),
    "F分配"          = c(0,10),
    "卜瓦松分配"     = c(0,30)
)

df.tibble <- tibble(
    "隨機變數"       = -10:10,
    "-1<=x<=1"       = seq(-1,1,2/20),
    "p[-1<=x<=1]"    = dnorm(`-1<=x<=1`,mean=0,sd=2)
)

pois.lambda <- 10

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
x.uni<-100:160
plot(x.uni,dunif(x.uni,min=120,max=140),type='l')

x.norm<--10:10
plot(x.norm,dnorm(x.norm,mean=0,sd=2),type='l')
x.norm<-seq(-10,10,0.001)
plot(x.norm,dnorm(x.norm,mean=0,sd=2),type='l')
plot(x.norm,pnorm(x.norm,mean=0,sd=2),type='l')

x.pois<-0:30
plot(x.pois,dpois(x.pois,pois.lambda),type='h')

curve(dunif(x,min=120,max=140),from=100,to=160)
curve(dnorm(x,mean=0,sd=2),from=-10,to=10)
curve(pnorm(x,mean=0,sd=2),from=-10,to=10)
barplot(dpois(x.pois,pois.lambda))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#已statistic為主的寫法
ggplot(df.base, aes(x=均勻分配)) + 
    stat_function(geo="point",fun = dunif, colour = "red", args=list(min=120,max=140))
#已geometric為主的寫法
ggplot(df.base, aes(x=均勻分配)) + 
    geom_point(stat="function",fun=dunif, args=list(min=120,max=140))

ggplot(df.base, aes(x=常態分配)) + 
    geom_line(stat="function",fun=dnorm, args=list(mean=0,sd=2))
ggplot(df.base, aes(x=常態分配)) + 
    geom_line(stat="function",fun=pnorm, args=list(mean=0,sd=2))

#data.frame沒辦法遞迴的計算
ggplot(df.tibble) + 
    geom_line(aes(x=隨機變數),stat="function",
              fun=dnorm, args=list(mean=0,sd=2)) +
    geom_area(aes(x=`-1<=x<=1`,y=`p[-1<=x<=1]`), 
              stat="identity", fill="green", alpha="0.4")

#離散分配使用stat="function"計算會有問題，因為會強制變成連續函數
ggplot(df.base, aes(x=卜瓦松分配)) + 
    geom_bar(stat="function",fun=dpois, args=list(lambda=pois.lambda))

pois_exp <- tibble(
    "隨機變數"=0:20,
    "機率"=dpois(x,pois.lambda)
)
ggplot(pois_exp, aes(x=隨機變數, y=機率, fill=機率)) +
    geom_bar(stat="identity")

###############################################################################



































