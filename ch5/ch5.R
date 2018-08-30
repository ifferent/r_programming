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
#卜瓦松分配例題
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
#均勻分配例題
#1
(punif(48,min=45,max=60)-punif(47,min=40,max=60)) +
    (punif(52,min=40,max=60)-punif(51,min=40,max=60)) +
        (punif(58,min=40,max=60)-punif(57,min=40,max=60))

#2
(punif(48,min=40,max=60)-punif(47,min=40,max=60)) +
    (punif(52,min=40,max=60)-punif(51,min=40,max=60)) +
    (punif(58,min=40,max=60)-punif(57,min=40,max=60))

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
#中央極限定理
size<-3000
popu<-tibble(
    uni   = runif(size,min=0,max=50),
    exp   = 10*rexp(size,rate=1),
    uquad = ruquad(size,b=50)
)
ggplot(popu,aes(x=popu$exp))+geom_histogram(binwidth=0.5)
hist(popu$exp,breaks=50)

cen.lim.2<-tibble(
    x.uni   = dsample(popu$uni,size=2, time=3000),
    x.exp   = dsample(popu$exp,size=2, time=3000),
    x.uquad = dsample(popu$uquad,size=2, time=3000)
)

cen.lim.5<-tibble(
    x.uni   = dsample(popu$uni,size=5, time=3000),
    x.exp   = dsample(popu$exp,size=5, time=3000),
    x.uquad = dsample(popu$uquad,size=5, time=3000)
)

cen.lim.10<-tibble(
    x.uni   = dsample(popu$uni,size=10, time=3000),
    x.exp   = dsample(popu$exp,size=10, time=3000),
    x.uquad = dsample(popu$uquad,size=10, time=3000)
)
cen.lim.30<-tibble(
    x.uni   = dsample(popu$uni,size=30, time=3000),
    x.exp   = dsample(popu$exp,size=30, time=3000),
    x.uquad = dsample(popu$uquad,size=30, time=3000)
)

central_lim<-tibble(
    "取樣"      = c(popu$uni,  cen.lim.2$x.uni,  cen.lim.10$x.uni,  cen.lim.30$x.uni,  cen.lim.100$x.uni,
                    popu$exp,  cen.lim.2$x.exp,  cen.lim.10$x.exp,  cen.lim.30$x.exp,  cen.lim.100$x.exp,
                    popu$uquad,cen.lim.2$x.uquad,cen.lim.10$x.uquad,cen.lim.30$x.uquad,cen.lim.100$x.uquad),
    "母體分配"  = c(rep("uni",15000),rep("exp",15000),rep("uquad",15000)),
    "抽樣數(n)" = c(rep(0,3000), rep(2,3000), rep(5,3000), rep(10,3000), rep(30,3000),
                    rep(0,3000), rep(2,3000), rep(5,3000), rep(10,3000), rep(30,3000),
                    rep(0,3000), rep(2,3000), rep(5,3000), rep(10,3000), rep(30,3000))
)

ggplot(central_lim, aes(x=取樣,fill=母體分配)) +
    geom_histogram(binwidth=0.5) +
        facet_grid(`抽樣數(n)`~母體分配, scales="free") +
            xlab("母體隨機變數 & 取樣平均數") + ylab("相對次數")


###############################################################################
# popu.m        :母體平均值
# popu.sd       :母體標準差
# popu.est.m    :取樣平均值
# popu.est.sd   :取樣標準差
# up.interval   :+500元上限
# down.interval :-500元下限

popu.person_info<-tibble(
    member.ID = 1:8000,
    salary    = rnorm(8000, mean=52000, sd=6000)
)

#population
popu.m<-mean(popu.person_info$salary)
popu.sd<-sd(popu.person_info$salary)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#點估計
n.2 <- sample(popu.person_info$salary, 2)
popu.est.m.2<-mean(n.2)
popu.est.sd.2<-sd(n.2)
#計算標準偏差(sem)
sem.n2<-popu.sd/sqrt(length(n.2))
up.interval.2<-popu.est.m.2 + 500
down.interval.2<-popu.est.m.2 - 500

#樣本平均數落在+-$500的機率(母體平均數:標準差未知)
pnorm(up.interval.2, mean=popu.est.m.2, sd=popu.est.sd.2) -
    pnorm(down.interval.2, mean=popu.est.m.2, sd=popu.est.sd.2)

#樣本平均數落在+-$500的機率(母體平均數:標準差已知)
pnorm(up.interval.2, mean=popu.est.m.2, sd=sem.n2) -
    pnorm(down.interval.2, mean=popu.est.m.2, sd=sem.n2)


#母體平均值與取樣平均值的差值
abs(popu.m - popu.est.m.2)
up.interval.est<-popu.est.m.2 + 5500
down.interval.est<-popu.est.m.2 - 5500
pnorm(up.interval.est, mean=popu.m, sd=sem.n2) -
    pnorm(down.interval.est, mean=popu.m, sd=sem.n2)

df.est<-tibble(
    "取樣分佈(n=2)"   = (popu.est.m.2-3.5*sem.n2):(popu.est.m.2+3.5*sem.n2),
    "-500<=x<=500"    = seq(down.interval.2,up.interval.2,(up.interval.2-down.interval.2)/(length(`取樣分佈(n=2)`)-1)),
    "p[-500<=x<=500]" = dnorm(`-500<=x<=500`, mean=popu.est.m.2, sd=sem.n2)
)
ggplot(df.est) +
    geom_line(aes(x=`取樣分佈(n=2)`), stat="function", fun=dnorm, 
              args=list(mean=popu.est.m.2, sd=sem.n2)) +
    geom_area(aes(x=`-500<=x<=500`, y=`p[-500<=x<=500]`), stat="identity", alpha=0.5, fill="#e67300") +
    annotate("segment", x=popu.m, xend=popu.m, y=0, yend=dnorm(popu.m, mean=popu.est.m.2, sd=sem.n2), 
             colour="#99004d", size=1)


#*********!!!!!!!!*********!!!!!!!!*******
n.10 = sample(popu.person_info$salary, 10)
popu.est.m.10<-mean(n.10)
popu.est.sd.10<-sd(n.10)
#計算標準偏差(sem)
sem.n10<-popu.sd/sqrt(length(n.10))
up.interval.10<-popu.est.m.10 + 500
down.interval.10<-popu.est.m.10 - 500

#樣本平均數落在+-$500的機率(母體平均數:標準差未知)
pnorm(up.interval.10, mean=popu.est.m.10, sd=popu.est.sd.10) -
    pnorm(down.interval.10, mean=popu.est.m.10, sd=popu.est.sd.10)

#樣本平均數落在+-$500的機率(母體平均數:標準差已知)
pnorm(up.interval.10, mean=popu.m, sd=sem.n10) -
    pnorm(down.interval.10, mean=popu.m, sd=sem.n10)


#母體平均值與取樣平均值的差值
abs(popu.m - popu.est.m.10)
up.interval.est<-popu.est.m.10 + 5500
down.interval.est<-popu.est.m.10 - 5500
pnorm(up.interval.est, mean=popu.m, sd=sem.n10) -
    pnorm(down.interval.est, mean=popu.m, sd=sem.n10)

df.est<-tibble(
    "取樣分佈(n=10)"   = (popu.est.m.10-3.5*sem.n10):(popu.est.m.10+3.5*sem.n10),
    "-500<=x<=500"    = seq(down.interval.10,up.interval.10,(up.interval.10-down.interval.10)/(length(`取樣分佈(n=10)`)-1)),
    "p[-500<=x<=500]" = dnorm(`-500<=x<=500`, mean=popu.est.m.10, sd=sem.n10)
)
ggplot(df.est) +
    geom_line(aes(x=`取樣分佈(n=10)`), stat="function", fun=dnorm, 
              args=list(mean=popu.est.m.10, sd=sem.n10)) +
    geom_area(aes(x=`-500<=x<=500`, y=`p[-500<=x<=500]`), stat="identity", alpha=0.5, fill="#e67300") +
    annotate("segment", x=popu.m, xend=popu.m, y=0, yend=dnorm(popu.m, mean=popu.est.m.10, sd=sem.n10), 
             colour="#99004d", size=1)


#*********!!!!!!!!*********!!!!!!!!*******
n.30 = sample(popu.person_info$salary, 30)
popu.est.m.30<-mean(n.30)
popu.est.sd.30<-sd(n.30)
#計算標準偏差(sem)
sem.n30<-popu.sd/sqrt(length(n.30))
up.interval.30<-popu.est.m.30 + 500
down.interval.30<-popu.est.m.30 - 500

#樣本平均數落在+-$500的機率(母體平均數:標準差未知)
pnorm(up.interval.30, mean=popu.est.m.30, sd=popu.est.sd.30) -
    pnorm(down.interval.30, mean=popu.est.m.30, sd=popu.est.sd.30)

#樣本平均數落在+-$500的機率(母體平均數:標準差已知)
pnorm(up.interval.30, mean=popu.m.30, sd=sem.n30) -
    pnorm(down.interval.30, mean=popu.m.30, sd=sem.n30)


#母體平均值與取樣平均值的差值
abs(popu.m - popu.est.m.30)
up.interval.est<-popu.est.m.30 + 5500
down.interval.est<-popu.est.m.30 - 5500
pnorm(up.interval.est, mean=popu.m, sd=sem.n30) -
    pnorm(down.interval.est, mean=popu.m, sd=sem.n30)

df.est<-tibble(
    "取樣分佈(n=30)"   = (popu.est.m.30-3.5*sem.n30):(popu.est.m.30+3.5*sem.n30),
    "-500<=x<=500"    = seq(down.interval.30,up.interval.30,(up.interval.30-down.interval.30)/(length(`取樣分佈(n=30)`)-1)),
    "p[-500<=x<=500]" = dnorm(`-500<=x<=500`, mean=popu.est.m.30, sd=sem.n30)
)
ggplot(df.est) +
    geom_line(aes(x=`取樣分佈(n=30)`), stat="function", fun=dnorm, 
              args=list(mean=popu.est.m.30, sd=sem.n30)) +
    geom_area(aes(x=`-500<=x<=500`, y=`p[-500<=x<=500]`), stat="identity", alpha=0.5, fill="#e67300") +
    annotate("segment", x=popu.m, xend=popu.m, y=0, yend=dnorm(popu.m, mean=popu.est.m.30, sd=sem.n30), 
             colour="#99004d", size=1)
             
             
#*********!!!!!!!!*********!!!!!!!!*******
n.100 = sample(popu.person_info$salary, 100)
popu.est.m.100<-mean(n.100)
popu.est.sd.100<-sd(n.100)
#計算標準偏差(sem)
sem.n100<-popu.sd/sqrt(length(n.100))
up.interval.100<-popu.est.m.100 + 500
down.interval.100<-popu.est.m.100 - 500

#樣本平均數落在+-$500的機率(母體平均數:標準差未知)
pnorm(up.interval.100, mean=popu.est.m.100, sd=popu.est.sd.100) -
    pnorm(down.interval.100, mean=popu.est.m.100, sd=popu.est.sd.100)

#樣本平均數落在+-$500的機率(母體平均數:標準差已知)
pnorm(up.interval.100, mean=popu.m, sd=sem.n100) -
    pnorm(down.interval.100, mean=popu.m, sd=sem.n100)


#母體平均值與取樣平均值的差值
abs(popu.m - popu.est.m.100)
up.interval.est<-popu.est.m.100 + 5500
down.interval.est<-popu.est.m.100 - 5500
pnorm(up.interval.est, mean=popu.m, sd=sem.n100) -
    pnorm(down.interval.est, mean=popu.m, sd=sem.n100)

df.est<-tibble(
    "取樣分佈(n=100)"   = (popu.est.m.100-3.5*sem.n100):(popu.est.m.100+3.5*sem.n100),
    "-500<=x<=500"    = seq(down.interval.100,up.interval.100,(up.interval.100-down.interval.100)/(length(`取樣分佈(n=100)`)-1)),
    "p[-500<=x<=500]" = dnorm(`-500<=x<=500`, mean=popu.est.m.100, sd=sem.n100)
)
ggplot(df.est) +
    geom_line(aes(x=`取樣分佈(n=100)`), stat="function", fun=dnorm, 
              args=list(mean=popu.est.m.100, sd=sem.n100)) +
    geom_area(aes(x=`-500<=x<=500`, y=`p[-500<=x<=500]`), stat="identity", alpha=0.5, fill="#e67300") +
    annotate("segment", x=popu.m, xend=popu.m, y=0, yend=dnorm(popu.m, mean=popu.est.m.100, sd=sem.n100), 
             colour="#99004d", size=1)


#*********!!!!!!!!*********!!!!!!!!*******
n.500 = sample(popu.person_info$salary, 500)
popu.est.m.500<-mean(n.500)
popu.est.sd.500<-sd(n.500)
#計算標準偏差(sem)
sem.n500<-popu.sd/sqrt(length(n.500))
up.interval.500<-popu.est.m.500 + 500
down.interval.500<-popu.est.m.500 - 500

#樣本平均數落在+-$500的機率(母體平均數:標準差未知)
pnorm(up.interval.500, mean=popu.est.m.500, sd=popu.est.sd.500) -
    pnorm(down.interval.500, mean=popu.est.m.500, sd=popu.est.sd.500)

#樣本平均數落在+-$500的機率(母體平均數:標準差已知)
pnorm(up.interval.500, mean=popu.m, sd=sem.n500) -
    pnorm(down.interval.500, mean=popu.m, sd=sem.n500)


#母體平均值與取樣平均值的差值
abs(popu.m - popu.est.m.500)
up.interval.est<-popu.est.m.500 + 5500
down.interval.est<-popu.est.m.500 - 5500
pnorm(up.interval.est, mean=popu.m, sd=sem.n500) -
    pnorm(down.interval.est, mean=popu.m, sd=sem.n500)

df.est<-tibble(
    "取樣分佈(n=500)"   = (popu.est.m.500-3.5*sem.n500):(popu.est.m.500+3.5*sem.n500),
    "-500<=x<=500"    = seq(down.interval.500,up.interval.500,(up.interval.500-down.interval.500)/(length(`取樣分佈(n=500)`)-1)),
    "p[-500<=x<=500]" = dnorm(`-500<=x<=500`, mean=popu.est.m.500, sd=sem.n500)
)
ggplot(df.est) +
    geom_line(aes(x=`取樣分佈(n=500)`), stat="function", fun=dnorm, 
              args=list(mean=popu.est.m.500, sd=sem.n500)) +
    geom_area(aes(x=`-500<=x<=500`, y=`p[-500<=x<=500]`), stat="identity", alpha=0.5,fill="#e67300") +
    annotate("segment", x=popu.m, xend=popu.m, y=0, yend=dnorm(popu.m, mean=popu.est.m.500, sd=sem.n500), 
             colour="#99004d", size=1)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#區間估計與假設檢定
#母體平均數與標準差未知
inter.est.m<-mean(ch5sample.exp1$"卡款")
inter.est.sd<-sd(ch5sample.exp1$"卡款")
#使用t分配取自由度為樣本大小-1
df.sample<-length(ch5sample.exp1$"卡款")-1
#信賴區間95%
alpha.half<-(1-0.95)/2
t_statistical<-qt(alpha.half,df=df.sample)
#利用弱大數法則計算樣本標準差
sem.est<-inter.est.sd/sqrt(length(ch5sample.exp1$"卡款"))
#計算邊際誤差
margin_err<-abs(t_statistical)*sem.est

up.interval.unknow<-inter.est.m + margin_err
down.interval.unknow<-inter.est.m - margin_err
df.est.unknow<-tibble(
    "取樣分佈"          = (inter.est.m-3.5*sem.est):(inter.est.m+3.5*sem.est),
    "x_interval"        = seq(down.interval.unknow,up.interval.unknow,(up.interval.unknow-down.interval.unknow)/(length(取樣分佈)-1)),
    "p[x+-margin_err]"  = dnorm(x_interval, mean=inter.est.m, sd=sem.est)   
)

ggplot(df.est.unknow) +
    geom_line(aes(x=取樣分佈), stat="function", fun=dnorm, 
              args=list(mean=inter.est.m, sd=sem.est)) +
    geom_area(aes(x=x_interval, y=`p[x+-margin_err]`), stat="identity", alpha=0.5,fill="#e67300") +
    annotate("segment", x=inter.est.m, xend=inter.est.m, 
             y=0, yend=dnorm(inter.est.m, mean=inter.est.m, sd=sem.est),colour="#99004d", size=1)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#區間估計與假設檢定
#母體平均數與標準差已知

inter.est.m<-mean(ch5sample.exp1$"卡款")
inter.est.sd<-4007
#信賴區間95%
alpha.half<-(1-0.95)/2
z<-qnorm(alpha.half)
#利用弱大數法則計算樣本標準差
sem.est<-inter.est.sd/sqrt(length(ch5sample.exp1$"卡款"))
#計算邊際誤差
margin_err<-abs(z)*sem.est

up.interval.know<-inter.est.m + margin_err
down.interval.know<-inter.est.m - margin_err
df.est.unknow<-tibble(
    "取樣分佈"          = (inter.est.m-3.5*sem.est):(inter.est.m+3.5*sem.est),
    "x_interval"        = seq(down.interval.know,up.interval.know,(up.interval.know-down.interval.know)/(length(取樣分佈)-1)),
    "p[x+-margin_err]"  = dnorm(x_interval, mean=inter.est.m, sd=sem.est)   
)

ggplot(df.est.unknow) +
    geom_line(aes(x=取樣分佈), stat="function", fun=dnorm, 
              args=list(mean=inter.est.m, sd=sem.est)) +
    geom_area(aes(x=x_interval, y=`p[x+-margin_err]`), stat="identity", alpha=0.5,fill="#e67300") +
    annotate("segment", x=inter.est.m, xend=inter.est.m, 
             y=0, yend=dnorm(inter.est.m, mean=inter.est.m, sd=sem.est),colour="#99004d", size=1)


























