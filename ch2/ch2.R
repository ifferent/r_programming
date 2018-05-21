show_repeat<-rep(3,5)
show_repeat
a_set<-c(0.5,-3,25,78,10)
a_seq<-c(1:5)
a_set.string<-c("Hellow","everyone","!")
null_set<-NA
a_mixture<-list(a_set,a_seq,a_set.string,null_set)

mean(sample(1:sample(1:exp(5),1), sample(1:log(1500,exp(1)),1)))

#############################################################################
############## 

library("readr")
library("tidyverse")

path.Ch2_exp_table1.csv<-"D:/my_code/R_code/presentation_sample_ppt/ch2/Ch2_exp_table1.csv"

mych2.csv<-read_csv(path.Ch2_exp_table1.csv)
#mych2.csv<-read_csv("D:/my_code/R_code/presentation_sample_ppt/ch2/Ch2_exp_table1.csv")

mych2.csv2<-read.csv(path.Ch2_exp_table1.csv,header=TRUE)
#mych2.csv2<-read.csv("D:/my_code/R_code/presentation_sample_ppt/ch2/Ch2_exp_table1.csv",header=TRUE)

mych2.csv3<-read.csv(path.Ch2_exp_table1.csv,header=FALSE)
#mych2.csv3<-read.csv("D:/my_code/R_code/presentation_sample_ppt/ch2/Ch2_exp_table1.csv",header=FALSE)

filter(mych2.csv, month==12, day==25)

filter(mych2.csv, month==2, dep_delay<=0)  
  
  

