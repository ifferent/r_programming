library("readr")

############################################檔案載入與設定################################################

ch2sample.exp1_path<-"ch2/sample_data/Ch2_exp_table1.csv"

##########################################################################################################

mych2.csv<-read_csv(ch2sample.exp1_path)
#mych2.csv<-read_csv("D:/my_code/R_code/presentation_sample_ppt/ch2/Ch2_exp_table1.csv")

mych2.csv2<-read.csv(ch2sample.exp1_path,header=TRUE)
#mych2.csv2<-read.csv("D:/my_code/R_code/presentation_sample_ppt/ch2/Ch2_exp_table1.csv",header=TRUE)

mych2.csv3<-read.csv(path.Ch2_exp_table1.csv,header=FALSE)
#mych2.csv3<-read.csv("D:/my_code/R_code/presentation_sample_ppt/ch2/Ch2_exp_table1.csv",header=FALSE)

filter(mych2.csv, month==12, day==25)

filter(mych2.csv, month==2, dep_delay<=0)  
  
  

