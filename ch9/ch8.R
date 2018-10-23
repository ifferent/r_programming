source("common/check_package.R")#檢查是否有未安裝的套件
source("common/function.R",encoding="utf-8") #將公用自訂函數載起來
source("ch8/ch8_model_function.R",encoding="utf-8")

library(readr)

ch8toAnalyData1.path<-"ch8/sample_data/測試1.csv"
ch8toAnalyData2.path<-"ch8/sample_data/測試2.csv"
ch8toAnalyData3.path<-"ch8/sample_data/測試3.csv"

ch8toAnalyData4.path<-"ch8/sample_data/測試na.csv"
ch8toAnalyData_out.path<-"ch8/sample_data/測試結果.csv"

ch8toAnalyData1<-read_csv(ch8toAnalyData1.path, col_names=TRUE)
ch8toAnalyData2<-read_csv(ch8toAnalyData2.path, col_names=TRUE)
ch8toAnalyData3<-read_csv(ch8toAnalyData3.path, col_names=TRUE)

ch8toAnalyData4<-read_csv(ch8toAnalyData4.path, col_names=TRUE)
ch8toAnalyData_out<-read_csv(ch8toAnalyData_out.path, col_names=TRUE)

ch8toAnalyData1$貨物稅
ch8toAnalyData2$CPI2
spec.pgram(cbind(ch8toAnalyData1$"貨物稅", ch8toAnalyData2$"CPI2"), fast=FALSE, taper=FALSE,
           spans = c(4,4), plot=FALSE)$coh

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_corss_path1<-"ch8/sample_data/物價2.csv"
test_corss_path2<-"ch8/sample_data/稅.csv"
test_corss1<-read_csv(test_corss_path1, col_names=TRUE)
test_corss2<-read_csv(test_corss_path2, col_names=TRUE)

test_corss1.order<-test_corss2 %>%
    gen_fft_data() %>% 
    gen_fft_order()
test_corss2.order<-test_corss1 %>%
    gen_fft_data() %>% 
    gen_fft_order()
test_cross.coh<-coh_test(test_corss2,test_corss1,feq.sampling=37,coh_level=0.5)

source("ch8/ch8_model_function.R",encoding="utf-8")
cross_resault<-gen_freqdom_cross_table(test_cross.coh, test_corss1.order, 
                                       test_corss2.order, filter_condi="two_side")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





