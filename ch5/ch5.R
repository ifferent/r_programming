library(readr)
library(tibble)
library(ggplot2)
################################檔案載入與設定#################################


###############################################################################
score_range=1:100

sample(score_range,50,replace=FALSE)
student.score <- sample(score_range,50,replace=TRUE)
#sample(score_range,50),預設replace是FALSE

mean(student.score)
ggplot
