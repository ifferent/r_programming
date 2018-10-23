###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
###                                                                       ###
### 完成日期: 2018-10-23                                                  ###
### 作者：Roddy Hung                                                      ###
### 版本：V0.1                                                            ###
###                                                                       ###
### 第8章範例程式:                                                        ###
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

library(tibble)

################################檔案載入與設定#################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####自訂函數

df <- tibble::tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
    (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
    (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
    (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
    (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rescale01 <- function(x) {
    (x - min(x, na.rm = TRUE)) / 
        (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

rescale01(df$a)
rescale01(df$b)
rescale01(df$c)
rescale01(df$d)
rescale01(c(3,4,6))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df.a_rag<-range(df$a)
df.a_rag[1] #min
df.a_rag[2] #max

source("ch8/ch8_exp_fun.R")

rescale02(df$a)
rescale02(df$b)
rescale02(df$c)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####串列(list)

x <- list(1, 2, 3)

x_named <- list(a = 1, b = 2, c = 3)

x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####迴圈

df[[1]] #a
df[[2]] #b
df[[3]] #b
df[[4]] #b

for( i in 1:4)
{
    df[[i]] <- (df[[i]] - min(df[[i]], na.rm = TRUE)) / 
        (max(df[[i]], na.rm = TRUE) - min(df[[i]], na.rm = TRUE))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

seq_along(df)

output <- vector("list", length(df)) 
for( i in seq_along(df))
{
    output[[i]] <- (df[[i]] - min(df[[i]], na.rm = TRUE)) / 
        (max(df[[i]], na.rm = TRUE) - min(df[[i]], na.rm = TRUE))
}
output
