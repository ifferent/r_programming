library(readr)
library(dplyr)
library(purrr)
library(magrittr)
library(stringr)
library(tidyr)

check_data_fft_point <- function(x,ref_data)
{
    x_point<-length(x)
    ref_data_point<-length(ref_data)
    if(x_point>ref_data_point)
    {
        x<-x[1:ref_data_point]
        return(x)
    }
    else if(x_point<ref_data_point)
    {
        diff_point<-ref_data_point-x_point
        x<-c(x,rep(0,diff_point))
        return(x)
    }
    else return(x)
}

gen_fft_data <- function(df, point=NULL, na.rm=FALSE)
{
    if(!is.data.frame(df))
        return("輸入必須是data frame")
    
    df.name<-names(df)
    
    if(df.name[1]!="時間點")
        return("非標準分析表格")
    
    if(is.null(point))
    {
        df.fft<-tibble(
            fft_index=1:length(df[[1]])
        )
    }
    else
    {
        df.fft<-tibble(
            fft_index=1:point
        )
    }
    
    for(i in (seq_along(df)-1))
    {
        if(i!=0)
        {
            temp_name<-str_c(df.name[i+1],"to頻域")
            temp_fft<-fft(check_data_fft_point(df[[i+1]],df.fft$fft_index))
            df.fft<-df.fft %>%
                mutate(
                    !!(temp_name):=temp_fft    
                )
        }
    }
    
    df.fft
}
