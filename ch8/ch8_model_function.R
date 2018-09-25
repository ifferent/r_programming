library(readr)
library(dplyr)
library(purrr)
library(magrittr)
library(stringr)
library(tidyr)

shift_point <- function(x, lag) {
    n <- length(x)
    xnew <- rep(NA, n)
    if (lag < 0) {
        xnew[1:(n-abs(lag))] <- x[(abs(lag)+1):n]
    } else if (lag > 0) {
        xnew[(lag+1):n] <- x[1:(n-lag)]
    } else {
        xnew <- x
    }
    return(xnew)
}

fft_del_dc <- function(x)
{
    for(i in seq_along(x))
    {
        if(x[i]==0)
        {
            x[i:length(x)]<-shift_point(x[i:length(x)],-1)
            return(x)
        }
        
    }
    
}

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

gen_fft_order <- function(df, write2csv=FALSE, path=NULL)
{
    if(!is.data.frame(df))
        return("輸入必須是data frame")
    
    df.name<-names(df)
    
    if(df.name[1]!="fft_index")
        return("非分析標準格式")
    
    df.fft_order<-tibble(
        index=1:10
    )
    
    point<-round(length(df$fft_index)/2,0)
    print(length(df$fft_index))
    print(point)
    
    for(i in (seq_along(df)-1))
    {
        if(i!=0)
        {
            fft.mod<-Mod(df[[i+1]])
            fft.order<-fft_del_dc(rev(order(fft.mod[1:point]))-1)
            fft.priodic<-length(df$fft_index)/fft.order
            
            temp_name1<-df.name[[i+1]] %>%
                str_sub(1,-5) %>%
                str_c("前十大週期")
            
            temp_name2<-df.name[[i+1]] %>%
                str_sub(1,-5) %>%
                str_c("週期")
            
            df.fft_order<-df.fft_order %>%
                mutate(
                    !!(temp_name1):= fft.order[1:10], 
                    !!(temp_name2):= fft.priodic[1:10]   
                )
        }
    }
    
    if(write2csv==TRUE)
    {
        if(is.null(path))
        {
            return("沒有輸出檔名、輸出路徑")
        }
        write_excel_csv(df.fft_order, path=path)
    }
    df.fft_order
}
