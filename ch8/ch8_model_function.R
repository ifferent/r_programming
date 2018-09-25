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

set_coh_output_level<-function(coh_data, level)
{
    n<-length(coh_data)
    index<-(coh_data[2:n]<level)
    for(i in seq_along(coh_data))
    {
        if(i!=1)
        {
            coh_data[[i]][index[,i-1]]<-NA
        }
    }
    
    return(coh_data)
}

make_coh_test_report <- function(coh_data, coh_data.name, feq.sampling, feq.obs=NULL)
{
    n  <- length(unlist(coh_data.name))
    n1 <- length(coh_data.name)
    n2 <- n/n1
    n.sampling <- round(feq.sampling/2,0)
    
    if(is.null(feq.obs))
    {
        report<-tibble(
            "頻率"=1:n.sampling
        )
    }
    else
    {
        report<-tibble(
            "頻率" = feq.obs
        )
    }
    
    for( i in 1:n1)
    {
        for( j in 1:n2)
        {
            if(is.null(feq.obs))
            {
                report<- report %>%
                    mutate(
                        !!(coh_data.name[[i]][[j]]):=(coh_data[[i]][[j]][,2])  
                    )
            }
            else
            {
                report<- report %>%
                    mutate(
                        !!(coh_data.name[[i]][[j]]):=(coh_data[[i]][[j]][feq.obs,2])  
                    )
            }
        }
    }
    
    return(report)
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

coh_test<-function(df1, df2, 
                   feq.sampling=NULL, feq.obs=NULL,
                   ...,
                   coh_level=0,plot=FALSE)
{
    if(!is.data.frame(df1)||!is.data.frame(df2))
        return("輸入必須是data frame")
    
    if((names(df1)[1]!="時間點")||(names(df2)[1]!="時間點"))
        return("非分析標準格式")
    
    if((as.integer(feq.sampling)<feq.sampling) ||
       is.null(feq.sampling))
    {    
        if(is.null(feq.sampling))
            feq.sampling = length(df1$時間點)
        else
            return("頻率必須是整數")
    }
    if((as.integer(feq.obs)<feq.obs) ||
       is.null(feq.obs))
    {
        if(!is.null(feq.obs))
            return("頻率必須是整數")
    }
    
    n1<-length(df1)
    df1.name <- names(df1[2:n1])
    n2<-length(df2)
    df2.name <- names(df2)
    
    df.wave1 <- df1[2:n1]
    
    df.coh_total<-2:n2 %>%
        map(~map(df.wave1, coh, wave2=df2[.], f=feq.sampling, plot=F))
    df.name<-2:n2 %>% 
        map(~map(df1.name,str_c,"to",df2.name[.]))
    
    if(coh_level!=0)
    {
        output<-make_coh_test_report(df.coh_total, df.name, 
                                     feq.sampling=feq.sampling, 
                                     feq.obs=feq.obs) %>%
            set_coh_output_level(level=coh_level)
    }
    else
    {
        output<-make_coh_test_report(df.coh_total, df.name, 
                                     feq.sampling=feq.sampling, 
                                     feq.obs=feq.obs)
    }
    
    output
}

