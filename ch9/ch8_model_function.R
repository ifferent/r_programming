library(readr)
library(dplyr)
library(purrr)
library(magrittr)
library(stringr)
library(tidyr)
library(tibble)

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


check_ref <- function(df, ref_index, pos)
{
    
    new_ptr <- length(df)
    df_index <- which(!is.na(df[[new_ptr]]))
    now_ref_index <- ref_index[[pos]]
    
    ref_vec <- vector("character",length(df[[new_ptr]]))

    for( i in seq_along(df_index))
    {
        if(df_index[i] == now_ref_index[i])
        {
            ref_vec[df_index[i]]<-"YES"
        }
        else
        {
            ref_vec[df_index[i]]<-"NO"
        }
    }
    
    ref_vec
}


make_cross_table_report <- function(df, ref_left=NULL, ref_right=NULL)
{
    
    new_df <- select(df, 1)
    
    for( i in seq_along(df))
    {
        if(i!=1)
        {
            new_df <- mutate(new_df,
                             !!(names(df)[i]):=df[[i]] 
            )
            left_table <- check_ref(new_df, ref_left,i-1)
            right_table <- check_ref(new_df, ref_right,i-1)

            left_name <- str_c(unlist(str_split(names(df)[i],"to"))[1],"(",(i-1),")","交叉比對")
            right_name <- str_c(unlist(str_split(names(df)[i],"to"))[2],"(",(i-1),")","交叉比對")
            
            new_df <- mutate(new_df,
                             !!(left_name):=left_table,
                             !!(right_name):=right_table 
            )
            
        }
    }
    
    new_df
    
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

gen_fft_order <- function(df, fft.order=10)
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

gen_freqdom_cross_table<-function(df1, df2, df3=NULL, ..., filter_condi=NULL)
{
    
    p_sel1<-2:length(df1) %>%
        map(~which(df1[[.]]>=0.5))
    
    p_sel2<-df2 %>% 
        select(ends_with("前十大週期"))
    
    if(!is.null(df3))
    {
        p_sel3<-df3 %>% 
            select(ends_with("前十大週期"))
    }
    
    if(filter_condi=="right_side")
    {
        p_cross_index.right<-cross_table.right(p_sel1,p_sel2)
        #print(p_cross_index.right)
    }
    else if(filter_condi=="left_side")
    {
        p_cross_index.left<-cross_table.left(p_sel1,p_sel2)
        #print(p_cross_index.left)
    }
    else if(filter_condi=="two_side")
    {
        p_cross_index.left<-cross_table.left(p_sel1,p_sel3)
        p_cross_index.right<-cross_table.left(p_sel1,p_sel2)
        #print(p_cross_index.right)
        #print(p_cross_index.left)
    }
    else
    {}

    make_cross_table_report(df1, p_cross_index.left, p_cross_index.right)
    
}

cross_table.right<-function(p1, p2)
{
    
    cross_table_output<-vector("list",length(p1))
    k<-1
    
    for(i in seq_along(p1))
    {
        for(j in seq_along(p1[[i]]))
        {
            
            if(any(p2[[k]]==p1[[i]][j]))
            {
                cross_table_output[[i]][j]<-p1[[i]][j]
            }
            else
            {
                cross_table_output[[i]][j]<-0
            }
        }
        
        i_check<-i%%length(p2)
        
        if((i_check==1)&&(i!=1))
        {
            k<-k+1
        }
    }
    
    cross_table_output
    
}

cross_table.left<-function(p1, p2)
{
    
    cross_table_output<-vector("list",length(p1))
    for(i in seq_along(p1))
    {
        for(j in seq_along(p2))
        {
            for(k in seq_along(p1[[i]]) )
            {
                if( any(p2[[j]]==p1[[i]][k]) )
                {
                    cross_table_output[[i]][k]<-p1[[i]][k]
                }
                else
                {
                    cross_table_output[[i]][k]<-0
                }
            }
        }
    }
    
    cross_table_output
    
}

#cross_table.two_side<-function(p1, p2)
#{

#}

