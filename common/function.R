library(gtools)
library(tibble)

m2gg.scope <- function(df, repeats.allowed=T) {
  
    df.dim <- dim(df)
    ifelse(repeats.allowed,order<-permutations(df.dim[2]-3,2,repeats.allowed = T)+1,
                           order<-combinations(df.dim[2]-3,2,repeats.allowed = F)+1)
  
    scope1<-vector("integer", length(order[,1])*length(df[[2]]))
    scope2<-vector("integer", length(order[,2])*length(df[[2]]))
    subject1<-vector("integer", length(order[,1])*length(df[[2]]))
    subject2<-vector("integer", length(order[,2])*length(df[[2]]))
    get_df_name <- attr(df,"names")
  
    for(i in seq_along(order[,1])) {
    
        ifelse(i==1,scope1<-c(df[[order[i,1]]]),
                    scope1<-c(scope1,df[[order[i,1]]]))
    
        ifelse(i==1,scope2<-c(df[[order[i,2]]]),
                    scope2<-c(scope2,df[[order[i,2]]]))
    
        ifelse(i==1,subject1<-c(rep(get_df_name[order[i,1]],100))
                   ,subject1<-c(subject1, rep(get_df_name[order[i,1]],100)))
        
        ifelse(i==1,subject2<-c(rep(get_df_name[order[i,2]],100))
                   ,subject2<-c(subject2, rep(get_df_name[order[i,2]],100)))
    }
  
    df_out <- tibble(
      學號=rep(1:100,length(order[,1])),
      性別=rep(df[[6]],length(order[,1])),
      班級=rep(df[[7]],length(order[,1])),
      分數1=scope1,
      分數2=scope2,
      科目1=subject1,
      科目2=subject2
    )
    return(df_out)
}
    

