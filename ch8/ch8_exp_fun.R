rescale02 <- function(x) 
{
    rag_x<-range(x,na.rm = TRUE)
    (x - rag_x[1]) / (rag_x[2] - rag_x[1])
}