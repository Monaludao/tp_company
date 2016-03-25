sample1<-function(){
    sample1.df<-read.csv("./tpprocess/company_map.csv",stringsAsFactors = FALSE)
    sample1.df<-sample1.df[grep("304898.19",sample1.df$Response_X),]
    sample1.df$level<-gsub(".*號(.*)樓.*","\\1",sample1.df$CADDR)
    for(i in 1:nrow(sameple1.df)){
        sample1.df$level[i]<-check_digit(sample1.df$level[i])
    }
}