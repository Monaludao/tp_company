sample1<-function(){
    sample1.df<-read.csv("./tpprocess/company_map.csv",stringsAsFactors = FALSE)
    sample1.df<-sample1.df[grep("304898.19",sample1.df$Response_X),]
    sample1.df$level<-gsub(".*號(.*)樓.*","\\1",sample1.df$CADDR)
    
    for(i in 1:nrow(sample1.df)){
        sample1.df$level[i]<-check_digit(sample1.df$level[i])
    }
    
    sample.df$R_cat=0;sample.df$P_cat=0;sample.df$M_cat=0
    
    sample.df$R_cat[grep("R_cat",sample.df$IND_CAT)]=1
    sample.df$P_cat[grep("P_cat",sample.df$IND_CAT)]=1
    sample.df$M_cat[grep("M_cat",sample.df$IND_CAT)]=1
    
    melt.df<-melt(sample.df,id=c("level"),measure.vars = c("R_cat","P_cat","M_cat"))
    cast.df<-dcast(melt.df,level~variable,sum)
    cast.df$max_cat<-colnames(cast.df)[(max.col(cast.df[,2:4]))+1]
    
    write.csv(cast.df,file="./tpprocess/sample1_map.csv",row.names=FALSE)
}