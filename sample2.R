sample2<-function(){
    sample2.df<-read.csv("./tpprocess/company_map.csv",stringsAsFactors = FALSE)
    sample2.df<-sample2.df[grep("臺北市大安區敦化南路1段190巷",sample2.df$CADDR),]
    sample2.df$number<-gsub(".*巷(.*)號.*","\\1",sample2.df$CADDR)
    sample2.df<-sample2.df[as.integer(sample2.df$number) %% 2 == 1,]
    sample2.df<-sample2.df[as.integer(sample2.df$number)<=33 & as.integer(sample2.df$number)>=11,]
    
    sample2.df$level<-gsub(".*號(.*)樓.*","\\1",sample2.df$CADDR)
    sample2.df$level[!grepl("樓",sample2.df$CADDR)]<-"1"
    
    sample2.df$M_cat<-NULL;sample2.df$P_cat<-NULL;sample2.df$R_cat<-NULL
    sample2.df$M_cat=0;sample2.df$P_cat=0;sample2.df$R_cat=0
    
    sample2.df$R_cat[grep("R_cat",sample2.df$max_cat)]=1
    sample2.df$P_cat[grep("P_cat",sample2.df$max_cat)]=1
    sample2.df$M_cat[grep("M_cat",sample2.df$max_cat)]=1
    
    melt.df<-melt(sample2.df,id=c("number","level"),measure.vars = c("M_cat","P_cat","R_cat"))
    cast.df<-dcast(melt.df,number+level~variable,sum)
    cast.df$max_cat<-colnames(cast.df)[(max.col(cast.df[,3:5]))+2]
    
    cast.df$index<-paste0(cast.df$number,"_",cast.df$level)
    
    cast.df$max_cat[grep("17_2",cast.df$index)]<-"M_cat"
    
    write.csv(cast.df,file="./tpprocess/sample2_map.csv",row.names=FALSE)
}