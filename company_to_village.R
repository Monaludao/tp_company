company_to_village<-function(){
    library(reshape2)
    
    bus.file<-"./tpprocess/business_data.csv"
    com.file<-"./tpprocess/company_data.csv"
    sep.file<-"./tpprocess/specific_data.csv"
    
    file.list<-c(bus.file,com.file,sep.file)
    
    total.df<-data.frame()
    
    for(i in 1:length(file.list)){
        data.df<-read.csv(file.list[i],stringsAsFactors = FALSE)
        total.df<-rbind(total.df,data.df)
    }
    
    total.df$admin<-paste0(total.df$village,total.df$neighbour)
    total.df$R_cat=0;total.df$P_cat=0;total.df$M_cat=0
    
    total.df$R_cat[grep("R_cat",total.df$IND_CAT)]=1
    total.df$P_cat[grep("P_cat",total.df$IND_CAT)]=1
    total.df$M_cat[grep("M_cat",total.df$IND_CAT)]=1
    
    melt.df<-melt(total.df,id=c("admin"),measure.vars = c("R_cat","P_cat","M_cat"))
    cast.df<-dcast(melt.df,admin~variable,sum)
    cast.df$max_cat<-colnames(cast.df)[(max.col(cast.df[,2:4]))+1]
    
    cast.df$admin<-gsub("富臺","富台",cast.df$admin)
    
    write.csv(cast.df,file="./tpprocess/admin_map.csv",row.names=FALSE)
}

company_to_xy<-function(){
    library(reshape2)
    
    bus.file<-"./tpprocess/business_data.csv"
    com.file<-"./tpprocess/company_data.csv"
    sep.file<-"./tpprocess/specific_data.csv"
    
    file.list<-c(bus.file,com.file,sep.file)
    
    total.df<-data.frame()
    
    for(i in 1:length(file.list)){
        data.df<-read.csv(file.list[i],stringsAsFactors = FALSE)
        total.df<-rbind(total.df,data.df)
    }
    
    total.df$R_cat=0;total.df$P_cat=0;total.df$M_cat=0
    
    total.df$R_cat[grep("R_cat",total.df$IND_CAT)]=1
    total.df$P_cat[grep("P_cat",total.df$IND_CAT)]=1
    total.df$M_cat[grep("M_cat",total.df$IND_CAT)]=1
    
    melt.df<-melt(total.df,id=c("Response_X","Response_Y"),measure.vars = c("R_cat","P_cat","M_cat"))
    cast.df<-dcast(melt.df,Response_X+Response_Y~variable,sum)
    cast.df<-cast.df[!is.na(cast.df$Response_X),]
    
    write.csv(cast.df,file="./tpprocess/xy_map.csv",row.names=FALSE)
}