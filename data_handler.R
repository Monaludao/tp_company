data_handle<-function(file.num){
    file.num=1
    
    file.list<-dir("./tpdata")
    file.path<-paste0("./tpdata/",file.list)
    
    csv.df<-read.csv(file.path[file.num],stringsAsFactors = FALSE)
    
    csv.df$CID<-as.character(csv.df$CID)
    for (i in 1:nrow(csv.df)){
        csv.df$CID[i]<-check_CID(csv.df$CID[i])
    }
                       
    csv.df$village<-gsub(".*區(.*里).*","\\1",csv.df[,4])
    csv.df$village<-as.character(csv.df$village)
    
    csv.df$neighbour<-gsub(".*里(.*鄰).*","\\1",csv.df[,4])
    csv.df$neighbour<-as.character(csv.df$neighbour)
    
    
}

check_CID<-function(CID){
    if(nchar(CID)!=8){
        CID<-paste0(paste(rep("0",(8-nchar(CID))),collapse = ""),CID)
    }
    return(CID)
}