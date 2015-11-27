data_handle<-function(file.num){
    file.num=1
    
    file.list<-dir("./tpdata")
    file.list<-file.list[grep(".csv",file.list)]
    file.path<-paste0("./tpdata/",file.list)
    
    csv.df<-read.csv(file.path[file.num],stringsAsFactors = FALSE)
    
    csv.df$CID<-as.character(csv.df$CID)
    for (i in 1:nrow(csv.df)){
        csv.df$CID[i]<-check_CID(csv.df$CID[i])
    }

    csv.df[grep("<U+",csv.df[,4]),4]<-"找不到指定的門牌地址。:UTF16"
                           
    csv.df$city<-as.factor(gsub("^(.*(市|縣)).*","\\1",csv.df[1,4]))
    csv.df$dist<-as.factor(gsub(".*市(.*區).*","\\1",csv.df[1,4]))
    csv.df$village<-as.factor(gsub(".*區(.*里).*","\\1",csv.df[,4]))
    csv.df$neighbour<-as.factor(gsub(".*里(.*鄰).*","\\1",csv.df[,4]))
    csv.df$road<-as.factor(gsub(".*鄰(.*(路|街|大道)).*","\\1",csv.df[,4]))
    
    csv.df$sec<-NA
    for (i in 1:nrow(csv.df)){
        csv.df$sec[i]<-check_section(csv.df$Response_Address)
    }
    
}

check_CID<-function(CID){
    if(nchar(CID)!=8){
        CID<-paste0(paste(rep("0",(8-nchar(CID))),collapse = ""),CID)
    }
    return(CID)
}

check_section<-function(address){
    if(grepl("段",address)){
        section<-gsub(".*(路|街|大道)(.*段).*","\\2",address)
    } else {
        section<-NA
    }
    return(section)
}