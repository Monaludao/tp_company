data_handle<-function(){
    file.list<-dir("./tpdata")
    file.list<-file.list[grep(".csv",file.list)]
    file.list<-file.list[grep("done",file.list)]
    file.path<-paste0("./tpdata/",file.list)
    
    csv.df<-data.frame()
    
    for(i in 1:length(file.list)){
        print (file.list[i])
        csv.df<-rbind(csv.df,set_csv(file.path[i]))
    }
    
    write.csv(csv.df,file="./tpprocess/totaldata.csv",row.names=FALSE)
    
    address.df<-unique(csv.df[!grepl("找不到",csv.df[,4]),-c(1:3,15)])
    
    write.csv(address.df,file="./tpprocess/addressbook.csv",row.names=FALSE)
    
}
    
set_csv<-function(file.path){    
    csv.df<-read.csv(file.path,stringsAsFactors = FALSE)
    
    csv.df[grep("<U+",csv.df[,4]),4]<-"找不到指定的門牌地址。:UTF16"
                           
    csv.df$city<-as.factor(gsub("^(.*(市|縣)).*","\\1",csv.df[,4]))
    csv.df$dist<-as.factor(gsub(".*市(.*區).*","\\1",csv.df[,4]))
    csv.df$village<-as.factor(gsub(".*區(.*里).*","\\1",csv.df[,4]))
    csv.df$neighbour<-as.factor(gsub(".*里(.*鄰).*","\\1",csv.df[,4]))
    
    csv.df$CID<-as.character(csv.df$CID)
    csv.df$road<-NA
    csv.df$lane<-NA
    csv.df$alley<-NA
    csv.df$number<-NA
    
    for (i in 1:nrow(csv.df)){
        csv.df$CID[i]<-check_CID(csv.df$CID[i])
        address.vector<-check_address(csv.df$Response_Address[i])
        csv.df$road[i]<-gsub("NA","",paste0(address.vector[1],address.vector[2]))
        csv.df$lane[i]<-address.vector[3]
        csv.df$alley[i]<-address.vector[4]
        csv.df$number[i]<-address.vector[5]
    }
    
    csv.df$CID<-as.factor(csv.df$CID)
    csv.df$CNAME<-as.factor(csv.df$CNAME)
    csv.df$Response_Address<-as.factor(csv.df$Response_Address)
    csv.df$road<-as.factor(csv.df$road)
    csv.df$floor<-gsub(".*號(.*)$","\\1",csv.df[,3])
    
    return(csv.df)
}

check_CID<-function(CID){
    if(nchar(CID)!=8){
        CID<-paste0(paste(rep("0",(8-nchar(CID))),collapse = ""),CID)
    }
    return(CID)
}

check_address<-function(address){
    if(grepl("新市街",address)) {
        road<-"新市街"
    } else {
        road<-gsub(".*(市|區|里|鄰)(.*(路|街|大道)).*","\\2",address)
    }
    if(road == "民大道") road = "市民大道"
    
    if(grepl("段",address)){
        section<-gsub(".*(路|街|大道)(.*段).*","\\2",address)
        lane.vector<-check_lane(strsplit(address,("段"))[[1]][2])
    } else {
        section<-NA
        ##lane.vector<-check_lane(strsplit(address,("路|街|大道"))[[1]][2])
        lane.vector<-check_lane(gsub(".*(路|街|大道)(.*)","\\2",address))
    }
    return(c(road,section,lane.vector))
}

check_lane<-function(address){
    if(grepl("巷",address)){
        lane<-gsub("^(.*巷).*","\\1",address)
        alley.vector<-check_alley(strsplit(address,("巷"))[[1]][2])
    } else {
        lane<-NA
        alley.vector<-check_alley(address)
    }
    return(c(lane,alley.vector))
}

check_alley<-function(address){
    if(grepl("弄",address)){
        alley<-gsub("^(.*弄).*","\\1",address)
        number<-strsplit(address,("弄"))[[1]][2]
    } else {
        alley<-NA
        number<-address
    }
    
    if(grepl("、|\\.|，",number)) {
        number<-paste0(strsplit(number,"、|\\.|，")[[1]][1],"號")
    }
    
    return(c(alley,number))
}