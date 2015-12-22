combine_addressbook<-function(){
    file.root<-"./tpprocess/"
    lackresponse.file<-"lackresponse.csv"
    addressbook.file<-"addressbook.csv"
    
    lackresponse.df<-read.csv(paste0(file.root,lackresponse.file),stringsAsFactors = FALSE)
    addressbook.df<-read.csv(paste0(file.root,addressbook.file),stringsAsFactors = FALSE)
    
    lackresponse.df<-lackresponse.df[3:5]
    lackresponse.df<-lackresponse.df[!grepl("找不到",lackresponse.df$Response_Address),]
    lackresponse.df<-lackresponse.df[!grepl("Incorrect syntax|overflow",lackresponse.df$Response_Address),]
    
    lackresponse.df$city<-as.factor(gsub("^(.*(市|縣)).*","\\1",lackresponse.df[,1]))
    lackresponse.df$dist<-as.factor(gsub(".*市(.*區).*","\\1",lackresponse.df[,1]))
    lackresponse.df$city[grep("臺北市.*區",lackresponse.df$city)]<-"臺北市"
    lackresponse.df$village<-as.factor(gsub(".*區(.*里).*","\\1",lackresponse.df[,1]))
    lackresponse.df$neighbour<-as.factor(gsub(".*里(.*鄰).*","\\1",lackresponse.df[,1]))
    
    for(i in 1:nrow(lackresponse.df)){
        a.vector<-check_address(lackresponse.df[i,1])
        lackresponse.df$road[i]<-gsub("NA","",paste0(a.vector[1],a.vector[2]))
        lackresponse.df$lane[i]<-a.vector[3]
        lackresponse.df$alley[i]<-a.vector[4]
        lackresponse.df$number[i]<-a.vector[5]
        
        print(i)
    }
    
    addressbook.new<-unique(rbind(addressbook.df,lackresponse.df))
    
    write.csv(addressbook.new,file="./tpprocess/addressbook.csv",row.names=FALSE)
    ##return(addressbook.new)
}