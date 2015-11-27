data_generate<-function(){
    #Sys.setlocale(category='LC_ALL', locale='C')
    
    #source("data_handler.R")
    
    csv.df<-data_handle(1)
    
    number.df<-data.frame("en"=as.character(c(1:9)),"zh"=c("一","二","三","四","五","六","七","八","九"))
    
    csv.com<-read.csv("./tpdata/tp04.csv",stringsAsFactors = FALSE,header=FALSE,col.names=c("CID","CNAME","CADDR"))
    
    for (i in 1:nrow(csv.com)){
        address<-csv.com[i,3]
        o.vector<-check_address(address)
        section<-strsplit(o.vector[2],"")[[1]]
        for (j in 1:length(section)){
            if(grepl("[0-9]",section[j])) section[j]<-as.character(number.df[grep(section[1],number.df[,1]),2])
        }
        o.vector[2]<-paste0(section,collapse="")
        csv.com$o.dist[i]<-gsub(".*市(.*區).*","\\1",address)
        csv.com$o.road[i]<-gsub("NA","",paste0(o.vector[1],o.vector[2]))
        csv.com$o.lane[i]<-o.vector[3]
        csv.com$o.alley[i]<-o.vector[4]
        csv.com$o.number[i]<-paste0(strsplit(o.vector[5],"號")[[1]][1],"號")
        
        search.df<-csv.df[csv.df$dist==csv.com$o.dist[i],]
        search.df<-search.df[search.df$road==csv.com$o.road[i],]
        if(!is.na(csv.com$o.lane[i])){
            search.df<-search.df[search.df$lane==csv.com$o.lane[i],]
        } else {
            search.df<-search.df[is.na(search.df$lane),]
        }
        if(!is.na(csv.com$o.alley[i])){
            search.df<-search.df[search.df$alley==csv.com$o.alley[i],]
        } else {
            search.df<-search.df[is.na(search.df$alley),]
        }
        search.df<-search.df[search.df$number==csv.com$o.number[i],]
        search.df<-search.df[!is.na(search.df[,1]),]
        
        if(length(unique(search.df$Response_X))>1){
            print(c(unique(search.df$Response_Address),i))
        }
    }
}
