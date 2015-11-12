gl_api_runner<-function(day){
    library(bitops)
    library(RCurl)
    library(XML)
    library(httr)
    
    ##day<-as.integer(day)
    API.key<-"AIzaSyB8jeLo4qCSGQaT1XDTwLO7Zi_zJHIO4Cs"
    
    file.list<-dir("./tpdata")
    file.list<-file.list[order(file.list,decreasing=TRUE)]
    file.list<-paste0("./tpdata/",file.list)
    
    cnt<-1

    csv.file<-read.csv(file.list[1],header=FALSE,stringsAsFactors = FALSE,col.names=c("CID","CNAME","CADDR"),encoding="big5")
    
    for(i in 1:length(csv.file)){
        API.url<-"https://maps.googleapis.com/maps/api/geocode/json?"
        addr<-csv.file[i,3]
        geturl<-paste0(API.url,"address=",addr,"&key=",API.key)
        
        json<-getURL(geturl, encoding="utf-8")
        
        cnt=cnt+1
    }
    
    ##https://maps.googleapis.com/maps/api/geocode/json?address=1600+Amphitheatre+Parkway,+Mountain+View,+CA&key=YOUR_API_KEY
    
}