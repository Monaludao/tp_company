gl_convert<-function(){
    file.root<-"./tpdata/"
    today<-as.character(Sys.Date())
    api.limit<-2450
    
    if(file.exists("glapi_record.csv")){
        record.df<-read.csv("glapi_record.csv",col.names=c("Date","cnt","file","row"),stringsAsFactors = FALSE)
    } else {
        record.df<-data.frame(Date=today,cnt="0",file="tp45.csv",record="0",stringsAsFactors = FALSE)
    }
    
    last.record<-as.vector(as.matrix(record.df[nrow(record.df),]))
    last.date<-last.record[1]
    last.cnt<-as.integer(last.record[2])
    last.file<-last.record[3]
    last.row<-as.integer(last.record[4])
    
    if(last.date==today){
        if(last.cnt>=api.limit) stop("API Limit reached!")
    } else {
        last.cnt<-0
    }
    
    file.list<-dir("./tpdata")
    file.list<-file.list[order(file.list,decreasing=TRUE)]
    
    act.file<-file.list[file.list==last.file]
    csv.df<-getcsv(act.file,file.root)
    
    if(nrow(csv.df) <= last.row){
        act.file<-file.list[grep(last.file,file.list)+1]
        csv.df<-getcsv(act.file)
        start.row<-1
    } else {
        start.row<-last.row+1
    }
    
    if((nrow(csv.df)-start.row+1)<=(api.limit-last.cnt)){
        end.row<-nrow(csv.df)
    } else {
        end.row<-(start.row+(api.limit-last.cnt)-1)
    }
    
    run.df<-csv.df[start.row:end.row,]
    
    gl_api_runner(run.df,act.file)
    
    if(length(record.df[record.df$Date==today])==0){
        n.cnt<-end.row-start.row+1
        record.df<-rbind(record.df,c(today,n.cnt,act.file,end.row))
    } else {
        n.cnt<-last.cnt+(end.row-start.row+1)
        record.df[record.df$Date==today,2]<-n.cnt
        record.df[record.df$Date==today,3]<-act.file
        record.df[record.df$Date==today,4]<-end.row
    }
    
    print(record.df)
    
    write.csv(record.df,"glapi_record.csv",row.names=FALSE)
}

getcsv<-function(file.name,file.root){
    file.path<-paste0(file.root,file.name)
    csv.df<-read.csv(file.path,header=FALSE,stringsAsFactors = FALSE,col.names=c("CID","CNAME","CADDR"),encoding="big5")
    return(csv.df)
}

gl_api_runner<-function(run.df,act.file){
    library(bitops)
    library(RCurl)
    library(XML)
    library(httr)
    
    Sys.setlocale(category='LC_ALL', locale='C')
    
    root<-"https://maps.googleapis.com/maps/api/geocode/json?"
    API.key<-"AIzaSyB8jeLo4qCSGQaT1XDTwLO7Zi_zJHIO4Cs"
    
    print(paste("File contains",nrow(run.df),"rows."))
    
    pb <- txtProgressBar(max = nrow(run.df), style = 3)
    
    for(i in 1:nrow(run.df)){
        
        row.id<-rownames(run.df[i,])
        
        addr<-iconv(run.df[i,3],"big5","utf-8")
        geturl<-paste0(root,"address=",addr,"&key=",API.key)
        geturl<-URLencode(geturl)
        
        json<-getURL(geturl)
        json<-iconv(json,"UTF-8","BIG5")
        
        output.name<-paste0(act.file,paste0(rep("0",(5-nchar(row.id))),collapse=""),row.id,".json")
        output.name<-gsub(".csv","",output.name)
        output.path<-paste0("./tpold/json/",output.name)
        
        write(json,file=output.path)
        
        setTxtProgressBar(pb, i)
        
        Sys.sleep(0.5)
        
        #print(json)
    }
}