csv_filter<-function(){
    csv.list<-paste0("./csv/",dir("csv/"))
    for (i in 1:length(dir("csv/"))){
        csv.lines<-readLines(csv.list[i],encoding="UTF-8")
        csv.lines<-gsub("\"","",csv.lines)
        csv.lines<-csv.lines[grep("臺北市",csv.lines)]
        
        ##addr<-gsub(".*,.*,(臺北市.*$)","\\1",csv.lines[i])
        ##csv.lines<-gsub(".*,.*,(臺北市.*$)",gsub(","," ",addr),csv.lines[i])
        
        ##csv.lines<-strsplit(csv.lines,",")    
        c.number<-paste0(rep("0",(2-nchar(as.character(i)))),as.character(i))
        
        write(csv.lines,file=paste0("./tpdata/tp",c.number,".csv"))
    }
    
}

csv_convert<-function(){
    cnt<-0
    
    for(i in 1:length(dir("./tpdata/"))){
        file.name<-paste0("./tpdata/",dir("./tpdata/")[i])
        csv.df<-read.csv(file.name,header=FALSE,sep=",",stringsAsFactors = FALSE,col.names=c("CID","CNAME","CADDR"))
        c.number<-paste0(rep("0",(2-nchar(as.character(i)))),as.character(i))
        
        cnt<-cnt+nrow(csv.df)
        
        for(c in 1:ceiling(nrow(csv.df)/10000)){
            start<-(c-1)*10000+1
            end<-c*10000
            seg<-csv.df[start:end,3]
            seg<-seg[!is.na(seg)]
            
            output.df<-cbind(c(start:(start+length(seg)-1)),seg,c(NA),c(NA),c(NA))
            colnames(output.df)<-c("id","Address","Response_Address","Response_X","Response_Y")
            write.csv(output.df,paste0("./tpupload/tp",c.number,"_",c,".csv"),na="",row.names=FALSE)
        }
    }
    print(cnt)
}