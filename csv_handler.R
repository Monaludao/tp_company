csv_filter<-function(){
    dir.root<-"./csv/"
    dir.list<-dir(dir.root)
    for (d in 1:length(dir.list)){
        dir.name<-dir.list[d]
        csv.root<-paste0(dir.root,dir.name,"/")
        csv.list<-dir(csv.root)
        csv.path<-paste0(csv.root,csv.list)
        
        for (i in 1:length(csv.list)){
            csv.name<-gsub(".csv","",csv.list[i])
            csv.lines<-readLines(csv.path[i],encoding="UTF-8")
            csv.lines<-gsub("\"","",csv.lines)
            csv.lines<-csv.lines[grep("臺北市",csv.lines)]
            csv.lines<-paste0(csv.lines,",",csv.name)
            
            c.number<-paste0(rep("0",(2-nchar(as.character(i)))),as.character(i))
            c.path<-paste0("./tpprocess/",dir.name,"/")
            
            write(csv.lines,file=paste0(c.path,c.number,csv.name,".csv"))
        }
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