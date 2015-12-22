fit_data<- function(){
    bus.root<-"./tpprocess/business"
    com.root<-"./tpprocess/company"
    sep.root<-"./tpprocess/specific"
    
    bus<-dir(bus.root)
    com<-dir(com.root)
    sep<-dir(sep.root)
    
    fld.list<-list(list(bus.root,bus),list(com.root,com),list(sep.root,sep))
    
    address.df<-read_address()
    lackadd<-c()
    
    for (i in 1:length(fld.list)){
        file.root<-fld.list[[i]][[1]]
        
        print(fld.list[[i]])
        
        total.df<-NULL
        
        for (j in 1:length(fld.list[[i]][[2]])){
            file.path<-paste0(file.root,"/",fld.list[[i]][[2]][j])
            data.df<-data_parse(file.path)
            
            merge.df<-merge(data.df,address.df,all.x = TRUE)
            
            lackadd<-c(lackadd,as.character(merge.df[is.na(merge.df$Response_Address),1]))
            lackadd<-unique(lackadd)
            
            total.df<-rbind(total.df,merge.df)
        }
        
        total.df<-unique(total.df)
        write.csv(total.df,file=paste0(file.root,"_data.csv"),row.names=FALSE)
    }
    
    lackadd.df<-data.frame(Response_Address=as.character(lackadd))
    
    write.csv(lackadd.df,file="./tpprocess/lackaddress.csv",row.names=FALSE)
    ##return(lackadd.df)
}

compile_lack_response<-function(){
    file.root<-"./tpprocess/"
    file.vector<-dir(file.root)[grep("_re",dir(file.root))]
    
    address.df<-NULL
    for(i in 1:length(file.vector)){
        file.df<-read.csv(paste0(file.root,file.vector[i]),stringsAsFactors = FALSE)
        address.df<-rbind(address.df,file.df)
    }
    write.csv(address.df,file="./tpprocess/lackresponse.csv",row.names=FALSE)
}

read_address<-function(){
    address.root<-"./tpprocess/"
    address.file<-"addressbook.csv"
    address.path<-paste0(address.root,address.file)
    
    address.df<-read.csv(address.path,stringsAsFactors = FALSE)
    ##address.df$fit_Address<-gsub("NA","",paste0(address.df$city,address.df$dist,address.df$road,address.df$lane,address.df$alley,address.df$number))
    address.df$fit_Address<-gsub("NA","",paste0(address.df$city,address.df$road,address.df$lane,address.df$alley,address.df$number))
    
    return(address.df)
}

data_parse<-function(file.path){
    
    number.df<-data.frame("en"=as.character(c(1:9)),"zh"=c("一","二","三","四","五","六","七","八","九"),"cap"=c("１","２","３","４","５","６","７","８","９"))
    
    data.df<-read.csv(file.path,stringsAsFactors = FALSE,header=FALSE,col.names=c("CID","CNAME","CADDR","CIND"))
    data.df<-data.df[!nchar(data.df$CNAME)==0,]
    data.df$CIND<-data.df$CID[1]
    
    for (i in 1:nrow(data.df)){
        address<-data.df[i,3]
        
        if(grepl("樓",address)) address<-gsub(gsub(".*樓(.*)$","\\1",address),"",address)
        
        for(n in 1:9) address<-gsub(number.df[n,3],number.df[n,1],address)
        address<-gsub("０","0",address)
        
        if(grepl("鄭州地下街商場|鄭州路地下街商場",address)) address<-"臺北市市民大道1段100號"
        if(grepl("永樂市場",address)) address<-"台北市大同區迪化街一段21號"
        
        if(grepl("段",address)){
            address.split<-strsplit(address,"段")[[1]]
            sec.check<-gsub(".*區(.*)","\\1",address.split[1])
            if(!grepl("路|街|大道",sec.check)){
                sec.check<-gsub("(.*)(東|西|南|北)([0-9])","\\1\\2路\\3",sec.check)
                address<-paste0(gsub("(.*區)(.*)","\\1",address.split[1]),sec.check,"段",address.split[2])
            }
        }
        
        if(grepl("臺北市民大道",address)) address<-gsub("臺北市民大道","臺北市市民大道",address)
        if(grepl("八德4",address)) address<-gsub("八德4","八德路4",address)
        if(grepl("八德硌",address)) address<-gsub("八德硌","八德路",address)
        if(grepl("羅斯福3",address)) address<-gsub("羅斯福3","羅斯福路3",address)
        if(grepl("羅斯福5",address)) address<-gsub("羅斯福5","羅斯福路5",address)
        if(grepl("中山北６",address)) address<-gsub("中山北６","中山北路6",address)
        if(grepl("中山北6",address)) address<-gsub("中山北6","中山北路6",address)
        if(grepl("信義2",address)) address<-gsub("信義2","信義路2",address)
        if(grepl("基隆2",address)) address<-gsub("基隆2","基隆路2",address)
        if(grepl("基隆1",address)) address<-gsub("基隆1","基隆路1",address)
        if(grepl("虎林242",address)) address<-gsub("虎林242","虎林街242",address)
        if(grepl("重慶南一",address)) address<-gsub("重慶南一","重慶南路一",address)
        if(grepl("市民大道211號1段",address)) address<-gsub("市民大道211號1段","市民大道1段211號",address)
        
        
        o.vector<-check_address(address)
        section<-strsplit(o.vector[2],"")[[1]]
        
        ##print(c(file.path,i,address))
        
        for (j in 1:length(section)){
            if(grepl("[0-9]",section[j])) section[j]<-as.character(number.df[grep(section[j],number.df[,1]),2])
        }
        o.vector[2]<-paste0(section,collapse="")
        data.df$o.dist[i]<-gsub(".*市(.*區).*","\\1",address)
        data.df$o.road[i]<-gsub("NA","",paste0(o.vector[1],o.vector[2]))
        data.df$o.lane[i]<-o.vector[3]
        data.df$o.alley[i]<-o.vector[4]
        data.df$o.number[i]<-paste0(strsplit(o.vector[5],"號")[[1]][1],"號")
    }
    
    ##data.df$fit_Address<-gsub("NA","",paste0("臺北市",data.df$o.dist,data.df$o.road,data.df$o.lane,data.df$o.alley,data.df$o.number))
    data.df$fit_Address<-gsub("NA","",paste0("臺北市",data.df$o.road,data.df$o.lane,data.df$o.alley,data.df$o.number))
    data.df<-data.frame(CID=data.df$CID,CNAME=data.df$CNAME,CADDR=data.df$CADDR,fit_Address=data.df$fit_Address,stringsAsFactors = FALSE)
    
    return(data.df)
}

sep_lackaddress<-function(){
    file.root<-"./tpprocess/"
    file.name<-"lackaddress.csv"
    lackaddress.df<-read.csv(paste0(file.root,file.name),stringsAsFactors = FALSE)
    
    for(c in 1:ceiling(nrow(lackaddress.df)/10000)){
        start<-(c-1)*10000+1
        end<-c*10000
        seg<-lackaddress.df[start:end,1]
        seg<-seg[!is.na(seg)]
        
        output.df<-cbind(c(start:(start+length(seg)-1)),seg,c(NA),c(NA),c(NA))
        colnames(output.df)<-c("id","Address","Response_Address","Response_X","Response_Y")
        write.csv(output.df,paste0(file.root,c,".csv"),na="",row.names=FALSE)
    }
}