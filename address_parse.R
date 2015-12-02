fit_address<- function(){
    bus.root<-"./tpprocess/business"
    com.root<-"./tpprocess/company"
    sep.root<-"./tpprocess/specific"
    
    bus<-dir(bus.root)
    com<-dir(com.root)
    sep<-dir(sep.root)
    
    fld.list<-list(list(bus.root,bus),list(com.root,com),list(sep.root,sep))
    
    address.df<-read_address()
    
    for (i in 1:length(fld.list)){
        file.root<-fld.list[[i]][[1]]
        
        for (j in 1:length(fld.list[[i]][[2]])){
            file.path<-paste0(file.root,"/",fld.list[[i]][[2]][j])
            data.df<-data_parse(file.path)
            head(data.df,1)
        }
    }
}

read_address<-function(){
    address.root<-"./tpprocess/"
    address.file<-"addressbook.csv"
    address.path<-paste0(address.root,address.file)
    
    address.df<-read.csv(address.path)##,encoding="UTF-8")
    
    return(address.df)
}

data_parse<-function(file.path){
    
    number.df<-data.frame("en"=as.character(c(1:9)),"zh"=c("一","二","三","四","五","六","七","八","九"))
    
    data.df<-read.csv(file.path,stringsAsFactors = FALSE,header=FALSE,col.names=c("CID","CNAME","CADDR","CIND"))
    
    for (i in 1:nrow(data.df)){
        address<-data.df[i,3]
        
        if(grepl("樓",address)) address<-gsub(gsub(".*樓(.*)$","\\1",address),"",address)
        
        if(grepl("段",address)){
            address.split<-strsplit(address,"段")[[1]]
            sec.check<-gsub(".*區(.*)","\\1",address.split[1])
            if(!grepl("路|街|大道",sec.check)){
                sec.check<-gsub("(.*)(東|西|南|北)([0-9])","\\1\\2路\\3",sec.check)
                address<-paste0(gsub("(.*區)(.*)","\\1",address.split[1]),sec.check,"段",address.split[2])
            }
        }
        
        ##if(grepl("民權東3",address)) address<-gsub("民權東3","民權東路3",address)
        if(grepl("八德4",address)) address<-gsub("八德4","八德路4",address)
        if(grepl("八德硌",address)) address<-gsub("八德硌","八德路",address)
        ##if(grepl("中央北4",address)) address<-gsub("中央北4","中央北路4",address)
        if(grepl("羅斯福3",address)) address<-gsub("羅斯福3","羅斯福路3",address)
        if(grepl("羅斯福5",address)) address<-gsub("羅斯福5","羅斯福路5",address)
        ##if(grepl("南京東3",address)) address<-gsub("南京東3","南京東路3",address)
        if(grepl("中山北６",address)) address<-gsub("中山北６","中山北路6",address)
        ##if(grepl("忠孝東5",address)) address<-gsub("忠孝東5","忠孝東路5",address)
        ##if(grepl("忠孝東3",address)) address<-gsub("忠孝東3","忠孝東路3",address)
        ##if(grepl("長安東1",address)) address<-gsub("長安東1","長安東路1",address)
        if(grepl("信義2",address)) address<-gsub("信義2","信義路2",address)
        if(grepl("基隆2",address)) address<-gsub("基隆2","基隆路2",address)
        
        o.vector<-check_address(address)
        section<-strsplit(o.vector[2],"")[[1]]
        
        print(c(file.path,i,address))
        
        for (j in 1:length(section)){
            ##print(section)
            
            if(grepl("[0-9]",section[j])) section[j]<-as.character(number.df[grep(section[1],number.df[,1]),2])
        }
        o.vector[2]<-paste0(section,collapse="")
        data.df$o.dist[i]<-gsub(".*市(.*區).*","\\1",address)
        data.df$o.road[i]<-gsub("NA","",paste0(o.vector[1],o.vector[2]))
        data.df$o.lane[i]<-o.vector[3]
        data.df$o.alley[i]<-o.vector[4]
        data.df$o.number[i]<-paste0(strsplit(o.vector[5],"號")[[1]][1],"號")
    }
    
    return(data.df)
}

