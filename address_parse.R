fit_data<- function(){
    library(reshape2)
    
    bus.root<-"./tpprocess/business"
    com.root<-"./tpprocess/company"
    sep.root<-"./tpprocess/specific"
    
    bus<-dir(bus.root)
    com<-dir(com.root)
    sep<-dir(sep.root)
    
    fld.list<-list(list(bus.root,bus),list(com.root,com),list(sep.root,sep))
    cnt.table<-data.frame(stringsAsFactors = FALSE)
    
    address.df<-read_address()

    lackadd<-c()
    
    for (i in 1:length(fld.list)){
        file.root<-fld.list[[i]][[1]]
        
        print(fld.list[[i]])
        
        total.df<-NULL
        total.rows<-0
        
        pb <- txtProgressBar(max = length(fld.list[[i]][[2]]), style = 3)
        
        for (j in 1:length(fld.list[[i]][[2]])){
            file.path<-paste0(file.root,"/",fld.list[[i]][[2]][j])
            data.df<-data_parse(file.path)
            total.df<-rbind(total.df,data.df)
            total.rows<-total.rows+nrow(data.df)
        
            setTxtProgressBar(pb, j)
        }
        
        print(c("melt go",Sys.time()));total.df<-melt(total.df,id=c("CID","CNAME","CADDR","fit_Address"),measure.vars = c("M_cat","P_cat","R_cat","e_cat"));print(c("melt done",Sys.time()))
        print(c("cast go",Sys.time()));total.df<-dcast(total.df,CID+CNAME+CADDR+fit_Address~variable,sum);print(c("cast done",Sys.time()))
        print(c("ind_cat go",Sys.time()));total.df$IND_CAT<-colnames(total.df)[(max.col(total.df[,5:8]))+4];print(c("ind_cat done",Sys.time()))
        print(c("merge go",Sys.time()));total.df<-merge(total.df,address.df,all.x = TRUE);print(c("merge done",Sys.time()))
        
        lackadd<-total.df$fit_Address[is.na(total.df$Response_Address)]
        cnt.table<-rbind(cnt.table,c(i,nrow(total.df),sum(!is.na(total.df$Response_Address)),sum(is.na(total.df$Response_Address))))
        
        total.df$M_cat<-NULL
        total.df$P_cat<-NULL
        total.df$R_cat<-NULL
        total.df$e_cat<-NULL
        total.df$fit_Address<-NULL
        write.csv(total.df,file=paste0(file.root,"_data.csv"),row.names=FALSE)
    }
    
    lackadd.df<-data.frame(Response_Address=as.character(lackadd))
    
    colnames(cnt.table)=c("item","rows","fit","lack")
    cnt.table$percent<-cnt.table$lack/(cnt.table$fit+cnt.table$lack)
    print(cnt.table)
    
    write.csv(lackadd.df,file="./tpprocess/lackaddress.csv",row.names=FALSE)
}

check_category<-function(ind.name){
    category.df<-read.csv("category.csv",stringsAsFactors = FALSE)
    category.df$Business_Item_Content<-NULL
    
    category.name<-category.df$Category[grep(paste0("^",ind.name,"(工程)?(業)?$"),category.df$Business_Item_Desc)]
    if(ind.name == "其他服務" ||ind.name == "逾期應收帳款管理服務") category.name<-category.df$Category[grep("^未分類其他服務業$",category.df$Business_Item_Desc)]
    if(length(category.name)==0) category.name<-category.df$Category[grep(paste0("^",gsub("室內","",ind.name),"(工程)?(業)?$"),category.df$Business_Item_Desc)]
    if(length(category.name)==0) category.name<-category.df$Category[grep(paste0("^",gsub("行","",ind.name),"(工程)?(業)?$"),category.df$Business_Item_Desc)]
    if(length(category.name)==0) category.name<-category.df$Category[grep(paste0("^",gsub("服務","",ind.name),"(工程)?(業)?$"),category.df$Business_Item_Desc)]
    if(length(category.name)==0) category.name<-unique(category.df$Category[grep(paste0("^",ind.name,"(工程)?(業)?$"),category.df$Subcategories_Name)])
    if(length(category.name)==0) category.name<-unique(category.df$Category[grep(paste0("^",gsub("服務","",ind.name),"(工程)?(業)?$"),category.df$Subcategories_Name)])
    if(length(category.name)==0) category.name<-unique(category.df$Category[grep(substr(ind.name,1,2),category.df$Business_Item_Desc)])
    if(length(category.name)==0) category.name<-unique(category.df$Category[grep(substr(ind.name,3,4),category.df$Business_Item_Desc)])
    
    return(category.name)
    
    #if(category.name %in% c("A","B","C","D","E","G")) {return("M")}
    #else if(category.name %in% c("H","I")) {return("P")}
    #else if(category.name %in% c("F","J")) {return("R")}
    #else {return("e")}
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
    address.df$fit_Address<-gsub("NA","",paste0(address.df$city,address.df$road,address.df$lane,address.df$alley,address.df$number))
    
    address.df$road<-NULL
    address.df$lane<-NULL
    address.df$alley<-NULL
    address.df$number<-NULL
    
    return(address.df)
}

data_parse<-function(file.path){
    number.df<-data.frame("en"=as.character(c(1:9)),"zh"=c("一","二","三","四","五","六","七","八","九"),
                          "cap"=c("１","２","３","４","５","６","７","８","９"),stringsAsFactors=FALSE)
    
    data.df<-read.csv(file.path,stringsAsFactors = FALSE,header=FALSE,col.names=c("CID","CNAME","CADDR","CIND"))
    data.df<-data.df[!nchar(data.df$CNAME)==0,]
    
    IND_CAT<-check_category(data.df$CIND[1])
    
    for (i in 1:nrow(data.df)){
        data.df$CID[i]<-check_CID(data.df$CID[i])
        
        address<-data.df[i,3]
        address<-address_correct(address)
        
        o.vector<-check_address(address)
        section<-strsplit(o.vector[2],"")[[1]]
        
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
    
    data.df$fit_Address<-gsub("NA","",paste0("臺北市",data.df$o.road,data.df$o.lane,data.df$o.alley,data.df$o.number))
    #data.df<-data.frame(CID=data.df$CID,CNAME=data.df$CNAME,CADDR=data.df$CADDR,fit_Address=data.df$fit_Address,CIND=data.df$CIND,IND_CAT=data.df$IND_CAT,stringsAsFactors = FALSE)
    data.df<-data.frame(CID=data.df$CID,CNAME=data.df$CNAME,CADDR=data.df$CADDR,fit_Address=data.df$fit_Address,stringsAsFactors = FALSE)
    
    data.df$M_cat<-0;data.df$P_cat<-0;data.df$R_cat<-0;data.df$e_cat<-0
    
    if(IND_CAT %in% c("A","B","C","D","E","G")) {data.df$M_cat<-1}
    else if(IND_CAT %in% c("H","I")) {data.df$P_cat<-1}
    else if(IND_CAT %in% c("F","J")) {data.df$R_cat<-1}
    else {data.df$e_cat<-1}
    
    return(data.df)
}

address_correct<-function(address){
    number.df<-data.frame("en"=as.character(c(1:9)),"zh"=c("一","二","三","四","五","六","七","八","九"),
                          "cap"=c("１","２","３","４","５","６","７","８","９"),stringsAsFactors=FALSE)
    
    ##去除樓層後方文字
    if(grepl("樓",address)) address<-gsub(gsub(".*樓(.*)$","\\1",address),"",address)
    ##全型數字轉半型
    for(n in 1:9) address<-gsub(number.df[n,3],number.df[n,1],address)
    address<-gsub("０|Ｏ","0",address)
    ##去除"臨"字
    address<-gsub("(.*)(臨)([0-9]+((之|-)[0-9]+)?號$)","\\1\\3",address)
    ##去除"(南)"字
    address<-gsub("\\(南\\)","",address)
    ##東西南北路少"路"字就補上
    if(grepl("段",address)){
        address.split<-strsplit(address,"段")[[1]]
        sec.check<-gsub(".*區(.*)","\\1",address.split[1])
        if(!grepl("路|街|大道",sec.check)){
            sec.check<-gsub("(.*)(東|西|南|北)([0-9])","\\1\\2路\\3",sec.check)
            address<-paste0(gsub("(.*區)(.*)","\\1",address.split[1]),sec.check,"段",address.split[2])
        }
    }
    ##市場名稱改成市場地址
    #if(grepl("臺北市大安區大安路1段77號",address)) address<-"臺北市大安區大安路1段77號"
    #if(grepl("臺北市中正區忠孝西路1段50之1號",address)) address<-"臺北市中正區忠孝西路1段50之1號"
    #if(grepl("臺北市萬華區環河南路1段162號",address)) address<-"臺北市萬華區環河南路1段162號"
    if(grepl("中山地下街|臺北市(大同區)?長安西路52之1號",address)) address<-"臺北市大同區長安西路52之1號"
    if(grepl("東區地下街",address)) address<-"臺北市大安區大安路1段77號"
    if(grepl("光華數位新天地",address)) address<-"臺北市中正區市民大道3段8號"
    if(grepl("台北地下街|鄭州(路)?地下(街)?商場|臺北市中正區市民大道(一|1)段(一|1)00號",address)) {
        address<-"臺北市中正區市民大道1段100號"}
    if(grepl("商場",address)){
        if(grepl("龍山商場",address)) address<-"臺北市萬華區和平西路三段120號"
        if(grepl("龍山寺地下街商場",address)) address<-"臺北市萬華區西園路1段145號"
        if(grepl("河濱商場",address)) address<-"臺北市萬華區環河南路一段286號"
    }
    if(grepl("市場",address)){
        if(grepl("永樂市場",address)) address<-"臺北市大同區迪化街一段21號"
        if(grepl("永吉市場",address)) address<-"臺北市信義區永吉路278巷1弄30號"
        if(grepl("西門市場",address)) address<-"臺北市萬華區西寧南路177號"
        if(grepl("水源市場",address)) address<-"臺北市中正區羅斯福路四段92號"
        if(grepl("永吉市場",address)) address<-"臺北市信義區永吉路278巷1弄30號"
        if(grepl("自強市場",address)) address<-"臺北市中正區重慶南路3段"
        if(grepl("新興(綜合)?市場",address)) address<-"臺北市中山區林森北路487號"
        if(grepl("雙連市場",address)) address<-"臺北市大同區民生西路198號"
        if(grepl("成功(臨時攤棚)?市場",address)) address<-"臺北市大安區四維路192巷"
        if(grepl("松江市場",address)) address<-"臺北市中山區錦州街222號"
        if(grepl("南門市場",address)) address<-"臺北市中正區羅斯福路一段8號"
        if(grepl("環南(綜合)?(公有)?市場",address)) address<-"臺北市萬華區環河南路二段245號"
        if(grepl("直興市場",address)) address<-"臺北市萬華區康定路172巷1號"
        if(grepl("松山市場",address)) address<-"臺北市松山區八德路4段679號"
    }
    ##錯誤街道名修正
    if(grepl("虎林242",address)) address<-gsub("虎林242","虎林街242",address)
    if(grepl("成都106",address)) address<-gsub("成都106","成都路106",address)
    if(grepl("松江204",address)) address<-gsub("松江204","松江路204",address)
    if(grepl("社子32",address)) address<-gsub("社子32","社子街32",address)
    if(grepl("廈門街",address)) address<-gsub("廈門街","?門街",address)
    if(grepl("(南)中原街",address)) address<-gsub("(南)中原街","中原街",address)        
    if(grepl("八德4",address)) address<-gsub("八德4","八德路4",address)
    if(grepl("八德硌",address)) address<-gsub("八德硌","八德路",address)
    if(grepl("羅斯福[0-9]",address)) address<-gsub("(羅斯福)([0-9])","\\1路\\2",address)
    if(grepl("信義2",address)) address<-gsub("信義2","信義路2",address)
    if(grepl("基隆[0-9]",address)) address<-gsub("(基隆)([0-9])","\\1路\\2",address)
    if(grepl("市民大道211號1段",address)) address<-gsub("市民大道211號1段","市民大道1段211號",address)
    
    return(address)
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