data_dl<-function(){
    library(bitops)
    library(RCurl)
    library(XML)
    library(httr)
    
    ##特定行業登記
    get_url<-getURL("http://data.gcis.nat.gov.tw/od/detail?oid=20D47ED7-D24E-47FB-B746-252AF7A7FAFC", encoding="utf-8")
    url_parse<-htmlParse(get_url, encoding="utf-8")
    
    ind.name<-xpathSApply(url_parse,"//a[@class='file CSV']",xmlAttrs)[2,]
    ind.name<-iconv(ind.name,"utf-8","big5")
    
    ind.file<-xpathSApply(url_parse,"//a[@class='file CSV']",xmlAttrs)[3,]
    link.root<-"http://data.gcis.nat.gov.tw"
    ind.file<-paste0(link.root,ind.file)
    
    pb <- txtProgressBar(max = length(ind.file), style = 3)
    
    for(i in 1:length(ind.file)){
        link<-GET(ind.file[i])
        filelocale<-paste("raw/specific_",ind.name[i],".zip",sep="")
        download.file(link$url,filelocale,mode="wb")
        
        setTxtProgressBar(pb, i)
    }
    
    ##公司登記
    get_url<-getURL("http://data.gcis.nat.gov.tw/od/detail?oid=8661DAA7-EB26-4684-9833-EE8CFB414A15", encoding="utf-8")
    url_parse<-htmlParse(get_url, encoding="utf-8")
    
    ind.name<-xpathSApply(url_parse,"//a[@class='file CSV']",xmlAttrs)[2,]
    ind.name<-iconv(ind.name,"utf-8","big5")
    
    ind.file<-xpathSApply(url_parse,"//a[@class='file CSV']",xmlAttrs)[3,]
    link.root<-"http://data.gcis.nat.gov.tw"
    ind.file<-paste0(link.root,ind.file)
    
    pb <- txtProgressBar(max = length(ind.file), style = 3)
    
    for(i in 1:length(ind.file)){
        ind.number<-paste0(paste(rep("0",2-nchar(as.character(i))),collapse = ""),i)
        
        link<-GET(ind.file[i])
        filelocale<-paste("raw/company_",ind.number,ind.name[i],".zip",sep="")
        download.file(link$url,filelocale,mode="wb")
        
        setTxtProgressBar(pb, i)
    }
    
    ##商業登記
    get_url<-getURL("http://data.gcis.nat.gov.tw/od/detail?oid=AC0AF2AC-6349-46E9-91FE-0F9C36CCC3AB#", encoding="utf-8")
    url_parse<-htmlParse(get_url, encoding="utf-8")
    
    ind.name<-xpathSApply(url_parse,"//a[@class='file CSV']",xmlAttrs)[2,]
    ind.name<-iconv(ind.name,"utf-8","big5")
    
    ind.file<-xpathSApply(url_parse,"//a[@class='file CSV']",xmlAttrs)[3,]
    link.root<-"http://data.gcis.nat.gov.tw"
    ind.file<-paste0(link.root,ind.file)
    
    pb <- txtProgressBar(max = length(ind.file), style = 3)
    
    for(i in 1:length(ind.file)){
        ind.number<-paste0(paste(rep("0",2-nchar(as.character(i))),collapse = ""),i)
        
        link<-GET(ind.file[i])
        filelocale<-paste("raw/bussiness_",ind.number,ind.name[i],".zip",sep="")
        download.file(link$url,filelocale,mode="wb")
        
        setTxtProgressBar(pb, i)
    }
}

