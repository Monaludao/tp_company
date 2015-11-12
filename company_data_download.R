data_dl<-function(){
    library(bitops)
    library(RCurl)
    library(XML)
    library(httr)
    
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
        filelocale<-paste("raw/",ind.name[i],".zip",sep="")
        download.file(link$url,filelocale,mode="wb")
        
        setTxtProgressBar(pb, i)
    }
}