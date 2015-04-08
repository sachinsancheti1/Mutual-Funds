library(RCurl)
library(XML)
library(dplyr)
library(tidyr)
library(stringr)
library(gdata)
library(reshape2)
#
#"http://economictimes.indiatimes.com/lt-equity-fund--direct-plan/mffactsheet/schemeid-16212.cms"
#allfunds = "http://economictimes.indiatimes.com/equity-large-cap/mffundsbycategory.cms?primaryobj=equity&secondaryobj=large%20cap&period=r1year"

allfunds = tbl_df(read.xls("Types of funds.xlsx",header = T))
allfunds$newlink = paste("http://economictimes.indiatimes.com",allfunds$link,sep = "")
lint <- data.frame(hyperlink = character(),
                   fundname = character())

#linset <- hyperlinkoffunds(allfunds$newlink[1])
# This could take upto 10 mins
system.time({
  for(i in 1:27){
    linset <- tryCatch(allfunds$newlink[i] %>% 
                         hyperlinkoffunds,
                       error = function(e) list(hyperlink = paste("err",i),
                                                fundname = paste("err",i))
    )
    cat("Done with",i,"\n")
    lint <- rbind(lint,
                  data.frame(hyperlink = as.vector(linset$hyperlink),
                             fundname = as.vector(linset$fundname))
    )
  }
})

unilint <- unique(lint)
unilint <- convert.magic(unilint,"character")
unilint$newlink = paste("http://economictimes.indiatimes.com",unilint$hyperlink,sep = "")

alldata <- list()

# this could take upto 4 sec per page
# 5 hours for 3235 records
sink("mainextraction-logs.txt")
system.time({
  for(i in 1:dim(unilint)[1]){
    fname <- as.character(unilint$fundname[i])
    lnk <- as.character(unilint$newlink[i])
    alldetails <- fundextract(fname,lnk,i)
    alldata[i]<-list(alldetails)
    #alldata <- c(alldata,fund = list(alldetails))
  }
})
sink()
# fix those entries which did have problem
system.time({
  for(i in 1:dim(unilint)[1]){
    if (is.null(alldata[i][[1]]$problem)==TRUE){
      alldetails <- fundextract(fname,lnk,i)
      alldata[i]<-list(alldetails)
    }
  }
})

save(alldata,file = "alldata2015-04-06.RData")

# Function to parse the url from the html
parseIt <- function(x){
  htmlTreeParse(x, useInternalNodes = TRUE)
}

# Extraction of funds
fundextract <- function(fundname,newlink,i){
  cat("Working on:",as.character(fundname),"\n")
  ps <- tryCatch(parseIt(newlink),
                 error = function(e) NULL)
  alldetails <- tryCatch(
    list(smry = summarydata(ps),
         sector = sectorinfo(ps),
         fundbasics = fundinfo(ps),
         manager = fundmgrinfo(ps),
         #risk = try(riskinfo(ps)),
         returnsinformation = returnsinfo(ps),
         url = newlink,
         fundname = fundname,
         i = i,
         problems = is.null(ps)),
    error = function(e) cat("Error in",i,"\n"))
  cat("Done with:",i,"\n\n")
  return(alldetails)
}

# Function to get all the hyperlinks in the list of funds page for further data parsing
hyperlinkoffunds <- function(x = "http://economictimes.indiatimes.com/equity-large-cap/mffundsbycategory.cms?primaryobj=equity&secondaryobj=large%20cap&period=r1year"){
  ts = parseIt(x)
  cat("working on",x,"\n\n")
  tt = xpathApply(ts,'//div/div/div/div/div/div/div',fun = xmlChildren)
  value = matrix(NA,ncol = 2,nrow = 0)
  colnames(value) = c("hyperlink","fundname")
  for(i in 1:length(tt))({
    g = tryCatch(tt[i][[1]]$strong %>%
                   xpathApply('a',fun=xmlAttrs) %>% 
                   as.character,
                 error = function(e) NULL)
    if(!(is.null(g) | length(g) == 0L))
    {
      if(str_sub(g,-3,-1)=="cms")
        value = rbind(value,
                      c(g,
                        tt[i][[1]]$strong %>% 
                          xpathApply('a',fun = xmlValue) %>% 
                          as.character))
    }
  })
  tt = xpathApply(ts,'//div/div/div/div/div/div/div',fun = xmlChildren)
  for(i in 1:length(tt))({
    g = tryCatch(tt[i][[1]]$a %>% 
                   xmlAttrs %>% 
                   as.character,
                 error = function(e) NULL)
    if(!(is.null(g) | length(g) == 0L))
    {
      if(str_sub(g,-3,-1)=="cms")
        value = rbind(value,
                      c(g,
                        tt[i][[1]]$a %>% 
                          xmlValue %>% 
                          as.character))
    }
  })
  value %>% as.data.frame
}

# Function to find the mean, stdev, sharpe,infoR
summarydata <- function(x){
  t = xpathApply(x,'//div/div/div/div/div/div/div',fun = xmlChildren)
  vars = c("Sharpe Ratio", "Standard Deviation","Mean","Information Ratio")
  value = matrix(NA,ncol = 2,nrow = 0)
  colnames(value) = c("Statistic","Value")
  j=1
  for(i in 1:length(t))({
    g = try(t[i][[1]]$text,silent=T)
    if(is.null(g)==FALSE)
      if(xmlValue(g) %in% vars){
        #print(c((xmlValue(g)),as.character(xmlValue(t[i+1][[1]]$text))))
        #print(i)
        value = rbind(value,
                      c(as.character(xmlValue(g)),
                        try(as.character(xmlValue(t[i+1][[1]]$text)),
                            silent = T)))
        j=j+1
      }
  })
  tbl_df(as.data.frame(value))
}

# Function to find the top 5 sector investments
sectorinfo <- function(x){
  t = xpathApply(x,'//div/div/div/div/div/div/div',fun = xmlChildren)
  vars = c("SECTOR")
  jt = character()
  k = 1
  for(i in 1:length(t))({
    g = try(t[i][[1]]$text,silent=T)
    if(is.null(g)==FALSE)
      if(xmlValue(g) %in% vars){
        #print(c((xmlValue(g)),as.character(xmlValue(t[i+1][[1]]$text))))
        if(k==2)
          for(j in 0:17)
            jt = c(jt,
                   try(as.character(xmlValue(t[i+j][[1]]$text)),
                       silent = T)
            )#;print(i)
        if(k==1)
          k = k+1        
      }
  })
  value = matrix(jt[4:18],ncol = 3,nrow = 5, byrow = T)
  colnames(value) = c("Sector","All.pc","All.value")
  top5 = tbl_df(as.data.frame(value))
  top5[] = lapply(top5,as.character)
  top5[,c(2,3)] <- convert.magic(top5[,c(2,3)],"numeric")  
  top5
}

# Function to change the datatype of vectors within datafram
convert.magic <- function(obj, type){
  FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  out <- lapply(obj, FUN1)
  as.data.frame(out)
}

fundinfo <- function(x){
  t = xpathApply(x,'//div/div/span',fun = xmlChildren)
  vars = c("Fund Family: ")
  jt = character()
  tr = character()
  for(i in 1:length(t))({
    g = try(t[i][[1]]$text,silent=T)
    if(is.null(g)==FALSE)
      if(xmlValue(g) %in% vars){
        #print(i)
        #print(c((xmlValue(g)),as.character(xmlValue(t[i+1][[1]]$text))))
        for(j in 0:6){
          jt = c(jt,
                 paste0(try(as.character(xmlValue(t[i+j][[1]]$text)),
                            silent = T),
                        tryCatch(as.character(xmlValue(
                          xmlChildren(t[i+j][[1]]$a)$text)
                        ),       error = function(e) ""),
                        collapse = "")
          )
        }
      }
  })
  separate(as.data.frame(jt),
           col = jt,
           into = c("Fund_Info","Details"),
           sep = ":")
}

returnsinfo <- function(x, y = "Fund Return"){
  t = xpathApply(x,'//div/div/div/div/div/div/div/div',fun = xmlChildren)
  vars = c(y)
  value = matrix(NA,ncol = 8,nrow = 0)
  jt = character()
  for(i in 1:length(t))({
    g = try(t[i][[1]]$strong,silent=T)
    if(is.null(g)==FALSE)
      if(xmlValue(g) %in% vars){
        jt = character()
        jt = c(as.character(xmlValue(g)))
        for(j in 1:7)
          jt = c(jt,
                 try(as.character(xmlValue(t[i+j][[1]]$text)),
                     silent = T)
          )#;print(i)
        value = rbind(value,jt)
      }
  })
  #value
  returntab = tbl_df(as.data.frame(value))
  names(returntab) <- c("Return","T7","T30","T90","T180","T360","T1080","T7200")
  returntab[] = lapply(returntab,as.character)
  return(returntab)
}

fundmgrinfo <- function(x){
  t = xpathApply(x,'//div/div/div/div/div/h4',fun = xmlChildren)
  vars = c("Fund Manager: ")
  jt = character()
  for(i in 1:length(t))({
    g = try(t[i][[1]]$text,silent=T)
    if(is.null(g)==FALSE)
      if(xmlValue(g) %in% vars){
        #print(i)
        #print(c((xmlValue(g)),as.character(xmlValue(t[i+1][[1]]$text))))
        mgr = paste0(xmlValue(xmlChildren(t[i][[1]]$select)$option),collapse = ";")
      }
  })
  mgr
}

riskinfo <- function(x){
  t = xpathApply(x,'//div/div/div/div/div/div/div/div/div/div',fun = xmlChildren)
  vars = c("Principal at")
  jt = character()
  for(i in 1:length(t))({
    g = try(t[i][[1]]$text,silent=T)
    if(is.null(g)==FALSE)
      if(substr(xmlValue(g),1,12) == vars){
        #print(i)
        rsk = substr(xmlValue(g),
                     13,
                     nchar(xmlValue(g)))
      }
  })
  rsk
}

#value = matrix(jt[4:18],ncol = 3,nrow = 5, byrow = T)
#colnames(value) = c("Sector","All.pc","All.value")
#top5 = tbl_df(as.data.frame(value))
#top5[] = lapply(top5,as.character)
#top5[,c(2,3)] <- convert.magic(top5[,c(2,3)],"numeric")  
#top5
#t = xpathApply(ps,'//div/div/div/div/div/div/div/div',fun = xmlChildren)
# 
# value = matrix(jt[4:18],ncol = 3,nrow = 5, byrow = T)
# colnames(value) = c("Sector","All.pc","All.value")
# top5 = tbl_df(as.data.frame(value))
# top5[] = lapply(top5,as.character)
# top5[,c(2,3)] <- convert.magic(top5[,c(2,3)],"numeric")  
# top5
# }


#t = xpathApply(ps,'//div/div/div/div/div/div/div/div/div',fun = xmlAttrs)
#t = xpathApply(ps,'//div/div/div/div/div/div/div/div',fun = xmlChildren)
#length(t)
#summary(t)
