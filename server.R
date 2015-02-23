library(XML)
library(dplyr)
library(shiny)
library(tidyr)
library(stringr)
parseIt <- function(x){
  htmlTreeParse(x, useInternalNodes = TRUE)
}

# Function to get all the hyperlinks in the list of funds page for further data parsing
hyperlinkoffunds <- function(x = "http://economictimes.indiatimes.com/equity-large-cap/mffundsbycategory.cms?primaryobj=equity&secondaryobj=large%20cap&period=r1year"){
  ts = parseIt(x)
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
  value
  returntab = tbl_df(as.data.frame(value))
  names(returntab) <- c("Return","T7","T30","T90","T180","T360","T1080","T7200")
  returntab[] = lapply(returntab,as.character)
  returntab
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
shinyServer(function(input, output) {
  allfunds = "http://economictimes.indiatimes.com/equity-large-cap/mffundsbycategory.cms?primaryobj=equity&secondaryobj=large%20cap&period=r1year"  
  url = "http://economictimes.indiatimes.com/uti-equity-fund/mffactsheet/schemeid-247.cms"
  linset <- hyperlinkoffunds(allfunds)
  
  dataset <- reactive({
    url <- paste0("http://economictimes.indiatimes.com",
                  linset[linset[,"fundname"] == input$fundselected,]$hyperlink,
                  collapse = "")
    ps = parseIt(url)
    list(smry = summarydata(ps),
         sector = sectorinfo(ps),
         fundbasics = fundinfo(ps),
         manager = fundmgrinfo(ps),
         risk = riskinfo(ps),
         url = url,
         ps = ps)
  })
  
  
  output$displayside <- renderUI(
    selectInput(inputId = 'fundselected',
                label = "Fund",
                choices = levels(linset[,2]))
  )
  
  output$displaymain <- renderUI(
    tabsetPanel(
      tabPanel("Summary",dataTableOutput(outputId = 'smry')),
      tabPanel("Sector",dataTableOutput(outputId = 'sector')),
      tabPanel("Basic Information",dataTableOutput(outputId =  'fundbasics')),
      tabPanel("Manager",dataTableOutput(outputId =  'manager')),
      tabPanel("Risk",dataTableOutput(outputId =  'risk')),
      tabPanel("URL",textOutput('url'))
    )
  )
  
  output$smry <- renderDataTable({
    dataset()$smry %>% as.data.frame
  })
  
  output$sector <- renderDataTable({
    dataset()$sector %>% as.data.frame
  })
  
  output$fundbasics <- renderDataTable({
    dataset()$fundbasics %>% as.data.frame
  })
  
  output$manager <- renderDataTable({
    dataset()$manager %>% as.data.frame
  })
  output$risk <- renderDataTable({
    dataset()$risk %>% as.data.frame
  })
  
  output$url <- renderText(dataset()$url)
  
})