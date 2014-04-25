#library(data.table)
library(gdata)
#library(HTMLUtils)
library(plyr)
#library(R2HTML)
library(RCurl)
#library(stringr)
#library(tm)
#library(tm.plugin.webmining)
library(XML)
#library(XML2R)




#Good sample
#good_df <- xmlToDataFrame("c:/temp/sample.xml") 



#Get page

url <- "http://www.sec.gov/Archives/edgar/data/1414040/000141404014000007/0001414040-14-000007.hdr.sgml"

webpage <- getURL(url,encoding="UTF-8")
webpage2 <- readLines(tc <- textConnection(webpage)); close(tc)
webpage_org_df <- as.data.frame(webpage2,stringsAsFactors=FALSE)
colnames(webpage_org_df) <- "raw"
webpage_org_df[,"raw"] <- toupper(webpage_org_df[,"raw"])

webpage_df_xml_only <- webpage_org_df
webpage_df_xml_only2 <- webpage_df_xml_only[!(is.na(webpage_df_xml_only) | webpage_df_xml_only=="")]
webpage_df_xml_only3 <- gsub("&nbsp;"," ",webpage_df_xml_only2)

webpage_df_xml_only_df <- as.data.frame(webpage_df_xml_only3,stringsAsFactors=FALSE)
colnames(webpage_df_xml_only_df) <- c("raw")


#Seperate tags and values
webpage_sep <- data.frame(tag_status_open=NA,
                          tag_status_close=NA,
                          raw=webpage_df_xml_only_df,
                          raw_encoded=NA,
                          tag=NA,
                          tag_short=NA,
                          type=NA,
                          value=NA,
                          stringsAsFactors=FALSE)
#Get Tag
webpage_sep[,"tag"] <-  gsub(".*?<(.*?)>.*", "\\1", webpage_sep[,"raw"]) 

#Clean Tag
for(i in 1:ncol(webpage_sep))
{
  webpage_sep[,i] <- iconv(webpage_sep[,i], "latin1", "ASCII", sub="")
}

for(i in which(sapply(webpage_sep,class)=="character"))
{
  webpage_sep[[i]] = trim(webpage_sep[[i]])
}
for (i in 1:ncol(webpage_sep))
{
  webpage_sep[,i] <- unknownToNA(webpage_sep[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                            NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                            NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  webpage_sep[,i] <- ifelse(is.na(webpage_sep[,i]),NA, webpage_sep[,i])
} 


#Determine is open/close tag
webpage_sep[,"type"] <-  ifelse(grepl("/", webpage_sep[,"tag"]), "close", "open")

#Get value
webpage_sep[,"value"] <- sapply(strsplit(webpage_sep[,"raw"], paste("<",webpage_sep[,"tag"],">",sep="")), "[", 2)

#Remove closing tag from tag_short column
webpage_sep[,"tag_short"] <- gsub("/", "", webpage_sep[,"tag"])

#Add brackets to tags
webpage_sep[,"tag"] <- paste("<",webpage_sep[,"tag"],">",sep="")

#Find all possible tags
tags <- unique(webpage_sep[,"tag_short"])

#Count open and closed tags
tags_df <- data.frame(type=tags,
                      open_count=NA,
                      close_count=NA,
                      stringsAsFactors=FALSE)

for (i in 1:nrow(tags_df))
{
  #i <- 1
  
  webpage_sep[,"tag_status_open"] <- ifelse(grepl(paste("<",tags_df[i,"type"],">",sep=""), webpage_sep[,"raw"]), 1, 0)
  webpage_sep[,"tag_status_close"] <- ifelse(grepl(paste("</",tags_df[i,"type"],">",sep=""), webpage_sep[,"raw"]), 1, 0)
  
  tags_df[i,"open_count"] <- sum(webpage_sep[,"tag_status_open"])
  tags_df[i,"close_count"] <- sum(webpage_sep[,"tag_status_close"])
  
  webpage_sep[,"tag_status_open"] <- NA
  webpage_sep[,"tag_status_close"] <- NA
} 

#Find tags that are not closed
tags_bad <- tags_df[which(tags_df[,"open_count"]-tags_df[,"close_count"] != 0),]
row.names(tags_bad) <- seq(nrow(tags_bad))

#Find rows that need to be fixed
colnames(webpage_sep)[match("tag_status_open",names(webpage_sep))] <- "Final_tag"
colnames(webpage_sep)[match("tag_status_close",names(webpage_sep))] <- "Good"

#Find tags that need to be closed
webpage_sep[,"Good"] <- ifelse(webpage_sep[,"tag_short"] %in% tags_bad[,"type"], 0, 1)

#Encode HTML entities
entity_encoding <- read.csv(file="C:\\Research_temp3\\Entity_encoding.csv",header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

#Clean
entity_encoding_clean <- entity_encoding
#for(i in 1:ncol(entity_encoding_clean))
#{
#  entity_encoding_clean[,i] <- iconv(entity_encoding_clean[,i], "latin1", "ASCII", sub="")
#}

for(i in which(sapply(entity_encoding_clean,class)=="character"))
{
  entity_encoding_clean[[i]] = trim(entity_encoding_clean[[i]])
}
for (i in 1:ncol(entity_encoding_clean))
{
  entity_encoding_clean[,i] <- unknownToNA(entity_encoding_clean[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  entity_encoding_clean[,i] <- ifelse(is.na(entity_encoding_clean[,i]),NA, entity_encoding_clean[,i])
} 


entity_encoding_trim <- entity_encoding_clean[(!(is.na(entity_encoding_clean[,"ASCII.Looks.Like"])) & 
                                                 !(is.na(entity_encoding_clean[,"Entity.Encoding"]))),]
row.names(entity_encoding_trim) <- seq(nrow(entity_encoding_trim))

#Replacement
for (i in 1:nrow(entity_encoding_trim))
{
  webpage_sep[,"value"] <- gsub(entity_encoding_trim[i,"ASCII.Looks.Like"], 
                                entity_encoding_trim[i,"Entity.Encoding"], 
                                webpage_sep[,"value"])
  
} 
#Clean
for(i in which(sapply(webpage_sep,class)=="character"))
{
  webpage_sep[[i]] = trim(webpage_sep[[i]])
}
for (i in 1:ncol(webpage_sep))
{
  webpage_sep[,i] <- unknownToNA(webpage_sep[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                            NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                            NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  webpage_sep[,i] <- ifelse(is.na(webpage_sep[,i]),NA, webpage_sep[,i])
} 

#Create raw encoded tags
webpage_sep[,"raw_encoded"] <- ifelse(is.na(webpage_sep[,"value"]),
                                      webpage_sep[,"tag"], 
                                      paste(webpage_sep[,"tag"],webpage_sep[,"value"],sep=""))

#Creat final tags
webpage_sep[,"Final_tag"] <- ifelse(webpage_sep[,"Good"]==1, 
                                    webpage_sep[,"raw_encoded"], 
                                    paste(webpage_sep[,"raw_encoded"],"</",webpage_sep[,"tag_short"],">",sep=""))

data <- webpage_sep[,"Final_tag"]
#data <- webpage_sep[c(38:214),"Final_tag"]
#data <- webpage_sep[c(39:47),"Final_tag"]

data_parse <- xmlTreeParse(data,useInternalNodes = TRUE, options = HUGE, isSchema=FALSE)
#data_parse <- xmlTreeParse(data,useInternalNodes = FALSE, options = HUGE, isSchema=FALSE)


test_df <- xmlToDataFrame(data_parse)



dumFun <- function(x){
  
  #x <- getNodeSet(data_parse, paste("//*/","CLASS-CONTRACT",sep=""))[[1]]
  
  xname <- xmlName(x)
  xattrs <- xmlAttrs(x)
  x_combined <- c(sapply(xmlChildren(x), xmlValue), name = xname, attrs = xattrs)
  return(x_combined)
}
dumFun2 <- function(doc,path){
  
  #doc <- data_parse
  #path <- paste("//","SEC-HEADER",sep="")
  #path <- paste("//*/","SERIES",sep="")
  #path <- paste("//*/","CLASS-CONTRACT",sep="")
  
  temp1 <- getNodeSet(doc, path)
  allcols <- unique(unlist(sapply(temp1, names))) 
  allcols_df <- as.data.frame(allcols, stringsAsFactors = FALSE)
  allcols_df <- allcols_df[,colnames(unique(as.matrix(allcols_df), MARGIN=2))]
  
  for (i in 1:length(temp1))
  {
    #i <- 1
    
    xname <- xmlName(temp1[[i]])
    xattrs <- xmlAttrs(temp1[[i]])
    temp1[[i]] <- c(name = xname, attrs = xattrs, sapply(xmlChildren(temp1[[i]]), xmlValue))
  } 
  
  allcols2 <- c("name","attrs",allcols_df)
  
  temp2 <- temp1
  for (i in 1:length(temp2))
  {
    #i <- 1
    
    missingColumns <- allcols2[which(!allcols2 %in% names(temp2[[i]]))] 
    temp2[[i]][missingColumns] <- NA 
    
    #temp2[[i]] <- temp2[[i]][order(names(temp2[[i]]))]
    #temp2[[i]] <- temp2[[i]][sort.list(temp2[[i]])]
    #order(names(temp2[[i]])) <- allcols2
    #temp2[[i]] <- temp2[[i]](names("name"))
    #temp2[[i]][order(names(temp2[[i]]))] <- allcols2
    #temp2[[i]][names(temp2[[i]])] <- allcols2
    temp_df <- as.data.frame(temp2[[i]], stringsAsFactors = FALSE)
    temp2[[i]] <- temp_df[allcols2,]
    
  } 
  
  temp3 <- do.call(rbind, temp2)
  temp4 <- as.data.frame(temp3, stringsAsFactors = FALSE)
  colnames(temp4) <- allcols2
  return(temp4)
  
}


test1 <- xmlToDataFrame(data_parse)

#FIRST LEVEL

#test1_sh <-  xmlToDataFrame(getNodeSet(data_parse, paste("//","SEC-HEADER",sep="")), stringsAsFactors = FALSE)
#test1_sh$Name <- "SEC-HEADER"
test1_sh <- as.data.frame(t(xpathSApply(data_parse, paste("//","SEC-HEADER",sep=""), dumFun)), stringsAsFactors = FALSE)
for(i in which(sapply(test1_sh,class)=="character"))
{
  test1_sh[,i] <- gsub("\n","", test1_sh[,i])
  
}
for(i in which(sapply(test1_sh,class)=="character"))
{
  test1_sh[[i]] = trim(test1_sh[[i]])
}
for (i in 1:ncol(test1_sh))
{
  test1_sh[,i] <- unknownToNA(test1_sh[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                            NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                            NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  test1_sh[,i] <- ifelse(is.na(test1_sh[,i]),NA, test1_sh[,i])
} 
test1_sh <- data.frame(test1_sh, stringsAsFactors = FALSE)
test1_sh <- test1_sh[,colSums(is.na(test1_sh))<nrow(test1_sh)]

#test1_a_dt <-  xmlToDataFrame(getNodeSet(data_parse, paste("//*/","ACCEPTANCE-DATETIME",sep="")), stringsAsFactors = FALSE)
#test1_a_num <-  xmlToDataFrame(getNodeSet(data_parse, paste("//*/","ACCESSION-NUMBER",sep="")), stringsAsFactors = FALSE)

#test1_filer <-  xmlToDataFrame(getNodeSet(data_parse, paste("//*/","FILER",sep="")), stringsAsFactors = FALSE)
#test1_series <-  xmlToDataFrame(getNodeSet(data_parse, paste("//*/","SERIES",sep="")), stringsAsFactors = FALSE)
#test1_cc <-  xmlToDataFrame(getNodeSet(data_parse, paste("//*/","CLASS-CONTRACT",sep="")), stringsAsFactors = FALSE)

#test1_filer <- as.data.frame(t(xpathSApply(data_parse, paste("//*/","FILER",sep=""), dumFun)), stringsAsFactors = FALSE)
#test1_series <- as.data.frame(t(xpathSApply(data_parse, paste("//*/","SERIES",sep=""), dumFun)), stringsAsFactors = FALSE)

#test1_sh <- dumFun2(data_parse, paste("//","SEC-HEADER",sep=""))
test1_a_dt <- dumFun2(data_parse, paste("//*/","ACCEPTANCE-DATETIME",sep=""))
test1_a_num <- dumFun2(data_parse, paste("//*/","ACCESSION-NUMBER",sep=""))

test1_filer <- dumFun2(data_parse, paste("//*/","FILER",sep=""))
test1_series <- dumFun2(data_parse, paste("//*/","SERIES",sep=""))
test1_cc <- dumFun2(data_parse, paste("//*/","CLASS-CONTRACT",sep=""))








aa <- xmlSApply(xmlRoot(data_parse)[[2]], function(x) xmlValue(x[[1]]))

#top <- xmlRoot(data_parse)

i <- 13
xmlName(xmlRoot(data_parse)[[i]])
xmlValue(xmlRoot(data_parse)[[i]])
xmlChildren(xmlRoot(data_parse)[[i]])



# #Convert to html
# webpage_html <- htmlTreeParse(webpage_sep[,"raw"],asTree = TRUE, useInternalNodes = TRUE, options = HUGE)
# saveXML(webpage_html, file="C:\\Research_temp3\\temp.html")
# 
# #Open html file
# webpage3 <- readLines("C:\\Research_temp3\\temp.html")
# webpage_html_df <- as.data.frame(webpage3,stringsAsFactors=FALSE)
# colnames(webpage_html_df) <- "raw"
# #webpage_html_df[,"raw"] <- toupper(webpage_html_df[,"raw"])
# 
# tags_html <- tolower(c("HTML","BODY",tags_df[,"type"]))
# 
# #Put every tag on new line
# html_raw <- webpage_html_df[,"raw"]
# 
# #Closing Tags
# for (i in 1:length(tags_html))
# {
#   #i <- 1
#   
#   #temp <- strsplit(html_raw, paste("</",tags_html[i],">",sep=""), perl = TRUE)
#   temp <- str_split(html_raw, paste("</",tags_html[i],">",sep=""))
#   
#   temp2 <- lapply(temp, function(x,split=paste("</",tags_html[i],">",sep="")){
#     
#     if (length(x)>1)
#     {
#       x[2:length(x)] <- paste(split,x[2:length(x)],sep="")
#       return(x)
#       
#     } else
#     {
#       
#       return(x)
#     }
#   })
#   temp3 <- trim(temp2)
#   
#   #temp4 <- unlist(temp3)
#   temp4 <- rle(unlist(temp3))$values
#   temp5 <- as.data.frame(temp4,stringsAsFactors=FALSE)
#   
#   html_raw <- temp5[,1]
# } 
# 
# #Opening Tags
# for (i in 1:length(tags_html))
# {
#   #i <- 1
#   
#   #temp <- strsplit(html_raw, paste("<",tags_html[i],">",sep=""), perl = TRUE)
#   temp <- str_split(html_raw, paste("<",tags_html[i],">",sep=""))
#   
#   temp2 <- lapply(temp, function(x,split=paste("<",tags_html[i],">",sep="")){
#     
#     if (length(x)>1)
#     {
#       x[2:length(x)] <- paste(split,x[2:length(x)],sep="")
#       return(x)
#       
#     } else
#     {
#       
#       return(x)
#     }
#   })
#   temp3 <- trim(temp2)
#   
#   #temp4 <- unlist(temp3)
#   temp4 <- rle(unlist(temp3))$values
#   temp5 <- as.data.frame(temp4,stringsAsFactors=FALSE)
#   
#   html_raw <- temp5[,1]
# 
# } 
# 
# html_expand <- as.data.frame(html_raw,stringsAsFactors=FALSE)
# 
# 
# #Clean HTML Tags
# for(i in which(sapply(html_expand,class)=="character"))
# {
#   html_expand[[i]] = trim(html_expand[[i]])
# }
# for (i in 1:ncol(html_expand))
# {
#   html_expand[,i] <- unknownToNA(html_expand[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
#                                                             NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
#                                                             NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
#   html_expand[,i] <- ifelse(is.na(html_expand[,i]),NA, html_expand[,i])
# } 
# html_expand_trim <- html_expand[!(is.na(html_expand))]
# html_expand_trim <- as.data.frame(html_expand_trim,stringsAsFactors=FALSE)


# html_expand_trim2 <- as.data.frame(html_expand_trim[4:343,1],stringsAsFactors=FALSE)
# 
# 
# #test3 <- xmlToDataFrame(html_expand_trim[,1])
# test3 <- xmlToDataFrame(html_expand_trim[4:343,1])
# 
# 
# 
# 
# 
# 
# 
# top <- xmlRoot(UIN_True)
# top_node <- xmlName(top)
# #top_node_child_nodes <- as.data.frame(names(top))
# top_node_child_nodes <- names(top)
# top_node_child_nodes1 <- names(top[[1]])
# sec_header <- top[[1]][["sec-header"]]
# sec_header_child_nodes <- names(sec_header)
# sec_header_child_nodes1 <- names(sec_header[[1]])
# sec_header_child_nodes2 <- names(sec_header[[2]])
# 
# temp <- xmlSApply(sec_header[[2]], xmlValue)
# temp2 <- xmlSApply(sec_header, function(x) xmlSApply(x, xmlValue))
# 
# 
# 
# 
# 
# test3 <- xmlParse(data, asText=TRUE, useInternalNodes = FALSE, isSchema=TRUE)
# test3 <- htmlParse(data, asText=TRUE,useInternalNodes = TRUE, isSchema=TRUE,encoding = "UTF-8")
# 
# test3 <- xmlTreeParse(data,asTree = TRUE, useInternalNodes = FALSE, options = HUGE, 
#              handlers=list(entity=function(x){
#                cat("In entity",x$name, x$value,"\n")
#                return(x)
#                }))
# 
# 
# 
# 
# 
titles = list()
xmlTreeParse(data, handlers = list(title = function(x){
  titles[[length(titles) + 1]] <<- x
}))
sapply(titles, xmlValue)


doc <- xmlTreeParse(data,  handlers = (function() { 
  vars <- character(0) ;
  list(variable=function(x, attrs) { 
    vars <<- c(vars, xmlValue(x[[1]])); 
    NULL}, 
    startElement=function(x,attr){
      NULL
    }, 
    names = function() {
      vars
    }
  )
})()
)
doc <- xmlTreeParse(data, asTree=TRUE,handlers = list(variable=function(x, attrs) {
  #print(xmlValue(x[[1]])) 
  cat(xmlValue(x[[1]]),"/n") 
  return(TRUE)
  }))


# #test3 <- htmlParse(data, asText=TRUE,useInternalNodes = TRUE, isSchema=TRUE)
# #test3a <- xpathApply(test3,"//body//text()",  xmlValue)
# #test3a <- xpathApply(test3,"//body",  xmlValue)
# #test3a <- xpathApply(test3,"\n",  xmlValue)
# #test3a2 <- test3a[[1]]
# #test3b <- getNodeSet(test3, "//body")
# #test3b2 <- test3b[[1]]
# #test3b3 <- xmlValue(test3b2) 
# 
# 
# # getDTD=FALSE, parentFirst=FALSE,isSchema=TRUE,fullNamespaceInfo=TRUE,xinclude=TRUE, asTree=TRUE)
# 
# #Default
# UIN_False <- htmlTreeParse(data, asText=TRUE, useInternalNodes = FALSE, options = HUGE,getDTD=FALSE)
# UIN_False_sh <- UIN_False$children$html$children$body$children
# UIN_False_sh_name <- UIN_False_sh[[1]]$name
# 
# UIN_False$children[[1]]$name
# UIN_False$children[[1]]$attributes
# UIN_False$children[[1]]$children
# UIN_False$children[[1]]$value
# 
# UIN_True <- htmlTreeParse(data, asText=TRUE, useInternalNodes = TRUE, options = HUGE, 
#                           parentFirst=TRUE,isSchema=TRUE,fullNamespaceInfo=TRUE,xinclude=TRUE,getDTD=FALSE,asTree=TRUE)
# 
# UIN_True_node <- getNodeSet(UIN_True, "//body")
# 




# test4 <- htmlTreeParse(data, asText=TRUE, useInternalNodes = FALSE, options = HUGE, 
#                        parentFirst=FALSE,isSchema=TRUE,fullNamespaceInfo=TRUE,xinclude=TRUE,getDTD=FALSE,asTree=TRUE)
# 
# test4b1 <- getNodeSet(test4, "//body")
# a <- test4b1[[1]]
# test4b1 <- getNodeSet(test4, "//body")
# test4b1[[1]]$XMLInternalNode
# test4b2 <- xmlValue(test4b1[[1]])
# #test4b3 <- xmlGetAttr(test4b1[[1]],"class")
# test4b3 <- xmlAttrs(test4b1[[1]])
# test4b4 <- xmlName(test4b1[[1]])
# test4b5 <- xmlChildren(test4b1[[1]])
# test4b6 <- xmlNamespace(test4b1[[1]])
# 
# test4b1[[1]]$XMLNodeList
# attr(test4b1, XMLNodeList)
# 
# temp4c <- xpathSApply(test4,"//body",  xmlValue)
# temp4d <- xpathSApply(test4,"//body")
# 
# #Test stuff
# xml2r_test <- docsToNodes(test4)
# xml2r_test2 <-nodesToList(xml2r_test)
# xml2r_test3 <- listsToObs(xml2r_test2,c(NA,NA,NA))
# xml2r_test4 <- do.call(rbind, xml2r_test3)
# 
# 
# 
# #test5 <- htmlTreeParse(data, asText=TRUE,useInternalNodes = FALSE, options = HUGE, isSchema=TRUE)
# test5 <- htmlTreeParse(data, asText=TRUE,useInternalNodes = FALSE, options = HUGE, isSchema=FALSE)
# 
# #test5b <- getNodeSet(test5, "//body")
# #test5b <- xpathSApply(test5,"//body",  xmlValue)
# test5b <- test5$children$html
# test5c <- do.call(rbind, test5b)
# 
# as.data.frame(test5$children$html, stringsAsFactors=FALSE)
# 
# 
# body <- test5$children$html$children$body 
# sec_header <- body$children$sec-header$children
# 
# 
# test6 <- xmlToDataFrame(test5)
# 
# 
# 
# test7 <- XMLSource(test5)
# 
# 
# 
# 
# 
# #test3a <- xpathApply(test3,"//body//text()",  xmlValue)
# #test3a <- xpathSApply(test3,"//body//text()",  xmlValue)
# 
# # test <- xmlTreeParse(webpage_sep[,"Final_tag"],useInternalNodes = FALSE, options = HUGE)
# # test <- xmlToDataFrame(webpage_sep[,"Final_tag"])
# 
# # test <- xmlEventParse(webpage_sep[,"Final_tag"])
# # test <- xmlTreeParse(webpage_sep[,"Final_tag"],useInternalNodes = FALSE, options = HUGE)
# # test <- xmlTreeParse(webpage_sep[,"Final_tag"],useInternalNodes = TRUE, options = HUGE,error=function(e){ cat( "\n") })
# 
# # test <- xmlParse(webpage_sep[,"Final_tag"])
#  
# # webpage_df2 <- data.frame(webpage_df,document_tag=NA,stringsAsFactors=FALSE)
# # webpage_df2[,"document_tag"] <- ifelse(grepl("<DOCUMENT>", webpage_df2[,"webpage"]), 1, 0)
# # test <- xpathSApply(webpage, "//document", xmlValue)
# 
# # readKeyValueDB(url)


#####################################################

library(XBRL)
library(sqldf)

ko = "http://www.sec.gov/Archives/edgar/data/21344/000002134413000050/ko-20130927.xml"
#ko = "http://www.sec.gov/Archives/edgar/data/1414040/000141404014000007/0001414040-14-000007.hdr.sgml"

xbrl.vars <- xbrlDoAll(ko, verbose=TRUE)



###combine things


xbrl_to_df = function(xbrl.vars){
  name_list = names(xbrl.vars)
  for(nom in name_list){
    eval(parse(text=paste0("df_",nom,"= xbrl.vars$",nom)))
    
  }
  big_data_frame = sqldf('select a.elementid, a.contextid, a.unitid, a.fact, a.decimals, a.factid, b.startdate,         b.enddate, b.dimension1, b.value1, b.dimension2, b.value2, b.dimension3, b.value3, b.dimension4, b.value4,     c.footnoteString

      from df_fact a left join df_context b on a.contextid = b.contextid left join df_footnote c on     c.factid=a.factid ')
  
  return(big_data_frame)
  
  
}

my_data = xbrl_to_df(xbrl.vars)






# http://www.sec.gov/Archives/edgar/data/1414040/000141404014000007/
# http://www.sec.gov/Archives/edgar/data/1414040/
# http://www.sec.gov/Archives/edgar/data/21344/000002134413000050/ko-20130927.xml
# http://searchsoa.techtarget.com/definition/SGML
# http://www.techwr-l.com/archives/9805/techwhirl-9805-01280.html#.U1Xb0_ldWBI
# http://xml.coverpages.org/grahamTransTools.html
# http://tolstoy.newcastle.edu.au/R/help/05/01/10243.html
# https://www.sec.gov/info/edgar/pdsdissemspec910.pdf
# http://www.sec.gov/edgar/searchedgar/edgarzones.htm
# http://iangow.wordpress.com/2011/08/29/getting-sec-filing-header-files/
# http://stackoverflow.com/questions/12412994/use-lxml-to-parse-text-file-with-bad-header-in-python
# http://www.reddit.com/r/algotrading/comments/23jjji/obtaining_easily_parseable_sec_filings_data/











