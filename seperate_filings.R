library(gdata)
library(RCurl)
library(XML)


# 
# url <- "http://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r"
# doc <- htmlParse(url)
# links <- xpathSApply(doc, "//a/@href")
# free(doc)
# 
# 
# 
# library(XML)
# 
# doc.html = htmlTreeParse('http://www.gutenberg.org/files/145/145-h/145-h.htm',
#                          useInternal = TRUE)
# doc.value <- xpathApply(doc.html, '//h2|//p', xmlValue)
# doc.html.value <- xpathApply(doc.html, '//h2|//p')
# doc.html.name.value <- xpathApply(doc.html, '//h2|//p', function(x) { list(name=xmlName(x), content=xmlValue(x)); })
# 
# doc.html.name.value[[1]]
# 





webpage_address <- "ftp://ftp.sec.gov/edgar/data/1414040/0000950136-08-000294.txt" 

webpage <- getURL(webpage_address)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
webpage_df <- as.data.frame(webpage,stringsAsFactors=FALSE)

webpage_df_xml_only <- webpage_df[11:(nrow(webpage_df)-2),]
webpage_df_xml_only2 <- webpage_df_xml_only[!(is.na(webpage_df_xml_only) | webpage_df_xml_only=="")]
webpage_df_xml_only3 <- gsub("&nbsp;"," ",webpage_df_xml_only2)

webpage_df_xml_only_df <- as.data.frame(webpage_df_xml_only3,stringsAsFactors=FALSE)


test <- xmlTreeParse(webpage_df_xml_only3,useInternalNodes = FALSE, options = HUGE)
test <- xmlToDataFrame(webpage_df_xml_only3)










webpage_df2 <- data.frame(webpage_df,
                          document_tag=NA,
                          stringsAsFactors=FALSE)

webpage_df2[,"document_tag"] <- ifelse(grepl("<DOCUMENT>", webpage_df2[,"webpage"]), 1, 0)

#webpage_df <- gsub("^\\s+|\\s+$", "", webpage_df)
#webpage_df <- trim(webpage_df)
#Trim strings
# sample_data_all[,"Strategy"] <- trim(sample_data_all[,"Strategy"])
# 
# #Remove multiple spaces (run a couple times)
# for (a in 1:5)
# {
#   #a <- 1
#   sample_data_all[,"Strategy"] <- gsub(pattern=" {2,}", replacement=" ", x=sample_data_all[,"Strategy"])
#   
# }

test <- xpathSApply(webpage, "//document", xmlValue)
xmlToDataFrame("c:/Temp/foo.xml") 
