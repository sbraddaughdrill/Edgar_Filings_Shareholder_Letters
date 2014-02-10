library(chron)
library(DataCombine)
library(gdata)
library(RCurl)
library(XML)




# 
# htmlToText <- function(input, ...) {
#   ###---PACKAGES ---###
#   require(RCurl)
#   require(XML)
#   
#   
#   ###--- LOCAL FUNCTIONS ---###
#   # Determine how to grab html for a single input element
#   evaluate_input <- function(input) {    
#     # if input is a .html file
#     if(file.exists(input)) {
#       char.vec <- readLines(input, warn = FALSE)
#       return(paste(char.vec, collapse = ""))
#     }
#     
#     # if input is html text
#     if(grepl("</html>", input, fixed = TRUE)) return(input)
#     
#     # if input is a URL, probably should use a regex here instead?
#     if(!grepl(" ", input)) {
#       # downolad SSL certificate in case of https problem
#       if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
#       return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
#     }
#     
#     # return NULL if none of the conditions above apply
#     return(NULL)
#   }
#   
#   # convert HTML to plain text
#   convert_html_to_text <- function(html) {
#     doc <- htmlParse(html, asText = TRUE)
#     text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
#     return(text)
#   }
#   
#   # format text vector into one character string
#   collapse_text <- function(txt) {
#     return(paste(txt, collapse = " "))
#   }
#   
#   ###--- MAIN ---###
#   # STEP 1: Evaluate input
#   html.list <- lapply(input, evaluate_input)
#   
#   # STEP 2: Extract text from HTML
#   text.list <- lapply(html.list, convert_html_to_text)
#   
#   # STEP 3: Return text
#   text.vector <- sapply(text.list, collapse_text)
#   return(text.vector)
# }


cik <- "0001414040"

webpage_address <- paste("http://www.sec.gov/cgi-bin/series?company=&sc=companyseries&ticker=&CIK=",cik,"&type=N-PX",sep="") 
#webpage_address <- paste("http://www.sec.gov/cgi-bin/browse-edgar?CIK=",cik,"&action=getcompany&scd=series",sep="")

#webpage <- getURL(webpage_address)
webpage <- getURLContent(webpage_address)[[1]]

webpage <- readLines(tc <- textConnection(webpage)); close(tc)

webpage_df_html_only <- webpage[3:(length(webpage)-2)]
webpage_df_html_only2 <- webpage_df_html_only[!(is.na(webpage_df_html_only) | webpage_df_html_only=="")]

webpage_df <- as.data.frame(webpage_df_html_only2,stringsAsFactors=FALSE)

#doc.html <- htmlTreeParse(webpage_df_html_only2, error=function(...){}, useInternalNodes = TRUE)
doc.html <- htmlTreeParse(webpage, useInternalNodes = TRUE, options = HUGE)

#doc.value <- xpathApply(doc.html, '//h2|//p', xmlValue)
#doc.html.value <- xpathApply(doc.html, '//h2|//p')
#doc.html.name.value <- xpathApply(doc.html, '//h2|//p', function(x) { list(name=xmlName(x), content=xmlValue(x)); })


#doc.value <- xpathSApply(doc.html, '//a/@href',xmlValue)
doc.html.value <- xpathSApply(doc.html, '//a/@href')
doc.html.value_df <- as.data.frame(doc.html.value,stringsAsFactors=FALSE)




doc.html.name.value <- xpathSApply(doc.html, '//a/@href', function(x) { list(name=xmlName(x), content=xmlValue(x)); })

doc.html.name.value[[1]]

#test1 <- pagetree[[1]]
#test2 <- test1[[1]]

#pagetree_df <- as.data.frame(pagetree,stringsAsFactors=FALSE)






#doc = htmlTreeParse(webpage_address, useInternalNodes = T)
#txt <- htmlToText(webpage_address)
#txt_df <- as.data.frame(txt,stringsAsFactors=FALSE)



#table <- readHTMLTable(webpage_address)
table <- readHTMLTable(webpage_df_html_only2)


table_data <- table[[6]]

table_data_df <- as.data.frame(table_data,stringsAsFactors=FALSE)
table_data_df2 <- data.frame(lapply(table_data_df, as.character), stringsAsFactors=FALSE)

colnames(table_data_df2) <- seq(1,ncol(table_data_df2),1)

for(i in 1:ncol(table_data_df2))
{
  
  table_data_df2[,i] <- iconv(table_data_df2[,i], "latin1", "ASCII", sub="")
}

#table_data_df2[,"2"] <- slide(table_data_df2,"2", slideBy = 1)[ncol(table_data_df2)+1]
#table_data_df2[,"3"] <- slide(table_data_df2,"3", slideBy = 2)[ncol(table_data_df2)+1]
#table_data_df2[,"4"] <- slide(table_data_df2,"4", slideBy = 3)[ncol(table_data_df2)+1]
#table_data_df2[,"5"] <- slide(table_data_df2,"5", slideBy = 4)[ncol(table_data_df2)+1]

#table_data_cik <- table_data_df2[!(is.na(table_data_df2[,"1"]) | table_data_df2[,"1"]==""),"1"]
#table_data_temp <- table_data_df2[!(is.na(table_data_df2[,"2"]) | table_data_df2[,"2"]==""),]

for(i in which(sapply(table_data_df2,class)=="character"))
{
  table_data_df2[[i]] = trim(table_data_df2[[i]])
}
for (i in 1:ncol(table_data_df2))
{
  table_data_df2[,i] <- unknownToNA(table_data_df2[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                  NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                  NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  table_data_df2[,i] <- ifelse(is.na(table_data_df2[,i]),NA, table_data_df2[,i])
} 

#Shift row elements across column where NA

shift_left <- function(data,var) {
  
  for (i in 1:nrow(data))
  {
    #i <- 1
    #i <- 2
    #i <- 3
    #i <- 4
    #i <- 5
    #i <- 6
    
    if ((is.na(data[i,col_num]) | data[i,col_num]=="NA"))
    {
      data[i,col_num:ncol(data)] <- t(c(data[i,(col_num+1):ncol(data)],"NA"))
      
    } else
    {
      data[i,col_num:ncol(data)] <- data[i,col_num:ncol(data)]
      
    }
    
  } 
  
  for (i in 1:ncol(data))
  {
    data[,i] <- unknownToNA(data[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
    data[,i] <- ifelse(is.na(data[,i]),NA, data[,i])
  } 
  
  return(data)
  
}

table_data_df3 <- shift_left(table_data_df2,"1")
table_data_df4 <- shift_left(table_data_df3,"1")

table_data_df4 <- table_data_df4[rowSums(is.na(table_data_df4[,1:ncol(table_data_df4)]))<ncol(table_data_df4),]
row.names(table_data_df4) <- seq(nrow(table_data_df4))

for(i in which(sapply(table_data_df4,class)=="character"))
{
  table_data_df4[[i]] = trim(table_data_df4[[i]])
}

name <- table_data_df4[4,2]

table_data_df5 <- table_data_df4[5:nrow(table_data_df4),1:3]
row.names(table_data_df5) <- seq(nrow(table_data_df5))


