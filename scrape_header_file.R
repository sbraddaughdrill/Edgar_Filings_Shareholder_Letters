library(gdata)
library(plyr)
library(RCurl)
library(XML)

directory <- normalizePath("F:/Research_temp3/", winslash = "\\", mustWork = TRUE)


###############################################################################
cat("SECTION: IMPORT URL", "\n")
###############################################################################

#Get page

url <- "http://www.sec.gov/Archives/edgar/data/1414040/000141404014000007/0001414040-14-000007.hdr.sgml"

webpage <- getURL(url,encoding="UTF-8")

tc <- textConnection(webpage)
webpage2 <- readLines(tc)
close(tc)

webpage_org_df <- as.data.frame(webpage2,stringsAsFactors=FALSE)
colnames(webpage_org_df) <- "raw"
webpage_org_df[,"raw"] <- toupper(webpage_org_df[,"raw"])

webpage_df_xml_only1 <- webpage_org_df
webpage_df_xml_only2 <- webpage_df_xml_only1[!(is.na(webpage_df_xml_only1) | webpage_df_xml_only1=="")]
webpage_df_xml_only3 <- gsub("&nbsp;"," ",webpage_df_xml_only2)

webpage_df_xml_only_df <- as.data.frame(webpage_df_xml_only3,stringsAsFactors=FALSE)
colnames(webpage_df_xml_only_df) <- c("raw")

rm(url,tc,webpage,webpage2,webpage_org_df)
rm(webpage_df_xml_only1,webpage_df_xml_only2,webpage_df_xml_only3)


###############################################################################
cat("SECTION: FIX TAGS", "\n")
###############################################################################

#Seperate tags and values
webpage_tags <- data.frame(tag_status_open=NA,
                           tag_status_close=NA,
                           raw=webpage_df_xml_only_df,
                           raw_encoded=NA,
                           tag=NA,
                           tag_short=NA,
                           type=NA,
                           value=NA,
                           stringsAsFactors=FALSE)
#Get Tag
webpage_tags[,"tag"] <-  gsub(".*?<(.*?)>.*", "\\1", webpage_tags[,"raw"]) 

#Clean Tag
for(i in 1:ncol(webpage_tags))
{
  webpage_tags[,i] <- iconv(webpage_tags[,i], "latin1", "ASCII", sub="")
}
rm(i)

for(i in which(sapply(webpage_tags,class)=="character"))
{
  webpage_tags[[i]] = trim(webpage_tags[[i]])
}
rm(i)

for (i in 1:ncol(webpage_tags))
{
  webpage_tags[,i] <- unknownToNA(webpage_tags[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                              NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                              NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  webpage_tags[,i] <- ifelse(is.na(webpage_tags[,i]),NA, webpage_tags[,i])
} 
rm(i)


#Determine is open/close tag
webpage_tags[,"type"] <-  ifelse(grepl("/", webpage_tags[,"tag"]), "close", "open")

#Get value
webpage_tags[,"value"] <- sapply(strsplit(webpage_tags[,"raw"], paste("<",webpage_tags[,"tag"],">",sep="")), "[", 2)


#Find all possible tags
raw_tags1 <- data.frame(raw=unique(webpage_tags[,"tag"]),
                        cleaned=unique(webpage_tags[,"tag"]),
                        stringsAsFactors=FALSE)

#Replace hyphens
raw_tags1[,"cleaned"] <- gsub("-","_",raw_tags1[,"cleaned"])

raw_tags1_trim <- raw_tags1[!(raw_tags1[,"raw"]==raw_tags1[,"cleaned"]),]
row.names(raw_tags1_trim) <- seq(nrow(raw_tags1_trim))
rm(raw_tags1)

for (i in 1:nrow(raw_tags1_trim))
{
  #i <- 1
  #webpage_tags[,"tag"] <- gsub(raw_tags1_trim[i,"raw"],raw_tags1_trim[i,"cleaned"],webpage_tags[,"tag"])
  #webpage_tags[,"tag_short"] <- gsub(raw_tags1_trim[i,"raw"],raw_tags1_trim[i,"cleaned"],webpage_tags[,"tag_short"])
  
  webpage_tags[,"tag"] <- ifelse(webpage_tags[,"tag"]==raw_tags1_trim[i,"raw"], raw_tags1_trim[i,"cleaned"], webpage_tags[,"tag"])
  
} 
rm(raw_tags1_trim,i)

#Remove closing tag from tag_short column
webpage_tags[,"tag_short"] <- gsub("/", "", webpage_tags[,"tag"])

#Add brackets to tags
webpage_tags[,"tag"] <- paste("<",webpage_tags[,"tag"],">",sep="")

#Find all possible tags
raw_tags2 <- data.frame(cleaned=unique(webpage_tags[,"tag_short"]),
                        open_count=NA,
                        close_count=NA,
                        stringsAsFactors=FALSE)

for (i in 1:nrow(raw_tags2))
{
  #i <- 1
  
  webpage_tags[,"tag_status_open"] <- ifelse(grepl(paste("<",raw_tags2[i,"cleaned"],">",sep=""), webpage_tags[,"tag"]), 1, 0)
  webpage_tags[,"tag_status_close"] <- ifelse(grepl(paste("</",raw_tags2[i,"cleaned"],">",sep=""), webpage_tags[,"tag"]), 1, 0)
  
  raw_tags2[i,"open_count"] <- sum(webpage_tags[,"tag_status_open"])
  raw_tags2[i,"close_count"] <- sum(webpage_tags[,"tag_status_close"])
  
  webpage_tags[,"tag_status_open"] <- NA
  webpage_tags[,"tag_status_close"] <- NA
} 
rm(i)

#Find tags that are not closed
raw_tags2_bad <- raw_tags2[which(raw_tags2[,"open_count"]-raw_tags2[,"close_count"] != 0),]
row.names(raw_tags2_bad) <- seq(nrow(raw_tags2_bad))
rm(raw_tags2)

#Find rows that need to be fixed
colnames(webpage_tags)[match("tag_status_open",names(webpage_tags))] <- "Final_tag"
colnames(webpage_tags)[match("tag_status_close",names(webpage_tags))] <- "Good"

#Find tags that need to be closed
webpage_tags[,"Good"] <- ifelse(webpage_tags[,"tag_short"] %in% raw_tags2_bad[,"cleaned"], 0, 1)
rm(raw_tags2_bad)

#Encode HTML entities
entity_encoding <- read.csv(file=paste(directory,"Entity_encoding.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

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
rm(i)

for (i in 1:ncol(entity_encoding_clean))
{
  entity_encoding_clean[,i] <- unknownToNA(entity_encoding_clean[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  entity_encoding_clean[,i] <- ifelse(is.na(entity_encoding_clean[,i]),NA, entity_encoding_clean[,i])
} 
rm(i)

entity_encoding_trim <- entity_encoding_clean[(!(is.na(entity_encoding_clean[,"ASCII.Looks.Like"])) & 
                                                 !(is.na(entity_encoding_clean[,"Entity.Encoding"]))),]
row.names(entity_encoding_trim) <- seq(nrow(entity_encoding_trim))

rm(entity_encoding,entity_encoding_clean)

#Replacement
for (i in 1:nrow(entity_encoding_trim))
{
  webpage_tags[,"value"] <- gsub(entity_encoding_trim[i,"ASCII.Looks.Like"], entity_encoding_trim[i,"Entity.Encoding"], webpage_tags[,"value"])
  
} 
rm(entity_encoding_trim,i)

#Clean
for(i in which(sapply(webpage_tags,class)=="character"))
{
  webpage_tags[[i]] = trim(webpage_tags[[i]])
}
rm(i)

for (i in 1:ncol(webpage_tags))
{
  webpage_tags[,i] <- unknownToNA(webpage_tags[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                              NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                              NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  webpage_tags[,i] <- ifelse(is.na(webpage_tags[,i]),NA, webpage_tags[,i])
} 
rm(i)

#Create raw encoded tags
webpage_tags[,"raw_encoded"] <- ifelse(is.na(webpage_tags[,"value"]),
                                       webpage_tags[,"tag"], 
                                       paste(webpage_tags[,"tag"],webpage_tags[,"value"],sep=""))

#Creat final tags
webpage_tags[,"Final_tag"] <- ifelse(webpage_tags[,"Good"]==1, 
                                     webpage_tags[,"raw_encoded"], 
                                     paste(webpage_tags[,"raw_encoded"],"</",webpage_tags[,"tag_short"],">",sep=""))


###############################################################################
cat("SECTION: SEPERATE SECTIONS", "\n")
###############################################################################

webpage_sep <- webpage_tags
webpage_sep[1,"Final_tag"] <- "<SEC_HEADER>"

#Find all possible tags
sep_tags1 <- data.frame(cleaned=unique(webpage_sep[,"tag_short"]),
                        open_count=NA,
                        close_count=NA,
                        stringsAsFactors=FALSE)

for (i in 1:nrow(sep_tags1))
{
  #i <- 1
  #i <- 2
  
  webpage_sep[,"tag_status_open"] <- ifelse(grepl(paste("<",sep_tags1[i,"cleaned"],">",sep=""), webpage_sep[,"Final_tag"]), 1, 0)
  webpage_sep[,"tag_status_close"] <- ifelse(grepl(paste("</",sep_tags1[i,"cleaned"],">",sep=""), webpage_sep[,"Final_tag"]), 1, 0)
  
  sep_tags1[i,"open_count"] <- sum(webpage_sep[,"tag_status_open"])
  sep_tags1[i,"close_count"] <- sum(webpage_sep[,"tag_status_close"])
  
  webpage_sep[,"tag_status_open"] <- NA
  webpage_sep[,"tag_status_close"] <- NA
} 
rm(i)


for (i in 1:nrow(sep_tags1))
{
  #i <- 1
  #i <- 2
  
  webpage_sep[,"tag_status_open"] <- ifelse(grepl(paste("<",sep_tags1[i,"cleaned"],">",sep=""), webpage_sep[,"Final_tag"]), 1, 0)
  webpage_sep[,"tag_status_close"] <- ifelse(grepl(paste("</",sep_tags1[i,"cleaned"],">",sep=""), webpage_sep[,"Final_tag"]), 1, 0)
  
  sep_tags1[i,"open_count"] <- sum(webpage_sep[,"tag_status_open"])
  sep_tags1[i,"close_count"] <- sum(webpage_sep[,"tag_status_close"])
  
  webpage_sep[,"tag_status_open"] <- NA
  webpage_sep[,"tag_status_close"] <- NA
} 
rm(i)

sep_tags2 <- sep_tags1[,"cleaned"]
rm(sep_tags1)

index_temp <- llply(.data=sep_tags2, .fun = function(x,data,tag_col){
  
  #x <- sep_tags2[1]
  #x <- sep_tags2[2]
  #x <- sep_tags2[34]
  #data <- webpage_sep
  #tag_col <- "Final_tag"
  
  temp_tag <- data.frame(temp_col=data[,tag_col], 
                         temp_open_flag=NA,
                         temp_open_index=NA,
                         temp_close_flag=NA,
                         temp_close_index=NA,
                         temp_diff_flag=NA,
                         temp_comb_flag=NA,
                         temp_comb_index=NA, 
                         stringsAsFactors=FALSE)
  
  temp_tag[,"temp_open_flag"] <- ifelse(grepl(paste("<",x,">",sep=""), temp_tag[,"temp_col"]), 1, 0)
  temp_tag[,"temp_open_index"] <- apply(data.frame(temp_tag[,"temp_open_flag"]), 2, cumsum)
  
  temp_tag[,"temp_close_flag"] <- ifelse(grepl(paste("</",x,">",sep=""), temp_tag[,"temp_col"]), 1, 0)
  temp_tag[,"temp_close_index"] <- apply(data.frame(temp_tag[,"temp_close_flag"]), 2, cumsum)
  
  temp_tag[,"temp_diff_flag"] <- (temp_tag[,"temp_open_index"]-temp_tag[,"temp_close_index"])
  
  temp_tag[,"temp_comb_flag"] <- (temp_tag[,"temp_close_flag"]+temp_tag[,"temp_diff_flag"])
  temp_tag[,"temp_comb_index"] <- ifelse(temp_tag[,"temp_comb_flag"]==1, temp_tag[,"temp_open_index"], 0)
  
  colnames(temp_tag) <- c(tag_col, 
                          paste(x,"OPEN_FLAG",sep="_"), paste(x,"OPEN_INDEX",sep="_"),
                          paste(x,"CLOSE_FLAG",sep="_"), paste(x,"CLOSE_INDEX",sep="_"),
                          paste(x,"DIFF_FLAG",sep="_"),
                          paste(x,"COMB_FLAG",sep="_"), paste(x,"COMB_INDEX",sep="_"))
  
  output <- temp_tag[,c(paste(x,"COMB_INDEX",sep="_"))]
  
  return(output)
  
}, data=webpage_sep, tag_col="Final_tag",
.progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)

index <- do.call(cbind, index_temp)
index <- as.data.frame(index,stringsAsFactors=FALSE)
colnames(index) <- paste(sep_tags2,"INDEX",sep="_")

rm(index_temp)

webpage_sep_index <- data.frame(Final_tag=webpage_sep[,"Final_tag"],index,stringsAsFactors=FALSE)

rm(sep_tags2,webpage_sep,index)


###############################################################################
cat("SECTION: HEADER SECTION", "\n")
###############################################################################

#Get Class Contract Info
header_index_val <- "SEC_HEADER"
header_data_temp <- webpage_sep_index[!(webpage_sep_index[,paste(header_index_val,"INDEX",sep="_")] %in% c(0)),]
row.names(header_data_temp) <- seq(nrow(header_data_temp))

header_info_sub_index_val <- c("FILER","SERIES_AND_CLASSES_CONTRACTS_DATA")
header_info <- ddply(.data=header_data_temp, .variables=paste(header_index_val,"INDEX",sep="_"), 
                     .fun = function(x,xml_col,index_col,sub_index_col){
                       
                       # x <- header_data_temp
                       # xml_col <- "Final_tag"
                       # index_col <- paste(header_index_val,"INDEX",sep="_")
                       # sub_index_col <- paste(header_info_sub_index_val,"INDEX",sep="_")
                       
                       xml_col_num <- which(colnames(x)==xml_col)
                       index_col_num <- which(colnames(x)==index_col)
                       sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
                       sub_index_col_num_max <- max(sub_index_col_num)
                       
                       x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]

                       keep_rows <- sort(unique(unlist(lapply(sub_index_col, function(y,data){ which(data[,y]!=0) },data=x_trim))))
                       drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
                
                       x_trim2 <- x_trim[c(1,drop_rows,nrow(x_trim)),]

                       temp_df <- xmlToDataFrame(x_trim2[,xml_col])
                       
                     }, xml_col="Final_tag",index_col=paste(header_index_val,"INDEX",sep="_"),
                     sub_index_col=paste(header_info_sub_index_val,"INDEX",sep="_"), 
                     .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

rm(header_info_sub_index_val)
rm(header_index_val,header_data_temp)




###############################################################################
cat("SECTION: FILER SECTION", "\n")
###############################################################################

filer_index_val <- "FILER"
filer_data_temp <- webpage_sep_index[!(webpage_sep_index[,paste(filer_index_val,"INDEX",sep="_")] %in% c(0)),]
row.names(filer_data_temp) <- seq(nrow(filer_data_temp))

filer_company_data_sub_index_val <- "COMPANY_DATA"
filer_company_data <- ddply(.data=filer_data_temp, .variables=paste(filer_index_val,"INDEX",sep="_"), 
                            .fun = function(x,xml_col,index_col,sub_index_col){
                              
                              # x <- filer_data_temp[filer_data_temp[,paste(filer_index_val,"INDEX",sep="_")]==1,]
                              # xml_col <- "Final_tag"
                              # index_col <- paste(filer_index_val,"INDEX",sep="_")
                              # sub_index_col <- paste(filer_company_data_sub_index_val,"INDEX",sep="_")
                              
                              xml_col_num <- which(colnames(x)==xml_col)
                              index_col_num <- which(colnames(x)==index_col)
                              sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
                              sub_index_col_num_max <- max(sub_index_col_num)
                              
                              x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
                              
                              keep_rows <- sort(unique(unlist(lapply(sub_index_col, function(y,data){ which(data[,y]!=0) },data=x_trim))))
                              #drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
                              
                              x_trim2 <- x_trim[c(1,keep_rows,nrow(x_trim)),]
                              
                              temp_df <- xmlToDataFrame(x_trim2[,xml_col])
                              
                            }, xml_col="Final_tag",index_col=paste(filer_index_val,"INDEX",sep="_"),
                            sub_index_col=paste(filer_company_data_sub_index_val,"INDEX",sep="_"), 
                            .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

filer_filing_values_sub_index_val <- "FILING_VALUES"
filer_filing_values <- ddply(.data=filer_data_temp, .variables=paste(filer_index_val,"INDEX",sep="_"), 
                             .fun = function(x,xml_col,index_col,sub_index_col){
                               
                               xml_col_num <- which(colnames(x)==xml_col)
                               index_col_num <- which(colnames(x)==index_col)
                               sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
                               sub_index_col_num_max <- max(sub_index_col_num)
                               
                               x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
                               
                               keep_rows <- sort(unique(unlist(lapply(sub_index_col, function(y,data){ which(data[,y]!=0) },data=x_trim))))
                               #drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
                               
                               x_trim2 <- x_trim[c(1,keep_rows,nrow(x_trim)),]
                               
                               temp_df <- xmlToDataFrame(x_trim2[,xml_col])
                               
                             }, xml_col="Final_tag",index_col=paste(filer_index_val,"INDEX",sep="_"),
                             sub_index_col=paste(filer_filing_values_sub_index_val,"INDEX",sep="_"), 
                             .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

filer_merge1 <- merge(filer_company_data, filer_filing_values, 
                      by.x=c(paste(filer_index_val,"INDEX",sep="_")),by.y=c(paste(filer_index_val,"INDEX",sep="_")),
                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables = NA)

rm(filer_company_data_sub_index_val,filer_filing_values_sub_index_val,filer_company_data, filer_filing_values)

filer_business_address_sub_index_val <- "BUSINESS_ADDRESS"
filer_business_address <- ddply(.data=filer_data_temp, .variables=paste(filer_index_val,"INDEX",sep="_"), 
                                .fun = function(x,xml_col,index_col,sub_index_col){
                                  
                                  xml_col_num <- which(colnames(x)==xml_col)
                                  index_col_num <- which(colnames(x)==index_col)
                                  sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
                                  sub_index_col_num_max <- max(sub_index_col_num)
                                  
                                  x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
                                  
                                  keep_rows <- sort(unique(unlist(lapply(sub_index_col, function(y,data){ which(data[,y]!=0) },data=x_trim))))
                                  #drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
                                  
                                  x_trim2 <- x_trim[c(1,keep_rows,nrow(x_trim)),]
                                  
                                  temp_df <- xmlToDataFrame(x_trim2[,xml_col])
                                  
                                }, xml_col="Final_tag",index_col=paste(filer_index_val,"INDEX",sep="_"),
                                sub_index_col=paste(filer_business_address_sub_index_val,"INDEX",sep="_"), 
                                .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

filer_business_non_index_cols <- colnames(filer_business_address[,!(colnames(filer_business_address) %in% paste(filer_index_val,"INDEX",sep="_"))])
filer_business_address <- filer_business_address[,c(paste(filer_index_val,"INDEX",sep="_"),filer_business_non_index_cols)]
colnames(filer_business_address) <- c(paste(filer_index_val,"INDEX",sep="_"),paste(filer_business_non_index_cols,"BUSINESS",sep="_"))

filer_merge2 <- merge(filer_merge1, filer_business_address, 
                      by.x=c(paste(filer_index_val,"INDEX",sep="_")),by.y=c(paste(filer_index_val,"INDEX",sep="_")),
                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables = NA)

rm(filer_business_address_sub_index_val,filer_business_non_index_cols,filer_merge1,filer_business_address)

filer_mail_address_sub_index_val <- "MAIL_ADDRESS"
filer_mail_address <- ddply(.data=filer_data_temp, .variables=paste(filer_index_val,"INDEX",sep="_"), 
                            .fun = function(x,xml_col,index_col,sub_index_col){
                              
                              xml_col_num <- which(colnames(x)==xml_col)
                              index_col_num <- which(colnames(x)==index_col)
                              sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
                              sub_index_col_num_max <- max(sub_index_col_num)
                              
                              x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
                              
                              keep_rows <- sort(unique(unlist(lapply(sub_index_col, function(y,data){ which(data[,y]!=0) },data=x_trim))))
                              #drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
                              
                              x_trim2 <- x_trim[c(1,keep_rows,nrow(x_trim)),]
                              
                              temp_df <- xmlToDataFrame(x_trim2[,xml_col])
                              
                            }, xml_col="Final_tag",index_col=paste(filer_index_val,"INDEX",sep="_"),
                            sub_index_col=paste(filer_mail_address_sub_index_val,"INDEX",sep="_"), 
                            .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

filer_mail_non_index_cols <- colnames(filer_mail_address[,!(colnames(filer_mail_address) %in% paste(filer_index_val,"INDEX",sep="_"))])
filer_mail_address <- filer_mail_address[,c(paste(filer_index_val,"INDEX",sep="_"),filer_mail_non_index_cols)]
colnames(filer_mail_address) <- c(paste(filer_index_val,"INDEX",sep="_"),paste(filer_mail_non_index_cols,"MAIL",sep="_"))

filer_merge3 <- merge(filer_merge2, filer_mail_address, 
                      by.x=c(paste(filer_index_val,"INDEX",sep="_")),by.y=c(paste(filer_index_val,"INDEX",sep="_")),
                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables = NA)

rm(filer_mail_address_sub_index_val,filer_mail_non_index_cols,filer_merge2,filer_mail_address)
rm(filer_index_val,filer_data_temp)


###############################################################################
cat("SECTION: SERIES SECTION", "\n")
###############################################################################

#Get Class Contract Info
series_index_val <- "SERIES"
series_data_temp <- webpage_sep_index[!(webpage_sep_index[,paste(series_index_val,"INDEX",sep="_")] %in% c(0)),]
row.names(series_data_temp) <- seq(nrow(series_data_temp))

series_other_sub_index_val <- "CLASS_CONTRACT"
series_other <- ddply(.data=series_data_temp, .variables=paste(series_index_val,"INDEX",sep="_"), 
                      .fun = function(x,xml_col,index_col,sub_index_col){
                        
                        # x <- series_data_temp[series_data_temp[,paste(series_index_val,"INDEX",sep="_")]==1,]
                        # xml_col <- "Final_tag"
                        # index_col <- paste(series_index_val,"INDEX",sep="_")
                        # sub_index_col <- paste(series_class_contract_sub_index_val,"INDEX",sep="_")
                        
                        xml_col_num <- which(colnames(x)==xml_col)
                        index_col_num <- which(colnames(x)==index_col)
                        sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
                        sub_index_col_num_max <- max(sub_index_col_num)
                        
                        x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
                        
                        keep_rows <- sort(unique(unlist(lapply(sub_index_col, function(y,data){ which(data[,y]!=0) },data=x_trim))))
                        drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
                        
                        x_trim2 <- x_trim[c(1,drop_rows,nrow(x_trim)),]
                        
                        temp_df <- xmlToDataFrame(x_trim2[,xml_col])
                        
                      }, xml_col="Final_tag",index_col=paste(series_index_val,"INDEX",sep="_"),
                      sub_index_col=paste(series_other_sub_index_val,"INDEX",sep="_"), 
                      .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

series_class_contract_sub_index_val <- "CLASS_CONTRACT"
series_class_contract <- ddply(.data=series_data_temp, .variables=paste(series_index_val,"INDEX",sep="_"), 
                               .fun = function(x,xml_col,index_col,sub_index_col){
                                 
                                 # x <- series_data_temp[series_data_temp[,paste(series_index_val,"INDEX",sep="_")]==1,]
                                 # xml_col <- "Final_tag"
                                 # index_col <- paste(series_index_val,"INDEX",sep="_")
                                 # sub_index_col <- paste(series_class_contract_sub_index_val,"INDEX",sep="_")
                                 
                                 xml_col_num <- which(colnames(x)==xml_col)
                                 index_col_num <- which(colnames(x)==index_col)
                                 sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
                                 sub_index_col_num_max <- max(sub_index_col_num)
                                 
                                 x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
                                 
                                 keep_rows <- sort(unique(unlist(lapply(sub_index_col, function(y,data){ which(data[,y]!=0) },data=x_trim))))
                                 #drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
                                 
                                 x_trim2 <- x_trim[c(1,keep_rows,nrow(x_trim)),]
                                 
                                 temp_df <- xmlToDataFrame(x_trim2[,xml_col])
                                 
                               }, xml_col="Final_tag",index_col=paste(series_index_val,"INDEX",sep="_"),
                               sub_index_col=paste(series_class_contract_sub_index_val,"INDEX",sep="_"), 
                               .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

series_merge1 <- merge(series_other,series_class_contract,
                       by.x=c(paste(series_index_val,"INDEX",sep="_")),by.y=c(paste(series_index_val,"INDEX",sep="_")),
                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables = NA)

rm(series_class_contract_sub_index_val,series_other_sub_index_val,series_class_contract,series_other)
rm(series_index_val,series_data_temp)


