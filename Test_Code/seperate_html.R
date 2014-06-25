library(gdata)
library(plyr)
library(RCurl)
library(XML)

directory <- normalizePath("F:/Research_temp3/", winslash = "\\", mustWork = TRUE)


###############################################################################
cat("SECTION: IMPORT URL", "\n")
###############################################################################

#Get page

file_path <- paste(directory,"MF_Shareholder_Reports_N-CSR","//","2003","//","original","//",sep="")

filename <- "0000014170-03-000011.txt"

file_path_full <- paste(file_path,filename,sep="")

webpage2 <- readLines(file_path_full)

webpage_org_df <- as.data.frame(webpage2,stringsAsFactors=FALSE)
colnames(webpage_org_df) <- "raw"
webpage_org_df[,"raw"] <- toupper(webpage_org_df[,"raw"])

webpage_df_xml_only1 <- webpage_org_df
webpage_df_xml_only2 <- webpage_df_xml_only1[!(is.na(webpage_df_xml_only1) | webpage_df_xml_only1=="")]
webpage_df_xml_only3 <- gsub("&NBSP;"," ",webpage_df_xml_only2)

webpage_df_xml_only_df <- as.data.frame(webpage_df_xml_only3,stringsAsFactors=FALSE)
colnames(webpage_df_xml_only_df) <- c("raw")


#####################FIX HMTL


sep_tags2 <- c("HTML","BODY")

index_temp <- llply(.data=sep_tags2, .fun = function(x,data,tag_col){
  
  #x <- sep_tags2[1]
  #x <- sep_tags2[2]
  #x <- sep_tags2[34]
  #data <- webpage_org_df
  #tag_col <- "raw"
  
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
  
}, data=webpage_df_xml_only_df, tag_col="raw",
.progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)

index <- do.call(cbind, index_temp)
index <- as.data.frame(index,stringsAsFactors=FALSE)
colnames(index) <- paste(sep_tags2,"INDEX",sep="_")

rm(index_temp)

webpage_sep_index <- data.frame(raw=webpage_df_xml_only_df[,"raw"],index,stringsAsFactors=FALSE)

#Seperate HTML sections
header_index_val <- "HTML"
header_data_temp <- webpage_sep_index[!(webpage_sep_index[,paste(header_index_val,"INDEX",sep="_")] %in% c(0)),]
row.names(header_data_temp) <- seq(nrow(header_data_temp))

header_info_sub_index_val <- c("BODY")
header_info <- ddply(.data=header_data_temp, .variables=paste(header_index_val,"INDEX",sep="_"), 
                     .fun = function(x,xml_col,index_col,sub_index_col){
                       
                       # x <- header_data_temp[header_data_temp[,paste(header_index_val,"INDEX",sep="_")]==1,]
                       # x <- header_data_temp[header_data_temp[,paste(header_index_val,"INDEX",sep="_")]==2,]
                       # xml_col <- "raw"
                       # index_col <- paste(header_index_val,"INDEX",sep="_")
                       # sub_index_col <- paste(header_info_sub_index_val,"INDEX",sep="_")
                       
                       xml_col_num <- which(colnames(x)==xml_col)
                       index_col_num <- which(colnames(x)==index_col)
                       sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
                       sub_index_col_num_max <- max(sub_index_col_num)
                       
                       #x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
                       
                       #keep_rows <- sort(unique(unlist(lapply(sub_index_col, function(y,data){ which(data[,y]!=0) },data=x_trim))))
                       #drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
                       
                       #x_trim2 <- x_trim[c(1,drop_rows,nrow(x_trim)),]
                       
                       #temp_df <- htmlTreeParse(x[,xml_col], useInternalNodes = T)
                       #temp_df2 <- xmlToDataFrame(temp_df)
                       
                       temp_df <- htmlParse(x[,xml_col], asText=TRUE)
                       plain.text <- xpathSApply(temp_df, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
                       plain.text2 <- as.data.frame(plain.text,stringsAsFactors=FALSE)
                       #plain.text2 <- paste(plain.text, collapse = " ")
                       
                       plain.text2[,1]  <- iconv(plain.text2[,1] , "latin1", "ASCII", sub=" ")
                       plain.text2[,1] <- gsub(" {2,}", " ", plain.text2[,1])
                       plain.text2[,1] <- gsub(" {2,}", " ", plain.text2[,1])
                       plain.text2[,1] <- gsub(" {2,}", " ", plain.text2[,1])
                       plain.text2[,1] <- gsub(" {2,}", " ", plain.text2[,1])
                       plain.text2[,1] <- gsub("^\\s+|\\s+$", "", plain.text2[,1])
                       
                       plain.text3 <-   plain.text2[!(plain.text2[,1]==""),1]
                       plain.text3 <- as.data.frame(plain.text3,stringsAsFactors=FALSE)
                       
                     }, xml_col="raw",index_col=paste(header_index_val,"INDEX",sep="_"),
                     sub_index_col=paste(header_info_sub_index_val,"INDEX",sep="_"), 
                     .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

rm(header_info_sub_index_val)
rm(header_index_val,header_data_temp)










