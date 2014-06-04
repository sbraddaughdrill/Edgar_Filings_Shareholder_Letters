# 
# import_local_edgar_file <- function(file){
#   
#   #file <- filepath
#   
#   #webpage2 <- read.table(file=file,header = FALSE, na.strings="NA",stringsAsFactors=FALSE, 
#   #                       sep = "", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
#   #webpage2 <- read.table(file=file,header = FALSE, na.strings="",sep="\t",stringsAsFactors=FALSE)
#   webpage2 <- read.table(file=file,header = FALSE, na.strings="",sep="\n",stringsAsFactors=FALSE)
#   webpage2_row_count <- length(count.fields(file=file, sep = "\n"))
#   
#   if(nrow(webpage2)!=webpage2_row_count) { cat("ROW COUNTS NOT EQUAL","\n") }
#   
#   webpage_org_df <- as.data.frame(webpage2,stringsAsFactors=FALSE)
#   colnames(webpage_org_df) <- "raw"
#   webpage_org_df[,"raw"] <- toupper(webpage_org_df[,"raw"])
#   
#   webpage_df_xml_only1 <- webpage_org_df
#   webpage_df_xml_only2 <- webpage_df_xml_only1[!(is.na(webpage_df_xml_only1) | webpage_df_xml_only1=="")]
#   
#   webpage_df_xml_only_df <- as.data.frame(webpage_df_xml_only2,stringsAsFactors=FALSE)
#   colnames(webpage_df_xml_only_df) <- c("raw")
#   
#   return(webpage_df_xml_only_df)
#   
# }  
# 
# fix_edgar_tags <- function(file,entity_encoding){
#   
#   #file <- webpage_df_xml_only_df[,"raw"]
#   #entity_encoding <- entity_encoding_trim
#   
#   require(gdata)
#   
#   #Seperate tags and values
#   webpage_tags <- data.frame(tag_status_open=NA,
#                              tag_status_close=NA,
#                              raw=file,
#                              raw_encoded=NA,
#                              tag=NA,
#                              tag_short=NA,
#                              type=NA,
#                              value=NA,
#                              stringsAsFactors=FALSE)
#   #Get Tag
#   webpage_tags[,"tag"] <-  gsub(".*?<(.*?)>.*", "\\1", webpage_tags[,"raw"]) 
#   
#   #Clean Tag
#   for(i in 1:ncol(webpage_tags))
#   {
#     webpage_tags[,i] <- iconv(webpage_tags[,i], "latin1", "ASCII", sub=" ")
#   }
#   rm(i)
#   
#   for(i in which(sapply(webpage_tags,class)=="character"))
#   {
#     webpage_tags[[i]] = trim(webpage_tags[[i]])
#   }
#   rm(i)
#   
#   for (i in 1:ncol(webpage_tags))
#   {
#     webpage_tags[,i] <- unknownToNA(webpage_tags[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
#                                                                 NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
#                                                                 NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
#     webpage_tags[,i] <- ifelse(is.na(webpage_tags[,i]),NA, webpage_tags[,i])
#   } 
#   rm(i)
#   
#   
#   #Determine is open/close tag
#   webpage_tags[,"type"] <-  ifelse(grepl("/", webpage_tags[,"tag"]), "close", "open")
#   
#   #Get value
#   webpage_tags[,"value"] <- sapply(strsplit(webpage_tags[,"raw"], paste("<",webpage_tags[,"tag"],">",sep="")), "[", 2)
#   
#   
#   #Find all possible tags
#   raw_tags1 <- data.frame(raw=unique(webpage_tags[,"tag"]),
#                           cleaned=unique(webpage_tags[,"tag"]),
#                           stringsAsFactors=FALSE)
#   
#   #Replace hyphens
#   raw_tags1[,"cleaned"] <- gsub("-","_",raw_tags1[,"cleaned"])
#   
#   raw_tags1_trim <- raw_tags1[!(raw_tags1[,"raw"]==raw_tags1[,"cleaned"]),]
#   row.names(raw_tags1_trim) <- seq(nrow(raw_tags1_trim))
#   rm(raw_tags1)
#   
#   for (i in 1:nrow(raw_tags1_trim))
#   {
#     #i <- 1
#     #webpage_tags[,"tag"] <- gsub(raw_tags1_trim[i,"raw"],raw_tags1_trim[i,"cleaned"],webpage_tags[,"tag"])
#     #webpage_tags[,"tag_short"] <- gsub(raw_tags1_trim[i,"raw"],raw_tags1_trim[i,"cleaned"],webpage_tags[,"tag_short"])
#     
#     webpage_tags[,"tag"] <- ifelse(webpage_tags[,"tag"]==raw_tags1_trim[i,"raw"], raw_tags1_trim[i,"cleaned"], webpage_tags[,"tag"])
#     
#   } 
#   rm(raw_tags1_trim,i)
#   
#   #Remove closing tag from tag_short column
#   webpage_tags[,"tag_short"] <- gsub("/", "", webpage_tags[,"tag"])
#   
#   #Add brackets to tags
#   webpage_tags[,"tag"] <- paste("<",webpage_tags[,"tag"],">",sep="")
#   
#   #Find all possible tags
#   raw_tags2 <- data.frame(cleaned=unique(webpage_tags[,"tag_short"]),
#                           open_count=NA,
#                           close_count=NA,
#                           stringsAsFactors=FALSE)
#   
#   for (i in 1:nrow(raw_tags2))
#   {
#     #i <- 1
#     
#     webpage_tags[,"tag_status_open"] <- ifelse(grepl(paste("<",raw_tags2[i,"cleaned"],">",sep=""), webpage_tags[,"tag"]), 1, 0)
#     webpage_tags[,"tag_status_close"] <- ifelse(grepl(paste("</",raw_tags2[i,"cleaned"],">",sep=""), webpage_tags[,"tag"]), 1, 0)
#     
#     raw_tags2[i,"open_count"] <- sum(webpage_tags[,"tag_status_open"])
#     raw_tags2[i,"close_count"] <- sum(webpage_tags[,"tag_status_close"])
#     
#     webpage_tags[,"tag_status_open"] <- NA
#     webpage_tags[,"tag_status_close"] <- NA
#   } 
#   rm(i)
#   
#   #Find tags that are not closed
#   raw_tags2_bad <- raw_tags2[which(raw_tags2[,"open_count"]-raw_tags2[,"close_count"] != 0),]
#   row.names(raw_tags2_bad) <- seq(nrow(raw_tags2_bad))
#   rm(raw_tags2)
#   
#   #Find rows that need to be fixed
#   colnames(webpage_tags)[match("tag_status_open",names(webpage_tags))] <- "Final_tag"
#   colnames(webpage_tags)[match("tag_status_close",names(webpage_tags))] <- "Good"
#   
#   #Find tags that need to be closed
#   webpage_tags[,"Good"] <- ifelse(webpage_tags[,"tag_short"] %in% raw_tags2_bad[,"cleaned"], 0, 1)
#   rm(raw_tags2_bad)
#   
#   #Entity Replacement
#   for (i in 1:nrow(entity_encoding))
#   {
#     webpage_tags[,"value"] <- gsub(entity_encoding[i,"ASCII.Looks.Like"], entity_encoding[i,"Entity.Encoding"], webpage_tags[,"value"])
#     
#   } 
#   rm(entity_encoding,i)
#   
#   #Clean
#   for(i in which(sapply(webpage_tags,class)=="character"))
#   {
#     webpage_tags[[i]] = trim(webpage_tags[[i]])
#   }
#   rm(i)
#   
#   for (i in 1:ncol(webpage_tags))
#   {
#     webpage_tags[,i] <- unknownToNA(webpage_tags[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
#                                                                 NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
#                                                                 NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
#     webpage_tags[,i] <- ifelse(is.na(webpage_tags[,i]),NA, webpage_tags[,i])
#   } 
#   rm(i)
#   
#   #Create raw encoded tags
#   webpage_tags[,"raw_encoded"] <- ifelse(is.na(webpage_tags[,"value"]),
#                                          webpage_tags[,"tag"], 
#                                          paste(webpage_tags[,"tag"],webpage_tags[,"value"],sep=""))
#   
#   #Creat final tags
#   webpage_tags[,"Final_tag"] <- ifelse(webpage_tags[,"Good"]==1, 
#                                        webpage_tags[,"raw_encoded"], 
#                                        paste(webpage_tags[,"raw_encoded"],"</",webpage_tags[,"tag_short"],">",sep=""))
#   
#   return(webpage_tags)
#   
# }
# 
# find_individual_tags <- function(data,tag_raw_col,tag_short_col,tag_open_col,tag_close_col){
#   
#   #data <- webpage_sep
#   #tag_raw_col <- "Final_tag" 
#   #tag_short_col <- "tag_short"
#   #tag_open_col <- "tag_status_open"
#   #tag_close_col <- "tag_status_close"
#   
#   sep_tags1 <- data.frame(cleaned=unique(data[,tag_short_col]),
#                           open_count=NA,
#                           close_count=NA,
#                           stringsAsFactors=FALSE)
#   
#   for (i in 1:nrow(sep_tags1))
#   {
#     #i <- 1
#     #i <- 2
#     
#     data[,tag_open_col] <- ifelse(grepl(paste("<",sep_tags1[i,"cleaned"],">",sep=""), data[,tag_raw_col]), 1, 0)
#     data[,tag_close_col] <- ifelse(grepl(paste("</",sep_tags1[i,"cleaned"],">",sep=""), data[,tag_raw_col]), 1, 0)
#     
#     sep_tags1[i,"open_count"] <- sum(data[,tag_open_col])
#     sep_tags1[i,"close_count"] <- sum(data[,tag_close_col])
#     
#     data[,tag_open_col] <- NA
#     data[,tag_close_col] <- NA
#   } 
#   rm(i)
#   
#   return(sep_tags1)
#   
# }
# 
# create_tag_index <- function(tag,data,tag_raw_col){
#   
#   #tag <- sep_tags2[1]
#   #data <- webpage_sep
#   #tag_raw_col <- "Final_tag"
#   
#   temp_tag <- data.frame(temp_col=data[,tag_raw_col], 
#                          temp_open_flag=NA,
#                          temp_open_index=NA,
#                          temp_close_flag=NA,
#                          temp_close_index=NA,
#                          temp_diff_flag=NA,
#                          temp_comb_flag=NA,
#                          temp_comb_index=NA, 
#                          stringsAsFactors=FALSE)
#   
#   temp_tag[,"temp_open_flag"] <- ifelse(grepl(paste("<",tag,">",sep=""), temp_tag[,"temp_col"]), 1, 0)
#   temp_tag[,"temp_open_index"] <- apply(data.frame(temp_tag[,"temp_open_flag"]), 2, cumsum)
#   
#   temp_tag[,"temp_close_flag"] <- ifelse(grepl(paste("</",tag,">",sep=""), temp_tag[,"temp_col"]), 1, 0)
#   temp_tag[,"temp_close_index"] <- apply(data.frame(temp_tag[,"temp_close_flag"]), 2, cumsum)
#   
#   temp_tag[,"temp_diff_flag"] <- (temp_tag[,"temp_open_index"]-temp_tag[,"temp_close_index"])
#   
#   temp_tag[,"temp_comb_flag"] <- (temp_tag[,"temp_close_flag"]+temp_tag[,"temp_diff_flag"])
#   temp_tag[,"temp_comb_index"] <- ifelse(temp_tag[,"temp_comb_flag"]==1, temp_tag[,"temp_open_index"], 0)
#   
#   colnames(temp_tag) <- c(tag_raw_col, 
#                           paste(tag,"OPEN_FLAG",sep="_"), paste(tag,"OPEN_INDEX",sep="_"),
#                           paste(tag,"CLOSE_FLAG",sep="_"), paste(tag,"CLOSE_INDEX",sep="_"),
#                           paste(tag,"DIFF_FLAG",sep="_"),
#                           paste(tag,"COMB_FLAG",sep="_"), paste(tag,"COMB_INDEX",sep="_"))
#   
#   output <- temp_tag[,c(paste(tag,"COMB_INDEX",sep="_"))]
#   
#   return(output)
#   
# }
# 
# # extract_header_intro_section <- function(x,xml_col,index_col,sub_index_col){
# #   
# #   # header_index_val <- "SEC_HEADER"
# #   # header_info_sub_index_val <- c("FILER","SERIES_AND_CLASSES_CONTRACTS_DATA")
# #   # header_info_sub_index_val <- c("FILER")
# #   
# #   # xml_col <- "Final_tag"
# #   # index_col <- paste(header_index_val,"INDEX",sep="_")
# #   # sub_index_col <- paste(header_info_sub_index_val,"INDEX",sep="_")
# #   # x <- header_data_temp[header_data_temp[,index_col]==1,]
# #   
# #   require(XML)
# #   
# #   xml_col_num <- which(colnames(x)==xml_col)
# #   index_col_num <- which(colnames(x)==index_col)
# #   sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
# #   sub_index_col_num_max <- max(sub_index_col_num)
# #   
# #   x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
# #   
# #   keep_rows <- lapply(sub_index_col, function(y,data){
# #     
# #     #y <- "FILER_INDEX"
# #     #y <- "SERIES_AND_CLASSES_CONTRACTS_DATA_INDEX"
# #     #data <- x_trim
# #     
# #     if(y %in% colnames(data)) {
# #       
# #       return(which(data[,y]!=0))
# #       
# #     } else {
# #       
# #       return(NA)
# #       
# #     }
# #     
# #   },data=x_trim)
# #   keep_rows <- sort(unique(unlist(keep_rows)))
# #   drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
# #   
# #   x_trim2 <- x_trim[c(1,drop_rows,nrow(x_trim)),]
# #   
# #   temp_df <- xmlToDataFrame(x_trim2[,xml_col])
# #   
# #   return(temp_df)
# # }
# 
# 
# 
# extract_filing_section_by_drop <- function(x,xml_col,index_col,sub_index_col){
#   
#   # filer_index_val <- "FILER"
#   # filer_company_data_sub_index_val <- "COMPANY_DATA"
#   
#   # xml_col <- "Final_tag"
#   # index_col <- paste(filer_index_val,"INDEX",sep="_")
#   # sub_index_col <- paste(filer_company_data_sub_index_val,"INDEX",sep="_")
#   # x <- filer_data_temp[filer_data_temp[,index_col]==1,]
#   
#   require(XML)
#   
#   xml_col_num <- which(colnames(x)==xml_col)
#   index_col_num <- which(colnames(x)==index_col)
#   sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
#   sub_index_col_num_max <- max(sub_index_col_num)
#   
#   x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
#   
#   keep_rows <- lapply(sub_index_col, function(y,data){
#     
#     #y <- "FILER_INDEX"
#     #y <- "SERIES_AND_CLASSES_CONTRACTS_DATA_INDEX"
#     #data <- x_trim
#     
#     if(y %in% colnames(data)) {
#       
#       return(which(data[,y]!=0))
#       
#     } else {
#       
#       return(NA)
#       
#     }
#     
#   },data=x_trim)
#   keep_rows <- sort(unique(unlist(keep_rows)))
#   drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
#   
#   x_trim2 <- x_trim[c(1,drop_rows,nrow(x_trim)),]
#   
#   temp_df <- xmlToDataFrame(x_trim2[,xml_col])
#   
#   return(temp_df)
# }
# 
# extract_filing_section_by_keep <- function(x,xml_col,index_col,sub_index_col){
#   
#   # filer_index_val <- "FILER"
#   # filer_company_data_sub_index_val <- "COMPANY_DATA"
#   
#   # xml_col <- "Final_tag"
#   # index_col <- paste(filer_index_val,"INDEX",sep="_")
#   # sub_index_col <- paste(filer_company_data_sub_index_val,"INDEX",sep="_")
#   # x <- filer_data_temp[filer_data_temp[,index_col]==1,]
#   
#   require(XML)
#   
#   xml_col_num <- which(colnames(x)==xml_col)
#   index_col_num <- which(colnames(x)==index_col)
#   sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
#   sub_index_col_num_max <- max(sub_index_col_num)
#   
#   x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
#   
#   keep_rows <- lapply(sub_index_col, function(y,data){
#     
#     #y <- "FILER_INDEX"
#     #y <- "SERIES_AND_CLASSES_CONTRACTS_DATA_INDEX"
#     #data <- x_trim
#     
#     if(y %in% colnames(data)) {
#       
#       return(which(data[,y]!=0))
#       
#     } else {
#       
#       return(NA)
#       
#     }
#     
#   },data=x_trim)
#   keep_rows <- sort(unique(unlist(keep_rows)))
#   drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
#   
#   x_trim2 <- x_trim[c(1,keep_rows,nrow(x_trim)),]
#   
#   temp_df <- xmlToDataFrame(x_trim2[,xml_col])
#   
#   return(temp_df)
# }
# 
# create_sub_index_sequence <- function(data,index_val,sub_index_val,nonindex_prefix){
#   
#   #data <- filer_company_data
#   #index_val <- paste(filer_index_val,"INDEX",sep="_")
#   #sub_index_val <- paste(filer_company_data_sub_index_val,"INDEX",sep="_")
#   #nonindex_prefix <- ""
#   
#   #data <- series_class_contract
#   #index_val <- paste(series_index_val,"INDEX",sep="_")
#   #sub_index_val <- paste(series_class_contract_sub_index_val,"INDEX",sep="_")
#   #nonindex_prefix <- ""
#   
#   #data <- filer_business_address
#   #index_val <- paste(filer_index_val,"INDEX",sep="_")
#   #sub_index_val <- paste(filer_business_address_sub_index_val,"INDEX",sep="_")
#   #nonindex_prefix <- "_BUSINESS"
#   
#   require(plyr)
#   
#   data2 <- ddply(.data=data,.variables=index_val,.fun=function(x,sub_index_val){
#     data2_temp <- data.frame(tempseq=NA,x,stringsAsFactors=FALSE)
#     data2_temp[,"tempseq"] <- seq(1,nrow(data2_temp),1)
#     colnames(data2_temp)[1] <- sub_index_val
#     
#     return(data2_temp)
#     
#   },sub_index_val=sub_index_val)
#   data2[sapply(data2, is.factor)] <- lapply(data2[sapply(data2, is.factor)], as.character)
#   
#   data2_cols <- colnames(data2)
#   filer_company_data_index_cols <- c("file",index_val,sub_index_val)
#   filer_company_data_non_index_cols <- data2_cols[!(data2_cols %in% filer_company_data_index_cols)]
#   data3 <- data2[,c(filer_company_data_index_cols,filer_company_data_non_index_cols)]
#   
#   data4 <- data3
#   colnames(data4) <- c(filer_company_data_index_cols,paste(filer_company_data_non_index_cols,nonindex_prefix,sep=""))
#   
#   data4 <- data4[order(data4[,"file"],data4[,index_val],data4[,sub_index_val]),]
#   row.names(data4) <- seq(nrow(data4))
#   
#   rm(data2,data3, filer_company_data_index_cols,filer_company_data_non_index_cols)
#   
#   return(data4)
# }
# 
