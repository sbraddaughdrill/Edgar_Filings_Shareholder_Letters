# TODO: Add comment
# 
# Author:  Brad
# File:    Get_Header_Info.R
# Version: 1.0
# Date:    06.02.2014
# Purpose: Get the header information
###############################################################################

###############################################################################
# INITIAL SETUP;
cat("SECTION: INITIAL SETUP", "\n")
###############################################################################

# Clear workspace
rm(list = ls(all = TRUE))

# Limit History to not exceed 50 lines
Sys.setenv(R_HISTSIZE = 500)

repo <- c("http://cran.us.r-project.org")
options(repos = structure(repo))
options(install.packages.check.source = FALSE)
# String as factors is False -- used for read.csv
options(StringsAsFactors = FALSE)

# Default maxprint option
options(max.print = 500)
# options(max.print=99999)

# Memory limit
#memory.limit(size = 8183)

# Set location (1=HOME,2=WORK,3=CORALSEA FROM HOME,4=CORALSEA FROM WORK) Location <- 1
Location <- 1

if (Location == 1) {
  #setwd("C:/Research_temp3/")
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Fund_Letters/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 2) {
  #setwd("C:/Research_temp3/")
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Letters/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R",winslash="\\", mustWork=TRUE) 
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 3) {
  #setwd("//tsclient/C/Research_temp3/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp3", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 4) {
  #setwd("//tsclient/C/Research_temp3/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp3", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)       
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)


###############################################################################
# FUNCTIONS;
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

#source(file=paste(function_directory,"functions_db.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_statistics.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_text_analysis.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)



import_local_edgar_file <- function(file){
  
  #file <- filepath
  
  #webpage2 <- read.table(file=file,header = FALSE, na.strings="NA",stringsAsFactors=FALSE, 
  #                       sep = "", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  #webpage2 <- read.table(file=file,header = FALSE, na.strings="",sep="\t",stringsAsFactors=FALSE)
  webpage2 <- read.table(file=file,header = FALSE, na.strings="",sep="\n",stringsAsFactors=FALSE)
  webpage2_row_count <- length(count.fields(file=file, sep = "\n"))
  
  if(nrow(webpage2)!=webpage2_row_count) { cat("ROW COUNTS NOT EQUAL","\n") }
  
  webpage_org_df <- as.data.frame(webpage2,stringsAsFactors=FALSE)
  colnames(webpage_org_df) <- "raw"
  webpage_org_df[,"raw"] <- toupper(webpage_org_df[,"raw"])
  
  webpage_df_xml_only1 <- webpage_org_df
  webpage_df_xml_only2 <- webpage_df_xml_only1[!(is.na(webpage_df_xml_only1) | webpage_df_xml_only1=="")]
  
  webpage_df_xml_only_df <- as.data.frame(webpage_df_xml_only2,stringsAsFactors=FALSE)
  colnames(webpage_df_xml_only_df) <- c("raw")
  
  return(webpage_df_xml_only_df)
  
}  

fix_edgar_tags <- function(file,entity_encoding){
  
  #file <- webpage_df_xml_only_df[,"raw"]
  #entity_encoding <- entity_encoding_trim
  
  require(gdata)
  
  #Seperate tags and values
  webpage_tags <- data.frame(tag_status_open=NA,
                             tag_status_close=NA,
                             raw=file,
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
    webpage_tags[,i] <- iconv(webpage_tags[,i], "latin1", "ASCII", sub=" ")
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
  
  #Entity Replacement
  for (i in 1:nrow(entity_encoding))
  {
    webpage_tags[,"value"] <- gsub(entity_encoding[i,"ASCII.Looks.Like"], entity_encoding[i,"Entity.Encoding"], webpage_tags[,"value"])
    
  } 
  rm(entity_encoding,i)
  
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
  
  return(webpage_tags)
  
}

find_individual_tags <- function(data,tag_raw_col,tag_short_col,tag_open_col,tag_close_col){
  
  #data <- webpage_sep
  #tag_raw_col <- "Final_tag" 
  #tag_short_col <- "tag_short"
  #tag_open_col <- "tag_status_open"
  #tag_close_col <- "tag_status_close"
  
  sep_tags1 <- data.frame(cleaned=unique(data[,tag_short_col]),
                          open_count=NA,
                          close_count=NA,
                          stringsAsFactors=FALSE)
  
  for (i in 1:nrow(sep_tags1))
  {
    #i <- 1
    #i <- 2
    
    data[,tag_open_col] <- ifelse(grepl(paste("<",sep_tags1[i,"cleaned"],">",sep=""), data[,tag_raw_col]), 1, 0)
    data[,tag_close_col] <- ifelse(grepl(paste("</",sep_tags1[i,"cleaned"],">",sep=""), data[,tag_raw_col]), 1, 0)
    
    sep_tags1[i,"open_count"] <- sum(data[,tag_open_col])
    sep_tags1[i,"close_count"] <- sum(data[,tag_close_col])
    
    data[,tag_open_col] <- NA
    data[,tag_close_col] <- NA
  } 
  rm(i)
  
  return(sep_tags1)
  
}

create_tag_index <- function(tag,data,tag_raw_col){
  
  #tag <- sep_tags2[1]
  #data <- webpage_sep
  #tag_raw_col <- "Final_tag"
  
  temp_tag <- data.frame(temp_col=data[,tag_raw_col], 
                         temp_open_flag=NA,
                         temp_open_index=NA,
                         temp_close_flag=NA,
                         temp_close_index=NA,
                         temp_diff_flag=NA,
                         temp_comb_flag=NA,
                         temp_comb_index=NA, 
                         stringsAsFactors=FALSE)
  
  temp_tag[,"temp_open_flag"] <- ifelse(grepl(paste("<",tag,">",sep=""), temp_tag[,"temp_col"]), 1, 0)
  temp_tag[,"temp_open_index"] <- apply(data.frame(temp_tag[,"temp_open_flag"]), 2, cumsum)
  
  temp_tag[,"temp_close_flag"] <- ifelse(grepl(paste("</",tag,">",sep=""), temp_tag[,"temp_col"]), 1, 0)
  temp_tag[,"temp_close_index"] <- apply(data.frame(temp_tag[,"temp_close_flag"]), 2, cumsum)
  
  temp_tag[,"temp_diff_flag"] <- (temp_tag[,"temp_open_index"]-temp_tag[,"temp_close_index"])
  
  temp_tag[,"temp_comb_flag"] <- (temp_tag[,"temp_close_flag"]+temp_tag[,"temp_diff_flag"])
  temp_tag[,"temp_comb_index"] <- ifelse(temp_tag[,"temp_comb_flag"]==1, temp_tag[,"temp_open_index"], 0)
  
  colnames(temp_tag) <- c(tag_raw_col, 
                          paste(tag,"OPEN_FLAG",sep="_"), paste(tag,"OPEN_INDEX",sep="_"),
                          paste(tag,"CLOSE_FLAG",sep="_"), paste(tag,"CLOSE_INDEX",sep="_"),
                          paste(tag,"DIFF_FLAG",sep="_"),
                          paste(tag,"COMB_FLAG",sep="_"), paste(tag,"COMB_INDEX",sep="_"))
  
  output <- temp_tag[,c(paste(tag,"COMB_INDEX",sep="_"))]
  
  return(output)
  
}

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
#   # xml_col <- "Final_tag"
#   # index_col <- filer_index_val2
#   # sub_index_col <- filer_former_company_sub_index_val2
#   # x <- filer_data_temp[filer_data_temp[,index_col]==1,]
#   
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


extract_filing_section_by_drop <- function(x,xml_col,tag_col,index_col,sub_index_col,index_flag){
  
  #xml_col="Final_tag"
  #tag_col="tag_short"

  #index_col=filer_index_val
  #sub_index_col=filer_former_company_sub_index_val
  #index_flag <- filer_index_flag
  
  #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==1,]
  #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==2,]
  #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==3,]
  #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==4,]
  
  #index_col=header_index_val
  #sub_index_col=header_info_sub_index_val
  #index_flag=header_index_flag
  
  #x <- header_data_temp[header_data_temp[,header_index_val2]==1,]
  
  require(XML)
  
  extract_filing_section_by_drop_sub <- function(x,xml_col,index_col_sub,sub_index_col_sub){

    # xml_col <- xml_col
    # index_col_sub <- index_col2
    # sub_index_col_sub <- sub_index_col2
    # x <- x
    
    xml_col_num <- which(colnames(x)==xml_col)
    index_col_num <- which(colnames(x)==index_col_sub)
    sub_index_col_num <-  unlist(lapply(sub_index_col_sub, function(y,cols){ which(cols==y) },cols=colnames(x)))
    sub_index_col_num_max <- max(sub_index_col_num)
    
    x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
    
    keep_rows <- lapply(sub_index_col_sub, function(y,data){
      
      #y <- "FILER_INDEX"
      #y <- "SERIES_AND_CLASSES_CONTRACTS_DATA_INDEX"
      #data <- x_trim
      
      if(y %in% colnames(data)) {
        
        return(which(data[,y]!=0))
        
      } else {
        
        return(NA)
        
      }
      
    },data=x_trim)
    keep_rows <- sort(unique(unlist(keep_rows)))
    drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
    
    x_trim2 <- x_trim[c(1,drop_rows,nrow(x_trim)),]
    
    temp_df <- xmlToDataFrame(x_trim2[,xml_col])
    
    return(temp_df)
  }
  index_col2 <- paste(index_col,"INDEX",sep="_")
  sub_index_col2 <- paste(sub_index_col,"INDEX",sep="_")
  
  index_val2 <- unique(x[,index_col2])
  
  flag <- (sub_index_col %in% x[,tag_col])
  
  if(any(flag)) {
    
    merge0 <- data.frame(file=file,index_col=index_val2,stringsAsFactors=FALSE)
    colnames(merge0) <- c("file",index_col2)
    
    merge1 <- extract_filing_section_by_drop_sub(x,xml_col,index_col2,sub_index_col2)
    merge <- data.frame(merge0,merge1,stringsAsFactors=FALSE)
    
    rm(merge0,merge1)
    
  } else {
    
    merge <- data.frame(file=file,index_col=index_val2,stringsAsFactors=FALSE)
    colnames(merge) <- c("file",index_col2)
    
  }
  
  rm(index_col2,sub_index_col2,index_val2,flag)
  
  return(merge)
  
}


extract_filing_section_by_keep <- function(x,xml_col,tag_col,index_col,sub_index_col,index_flag){
  
  #xml_col="Final_tag"
  #tag_col="tag_short"
  #index_col=filer_index_val
  #sub_index_col=filer_former_company_sub_index_val
  #index_flag <- filer_index_flag
  #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==1,]
  #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==2,]
  #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==3,]
  #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==4,]
  
  require(XML)
  
  extract_filing_section_by_keep_sub <- function(x,xml_col,index_col,sub_index_col){
    
    # xml_col <- "Final_tag"
    # index_col <- filer_index_val2
    # sub_index_col <- filer_former_company_sub_index_val2
    # x <- filer_data_temp[filer_data_temp[,index_col]==1,]
    
    xml_col_num <- which(colnames(x)==xml_col)
    index_col_num <- which(colnames(x)==index_col)
    sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
    sub_index_col_num_max <- max(sub_index_col_num)
    
    x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
    
    keep_rows <- lapply(sub_index_col, function(y,data){
      
      #y <- "FILER_INDEX"
      #y <- "SERIES_AND_CLASSES_CONTRACTS_DATA_INDEX"
      #data <- x_trim
      
      if(y %in% colnames(data)) {
        
        return(which(data[,y]!=0))
        
      } else {
        
        return(NA)
        
      }
      
    },data=x_trim)
    keep_rows <- sort(unique(unlist(keep_rows)))
    drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
    
    x_trim2 <- x_trim[c(1,keep_rows,nrow(x_trim)),]
    
    temp_df <- xmlToDataFrame(x_trim2[,xml_col])
    
    return(temp_df)
  }
  
  index_col2 <- paste(index_col,"INDEX",sep="_")
  sub_index_col2 <- paste(sub_index_col,"INDEX",sep="_")
  
  index_val2 <- unique(x[,index_col2])
  
  flag <- (sub_index_col %in% x[,tag_col])
  
  if(any(flag)) {
    
    merge0 <- data.frame(file=file,index_col=index_val2,stringsAsFactors=FALSE)
    colnames(merge0) <- c("file",index_col2)
    
    merge1 <- extract_filing_section_by_keep_sub(x,xml_col,index_col2,sub_index_col2)
    merge <- data.frame(merge0,merge1,stringsAsFactors=FALSE)
    
    rm(merge0,merge1)
    
  } else {
    
    merge <- data.frame(file=file,index_col=index_val2,stringsAsFactors=FALSE)
    colnames(merge) <- c("file",index_col2)
    
  }
  
  rm(index_col2,sub_index_col2,index_val2,flag)
  
  return(merge)
  
}



create_sub_index_sequence <- function(data,index_val,sub_index_val,nonindex_prefix){
  
  #data <- header_intro_merge2
  #index_val <- header_index_val2
  #sub_index_val <- header_info_sub_index_val2
  #nonindex_prefix <- ""
  
  #data <- filer_company_data
  #index_val <- paste(filer_index_val,"INDEX",sep="_")
  #sub_index_val <- paste(filer_company_data_sub_index_val,"INDEX",sep="_")
  #nonindex_prefix <- ""
  
  #data <- series_class_contract
  #index_val <- paste(series_index_val,"INDEX",sep="_")
  #sub_index_val <- paste(series_class_contract_sub_index_val,"INDEX",sep="_")
  #nonindex_prefix <- ""
  
  #data <- filer_business_address
  #index_val <- paste(filer_index_val,"INDEX",sep="_")
  #sub_index_val <- paste(filer_business_address_sub_index_val,"INDEX",sep="_")
  #nonindex_prefix <- "_BUSINESS"
  
  sub_index_val_top <- head(sub_index_val,1)
  
  require(plyr)
  
  data2 <- ddply(.data=data,.variables=index_val,.fun=function(x,sub_index_val_top){
    data2_temp <- data.frame(tempseq=NA,x,stringsAsFactors=FALSE)
    data2_temp[,"tempseq"] <- seq(1,nrow(data2_temp),1)
    colnames(data2_temp)[1] <- sub_index_val_top
    
    return(data2_temp)
    
  },sub_index_val_top=sub_index_val_top)
  data2[sapply(data2, is.factor)] <- lapply(data2[sapply(data2, is.factor)], as.character)
  
  data2_cols <- colnames(data2)
  filer_company_data_index_cols <- c("file",index_val,sub_index_val_top)
  filer_company_data_non_index_cols <- data2_cols[!(data2_cols %in% filer_company_data_index_cols)]
  data3 <- data2[,c(filer_company_data_index_cols,filer_company_data_non_index_cols)]
  
  data4 <- data3
  colnames(data4) <- c(filer_company_data_index_cols,paste(filer_company_data_non_index_cols,nonindex_prefix,sep=""))
  
  data4 <- data4[order(data4[,"file"],data4[,index_val],data4[,sub_index_val_top]),]
  row.names(data4) <- seq(nrow(data4))
  
  rm(data2,data3, filer_company_data_index_cols,filer_company_data_non_index_cols)
  
  return(data4)
}

create_comb_df <- function(list,list_pos,list_file_col,filing_info,filing_info_file_col){
  
  #list <- trim_headings
  #list_pos <- 1
  #list_file_col <- "file"
  #filing_info <- filings_trim2_short
  #filing_info_file_col <- "file_header"
  
  temp_comb <- sapply(list, "[", list_pos)
  temp_comb2 <- do.call(rbind.fill,temp_comb)
  temp_comb3 <- merge(filing_info,temp_comb2, by.x=filing_info_file_col,by.y=list_file_col,
                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables = NA)
  temp_comb3 <- temp_comb3[order(temp_comb3[,filing_info_file_col]),]
  row.names(temp_comb3) <- seq(nrow(temp_comb3))
  temp_comb3 <- temp_comb3[,c(colnames(filing_info),
                              colnames(temp_comb3[,!(colnames(temp_comb3) %in% colnames(filing_info))]))]
  
  temp_comb4 <- temp_comb3
  temp_comb4[sapply(temp_comb4, is.factor)] <- lapply(temp_comb4[sapply(temp_comb4, is.factor)], as.character)
  
  return(temp_comb4)
  
}


###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("gdata","plyr")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(installed_packages)


###############################################################################
#PARAMETERS;
###############################################################################

#If using windows, set to "\\" - if mac (or unix), set to "/";
slash <- "\\"

#First year you want index files for:

#startyear <- 1993
startyear <- 2003

#Last year you want index files for:
#endyear <- 2004
endyear <- 2013

#First qtr you want index files for (usually 1):
startqtr <- 1

#Last qtr you want index files for (usually 4):
endqtr <- 4

#downloadfolder <- "N-1"
#downloadfolder <- "DEF 14A"
#downloadfolder <- "MF_All"
#downloadfolder <- "MF_SemiAnnual_Reports"
#downloadfolder <- "MF_Annual_Reports"
downloadfolder <- "MF_Shareholder_Reports_N-CSR-A"
#downloadfolder <- "MF_Shareholder_Reports_N-CSRS-A"
#downloadfolder <- "MF_Shareholder_Reports_N-CSR"
#downloadfolder <- "MF_Shareholder_Reports_N-CSRS"

#The sub directory you are going to download filings to
headerfolder <- "header"

#The file that will contain the filings you want to download.
infile <- "filings_list_comb.csv"

yr_qtr_comb <- expand.grid(yr = seq(startyear, endyear, 1), qtr = seq(1, 4, 1))

yr_qtr_comb <- yr_qtr_comb[order(yr_qtr_comb[,"yr"],yr_qtr_comb[,"qtr"]),]
row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))

yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==startyear & yr_qtr_comb[,"qtr"] < startqtr),NA,yr_qtr_comb[,"qtr"])
yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==endyear & yr_qtr_comb[,"qtr"] > endqtr),NA,yr_qtr_comb[,"qtr"])

rm(startyear,startqtr,endyear,endqtr)

yr_qtr_comb <- yr_qtr_comb[(!is.na(yr_qtr_comb[,"qtr"])),]
row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))

yr_qtr_comb2 <- data.frame(yr_qtr_comb,
                           yr_qtr=NA,
                           stringsAsFactors=FALSE)

rm(yr_qtr_comb)

yr_qtr_comb2[,"yr_qtr"] <- paste(yr_qtr_comb2[,"yr"],
                                 yr_qtr_comb2[,"qtr"],
                                 sep="_")

#Check to see if output directory exists.  If not, create it.
create_directory(output_directory,remove=1)

#Check to see if download folder exists.  If not, create it.
download_folder_path <- paste(output_directory, downloadfolder, sep = slash, collapse = slash)  
create_directory(download_folder_path,remove=1)


###############################################################################
cat("Get list files \n")
###############################################################################

filings <- read.table(file=paste(download_folder_path,"\\",infile,sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                      sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

filings2 <- data.frame(yr_qtr=NA,
                       filings,
                       stringsAsFactors=FALSE)

rm(filings)

filings2[,"yr_qtr"] <- paste(filings2[,"yr"],
                             filings2[,"qtr"],
                             sep="_")

filings2 <- filings2[,c(c("yr","qtr","yr_qtr"),
                        colnames(filings2[,!(colnames(filings2) %in% c("yr","qtr","yr_qtr"))]))]

filings_trim <- filings2[ filings2[,"yr_qtr"] %in% yr_qtr_comb2[,"yr_qtr"],]
row.names(filings_trim) <- seq(nrow(filings_trim))

filings_trim2 <- data.frame(overall_id=NA,
                            filings_trim,
                            stringsAsFactors=FALSE)
filings_trim2[,"overall_id"] <- seq(1,nrow(filings_trim2),1)

rm(filings_trim)


###############################################################################
cat("Import HTML entities \n")
###############################################################################

#Encode HTML entities
#entity_encoding <- read.csv(file=paste(output_directory,"Entity_encoding.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
entity_encoding <- read.table(file=paste(output_directory,"Entity_encoding.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                              sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

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


###############################################################################
cat("Get header information \n")
###############################################################################

filings_header_info <- dlply(.data=filings_trim2, .variables=c("yr"), 
                             .fun = function(x, path_ouput,sub_folder,entity_encoding){
                               
                               #x <- filings_trim2[(filings_trim2[,"yr"]==2003),]
                               #x <- filings_trim2[(filings_trim2[,"yr"]==2013),]
                               #path_output <- paste(output_directory,downloadfolder,sep=slash)
                               #subfolder <- headerfolder
                               #entity_encoding <- entity_encoding_trim
                               
                               filings_trim2_short <- x[,!(colnames(x) %in% c("file_txt","file_index_htm"))]
                               
                               yr <-  unique(x[,"yr"])
                               
                               cat("\n",yr,"\n")
                               
                               #Check to see if yr folder exists.  If not, create it.
                               yr_folder_path <- paste(path_output, yr, sep = "\\", collapse = "\\")   
                               create_directory(yr_folder_path,remove=1)
                               
                               sub_folder_path <- paste(yr_folder_path, subfolder, sep = "\\", collapse = "\\")   
                               create_directory(sub_folder_path,remove=1)
                               
                               #Get name of downloaded files
                               downloaded_files <- data.frame(file=list.files(sub_folder_path),stringsAsFactors=FALSE)
                               downloaded_files2 <- ddply(.data=downloaded_files, .variables=c("file"), .fun = function(x,folder){
                                 
                                 filepath <- paste(folder,x,sep="\\")
                                 output <- data.frame(filepath=filepath,file.info(filepath),stringsAsFactors=FALSE)
                                 
                               }, folder=sub_folder_path, 
                               .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL, .id = NA)
                               
                               rm(downloaded_files)
                               
                               downloaded_files2 <- downloaded_files2[order(downloaded_files2[,"filepath"]),]
                               row.names(downloaded_files2) <- seq(nrow(downloaded_files2))
                               
                               downloaded_files3 <- data.frame(yr_id=NA,
                                                               downloaded_files2,
                                                               stringsAsFactors=FALSE)
                               downloaded_files3[,"yr_id"] <- seq(1,nrow(downloaded_files3),1)
                               
                               rm(downloaded_files2)
                               
                               trim_headings <- dlply(.data=downloaded_files3, .variables=c("yr_id"), 
                                                      .fun = function(y,entity_encoding){
                                                        
                                                        #y <- downloaded_files3[(downloaded_files3[,"file"]=="0000842939-03-000055.hdr.sgml"),]
                                                        #y <- downloaded_files3[(downloaded_files3[,"file"]=="0000078713-13-000036.hdr.sgml"),]
                                                        #y <- downloaded_files3[(downloaded_files3[,"file"]=="0001193125-13-456867.hdr.sgml"),]
                                                        #y <- downloaded_files3[(downloaded_files3[,"file"]=="0001571049-13-000837.hdr.sgml"),]
                                                        
                                                        #entity_encoding <- entity_encoding
                                                        
                                                        file <- unique(y[,"file"])
                                                        filepath <- unique(y[,"filepath"])
                                                        
                                                        cat("\n",file,"\n")
                                                        
                                                        webpage_df_xml_only_df <- import_local_edgar_file(filepath)
                                                        webpage_df_xml_only_df[,"raw"] <- gsub("&NBSP;"," ",webpage_df_xml_only_df[,"raw"])
                                                        
                                                        webpage_tags <- fix_edgar_tags(webpage_df_xml_only_df[,"raw"],entity_encoding)
                                                        
                                                        rm(webpage_df_xml_only_df)
                                                        
                                                        webpage_sep <- webpage_tags
                                                        webpage_sep[1,"Final_tag"] <- "<SEC_HEADER>"
                                                        
                                                        rm(webpage_tags)
                                                        
                                                        webpage_sep <- data.frame(webpage_sep,tag_status_open=NA,tag_status_close=NA,stringsAsFactors=FALSE)
                                                        
                                                        #Find all possible tags
                                                        sep_tags1 <- find_individual_tags(data=webpage_sep,tag_raw_col="Final_tag",tag_short_col="tag_short",
                                                                                          tag_open_col="tag_status_open",tag_close_col="tag_status_close")
                                                        sep_tags2 <- sep_tags1[,"cleaned"]
                                                        
                                                        index_temp <- llply(.data=sep_tags2, create_tag_index,data=webpage_sep, tag_raw_col="Final_tag",
                                                                            .progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
                                                        index <- do.call(cbind, index_temp)
                                                        index <- as.data.frame(index,stringsAsFactors=FALSE)
                                                        colnames(index) <- paste(sep_tags2,"INDEX",sep="_")
                                                        rm(index_temp)
                                                        
                                                        webpage_sep_index <- data.frame(file=file, Final_tag=webpage_sep[,"Final_tag"],tag_short=webpage_sep[,"tag_short"],index,stringsAsFactors=FALSE)
                                                        rm(index)
                                                        
                                                        #Header Section - Setup
                                                        header_index_val <- "SEC_HEADER"
                                                        header_index_val2 <- paste(header_index_val,"INDEX",sep="_")
                                                        header_index_flag <- (header_index_val %in% sep_tags2)
                                                        
                                                        #Header Section - Intro
                                                        header_info_sub_index_val <- c("FILER","SERIES_AND_CLASSES_CONTRACTS_DATA")
                                                        header_info_sub_index_val2 <- paste(header_info_sub_index_val,"INDEX",sep="_")

                                                        if(header_index_flag) {
                                                          
                                                          header_data_temp <- webpage_sep_index[!(webpage_sep_index[,header_index_val2] %in% c(0)),]
                                                          row.names(header_data_temp) <- seq(nrow(header_data_temp))
                                                          
                                                          header_intro_merge1 <- dlply(.data=header_data_temp, .variables=c("file",header_index_val2), extract_filing_section_by_drop,
                                                                                             xml_col="Final_tag",tag_col="tag_short",index_col=header_index_val,
                                                                                             sub_index_col=header_info_sub_index_val, index_flag=header_index_flag)
                                                          header_intro_merge2 <- do.call(rbind.fill,header_intro_merge1)
                                                          
                                                          header_intro_merge <-  create_sub_index_sequence(data=header_intro_merge2,index_val=header_index_val2,
                                                                                                                 sub_index_val=header_info_sub_index_val2,nonindex_prefix="")
                                                          
                                                          rm(header_data_temp,header_intro_merge1,header_intro_merge2)
                                                          
                                                        } else {
                                                          
                                                          header_intro_merge <- data.frame(file=file,index_col=NA,sub_index_col=NA,stringsAsFactors=FALSE)
                                                          colnames(header_intro_merge) <- c("file",header_index_val2,head(header_info_sub_index_val2,1))
                                                          
                                                        }
                                                        rm(header_info_sub_index_val,header_info_sub_index_val2)
                                                        
#                                                         
#                                                         #Header Section - Setup
#                                                         header_index_val <- "SEC_HEADER"
#                                                         header_index_val2 <- paste(header_index_val,"INDEX",sep="_")
#                                                         header_data_temp <- webpage_sep_index[!(webpage_sep_index[,header_index_val2] %in% c(0)),]
#                                                         row.names(header_data_temp) <- seq(nrow(header_data_temp))
#                                                         
#                                                         #Header Section - Intro
#                                                         header_info_sub_index_val <- c("FILER","SERIES_AND_CLASSES_CONTRACTS_DATA")
#                                                         header_info_sub_index_val2 <- paste(header_info_sub_index_val,"INDEX",sep="_")
#                                                         header_intro <- ddply(.data=header_data_temp, .variables=c("file",header_index_val2), extract_filing_section_by_drop,
#                                                                               xml_col="Final_tag",tag_col="tag_short",index_col=header_index_val,
#                                                                               sub_index_col=header_info_sub_index_val2, index_flag=filer_index_flag)
#                                                         
#                                                         
                                                        
                                                        #Filer Section - Setup
                                                        filer_index_val <- "FILER"
                                                        filer_index_val2 <- paste(filer_index_val,"INDEX",sep="_")
                                                        filer_index_flag <- (filer_index_val %in% sep_tags2)
                                                        
                                                        #Filer Section - Company Data
                                                        filer_company_data_sub_index_val <- "COMPANY_DATA"
                                                        filer_company_data_sub_index_val2 <- paste(filer_company_data_sub_index_val,"INDEX",sep="_")
                                                        
                                                        if(filer_index_flag) {
                                                          
                                                          filer_data_temp <- webpage_sep_index[!(webpage_sep_index[,filer_index_val2] %in% c(0)),]
                                                          row.names(filer_data_temp) <- seq(nrow(filer_data_temp))
                                                          
                                                          filer_company_data_merge1 <- dlply(.data=filer_data_temp, .variables=c("file",filer_index_val2), extract_filing_section_by_keep,
                                                                                             xml_col="Final_tag",tag_col="tag_short",index_col=filer_index_val,
                                                                                             sub_index_col=filer_company_data_sub_index_val, index_flag=filer_index_flag)
                                                          filer_company_data_merge2 <- do.call(rbind.fill,filer_company_data_merge1)
                                                          
                                                          filer_company_data_merge <-  create_sub_index_sequence(data=filer_company_data_merge2,index_val=filer_index_val2,
                                                                                                                 sub_index_val=filer_company_data_sub_index_val2,nonindex_prefix="")
                                                          
                                                          rm(filer_data_temp,filer_company_data_merge1,filer_company_data_merge2)
                                                          
                                                        } else {
                                                          
                                                          filer_company_data_merge <- data.frame(file=file,index_col=NA,sub_index_col=NA,stringsAsFactors=FALSE)
                                                          colnames(filer_company_data_merge) <- c("file",filer_index_val2,head(filer_company_data_sub_index_val2,1))
                                                          
                                                        }
                                                        rm(filer_company_data_sub_index_val,filer_company_data_sub_index_val2)
                                                        
                                                        
                                                        #Filer Section - Filing Values
                                                        filer_filing_values_sub_index_val <- "FILING_VALUES"
                                                        filer_filing_values_sub_index_val2 <- paste(filer_filing_values_sub_index_val,"INDEX",sep="_")
                                                        
                                                        if(filer_index_flag) {
                                                          
                                                          filer_data_temp <- webpage_sep_index[!(webpage_sep_index[,filer_index_val2] %in% c(0)),]
                                                          row.names(filer_data_temp) <- seq(nrow(filer_data_temp))
                                                          
                                                          filer_filing_values_merge1 <- dlply(.data=filer_data_temp, .variables=c("file",filer_index_val2), extract_filing_section_by_keep,
                                                                                              xml_col="Final_tag",tag_col="tag_short",index_col=filer_index_val,
                                                                                              sub_index_col=filer_filing_values_sub_index_val, index_flag=filer_index_flag)
                                                          filer_filing_values_merge2 <- do.call(rbind.fill,filer_filing_values_merge1)
                                                          
                                                          filer_filing_values_merge <-  create_sub_index_sequence(data=filer_filing_values_merge2,index_val=filer_index_val2,
                                                                                                                  sub_index_val=filer_filing_values_sub_index_val2,nonindex_prefix="")
                                                          
                                                          rm(filer_data_temp,filer_filing_values_merge1,filer_filing_values_merge2)
                                                          
                                                        } else {
                                                          
                                                          filer_filing_values_merge <- data.frame(file=file,index_col=NA,sub_index_col=NA,stringsAsFactors=FALSE)
                                                          colnames(filer_filing_values_merge) <- c("file",filer_index_val2,head(filer_filing_values_sub_index_val2,1))
                                                          
                                                        }
                                                        rm(filer_filing_values_sub_index_val,filer_filing_values_sub_index_val2)
                                                        
                                                        
                                                        #Filer Section - Business Address
                                                        filer_business_address_sub_index_val <- "BUSINESS_ADDRESS"
                                                        filer_business_address_sub_index_val2 <- paste(filer_business_address_sub_index_val,"INDEX",sep="_")
                                                        
                                                        if(filer_index_flag) {
                                                          
                                                          filer_data_temp <- webpage_sep_index[!(webpage_sep_index[,filer_index_val2] %in% c(0)),]
                                                          row.names(filer_data_temp) <- seq(nrow(filer_data_temp))
                                                          
                                                          filer_business_address_merge1 <- dlply(.data=filer_data_temp, .variables=c("file",filer_index_val2), extract_filing_section_by_keep,
                                                                                                 xml_col="Final_tag",tag_col="tag_short",index_col=filer_index_val,
                                                                                                 sub_index_col=filer_business_address_sub_index_val, index_flag=filer_index_flag)
                                                          filer_business_address_merge2 <- do.call(rbind.fill,filer_business_address_merge1)
                                                          
                                                          filer_business_address_merge <-  create_sub_index_sequence(data=filer_business_address_merge2,index_val=filer_index_val2,
                                                                                                                     sub_index_val=filer_business_address_sub_index_val2,nonindex_prefix="_BUSINESS")
                                                          
                                                          rm(filer_data_temp,filer_business_address_merge1,filer_business_address_merge2)
                                                          
                                                        } else {
                                                          
                                                          filer_business_address_merge <- data.frame(file=file,index_col=NA,sub_index_col=NA,stringsAsFactors=FALSE)
                                                          colnames(filer_business_address_merge) <- c("file",filer_index_val2,head(filer_business_address_sub_index_val2,1))
                                                          
                                                        }
                                                        rm(filer_business_address_sub_index_val,filer_business_address_sub_index_val2)
                                                        
                                                        
                                                        #Filer Section - Mail Address
                                                        filer_mail_address_sub_index_val <- "MAIL_ADDRESS"
                                                        filer_mail_address_sub_index_val2 <- paste(filer_mail_address_sub_index_val,"INDEX",sep="_")
                                                        
                                                        if(filer_index_flag) {
                                                          
                                                          filer_data_temp <- webpage_sep_index[!(webpage_sep_index[,filer_index_val2] %in% c(0)),]
                                                          row.names(filer_data_temp) <- seq(nrow(filer_data_temp))
                                                          
                                                          filer_mail_address_merge1 <- dlply(.data=filer_data_temp, .variables=c("file",filer_index_val2), extract_filing_section_by_keep,
                                                                                             xml_col="Final_tag",tag_col="tag_short",index_col=filer_index_val,
                                                                                             sub_index_col=filer_mail_address_sub_index_val, index_flag=filer_index_flag)
                                                          filer_mail_address_merge2 <- do.call(rbind.fill,filer_mail_address_merge1)
                                                          
                                                          filer_mail_address_merge <-  create_sub_index_sequence(data=filer_mail_address_merge2,index_val=filer_index_val2,
                                                                                                                 sub_index_val=filer_mail_address_sub_index_val2,nonindex_prefix="_MAIL")
                                                          
                                                          rm(filer_data_temp,filer_mail_address_merge1,filer_mail_address_merge2)
                                                          
                                                        } else {
                                                          
                                                          filer_mail_address_merge <- data.frame(file=file,index_col=NA,sub_index_col=NA,stringsAsFactors=FALSE)
                                                          colnames(filer_mail_address_merge) <- c("file",filer_index_val2,head(filer_mail_address_sub_index_val2,1))
                                                          
                                                        }
                                                        rm(filer_mail_address_sub_index_val,filer_mail_address_sub_index_val2)
                                                        
                                                        
                                                        #Filer Section - Former Company
                                                        filer_former_company_sub_index_val <- "FORMER_COMPANY"
                                                        filer_former_company_sub_index_val2 <- paste(filer_former_company_sub_index_val,"INDEX",sep="_")
                                                        
                                                        if(filer_index_flag) {
                                                          
                                                          filer_data_temp <- webpage_sep_index[!(webpage_sep_index[,filer_index_val2] %in% c(0)),]
                                                          row.names(filer_data_temp) <- seq(nrow(filer_data_temp))
                                                          
                                                          filer_former_company_merge1 <- dlply(.data=filer_data_temp, .variables=c("file",filer_index_val2), extract_filing_section_by_keep,
                                                                                               xml_col="Final_tag",tag_col="tag_short",index_col=filer_index_val,
                                                                                               sub_index_col=filer_former_company_sub_index_val, index_flag=filer_index_flag)
                                                          filer_former_company_merge2 <- do.call(rbind.fill,filer_former_company_merge1)
                                                          
                                                          filer_former_company_merge <-  create_sub_index_sequence(data=filer_former_company_merge2,index_val=filer_index_val2,
                                                                                                                   sub_index_val=filer_former_company_sub_index_val2,nonindex_prefix="")
                                                          
                                                          rm(filer_data_temp,filer_former_company_merge1,filer_former_company_merge2)
                                                          
                                                        } else {
                                                          
                                                          filer_former_company_merge <- data.frame(file=file,index_col=NA,sub_index_col=NA,stringsAsFactors=FALSE)
                                                          colnames(filer_former_company_merge) <- c("file",filer_index_val2,head(filer_former_company_sub_index_val2,1))
                                                          
                                                        }
                                                        rm(filer_former_company_sub_index_val,filer_former_company_sub_index_val2)
                                                        
                                                        
                                                        rm(filer_index_val,filer_index_val2)
                                                        
                                                        
                                                        #Series Section - Setup
                                                        series_index_val <- "SERIES"
                                                        series_index_val2 <- paste(series_index_val,"INDEX",sep="_")
                                                        series_index_flag <- (series_index_val %in% sep_tags2)
                                                        
                                                        #Series Section - Other
                                                        series_other_sub_index_val <- "CLASS_CONTRACT"
                                                        series_other_sub_index_val2 <- paste(series_other_sub_index_val,"INDEX",sep="_")
                                                        
                                                        if(series_index_flag) {
                                                          
                                                          series_data_temp <- webpage_sep_index[!(webpage_sep_index[,series_index_val2] %in% c(0)),]
                                                          row.names(series_data_temp) <- seq(nrow(series_data_temp))
                                                          
                                                          series_other_merge1 <- dlply(.data=series_data_temp, .variables=c("file",series_index_val2), extract_filing_section_by_drop,
                                                                                       xml_col="Final_tag",tag_col="tag_short",index_col=series_index_val,
                                                                                       sub_index_col=series_other_sub_index_val, index_flag=series_index_flag)
                                                          series_other_merge2 <- do.call(rbind.fill,series_other_merge1)
                                                          
                                                          series_other_merge <-  create_sub_index_sequence(data=series_other_merge2,index_val=series_index_val2,
                                                                                                           sub_index_val=series_other_sub_index_val2,nonindex_prefix="")
                                                          
                                                          rm(series_data_temp,series_other_merge1,series_other_merge2)
                                                          
                                                        } else {
                                                          
                                                          series_other_merge <- data.frame(file=file,index_col=NA,sub_index_col=NA,stringsAsFactors=FALSE)
                                                          colnames(series_other_merge) <- c("file",series_index_val2,head(series_other_sub_index_val2,1))
                                                          
                                                        }
                                                        rm(series_other_sub_index_val,series_other_sub_index_val2)
                                                        
                                                        #Series Section - Class Contract
                                                        series_class_contract_sub_index_val <- "CLASS_CONTRACT"
                                                        series_class_contract_sub_index_val2 <- paste(series_class_contract_sub_index_val,"INDEX",sep="_")
                                                        
                                                        if(series_index_flag) {
                                                          
                                                          series_data_temp <- webpage_sep_index[!(webpage_sep_index[,series_index_val2] %in% c(0)),]
                                                          row.names(series_data_temp) <- seq(nrow(series_data_temp))
                                                          
                                                          series_class_contract_merge1 <- dlply(.data=series_data_temp, .variables=c("file",series_index_val2), extract_filing_section_by_keep,
                                                                                                xml_col="Final_tag",tag_col="tag_short",index_col=series_index_val,
                                                                                                sub_index_col=series_class_contract_sub_index_val, index_flag=series_index_flag)
                                                          series_class_contract_merge2 <- do.call(rbind.fill,series_class_contract_merge1)
                                                          
                                                          series_class_contract_merge <-  create_sub_index_sequence(data=series_class_contract_merge2,index_val=series_index_val2,
                                                                                                                    sub_index_val=series_class_contract_sub_index_val2,nonindex_prefix="")
                                                          
                                                          rm(series_data_temp,series_class_contract_merge1,series_class_contract_merge2)
                                                          
                                                        } else {
                                                          
                                                          series_class_contract_merge <- data.frame(file=file,index_col=NA,sub_index_col=NA,stringsAsFactors=FALSE)
                                                          colnames(series_class_contract_merge) <- c("file",series_index_val2,head(series_class_contract_sub_index_val2,1))
                                                          
                                                        }
                                                        rm(series_class_contract_sub_index_val,series_class_contract_sub_index_val2)
                                                        
                                                        rm(series_index_val,series_index_val2)
                                                        
                                                        df_comb_list <- list(header_intro_merge,
                                                                             filer_company_data_merge,filer_filing_values_merge,filer_business_address_merge, filer_mail_address_merge,filer_former_company_merge,
                                                                             series_other_merge,series_class_contract_merge)
                                                        
                                                        rm(header_intro_merge)
                                                        rm(filer_company_data_merge,filer_filing_values_merge,filer_business_address_merge, filer_mail_address_merge,filer_former_company_merge)
                                                        rm(series_other_merge,series_class_contract_merge)
                                                        
                                                        rm(filer_index_flag,series_index_flag)
                                                        
                                                        rm(file,filepath)
                                                        rm(sep_tags1,sep_tags2,webpage_sep,webpage_sep_index)
                                                        
                                                        return(df_comb_list)
                                                        
                                                      },
                                                      entity_encoding=entity_encoding,
                                                      .progress = "text",.inform = TRUE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
                               
                               header_intro_comb <- create_comb_df(list=trim_headings,list_pos=1,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               filer_company_data_comb <- create_comb_df(list=trim_headings,list_pos=2,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               filer_filing_values_comb <- create_comb_df(list=trim_headings,list_pos=3,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               filer_business_address_comb <- create_comb_df(list=trim_headings,list_pos=4,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               filer_mail_address_comb <- create_comb_df(list=trim_headings,list_pos=5,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               filer_former_company_comb <- create_comb_df(list=trim_headings,list_pos=6,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               series_other_comb <- create_comb_df(list=trim_headings,list_pos=7,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               series_class_contract_comb <- create_comb_df(list=trim_headings,list_pos=8,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               
                               df_comb_list_all <- list(header_intro_comb,
                                                        filer_company_data_comb,filer_filing_values_comb,filer_business_address_comb, filer_mail_address_comb,filer_former_company_comb,
                                                        series_other_comb,series_class_contract_comb)
                               
                               rm(header_intro_comb)
                               rm(filer_company_data_comb,filer_filing_values_comb,filer_business_address_comb,filer_mail_address_comb,filer_former_company_comb)
                               rm(series_other_comb,series_class_contract_comb)
                               
                               rm(entity_encoding,yr,yr_folder_path,sub_folder_path,downloaded_files3)
                               rm(trim_headings)
                               
                               return(df_comb_list_all)
                               
                             },
                             path_output=paste(output_directory,downloadfolder,sep=slash),
                             subfolder=headerfolder,entity_encoding=entity_encoding_trim,
                             .progress = "text",.inform = TRUE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)


###############################################################################
cat("Output Combined Files \n")
###############################################################################

write.table(filings_comb,file=paste(download_folder_path,"\\",outfile,sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)