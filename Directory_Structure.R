library(chron)
library(DataCombine)
library(gdata)
library(plyr)
library(RCurl)
library(RJDBC)
library(XML)

options(scipen=999)

convb <- function(x){
  ptn <- "(\\d*(.\\d+)*)(.*)"
  num  <- as.numeric(sub(ptn, "\\1", x))
  unit <- sub(ptn, "\\3", x)             
  unit[unit==""] <- "1" 
  
  mult <- c("1"=1, "K"=1024, "M"=1024^2, "G"=1024^3)
  
  return(num * unname(mult[unit]))
}

directory_listing <- function(http_address){
  
  #webpage <- getURL(http_address, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  webpage <- getURLContent(http_address)[[1]]
  
  webpage_by_lines <- readLines(tc <- textConnection(webpage)); close(tc)
  
  webpage_df_html_only <- webpage_by_lines[2:length(webpage_by_lines)]
  webpage_df_html_only2 <- webpage_df_html_only[!(is.na(webpage_df_html_only) | webpage_df_html_only=="")]
  
  webpage_df <- as.data.frame(webpage_df_html_only2,stringsAsFactors=FALSE)
  
  #output_directory <- htmlParse(webpage_df_html_only2,isURL=FALSE, options = HUGE)
  #output_directory <- htmlTreeParse(webpage, useInternalNodes = TRUE, options = HUGE)
  #output_directory <- htmlTreeParse(webpage, useInternalNodes = FALSE, options = HUGE) 
  #output_directory <- xmlTreeParse(webpage,useInternalNodes = FALSE, options = HUGE)
  output_directory <- readHTMLTable(http_address)
  output_directory <- as.data.frame(do.call(rbind, output_directory),stringsAsFactors=FALSE)
  output_directory <- data.frame(lapply(output_directory, as.character), stringsAsFactors=FALSE)
  
  return(output_directory)
}

directory_trimming <- function(directory_df){
  
  #directory_df <- temp_directory
  
  for(i in 1:ncol(directory_df))
  {
    directory_df[,i] <- iconv(directory_df[,i], "latin1", "ASCII", sub="")
  }
  
  for(i in which(sapply(directory_df,class)=="character"))
  {
    directory_df[[i]] = trim(directory_df[[i]])
  }
  for (i in 1:ncol(directory_df))
  {
    directory_df[,i] <- unknownToNA(directory_df[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
    directory_df[,i] <- ifelse(is.na(directory_df[,i]),NA, directory_df[,i])
  } 
  
  #Remove all NA cols
  directory_df <- directory_df[,colSums(is.na(directory_df[1:nrow(directory_df),]))<nrow(directory_df)]
  
  #Remove all NA rows
  directory_df <- directory_df[rowSums(is.na(directory_df[,1:ncol(directory_df)]))<ncol(directory_df),]

  #Remove parent directory row
  directory_df <- directory_df[!(directory_df[,"Name"]=="Parent Directory"),]
  
  #Remove NA names row
  directory_df <- directory_df[!(is.na(directory_df[,"Name"])),]
  
  #Reorder row numbers
  row.names(directory_df) <- seq(nrow(directory_df))
  
  for(i in which(sapply(directory_df,class)=="character"))
  {
    directory_df[[i]] = trim(directory_df[[i]])
  }
  for (i in 1:ncol(directory_df))
  {
    #i <- 1
    #i <- 2
    directory_df[,i] <- unknownToNA(directory_df[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  } 
  for(i in which(sapply(directory_df,class)=="character"))
  {
    #i <- 1
    directory_df[[i]] <- ifelse(is.na(directory_df[[i]]),NA, directory_df[[i]])
  }
  
  return(directory_df)
}

directory_cleaning <- function(x){
  
  #Add sub addresses
  x[,"Sub_Link_HTTP"] <- ifelse((is.na(x[,"Directory_Address_HTTP"]) & is.na(x[,"Name"])),NA,paste(x[,"Directory_Address_HTTP"],x[,"Name"],sep=""))
  x[,"Sub_Link_FTP"] <- ifelse((is.na(x[,"Directory_Address_FTP"]) & is.na(x[,"Name"])),NA,paste(x[,"Directory_Address_FTP"],x[,"Name"],sep=""))
  
  #Convert Last_Modified to Date
  x[,"Last_Modified"] <- as.POSIXct(x[,"Last_Modified"], format="%d-%b-%Y %H:%M")
  
  #Standardize size to bytes
  x[,"Size"] <- ifelse(x[,"Size"]=="-",NA, x[,"Size"])
  x[,"Size"] <- convb(x[,"Size"])
  
  #Add directory description
  #x[,"Description"] <- ifelse(is.na(x[,"Description"]),"Directory", x[,"Description"])
  x[,"Description"] <- ifelse((substr(x[,"Name"], nchar(x[,"Name"]), nchar(x[,"Name"]))=="/" & is.na(x[,"Size"])),"Directory", x[,"Description"])
  
  x[,"CIK_no_pad"] <- as.numeric(x[,"CIK_no_pad"])
  
  #Create Accession Number
  #x <- directory_Accession_num_L1(x)
  
  #Remove ALL NA rows
  x <- x[rowSums(is.na(x[,1:ncol(x)]))<ncol(x),]
  
  #Reorder row numbers
  row.names(x) <- seq(nrow(x))
  
  return(x)
  
}

run_directory <- function(input_ds){
  
  directory_temp <- adply(input_ds,1,.fun=function(x,linkcol="Sub_Link_HTTP"){
    
    return(directory_listing(x[,linkcol]))
    
  }
  
  ,.progress = "text", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
  
  #Trim directory
  directory_temp <- directory_trimming(directory_temp)
  
  #Find first row that is empty in final dataset
  #empty_rows <- which(rowSums(is.na(output_ds[,1:ncol(output_ds)]))==ncol(output_ds))
  
  #Add temp directory columns to final dataset
  #directory[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_temp)-1),
  #                      c("Name","Last_Modified","Size","Description")] <- directory_temp[,c("Name","Last.modified","Size","Description")]
  #output_ds[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_temp)-1),"CIK"] <- directory_temp[,"CIK"]
  #output_ds[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_temp)-1),"CIK_no_pad"] <- directory_temp[,"CIK_no_pad"]
  #output_ds[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_temp)-1),"Directory_Address_HTTP"] <- directory_temp[,"Sub_Link_HTTP"]
  #output_ds[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_temp)-1),"Directory_Address_FTP"] <- directory_temp[,"Sub_Link_FTP"]
  #output_ds[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_temp)-1),"Name"] <- directory_temp[,"Name"]
  #output_ds[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_temp)-1),"Last_Modified"] <- directory_temp[,"Last.modified"]
  #output_ds[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_temp)-1),"Size"] <- directory_temp[,"Size"]
  #output_ds[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_temp)-1),"Description"] <- directory_temp[,"Description"]
  
  output_ds <- data.frame(CIK=directory_temp[,"CIK"],
                          CIK_no_pad <- directory_temp[,"CIK_no_pad"],
                          Directory_Address_HTTP <- directory_temp[,"Sub_Link_HTTP"],                          
                          Directory_Address_FTP <- directory_temp[,"Sub_Link_FTP"],                          
                          Accession_Num <- NA,                          
                          Name <- directory_temp[,"Name"],
                          Last_Modified <- directory_temp[,"Last.modified"],
                          Size <- directory_temp[,"Size"],
                          Description <- directory_temp[,"Description"],
                          Sub_Link_HTTP <- NA,
                          Sub_Link_FTP <- NA,
                          stringsAsFactors=FALSE)
  colnames(output_ds) <- c("CIK","CIK_no_pad","Directory_Address_HTTP","Directory_Address_FTP","Accession_Num",
                           "Name","Last_Modified","Size","Description","Sub_Link_HTTP","Sub_Link_FTP")
  
  #Clean directory
  output_ds <- directory_cleaning(output_ds)
  
  return(output_ds)
}


directory_Accession_num_L1 <- function(directory_df){
  
  #Accession string positions and lengths
  Accession_start1 <- 0
  Accession_length1 <- 10
  Accession_start2 <- 11
  Accession_length2 <- 1
  Accession_start3 <- 13
  Accession_length3 <- 5
  
  #Create Accession number
  #directory_df[,"Accession_num"] <- paste(substring(gsub(pattern="-", replacement="", directory_df[,"Name"]), Accession_start1, Accession_start1+Accession_length1),
  #                                          substring(gsub(pattern="-", replacement="", directory_df[,"Name"]), Accession_start2, Accession_start2+Accession_length2),
  #                                          substring(gsub(pattern="-", replacement="", directory_df[,"Name"]), Accession_start3, Accession_start3+Accession_length3),
  #                                          sep="-")
  

  
  #return(directory_df)
}

directory_Accession_num_L2 <- function(directory_df){
  
  #Accession string positions and lengths
  Accession_start1 <- 0
  Accession_length1 <- 10
  Accession_start2 <- 11
  Accession_length2 <- 1
  Accession_start3 <- 13
  Accession_length3 <- 5
  
  #Create Accession number
  #directory_df[,"Accession_num"] <- paste(substring(gsub(pattern="-", replacement="", directory_df[,"Name"]), Accession_start1, Accession_start1+Accession_length1),
  #                                          substring(gsub(pattern="-", replacement="", directory_df[,"Name"]), Accession_start2, Accession_start2+Accession_length2),
  #                                          substring(gsub(pattern="-", replacement="", directory_df[,"Name"]), Accession_start3, Accession_start3+Accession_length3),
  #                                          sep="-")
  
  #return(directory_df)
}


###############################################################################
cat("SECTION: PREALLOCATE DATA", "\n")
############################################################################### 

directory_level1_cols <- c("CIK","CIK_no_pad","Directory_Address_HTTP","Directory_Address_FTP","Accession_Num",
                           "Name","Last_Modified","Size","Description","Sub_Link_HTTP","Sub_Link_FTP")
directory_level1 <- data.frame(matrix(NA, nrow = 5000, ncol = 11,dimnames = list(NULL,directory_level1_cols)),
                               stringsAsFactors=FALSE)

directory_level2_cols <- c("CIK","CIK_no_pad","Directory_Address_HTTP","Directory_Address_FTP","Accession_Num",
                           "Name","Last_Modified","Size","Description","Sub_Link_HTTP","Sub_Link_FTP")
directory_level2 <- data.frame(matrix(NA, nrow = 20000, ncol = 11,dimnames = list(NULL,directory_level2_cols)),
                               stringsAsFactors=FALSE)

###############################################################################
cat("SECTION: IMPORT CIK CATALOG", "\n")
###############################################################################

output_directory <- normalizePath("C:/Research_temp3/",winslash="\\", mustWork=TRUE)

cik_catalog_temp <- read.csv(file=paste(output_directory,"cik_list.csv",sep="/"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
cik_catalog_temp <- as.data.frame(cik_catalog_temp,stringsAsFactors=FALSE)
colnames(cik_catalog_temp) <- "CIK_no_pad"

#good_rows <- adply(cik_catalog_temp,1,.fun=function(x,col="CIK_no_pad"){
#  
#  tryCatch(x[,col] <- as.integer(x[,col]), 
#           warning=function(w){ cat( "\n", "problem rows:", row.names(x), "\n") } )
#
#  }
#  ,.progress = "text", .inform = FALSE,.parallel = FALSE, .paropts = NULL)

cik_catalog_temp[,"CIK_no_pad"] <- as.numeric(cik_catalog_temp[,"CIK_no_pad"])
cik_catalog_temp[,"CIK_no_pad"] <- round(cik_catalog_temp[,"CIK_no_pad"], digits = 0)

for (i in 1:ncol(cik_catalog_temp))
{
  cik_catalog_temp[,i] <- unknownToNA(cik_catalog_temp[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                              NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                              NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  cik_catalog_temp[,i] <- ifelse(is.na(cik_catalog_temp[,i]),NA, cik_catalog_temp[,i])
} 

cik_catalog <- data.frame(CIK=NA,
                          CIK_no_pad=cik_catalog_temp,
                          Description="Directory",
                          Sub_Link_HTTP=NA,
                          Sub_Link_FTP=NA,
                          stringsAsFactors=FALSE)

#Pad CIK
cik_catalog[,"CIK"] <- format(cik_catalog[,"CIK_no_pad"], trim=TRUE, digits = 0, scientific = 999)
cik_catalog[,"CIK"] <- sprintf("%010s",cik_catalog[,"CIK"])
cik_catalog[,"CIK"] <- gsub(" ", "0", cik_catalog[,"CIK"])

#Get addresses
cik_catalog[,"Sub_Link_HTTP"] <- paste("http://www.sec.gov/Archives/edgar/data/",cik_catalog[,"CIK_no_pad"],"/",sep="") 
cik_catalog[,"Sub_Link_FTP"] <- paste("ftp://ftp.sec.gov/edgar/data/",cik_catalog[,"CIK_no_pad"],"/",sep="") 

rm(cik_catalog_temp)


###############################################################################
cat("SECTION: LEVEL 1 DIRECTORY", "\n")
###############################################################################

#TEMP!!
cik_catalog <- cik_catalog[cik_catalog[,"CIK"] %in% c("0000876603","0001414040"),] 
row.names(cik_catalog) <- seq(nrow(cik_catalog))
directory_level1_test <- directory_level1
directory_level1_test2 <- directory_level1
#END TEMP!

# for(i in which(cik_catalog[,"Description"]=="Directory"))
# {
#   #i <- tail(head(which(cik_catalog[,"Description"]=="Directory"),1),1)
#   #i <- tail(head(which(cik_catalog[,"Description"]=="Directory"),2),1)
#   
#   #Get Directory
#   temp_directory <- directory_listing(cik_catalog[i,"Sub_Link_HTTP"])
#   
#   #Clean Directory
#   temp_directory <- directory_cleaning(temp_directory)
#   
#   #Find first row that is empty in final dataset
#   empty_rows <- which(rowSums(is.na(directory_level1[,1:ncol(directory_level1)]))==ncol(directory_level1))
#   
#   #Add CIK, CIK_no_pad, directory_address_http, & directory_address_ftp to final dataset
#   directory_level1[head(empty_rows,1):(head(empty_rows,1)+nrow(temp_directory)-1),"CIK"] <- cik_catalog[i,"CIK"]
#   directory_level1[head(empty_rows,1):(head(empty_rows,1)+nrow(temp_directory)-1),"CIK_no_pad"] <- cik_catalog[i,"CIK_no_pad"]
#   directory_level1[head(empty_rows,1):(head(empty_rows,1)+nrow(temp_directory)-1),"Directory_Address_HTTP"] <- cik_catalog[i,"Sub_Link_HTTP"]
#   directory_level1[head(empty_rows,1):(head(empty_rows,1)+nrow(temp_directory)-1),"Directory_Address_FTP"] <- cik_catalog[i,"Sub_Link_FTP"]
#   
#   #Add temp directory to final dataset
#   directory_level1[head(empty_rows,1):(head(empty_rows,1)+nrow(temp_directory)-1),c("Name","Last_Modified","Size","Description")] <- temp_directory
#   
#   #Add sub addresses
#   directory_level1[,"Sub_Link_HTTP"] <- ifelse((is.na(directory_level1[,"Directory_Address_HTTP"]) & is.na(directory_level1[,"Name"])),
#                                                NA,paste(directory_level1[,"Directory_Address_HTTP"],directory_level1[,"Name"],sep=""))
#   directory_level1[,"Sub_Link_FTP"] <- ifelse((is.na(directory_level1[,"Directory_Address_FTP"]) & is.na(directory_level1[,"Name"])),
#                                               NA,paste(directory_level1[,"Directory_Address_FTP"],directory_level1[,"Name"],sep=""))
#   
#   #rm(temp_directory)
#   
# }
# 
# #Convert Last_Modified to Date
# directory_level1[,"Last_Modified"] <- as.POSIXct(directory_level1[,"Last_Modified"], format="%d-%b-%Y %H:%M")
# 
# #Standardize size to bytes
# directory_level1[,"Size"] <- ifelse(directory_level1[,"Size"]=="-",NA, directory_level1[,"Size"])
# directory_level1[,"Size"] <- convb(directory_level1[,"Size"])
# 
# #Add directory description
# #directory_level1[,"Description"] <- ifelse(is.na(directory_level1[,"Description"]),"Directory", directory_level1[,"Description"])
# directory_level1[,"Description"] <- ifelse((substr(directory_level1[,"Name"], nchar(directory_level1[,"Name"]), nchar(directory_level1[,"Name"]))=="/" & 
#                                               is.na(directory_level1[,"Size"])),"Directory", directory_level1[,"Description"])
# 
# #Create Accession Number
# #directory_level1 <- directory_Accession_num_L1(directory_level1)
# 
# #Remove ALL NA rows
# directory_level1 <- directory_level1[rowSums(is.na(directory_level1[,1:ncol(directory_level1)]))<ncol(directory_level1),]
# 
# #Reorder row numbers
# row.names(directory_level1) <- seq(nrow(directory_level1))

# 
# directory_level1_temp <- adply(cik_catalog[which(cik_catalog[,"Description"]=="Directory"),],1,.fun=function(x,linkcol="Sub_Link_HTTP"){
#   
#  return(directory_listing(x[,linkcol]))
#  
#  }
#  ,.progress = "text", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
# 
# #Trim directory
# directory_level1_temp <- directory_trimming(directory_level1_temp)
# 
# #Find first row that is empty in final dataset
# empty_rows <- which(rowSums(is.na(directory_level1_test[,1:ncol(directory_level1_test)]))==ncol(directory_level1_test))
# 
# #Add temp directory columns to final dataset
# #directory_level1_test[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_level1_temp)-1),
# #                      c("Name","Last_Modified","Size","Description")] <- directory_level1_temp[,c("Name","Last.modified","Size","Description")]
# directory_level1_test[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_level1_temp)-1),"CIK"] <- directory_level1_temp[,"CIK"]
# directory_level1_test[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_level1_temp)-1),"CIK_no_pad"] <- directory_level1_temp[,"CIK_no_pad"]
# directory_level1_test[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_level1_temp)-1),"Directory_Address_HTTP"] <- directory_level1_temp[,"Sub_Link_HTTP"]
# directory_level1_test[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_level1_temp)-1),"Directory_Address_FTP"] <- directory_level1_temp[,"Sub_Link_FTP"]
# directory_level1_test[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_level1_temp)-1),"Name"] <- directory_level1_temp[,"Name"]
# directory_level1_test[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_level1_temp)-1),"Last_Modified"] <- directory_level1_temp[,"Last.modified"]
# directory_level1_test[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_level1_temp)-1),"Size"] <- directory_level1_temp[,"Size"]
# directory_level1_test[head(empty_rows,1):(head(empty_rows,1)+nrow(directory_level1_temp)-1),"Description"] <- directory_level1_temp[,"Description"]
# 
# #Clean directory
# directory_level1_test <- directory_cleaning(directory_level1_test)


directory_level1_test1 <- run_directory(cik_catalog[which(cik_catalog[,"Description"]=="Directory"),])


# 
# 
# comparison <- compare(directory_level1,directory_level1_test,allowAll=TRUE)
# comparison$tM
# #difference <-data.frame(lapply(1:ncol(a1),function(i)setdiff(a1[,i],comparison$tM[,i])))
# 
# a1NotIna2 <- sqldf('SELECT * FROM directory_level1 EXCEPT SELECT * FROM directory_level1_test')
# 
# #onenotintwo <- XnotinY(directory_level1,directory_level1_test)
# 
# temp1 <- directory_level1[directory_level1[,"Name"]=="00/",]
# temp2a <- directory_level1_test[directory_level1_test[,"Name"]=="00/",]
# temp2b <- directory_level1_temp[directory_level1_temp[,"Name"]=="00/",]


###############################################################################
cat("SECTION: LEVEL 2 DIRECTORY", "\n")
###############################################################################

#TEMP!!
directory_level1_test2 <- directory_level1_test2[directory_level1_test2[,"Name"] %in% c("00/","000087660300000005/"),] 
row.names(directory_level1_test2) <- seq(nrow(directory_level1_test2))
directory_level2_test2 <- directory_level1
#END TEMP!


# for(j in which(directory_level1[,"Description"]=="Directory"))
# {
#   #j <- tail(head(which(directory_level1[,"Description"]=="Directory"),1),1)
#   #j <- tail(head(which(directory_level1[,"Description"]=="Directory"),2),1)
#   
#   #Get identifiers
#   #sub_cik <- directory_level1[j,"CIK"]
#   #sub_cik_no_pad <- directory_level1[j,"CIK_no_pad"]
#   
#   #Get addresses
#   #sub_directory_address_http <- directory_level1[j,"Link_HTTP"]
#   #sub_directory_address_ftp <- directory_level1[j,"Link_FTP"]
# 
#   #Get Directory
#   temp_sub_directory <- directory_listing(directory_level1[j,"Sub_Link_HTTP"])
#   
#   #Clean Directory
#   temp_sub_directory <- directory_cleaning(temp_sub_directory)
#   
#   #Find first row that is empty in final dataset
#   sub_empty_rows <- which(rowSums(is.na(directory_level2[,1:ncol(directory_level2)]))==ncol(directory_level2))
#   
#   #Add CIK, CIK_no_pad, directory_address_http, & directory_address_ftp to final dataset
#   directory_level2[head(sub_empty_rows,1):(head(sub_empty_rows,1)+nrow(temp_sub_directory)-1),"CIK"] <- directory_level1[j,"CIK"]
#   directory_level2[head(sub_empty_rows,1):(head(sub_empty_rows,1)+nrow(temp_sub_directory)-1),"CIK_no_pad"] <- directory_level1[j,"CIK_no_pad"]
#   directory_level2[head(sub_empty_rows,1):(head(sub_empty_rows,1)+nrow(temp_sub_directory)-1),"Directory_Address_HTTP"] <- directory_level1[j,"Sub_Link_HTTP"]
#   directory_level2[head(sub_empty_rows,1):(head(sub_empty_rows,1)+nrow(temp_sub_directory)-1),"Directory_Address_FTP"] <- directory_level1[j,"Sub_Link_FTP"]
#   
#   #Add temp directory to final dataset
#   directory_level2[head(sub_empty_rows,1):(head(sub_empty_rows,1)+nrow(temp_sub_directory)-1),c("Name","Last_Modified","Size","Description")] <- temp_sub_directory
#   
#   #Add sub addresses
#   directory_level2[,"Sub_Link_HTTP"] <- ifelse((is.na(directory_level2[,"Directory_Address_HTTP"]) & is.na(directory_level2[,"Name"])),
#                                                NA,
#                                                paste(directory_level2[,"Directory_Address_HTTP"],directory_level2[,"Name"],sep=""))
#   directory_level2[,"Sub_Link_FTP"] <- ifelse((is.na(directory_level2[,"Directory_Address_FTP"]) & is.na(directory_level2[,"Name"])),
#                                               NA,
#                                               paste(directory_level2[,"Directory_Address_FTP"],directory_level2[,"Name"],sep=""))
#   
# }
# 
# #Convert Last_Modified to Date
# directory_level2[,"Last_Modified"] <- as.POSIXct(directory_level2[,"Last_Modified"], format="%d-%b-%Y %H:%M")
# 
# #Standardize size to bytes
# directory_level2[,"Size"] <- ifelse(directory_level2[,"Size"]=="-",NA, directory_level2[,"Size"])
# directory_level2[,"Size"] <- convb(directory_level2[,"Size"])
# 
# #Add directory description
# #directory_level2[,"Description"] <- ifelse(is.na(directory_level2[,"Description"]),"Directory", directory_level2[,"Description"])
# directory_level2[,"Description"] <- ifelse((substr(directory_level2[,"Name"], nchar(directory_level2[,"Name"]), nchar(directory_level2[,"Name"]))=="/" & 
#                                               is.na(directory_level2[,"Size"])),"Directory", directory_level2[,"Description"])
# 
# #Create Accession Number
# #directory_level2 <- directory_Accession_num_L2(directory_level2)
# 
# #Remove ALL NA rows
# directory_level2 <- directory_level2[rowSums(is.na(directory_level2[,1:ncol(directory_level2)]))<ncol(directory_level2),]
# 
# #Reorder row numbers
# row.names(directory_level2) <- seq(nrow(directory_level2))
# 
# #rm(temp_sub_directory)
# 


directory_level2_test2 <- run_directory(directory_level1_test2[which(directory_level1_test2[,"Description"]=="Directory"),])
