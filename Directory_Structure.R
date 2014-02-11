library(chron)
library(DataCombine)
library(gdata)
library(plyr)
library(RCurl)
library(XML)

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
  
  #temp_directory <- htmlParse(webpage_df_html_only2,isURL=FALSE, options = HUGE)
  #temp_directory <- htmlTreeParse(webpage, useInternalNodes = TRUE, options = HUGE)
  #temp_directory <- htmlTreeParse(webpage, useInternalNodes = FALSE, options = HUGE) 
  #temp_directory <- xmlTreeParse(webpage,useInternalNodes = FALSE, options = HUGE)
  temp_directory <- readHTMLTable(http_address)
  temp_directory <- as.data.frame(do.call(rbind, temp_directory),stringsAsFactors=FALSE)
  temp_directory <- data.frame(lapply(temp_directory, as.character), stringsAsFactors=FALSE)
  
  return(temp_directory)
}

directory_cleaning <- function(directory_df){
  
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

#Get identifiers
cik <- c("0001414040","0000876603")

cik_catalog <- data.frame(CIK=cik,
                          CIK_no_pad=NA,
                          Description="Directory",
                          Sub_Link_HTTP=NA,
                          Sub_Link_FTP=NA,
                          stringsAsFactors=FALSE)

#Remove leading zeros in CIK
cik_catalog[,"CIK_no_pad"] <- as.character(as.integer(cik_catalog[,"CIK"]))

#Get addresses
cik_catalog[,"Sub_Link_HTTP"] <- paste("http://www.sec.gov/Archives/edgar/data/",cik_catalog[,"CIK_no_pad"],"/",sep="") 
cik_catalog[,"Sub_Link_FTP"] <- paste("ftp://ftp.sec.gov/edgar/data/",cik_catalog[,"CIK_no_pad"],"/",sep="") 


###############################################################################
cat("SECTION: LEVEL 1 DIRECTORY", "\n")
###############################################################################

for(i in which(cik_catalog[,"Description"]=="Directory"))
{
  #i <- tail(head(which(cik_catalog[,"Description"]=="Directory"),1),1)
  #i <- tail(head(which(cik_catalog[,"Description"]=="Directory"),2),1)

  #Get Directory
  temp_directory <- directory_listing(cik_catalog[i,"Sub_Link_HTTP"])
  
  #Clean Directory
  temp_directory <- directory_cleaning(temp_directory)
  
  #Find first row that is empty in final dataset
  empty_rows <- which(rowSums(is.na(directory_level1[,1:ncol(directory_level1)]))==ncol(directory_level1))
  
  #Add CIK, CIK_no_pad, directory_address_http, & directory_address_ftp to final dataset
  directory_level1[head(empty_rows,1):(head(empty_rows,1)+nrow(temp_directory)-1),"CIK"] <- cik_catalog[i,"CIK"]
  directory_level1[head(empty_rows,1):(head(empty_rows,1)+nrow(temp_directory)-1),"CIK_no_pad"] <- cik_catalog[i,"CIK_no_pad"]
  directory_level1[head(empty_rows,1):(head(empty_rows,1)+nrow(temp_directory)-1),"Directory_Address_HTTP"] <- cik_catalog[i,"Sub_Link_HTTP"]
  directory_level1[head(empty_rows,1):(head(empty_rows,1)+nrow(temp_directory)-1),"Directory_Address_FTP"] <- cik_catalog[i,"Sub_Link_FTP"]
  
  #Add temp directory to final dataset
  directory_level1[head(empty_rows,1):(head(empty_rows,1)+nrow(temp_directory)-1),c("Name","Last_Modified","Size","Description")] <- temp_directory
    
  #Add sub addresses
  directory_level1[,"Sub_Link_HTTP"] <- ifelse((is.na(directory_level1[,"Directory_Address_HTTP"]) & is.na(directory_level1[,"Name"])),
                                               NA,
                                               paste(directory_level1[,"Directory_Address_HTTP"],directory_level1[,"Name"],sep=""))
  directory_level1[,"Sub_Link_FTP"] <- ifelse((is.na(directory_level1[,"Directory_Address_FTP"]) & is.na(directory_level1[,"Name"])),
                                               NA,
                                               paste(directory_level1[,"Directory_Address_FTP"],directory_level1[,"Name"],sep=""))
  
}

#Convert Last_Modified to Date
directory_level1[,"Last_Modified"] <- as.POSIXct(directory_level1[,"Last_Modified"], format="%d-%b-%Y %H:%M")

#Standardize size to bytes
directory_level1[,"Size"] <- ifelse(directory_level1[,"Size"]=="-",NA, directory_level1[,"Size"])
directory_level1[,"Size"] <- convb(directory_level1[,"Size"])

#Add directory description
#directory_level1[,"Description"] <- ifelse(is.na(directory_level1[,"Description"]),"Directory", directory_level1[,"Description"])
directory_level1[,"Description"] <- ifelse((substr(directory_level1[,"Name"], nchar(directory_level1[,"Name"]), nchar(directory_level1[,"Name"]))=="/" & 
                                              is.na(directory_level1[,"Size"])),"Directory", directory_level1[,"Description"])

#Create Accession Number
#directory_level1 <- directory_Accession_num_L1(directory_level1)

#Remove ALL NA rows
directory_level1 <- directory_level1[rowSums(is.na(directory_level1[,1:ncol(directory_level1)]))<ncol(directory_level1),]

#Reorder row numbers
row.names(directory_level1) <- seq(nrow(directory_level1))

#rm(temp_directory)


###############################################################################
cat("SECTION: LEVEL 2 DIRECTORY", "\n")
###############################################################################

for(j in which(directory_level1[,"Description"]=="Directory"))
{
  #j <- tail(head(which(directory_level1[,"Description"]=="Directory"),1),1)
  #j <- tail(head(which(directory_level1[,"Description"]=="Directory"),2),1)
  
  #Get identifiers
  #sub_cik <- directory_level1[j,"CIK"]
  #sub_cik_no_pad <- directory_level1[j,"CIK_no_pad"]
  
  #Get addresses
  #sub_directory_address_http <- directory_level1[j,"Link_HTTP"]
  #sub_directory_address_ftp <- directory_level1[j,"Link_FTP"]

  #Get Directory
  temp_sub_directory <- directory_listing(directory_level1[j,"Sub_Link_HTTP"])
  
  #Clean Directory
  temp_sub_directory <- directory_cleaning(temp_sub_directory)
  
  #Find first row that is empty in final dataset
  sub_empty_rows <- which(rowSums(is.na(directory_level2[,1:ncol(directory_level2)]))==ncol(directory_level2))
  
  #Add CIK, CIK_no_pad, directory_address_http, & directory_address_ftp to final dataset
  directory_level2[head(sub_empty_rows,1):(head(sub_empty_rows,1)+nrow(temp_sub_directory)-1),"CIK"] <- directory_level1[j,"CIK"]
  directory_level2[head(sub_empty_rows,1):(head(sub_empty_rows,1)+nrow(temp_sub_directory)-1),"CIK_no_pad"] <- directory_level1[j,"CIK_no_pad"]
  directory_level2[head(sub_empty_rows,1):(head(sub_empty_rows,1)+nrow(temp_sub_directory)-1),"Directory_Address_HTTP"] <- directory_level1[j,"Sub_Link_HTTP"]
  directory_level2[head(sub_empty_rows,1):(head(sub_empty_rows,1)+nrow(temp_sub_directory)-1),"Directory_Address_FTP"] <- directory_level1[j,"Sub_Link_FTP"]
  
  #Add temp directory to final dataset
  directory_level2[head(sub_empty_rows,1):(head(sub_empty_rows,1)+nrow(temp_sub_directory)-1),c("Name","Last_Modified","Size","Description")] <- temp_sub_directory
  
  #Add sub addresses
  directory_level2[,"Sub_Link_HTTP"] <- ifelse((is.na(directory_level2[,"Directory_Address_HTTP"]) & is.na(directory_level2[,"Name"])),
                                               NA,
                                               paste(directory_level2[,"Directory_Address_HTTP"],directory_level2[,"Name"],sep=""))
  directory_level2[,"Sub_Link_FTP"] <- ifelse((is.na(directory_level2[,"Directory_Address_FTP"]) & is.na(directory_level2[,"Name"])),
                                              NA,
                                              paste(directory_level2[,"Directory_Address_FTP"],directory_level2[,"Name"],sep=""))
  
}

#Convert Last_Modified to Date
directory_level2[,"Last_Modified"] <- as.POSIXct(directory_level2[,"Last_Modified"], format="%d-%b-%Y %H:%M")

#Standardize size to bytes
directory_level2[,"Size"] <- ifelse(directory_level2[,"Size"]=="-",NA, directory_level2[,"Size"])
directory_level2[,"Size"] <- convb(directory_level2[,"Size"])

#Add directory description
#directory_level2[,"Description"] <- ifelse(is.na(directory_level2[,"Description"]),"Directory", directory_level2[,"Description"])
directory_level2[,"Description"] <- ifelse((substr(directory_level2[,"Name"], nchar(directory_level2[,"Name"]), nchar(directory_level2[,"Name"]))=="/" & 
                                              is.na(directory_level2[,"Size"])),"Directory", directory_level2[,"Description"])

#Create Accession Number
#directory_level2 <- directory_Accession_num_L2(directory_level2)

#Remove ALL NA rows
directory_level2 <- directory_level2[rowSums(is.na(directory_level2[,1:ncol(directory_level2)]))<ncol(directory_level2),]

#Reorder row numbers
row.names(directory_level2) <- seq(nrow(directory_level2))

#rm(temp_sub_directory)



