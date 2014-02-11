library(chron)
library(DataCombine)
library(gdata)
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
  
  return(directory_df)
}

directory_expand <- function(directory_df,http_address,ftp_address){
  temp_directory_full <- data.frame(CIK=cik,
                                    CIK_no_pad=cik_no_pad,
                                    Accession_num=NA,
                                    Name=directory_df[,"Name"],                           
                                    Last_Modified=directory_df[,"Last.modified"],                          
                                    Size=directory_df[,"Size"],                            
                                    Description=directory_df[,"Description"], 
                                    Link_HTTP=NA,
                                    Link_FTP=NA,
                                    stringsAsFactors=FALSE)
  
  #Remove all NA rows
  temp_directory_full <- temp_directory_full[rowSums(is.na(temp_directory_full[,1:ncol(temp_directory_full)]))<ncol(temp_directory_full),]
  
  #Remove parent directory row
  temp_directory_full <- temp_directory_full[!(temp_directory_full[,"Name"]=="Parent Directory"),]
  
  #Remove NA names row
  temp_directory_full <- temp_directory_full[!(is.na(temp_directory_full[,"Name"])),]
  
  #Reorder row numbers
  row.names(temp_directory_full) <- seq(nrow(temp_directory_full))
  
  #Convert Last_Modified to Date
  temp_directory_full[,"Last_Modified"] <- as.POSIXct(temp_directory_full[,"Last_Modified"], format="%d-%b-%Y %H:%M")
  
  temp_directory_full[,"Description"] <- ifelse(is.na(temp_directory_full[,"Description"]),"Directory", temp_directory_full[,"Description"])
  
  #Standardize size to bytes
  temp_directory_full[,"Size"] <- ifelse(temp_directory_full[,"Size"]=="-",NA, temp_directory_full[,"Size"])
  temp_directory_full[,"Size"] <- convb(temp_directory_full[,"Size"])
  
  #Create Link
  #temp_directory_full[,"Link"] <- ifelse(temp_directory_full[,"Description"]=="Directory",
  #                                paste(http_address,temp_directory_full[,"Name"],sep=""),
  #                                paste(ftp_address,temp_directory_full[,"Name"],sep=""))
  
  temp_directory_full[,"Link_HTTP"] <- paste(http_address,temp_directory_full[,"Name"],sep="")
  temp_directory_full[,"Link_FTP"] <- paste(ftp_address,temp_directory_full[,"Name"],sep="")
  
  
  return(temp_directory_full)
}

directory_Accession_num_L1 <- function(directory_df)
  
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
  

  
  return(directory_df)
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
  
  return(directory_df)
}


###############################################################################
cat("SECTION: PREALLOCATE DATA", "\n")
############################################################################### 

#Create base column table
#temp_data_cols <- as.data.frame(matrix(NA, ncol=7, nrow=200),stringsAsFactors=FALSE)
#colnames(temp_data_cols) <- c("order","isnum","ischar","isdate","isfactor","colnames","desc")
#temp_data_cols[,1] <- as.numeric(temp_data_cols[,1])
#temp_data_cols[,2] <- as.numeric(temp_data_cols[,2])
#temp_data_cols[,3] <- as.numeric(temp_data_cols[,3])
#temp_data_cols[,4] <- as.numeric(temp_data_cols[,4])
#temp_data_cols[,5] <- as.numeric(temp_data_cols[,5])
#temp_data_cols[,6] <- as.character(temp_data_cols[,6])
#temp_data_cols[,7] <- as.character(temp_data_cols[,7])


#Level 1 Directory table
#directory_level1_cols_count <- 9
#directory_level1_cols <- temp_data_cols[1:directory_level1_cols_count,]
#directory_level1_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="CIK","",stringsAsFactors=FALSE)
#directory_level1_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="CIK_no_pad","",stringsAsFactors=FALSE)
#directory_level1_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Accession_num","",stringsAsFactors=FALSE)
#directory_level1_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Name","",stringsAsFactors=FALSE)
#directory_level1_cols[5,] <- data.frame(order=5,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Last_Modified","",stringsAsFactors=FALSE)
#directory_level1_cols[6,] <- data.frame(order=6,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Size","",stringsAsFactors=FALSE)
#directory_level1_cols[7,] <- data.frame(order=7,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Description","",stringsAsFactors=FALSE)
#directory_level1_cols[8,] <- data.frame(order=8,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Link_HTTP","",stringsAsFactors=FALSE)
#directory_level1_cols[8,] <- data.frame(order=9,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Link_FTP","",stringsAsFactors=FALSE)
#directory_level1 <- as.data.frame(matrix(NA, ncol=directory_level1_cols_count, nrow=100),stringsAsFactors=FALSE)
#colnames(directory_level1) <- directory_level1_cols[,6]


directory_level1_cols <- c("CIK","CIK_no_pad","Accession_num","Name","Last_Modified","Size","Description","Link_HTTP","Link_FTP")
input_df <- data.frame(input,
                       matrix(NA, nrow = length(input), ncol = 6,dimnames = list(NULL,expanded_cols)),
                       stringsAsFactors=FALSE)






###############################################################################
cat("SECTION: LEVEL 1 DIRECTORY", "\n")
###############################################################################

#Get identifiers
cik <- "0001414040"
cik_no_pad <- as.character(as.integer(cik))

#Get addresses
directory_address_http <- paste("http://www.sec.gov/Archives/edgar/data/",cik_no_pad,"/",sep="") 
directory_address_ftp <- paste("ftp://ftp.sec.gov/edgar/data/",cik_no_pad,"/",sep="") 

#Get Directory
directory <- directory_listing(directory_address_http)

#Clean Directory
directory <- directory_cleaning(directory)

#Expand Directory
directory_full <- directory_expand(directory,directory_address_http,directory_address_ftp)

#Create Accession Number
#directory_full <- directory_Accession_num_L1(directory_full)


###############################################################################
cat("SECTION: LEVEL 2 DIRECTORY", "\n")
###############################################################################

for(i in which(directory_full[,"Description"]=="Directory"))
{
  #i <- head(which(directory_full[,"Description"]=="Directory"),1)

  #Get identifiers
  sub_cik <- directory_full[i,"CIK"]
  sub_cik_no_pad <- directory_full[i,"CIK_no_pad"]
  
  #Get addresses
  sub_directory_address_http <- directory_full[i,"Link_HTTP"]
  sub_directory_address_ftp <- directory_full[i,"Link_FTP"]


  #Get Directory
  sub_directory <- directory_listing(directory_full[i,"Link_HTTP"])
  
  #Clean Directory
  sub_directory <- directory_cleaning(sub_directory)
  
  #Expand Directory
  sub_directory_full <- directory_expand(sub_directory,sub_directory_address_http,sub_directory_address_ftp)
  
  #Create Accession Number
  #sub_directory_full <- directory_Accession_num_L2(sub_directory_full)
  
  
}







