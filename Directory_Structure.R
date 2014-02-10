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

directory_expand <- function(directory_df){
  temp_directory_full <- data.frame(CIK=cik,
                                    CIK_no_pad=cik_no_pad,
                                    Accession_num=NA,
                                    Name=directory_df[,"Name"],                           
                                    Last_Modified=directory_df[,"Last.modified"],                          
                                    Size=directory_df[,"Size"],                            
                                    Description=directory_df[,"Description"], 
                                    Link=NA,
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
  
  return(temp_directory_full)
}

directory_links_L1 <- function(directory_df,http_address,ftp_address){
  
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
  
  #Create Link
  directory_df[,"Link"] <- ifelse(directory_df[,"Description"]=="Directory",
                                  paste(http_address,directory_df[,"Name"],sep=""),
                                  paste(ftp_address,directory_df[,"Name"],sep=""))
  
  return(directory_df)
}

directory_links_L2 <- function(directory_df,http_address,ftp_address){
  
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
  
  #Create Link
  directory_df[,"Link"] <- ifelse(directory_df[,"Description"]=="Directory",
                                  paste(webpage_address_http,directory_df[,"Name"],sep=""),
                                  paste(webpage_address_ftp,directory_df[,"Name"],sep=""))
  
  return(directory_df)
}



#######################
# BEGIN
#####################


#####################################
# LEVEL 1
#####################################

cik <- "0001414040"
cik_no_pad <- as.character(as.integer(cik))

webpage_address_ftp <- paste("ftp://ftp.sec.gov/edgar/data/",cik_no_pad,"/",sep="") 
#filenames <- getURL(webpage_address_ftp, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE) 

webpage_address_http <- paste("http://www.sec.gov/Archives/edgar/data/",cik_no_pad,"/",sep="") 




#Get Directory
directory <- directory_listing(webpage_address_http)

#Clean Directory
directory <- directory_cleaning(directory)

#Expand Directory
directory_full <- directory_expand(directory)

#Create Level 1 Directory Links
directory_full <- directory_links_L1(directory_full,webpage_address_http,webpage_address_ftp)


#####################################
# LEVEL 2
#####################################

for(i in which(directory_full[,"Description"]=="Directory"))
{
  #i <- head(which(directory_full[,"Description"]=="Directory"),1)
  
  
  
  
  
  
  cik <- "0001414040"
  cik_no_pad <- as.character(as.integer(cik))
  
  webpage_address_ftp <- paste("ftp://ftp.sec.gov/edgar/data/",cik_no_pad,"/",sep="") 
  #filenames <- getURL(webpage_address_ftp, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE) 
  
  webpage_address_http <- paste("http://www.sec.gov/Archives/edgar/data/",cik_no_pad,"/",sep="") 
  
  
  
  
  
  
  
  
  
  #Get Directory
  sub_directory <- directory_listing(directory_full[i,"Link"])
  
  #Clean Directory
  sub_directory <- directory_cleaning(sub_directory)
  
  #Expand Directory
  sub_directory_full <- directory_expand(sub_directory)
  
  #Create Level 2 Directory Links
  sub_directory_full <- directory_links_L2(sub_directory_full,webpage_address_http,webpage_address_ftp)
  
  
  
}







