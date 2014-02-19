#library(chron)
#library(DataCombine)
library(gdata)
library(memoise)
library(plyr)
library(RCurl)
#library(RJDBC)
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

directory_Accession_num_L1 <- function(directory_df){
  
  #Accession string positions and lengths
  Accession_start1 <- 0
  Accession_length1 <- 10
  Accession_start2 <- 11
  Accession_length2 <- 1
  Accession_start3 <- 13
  Accession_length3 <- 5
  
  #Create Accession number
  directory_df[,"Accession_num"] <- paste(substring(gsub(pattern="-", replacement="", directory_df[,"Name"]), Accession_start1, Accession_start1+Accession_length1),
                                          substring(gsub(pattern="-", replacement="", directory_df[,"Name"]), Accession_start2, Accession_start2+Accession_length2),
                                          substring(gsub(pattern="-", replacement="", directory_df[,"Name"]), Accession_start3, Accession_start3+Accession_length3),
                                          sep="-")
  
  
  
  return(directory_df)
}

directory_listing <- function(http_address){
  
  #http_address <- cik_catalog[1,"Sub_Link_HTTP"]
  #http_address <- cik_catalog_test[1,"Sub_Link_HTTP"]
  #http_address <- cik_catalog_test[2,"Sub_Link_HTTP"]
  
  output_directory1 <- try(readHTMLTable(http_address), silent=T)
  if (inherits(output_directory1, "try-error")) 
  {
    cat( "\n", "BAD:", http_address, "\n") 
    
    output_directory3 <- data.frame(Trash=character(),
                                    Name=character(), 
                                    Last.modified=character(), 
                                    Size=character(),                                     
                                    Description=character(),                                     
                                    stringsAsFactors=FALSE)
    
    if(exists("bad_Address"))
    {
      #cat( "\n", "ADDING BAD ADDRESS:", http_address, "\n") 
      
      assign("bad_Address", rbind(bad_Address,data.frame(CIK=NA,
                                                         CIK_no_pad=NA,
                                                         Description="Directory",
                                                         Sub_Link_HTTP=http_address,
                                                         Sub_Link_FTP=NA,
                                                         stringsAsFactors=FALSE)), envir = .GlobalEnv)
      
    } else
    {
      #cat( "\n", "FIRST BAD ADDRESS:", http_address, "\n") 
      
      assign("bad_Address", data.frame(CIK=NA,
                                       CIK_no_pad=NA,
                                       Description="Directory",
                                       Sub_Link_HTTP=http_address,
                                       Sub_Link_FTP=NA,
                                       stringsAsFactors=FALSE), envir = .GlobalEnv)
      
    }
    
    return(output_directory3)
    
  } else
  {
    cat( "\n", "GOOD:", http_address, "\n") 
    
    output_directory2 <- do.call(rbind, output_directory1)
    #output_directory2 <- as.data.frame(do.call(rbind, output_directory1),stringsAsFactors=FALSE)
    output_directory3 <- data.frame(lapply(output_directory2, as.character), stringsAsFactors=FALSE)
    colnames(output_directory3) <- c("Trash","Name","Last.modified","Size","Description")
    
    rm(output_directory1,output_directory2)
    
    return(output_directory3)
    
  }
  
}

directory_trimming <- function(directory_df){
  
  #directory_df <- directory_temp
  
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
  
  #input_ds <- cik_catalog[which(cik_catalog[,"Description"]=="Directory"),]
  #input_ds <- cik_catalog_test[which(cik_catalog_test[,"Description"]=="Directory"),]
  
  directory_temp <- adply(input_ds,1,.fun=function(x,linkcol="Sub_Link_HTTP"){ return(directory_listing(x[,linkcol])) },
                          .progress = "text", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
  
  #Trim directory
  directory_temp2 <- directory_trimming(directory_temp)
  
  directory_temp3 <- data.frame(CIK=directory_temp2[,"CIK"],
                                CIK_no_pad <- directory_temp2[,"CIK_no_pad"],
                                Directory_Address_HTTP <- directory_temp2[,"Sub_Link_HTTP"],
                                Directory_Address_FTP <- directory_temp2[,"Sub_Link_FTP"], 
                                Accession_Num <- NA,
                                Name <- directory_temp2[,"Name"],
                                Last_Modified <- directory_temp2[,"Last.modified"],
                                Size <- directory_temp2[,"Size"],
                                Description <- directory_temp2[,"Description"],
                                Sub_Link_HTTP <- NA,
                                Sub_Link_FTP <- NA,
                                stringsAsFactors=FALSE)
  colnames(directory_temp3) <- c("CIK","CIK_no_pad","Directory_Address_HTTP","Directory_Address_FTP","Accession_Num",
                                 "Name","Last_Modified","Size","Description","Sub_Link_HTTP","Sub_Link_FTP")
  
  #Clean directory
  output_ds <- directory_cleaning(directory_temp3)
  
  return(output_ds)
}


###############################################################################
cat("SECTION: IMPORT CIK CATALOG", "\n")
###############################################################################

output_directory <- normalizePath("C:/Research_temp3/",winslash="\\", mustWork=TRUE)

cik_catalog_temp <- read.csv(file=paste(output_directory,"MF_CIKs","CIK_list.csv",sep="/"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
cik_catalog_temp <- as.data.frame(cik_catalog_temp,stringsAsFactors=FALSE)
colnames(cik_catalog_temp) <- c("CIK","CIK_no_pad")

#good_rows <- adply(cik_catalog_temp,1,.fun=function(x,col="CIK_no_pad"){
#  
#  tryCatch(x[,col] <- as.integer(x[,col]), 
#           warning=function(w){ cat( "\n", "problem rows:", row.names(x), "\n") } )
#
#  }
#  ,.progress = "text", .inform = FALSE,.parallel = FALSE, .paropts = NULL)

cik_catalog_temp[,"CIK_no_pad"] <- as.numeric(cik_catalog_temp[,"CIK_no_pad"])
cik_catalog_temp[,"CIK_no_pad"] <- round(cik_catalog_temp[,"CIK_no_pad"], digits = 0)

cik_catalog_temp <- cik_catalog_temp[order(cik_catalog_temp[,"CIK_no_pad"]),] 
row.names(cik_catalog_temp) <- seq(nrow(cik_catalog_temp))

for (i in 1:ncol(cik_catalog_temp))
{
  cik_catalog_temp[,i] <- unknownToNA(cik_catalog_temp[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                      NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  cik_catalog_temp[,i] <- ifelse(is.na(cik_catalog_temp[,i]),NA, cik_catalog_temp[,i])
} 

cik_catalog <- data.frame(CIK=NA,
                          CIK_no_pad=cik_catalog_temp[,"CIK_no_pad"],
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

rm(cik_catalog_temp,i)


###############################################################################
cat("SECTION: SEARCH N LEVEL DIRECTORIES", "\n")
###############################################################################

#cik_catalog_test <- cik_catalog[cik_catalog[,"CIK"] %in% c("0000876603","0001414040"),] 
#cik_catalog_test <- cik_catalog[1:10,] 
#row.names(cik_catalog_test) <- seq(nrow(cik_catalog_test))

#DEBUGGING
#last_good_row <- which(cik_catalog[,"CIK_no_pad"]=="7084")
#cik_catalog_test <- cik_catalog[(last_good_row):(last_good_row+10),] 
#row.names(cik_catalog_test) <- seq(nrow(cik_catalog_test))


files <- data.frame(name="cik_catalog",
                    checked=0,
                    initial=1,
                    stringsAsFactors=FALSE)

run_all <- function(ds_name){
  
  #ds_name <- "files"
  
  x <- get(ds_name)
  
  input_name <- ds_name
  #input_name <- deparse(substitute(x))
  #cat("INPUT:",input_name,"\n")
  
  x[nrow(x),"checked"] <- 1 
  
  dir_index <- which(get(x[nrow(x),"name"])[,"Description"]=="Directory")
  if (length(dir_index) != 0)
  {
    cat("LEVEL:",nrow(x),"\n")
    
    assign(paste("directory_level",nrow(x),sep=""), run_directory(get(x[nrow(x),"name"])[dir_index,]), envir = .GlobalEnv)
    
    temp <- rbind(x,c(paste("directory_level",nrow(x),sep=""),0,0))
    
    #x <- temp
    #return(run_all(x))
    
    assign(input_name, temp, envir = .GlobalEnv)
    
    return(run_all(input_name))
    
    
  } else
  {
    temp <- x
    
    assign(input_name, temp, envir = .GlobalEnv)
    
    cat("ALL DIRECTORIES SEARCHED!!!", "\n")
    
  }
  
}

run_all("files")


###############################################################################
cat("SECTION: COMBINE N LEVEL DIRECTORIES", "\n")
###############################################################################



