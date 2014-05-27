# http://iangow.wordpress.com/2012/05/22/extracting-files-from-sec-complete-submission-text-files/
# http://iangow.wordpress.com/2012/02/16/extracting-files-from-sec-complete-submission-text-filings/

extract.filings <- function(file) {
  ## A function to extract filings from complete submission text files submitted
  ## to the SEC into the component files contained within them.
  require(XML)
  
  new_location <- file.path(extract_directory, file)
  file <- file.path(raw_directory, file)
  
  # Parse the file as an XML file containing multiple documents
  webpage <- readLines(file)
  
  file.name <- gsub("<FILENAME>","", 
                    grep("<FILENAME>.*$", webpage,  perl=TRUE, value=TRUE))
  print(file.name)
  # If there are no file names, then the full text submission is simply a text file.
  # Rather than copying this to the new location, I just symlink it (this saves space).
  if (length(file.name)==0) { 
    if (!file.exists(new_location)) {
      dir.create(dirname(new_location), showWarnings=FALSE,
                 recursive = TRUE)
      try(file.remove(new_location))
      file.symlink(from=file, to=new_location)
    }
    return(new_location)
  } else {
    #  return(dirname(new_location))
  }
  
  # If got here, we have a full-text submission that isn't simply a text file
  # We need to make the parent directory for the component files that are 
  # embedded in the submission
  file.dir <- gsub("-(\\d{2})-(\\d{6})\\.txt$", "\\1\\2", new_location, perl=TRUE)
  print(file.dir)
  dir.create(file.dir, showWarnings=FALSE, recursive=TRUE)
  
  # Get a list of file names, and their start and end locations within the
  # text file. (I use unique file names, as sometimes--albeit rarely--the
  # filename is repeated).
  file.name <- unique(file.path(file.dir, file.name))
  start.line <- grep("<DOCUMENT>.*$", webpage,  perl=TRUE) 
  end.line <- grep("</DOCUMENT>.*$", webpage,  perl=TRUE)     
  print(file.name)
  
  for (i in 1:length(file.name)) {
    # Skip the file if it already exists and the extracted file was extracted 
    # recently.
    if(file.exists(file.name[i]) && 
         as.Date(file.info(file.name[i])$ctime) > "2012-02-15") {
      next
    }
    
    # Get the extension of the file to be extracted
    file.ext <- gsub(".*\\.(.*?)$", "\\1", file.name[i])
    
    # Extract binary files
    if (file.ext %in% c("zip", "xls", "jpg", "gif")) {
      temp <- webpage[start.line[i]:end.line[i]]
      pdf.start <- grep("^begin", temp,  perl=TRUE)
      pdf.end <- grep("^end", temp,  perl=TRUE)  
      t <- tempfile()
      writeLines(temp[pdf.start:pdf.end], con=t)
      print(paste("uudecode -o", file.name[i], t))
      system(paste("uudecode -o", file.name[i], t))
      unlink(t)
    }
    
    # Extract simple text files
    if (file.ext=="txt") {
      temp <- webpage[start.line[i]:end.line[i]]
      writeLines(temp, con=file.name[i])
    }
    
    # Extract text-based formatted file types
    if (file.ext %in% c("htm", "js", "css", "paper", "xsd")) {
      temp <- webpage[start.line[i]:end.line[i]]
      pdf.start <- grep("^<TEXT>", temp,  perl=TRUE) +1
      pdf.end <- grep("^</TEXT>", temp,  perl=TRUE) -1  
      t <- tempfile()
      writeLines(temp[pdf.start:pdf.end], con=file.name[i])
      unlink(t)
    }
    
    # Extract PDFs
    if (file.ext=="pdf") {
      temp <- webpage[start.line[i]:end.line[i]]
      pdf.start <- grep("^<PDF>", temp,  perl=TRUE) +1
      pdf.end <- grep("^</PDF>", temp,  perl=TRUE) -1  
      t <- tempfile()
      writeLines(temp[pdf.start:pdf.end], con=t)
      print(paste("uudecode -o", file.name[i], t))
      system(paste("uudecode -o", file.name[i], t))
      unlink(t)
    }
    
  }
  return(dirname(new_location))
}

raw_directory <- ""
extract_directory <- "/hdd"

extract.filings("edgar/data/1412665/0001144204-09-014344.txt")