# 
# 
# fix_edgar_tags <- function(file,entity_encoding,html_tags,sec_tags){
#   
#   #file <- webpage_tags_expand
#   #entity_encoding <- entity_encoding
#   #html_tags <- html_tags
#   #sec_tags <- sec_tags
#   
#   require(gdata)
#   require(plyr)
#   
#   non_ending_tags_html <- html_tags[html_tags[,c("IGNORE_CLOSING")]=="Yes",c("TAG_SHORT")]
#   non_ending_tags_sec <- sec_tags[sec_tags[,c("IGNORE_CLOSING")]=="Yes",c("TAG_SHORT")]
#   non_ending_tags <- c(non_ending_tags_html,non_ending_tags_sec)
#   rm(non_ending_tags_html,non_ending_tags_sec)
#   
#   col_input_temp <- colnames(file)
#   col_tag_first_legit <- col_input_temp[grep("tag_first_legit",col_input_temp)]
#   
#   col_input_temp <- col_input_temp[!(col_input_temp %in% col_tag_first_legit)]
#   col_tag_first <-  col_input_temp[grep("tag_first",col_input_temp)]
#   
#   col_input_temp <- col_input_temp[!(col_input_temp %in% col_tag_first)]
#   col_tag_short <- col_input_temp[grep("tag_short",col_input_temp)]
#   
#   col_input_temp <- col_input_temp[!(col_input_temp %in% col_tag_short)]
#   col_tag_type_oc <- col_input_temp[grep("tag_type_oc",col_input_temp)]
#   
#   col_input_temp <- col_input_temp[!(col_input_temp %in% col_tag_type_oc)]
#   col_tag_type_sh <- col_input_temp[grep("tag_type_sh",col_input_temp)]
#   
#   col_input_temp <- col_input_temp[!(col_input_temp %in% col_tag_type_sh)]
#   col_tag <- col_input_temp[grep("tag",col_input_temp)]
#   
#   col_input_temp <- col_input_temp[!(col_input_temp %in% col_tag)]
#   col_val <- col_input_temp[grep("val",col_input_temp)]
#   
#   col_val_decoded <- paste(col_val,"_decoded",sep="")
#   col_val_encoded <- paste(col_val,"_encoded",sep="")
#   
#   rm(col_input_temp)
#   
#   levels0 <- unlist(regmatches(colnames(file), gregexpr('\\(?[0-9,.]+', colnames(file))))
#   levels1 <- unique(levels0)
#   levels_val <- levels1
#   levels_other <- levels1[!levels1=="00"]
#   rm(levels0,levels1)
#   
#   #Make sure that tags are present
#   #webpage_tags1 <- webpage_tags0
#   #webpage_tags1[,"tag"] <-  ifelse((grepl("<", webpage_tags1[,"raw"]) & grepl(">", webpage_tags1[,"raw"])), webpage_tags1[,"tag"], NA)
#   #webpage_tags1[,"tag"] <-  ifelse((grepl(".*?<(.*?)>.*", webpage_tags1[,"raw"])), webpage_tags1[,"tag"], NA)
#   
#   
#   ##############SHOULD I HANDLE NON-ENDING TAGS HERE???? NOW, DOCTYPE SAYS OPEN
#   #webpage_tags1[,"type"] <-  ifelse(substring(webpage_tags1[,"tag_short"], 1, 1)=="!", NA,  webpage_tags1[,"type"])
#   
#   #Find all possible tags
#   raw_tags_first_legit0 <- file[,c(col_tag_first_legit,col_tag_type_sh)]
#   raw_tags_first_legit0 <- raw_tags_first_legit0[,sort(colnames(raw_tags_first_legit0))]
#   raw_tags_first_legit0 <- unique(raw_tags_first_legit0)
#   
#   raw_tags_first_legit0_stacked0 <- llply(.data=levels_other, .fun = function(x,data){
#     
#     col_keep <- colnames(data)[grep(x,colnames(data))]
#     
#     dataout <- data[,col_keep]
#     colnames(dataout) <- c("tag_first_legit","tag_type_sh")
#     return(dataout)
#     
#   }, data=raw_tags_first_legit0,  .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL)
#   raw_tags_first_legit0_stacked1 <- do.call(rbind,raw_tags_first_legit0_stacked0)
#   raw_tags_first_legit0_stacked <- unique(raw_tags_first_legit0_stacked1)
#   raw_tags_first_legit0_stacked <- raw_tags_first_legit0_stacked[!(raw_tags_first_legit0_stacked[,"tag_first_legit"]==""),]
#   
#   rm(raw_tags_first_legit0,raw_tags_first_legit0_stacked0,raw_tags_first_legit0_stacked1)
#   
#   raw_tags_first_legit1 <- data.frame(raw=unique(raw_tags_first_legit0_stacked[,"tag_first_legit"]),cleaned=unique(raw_tags_first_legit0_stacked[,"tag_first_legit"]),stringsAsFactors=FALSE)
#   raw_tags_first_legit1[,"cleaned"] <- gsub("-","_",raw_tags_first_legit1[,"cleaned"])
#   rm(raw_tags_first_legit0_stacked)
#   
#   #Replace tags with hyphens
#   raw_tags_first_legit1_trim <- raw_tags_first_legit1[!(raw_tags_first_legit1[,"raw"]==raw_tags_first_legit1[,"cleaned"]),]
#   row.names(raw_tags_first_legit1_trim) <- seq(nrow(raw_tags_first_legit1_trim))
#   rm(raw_tags_first_legit1)
#   
#   webpage_tags2 <- file
#   #cols_replace_hyphens <- c(col_tag_first_legit,col_tag_first,col_tag_short,col_tag)
#   for (i in 1:nrow(raw_tags_first_legit1_trim))
#   {
#     
#     for (j in 1:length(levels_other))
#     {
#       num_temp <- levels_other[j]
#       
#       col_tag_first_legit_temp <- col_tag_first_legit[grep(num_temp,col_tag_first_legit)]
#       webpage_tags2[,col_tag_first_legit_temp] <- ifelse(webpage_tags2[,col_tag_first_legit_temp]==raw_tags_first_legit1_trim[i,"raw"], raw_tags_first_legit1_trim[i,"cleaned"], webpage_tags2[,col_tag_first_legit_temp])
#       
#       col_tag_first_temp <- col_tag_first[grep(num_temp,col_tag_first)]
#       webpage_tags2[,col_tag_first_temp] <- ifelse(webpage_tags2[,col_tag_first_temp]==raw_tags_first_legit1_trim[i,"raw"], raw_tags_first_legit1_trim[i,"cleaned"], webpage_tags2[,col_tag_first_temp])
#       
#       col_tag_short_temp <- col_tag_short[grep(num_temp,col_tag_short)]
#       webpage_tags2[,col_tag_short_temp] <- ifelse(webpage_tags2[,col_tag_short_temp]==raw_tags_first_legit1_trim[i,"raw"], raw_tags_first_legit1_trim[i,"cleaned"], webpage_tags2[,col_tag_short_temp])
#       
#       col_tag_temp <- col_tag[grep(num_temp,col_tag)]
#       webpage_tags2[,col_tag_temp] <- ifelse(webpage_tags2[,col_tag_temp]==raw_tags_first_legit1_trim[i,"raw"], raw_tags_first_legit1_trim[i,"cleaned"], webpage_tags2[,col_tag_temp])
#       
#     } 
#     rm(num_temp,col_tag_first_legit_temp,col_tag_first_temp,col_tag_short_temp,col_tag_temp,j)
#     
#   } 
#   rm(raw_tags_first_legit1_trim,i)
#   
#   
#   #Add brackets to tags
#   #webpage_tags <- webpage_tags2
#   webpage_tags <- data.frame(matrix(NA, ncol=4, nrow=nrow(webpage_tags2), 
#                                     dimnames=list(c(), c("id","tag_status_open","tag_status_close","raw_encoded"))),
#                              matrix(NA, ncol=length(col_val), nrow=nrow(webpage_tags2), 
#                                     dimnames=list(c(), c(col_val_decoded))),
#                              matrix(NA, ncol=length(col_val), nrow=nrow(webpage_tags2), 
#                                     dimnames=list(c(), c(col_val_encoded))),
#                              webpage_tags2,
#                              stringsAsFactors=FALSE)
#   webpage_tags <- webpage_tags[,sort(colnames(webpage_tags))]
#   webpage_tags <- webpage_tags[,c(c("id","tag_status_open","tag_status_close","raw_encoded"),
#                                   colnames(webpage_tags[,!(colnames(webpage_tags) %in% c("id","tag_status_open","tag_status_close","raw_encoded"))]))]
#   webpage_tags[,"id"] <- seq(1,nrow(webpage_tags))
#   rm(webpage_tags2)
#   
#   #DO I NEED A RAW COLUMN???
#   
#   for (i in 1:length(levels_other))
#   {
#     num_temp <- levels_other[i]
#     col_tag_first_legit_temp <- col_tag_first_legit[grep(num_temp,col_tag_first_legit)]
#     col_tag_temp <- col_tag[grep(num_temp,col_tag)]
#     
#     webpage_tags[,col_tag_temp] <- ifelse(webpage_tags[,col_tag_first_legit_temp]=="", "", paste("<",webpage_tags[,col_tag_temp],">",sep=""))
#     
#   } 
#   rm(num_temp,col_tag_first_legit_temp,col_tag_temp,i)
#   
#   #Collapse Tags
#   tags_collapse <- adply(.data=webpage_tags, .margins=1, .fun = function(x,datacols) {
#     
#     data_temp <- x[,datacols]
#     data_temp <- gsub(" {2,}", " ", data_temp)
#     data_temp <- gsub(" ", "", data_temp)
#     data_temp <- gsub("^\\s+|\\s+$", "", data_temp)
#     data_temp_trim <- data_temp[!(data_temp=="")] 
#     data_temp_tags <- ifelse(length(data_temp_trim)==0,"",paste("<",data_temp_trim,">",sep="",collapse=" "))
#     data_temp_tags <- gsub(" {2,}", " ", data_temp_tags)
#     data_temp_tags <- gsub("^\\s+|\\s+$", "", data_temp_tags)
#     
#   }, datacols=col_tag_first_legit, .expand = FALSE, .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL)
#   colnames(tags_collapse) <- c("id","tag_first_legit_collapse")
#   
#   #Find all possible shortened tags
#   raw_tags_short0 <- file[,c(col_tag_short,col_tag_type_sh)]
#   raw_tags_short0 <- raw_tags_short0[,sort(colnames(raw_tags_short0))]
#   raw_tags_short0 <- unique(raw_tags_short0)
#   
#   raw_tags_short0_stacked0 <- llply(.data=levels_other, .fun = function(x,data){
#     
#     col_keep <- colnames(data)[grep(x,colnames(data))]
#     
#     dataout <- data[,col_keep]
#     colnames(dataout) <- c("tag_short","tag_type_sh")
#     return(dataout)
#     
#   }, data=raw_tags_short0,  .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL)
#   raw_tags_short0_stacked1 <- do.call(rbind,raw_tags_short0_stacked0)
#   raw_tags_short0_stacked <- unique(raw_tags_short0_stacked1)
#   raw_tags_short0_stacked <- raw_tags_short0_stacked[!(raw_tags_short0_stacked[,"tag_short"]==""),]
#   
#   rm(raw_tags_short0,raw_tags_short0_stacked0,raw_tags_short0_stacked1)
#   
#   raw_tags_short1 <- data.frame(raw_tags_short0_stacked,open_count=NA,close_count=NA,stringsAsFactors=FALSE)
#   colnames(raw_tags_short1) <- c("cleaned","tag_type_sh","open_count","close_count")
#   raw_tags_short1 <- unique(raw_tags_short1)
#   #raw_tags_short1 <- raw_tags_short1[!is.na(raw_tags_short1[,1]),]
#   row.names(raw_tags_short1) <- seq(nrow(raw_tags_short1))
#   raw_tags_short1[,"cleaned"] <- gsub("-","_",raw_tags_short1[,"cleaned"])
#   rm(raw_tags_short0_stacked)
#   
#   #   webpage_tags_status <- data.frame(matrix(NA, ncol=3, nrow=nrow(webpage_tags), 
#   #                                            dimnames=list(c(), c("id","tag_status_open","tag_status_close"))), 
#   #                                     stringsAsFactors=FALSE)
#   #   webpage_tags_status[,"id"] <- seq(1,nrow(webpage_tags_status))
#   
#   for (i in 1:nrow(raw_tags_short1))
#   {   
#     webpage_tags[,"tag_status_open"] <- strcount(tags_collapse[,"tag_first_legit_collapse"], paste("<",raw_tags_short1[i,"cleaned"],">",sep=""), " ")
#     webpage_tags[,"tag_status_close"] <- strcount(tags_collapse[,"tag_first_legit_collapse"], paste("</",raw_tags_short1[i,"cleaned"],">",sep=""), " ")
#     
#     raw_tags_short1[i,"open_count"] <- sum(webpage_tags[,"tag_status_open"])
#     raw_tags_short1[i,"close_count"] <- sum(webpage_tags[,"tag_status_close"])
#     
#     webpage_tags[,"tag_status_open"] <- NA
#     webpage_tags[,"tag_status_close"] <- NA
#     
#   } 
#   rm(i)
#   
#   #Rename temp columns
#   colnames(webpage_tags)[match("tag_status_open",names(webpage_tags))] <- "Final_tag"
#   colnames(webpage_tags)[match("tag_status_close",names(webpage_tags))] <- "Good"
#   
#   #Find tags that are not closed
#   raw_tags_short1_bad0 <- raw_tags_short1[which(raw_tags_short1[,"open_count"]-raw_tags_short1[,"close_count"] != 0),]
#   raw_tags_short1_bad1 <- raw_tags_short1_bad0[!(raw_tags_short1_bad0[,"cleaned"] %in% non_ending_tags),]
#   raw_tags_short1_bad <- raw_tags_short1_bad1[(raw_tags_short1_bad1[,"tag_type_sh"]=="sec"),]
#   #row.names(raw_tags_short1_bad) <- seq(nrow(raw_tags_short1_bad))
#   #rm(raw_tags_short1)
#   
#   #Find tags that need to be closed
#   #webpage_tags[,"Good"] <- ifelse(webpage_tags[,"tag_first_word_legit"] %in% raw_tags_short1_bad[,"cleaned"], 0, 1)
#   #webpage_tags[,"Good"] <- ifelse(is.na(webpage_tags[,"tag_first_word_legit"]), NA, webpage_tags[,"Good"])
#   
#   webpage_tags[,"Good"] <- ifelse(!(webpage_tags[,"col01_tag_short"] %in% raw_tags_short1_bad[,"cleaned"]), 1, 0)
#   webpage_tags[,"Good"] <- ifelse(!(webpage_tags[,"col01_tag_short"] %in% raw_tags_short1[(raw_tags_short1[,"tag_type_sh"]=="sec"),c("cleaned")]), "", webpage_tags[,"Good"])
#   webpage_tags[,"Good"] <- ifelse(webpage_tags[,"col01_tag_short"]=="", "", webpage_tags[,"Good"])
#   #rm(raw_tags_short1_bad)
#   
#   #Entity Replacement
#   entity_encoding_trim <- entity_encoding
#   #entity_encoding_trim <- entity_encoding[entity_encoding[,"ASCII.Looks.Like"]=="&",]
#   #entity_encoding_trim <- entity_encoding[!(entity_encoding[,"ASCII.Looks.Like"] %in% c("<",">")),]
#   
#   entity_encoding_trim_amp <- entity_encoding_trim[(entity_encoding_trim[,"ASCII.Looks.Like"] %in% c("&")),]
#   entity_encoding_trim_no_amp <-  entity_encoding_trim[!(entity_encoding_trim[,"ASCII.Looks.Like"] %in% c("&")),]
#   rm(entity_encoding_trim)
#   
#   #webpage_tags_decoded <- webpage_tags[,c(col_val)]
#   webpage_tags[,c(col_val_decoded)] <- webpage_tags[,c(col_val)]
#   for (i in 1:length(col_val_decoded))
#   {
#     col_temp <- col_val_decoded[i]
#     
#     for (j in 1:nrow(entity_encoding_trim_amp))
#     {
#       pattern_temp1 <- entity_encoding_trim_amp[j,"Entity.Encoding"]
#       replacement_temp1 <- entity_encoding_trim_amp[j,"ASCII.Looks.Like"]
#       webpage_tags[,col_temp] <- gsub(pattern_temp1, replacement_temp1, webpage_tags[,col_temp],ignore.case = TRUE)
#       rm(pattern_temp1,replacement_temp1)
#     } 
#     rm(j)
#     
#     for (k in 1:nrow(entity_encoding_trim_no_amp))
#     {
#       pattern_temp1 <- entity_encoding_trim_no_amp[k,"Entity.Encoding"]
#       replacement_temp1 <- entity_encoding_trim_no_amp[k,"ASCII.Looks.Like"]
#       webpage_tags[,col_temp] <- gsub(pattern_temp1, replacement_temp1, webpage_tags[,col_temp],ignore.case = TRUE)
#       rm(pattern_temp1,replacement_temp1)
#     } 
#     rm(col_temp,k)
#     
#   } 
#   rm(i)
#   
#   #webpage_tags_encoded <- webpage_tags_decoded
#   webpage_tags[,c(col_val_encoded)] <- webpage_tags[,c(col_val_decoded)]
#   for (i in 1:length(col_val_encoded))
#   {
#     col_temp <- col_val_encoded[i]
#     
#     #grep("&",webpage_tags[,col_temp])
#     #aa_test <- webpage_tags[grep("&",webpage_tags[,col_temp]),col_temp]
#     
#     for (j in 1:nrow(entity_encoding_trim_amp))
#     {
#       pattern_temp2 <- entity_encoding_trim_amp[j,"ASCII.Looks.Like"]
#       replacement_temp2 <- entity_encoding_trim_amp[j,"Entity.Encoding"]
#       replacement_temp2_trim <- gsub("&","",replacement_temp2)
#       replacement_temp2_trim <- gsub(" {2,}", " ", replacement_temp2_trim)
#       replacement_temp2_trim <- gsub("^\\s+|\\s+$", "", replacement_temp2_trim)
#       webpage_tags[,col_temp] <- gsub(pattern_temp2, replacement_temp2_trim, webpage_tags[,col_temp],ignore.case = FALSE)
#       webpage_tags[,col_temp] <- gsub(replacement_temp2_trim, replacement_temp2, webpage_tags[,col_temp],ignore.case = FALSE)
#       rm(pattern_temp2,replacement_temp2,replacement_temp2_trim)
#     } 
#     rm(j)
#     
#     for (k in 1:nrow(entity_encoding_trim_no_amp))
#     {
#       pattern_temp2 <- entity_encoding_trim_no_amp[k,"ASCII.Looks.Like"]
#       replacement_temp2 <- entity_encoding_trim_no_amp[k,"Entity.Encoding"]
#       replacement_temp2_trim <- gsub("&","",replacement_temp2)
#       replacement_temp2_trim <- gsub(" {2,}", " ", replacement_temp2_trim)
#       replacement_temp2_trim <- gsub("^\\s+|\\s+$", "", replacement_temp2_trim)
#       webpage_tags[,col_temp] <- gsub(pattern_temp2, replacement_temp2_trim, webpage_tags[,col_temp],ignore.case = FALSE)
#       webpage_tags[,col_temp] <- gsub(replacement_temp2_trim, replacement_temp2, webpage_tags[,col_temp],ignore.case = FALSE)
#       rm(pattern_temp2,replacement_temp2,replacement_temp2_trim)  
#     } 
#     rm(col_temp,k)
#     
#   } 
#   rm(i)
#   rm(entity_encoding_trim_amp,entity_encoding_trim_no_amp)
#   
#   
#   #Clean Encoded Values
#   for(i in which(sapply(webpage_tags,class)=="character"))
#   {
#     webpage_tags[[i]] <- gsub(" {2,}", " ", webpage_tags[[i]])
#     webpage_tags[[i]] <- trim(webpage_tags[[i]])
#   }
#   rm(i)
#   
#   for (i in 1:ncol(webpage_tags))
#   {
#     webpage_tags[,i] <- unknownToNA(webpage_tags[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
#                                                                 NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
#                                                                 NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
#     webpage_tags[,i] <- ifelse(is.na(webpage_tags[,i]),"", webpage_tags[,i])
#   } 
#   rm(i)
#   
#   
#   #Collapse Tags and Values
#   tags_collapse2 <- adply(.data=webpage_tags, .margins=1, .fun = function(x,datacols) {
#     
#     #x <- webpage_tags[1,]
#     #x <- webpage_tags[108,]
#     
#     data_temp <- x[,datacols]
#     data_temp <- gsub(" {2,}", " ", data_temp)
#     #data_temp <- gsub(" ", "", data_temp)
#     data_temp <- gsub("^\\s+|\\s+$", "", data_temp)
#     data_temp_trim <- data_temp[!(data_temp=="")] 
#     #data_temp_tags <- ifelse(length(data_temp_trim)==0,"",paste("<",data_temp_trim,">",sep="",collapse=""))
#     data_temp_tags <- ifelse(length(data_temp_trim)==0,"",paste("",data_temp_trim,"",sep="",collapse=""))
#     data_temp_tags <- gsub(" {2,}", " ", data_temp_tags)
#     data_temp_tags <- gsub("^\\s+|\\s+$", "", data_temp_tags)
#     
#   }, datacols=sort(c(col_tag,col_val_encoded)), .expand = FALSE, .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL)
#   colnames(tags_collapse2) <- c("id","raw_encoded")
#   
#   webpage_tags[,"raw_encoded"] <- tags_collapse2[,"raw_encoded"]
#   
#   #   #Create raw encoded tags
#   #   webpage_tags[,"raw_encoded"] <- ifelse(is.na(webpage_tags[,"value"]),
#   #                                          webpage_tags[,"tag"], 
#   #                                          paste(webpage_tags[,"tag"],webpage_tags[,"value"],sep=""))
#   #   webpage_tags[,"raw_encoded"] <- ifelse(is.na(webpage_tags[,"tag"]),
#   #                                          webpage_tags[,"value"], 
#   #                                          webpage_tags[,"raw_encoded"])
#   
#   #Creat final tags
#   webpage_tags[,"Final_tag"] <- ifelse(webpage_tags[,"Good"]==0, 
#                                        paste(webpage_tags[,"raw_encoded"],"</",webpage_tags[,"col01_tag_first_legit"],">",sep=""), 
#                                        webpage_tags[,"raw_encoded"])
#   webpage_tags[,"Final_tag"] <- ifelse(webpage_tags[,"Good"]=="", 
#                                        webpage_tags[,"raw_encoded"], 
#                                        webpage_tags[,"Final_tag"])
#   
#   webpage_tags_comb_list <- list(webpage_tags,raw_tags_short1)
#   
#   return(webpage_tags_comb_list)
#   
# }
# 
# 
# expand_edgar_tags <- function(file,html_tags,sec_tags){
#   
#   #file <- webpage_tags_clean[,"raw"]
#   #html_tags <- html_tags
#   #sec_tags <- sec_tags
#   
#   require(gdata)
#   require(plyr)
#   
#   #non_ending_tags <- c("S","C")
#   non_ending_tags_html <- html_tags[html_tags[,"IGNORE_CLOSING"]=="Yes","TAG_SHORT"]
#   non_ending_tags_sec <- sec_tags[sec_tags[,"IGNORE_CLOSING"]=="Yes","TAG_SHORT"]
#   non_ending_tags <- c(non_ending_tags_html,non_ending_tags_sec)
#   non_ending_tags <- gsub(" {2,}", " ", non_ending_tags)
#   non_ending_tags <- gsub("^\\s+|\\s+$", "", non_ending_tags)
#   non_ending_tags <- non_ending_tags[!is.na(non_ending_tags)]
#   non_ending_tags <- unique(non_ending_tags)
#   
#   html_tags_open <- html_tags[,c("START_TAG")]
#   html_tags_open <- gsub("<","",html_tags_open)
#   html_tags_open <- gsub(">","",html_tags_open)
#   html_tags_open <- gsub(" {2,}", " ", html_tags_open)
#   html_tags_open <- gsub("^\\s+|\\s+$", "", html_tags_open)
#   html_tags_open <- html_tags_open[!is.na(html_tags_open)]
#   html_tags_open <- unique(html_tags_open)
#   
#   html_tags_close <- html_tags[,c("END_TAG")]
#   html_tags_close <- gsub("<","",html_tags_close)
#   html_tags_close <- gsub(">","",html_tags_close)
#   html_tags_close <- gsub(" {2,}", " ", html_tags_close)
#   html_tags_close <- gsub("^\\s+|\\s+$", "", html_tags_close)
#   html_tags_close <- html_tags_close[!is.na(html_tags_close)]
#   html_tags_close <- unique(html_tags_close)
#   
#   html_tags_comb <- c(html_tags_open,html_tags_close)
#   
#   sec_tags_open <- sec_tags[,c("START_TAG")]
#   sec_tags_open <- gsub("<","",sec_tags_open)
#   sec_tags_open <- gsub(">","",sec_tags_open)
#   sec_tags_open <- gsub(" {2,}", " ", sec_tags_open)
#   sec_tags_open <- gsub("^\\s+|\\s+$", "", sec_tags_open)
#   sec_tags_open <- sec_tags_open[!is.na(sec_tags_open)]
#   sec_tags_open <- unique(sec_tags_open)
#   
#   sec_tags_close <- sec_tags[,c("END_TAG")]
#   sec_tags_close <- gsub("<","",sec_tags_close)
#   sec_tags_close <- gsub(">","",sec_tags_close)
#   sec_tags_close <- gsub(" {2,}", " ", sec_tags_close)
#   sec_tags_close <- gsub("^\\s+|\\s+$", "", sec_tags_close)
#   sec_tags_close <- sec_tags_close[!is.na(sec_tags_close)]
#   sec_tags_close <- unique(sec_tags_close)
#   
#   sec_tags_comb <- c(sec_tags_open,sec_tags_close)
#   
#   #Seperate tags and values
#   webpage_tags0 <- data.frame(id=NA,
#                               raw=file,
#                               stringsAsFactors=FALSE)
#   webpage_tags0[,"id"] <- seq(1,nrow(webpage_tags0))
#   
#   #Seperate tags and values
#   #webpage_tags0[,"tag"] <-  gsub(".*?<(.*?)>.*", "\\1", webpage_tags0[,"raw"]) 
#   
#   webpage_tags0_expand_tags0 <- dlply(.data=webpage_tags0, .variables="id", .fun = function(x,datacol,bycol,regex){
#     
#     matches <- regmatches(x[,datacol], gregexpr(regex, x[,datacol], perl=TRUE))[[1]]
#     matches_df <- data.frame(t(matches),stringsAsFactors=FALSE)
#     
#   }, datacol="raw",bycol="id",regex="(?<=\\<).*?(?=\\>)", .progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
#   webpage_tags0_expand_tags <- do.call(rbind.fill,webpage_tags0_expand_tags0)
#   colnames(webpage_tags0_expand_tags) <- paste("col",sprintf("%02d", seq(1,ncol(webpage_tags0_expand_tags))),"_tag",sep="")
#   rm(webpage_tags0_expand_tags0)
#   
#   webpage_tags0_expand_vals_beg0 <- dlply(.data=webpage_tags0, .variables="id", .fun = function(x,datacol,bycol,regex){
#     
#     # x <- webpage_tags0[1629,]
#     # datacol <- "raw"
#     # regex <- "(^(.*?)<)"
#     
#     matches <- regmatches(x[,datacol], gregexpr(regex, x[,datacol], perl=TRUE))[[1]]
#     matches <-  ifelse(substring(matches, nchar(matches), nchar(matches))=="<", substring(matches, 1, (nchar(matches)-1)), matches)
#     matches_df <- data.frame(t(matches),stringsAsFactors=FALSE)
#     
#   }, datacol="raw",bycol="id",regex="(^(.*?)<)", .progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
#   webpage_tags0_expand_vals_beg <- do.call(rbind.fill,webpage_tags0_expand_vals_beg0)
#   colnames(webpage_tags0_expand_vals_beg) <- paste("col00_value",sep="")
#   rm(webpage_tags0_expand_vals_beg0)
#   
#   webpage_tags0_expand_vals_mid0 <- dlply(.data=webpage_tags0, .variables="id", .fun = function(x,datacol,bycol,regex){
#     
#     matches <- regmatches(x[,datacol], gregexpr(regex, x[,datacol], perl=TRUE))[[1]]
#     matches_df <- data.frame(t(matches),stringsAsFactors=FALSE)
#     
#   }, datacol="raw",bycol="id",regex="(?<=\\>).*?(?=\\<)", .progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
#   webpage_tags0_expand_vals_mid <- do.call(rbind.fill,webpage_tags0_expand_vals_mid0)
#   rm(webpage_tags0_expand_vals_mid0)
#   
#   webpage_tags0_expand_vals_end0 <- dlply(.data=webpage_tags0, .variables="id", .fun = function(x,datacol,bycol,regex){
#     
#     matches <- regmatches(x[,datacol], gregexpr(regex, x[,datacol], perl=TRUE))[[1]]
#     matches_df <- data.frame(t(matches),stringsAsFactors=FALSE)
#     
#   }, datacol="raw",bycol="id",regex="([^>]*)$", .progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
#   webpage_tags0_expand_vals_end <- do.call(rbind.fill,webpage_tags0_expand_vals_end0)
#   rm(webpage_tags0_expand_vals_end0)
#   
#   webpage_tags0_expand_vals0 <- cbind(webpage_tags0_expand_vals_mid,webpage_tags0_expand_vals_end)
#   colnames(webpage_tags0_expand_vals0) <- paste("col",  sprintf("%02d", seq(1,ncol(webpage_tags0_expand_vals0))),"_value",sep="")
#   rm(webpage_tags0_expand_vals_mid,webpage_tags0_expand_vals_end)
#   
#   webpage_tags0_expand_vals <- cbind(webpage_tags0_expand_vals_beg,webpage_tags0_expand_vals0)
#   rm(webpage_tags0_expand_vals_beg,webpage_tags0_expand_vals0)
#   
#   #Clean Tag Columns
#   for(i in which(sapply(webpage_tags0_expand_tags,class)=="character"))
#   {
#     webpage_tags0_expand_tags[[i]] <- gsub(" {2,}", " ", webpage_tags0_expand_tags[[i]])
#     webpage_tags0_expand_tags[[i]] <- trim(webpage_tags0_expand_tags[[i]])
#   }
#   rm(i)
#   
#   for (i in 1:ncol(webpage_tags0_expand_tags))
#   {
#     webpage_tags0_expand_tags[,i] <- unknownToNA(webpage_tags0_expand_tags[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
#                                                                                           NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
#                                                                                           NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
#     webpage_tags0_expand_tags[,i] <- ifelse(is.na(webpage_tags0_expand_tags[,i]),"", webpage_tags0_expand_tags[,i])
#   } 
#   rm(i)
#   
#   #Clean Value Columns
#   for(i in which(sapply(webpage_tags0_expand_vals,class)=="character"))
#   {
#     webpage_tags0_expand_vals[[i]] <- gsub(" {2,}", " ", webpage_tags0_expand_vals[[i]])
#     webpage_tags0_expand_vals[[i]] <- trim(webpage_tags0_expand_vals[[i]])
#   }
#   rm(i)
#   
#   for (i in 1:ncol(webpage_tags0_expand_vals))
#   {
#     webpage_tags0_expand_vals[,i] <- unknownToNA(webpage_tags0_expand_vals[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
#                                                                                           NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
#                                                                                           NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
#     webpage_tags0_expand_vals[,i] <- ifelse(is.na(webpage_tags0_expand_vals[,i]),"", webpage_tags0_expand_vals[,i])
#   } 
#   rm(i)
#   
#   
#   #Make sure only getting beginning of tag
#   webpage_tags0_expand_tags_first <- webpage_tags0_expand_tags
#   colnames(webpage_tags0_expand_tags_first) <- paste("col", sprintf("%02d", seq(1,ncol(webpage_tags0_expand_tags_first))),"_tag_first",sep="")
#   for (i in 1:ncol(webpage_tags0_expand_tags_first))
#   {
#     #i <- 1
#     webpage_tags0_expand_tags_first[,i] <- ifelse((grepl(" ", webpage_tags0_expand_tags_first[,i])), gsub( " .*$", "", webpage_tags0_expand_tags_first[,i]), webpage_tags0_expand_tags_first[,i])
#     webpage_tags0_expand_tags_first[,i] <- gsub(" {2,}", " ", webpage_tags0_expand_tags_first[,i])
#     webpage_tags0_expand_tags_first[,i] <- gsub("^\\s+|\\s+$", "", webpage_tags0_expand_tags_first[,i])
#     #webpage_tags0_expand_tags_first[,i] <-  ifelse(is.na(webpage_tags0_expand_tags_first[,"tag"]), NA, webpage_tags0_expand_tags_first[,"tag_first_word"])
#     #webpage_tags0_expand_tags_first[,i] <-  ifelse(webpage_tags0_expand_tags_first[,i]=="", "", webpage_tags0_expand_tags_first[,i])
#     
#   } 
#   
#   #Make sure tags are legit
#   webpage_tags0_expand_tags_first_legit <- webpage_tags0_expand_tags_first
#   colnames(webpage_tags0_expand_tags_first_legit) <- paste("col", sprintf("%02d", seq(1,ncol(webpage_tags0_expand_tags_first_legit))),"_tag_first_legit",sep="")
#   for (i in 1:ncol(webpage_tags0_expand_tags_first_legit))
#   {
#     #i <- 1
#     webpage_tags0_expand_tags_first_legit[,i] <- ifelse(webpage_tags0_expand_tags_first_legit[,i] %in% c(html_tags_comb,sec_tags_comb), webpage_tags0_expand_tags_first_legit[,i], "")
#   } 
#   
#   #Remove closing tag from tag_short column
#   webpage_tags0_expand_tags_short <- webpage_tags0_expand_tags_first_legit
#   colnames(webpage_tags0_expand_tags_short) <- paste("col", sprintf("%02d", seq(1,ncol(webpage_tags0_expand_tags_short))),"_tag_short",sep="")
#   for (i in 1:ncol(webpage_tags0_expand_tags_short))
#   {
#     #i <- 1
#     webpage_tags0_expand_tags_short[,i] <- gsub("/", "", webpage_tags0_expand_tags_short[,i])
#     webpage_tags0_expand_tags_short[,i] <- gsub(" {2,}", " ", webpage_tags0_expand_tags_short[,i])
#     webpage_tags0_expand_tags_short[,i] <- gsub("^\\s+|\\s+$", "", webpage_tags0_expand_tags_short[,i])
#     
#   } 
#   
#   #Determine if open/close tag
#   webpage_tags0_expand_tags_type_oc <- webpage_tags0_expand_tags_first_legit
#   colnames(webpage_tags0_expand_tags_type_oc) <- paste("col", sprintf("%02d", seq(1,ncol(webpage_tags0_expand_tags_type_oc))),"_tag_type_oc",sep="")
#   for (i in 1:ncol(webpage_tags0_expand_tags_type_oc))
#   {
#     #i <- 1
#     #webpage_tags0_expand_tags_type_oc[,i] <-  ifelse(grepl("/", webpage_tags0_expand_tags_type_oc[,i]), "close", "open")
#     #webpage_tags0_expand_tags_type_oc[,i] <-  ifelse(substring(webpage_tags0_expand_tags_type_oc[,i], 1, 1)=="/", "close", "open")
#     webpage_tags0_expand_tags_type_oc[,i] <-  ifelse(webpage_tags0_expand_tags_type_oc[,i]=="", webpage_tags0_expand_tags_type_oc[,i],
#                                                      ifelse(substring(webpage_tags0_expand_tags_type_oc[,i], 1, 1)=="/", "close", "open"))
#     
#   } 
#   
#   #Determine if sec/html tag
#   webpage_tags0_expand_tags_type_sh <- webpage_tags0_expand_tags_first_legit
#   colnames(webpage_tags0_expand_tags_type_sh) <- paste("col", sprintf("%02d", seq(1,ncol(webpage_tags0_expand_tags_type_sh))),"_tag_type_sh",sep="")
#   for (i in 1:ncol(webpage_tags0_expand_tags_type_sh))
#   {
#     #i <- 1
#     #webpage_tags0_expand_tags_type_sh[,i] <-  ifelse(grepl("/", webpage_tags0_expand_tags_type_sh[,i]), "close", "open")
#     #webpage_tags0_expand_tags_type_sh[,i] <-  ifelse(substring(webpage_tags0_expand_tags_type_sh[,i], 1, 1)=="/", "close", "open")
#     webpage_tags0_expand_tags_type_sh[,i] <-  ifelse(webpage_tags0_expand_tags_type_sh[,i]=="", webpage_tags0_expand_tags_type_sh[,i],
#                                                      ifelse(webpage_tags0_expand_tags_type_sh[,i] %in% c(html_tags_comb), "html", 
#                                                             ifelse(webpage_tags0_expand_tags_type_sh[,i] %in% c(sec_tags_comb), "sec","ERROR")))
#   } 
#   
#   webpage_tags0_expand <- do.call(cbind, list(webpage_tags0_expand_tags,webpage_tags0_expand_tags_first,webpage_tags0_expand_tags_first_legit,
#                                               webpage_tags0_expand_tags_short,webpage_tags0_expand_tags_type_oc,webpage_tags0_expand_tags_type_sh,
#                                               webpage_tags0_expand_vals))
#   webpage_tags0_expand <- webpage_tags0_expand[,sort(colnames(webpage_tags0_expand))]
#   rm(webpage_tags0_expand_tags,webpage_tags0_expand_tags_first,webpage_tags0_expand_tags_first_legit)
#   rm(webpage_tags0_expand_tags_short,webpage_tags0_expand_tags_type_oc,webpage_tags0_expand_tags_type_sh)
#   rm(webpage_tags0_expand_vals)
#   
#   return(webpage_tags0_expand)
#   
# }
# 
# 
# extract_filing_section_by_drop_no_xml <- function(x,xml_col,tag_col,index_col,sub_index_col,index_flag,file){
#   
#   #xml_col="Final_tag"
#   #tag_col="tag_short"
#   
#   #index_col=document_index_val
#   #sub_index_col=document_filing_text_sub_index_val
#   #index_flag=document_index_flag
#   #file=file
#   #x <- document_data_temp[document_data_temp[,document_index_val2]==1,]
#   #x <- document_data_temp[document_data_temp[,document_index_val2]==2,]
#   #x <- document_data_temp[document_data_temp[,document_index_val2]==3,]
#   #x <- document_data_temp[document_data_temp[,document_index_val2]==4,]
#   
#   require(XML)
#   
#   extract_filing_section_by_drop_sub_no_xml <- function(x,xml_col,index_col_sub,sub_index_col_sub){
#     
#     # xml_col <- xml_col
#     # index_col_sub <- index_col2
#     # sub_index_col_sub <- sub_index_col2
#     # x <- x
#     
#     xml_col_num <- which(colnames(x)==xml_col)
#     index_col_num <- which(colnames(x)==index_col_sub)
#     sub_index_col_num <-  unlist(lapply(sub_index_col_sub, function(y,cols){ which(cols==y) },cols=colnames(x)))
#     sub_index_col_num_max <- max(sub_index_col_num)
#     
#     x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
#     
#     keep_rows <- lapply(sub_index_col_sub, function(y,data){
#       
#       #y <- "FILER_INDEX"
#       #y <- "SERIES_AND_CLASSES_CONTRACTS_DATA_INDEX"
#       #data <- x_trim
#       
#       if(y %in% colnames(data)) {
#         
#         return(which(data[,y]!=0))
#         
#       } else {
#         
#         return(NA)
#         
#       }
#       
#     },data=x_trim)
#     keep_rows <- sort(unique(unlist(keep_rows)))
#     drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
#     
#     x_trim2 <- x_trim[c(1,drop_rows,nrow(x_trim)),]
#     
#     #temp_df <- xmlToDataFrame(x_trim2[,xml_col])
#     temp_df <- x_trim2[,xml_col]
#     
#     return(temp_df)
#   }
#   index_col2 <- paste(index_col,"INDEX",sep="_")
#   sub_index_col2 <- paste(sub_index_col,"INDEX",sep="_")
#   
#   index_val2 <- unique(x[,index_col2])
#   
#   flag <- (sub_index_col %in% x[,tag_col])
#   
#   if(any(flag)) {
#     
#     merge0 <- data.frame(file=file,index_col=index_val2,stringsAsFactors=FALSE)
#     colnames(merge0) <- c("file",index_col2)
#  
#     merge1 <- extract_filing_section_by_drop_sub_no_xml(x,xml_col,index_col2,sub_index_col2)
#   
#     merge <- data.frame(merge0,merge1,stringsAsFactors=FALSE)
#     colnames(merge) <- c("file",index_col2,sub_index_col)
#     
#     rm(merge0,merge1)
#     
#   } else {
#   
#     merge <- data.frame(file=file,index_col=index_val2,temp_ret=NA,stringsAsFactors=FALSE)
#     colnames(merge) <- c("file",index_col2,sub_index_col)
# 
#   }
#   
#   rm(index_col2,sub_index_col2,index_val2,flag)
#   
#   return(merge)
#   
# }
# 
# 
# extract_filing_section_by_keep_no_xml <- function(x,xml_col,tag_col,index_col,sub_index_col,index_flag,file){
#   
#   #xml_col="Final_tag"
#   #tag_col="tag_short"
#   
#   #index_col=document_index_val
#   #sub_index_col=document_filing_text_sub_index_val
#   #index_flag=document_index_flag
#   #file=file
#   #x <- document_data_temp[document_data_temp[,document_index_val2]==1,]
#   #x <- document_data_temp[document_data_temp[,document_index_val2]==2,]
#   #x <- document_data_temp[document_data_temp[,document_index_val2]==3,]
#   #x <- document_data_temp[document_data_temp[,document_index_val2]==4,]
#   
#   require(XML)
#   
#   extract_filing_section_by_keep_sub_no_xml <- function(x,xml_col,index_col_sub,sub_index_col_sub){
#     
#     # xml_col <- xml_col
#     # index_col_sub <- index_col2
#     # sub_index_col_sub <- sub_index_col2
#     # x <- x
#     
#     xml_col_num <- which(colnames(x)==xml_col)
#     index_col_num <- which(colnames(x)==index_col_sub)
#     sub_index_col_num <-  unlist(lapply(sub_index_col_sub, function(y,cols){ which(cols==y) },cols=colnames(x)))
#     sub_index_col_num_max <- max(sub_index_col_num)
#     
#     x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
#     
#     keep_rows <- lapply(sub_index_col_sub, function(y,data){
#       
#       #y <- "FILER_INDEX"
#       #y <- "SERIES_AND_CLASSES_CONTRACTS_DATA_INDEX"
#       #y <- "DOCUMENT_INDEX"
#       #y <- "TEXT_INDEX"
#       #data <- x_trim
#       
#       if(y %in% colnames(data)) {
#         
#         return(which(data[,y]!=0))
#         
#       } else {
#         
#         return(NA)
#         
#       }
#       
#     },data=x_trim)
#     keep_rows <- sort(unique(unlist(keep_rows)))
#     drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
#     
#     x_trim2 <- x_trim[c(1,keep_rows,nrow(x_trim)),]
#     
#     #temp_df <- xmlToDataFrame(x_trim2[,xml_col])
#     temp_df <- x_trim2[,xml_col]
#     
#     return(temp_df)
#   }
#   
#   index_col2 <- paste(index_col,"INDEX",sep="_")
#   sub_index_col2 <- paste(sub_index_col,"INDEX",sep="_")
#   
#   index_val2 <- unique(x[,index_col2])
#   
#   flag <- (sub_index_col %in% x[,tag_col])
#   
#   if(any(flag)) {
#     
#     merge0 <- data.frame(file=file,index_col=index_val2,stringsAsFactors=FALSE)
#     colnames(merge0) <- c("file",index_col2)
#     
#     merge1 <- extract_filing_section_by_keep_sub_no_xml(x,xml_col,index_col2,sub_index_col2)
#     
#     merge <- data.frame(merge0,merge1,stringsAsFactors=FALSE)
#     colnames(merge) <- c("file",index_col2,sub_index_col)
#     rm(merge0,merge1)
#     
#   } else {
#     
#     merge <- data.frame(file=file,index_col=index_val2,temp_ret=NA,stringsAsFactors=FALSE)
#     colnames(merge) <- c("file",index_col2,sub_index_col)
#     
#   }
#   
#   rm(index_col2,sub_index_col2,index_val2,flag)
#   
#   return(merge)
#   
# }


# 
# extract_filing_section_by_drop <- function(x,xml_col,tag_col,index_col,sub_index_col,index_flag){
#   
#   #xml_col="Final_tag"
#   #tag_col="tag_short"
#   #index_col=filer_index_val
#   #sub_index_col=filer_former_company_sub_index_val
#   #index_flag <- filer_index_flag
#   #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==1,]
#   #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==2,]
#   #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==3,]
#   #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==4,]
#   
#   require(XML)
#   
#   extract_filing_section_by_drop_sub <- function(x,xml_col,index_col,sub_index_col){
#     
#     # filer_index_val <- "FILER"
#     # filer_company_data_sub_index_val <- "COMPANY_DATA"
#     
#     # xml_col <- "Final_tag"
#     # index_col <- paste(filer_index_val,"INDEX",sep="_")
#     # sub_index_col <- paste(filer_company_data_sub_index_val,"INDEX",sep="_")
#     # x <- filer_data_temp[filer_data_temp[,index_col]==1,]
#     
#     require(XML)
#     
#     xml_col_num <- which(colnames(x)==xml_col)
#     index_col_num <- which(colnames(x)==index_col)
#     sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
#     sub_index_col_num_max <- max(sub_index_col_num)
#     
#     x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
#     
#     keep_rows <- lapply(sub_index_col, function(y,data){
#       
#       #y <- "FILER_INDEX"
#       #y <- "SERIES_AND_CLASSES_CONTRACTS_DATA_INDEX"
#       #data <- x_trim
#       
#       if(y %in% colnames(data)) {
#         
#         return(which(data[,y]!=0))
#         
#       } else {
#         
#         return(NA)
#         
#       }
#       
#     },data=x_trim)
#     keep_rows <- sort(unique(unlist(keep_rows)))
#     drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
#     
#     x_trim2 <- x_trim[c(1,drop_rows,nrow(x_trim)),]
#     
#     temp_df <- xmlToDataFrame(x_trim2[,xml_col])
#     
#     return(temp_df)
#   }
#   index_col2 <- paste(index_col,"INDEX",sep="_")
#   sub_index_col2 <- paste(sub_index_col,"INDEX",sep="_")
#   
#   index_val2 <- unique(x[,index_col2])
#   
#   filer_former_company_flag <- (sub_index_col %in% x[,tag_col])
#   
#   if(filer_former_company_flag) {
#     
#     filer_former_company0 <- data.frame(file=file,index_col=index_val2,stringsAsFactors=FALSE)
#     colnames(filer_former_company0) <- c("file",index_col2)
#     
#     filer_former_company1 <- extract_filing_section_by_keep_sub(x,xml_col,index_col2,sub_index_col2)
#     filer_former_company <- data.frame(filer_former_company0,filer_former_company1,stringsAsFactors=FALSE)
#     
#     rm(filer_former_company0,filer_former_company1)
#     
#   } else {
#     
#     filer_former_company <- data.frame(file=file,index_col=index_val2,stringsAsFactors=FALSE)
#     colnames(filer_former_company) <- c("file",index_col2)
#     
#   }
#   
#   rm(index_col2,sub_index_col2,index_val2,filer_former_company_flag)
#   
#   return(filer_former_company)
#   
# }
# 
# 
# extract_filing_section_by_keep <- function(x,xml_col,tag_col,index_col,sub_index_col,index_flag){
#   
#   #xml_col="Final_tag"
#   #tag_col="tag_short"
#   #index_col=filer_index_val
#   #sub_index_col=filer_former_company_sub_index_val
#   #index_flag <- filer_index_flag
#   #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==1,]
#   #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==2,]
#   #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==3,]
#   #x <- filer_data_temp[filer_data_temp[,filer_index_val2]==4,]
#   
#   require(XML)
#   
#   extract_filing_section_by_keep_sub <- function(x,xml_col,index_col,sub_index_col){
#     
#     # xml_col <- "Final_tag"
#     # index_col <- filer_index_val2
#     # sub_index_col <- filer_former_company_sub_index_val2
#     # x <- filer_data_temp[filer_data_temp[,index_col]==1,]
#     
#     xml_col_num <- which(colnames(x)==xml_col)
#     index_col_num <- which(colnames(x)==index_col)
#     sub_index_col_num <-  unlist(lapply(sub_index_col, function(y,cols){ which(cols==y) },cols=colnames(x)))
#     sub_index_col_num_max <- max(sub_index_col_num)
#     
#     x_trim <- x[,c(xml_col_num,seq(index_col_num,sub_index_col_num_max))]
#     
#     keep_rows <- lapply(sub_index_col, function(y,data){
#       
#       #y <- "FILER_INDEX"
#       #y <- "SERIES_AND_CLASSES_CONTRACTS_DATA_INDEX"
#       #data <- x_trim
#       
#       if(y %in% colnames(data)) {
#         
#         return(which(data[,y]!=0))
#         
#       } else {
#         
#         return(NA)
#         
#       }
#       
#     },data=x_trim)
#     keep_rows <- sort(unique(unlist(keep_rows)))
#     drop_rows <- which(!(seq(1,nrow(x_trim)) %in% keep_rows))
#     
#     x_trim2 <- x_trim[c(1,keep_rows,nrow(x_trim)),]
#     
#     temp_df <- xmlToDataFrame(x_trim2[,xml_col])
#     
#     return(temp_df)
#   }
#   
#   index_col2 <- paste(index_col,"INDEX",sep="_")
#   sub_index_col2 <- paste(sub_index_col,"INDEX",sep="_")
#   
#   index_val2 <- unique(x[,index_col2])
#   
#   filer_former_company_flag <- (sub_index_col %in% x[,tag_col])
#   
#   if(filer_former_company_flag) {
#     
#     filer_former_company0 <- data.frame(file=file,index_col=index_val2,stringsAsFactors=FALSE)
#     colnames(filer_former_company0) <- c("file",index_col2)
#     
#     filer_former_company1 <- extract_filing_section_by_keep_sub(x,xml_col,index_col2,sub_index_col2)
#     filer_former_company <- data.frame(filer_former_company0,filer_former_company1,stringsAsFactors=FALSE)
#     
#     rm(filer_former_company0,filer_former_company1)
#     
#   } else {
#     
#     filer_former_company <- data.frame(file=file,index_col=index_val2,stringsAsFactors=FALSE)
#     colnames(filer_former_company) <- c("file",index_col2)
#     
#   }
#   
#   rm(index_col2,sub_index_col2,index_val2,filer_former_company_flag)
#   
#   return(filer_former_company)
#   
# }