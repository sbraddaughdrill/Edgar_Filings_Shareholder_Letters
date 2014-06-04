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