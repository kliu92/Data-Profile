library(plyr)


setwd('/Fliptop/')

total_list <- read.csv('/Fliptop/Unique_Accounts_list.csv', header = T, stringsAsFactor = F)

#append source name to a new column (X$source) for each source file e.g.

marketo$Source_name <- 'Marketo'

#then search to see if the company name is in the list of unique company names of Marketo customers grepl(name, list)
#then rbind.fill all the files together (let total_list be the combined of all datasets or of individual data sets)
#if combined then do a if clause to check if source is marketo

list1 <- marketo$Unique_ID
list2 <- c()
for (id in total_list$Unique_ID){
  if (total_list[total_list$Unique_ID == id, 'Source_name'] == 'Marketo'){
    list2 <- rbind.fill(list2, total_list[total_list$Unique_ID == id, ])
  }
  else if (grepl(id in list1))
    list2 <- rbind.fill(list2, total_list[total_list$Unique_ID == id, ])
}  


#select all the field you wish to show in a list (list_col) and then use

company_list <- x[, (names(x)) %in% list_col]

#to get rid of duplicates in PG_Company use

unique_company <- company_list[!duplicated(company_list$PG_Company),]

  
list_col <- c()
for (name in colnames(x)){
 if (grepl('InboundScore', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('DNB', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('Label', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('X', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('Fliptop', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('Lead', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('Unique', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('Indeed', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('Hygiene', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('Business.', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('Marketing.', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('Data.Quality', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('Personal', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('Marketo', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('Web', name)){
   list_col <- c(list_col, name)
 }
 else if (grepl('WB_is_live', name)){
   list_col <- c(list_col, name)
 }
}

company_list <- x[, !(names(x)) %in% list_col]

unique_company <- company_list[!duplicated(company_list$PG_Company),]



