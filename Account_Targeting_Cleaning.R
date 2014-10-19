setwd('C:/Fliptop/Account_List_Top1000/')
library(stringr)

gainsight <- read.csv('Gainsight.csv', header = T, stringsAsFactor = F)
gainsight_opp <- read.csv('9018017_Gainsight_opp_nam.csv', header = T, stringsAsFactor = F)

trackmaven <- read.csv('Trackmaven.csv', header = T, stringsAsFactor = F)
trackmaven_opp <- read.csv('9017980_Trackmaven_opp_nam.csv', header = T, stringsAsFactor = F)

jitterbit <- read.csv('C:/Users/koolk_000/Desktop/Jitterbit.csv', header = T, stringsAsFactor = F)
jitterbit_opp <- read.csv('9017978_Jitterbit_opp_nam.csv', header = T, stringsAsFactor = F)

newvoicemedia <- read.csv('NewVoiceMedia.csv', header = T, stringsAsFactor = F)
newvoicemedia_opp <- read.csv('9017938_Newvoicemedia_opp_nam.csv', header = T, stringsAsFactor = F)

appboy <- read.csv('Appboy.csv', header = T, stringsAsFactor = F)
appboy_opp <- read.csv('9017943_Appboy_opp_nam.csv', header = T, stringsAsFactor = F)

ON24 <- read.csv('C:/Users/koolk_000/Desktop/ON24.csv', header = T, stringsAsFactor = F)
ON24_opp <- read.csv('9018006_ON24_opp_nam.csv', header = T, stringsAsFactor = F)

marketo <- read.csv('C:/Users/koolk_000/Desktop/Marketo.csv', header = T, stringsAsFactor = F)
marketo_opp <- read.csv('9017947_Marketo_opp_nam.csv', header = T, stringsAsFactor = F)

insideview <- read.csv('InsideView.csv', header = T, stringsAsFactor = F)
insideview_opp <- read.csv('9017976_Insideview_opp_nam.csv', header = T, stringsAsFactor = F)

#cloudpassage <- read.csv('9017610_Cloudpassage.csv', header = T, stringsAsFactor = F)
#lifestyle <- read.csv('9017958_DF_Lifestyle.csv', header = T, stringsAsFactor = F)
#medical <- read.csv('9017976_InsideView.csv', header = T, stringsAsFactor = F)
#quickbooks <- read.csv('9017968_DF_Quickbooks.csv', header = T, stringsAsFactor = F)
#health <- read.csv('9017969_DF_Health.csv', header = T, stingsAsFactor = F)
#animal <- read.csv('9017971_DF_Animal.csv', header = T, stringsAsFactor = F)

gainsight_opp_name <- list(gainsight_opp$nam)
gainsight_opp_com_name <- list(gainsight_opp$com.nam)

trackmaven_opp_name <- list(trackmaven_opp$nam)
trackmaven_opp_com_name <- list(trackmaven_opp$com.nam)

jitterbit_opp_name <- list(jitterbit_opp$nam)
jitterbit_opp_com_name <- list(jitterbit_opp$com.nam)

newvoicemedia_opp_name <- list(newvoicemedia_opp$nam)
newvoicemedia_opp_com_name <- list(newvoicemedia_opp$com.nam)

appboy_opp_name <- list(appboy_opp$nam)
appboy_opp_com_name <- list(appboy_opp$com.nam)

ON24_opp_name <- list(ON24_opp$nam)
ON24_opp_com_name <- list(ON24_opp$com.nam)

marketo_opp_name <- list(marketo_opp$nam)
marketo_opp_com_name <- list(marketo_opp$com.nam)

insideview_opp_name <- list(insideview_opp$nam)
insideview_opp_com_name <- list(insideview_opp$com.nam)


con_in_acc <- function(df, df1, df2){
  row_to_omit <- c()
  for (row in 1:nrow(df)){
    if (grepl(',,', df$nam[row])){
      df <- df[-row, ]
    }
    else if (grepl('?', df$nam[row])){
      df <- df[-row, ]
    }
    if (grepl(df$nam[row], df1)){
      row_to_omit <- c(row_to_omit, row)
    }
    else if (grepl(df$nam[row], df2)){
      row_to_omit <- c(row_to_omit, row)
    }
  }
  row_to_omit
}

row_to_omit <- c()
for (row in 1:nrow(marketo_new)){
  if (grepl(',,', marketo_new$nam[row])){
    marketo_new <- marketo_new[-row, ]
  }
  else if (grepl('?', marketo_new$nam[row])){
    marketo_new <- marketo_new[-row, ]
  }
  else if (grepl(marketo_new$nam[row], marketo_opp_name)){
    print (grepl(marketo_new$nam[row], marketo_opp_name))
    print (row)
    row_to_omit <- c(row_to_omit, row)
  }
  else if (grepl(marketo_new$nam[row], marketo_opp_com_name)){
    print (grepl(marketo_new$nam[row], marketo_opp_com_name))
    print (row)
    row_to_omit <- c(row_to_omit, row)
  }
}


remove_columns <- function(df){
  fields_to_remove <- c()
  for (name in colnames(df)){
    if (grepl('X_id', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('own', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('ssv2', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('ssv3_0_', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('ssv3_1_', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('ssv3_2_', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('ssv3_100_', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('ssv3_100100_', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('ssv3_100001_dat', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('ssv3_100002_dat', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('rec', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('lis', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('ssv3_exists', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('exp_at', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('own_id', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('cre', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
    if (grepl('serialVersionUID', name)){
      fields_to_remove <- c(fields_to_remove, name)
    }
  }
  fields_to_remove
}

gainsight_new <- gainsight[, !(names(gainsight) %in% remove_columns(gainsight))]
trackmaven_new <- trackmaven[, !(names(trackmaven) %in% remove_columns(trackmaven))]
jitterbit_new <- jitterbit[, !(names(jitterbit) %in% remove_columns(jitterbit))]
newvoicemedia_new <- newvoicemedia[, !(names(newvoicemedia) %in% remove_columns(newvoicemedia))]
appboy_new <- appboy[, !(names(appboy) %in% remove_columns(appboy))]
ON24_new <- ON24[, !(names(ON24) %in% remove_columns(ON24))]
marketo_new <- marketo[, !(names(marketo) %in% remove_columns(marketo))]
insideview_new <- insideview[, !(names(insideview) %in% remove_columns(insideview))]

marketo_new$nam <- str_replace_all(marketo_new$nam, "[[:punct:]]", " ")
appboy_new$nam <- str_replace_all(appboy_new$nam, "[[:punct:]]", " ")
appboy_new <- appboy_new[-109, ]
ON24_new$nam <- str_replace_all(ON24_new$nam, "[[:punct:]]", " ")
#marketo_new$nam <- str_replace_all(marketo_new$nam, "[[:punct:]]", " ")



row_to_omit <- c()
for (row in 1:nrow(marketo_new)){
  if (grepl(marketo_new$nam[row], marketo_opp_name)){
    print (row)
    row_to_omit <- c(row_to_omit, row)
  }
  else if (grepl(marketo_new$nam[row], marketo_opp_com_name)){
    print (row)
    row_to_omit <- c(row_to_omit, row)
  }
}
marketo_opp_removed <- marketo_new[-row_to_omit, ]

row_to_omit <- c()
for (row in 1:nrow(appboy_new)){
  if (grepl(appboy_new$nam[row], appboy_opp_name)){
    print (row)
    row_to_omit <- c(row_to_omit, row)
  }
  else if (grepl(appboy_new$nam[row], appboy_opp_com_name)){
    print (row)
    row_to_omit <- c(row_to_omit, row)
  }
}
appboy_opp_removed <- appboy_new[-row_to_omit, ]

row_to_omit <- c()
for (row in 1:nrow(ON24_new)){
  if (grepl(ON24_new$nam[row], ON24_opp_name)){
    print (row)
    row_to_omit <- c(row_to_omit, row)
  }
  else if (grepl(ON24_new$nam[row], ON24_opp_com_name)){
    print (row)
    row_to_omit <- c(row_to_omit, row)
  }
}
ON24_opp_removed <- ON24_new[-row_to_omit, ]

gainsight_opp_removed <- gainsight_new[-con_in_acc(gainsight_new, gainsight_opp_com_name, gainsight_opp_name), ]
trackmaven_opp_removed <- trackmaven_new[-con_in_acc(trackmaven_new, trackmaven_opp_com_name, trackmaven_opp_name), ]
jitterbit_opp_removed <- jitterbit_new[-con_in_acc(jitterbit_new, jitterbit_opp_com_name, jitterbit_opp_name), ]
newvoicemedia_opp_removed <- newvoicemedia_new[-con_in_acc(newvoicemedia_new, newvoicemedia_opp_com_name, newvoicemedia_opp_name), ]

appboy_new <- appboy_new[!duplicated(appboy_new$nam),]
appboy_opp_removed <- appboy_new[-con_in_acc_other(appboy_new, appboy_opp_com_name, appboy_opp_name), ]
ON24_opp_removed <- ON24_new[-con_in_acc(ON24_new, ON24_opp_com_name, ON24_opp_name), ]
marketo_opp_removed <- marketo_new[-con_in_acc(marketo_new, marketo_opp_com_name, marketo_opp_name), ]
insideview_opp_removed <- insideview_new[-con_in_acc_other(insideview_new, insideview_opp_com_name, insideview_opp_name), ]

gainsight_opp_removed <- gainsight_opp_removed[1:1000, ]

insideview_opp_removed <- insideview_opp_removed[!duplicated(insideview_opp_removed$nam),]
insideview_opp_removed2 <- insideview_opp_removed[1:1000, ]

newvoicemedia_opp_removed <- newvoicemedia_opp_removed[1:1000, ]


write.csv(gainsight_opp_removed, 'Gainsight_cleaned.csv')
write.csv(trackmaven_opp_removed, 'Trackmaven_cleaned.csv')
write.csv(jitterbit_opp_removed, 'Jitterbit_cleaned.csv')
write.csv(newvoicemedia_opp_removed, 'NewVoiceMedia_cleaned.csv')
write.csv(appboy_opp_removed, 'Appboy_cleaned.csv')
write.csv(ON24_opp_removed, 'ON24_cleaned.csv')
write.csv(marketo_opp_removed, 'Marketo_cleaned.csv')
write.csv(insideview_opp_removed2, 'InsideView_cleaned.csv')


grepl(',,', list(appboy_new$nam))
