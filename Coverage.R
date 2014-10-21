setwd('/Users/Fliptop/EXTRACT_ALL/Report_Test')


options(digits=2)

#So far I have configured it to the point where there are 2 manual sections in generating the full data coverage sheet.
#The first is reading the file into the code and adding it to the list of collective data
# 1 ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
dmfc <- read.csv('import_910331_implicity_training.csv', header=T,stringsAsFactor=F)
dataSift <- read.csv('import_911240_implicity_training.csv', header=T,stringsAsFactor=F)
rktf <- read.csv('import_911395_implicity_training.csv', header=T,stringsAsFactor=F)
xngt <- read.csv('import_911421_implicity_training.csv', header=T,stringsAsFactor=F)
insv <- read.csv('/Users/Fliptop/Downloads/data_science_import_911577_explicitly_3cl_training.csv', header=T, stringsAsFactor=F)
#mkto <- read.csv('/Users/Fliptop/IdeaProjects/Classification/stuff2/lead_activity_features_9017947_OUT_test.csv', header=T, stringsAsFactor=F)
mkto <- read.csv('/Users/Fliptop/Downloads/data_science_import_911388_explicitly_3cl_training.csv', header=T, stringsAsFactor=F)
asana3 <- read.csv('/Users/Fliptop/EXTRACT_ALL/import_911655_explicitly_training.csv', header=T, stringsAsFactor=F)
gainsight <- read.csv('/Users/Fliptop/Downloads/data_science_import_911703_explicitly_3cl_training.csv', header=T,stringsAsFactor=F)
Zenefits2cl <- read.csv('/Users/Fliptop/Downloads/import_911555_explicitly_training.csv', header=T, stringsAsFactor=F)
#test_hashed <- read.csv('/Users/Fliptop/Downloads/test_hashed.csv', header = T, stringsAsFactor = F)
#train_hashed <- read.csv('/Users/Fliptop/Downloads/training_hashed.csv', header = T, stringsAsFactor = F)
qlik <- read.csv('/Users/Fliptop/Downloads/data_science_import_911738_explicitly_training.csv', header = T, stringsAsFactor = F)
quantcast2 <- read.csv('/Users/Fliptop/Downloads/data_science_import_911722_explicitly_training.csv', header = T, stringsAsFactor = F)
on24 <- read.csv('/Users/Fliptop/Downloads/data_science_import_911673_explicitly_training.csv', header = T, stringsAsFactor = F)

total_name_list <- c('DemandForce', 'DataSift', 'RocketFuel', 'Xangati', 'InsideView', 'Marketo', 'Asana3', 'Gainsight', 'Zenefits2cl', 'Qlik', 'Quantcast2', 'ON24')
total_list <- list(dmfc, dataSift, rktf, xngt, insv, mkto, asana3, gainsight, Zenefits2cl, qlik, quantcast2, on24)
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------


trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

dataCoverage <- function(df, dfname){
  #Data coverage - column wise
  #print(as.character(bquote(dfname)))
  
  m<-sapply(df, function(x) sum(trim(x) =='-1' | trim(x) =='' | is.na(x)))
  t<- data.frame(1-(m[order(m)] / nrow(df)))
  t$signalName <- row.names(t)
  t$nrows <- nrow(df)
  t <- data.frame(cbind(t[,2],t[,1],t[,3]), stringsAsFactors=FALSE)
  names(t) <- c('signalName', sprintf('%s_coveragePct', dfname), sprintf('%s_N', dfname))
  t
}
# Second Manual section is to run the dataCoverage function for each file previously read (can probably be automated).
# 2 ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
rktf_dc <- dataCoverage(rktf, "RocketFuel")
xngt_dc <- dataCoverage(xngt, "Xangati")
dmfc_dc <- dataCoverage(dmfc, "DemandForce")
insv_dc <- dataCoverage(insv, "InsideView")
dataSift_dc <- dataCoverage(dataSift, "DataSift")
mkto_dc <- dataCoverage(mkto, "Marketo")
asana3_dc <- dataCoverage(asana3, "Asana3")
gainsight_dc <- dataCoverage(gainsight, "Gainsight")
#mkto_leads_dc <- dataCoverage(mkto_leads, "mkto_leads")
Zenefits2cl_dc <- dataCoverage(Zenefits2cl, "Zenefits2cl")
#test_dc <- dataCoverage(test_hashed, "Test_hashed")
#train_dc <- dataCoverage(train_hashed, "Train_hashed")
qlik_dc <- dataCoverage(qlik, "Qlik")
quantcast_dc <- dataCoverage(quantcast2, "Quantcast2")
on24 <- dataCoverage(on24, "ON24")

total_list_dc <- list(dmfc_dc, dataSift_dc, rktf_dc, xngt_dc, insv_dc, mkto_dc, asana3_dc, gainsight_dc, Zenefits2cl_dc, qlik_dc, quantcast_dc, on24)
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------


xfinal <- as.data.frame(mkto_dc$signalName)
colnames(xfinal) = 'signalName'
for (name in total_list_dc){
  if (nrow(xfinal) > nrow(name)){
    xfinal <- merge(xfinal, name, by='signalName', all.x = T)
  }
  else{
    xfinal <- merge(xfinal, name, by='signalName', all.y = T)
  }
}

rownames(xfinal)<-xfinal$signalName
xfinal$signalName <-NULL

xfinal[is.na(xfinal)] <- 0

xfinal$numSum <- 0
xfinal$denomSum <- 0
for (name in total_name_list){
  x_name_count <- sprintf('%s_N', name)
  x_name_pct <- sprintf('%s_coveragePct', name)
  xfinal$numSum <- xfinal$numSum + (as.numeric(xfinal[,x_name_count]) * as.numeric(xfinal[,x_name_pct]))
  xfinal$denomSum <- xfinal$denomSum + as.numeric(xfinal[,x_name_count])
}

xfinal$globalPercentage <- xfinal$numSum/xfinal$denomSum

x_final_names_old <- colnames(xfinal)
x_final_names_new <- c(x_final_names_old)

for (name in total_name_list){
  x_name <- sprintf('%s_coveragePct', name)
  x_name_index <- sprintf('%s_index', name)
  x_final_names_new <- c(x_final_names_new, x_name_index)
  xfinal <- cbind(xfinal, as.numeric(xfinal[ ,x_name])/xfinal$globalPercentage)
  colnames(xfinal) <- x_final_names_new
}

write.csv(xfinal, file = "Coverage_Report.csv")

