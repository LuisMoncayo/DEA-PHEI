library(readxl)

publicFunding <- function() {
  subsidyData <- data.frame(read_xlsx('Data/execum.xlsx', col_names = TRUE, range = 'A8:D44'))
  colnames(subsidyData) <- c('PHEI','Acronym','Subsidy2019','Subsidy2016')
  subsidyData$Subsidy2019 <- as.numeric(gsub("[',]", "", subsidyData$Subsidy2019))/1e6
  return(subsidyData)
}

variables2019 <- function(subsidyData){
  #------------------------
  # Data for the year 2019 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #------------------------
  rawinputData2019 <- data.frame(read_xlsx('Data/execum5fea4dd57972b.xlsx', col_names = TRUE, sheet = 'datos 2019', range = 'A8:AL54'))
  inputData2019 <- rawinputData2019[c('INSTITUCIONES','...2','...4','...8','...10','...12','...14','...16','...18','...20','...24','...30','...33','...37')]
  colnames(inputData2019) <- c('PHEI','Lecturers','Students','Alumni','AcademicProgr','Researchers','WoSpapers','WoSworks','SCOPUSpapers','SCOPUSworks','PatMx','EditedJour','acreUndergradProg','acrePosgradPro')
  
  for(i in 2:ncol(inputData2019)) {
    if(class(inputData2019[,i]) == "character"){
      inputData2019[,i] <- as.numeric(inputData2019[,i])
    }
  }
  inputData2019[is.na(inputData2019)] <- 0
  
  data2019 <- merge(inputData2019, subsidyData, by = "PHEI")
  data2019 <- data2019[,-17]
  return(data2019)
}

variables2016 <- function(subsidyData){
  #------------------------
  # Data for the year 2016 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #------------------------
  rawinputData2016 <- data.frame(read_xlsx('Data/execum5fea4dd57972b.xlsx', col_names = TRUE, sheet = 'datos 2016', range = 'A8:AL54'))
  inputData2016 <- rawinputData2016[c('INSTITUCIONES','...2','...4','...8','...10','...12','...14','...16','...18','...20','...24','...30','...33','...37')]
  colnames(inputData2016) <- c('PHEI','Lecturers','Students','Alumni','AcademicProgr','Researchers','WoSpapers','WoSworks','SCOPUSpapers','SCOPUSworks','PatMx','EditedJour','acreUndergradProg','acrePosgradPro')
  
  for(i in 2:ncol(inputData2016)) {
    if(class(inputData2016[,i]) == "character"){
      inputData2016[,i] <- as.numeric(inputData2016[,i])
    }
  }
  inputData2016[is.na(inputData2016)] <- 0
  
  data2016 <- merge(inputData2016, subsidyData, by = "PHEI")
  data2016 <- data2016[,-16]
  return(data2016)
}
  
  
  
  
  
  
  