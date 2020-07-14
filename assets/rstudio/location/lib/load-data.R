


library(shiny)
library(ggplot2)

readDataset <- function(fileName) { read.csv(file.path(fileName)) }

AllPatients <- readDataset("AllPatients.csv")
hospitals <- readDataset("HospitalTreatCondition.csv")

factor_columns_AllPatients=c(names(Filter(is.factor, AllPatients)))
AllPatients[factor_columns_AllPatients]=lapply(AllPatients[factor_columns_AllPatients], as.character)


factor_columns_hospital=c(names(Filter(is.factor, hospitals)))
hospitals[factor_columns_hospital]=lapply(hospitals[factor_columns_hospital], as.character)

clients=list()
i <- 1

while(i<=nrow(hospitals)) {
  clients[[i]]=list( name=c(hospitals$PROVIDER_NAME[i]),Patient_count=c(hospitals$PATIENT_COUNT[i]),PROVIDER_ID=c(hospitals$PROVIDER_ID[i]))
  
  i <- i + 1
}

clientIds <- c(hospitals$PROVIDER_ID)
names(clients)=c(clientIds)