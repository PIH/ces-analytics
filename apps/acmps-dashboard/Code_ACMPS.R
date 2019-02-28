library(dplyr) 
library(ggplot2)
library(ggpubr)
theme_set(theme_pubclean())
library(XLConnect)
library(readxl)


#Download excel and read tabs separately 

full_excel_read<-function(fpath,v=TRUE){
  
  sheetnames <- excel_sheets(fpath)
  workbook <- sapply(sheetnames,function (x){readxl::read_excel(fpath,sheet = x)})
  for (sh in sheetnames) {
    workbook[[sh]]<-as.data.frame.table(workbook[[sh]])
  }
  if (v){
    lapply(sheetnames, function(x){View(workbook[[x]],x)})
  }
  
  workbook
}


#Merge CRONICOS - Hoja de visita and indicadores for based on caseID and month 

CR1 <- read_excel("~/Documents/R Project/ACMPS_2018.xlsx", sheet = 7)
CR2 <- read_excel("~/Documents/R Project/ACMPS_2018.xlsx", sheet = 8)
Casos_Cronicos <- read_excel("~/Documents/R Project/Cases_Cronicos.xls")

cronicas <- merge(CR1, CR2, by=c("form.case.@case_id", "form.mes"), all = TRUE)
cronicas <- merge(Casos_Cronicos, cronicas, by.x = "ï»¿caseid",  by.y= "form.case.@case_id", all = TRUE)


View(cronicas)
cronicas[6] <- lapply(cronicas[6], as.numeric)
cronicas[10:13] <- lapply(cronicas[10:13], as.numeric)
cronicas[15] <- lapply(cronicas[15], as.numeric)
cronicas[16] <- lapply(cronicas[16], as.numeric)
cronicas[18:26] <- lapply(cronicas[18:26], as.numeric)
cronicas[29] <- lapply(cronicas[29], as.numeric)
cronicas[9] <- lapply(cronicas[9], as.date)
