###Capital cost recovery model###

#Clean up working environment####
rm(list=ls())
gc()

#Define Using function####
using<-function(...,prompt=TRUE){
  libs<-sapply(substitute(list(...))[-1],deparse)
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  n<-length(need)
  installAndRequire<-function(){
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
  if(n>0){
    libsmsg<-if(n>2) paste(paste(need[1:(n-1)],collapse=", "),",",sep="") else need[1]
    if(n>1){
      libsmsg<-paste(libsmsg," and ", need[n],sep="")
    }
    libsmsg<-paste("The following packages count not be found: ",libsmsg,"n\r\n\rInstall missing packages?",collapse="")
    if(prompt==FALSE){
      installAndRequire()
    }else if(winDialog(type=c("yesno"),libsmsg)=="YES"){
      installAndRequire()
    }
  }
}



#Install and load packages#
# using(readxl)
# using(reshape2)
# using(plyr)
# using(OECD)
# using(here)
# using(tidyverse)

library(readxl)
library(reshape2)
library(plyr)
library(OECD)
library(here)
# library(tidyverse)

#Working space setup
#Find directory#
isRStudio <- Sys.getenv("RSTUDIO") == "1"
if (isRStudio){
  CURDIR = dirname(rstudioapi::getActiveDocumentContext()$path)
} else{
  CURDIR <- here::here()
}
# Create directories will write output to in case they don't exist
dir.create(file.path(CURDIR, "final-data"), showWarnings = FALSE)
dir.create(file.path(CURDIR, "final-outputs"), showWarnings = FALSE)
# create variables for paths for later read/write commands
source_data_path <- file.path(CURDIR, "source-data")
final_data_path <- file.path(CURDIR, "final-data")
final_outputs_path <- file.path(CURDIR, "final-outputs")

#Read in dataset containing depreciation data####
data <- read.csv(file.path(source_data_path, "cost_recovery_data.csv"))


#Limit countries to OECD and EU countries
data <- data[which(data$country=="AUS"
                   | data$country=="AUT"
                   | data$country=="BEL"
                   | data$country=="BGR"
                   | data$country=="CAN"
                   | data$country=="CHL"
                   | data$country=="COL"
                   | data$country=="CRI"
                   | data$country=="HRV"
                   | data$country=="CYP"
                   | data$country=="CZE"
                   | data$country=="DNK"
                   | data$country=="EST"
                   | data$country=="FIN"
                   | data$country=="FRA"
                   | data$country=="DEU"
                   | data$country=="GRC"
                   | data$country=="HUN"
                   | data$country=="ISL"
                   | data$country=="IRL"
                   | data$country=="ISR"
                   | data$country=="ITA"
                   | data$country=="JPN"
                   | data$country=="KOR"
                   | data$country=="LVA"
                   | data$country=="LTU"
                   | data$country=="LUX"
                   | data$country=="MLT"
                   | data$country=="MEX"
                   | data$country=="NLD"
                   | data$country=="NZL"
                   | data$country=="NOR"
                   | data$country=="POL"
                   | data$country=="PRT"
                   | data$country=="ROU"
                   | data$country=="SVK"
                   | data$country=="SVN"
                   | data$country=="ESP"
                   | data$country=="SWE"
                   | data$country=="CHE"
                   | data$country=="TUR"
                   | data$country=="GBR"
                   | data$country=="USA"),]


#Drop columns that are not needed
data <- subset(data, select = -c(inventoryval, total, statutory_corptax, EATR, EMTR))


#Define functions for present discounted value calculations#

#Straight-line method (SL)
SL <- function(rate,i){
  pdv <- ((rate*(1+i))/i)*(1-(1^(1/rate)/(1+i)^(1/rate)))
  return(pdv)
}

#Straight-line method with a one-time change in the depreciation rate (SL2)
SL2 <- function(rate1,year1,rate2,year2,i){
  SL1 <- ((rate1*(1+i))/i)*(1-(1^year1)/(1+i)^year1)
  SL2 <- ((rate2*(1+i))/i)*(1-(1^year2)/(1+i)^year2) / (1+i)^year1
  pdv <-  SL1 + SL2
  return(pdv)
}

#Straight-line method with two changes in the depreciation rate (SL3) (SL3 will be treated like SL2 - see Italy)
SL3 <- function(year1,rate1,year2,rate2,year3,rate3,i){
  pdv <- 0
  for (x in 0:(year1-1)){
    pdv <- pdv + (rate1 / ((1+i)^x))
  }
  for (x in year1:(year2-1)){
    pdv <- pdv + (rate2 / ((1+i)^x))
  }
  for (x in year2:(year3-1)){
    pdv <- pdv + (rate3 / ((1+i)^x))
  }
  return(pdv)
}

#Declining-balance method (DB)
DB <- function(rate,i){
  pdv<- (rate*(1+i))/(i+rate)
  return(pdv)
}

#Declining-balance method with an initial allowance (initialDB)
initialDB <- function(rate1,rate2,i){
  pdv <- rate1 + ((rate2*(1+i))/(i+rate2)*(1-rate1))/(1+i)
  return(pdv)
}

#Declining-balance method with switch to straight-line method (DB or SL)
DBSL2 <- function(rate1,year1,rate2,year2,i){
  top <- (rate1+(rate2/((1+i)^year1))/year2 )*(1+i)
  bottom <- i + (rate1+(rate2/((1+i)^year1))/year2)
  return(top/bottom)
}

#Italy's straight-line method for the years 1998-2007 for buildings and machinery (SLITA)
#Special case of SL3. Deduct 3x rate in yr1, 2x in yr2, 1x rate until the last year.
SLITA <- function(rate,year,i){
  pdv <- rate + (((rate*2)*(1+i))/i)*(1-(1^(2)/(1+i)^(2)))/(1+i) + ((rate*(1+i))/i)*(1-(1^(year-3)/(1+i)^(year-3)))/(1+i)^3
  return(pdv)
}

#Special depreciation method used in the Czech Republic and Slovakia (CZK)
CZK <- function(rate,i){
  value<-1
  pdv <- 0
  years<-round(((1/rate)-1))
  for (x in 0:years){
    if (x == 0){
      pdv <- pdv + rate
      value <- value - rate
    } else {
      pdv<- pdv + (((value*2)/((1/rate)-x+1))/(1+i)^x)
      value <- value - ((value*2)/((1/rate)-x+1))
    }
  }
  return(pdv)
}

#Declining-balance and straight-line method (NOT USED)
#DBSL1 <- function(rate1,year1,rate2,year2,i){
#  value <- 1
#  DB <- 0
#  SL <- 0
#  for (x in 0:(year1-1)){
#    DB <- DB + (rate1*(1-rate1)^x)/(1+i)^x
#  }
#  SL <- ((rate2*(1+i))/i)*(1-(1^(year2)/(1+i)^(year2)))/(1+i)^(year1)
#  return(DB+SL)
#}

#Define constant: Fixed discount rate#
discount_rate = 0.075

#Debug summarys#
summary(data)
summary(data$taxdepbuildtype)
summary(data$taxdepmachtype)
summary(data$taxdepintangibltype)


#Replace odd depreciation systems ("SL3" and "DB DB SL")####

#Treat SL3 as SL2
data[c("taxdepbuildtype", "taxdepmachtype", "taxdepintangibltype")] <- as.data.frame(sapply(data[c("taxdepbuildtype", "taxdepmachtype", "taxdepintangibltype")], function(x) gsub("SL3", "SL2", x)))

#Treat "DB DB SL" as initialDB ("DB DB SL" -> "initialDB")
data[c("taxdepbuildtype", "taxdepmachtype", "taxdepintangibltype")] <- as.data.frame(sapply(data[c("taxdepbuildtype", "taxdepmachtype", "taxdepintangibltype")], function(x) gsub("DB DB SL", "initialDB", x)))


#Corrections to the dataset#

#Ireland's machine schedules are messed up for the years 1988-1991 (they are way too high). We assume that this is the fix:
data[c('taxdepmachtimedb')][data$country == "IRL" & data$year >= 1988 & data$year <= 1991,] <- 1

#The US' 3-schedule straight-line ACRS for machinery is coded incorrectly for the years 1983-1986 (since this model does not support SL3 it is assumed to be SL2)
data[c('taxdepmachtimesl')][data$country == "USA" & data$year >1982 & data$year<1987,] <- 4


#Calculate net present values for the different asset types####

#machines_cost_recovery####

#DB
data$machines_cost_recovery[data$taxdepmachtype == "DB" & !is.na(data$taxdepmachtype)] <- DB(data$taxdeprmachdb[data$taxdepmachtype == "DB" & !is.na(data$taxdepmachtype)],discount_rate)
data$machines_cost_recovery[data$taxdepmachtype == "DB" & !is.na(data$taxdepmachtype)] <- DB(data$taxdeprmachdb[data$taxdepmachtype == "DB" & !is.na(data$taxdepmachtype)],discount_rate)

#SL
data$machines_cost_recovery[data$taxdepmachtype == "SL" & !is.na(data$taxdepmachtype)] <- SL(data$taxdeprmachsl[data$taxdepmachtype == "SL" & !is.na(data$taxdepmachtype)],discount_rate)

#initialDB
data$machines_cost_recovery[data$taxdepmachtype == "initialDB" & !is.na(data$taxdepmachtype)] <- initialDB(data$taxdeprmachdb[data$taxdepmachtype == "initialDB" & !is.na(data$taxdepmachtype)],
                                                                                                           data$taxdeprmachsl[data$taxdepmachtype == "initialDB" & !is.na(data$taxdepmachtype)], discount_rate)

#DB or SL
data$machines_cost_recovery[data$taxdepmachtype == "DB or SL" & !is.na(data$taxdepmachtype)] <- DBSL2(data$taxdeprmachdb[data$taxdepmachtype == "DB or SL" & !is.na(data$taxdepmachtype)],
                                                                                                      data$taxdepmachtimedb[data$taxdepmachtype == "DB or SL" & !is.na(data$taxdepmachtype)],
                                                                                                      data$taxdeprmachsl[data$taxdepmachtype == "DB or SL" & !is.na(data$taxdepmachtype)],
                                                                                                      data$taxdepmachtimesl[data$taxdepmachtype == "DB or SL" & !is.na(data$taxdepmachtype)], discount_rate)

#SL2
data$machines_cost_recovery[data$taxdepmachtype == "SL2" & !is.na(data$taxdepmachtype)] <- SL2(data$taxdeprmachdb[data$taxdepmachtype == "SL2" & !is.na(data$taxdepmachtype)],
                                                                                               data$taxdepmachtimedb[data$taxdepmachtype == "SL2" & !is.na(data$taxdepmachtype)],
                                                                                               data$taxdeprmachsl[data$taxdepmachtype == "SL2" & !is.na(data$taxdepmachtype)],
                                                                                               data$taxdepmachtimesl[data$taxdepmachtype == "SL2" & !is.na(data$taxdepmachtype)], discount_rate)

#SLITA
data$machines_cost_recovery[data$taxdepmachtype == "SLITA" & !is.na(data$taxdepmachtype)] <- SL(data$taxdeprmachsl[data$taxdepmachtype == "SLITA" & !is.na(data$taxdepmachtype)],discount_rate)

#CZK
for (x in 1:length(data$taxdeprmachdb)){
  if(grepl("CZK",data$taxdepmachtype[x]) == TRUE){
    data$machines_cost_recovery[x] <- CZK(data$taxdeprmachdb[x], discount_rate)
  }
}


#buildings_cost_recovery####

#DB
data$buildings_cost_recovery[data$taxdepbuildtype == "DB" & !is.na(data$taxdepbuildtype)] <- DB(data$taxdeprbuilddb[data$taxdepbuildtype == "DB" & !is.na(data$taxdepbuildtype)],discount_rate)
data$buildings_cost_recovery[data$taxdepbuildtype == "DB" & !is.na(data$taxdepbuildtype)] <- DB(data$taxdeprbuilddb[data$taxdepbuildtype == "DB" & !is.na(data$taxdepbuildtype)],discount_rate)

#SL
data$buildings_cost_recovery[data$taxdepbuildtype == "SL" & !is.na(data$taxdepbuildtype)] <- SL(data$taxdeprbuildsl[data$taxdepbuildtype == "SL" & !is.na(data$taxdepbuildtype)],discount_rate)

#initialDB
data$buildings_cost_recovery[data$taxdepbuildtype == "initialDB" & !is.na(data$taxdepbuildtype)] <- initialDB(data$taxdeprbuilddb[data$taxdepbuildtype == "initialDB" & !is.na(data$taxdepbuildtype)],
                                                                                                              data$taxdeprbuildsl[data$taxdepbuildtype == "initialDB" & !is.na(data$taxdepbuildtype)], discount_rate)

#DB or SL
data$buildings_cost_recovery[data$taxdepbuildtype == "DB or SL" & !is.na(data$taxdepbuildtype)] <- DBSL2(data$taxdeprbuilddb[data$taxdepbuildtype == "DB or SL" & !is.na(data$taxdepbuildtype)],
                                                                                                         data$taxdeprbuildtimedb[data$taxdepbuildtype == "DB or SL" & !is.na(data$taxdepbuildtype)],
                                                                                                         data$taxdeprbuildsl[data$taxdepbuildtype == "DB or SL" & !is.na(data$taxdepbuildtype)],
                                                                                                         data$taxdeprbuildtimesl[data$taxdepbuildtype == "DB or SL" & !is.na(data$taxdepbuildtype)], discount_rate)

#SL2
data$buildings_cost_recovery[data$taxdepbuildtype == "SL2" & !is.na(data$taxdepbuildtype)] <- SL2(data$taxdeprbuilddb[data$taxdepbuildtype == "SL2" & !is.na(data$taxdepbuildtype)],
                                                                                                  data$taxdeprbuildtimedb[data$taxdepbuildtype == "SL2" & !is.na(data$taxdepbuildtype)],
                                                                                                  data$taxdeprbuildsl[data$taxdepbuildtype == "SL2" & !is.na(data$taxdepbuildtype)],
                                                                                                  data$taxdeprbuildtimesl[data$taxdepbuildtype == "SL2" & !is.na(data$taxdepbuildtype)], discount_rate)

#SLITA
data$buildings_cost_recovery[data$taxdepbuildtype == "SLITA" & !is.na(data$taxdepbuildtype)]<-SL(data$taxdeprbuildsl[data$taxdepbuildtype == "SLITA" & !is.na(data$taxdepbuildtype)],discount_rate)

#CZK
for (x in 1:length(data$taxdeprbuilddb)){
  if(grepl("CZK",data$taxdepbuildtype[x]) == TRUE){
    data$buildings_cost_recovery[x] <- CZK(data$taxdeprbuilddb[x], discount_rate)
  }
}


#intangibles_cost_recovery####

#DB
data$intangibles_cost_recovery[data$taxdepintangibltype == "DB" & !is.na(data$taxdepintangibltype)] <- DB(data$taxdeprintangibldb[data$taxdepintangibltype == "DB" & !is.na(data$taxdepintangibltype)], discount_rate)
data$intangibles_cost_recovery[data$taxdepintangibltype == "DB" & !is.na(data$taxdepintangibltype)] <- DB(data$taxdeprintangibldb[data$taxdepintangibltype == "DB" & !is.na(data$taxdepintangibltype)], discount_rate)

#SL
data$intangibles_cost_recovery[data$taxdepintangibltype == "SL" & !is.na(data$taxdepintangibltype)] <- SL(data$taxdeprintangiblsl[data$taxdepintangibltype == "SL" & !is.na(data$taxdepintangibltype)], discount_rate)

#initialDB
data$intangibles_cost_recovery[data$taxdepintangibltype == "initialDB" & !is.na(data$taxdepintangibltype)] <- initialDB(data$taxdeprintangibldb[data$taxdepintangibltype == "initialDB" & !is.na(data$taxdepintangibltype)],
                                                                                                                        data$taxdeprintangiblsl[data$taxdepintangibltype == "initialDB" & !is.na(data$taxdepintangibltype)], discount_rate)

#DB or SL
data$intangibles_cost_recovery[data$taxdepintangibltype == "DB or SL" & !is.na(data$taxdepintangibltype)] <- DBSL2(data$taxdeprintangibldb[data$taxdepintangibltype == "DB or SL" & !is.na(data$taxdepintangibltype)],
                                                                                                                   data$taxdepintangibltimedb[data$taxdepintangibltype == "DB or SL" & !is.na(data$taxdepintangibltype)],
                                                                                                                   data$taxdeprintangiblsl[data$taxdepintangibltype == "DB or SL" & !is.na(data$taxdepintangibltype)],
                                                                                                                   data$taxdepintangibltimesl[data$taxdepintangibltype == "DB or SL" & !is.na(data$taxdepintangibltype)], discount_rate)

#SL2
data$intangibles_cost_recovery[data$taxdepintangibltype == "SL2" & !is.na(data$taxdepintangibltype)] <- SL2(data$taxdeprintangibldb[data$taxdepintangibltype == "SL2" & !is.na(data$taxdepintangibltype)],
                                                                                                            data$taxdepintangibltimedb[data$taxdepintangibltype == "SL2" & !is.na(data$taxdepintangibltype)],
                                                                                                            data$taxdeprintangiblsl[data$taxdepintangibltype == "SL2" & !is.na(data$taxdepintangibltype)],
                                                                                                            data$taxdepintangibltimesl[data$taxdepintangibltype == "SL2" & !is.na(data$taxdepintangibltype)], discount_rate)

#In 2000, Estonia moved to a cash-flow type business tax - all allowances need to be coded as 1
data[c('intangibles_cost_recovery','machines_cost_recovery','buildings_cost_recovery')][data$country == "EST" & data$year >=2000,] <- 1

#In 2018, Latvia also moved to a cash-flow type business tax
data[c('intangibles_cost_recovery','machines_cost_recovery','buildings_cost_recovery')][data$country == "LVA" & data$year >=2018,] <- 1

#In 2019, Canada introduced full expensing for machinery (For assets from Nov 20, 2018; do not count 2018 as reform being active)
data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2019,] <- (data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2019,] * 0.00) + 1.00
data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2020,] <- (data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2020,] * 0.00) + 1.00
data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2021,] <- (data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2021,] * 0.00) + 1.00
data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2022,] <- (data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2022,] * 0.00) + 1.00
data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2023,] <- (data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2023,] * 0.00) + 1.00
data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2024,] <- (data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2024,] * 0.10) + 0.90
data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2025,] <- (data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2025,] * 0.20) + 0.80
data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2026,] <- (data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2026,] * 0.30) + 0.70
data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2027,] <- (data[c('machines_cost_recovery')][data$country == "CAN" & data$year == 2027,] * 0.40) + 0.60

#In 2020, Chile introduced full expensing
data[c('intangibles_cost_recovery','machines_cost_recovery','buildings_cost_recovery')][data$country == "CHL" & data$year ==2020,] <- 1
data[c('intangibles_cost_recovery','machines_cost_recovery','buildings_cost_recovery')][data$country == "CHL" & data$year ==2021,] <- 1
data[c('intangibles_cost_recovery','machines_cost_recovery','buildings_cost_recovery')][data$country == "CHL" & data$year ==2022,] <- 1

#Adjust USA data to include bonus depreciation for machinery
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2002,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2002,] * 0.70) + 0.30
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2003,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2003,] * 0.70) + 0.30
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2004,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2004,] * 0.50) + 0.50
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2008,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2008,] * 0.50) + 0.50
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2009,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2009,] * 0.50) + 0.50
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2010,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2010,] * 0.50) + 0.50
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2011,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2011,] * 0.00) + 1.00
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2012,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2012,] * 0.50) + 0.50
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2013,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2013,] * 0.50) + 0.50
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2014,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2014,] * 0.50) + 0.50
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2015,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2015,] * 0.50) + 0.50
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2016,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2016,] * 0.50) + 0.50
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2017,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2017,] * 0.50) + 0.50
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2018,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2018,] * 0.00) + 1.00
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2019,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2019,] * 0.00) + 1.00
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2020,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2020,] * 0.00) + 1.00
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2021,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2021,] * 0.00) + 1.00
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2022,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2022,] * 0.00) + 1.00
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2023,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2023,] * 0.20) + 0.80
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2024,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2024,] * 0.40) + 0.60
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2025,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2025,] * 0.60) + 0.40
data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2026,] <- (data[c('machines_cost_recovery')][data$country == "USA" & data$year == 2026,] * 0.80) + 0.20

#Adjust UK data to include super-deduction and permanent full expensing
data[c('machines_cost_recovery')][data$country == "GBR" & data$year == 2021,] <- (data[c('machines_cost_recovery')][data$country == "GBR" & data$year == 2021,] * 0.00) + 1.30
data[c('machines_cost_recovery')][data$country == "GBR" & data$year == 2022,] <- (data[c('machines_cost_recovery')][data$country == "GBR" & data$year == 2022,] * 0.00) + 1.30
data[c('machines_cost_recovery')][data$country == "GBR" & data$year >= 2023,] <- 1

#Only keep columns with the calculated net present values
data <- subset(data, select = c(country, year, buildings_cost_recovery, machines_cost_recovery, intangibles_cost_recovery))


#Weighing the calculated net present values of each asset by its respective capital stock share (based on Devereux 2012)
data$weighted_machines <- data$machines_cost_recovery*.4391081
data$weighted_buildings <- data$buildings_cost_recovery*.4116638
data$weighted_intangibles <- data$intangibles_cost_recovery*.1492281

data$waverage <- rowSums(data[,c("weighted_machines","weighted_buildings","weighted_intangibles")])
data$average<-rowMeans(data[,c("machines_cost_recovery","buildings_cost_recovery","intangibles_cost_recovery")])

#Drop columns with weighted net present values by asset type
data <- subset(data, select = -c(weighted_machines, weighted_buildings, weighted_intangibles))


#Import and match country names by ISO-3 codes#####

#Read in country name file
country_names <- read.csv(file.path(source_data_path, "country_codes.csv"))

#Keep and rename selected columns
country_names <- subset(country_names, select = c("official_name_en", "ISO3166.1.Alpha.3", "ISO3166.1.Alpha.2"))

colnames(country_names)[colnames(country_names)=="official_name_en"] <- "country"
colnames(country_names)[colnames(country_names)=="ISO3166.1.Alpha.3"] <- "iso_3"
colnames(country_names)[colnames(country_names)=="ISO3166.1.Alpha.2"] <- "iso_2"

#Rename column "country" in data
colnames(data)[colnames(data)=="country"] <- "iso_3"

#Add country names to data
data <- merge(country_names, data, by='iso_3')


#Adding GDP to the dataset#######

#GDP Data####

#Reading in GDP data
gdp<- read_excel(file.path(source_data_path,"/RealGDPValues.xlsx"), range = "A13:BL233")

#Drop rows that contain data of regions
colnames(gdp)[colnames(gdp)=="Country"] <- "country"
gdp$country <- as.character(gdp$country)
gdp <- subset(gdp, gdp$country != "Africa"
              & gdp$country != "Asia"
              & gdp$country != "Asia and Oceania"
              & gdp$country != "Asia less Japan"
              & gdp$country != "Belgium and Luxembourg"
              & gdp$country != "East Asia"
              & gdp$country != "East Asia less Japan"
              & gdp$country != "Europe"
              & gdp$country != "European Union 15"
              & gdp$country != "European Union 28"
              & gdp$country != "European Union 27"
              & gdp$country != "Euro Zone"
              & gdp$country != "Former Soviet Union"
              & gdp$country != "Latin America"
              & gdp$country != "Middle East"
              & gdp$country != "North Africa"
              & gdp$country != "North America"
              & gdp$country != "Oceania"
              & gdp$country != "Other Asia Oceania"
              & gdp$country != "Other Caribbean Central America"
              & gdp$country != "Other Central Europe"
              & gdp$country != "Other East Asia"
              & gdp$country != "Other Europe"
              & gdp$country != "Other Former Soviet Union"
              & gdp$country != "Other Middle East"
              & gdp$country != "Other North Africa"
              & gdp$country != "Other Oceania"
              & gdp$country != "Other South America"
              & gdp$country != "Other South Asia"
              & gdp$country != "Other Southeast Asia"
              & gdp$country != "Other Sub-Saharan Africa"
              & gdp$country != "Other West African Community"
              & gdp$country != "Other Western Europe"
              & gdp$country != "Recently Acceded Countries"
              & gdp$country != "Recently acceded countries"
              & gdp$country != "South America"
              & gdp$country != "South Asia"
              & gdp$country != "Southeast Asia"
              & gdp$country != "Sub-Saharan Africa"
              & gdp$country != "United States and Canada"
              & gdp$country != "World"
              & gdp$country != "World less USA")

#Renaming countries in gdp dataset
gdp$country[gdp$country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
gdp$country[gdp$country == "Republic of Congo"] <- "Republic of the Congo"
gdp$country[gdp$country == "Swaziland/Eswatini"] <- "Swaziland"

#write.csv(gdp,"gdp.csv", row.names=F)

#Renaming countries in gdp dataset to match iso-codes
gdp$country[gdp$country == "Bolivia"] <- "Bolivia (Plurinational State of)"
gdp$country[gdp$country == "Brunei"] <- "Brunei Darussalam"
gdp$country[gdp$country == "Hong Kong"] <- "China, Hong Kong Special Administrative Region"
gdp$country[gdp$country == "Macau"] <- "China, Macao Special Administrative Region"
gdp$country[gdp$country == "Czech Republic"] <- "Czechia"
gdp$country[gdp$country == "Iran"] <- "Iran (Islamic Republic of)"
gdp$country[gdp$country == "Korea"] <- "Republic of Korea"
gdp$country[gdp$country == "Laos"] <- "Lao People's Democratic Republic"
gdp$country[gdp$country == "Macedonia"] <- "The former Yugoslav Republic of Macedonia"
gdp$country[gdp$country == "Moldova"] <- "Republic of Moldova"
gdp$country[gdp$country == "Republic of the Congo"] <- "Congo"
gdp$country[gdp$country == "Russia"] <- "Russian Federation"
gdp$country[gdp$country == "St. Kitts and Nevis"] <- "Saint Kitts and Nevis"
gdp$country[gdp$country == "St. Lucia"] <- "Saint Lucia"
gdp$country[gdp$country == "St. Vincent and Grenadines"] <- "Saint Vincent and the Grenadines"
gdp$country[gdp$country == "Syria"] <- "Syrian Arab Republic"
gdp$country[gdp$country == "Tanzania"] <- "United Republic of Tanzania"
gdp$country[gdp$country == "United Kingdom"] <- "United Kingdom of Great Britain and Northern Ireland"
gdp$country[gdp$country == "United States"] <- "United States of America"
gdp$country[gdp$country == "Venezuela"] <- "Venezuela (Bolivarian Republic of)"
gdp$country[gdp$country == "Vietnam"] <- "Viet Nam"


#Change format of GDP data from wide to long
gdp$country <- as.character(gdp$country)
data$country <- as.character(data$country)
gdp_long <- (melt(gdp, id=c("country")))
colnames(gdp_long)[colnames(gdp_long)=="variable"] <- "year"
colnames(gdp_long)[colnames(gdp_long)=="value"] <- "gdp"

#delete the "y" before the year"
gdp_long$year <- gsub("^.{0,1}", "", gdp_long$year)

#Merge net present value data with GDP data
data <- merge(data, gdp_long, by =c("country", "year"), all=TRUE)


#Renaming countries####
data$country[data$country == "Czechia"] <- "Czech Republic"
data$country[data$country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
data$country[data$country == "Republic of Korea"] <- "Korea"
data$country[data$country == "United States of America"] <- "United States"

#Drop non-OECD/non-EU countries
#Limit countries to OECD and EU countries
data <- data[which(data$iso_3=="AUS"
                   | data$iso_3=="AUT"
                   | data$iso_3=="BEL"
                   | data$iso_3=="BGR"
                   | data$iso_3=="CAN"
                   | data$iso_3=="CHL"
                   | data$iso_3=="COL"
                   | data$iso_3=="CRI"
                   | data$iso_3=="HRV"
                   | data$iso_3=="CYP"
                   | data$iso_3=="CZE"
                   | data$iso_3=="DNK"
                   | data$iso_3=="EST"
                   | data$iso_3=="FIN"
                   | data$iso_3=="FRA"
                   | data$iso_3=="DEU"
                   | data$iso_3=="GRC"
                   | data$iso_3=="HUN"
                   | data$iso_3=="ISL"
                   | data$iso_3=="IRL"
                   | data$iso_3=="ISR"
                   | data$iso_3=="ITA"
                   | data$iso_3=="JPN"
                   | data$iso_3=="KOR"
                   | data$iso_3=="LVA"
                   | data$iso_3=="LTU"
                   | data$iso_3=="LUX"
                   | data$iso_3=="MLT"
                   | data$iso_3=="MEX"
                   | data$iso_3=="NLD"
                   | data$iso_3=="NZL"
                   | data$iso_3=="NOR"
                   | data$iso_3=="POL"
                   | data$iso_3=="PRT"
                   | data$iso_3=="ROU"
                   | data$iso_3=="SVK"
                   | data$iso_3=="SVN"
                   | data$iso_3=="ESP"
                   | data$iso_3=="SWE"
                   | data$iso_3=="CHE"
                   | data$iso_3=="TUR"
                   | data$iso_3=="GBR"
                   | data$iso_3=="USA"),]


#Write data file#
write.csv(data, file.path(final_data_path, "npv_all_years.csv"), row.names = FALSE)

#Create output tables and data for the graphs included in the report#####

#Main overview table: "Net Present Value of Capital Allowances in OECD Countries, 2023"

#Limit to OECD countries and 2023
data_oecd_2023 <- subset(data, year==2023)
data_oecd_2023 <- subset(data_oecd_2023, subset = iso_3 != "BGR" & iso_3 != "HRV" & iso_3 != "CYP" & iso_3 != "MLT" & iso_3 != "ROU")

#Create rankings
data_2023_ranking <- data_oecd_2023

data_2023_ranking$buildings_rank <- rank(-data_2023_ranking$`buildings_cost_recovery`,ties.method = "min")
data_2023_ranking$machines_rank <- rank(-data_2023_ranking$`machines_cost_recovery`,ties.method = "min")
data_2023_ranking$intangibles_rank <- rank(-data_2023_ranking$`intangibles_cost_recovery`,ties.method = "min")

data_2023_ranking$waverage_rank <- rank(-data_2023_ranking$`waverage`, ties.method = "min")

data_2023_ranking <- subset(data_2023_ranking, select = -c(year, iso_3, average, gdp))

#Order columns and sort data
data_2023_ranking <- data_2023_ranking[c("country", "waverage_rank", "waverage", "buildings_rank", "buildings_cost_recovery", "machines_rank", "machines_cost_recovery", "intangibles_rank", "intangibles_cost_recovery")]

data_2023_ranking <- data_2023_ranking[order(-data_2023_ranking$waverage, data_2023_ranking$country),]

#Round digits
data_2023_ranking$waverage <- round(data_2023_ranking$waverage, digits=3)
data_2023_ranking$buildings_cost_recovery <- round(data_2023_ranking$buildings_cost_recovery, digits=3)
data_2023_ranking$machines_cost_recovery <- round(data_2023_ranking$machines_cost_recovery, digits=3)
data_2023_ranking$intangibles_cost_recovery <- round(data_2023_ranking$intangibles_cost_recovery, digits=3)

#Rename column headers
colnames(data_2023_ranking)[colnames(data_2023_ranking)=="country"] <- "Country"
colnames(data_2023_ranking)[colnames(data_2023_ranking)=="waverage"] <- "Weighted Average Allowance"
colnames(data_2023_ranking)[colnames(data_2023_ranking)=="waverage_rank"] <- "Weighted Average Rank"
colnames(data_2023_ranking)[colnames(data_2023_ranking)=="buildings_cost_recovery"] <- "Buildings Allowance"
colnames(data_2023_ranking)[colnames(data_2023_ranking)=="buildings_rank"] <- "Buildings Rank"
colnames(data_2023_ranking)[colnames(data_2023_ranking)=="machines_cost_recovery"] <- "Machinery Allowance"
colnames(data_2023_ranking)[colnames(data_2023_ranking)=="machines_rank"] <- "Machinery Rank"
colnames(data_2023_ranking)[colnames(data_2023_ranking)=="intangibles_cost_recovery"] <- "Intangibles Allowance"
colnames(data_2023_ranking)[colnames(data_2023_ranking)=="intangibles_rank"] <- "Intangibles Rank"

write.csv(data_2023_ranking, file.path(final_outputs_path, "npv_ranks_2023.csv"),row.names = FALSE)



#Data for chart: "Net Present Value of Capital Allowances in the OECD, 2000-2023"

#Limit to OECD countries
data_oecd_all_years <- subset(data, subset = iso_3 != "BGR" & iso_3 != "HRV" & iso_3 != "CYP" & iso_3 != "MLT" & iso_3 != "ROU")

#Calculate timeseries averages
data_oecd_all_years$gdp <- as.numeric(data_oecd_all_years$gdp)
data_oecd_all_years$waverage <- as.numeric(data_oecd_all_years$waverage)
data_oecd_all_years$average <- as.numeric(data_oecd_all_years$average)
data_weighted <- ddply(data_oecd_all_years, .(year),summarize, weighted_average = weighted.mean(waverage, gdp, na.rm = TRUE), average = mean(waverage, na.rm = TRUE), n = length(waverage[is.na(waverage) == FALSE]))

#Limit to years starting in 2000 (data for all OECD countries is available starting in 2000)
data_weighted <- data_weighted[data_weighted$year>1999,]

colnames(data_weighted)[colnames(data_weighted)=="n"] <- "country_count"

write.csv(data_weighted, file.path(final_outputs_path, "npv_weighted_timeseries.csv"), row.names = FALSE)



#Data for chart: "Statutory Weighted and Unweighted Combined Corporate Income Tax Rates in the OECD, 2000-2023"

#Read in dataset
dataset_list <- get_datasets()
#search_dataset("Corporate", data= dataset_list)
oecd_rates <- ("TABLE_II1")
#dstruc <- get_data_structure(oecd_rates)
#str(dstruc, max.level = 1)
#dstruc$VAR_DESC
#dstruc$CORP_TAX

#Alternative approach, importing CSV from source data
#oecd_rates <- read.csv(file.path(source_data_path, "corporate_rates.csv"))

#Keep and rename selected columns
#oecd_rates <- subset(oecd_rates, select = c(COU,YEA,Value))

#colnames(oecd_rates)[colnames(oecd_rates)=="Value"] <- "rate"
#colnames(oecd_rates)[colnames(oecd_rates)=="YEA"] <- "year"
#colnames(oecd_rates)[colnames(oecd_rates)=="COU"] <- "iso_3"

oecd_rates <- get_dataset("TABLE_II1", start_time = 2000)

#Keep and rename selected columns
oecd_rates <- subset(oecd_rates, oecd_rates$CORP_TAX=="COMB_CIT_RATE")
oecd_rates <- subset(oecd_rates, select = -c(CORP_TAX,TIME_FORMAT))

colnames(oecd_rates)[colnames(oecd_rates)=="ObsValue"] <- "rate"
colnames(oecd_rates)[colnames(oecd_rates)=="Time"] <- "year"
colnames(oecd_rates)[colnames(oecd_rates)=="COU"] <- "iso_3"

#Add country names
oecd_rates <- merge(oecd_rates, country_names, by='iso_3')


#Add GDP
oecd_rates$country <- as.character(oecd_rates$country)


oecd_rates <- merge(oecd_rates, gdp_long, by =c("country", "year"), all=FALSE)

#Weigh corporate rates by GDP
oecd_rates$gdp <- as.numeric(oecd_rates$gdp)
oecd_rates$rate <- as.numeric(oecd_rates$rate)
oecd_rates_weighted <- ddply(oecd_rates, .(year),summarize, weighted_average = weighted.mean(rate, gdp, na.rm = TRUE), average = mean(rate, na.rm = TRUE),n = length(rate[is.na(rate) == FALSE]))
write.csv(oecd_rates_weighted, file.path(final_outputs_path, "cit_rates_timeseries.csv"),row.names = FALSE)

#Data for map: "Net Present Value of Capital Allowances in Europe"

#Keep European countries and the year 2023
data_europe_2023 <- subset(data, year==2023)
data_europe_2023 <- subset(data_europe_2023, subset = iso_3 != "AUS" & iso_3 != "CAN" & iso_3 != "CHL" & iso_3 != "COL" & iso_3 != "CRI" & iso_3 != "ISR" & iso_3 != "JPN" & iso_3 != "KOR" & iso_3 != "MEX" & iso_3 != "NZL" & iso_3 != "USA")

#Drop columns that are not needed
data_europe_2023 <- subset(data_europe_2023, select = c(iso_3, country, year, waverage))

#Sort data
data_europe_2023 <- data_europe_2023[order(-data_europe_2023$waverage, data_europe_2023$country),]

#Add ranking
data_europe_2023$rank <- rank(-data_europe_2023$`waverage`,ties.method = "min")

write.csv(data_europe_2023, file.path(final_outputs_path,"npv_europe.csv"),row.names = FALSE)


#Data for chart: "Net Present Value of Capital Allowances in the EU compared to CCTB"

#Limit to EU countries and 2023
data_eu27_2023 <- subset(data, year==2023)
data_eu27_2023 <- subset(data_eu27_2023, subset = iso_3 != "AUS" & iso_3 != "CAN" & iso_3 != "CHL" & iso_3 != "COL" & iso_3 != "CRI"& iso_3 != "ISL" & iso_3 != "ISR" & iso_3 != "JPN" & iso_3 != "KOR" & iso_3 != "MEX" & iso_3 != "NZL" & iso_3 != "NOR" & iso_3 != "CHE" & iso_3 != "TUR" & iso_3 != "GBR" & iso_3 != "USA")

#Drop columns that are not needed
data_eu27_2023 <- subset(data_eu27_2023, select = c(iso_3, country, year, waverage))

#Sort data
data_eu27_2023 <- data_eu27_2023[order(-data_eu27_2023$waverage, data_eu27_2023$country),]

#Add weighted average of capital allowances under CCTB
cctb <- data.frame(iso_3 = c("CCTB"), country = c("CCTB"), year = c(2023), waverage = c(0.673))
data_eu27_2023 <- rbind(data_eu27_2023, cctb)

write.csv(data_eu27_2023, file.path(final_outputs_path,"eu_cctb.csv"),row.names = FALSE)


#Data for chart: "Net Present Value of Capital Allowances by Asset Type in the OECD, 2023"

#Calculate averages by asset type
average_assets <- ddply(data_oecd_2023, .(year),summarize, average_building = mean(buildings_cost_recovery, na.rm = TRUE), average_machines = mean(machines_cost_recovery, na.rm = TRUE), average_intangibles = mean(intangibles_cost_recovery, na.rm = TRUE))

write.csv(average_assets, file.path(final_outputs_path,"asset_averages.csv"),row.names = FALSE)