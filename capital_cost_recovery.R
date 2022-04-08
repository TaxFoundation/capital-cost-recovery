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



#Install packages#
using(readxl)
using(reshape2)
using(plyr)
using(OECD)
using(here)
using(tidyverse)


#Working space setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source_data<-"C:/Github/capital-cost-recovery/source-data/"
final_data<-"C:/Github/capital-cost-recovery/final-data/"
final_outputs<-"C:/Github/capital-cost-recovery/final-outputs/"


#Find directory#
CURDIR <- here::here()

# Ceate directories will write output to in case they don't exist
dir.create(file.path(CURDIR, "final-data"), showWarnings = FALSE)
dir.create(file.path(CURDIR, "final-outputs"), showWarnings = FALSE)

#Read in dataset containing depreciation data####
data <- read_csv(paste(source_data,"cost_recovery_data.csv",sep=""))


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
data$machines_cost_recovery[data$taxdepmachtype == "DB" & !is.na(data$taxdepmachtype)] <- DB(data$taxdeprmachdb[data$taxdepmachtype == "DB" & !is.na(data$taxdepmachtype)],0.075)
data$machines_cost_recovery[data$taxdepmachtype == "DB" & !is.na(data$taxdepmachtype)] <- DB(data$taxdeprmachdb[data$taxdepmachtype == "DB" & !is.na(data$taxdepmachtype)],0.075)

#SL
data$machines_cost_recovery[data$taxdepmachtype == "SL" & !is.na(data$taxdepmachtype)] <- SL(data$taxdeprmachsl[data$taxdepmachtype == "SL" & !is.na(data$taxdepmachtype)],0.075)

#initialDB
data$machines_cost_recovery[data$taxdepmachtype == "initialDB" & !is.na(data$taxdepmachtype)] <- initialDB(data$taxdeprmachdb[data$taxdepmachtype == "initialDB" & !is.na(data$taxdepmachtype)],
  data$taxdeprmachsl[data$taxdepmachtype == "initialDB" & !is.na(data$taxdepmachtype)], 0.075)

#DB or SL
data$machines_cost_recovery[data$taxdepmachtype == "DB or SL" & !is.na(data$taxdepmachtype)] <- DBSL2(data$taxdeprmachdb[data$taxdepmachtype == "DB or SL" & !is.na(data$taxdepmachtype)],
  data$taxdepmachtimedb[data$taxdepmachtype == "DB or SL" & !is.na(data$taxdepmachtype)],
  data$taxdeprmachsl[data$taxdepmachtype == "DB or SL" & !is.na(data$taxdepmachtype)],
  data$taxdepmachtimesl[data$taxdepmachtype == "DB or SL" & !is.na(data$taxdepmachtype)], 0.075)

#SL2
data$machines_cost_recovery[data$taxdepmachtype == "SL2" & !is.na(data$taxdepmachtype)] <- SL2(data$taxdeprmachdb[data$taxdepmachtype == "SL2" & !is.na(data$taxdepmachtype)],
  data$taxdepmachtimedb[data$taxdepmachtype == "SL2" & !is.na(data$taxdepmachtype)],
  data$taxdeprmachsl[data$taxdepmachtype == "SL2" & !is.na(data$taxdepmachtype)],
  data$taxdepmachtimesl[data$taxdepmachtype == "SL2" & !is.na(data$taxdepmachtype)], 0.075)

#SLITA
data$machines_cost_recovery[data$taxdepmachtype == "SLITA" & !is.na(data$taxdepmachtype)] <- SL(data$taxdeprmachsl[data$taxdepmachtype == "SLITA" & !is.na(data$taxdepmachtype)],0.075)

#CZK
for (x in 1:length(data$taxdeprmachdb)){
  if(grepl("CZK",data$taxdepmachtype[x]) == TRUE){
    data$machines_cost_recovery[x] <- CZK(data$taxdeprmachdb[x], 0.075)
  }
}


#buildings_cost_recovery####

#DB
data$buildings_cost_recovery[data$taxdepbuildtype == "DB" & !is.na(data$taxdepbuildtype)] <- DB(data$taxdeprbuilddb[data$taxdepbuildtype == "DB" & !is.na(data$taxdepbuildtype)],0.075)
data$buildings_cost_recovery[data$taxdepbuildtype == "DB" & !is.na(data$taxdepbuildtype)] <- DB(data$taxdeprbuilddb[data$taxdepbuildtype == "DB" & !is.na(data$taxdepbuildtype)],0.075)

#SL
data$buildings_cost_recovery[data$taxdepbuildtype == "SL" & !is.na(data$taxdepbuildtype)] <- SL(data$taxdeprbuildsl[data$taxdepbuildtype == "SL" & !is.na(data$taxdepbuildtype)],0.075)

#initialDB
data$buildings_cost_recovery[data$taxdepbuildtype == "initialDB" & !is.na(data$taxdepbuildtype)] <- initialDB(data$taxdeprbuilddb[data$taxdepbuildtype == "initialDB" & !is.na(data$taxdepbuildtype)],
  data$taxdeprbuildsl[data$taxdepbuildtype == "initialDB" & !is.na(data$taxdepbuildtype)], 0.075)

#DB or SL
data$buildings_cost_recovery[data$taxdepbuildtype == "DB or SL" & !is.na(data$taxdepbuildtype)] <- DBSL2(data$taxdeprbuilddb[data$taxdepbuildtype == "DB or SL" & !is.na(data$taxdepbuildtype)],
  data$taxdeprbuildtimedb[data$taxdepbuildtype == "DB or SL" & !is.na(data$taxdepbuildtype)],
  data$taxdeprbuildsl[data$taxdepbuildtype == "DB or SL" & !is.na(data$taxdepbuildtype)],
  data$taxdeprbuildtimesl[data$taxdepbuildtype == "DB or SL" & !is.na(data$taxdepbuildtype)], 0.075)

#SL2
data$buildings_cost_recovery[data$taxdepbuildtype == "SL2" & !is.na(data$taxdepbuildtype)] <- SL2(data$taxdeprbuilddb[data$taxdepbuildtype == "SL2" & !is.na(data$taxdepbuildtype)],
  data$taxdeprbuildtimedb[data$taxdepbuildtype == "SL2" & !is.na(data$taxdepbuildtype)],
  data$taxdeprbuildsl[data$taxdepbuildtype == "SL2" & !is.na(data$taxdepbuildtype)],
  data$taxdeprbuildtimesl[data$taxdepbuildtype == "SL2" & !is.na(data$taxdepbuildtype)], 0.075)

#SLITA
data$buildings_cost_recovery[data$taxdepbuildtype == "SLITA" & !is.na(data$taxdepbuildtype)]<-SL(data$taxdeprbuildsl[data$taxdepbuildtype == "SLITA" & !is.na(data$taxdepbuildtype)],0.075)

#CZK
for (x in 1:length(data$taxdeprbuilddb)){
  if(grepl("CZK",data$taxdepbuildtype[x]) == TRUE){
    data$buildings_cost_recovery[x] <- CZK(data$taxdeprbuilddb[x], 0.075)
  }
}


#intangibles_cost_recovery####

#DB
data$intangibles_cost_recovery[data$taxdepintangibltype == "DB" & !is.na(data$taxdepintangibltype)] <- DB(data$taxdeprintangibldb[data$taxdepintangibltype == "DB" & !is.na(data$taxdepintangibltype)], 0.075)
data$intangibles_cost_recovery[data$taxdepintangibltype == "DB" & !is.na(data$taxdepintangibltype)] <- DB(data$taxdeprintangibldb[data$taxdepintangibltype == "DB" & !is.na(data$taxdepintangibltype)], 0.075)

#SL
data$intangibles_cost_recovery[data$taxdepintangibltype == "SL" & !is.na(data$taxdepintangibltype)] <- SL(data$taxdeprintangiblsl[data$taxdepintangibltype == "SL" & !is.na(data$taxdepintangibltype)], 0.075)

#initialDB
data$intangibles_cost_recovery[data$taxdepintangibltype == "initialDB" & !is.na(data$taxdepintangibltype)] <- initialDB(data$taxdeprintangibldb[data$taxdepintangibltype == "initialDB" & !is.na(data$taxdepintangibltype)],
  data$taxdeprintangiblsl[data$taxdepintangibltype == "initialDB" & !is.na(data$taxdepintangibltype)], 0.075)

#DB or SL
data$intangibles_cost_recovery[data$taxdepintangibltype == "DB or SL" & !is.na(data$taxdepintangibltype)] <- DBSL2(data$taxdeprintangibldb[data$taxdepintangibltype == "DB or SL" & !is.na(data$taxdepintangibltype)],
  data$taxdepintangibltimedb[data$taxdepintangibltype == "DB or SL" & !is.na(data$taxdepintangibltype)],
  data$taxdeprintangiblsl[data$taxdepintangibltype == "DB or SL" & !is.na(data$taxdepintangibltype)],
  data$taxdepintangibltimesl[data$taxdepintangibltype == "DB or SL" & !is.na(data$taxdepintangibltype)], 0.075)

#SL2
data$intangibles_cost_recovery[data$taxdepintangibltype == "SL2" & !is.na(data$taxdepintangibltype)] <- SL2(data$taxdeprintangibldb[data$taxdepintangibltype == "SL2" & !is.na(data$taxdepintangibltype)],
  data$taxdepintangibltimedb[data$taxdepintangibltype == "SL2" & !is.na(data$taxdepintangibltype)],
  data$taxdeprintangiblsl[data$taxdepintangibltype == "SL2" & !is.na(data$taxdepintangibltype)],
  data$taxdepintangibltimesl[data$taxdepintangibltype == "SL2" & !is.na(data$taxdepintangibltype)], 0.075)

#In 2000, Estonia moved to a cash-flow type business tax - all allowances need to be coded as 1
data[c('intangibles_cost_recovery','machines_cost_recovery','buildings_cost_recovery')][data$country == "EST" & data$year >=2000,] <- 1

#In 2018, Latvia also moved to a cash-flow type business tax
data[c('intangibles_cost_recovery','machines_cost_recovery','buildings_cost_recovery')][data$country == "LVA" & data$year >=2018,] <- 1

#In fall 2018, Canada introduced full expensing for machinery
data[c('machines_cost_recovery')][data$country == "CAN" & data$year >= 2018,] <- 1

#In 2020, Chile introduced full expensing
data[c('intangibles_cost_recovery','machines_cost_recovery','buildings_cost_recovery')][data$country == "CHL" & data$year ==2020,] <- 1


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

#Ajdust UK data to include super-deduction
data[c('machines_cost_recovery')][data$country == "GBR" & data$year == 2021,] <- (data[c('machines_cost_recovery')][data$country == "GBR" & data$year == 2021,] * 0.00) + 1.00

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
country_names <- read_csv(paste(source_data,"country_codes.csv",sep=""))

#Keep and rename selected columns
country_names <- subset(country_names, select = c("official_name_en", "ISO3166-1-Alpha-3", "ISO3166-1-Alpha-2"))

colnames(country_names)[colnames(country_names)=="official_name_en"] <- "country"
colnames(country_names)[colnames(country_names)=="ISO3166-1-Alpha-3"] <- "iso_3"
colnames(country_names)[colnames(country_names)=="ISO3166-1-Alpha-2"] <- "iso_2"

#Rename column "country" in data
colnames(data)[colnames(data)=="country"] <- "iso_3"

#Add country names to data
data <- merge(country_names, data, by='iso_3')


#Adding GDP to the dataset#######

#GDP Data####

#Reading in GDP data
gdp_historical_2010<- read_excel("source-data/gdp_historical_2010.xlsx", range = "A12:AN230")
gdp_historical_2015<- read_excel("source-data/gdp_historical_2015.xlsx", range = "A14:V234")
gdp_projected_2015 <- read_excel("source-data/gdp_projected_2015.xlsx", range = "A14:K234")

#Merging historical and projected data
gdp_historical_2010 <-gdp_historical_2010[,-c(21:40)]
gdp_projected_2015 <- gdp_projected_2015[,-c(2:9)]

gdp_historical_2010$Country[gdp_historical_2010$Country == "Antigua Barbuda"] <- "Antigua and Barbuda"
gdp_historical_2010$Country[gdp_historical_2010$Country == "Dominican Rep"] <- "Dominican Republic"
gdp_historical_2010$Country[gdp_historical_2010$Country == "St Kitts Nevis"] <- "St. Kitts and Nevis"
gdp_historical_2010$Country[gdp_historical_2010$Country == "St Lucia"] <- "St. Lucia"
gdp_historical_2010$Country[gdp_historical_2010$Country == "St Vincent Grenadines"] <- "St. Vincent and Grenadines"
gdp_historical_2010$Country[gdp_historical_2010$Country == "UK"] <- "United Kingdom"
gdp_historical_2010$Country[gdp_historical_2010$Country == "Bosnia Herzegovina"] <- "Bosnia and Herzegovina"
gdp_historical_2015$Country[gdp_historical_2015$Country == "Côte d'Ivoire"] <- "Cote d'Ivoire"
gdp_projected_2015$Country[gdp_projected_2015$Country == "Côte d'Ivoire"] <- "Cote d'Ivoire"
gdp_historical_2010$Country[gdp_historical_2010$Country == "Guinea Bissau"] <- "Guinea-Bissau"
gdp_historical_2010$Country[gdp_historical_2010$Country == "Central Afr Rep"] <- "Central African Republic"
gdp_historical_2010$Country[gdp_historical_2010$Country == "Dem Rep Congo"] <- "Democratic Republic of the Congo"
gdp_historical_2015$Country[gdp_historical_2015$Country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
gdp_projected_2015$Country[gdp_projected_2015$Country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
gdp_historical_2010$Country[gdp_historical_2010$Country == "Rep Congo"] <- "Republic of the Congo"
gdp_historical_2015$Country[gdp_historical_2015$Country == "Republic of Congo"] <- "Republic of the Congo"
gdp_projected_2015$Country[gdp_projected_2015$Country == "Republic of Congo"] <- "Republic of the Congo"
gdp_historical_2010$Country[gdp_historical_2010$Country == "Sao Tome Principe"] <- "Sao Tome and Principe"
gdp_historical_2015$Country[gdp_historical_2015$Country == "São Tomé and Principe"] <- "Sao Tome and Principe"
gdp_projected_2015$Country[gdp_projected_2015$Country == "São Tomé and Principe"] <- "Sao Tome and Principe"
gdp_historical_2015$Country[gdp_historical_2015$Country == "St. Kitts Nevis"] <- "St. Kitts and Nevis"
gdp_projected_2015$Country[gdp_projected_2015$Country == "St. Kitts Nevis"] <- "St. Kitts and Nevis"
gdp_historical_2015$Country[gdp_historical_2015$Country == "St. Vincent Grenadines"] <- "St. Vincent and Grenadines"
gdp_projected_2015$Country[gdp_projected_2015$Country == "St. Vincent Grenadines"] <- "St. Vincent and Grenadines"
gdp_historical_2015$Country[gdp_historical_2015$Country == "Swaziland/Eswatini"] <- "Swaziland"
gdp_projected_2015$Country[gdp_projected_2015$Country == "Swaziland/Eswatini"] <- "Swaziland"


#Add Qatar and Zimbabwe to gdp_historical_2010
Qatar<-c("Qatar",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
Zimbabwe<-c("Zimbabwe",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

gdp_historical_2010<-(rbind(gdp_historical_2010,Qatar))
gdp_historical_2010<-(rbind(gdp_historical_2010,Zimbabwe))


#write.csv(gdp_historical_2010,"gdp_historical_2010.csv",row.names = F)
#write.csv(gdp_historical_2015,"gdp_historical_2015.csv",row.names = F)
#write.csv(gdp_projected_2015,"gdp_projected_2015.csv",row.names = F)

gdp <- merge(gdp_historical_2010, gdp_historical_2015, by="Country", All=T)
gdp <- merge(gdp, gdp_projected_2015, by="Country", All=T)

#write.csv(gdp,"gdp.csv", row.names=F)
colnames(gdp)[colnames(gdp)=="Country"] <- "country"

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


#Drop rows that contain data of regions
gdp$country <- as.character(gdp$country)
gdp <- subset(gdp, gdp$country != "Africa"
              & gdp$country != "Asia"
              & gdp$country != "Asia and Oceania"
              & gdp$country != "Asia Less Japan"
              & gdp$country != "Belgium Luxembourg"
              & gdp$country != "East Asia"
              & gdp$country != "East Asia Less Japan"
              & gdp$country != "Europe"
              & gdp$country != "European Union 15"
              & gdp$country != "European Union 28"
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
              & gdp$country != "South America"
              & gdp$country != "South Asia"
              & gdp$country != "Southeast Asia"
              & gdp$country != "Sub-Saharan Africa"
              & gdp$country != "World"
              & gdp$country != "World Less USA")



#Reading in and merging GDP datasets
#gdp_historical <- read_excel(paste(source_data, "gdp_historical.xlsx"), range = "A14:V234")
#gdp_projected <- read_excel(file.path(CURDIR, "source-data", "gdp_projected.xlsx"), range = "A14:J234")

#gdp_historical$Country[gdp_historical$Country == "UK"] <- "United Kingdom"

#gdp_projected <- subset(gdp_projected, select = c(Country, `2020`))
#gdp <- merge(gdp_historical,gdp_projected, by="Country")
#colnames(gdp)[colnames(gdp)=="Country"] <- "country"

#Renaming country names so data and gdp can be matched
#gdp$country <- as.character(gdp$country)
#data$country <- as.character(data$country)

#data$country[data$country == "Czechia"] <- "Czech Republic"
#data$country[data$country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
#data$country[data$country == "Republic of Korea"] <- "Korea"
#data$country[data$country == "United States of America"] <- "United States"

#Change format of GDP data from wide to long
gdp_long <- (melt(gdp, id=c("country")))
colnames(gdp_long)[colnames(gdp_long)=="variable"] <- "year"
colnames(gdp_long)[colnames(gdp_long)=="value"] <- "gdp"

#Merge net present value data with GDP data
data <- merge(data, gdp_long, by =c("country", "year"), all=TRUE)

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
write.csv(data, paste(final_data,"npv_all_years.csv",sep=""), row.names = FALSE)


#Create output tables and data for the graphs included in the report#####

#Main overview table: "Net Present Value of Capital Allowances in OECD Countries, 2021"

#Limit to OECD countries and 2021
data_oecd_2021 <- subset(data, year==2021)
data_oecd_2021 <- subset(data_oecd_2021, subset = iso_3 != "BGR" & iso_3 != "HRV" & iso_3 != "CYP" & iso_3 != "MLT" & iso_3 != "ROU")

#Create rankings
data_2021_ranking <- data_oecd_2021

data_2021_ranking$buildings_rank <- rank(-data_2021_ranking$`buildings_cost_recovery`,ties.method = "min")
data_2021_ranking$machines_rank <- rank(-data_2021_ranking$`machines_cost_recovery`,ties.method = "min")
data_2021_ranking$intangibles_rank <- rank(-data_2021_ranking$`intangibles_cost_recovery`,ties.method = "min")

data_2021_ranking$waverage_rank <- rank(-data_2021_ranking$`waverage`, ties.method = "min")

data_2021_ranking <- subset(data_2021_ranking, select = -c(year, iso_3, average, gdp))

#Order columns and sort data
data_2021_ranking <- data_2021_ranking[c("country", "waverage_rank", "waverage", "buildings_rank", "buildings_cost_recovery", "machines_rank", "machines_cost_recovery", "intangibles_rank", "intangibles_cost_recovery")]

data_2021_ranking <- data_2021_ranking[order(-data_2021_ranking$waverage, data_2021_ranking$country),]

#Round digits
data_2021_ranking$waverage <- round(data_2021_ranking$waverage, digits=3)
data_2021_ranking$buildings_cost_recovery <- round(data_2021_ranking$buildings_cost_recovery, digits=3)
data_2021_ranking$machines_cost_recovery <- round(data_2021_ranking$machines_cost_recovery, digits=3)
data_2021_ranking$intangibles_cost_recovery <- round(data_2021_ranking$intangibles_cost_recovery, digits=3)

#Rename column headers
colnames(data_2021_ranking)[colnames(data_2021_ranking)=="country"] <- "Country"
colnames(data_2021_ranking)[colnames(data_2021_ranking)=="waverage"] <- "Weighted Average Allowance"
colnames(data_2021_ranking)[colnames(data_2021_ranking)=="waverage_rank"] <- "Weighted Average Rank"
colnames(data_2021_ranking)[colnames(data_2021_ranking)=="buildings_cost_recovery"] <- "Buildings Allowance"
colnames(data_2021_ranking)[colnames(data_2021_ranking)=="buildings_rank"] <- "Buildings Rank"
colnames(data_2021_ranking)[colnames(data_2021_ranking)=="machines_cost_recovery"] <- "Machinery Allowance"
colnames(data_2021_ranking)[colnames(data_2021_ranking)=="machines_rank"] <- "Machinery Rank"
colnames(data_2021_ranking)[colnames(data_2021_ranking)=="intangibles_cost_recovery"] <- "Intangibles Allowance"
colnames(data_2021_ranking)[colnames(data_2021_ranking)=="intangibles_rank"] <- "Intangibles Rank"

write.csv(data_2021_ranking, paste(final_outputs,"npv_ranks_2021.csv",sep=""),row.names = FALSE)


#Data for chart: "Net Present Value of Capital Allowances in the OECD, 2000-2020"

#Limit to OECD countries
data_oecd_all_years <- subset(data, subset = iso_3 != "BGR" & iso_3 != "HRV" & iso_3 != "CYP" & iso_3 != "MLT" & iso_3 != "ROU")

#Calculate timeseries averages
data_weighted <- ddply(data_oecd_all_years, .(year),summarize, weighted_average = weighted.mean(waverage, gdp, na.rm = TRUE), average = mean(waverage, na.rm = TRUE),n = length(waverage[is.na(waverage) == FALSE]))

#Limit to years starting in 2000 (data for all OECD countries is available starting in 2000)
data_weighted <- data_weighted[data_weighted$year>1999,]

colnames(data_weighted)[colnames(data_weighted)=="n"] <- "country_count"

write.csv(data_weighted, paste(final_outputs,"npv_weighted_timeseries.csv",sep=""), row.names = FALSE)


#Data for chart: "Statutory Weighted and Unweighted Combined Corporate Income Tax Rates in the OECD, 2000-2020"

#Read in dataset
dataset_list <- get_datasets()
#search_dataset("Corporate", data= dataset_list)
oecd_rates <- ("TABLE_II1")
#dstruc <- get_data_structure(oecd_rates)
#str(dstruc, max.level = 1)
#dstruc$VAR_DESC
#dstruc$CORP_TAX

oecd_rates <- get_dataset("TABLE_II1", start_time = 2000)

#Keep and rename selected columns
oecd_rates <- subset(oecd_rates, oecd_rates$CORP_TAX=="COMB_CIT_RATE")
oecd_rates <- subset(oecd_rates, select = -c(CORP_TAX,TIME_FORMAT))

colnames(oecd_rates)[colnames(oecd_rates)=="obsValue"] <- "rate"
colnames(oecd_rates)[colnames(oecd_rates)=="obsTime"] <- "year"
colnames(oecd_rates)[colnames(oecd_rates)=="COU"] <- "iso_3"

#Add country names
oecd_rates <- merge(oecd_rates, country_names, by='iso_3')

#Add GDP (first rename country names)
oecd_rates$country <- as.character(oecd_rates$country)

oecd_rates$country[oecd_rates$country == "Czechia"] <- "Czech Republic"
oecd_rates$country[oecd_rates$country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
oecd_rates$country[oecd_rates$country == "Republic of Korea"] <- "Korea"
oecd_rates$country[oecd_rates$country == "United States of America"] <- "United States"

oecd_rates <- merge(oecd_rates, gdp_long, by =c("country", "year"), all=FALSE)

#Weigh corporate rates by GDP
oecd_rates_weighted <- ddply(oecd_rates, .(year),summarize, weighted_average = weighted.mean(rate, gdp, na.rm = TRUE), average = mean(rate, na.rm = TRUE),n = length(rate[is.na(rate) == FALSE]))

write.csv(oecd_rates_weighted, paste(final_outputs,"cit_rates_timeseries.csv",sep=""),row.names = FALSE)


#Data for map: "Net Present Value of Capital Allowances in Europe"

#Keep European countries and the year 2021
data_europe_2021 <- subset(data, year==2021)
data_europe_2021 <- subset(data_europe_2021, subset = iso_3 != "AUS" & iso_3 != "CAN" & iso_3 != "CHL" & iso_3 != "COL" & iso_3 != "CRI" & iso_3 != "ISR" & iso_3 != "JPN" & iso_3 != "KOR" & iso_3 != "MEX" & iso_3 != "NZL" & iso_3 != "USA")

#Drop columns that are not needed
data_europe_2021 <- subset(data_europe_2021, select = c(iso_3, country, year, waverage))

#Sort data
data_europe_2021 <- data_europe_2021[order(-data_europe_2021$waverage, data_europe_2021$country),]

#Add ranking
data_europe_2021$rank <- rank(-data_europe_2021$`waverage`,ties.method = "min")

write.csv(data_europe_2021, paste(final_outputs,"npv_europe.csv",sep=""),row.names = FALSE)


#Data for chart: "Net Present Value of Capital Allowances in the EU compared to CCTB"

#Limit to EU countries and 2021
data_eu27_2021 <- subset(data, year==2021)
data_eu27_2021 <- subset(data_eu27_2021, subset = iso_3 != "AUS" & iso_3 != "CAN" & iso_3 != "CHL" & iso_3 != "COL" & iso_3 != "ISL" & iso_3 != "ISR" & iso_3 != "JPN" & iso_3 != "KOR" & iso_3 != "MEX" & iso_3 != "NZL" & iso_3 != "NOR" & iso_3 != "CHE" & iso_3 != "TUR" & iso_3 != "GBR" & iso_3 != "USA")

#Drop columns that are not needed
data_eu27_2021 <- subset(data_eu27_2021, select = c(iso_3, country, year, waverage))

#Sort data
data_eu27_2021 <- data_eu27_2021[order(-data_eu27_2021$waverage, data_eu27_2021$country),]

#Add weighted average of capital allowances under CCTB
cctb <- data.frame(iso_3 = c("CCTB"), country = c("CCTB"), year = c(2021), waverage = c(0.673))
data_eu27_2021 <- rbind(data_eu27_2021, cctb)

write.csv(data_eu27_2021, paste(final_outputs,"eu_cctb.csv",sep=""),row.names = FALSE)


#Data for chart: "Net Present Value of Capital Allowances by Asset Type in the OECD, 2021"

#Calculate averages by asset type
average_assets <- ddply(data_oecd_2021, .(year),summarize, average_building = mean(buildings_cost_recovery, na.rm = TRUE), average_machines = mean(machines_cost_recovery, na.rm = TRUE), average_intangibles = mean(intangibles_cost_recovery, na.rm = TRUE))

write.csv(average_assets, paste(final_outputs,"asset_averages.csv",sep=""),row.names = FALSE)