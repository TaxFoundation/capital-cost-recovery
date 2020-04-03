###Capital cost recovery model###


#Clear working environment#
rm(list=ls())
gc()


#Set directory#
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Install packages#

library(readxl)
library(reshape2)
library(plyr)
library(OECD)


#Read in dataset containing depreciation data####
data <- read.csv("source-data/cost_recovery_data.csv")


#Limit countries to OECD and EU countries
data <- data[which(data$country=="AUS" 
                   | data$country=="AUT"
                   | data$country=="BEL"
                   | data$country=="BGR"
                   | data$country=="CAN" 
                   | data$country=="CHL"
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
data <- subset(data, select = -c(inventoryval, total, statutory_corptax, EATR, EMTR, ace))


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

#Only keep columns with the calculated net present values
data <- subset(data, select = c(country, year, buildings_cost_recovery, machines_cost_recovery, intangibles_cost_recovery))


#Weighing the calculated net present values of each asset by its respective capital stock share (based on Devereux 2012)
data$weighted_machines <- data$machines*.4391081
data$weighted_buildings <- data$buildings*.4116638
data$weighted_intangibles <- data$intangibles*.1492281

data$waverage <- rowSums(data[,c("weighted_machines","weighted_buildings","weighted_intangibles")])
data$average<-rowMeans(data[,c("machines_cost_recovery","buildings_cost_recovery","intangibles_cost_recovery")])

#Drop columns with weighted net present values by asset type
data <- subset(data, select = -c(weighted_machines, weighted_buildings, weighted_intangibles))


#Import and match country names by ISO-3 codes#####

#Read in country name file
country_names <- read.csv("source-data/country_codes.csv")

#Keep and rename selected columns
country_names <- subset(country_names, select = c(official_name_en, ISO3166.1.Alpha.3))

colnames(country_names)[colnames(country_names)=="official_name_en"] <- "country"
colnames(country_names)[colnames(country_names)=="ISO3166.1.Alpha.3"] <- "iso_3"

#Rename column "country" in data
colnames(data)[colnames(data)=="country"] <- "iso_3"

#Add country names to data
data <- merge(country_names, data, by='iso_3')


#Adding GDP to the dataset#######

#Reading in and merging GDP datasets
gdp_historical <- read_excel("source-data/gdp_historical.xlsx", range = "A12:AN230")
gdp_projected <- read_excel("source-data/gdp_projected.xlsx", range = "A11:J232")

gdp_historical$Country[gdp_historical$Country == "UK"] <- "United Kingdom"

gdp_projected <- subset(gdp_projected, select = c(Country, `2019`))
gdp <- merge(gdp_historical,gdp_projected, by="Country")
colnames(gdp)[colnames(gdp)=="Country"] <- "country"

#Renaming country names so data and gdp can be matched
gdp$country <- as.character(gdp$country)
data$country <- as.character(data$country)

data$country[data$country == "Czechia"] <- "Czech Republic"
data$country[data$country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
data$country[data$country == "Republic of Korea"] <- "Korea"
data$country[data$country == "United States of America"] <- "United States"

#Change format of GDP data from wide to long
gdp_long <- (melt(gdp, id=c("country")))
colnames(gdp_long)[colnames(gdp_long)=="variable"] <- "year"
colnames(gdp_long)[colnames(gdp_long)=="value"] <- "gdp"

#Merge net present value data with GDP data
data <- merge(data, gdp_long, by =c("country", "year"), all=FALSE)

#Write data file#
write.csv(data, "final-data/npv_all_years.csv")


#Create output tables and data for the graphs included in the report#####

#Main overview table: "Net Present Value of Capital Allowances in OECD Countries, 2019"

#Limit to OECD countries and 2019
data_oecd_2019 <- subset(data, year==2019)
data_oecd_2019 <- subset(data_oecd_2019, subset = iso_3 != "BGR" & iso_3 != "HRV" & iso_3 != "CYP" & iso_3 != "MLT" & iso_3 != "ROU")

#Create rankings
data_2019_ranking <- data_oecd_2019

data_2019_ranking$buildings_rank <- rank(-data_2019_ranking$`buildings_cost_recovery`,ties.method = "min")
data_2019_ranking$machines_rank <- rank(-data_2019_ranking$`machines_cost_recovery`,ties.method = "min")
data_2019_ranking$intangibles_rank <- rank(-data_2019_ranking$`intangibles_cost_recovery`,ties.method = "min")

data_2019_ranking$waverage_rank <- rank(-data_2019_ranking$`waverage`, ties.method = "min")

data_2019_ranking <- subset(data_2019_ranking, select = -c(year, iso_3, average, gdp))

#Order columns and sort data
data_2019_ranking <- data_2019_ranking[c("country", "waverage_rank", "waverage", "buildings_rank", "buildings_cost_recovery", "machines_rank", "machines_cost_recovery", "intangibles_rank", "intangibles_cost_recovery")]

data_2019_ranking <- data_2019_ranking[order(-data_2019_ranking$waverage, data_2019_ranking$country),]

#Round digits
data_2019_ranking$waverage <- round(data_2019_ranking$waverage, digits=3)
data_2019_ranking$buildings_cost_recovery <- round(data_2019_ranking$buildings_cost_recovery, digits=3)
data_2019_ranking$machines_cost_recovery <- round(data_2019_ranking$machines_cost_recovery, digits=3)
data_2019_ranking$intangibles_cost_recovery <- round(data_2019_ranking$intangibles_cost_recovery, digits=3)

#Rename column headers
colnames(data_2019_ranking)[colnames(data_2019_ranking)=="country"] <- "Country"
colnames(data_2019_ranking)[colnames(data_2019_ranking)=="waverage"] <- "Weighted Average Allowance"
colnames(data_2019_ranking)[colnames(data_2019_ranking)=="waverage_rank"] <- "Weighted Average Rank"
colnames(data_2019_ranking)[colnames(data_2019_ranking)=="buildings_cost_recovery"] <- "Buildings Allowance"
colnames(data_2019_ranking)[colnames(data_2019_ranking)=="buildings_rank"] <- "Buildings Rank"
colnames(data_2019_ranking)[colnames(data_2019_ranking)=="machines_cost_recovery"] <- "Machinery Allowance"
colnames(data_2019_ranking)[colnames(data_2019_ranking)=="machines_rank"] <- "Machinery Rank"
colnames(data_2019_ranking)[colnames(data_2019_ranking)=="intangibles_cost_recovery"] <- "Intangibles Allowance"
colnames(data_2019_ranking)[colnames(data_2019_ranking)=="intangibles_rank"] <- "Intangibles Rank"

write.csv(data_2019_ranking, "final-outputs/npv_ranks_2019.csv")


#Data for graph: "Net Present Value of Capital Allowances in the OECD, 2000-2019"

#Limit to OECD countries
data_oecd_all_years <- subset(data, subset = iso_3 != "BGR" & iso_3 != "HRV" & iso_3 != "CYP" & iso_3 != "MLT" & iso_3 != "ROU")

#Calculate timeseries averages
data_weighted <- ddply(data_oecd_all_years, .(year),summarize, weighted_average = weighted.mean(waverage, gdp, na.rm = TRUE), average = mean(waverage, na.rm = TRUE),n = length(waverage[is.na(waverage) == FALSE]))

#Limit to years starting in 2000 (data for all OECD countries is available starting in 2000)
data_weighted <- data_weighted[data_weighted$year>1999,]

write.csv(data_weighted, "final-outputs/npv_weighted_timeseries.csv")


#Data for graph: "Statutory Weighted and Unweighted Combined Corporate Income Tax Rates in the OECD, 2000-2019"

#Read in dataset
dataset_list <- get_datasets()
search_dataset("Corporate", data= dataset_list)
oecd_rates <- ("TABLE_II1")
dstruc <- get_data_structure(oecd_rates)
str(dstruc, max.level = 1)
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

write.csv(oecd_rates_weighted, "final-outputs/cit_rates_timeseries.csv")


#Data for map: "Net Present Value of Capital Allowances in Europe"

#Keep European countries and the year 2019
data_europe_2019 <- subset(data, year==2019)
data_europe_2019 <- subset(data_europe_2019, subset = iso_3 != "AUS" & iso_3 != "CAN" & iso_3 != "CHL" & iso_3 != "ISR" & iso_3 != "JPN" & iso_3 != "KOR" & iso_3 != "MEX" & iso_3 != "NZL" & iso_3 != "USA")

#Drop columns that are not needed
data_europe_2019 <- subset(data_europe_2019, select = c(iso_3, country, year, waverage))

#Sort data
data_europe_2019 <- data_europe_2019[order(-data_europe_2019$waverage, data_europe_2019$country),]

#Add ranking
data_europe_2019$rank <- rank(-data_europe_2019$`waverage`,ties.method = "min")

write.csv(data_europe_2019, "final-outputs/npv_europe.csv")


#Data for chart: "Net Present Value of Capital Allowances in the EU compared to CCTB"

#Limit to EU countries and 2019
data_eu27_2019 <- subset(data, year==2019)
data_eu27_2019 <- subset(data_eu27_2019, subset = iso_3 != "AUS" & iso_3 != "CAN" & iso_3 != "CHL" & iso_3 != "ISL" & iso_3 != "ISR" & iso_3 != "JPN" & iso_3 != "KOR" & iso_3 != "MEX" & iso_3 != "NZL" & iso_3 != "NOR" & iso_3 != "CHE" & iso_3 != "TUR" & iso_3 != "GBR" & iso_3 != "USA")

#Drop columns that are not needed
data_eu27_2019 <- subset(data_eu27_2019, select = c(iso_3, country, year, waverage))

#Sort data
data_eu27_2019 <- data_eu27_2019[order(-data_eu27_2019$waverage, data_eu27_2019$country),]

#Add weighted average of capital allowances under CCTB
cctb <- data.frame(iso_3 = c("CCTB"), country = c("CCTB"), year = c(2019), waverage = c(0.673))
data_eu27_2019 <- rbind(data_eu27_2019, cctb)

write.csv(data_eu27_2019, "final-outputs/eu_cctb.csv")


#Data for chart: "Net Present Value of Capital Allowances by Asset Type in the OECD, 2019"

#Calculate averages by asset type
average_assets <- ddply(data_oecd_2019, .(year),summarize, average_building = mean(buildings_cost_recovery, na.rm = TRUE), average_machines = mean(machines_cost_recovery, na.rm = TRUE), average_intangibles = mean(intangibles_cost_recovery, na.rm = TRUE))

write.csv(average_assets, "final-outputs/asset_averages.csv")