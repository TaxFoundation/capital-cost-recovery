###Cost recovery variables model###


#Clear working environment#
rm(list=ls())
gc()


#Set directory#
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Install packages#

library(readxl)
library(reshape2)
library(plyr)


#Read in dataset containing depreciation data
depdata <- read.csv("source-data/cost_recovery_data.csv")

#Drop non-OECD countries
depdata <- depdata[which(depdata$country=="AUS" 
                   | depdata$country=="AUT"
                   | depdata$country=="BEL" 
                   | depdata$country=="CAN" 
                   | depdata$country=="CHL" 
                   | depdata$country=="CZE" 
                   | depdata$country=="DNK" 
                   | depdata$country=="EST" 
                   | depdata$country=="FIN" 
                   | depdata$country=="FRA"
                   | depdata$country=="DEU"
                   | depdata$country=="GRC"
                   | depdata$country=="HUN"
                   | depdata$country=="ISL"
                   | depdata$country=="IRL"
                   | depdata$country=="ISR"
                   | depdata$country=="ITA"
                   | depdata$country=="JPN"
                   | depdata$country=="KOR"
                   | depdata$country=="LVA"
                   | depdata$country=="LTU"
                   | depdata$country=="LUX"
                   | depdata$country=="MEX"
                   | depdata$country=="NLD"
                   | depdata$country=="NZL"
                   | depdata$country=="NOR"
                   | depdata$country=="POL"
                   | depdata$country=="PRT"
                   | depdata$country=="SVK"
                   | depdata$country=="SVN"
                   | depdata$country=="ESP"
                   | depdata$country=="SWE"
                   | depdata$country=="CHE"
                   | depdata$country=="TUR"
                   | depdata$country=="GBR"
                   | depdata$country=="USA"),]

#Drop columns that are not needed
depdata <- subset(depdata, select = -c(inventoryval, total, statutory_corptax, EATR, EMTR, ace))


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

#NOT USED?
DBSL1 <- function(rate1,year1,rate2,year2,i){
  value <- 1
  DB <- 0
  SL <- 0
  for (x in 0:(year1-1)){
    DB <- DB + (rate1*(1-rate1)^x)/(1+i)^x
  }
  SL <- ((rate2*(1+i))/i)*(1-(1^(year2)/(1+i)^(year2)))/(1+i)^(year1)
  return(DB+SL)
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


#Debug summarys#
summary(depdata)
summary(depdata$taxdepbuildtype)
summary(depdata$taxdepmachtype)
summary(depdata$taxdepintangibltype)


#Replace odd depreciation systems ("SL3" and "DB DB SL")#

#Treat SL3 as SL2
depdata[c("taxdepbuildtype", "taxdepmachtype", "taxdepintangibltype")] <- as.data.frame(sapply(depdata[c("taxdepbuildtype", "taxdepmachtype", "taxdepintangibltype")], function(x) gsub("SL3", "SL2", x)))

#Treat "DB DB SL" as DB with switch to SL ("DB DB SL" -> "DB or SL")

###or really as initial DB??

depdata[c("taxdepbuildtype", "taxdepmachtype", "taxdepintangibltype")] <- as.data.frame(sapply(depdata[c("taxdepbuildtype", "taxdepmachtype", "taxdepintangibltype")], function(x) gsub("DB DB SL", "initialDB", x)))


#Corrections to the dataset#

#Ireland's machine schedules are messed up for the years 1988-1991 (they are way too high). We assume that these are the fixes:
depdata[c('taxdepmachtimedb')][depdata$country == "IRL" & depdata$year >= 1988 & depdata$year <= 1991,] <- 1

#The US' 3-schedule straight-line ACRS for machinery is coded incorrectly for the years 1983-1986 (since this model does not support SL3 it is assumed to be SL2)
depdata[c('taxdepmachtimesl')][depdata$country == "USA" & depdata$year >1982 & depdata$year<1987,] <- 4


#Calculate net present values for the different asset types#

#machines_cost_recovery

#DB
depdata$machines_cost_recovery[depdata$taxdepmachtype == "DB" & !is.na(depdata$taxdepmachtype)] <- DB(depdata$taxdeprmachdb[depdata$taxdepmachtype == "DB" & !is.na(depdata$taxdepmachtype)],0.075)

#SL
depdata$machines_cost_recovery[depdata$taxdepmachtype == "SL" & !is.na(depdata$taxdepmachtype)] <- SL(depdata$taxdeprmachsl[depdata$taxdepmachtype == "SL" & !is.na(depdata$taxdepmachtype)],0.075)

#initialDB
depdata$machines_cost_recovery[depdata$taxdepmachtype == "initialDB" & !is.na(depdata$taxdepmachtype)] <- initialDB(depdata$taxdeprmachdb[depdata$taxdepmachtype == "initialDB" & !is.na(depdata$taxdepmachtype)],
  depdata$taxdeprmachsl[depdata$taxdepmachtype == "initialDB" & !is.na(depdata$taxdepmachtype)], 0.075)

#DB or SL
depdata$machines_cost_recovery[depdata$taxdepmachtype == "DB or SL" & !is.na(depdata$taxdepmachtype)] <- DBSL2(depdata$taxdeprmachdb[depdata$taxdepmachtype == "DB or SL" & !is.na(depdata$taxdepmachtype)],
  depdata$taxdepmachtimedb[depdata$taxdepmachtype == "DB or SL" & !is.na(depdata$taxdepmachtype)],
  depdata$taxdeprmachsl[depdata$taxdepmachtype == "DB or SL" & !is.na(depdata$taxdepmachtype)],
  depdata$taxdepmachtimesl[depdata$taxdepmachtype == "DB or SL" & !is.na(depdata$taxdepmachtype)], 0.075)

#SL2
depdata$machines_cost_recovery[depdata$taxdepmachtype == "SL2" & !is.na(depdata$taxdepmachtype)] <- SL2(depdata$taxdeprmachdb[depdata$taxdepmachtype == "SL2" & !is.na(depdata$taxdepmachtype)],
  depdata$taxdepmachtimedb[depdata$taxdepmachtype == "SL2" & !is.na(depdata$taxdepmachtype)],
  depdata$taxdeprmachsl[depdata$taxdepmachtype == "SL2" & !is.na(depdata$taxdepmachtype)],
  depdata$taxdepmachtimesl[depdata$taxdepmachtype == "SL2" & !is.na(depdata$taxdepmachtype)], 0.075)

#SLITA
depdata$machines_cost_recovery[depdata$taxdepmachtype == "SLITA" & !is.na(depdata$taxdepmachtype)] <- SL(depdata$taxdeprmachsl[depdata$taxdepmachtype == "SLITA" & !is.na(depdata$taxdepmachtype)],0.075)

#CZK
for (x in 1:length(depdata$taxdeprmachdb)){
  if(grepl("CZK",depdata$taxdepmachtype[x]) == TRUE){
    depdata$machines_cost_recovery[x] <- CZK(depdata$taxdeprmachdb[x], 0.075)
  }
}


#buildings_cost_recovery

#DB
depdata$buildings_cost_recovery[depdata$taxdepbuildtype == "DB" & !is.na(depdata$taxdepbuildtype)] <- DB(depdata$taxdeprbuilddb[depdata$taxdepbuildtype == "DB" & !is.na(depdata$taxdepbuildtype)],0.075)

#SL
depdata$buildings_cost_recovery[depdata$taxdepbuildtype == "SL" & !is.na(depdata$taxdepbuildtype)] <- SL(depdata$taxdeprbuildsl[depdata$taxdepbuildtype == "SL" & !is.na(depdata$taxdepbuildtype)],0.075)

#initialDB
depdata$buildings_cost_recovery[depdata$taxdepbuildtype == "initialDB" & !is.na(depdata$taxdepbuildtype)] <- initialDB(depdata$taxdeprbuilddb[depdata$taxdepbuildtype == "initialDB" & !is.na(depdata$taxdepbuildtype)],
  depdata$taxdeprbuildsl[depdata$taxdepbuildtype == "initialDB" & !is.na(depdata$taxdepbuildtype)], 0.075)

#DB or SL
depdata$buildings_cost_recovery[depdata$taxdepbuildtype == "DB or SL" & !is.na(depdata$taxdepbuildtype)] <- DBSL2(depdata$taxdeprbuilddb[depdata$taxdepbuildtype == "DB or SL" & !is.na(depdata$taxdepbuildtype)],
  depdata$taxdeprbuildtimedb[depdata$taxdepbuildtype == "DB or SL" & !is.na(depdata$taxdepbuildtype)],
  depdata$taxdeprbuildsl[depdata$taxdepbuildtype == "DB or SL" & !is.na(depdata$taxdepbuildtype)],
  depdata$taxdeprbuildtimesl[depdata$taxdepbuildtype == "DB or SL" & !is.na(depdata$taxdepbuildtype)], 0.075)

#SL2
depdata$buildings_cost_recovery[depdata$taxdepbuildtype == "SL2" & !is.na(depdata$taxdepbuildtype)] <- SL2(depdata$taxdeprbuilddb[depdata$taxdepbuildtype == "SL2" & !is.na(depdata$taxdepbuildtype)],
  depdata$taxdeprbuildtimedb[depdata$taxdepbuildtype == "SL2" & !is.na(depdata$taxdepbuildtype)],
  depdata$taxdeprbuildsl[depdata$taxdepbuildtype == "SL2" & !is.na(depdata$taxdepbuildtype)],
  depdata$taxdeprbuildtimesl[depdata$taxdepbuildtype == "SL2" & !is.na(depdata$taxdepbuildtype)], 0.075)

#SLITA
depdata$buildings_cost_recovery[depdata$taxdepbuildtype == "SLITA" & !is.na(depdata$taxdepbuildtype)]<-SL(depdata$taxdeprbuildsl[depdata$taxdepbuildtype == "SLITA" & !is.na(depdata$taxdepbuildtype)],0.075)

#CZK
for (x in 1:length(depdata$taxdeprbuilddb)){
  if(grepl("CZK",depdata$taxdepbuildtype[x]) == TRUE){
    depdata$buildings_cost_recovery[x] <- CZK(depdata$taxdeprbuilddb[x], 0.075)
  }
}

#intangibles_cost_recovery

#DB
depdata$intangibles_cost_recovery[depdata$taxdepintangibltype == "DB" & !is.na(depdata$taxdepintangibltype)] <- DB(depdata$taxdeprintangibldb[depdata$taxdepintangibltype == "DB" & !is.na(depdata$taxdepintangibltype)], 0.075)

#SL
depdata$intangibles_cost_recovery[depdata$taxdepintangibltype == "SL" & !is.na(depdata$taxdepintangibltype)] <- SL(depdata$taxdeprintangiblsl[depdata$taxdepintangibltype == "SL" & !is.na(depdata$taxdepintangibltype)], 0.075)

#initialDB
depdata$intangibles_cost_recovery[depdata$taxdepintangibltype == "initialDB" & !is.na(depdata$taxdepintangibltype)] <- initialDB(depdata$taxdeprintangibldb[depdata$taxdepintangibltype == "initialDB" & !is.na(depdata$taxdepintangibltype)],
  depdata$taxdeprintangiblsl[depdata$taxdepintangibltype == "initialDB" & !is.na(depdata$taxdepintangibltype)], 0.075)

#DB or SL
depdata$intangibles_cost_recovery[depdata$taxdepintangibltype == "DB or SL" & !is.na(depdata$taxdepintangibltype)] <- DBSL2(depdata$taxdeprintangibldb[depdata$taxdepintangibltype == "DB or SL" & !is.na(depdata$taxdepintangibltype)],
  depdata$taxdepintangibltimedb[depdata$taxdepintangibltype == "DB or SL" & !is.na(depdata$taxdepintangibltype)],
  depdata$taxdeprintangiblsl[depdata$taxdepintangibltype == "DB or SL" & !is.na(depdata$taxdepintangibltype)],
  depdata$taxdepintangibltimesl[depdata$taxdepintangibltype == "DB or SL" & !is.na(depdata$taxdepintangibltype)], 0.075)

#SL2
depdata$intangibles_cost_recovery[depdata$taxdepintangibltype == "SL2" & !is.na(depdata$taxdepintangibltype)] <- SL2(depdata$taxdeprintangibldb[depdata$taxdepintangibltype == "SL2" & !is.na(depdata$taxdepintangibltype)],
  depdata$taxdepintangibltimedb[depdata$taxdepintangibltype == "SL2" & !is.na(depdata$taxdepintangibltype)],
  depdata$taxdeprintangiblsl[depdata$taxdepintangibltype == "SL2" & !is.na(depdata$taxdepintangibltype)],
  depdata$taxdepintangibltimesl[depdata$taxdepintangibltype == "SL2" & !is.na(depdata$taxdepintangibltype)], 0.075)

#In 2000, Estonia moved to a cash-flow type business tax. All allowances need to be coded as 1
depdata[c('intangibles_cost_recovery','machines_cost_recovery','buildings_cost_recovery')][depdata$country == "EST" & depdata$year >=2000,] <- 1

#In 2018, Latvia also moved to a cash-flow type business tax
depdata[c('intangibles_cost_recovery','machines_cost_recovery','buildings_cost_recovery')][depdata$country == "LVA" & depdata$year >=2018,] <- 1

#In fall 2018, Canada introduced full expensing for machinery
depdata[c('machines_cost_recovery')][depdata$country == "CAN" & depdata$year >= 2018,] <- 1

#Adjust USA data to include bonus depreciation for machinery
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2002,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2002,] * 0.70) + 0.30
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2003,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2003,] * 0.70) + 0.30
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2004,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2004,] * 0.50) + 0.50
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2008,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2008,] * 0.50) + 0.50
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2009,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2009,] * 0.50) + 0.50
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2010,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2010,] * 0.50) + 0.50
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2011,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2011,] * 0.00) + 1.00
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2012,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2012,] * 0.50) + 0.50
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2013,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2013,] * 0.50) + 0.50
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2014,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2014,] * 0.50) + 0.50
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2015,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2015,] * 0.50) + 0.50
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2016,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2016,] * 0.50) + 0.50
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2017,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2017,] * 0.50) + 0.50
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2018,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2018,] * 0.00) + 1.00
depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2019,] <- (depdata[c('machines_cost_recovery')][depdata$country == "USA" & depdata$year == 2019,] * 0.00) + 1.00

#Only keep columns with the calculated net present values
depdata <- subset(depdata, select = c(country, year, buildings_cost_recovery, machines_cost_recovery, intangibles_cost_recovery))


#Weighing the calculated net present values of each asset by its respective capital stock share (based on Devereux 2012)
depdata$weighted_machines <- depdata$machines*.4391081
depdata$weighted_buildings <- depdata$buildings*.4116638
depdata$weighted_intangibles <- depdata$intangibles*.1492281

depdata$waverage <- rowSums(depdata[,c("weighted_machines","weighted_buildings","weighted_intangibles")])
depdata$average<-rowMeans(depdata[,c("machines_cost_recovery","buildings_cost_recovery","intangibles_cost_recovery")])

#Drop columns with weighted net present values by asset type
depdata <- subset(depdata, select = -c(weighted_machines, weighted_buildings, weighted_intangibles))


#Import and match country names with ISO-3 codes#

#Read in country name file
country_names <- read.csv("source-data/country_codes.csv")

#Keep and rename selected columns
country_names <- subset(country_names, select = c(official_name_en, ISO3166.1.Alpha.3))

colnames(country_names)[colnames(country_names)=="official_name_en"] <- "country"
colnames(country_names)[colnames(country_names)=="ISO3166.1.Alpha.3"] <- "iso_3"

#Rename column "country" in depdata
colnames(depdata)[colnames(depdata)=="country"] <- "iso_3"

#Add country names to depdata
depdata <- merge(country_names, depdata, by='iso_3')



#Adding GDP to the dataset###

#Reading in and merging GDP datasets
gdp_historical <- read_excel("source-data/gdp_historical.xlsx", range = "A12:AN230")
gdp_projected <- read_excel("source-data/gdp_projected.xlsx", range = "A11:J232")

gdp_historical$Country[gdp_historical$Country == "UK"] <- "United Kingdom"

gdp_projected <- subset(gdp_projected, select = c(Country, `2019`))
gdp <- merge(gdp_historical,gdp_projected, by="Country")
colnames(gdp)[colnames(gdp)=="Country"] <- "country"

#Renaming country names so depdata and gdp can be matched
gdp$country <- as.character(gdp$country)
depdata$country <- as.character(depdata$country)

depdata$country[depdata$country == "Czechia"] <- "Czech Republic"
depdata$country[depdata$country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
depdata$country[depdata$country == "Republic of Korea"] <- "Korea"
depdata$country[depdata$country == "United States of America"] <- "United States"


#Change format of GDP data from wide to long
gdp_long <- (melt(gdp, id=c("country")))
colnames(gdp_long)[colnames(gdp_long)=="variable"] <- "year"
colnames(gdp_long)[colnames(gdp_long)=="value"] <- "gdp"

#Merge net present value data with GDP data
depdata <- merge(depdata, gdp_long, by =c("country", "year"), all=FALSE)

#Weigh weighted net present values by GDP for all years
#depdata$waverage_by_gdp <- weighted.mean(depdata$waverage, depdata$gdp, na.rm = TRUE)


#Write data file#
write.csv(depdata, "final-data/npv_all_years.csv")


#Create output tables and data for the graphs included in the report#

#Main overview table: "Net Present Value of Capital Allowances in OECD Countries, 2019" (Table 1)

depdata_2019 <- subset(depdata, year==2019)

depdata_2019_ranking <- depdata_2019

depdata_2019_ranking$buildings_rank <- rank(-depdata_2019_ranking$`buildings_cost_recovery`,ties.method = "min")
depdata_2019_ranking$machines_rank <- rank(-depdata_2019_ranking$`machines_cost_recovery`,ties.method = "min")
depdata_2019_ranking$intangibles_rank <- rank(-depdata_2019_ranking$`intangibles_cost_recovery`,ties.method = "min")

depdata_2019_ranking$waverage_rank <- rank(-depdata_2019_ranking$`waverage`,ties.method = "min")

depdata_2019_ranking <- subset(depdata_2019_ranking, select = -c(year, iso_3, average, gdp))

depdata_2019_ranking <- depdata_2019_ranking[c("country", "waverage_rank", "waverage", "buildings_rank", "buildings_cost_recovery", "machines_rank", "machines_cost_recovery", "intangibles_rank", "intangibles_cost_recovery")]

depdata_2019_ranking <- depdata_2019_ranking[order(-depdata_2019_ranking$waverage, depdata_2019_ranking$country),]

depdata_2019_ranking$waverage <- round(depdata_2019_ranking$waverage, digits=3)
depdata_2019_ranking$buildings_cost_recovery <- round(depdata_2019_ranking$buildings_cost_recovery, digits=3)
depdata_2019_ranking$machines_cost_recovery <- round(depdata_2019_ranking$machines_cost_recovery, digits=3)
depdata_2019_ranking$intangibles_cost_recovery <- round(depdata_2019_ranking$intangibles_cost_recovery, digits=3)

colnames(depdata_2019_ranking)[colnames(depdata_2019_ranking)=="country"] <- "Country"
colnames(depdata_2019_ranking)[colnames(depdata_2019_ranking)=="waverage"] <- "Weighted Average Allowance"
colnames(depdata_2019_ranking)[colnames(depdata_2019_ranking)=="waverage_rank"] <- "Weighted Average Rank"
colnames(depdata_2019_ranking)[colnames(depdata_2019_ranking)=="buildings_cost_recovery"] <- "Buildings Allowance"
colnames(depdata_2019_ranking)[colnames(depdata_2019_ranking)=="buildings_rank"] <- "Buildings Rank"
colnames(depdata_2019_ranking)[colnames(depdata_2019_ranking)=="machines_cost_recovery"] <- "Machinery Allowance"
colnames(depdata_2019_ranking)[colnames(depdata_2019_ranking)=="machines_rank"] <- "Machinery Rank"
colnames(depdata_2019_ranking)[colnames(depdata_2019_ranking)=="intangibles_cost_recovery"] <- "Intangibles Allowance"
colnames(depdata_2019_ranking)[colnames(depdata_2019_ranking)=="intangibles_rank"] <- "Intangibles Rank"

write.csv(depdata_2019_ranking, "final-outputs/npv_ranks_2019.csv")


#Data for graph: "Net Present Value of Capital Allowances, OECD, 2000-2019" (Figure 1) (data going back to 1980 is available but )

depdata_weighted <- ddply(depdata, .(year),summarize, weighted.average = weighted.mean(waverage, gdp, na.rm = TRUE), average = mean(waverage, na.rm = TRUE),n = length(waverage[is.na(waverage) == FALSE]))
depdata_weighted <- depdata_weighted[depdata_weighted$year>1999,]

write.csv(depdata_2019_ranking, "final-outputs/npv_ranks_2019.csv")




#data[c('taxdeprbuildtimesl')][data$country == "USA" & data$year == 2018,] <- 40
#data[c('taxdeprbuildsl')][data$country == "USA" & data$year == 2018,] <- 0.025

data$year<-data$year+1

#gdp_long <- subset(gdp_iso_long, year != 1996.1)