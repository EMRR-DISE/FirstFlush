# Package ID: edi.244.14 Cataloging System:https://pasta.edirepository.org.
# Data set title: Interagency Ecological Program: Over four decades of juvenile fish monitoring data from the San Francisco Estuary, collected by the Delta Juvenile Fish Monitoring Program, 1976-2025.
# Data set creator:   United States Fish and Wildlife Service -  
# Data set creator:  Jonathan Speegle - United States Fish and Wildlife Service 
# Data set creator:  Claudia Macfarlane - United States Fish and Wildlife Service 
# Data set creator:  Eric Huber - United States Fish and Wildlife Service 
# Data set creator:  Denise Goodman - United States Fish and Wildlife Service 
# Data set creator:  Kate Erly - United States Fish and Wildlife Service 
# Data set creator:  Ryan Cook - United States Fish and Wildlife Service 
# Contact:  Jonathan Speegle -  United States Fish and Wildlife Service  - jonathan_speegle@fws.gov
# Contact:  Claudia Macfarlane -  United States Fish and Wildlife Service  - claudia_macfarlane@fws.gov
# Contact:  Denise Goodman -  United States Fish and Wildlife Service  - denise_goodman@fws.gov
# Contact:  Kate Erly -  United States Fish and Wildlife Service  - kate_erly@fws.gov
# Contact:  Ryan Cook -  United States Fish and Wildlife Service  - john_cook@fws.gov
# Stylesheet v2.16 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")
	      

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/244/14/71c16ead9b8ffa4da7a52da180f601f4" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Location",     
                    "RegionCode",     
                    "StationCode",     
                    "SampleDate",     
                    "SampleTime",     
                    "MethodCode",     
                    "GearConditionCode",     
                    "WeatherCode",     
                    "DO",     
                    "WaterTemp",     
                    "Turbidity",     
                    "TurbidityFnu",     
                    "Secchi",     
                    "SpecificConductance",     
                    "TowNumber",     
                    "SamplingDirection",     
                    "TowDuration",     
                    "FlowDebris",     
                    "FlowmeterStart",     
                    "FlowmeterEnd",     
                    "FlowmeterDifference",     
                    "Volume",     
                    "OrganismCode",     
                    "IEPFishCode",     
                    "CommonName",     
                    "MarkCode",     
                    "StageCode",     
                    "Expression",     
                    "ForkLength",     
                    "RaceByLength",     
                    "TagCode",     
                    "RaceByTag",     
                    "ArchiveID",     
                    "SpecialStudy",     
                    "SpecialStudyID",     
                    "Count"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$Location)!="factor") dt1$Location<- as.factor(dt1$Location)
if (class(dt1$RegionCode)!="factor") dt1$RegionCode<- as.factor(dt1$RegionCode)
if (class(dt1$StationCode)!="factor") dt1$StationCode<- as.factor(dt1$StationCode)                                   
# attempting to convert dt1$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1SampleDate<-as.Date(dt1$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$SampleDate != "",]) == length(tmp1SampleDate[!is.na(tmp1SampleDate)])){dt1$SampleDate <- tmp1SampleDate } else {print("Date conversion failed for dt1$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(dt1$MethodCode)!="factor") dt1$MethodCode<- as.factor(dt1$MethodCode)
if (class(dt1$GearConditionCode)!="factor") dt1$GearConditionCode<- as.factor(dt1$GearConditionCode)
if (class(dt1$WeatherCode)!="factor") dt1$WeatherCode<- as.factor(dt1$WeatherCode)
if (class(dt1$DO)=="factor") dt1$DO <-as.numeric(levels(dt1$DO))[as.integer(dt1$DO) ]               
if (class(dt1$DO)=="character") dt1$DO <-as.numeric(dt1$DO)
if (class(dt1$WaterTemp)=="factor") dt1$WaterTemp <-as.numeric(levels(dt1$WaterTemp))[as.integer(dt1$WaterTemp) ]               
if (class(dt1$WaterTemp)=="character") dt1$WaterTemp <-as.numeric(dt1$WaterTemp)
if (class(dt1$Turbidity)=="factor") dt1$Turbidity <-as.numeric(levels(dt1$Turbidity))[as.integer(dt1$Turbidity) ]               
if (class(dt1$Turbidity)=="character") dt1$Turbidity <-as.numeric(dt1$Turbidity)
if (class(dt1$TurbidityFnu)=="factor") dt1$TurbidityFnu <-as.numeric(levels(dt1$TurbidityFnu))[as.integer(dt1$TurbidityFnu) ]               
if (class(dt1$TurbidityFnu)=="character") dt1$TurbidityFnu <-as.numeric(dt1$TurbidityFnu)
if (class(dt1$Secchi)=="factor") dt1$Secchi <-as.numeric(levels(dt1$Secchi))[as.integer(dt1$Secchi) ]               
if (class(dt1$Secchi)=="character") dt1$Secchi <-as.numeric(dt1$Secchi)
if (class(dt1$SpecificConductance)=="factor") dt1$SpecificConductance <-as.numeric(levels(dt1$SpecificConductance))[as.integer(dt1$SpecificConductance) ]               
if (class(dt1$SpecificConductance)=="character") dt1$SpecificConductance <-as.numeric(dt1$SpecificConductance)
if (class(dt1$TowNumber)=="factor") dt1$TowNumber <-as.numeric(levels(dt1$TowNumber))[as.integer(dt1$TowNumber) ]               
if (class(dt1$TowNumber)=="character") dt1$TowNumber <-as.numeric(dt1$TowNumber)
if (class(dt1$SamplingDirection)!="factor") dt1$SamplingDirection<- as.factor(dt1$SamplingDirection)
if (class(dt1$TowDuration)=="factor") dt1$TowDuration <-as.numeric(levels(dt1$TowDuration))[as.integer(dt1$TowDuration) ]               
if (class(dt1$TowDuration)=="character") dt1$TowDuration <-as.numeric(dt1$TowDuration)
if (class(dt1$FlowDebris)!="factor") dt1$FlowDebris<- as.factor(dt1$FlowDebris)
if (class(dt1$FlowmeterStart)=="factor") dt1$FlowmeterStart <-as.numeric(levels(dt1$FlowmeterStart))[as.integer(dt1$FlowmeterStart) ]               
if (class(dt1$FlowmeterStart)=="character") dt1$FlowmeterStart <-as.numeric(dt1$FlowmeterStart)
if (class(dt1$FlowmeterEnd)=="factor") dt1$FlowmeterEnd <-as.numeric(levels(dt1$FlowmeterEnd))[as.integer(dt1$FlowmeterEnd) ]               
if (class(dt1$FlowmeterEnd)=="character") dt1$FlowmeterEnd <-as.numeric(dt1$FlowmeterEnd)
if (class(dt1$FlowmeterDifference)=="factor") dt1$FlowmeterDifference <-as.numeric(levels(dt1$FlowmeterDifference))[as.integer(dt1$FlowmeterDifference) ]               
if (class(dt1$FlowmeterDifference)=="character") dt1$FlowmeterDifference <-as.numeric(dt1$FlowmeterDifference)
if (class(dt1$Volume)=="factor") dt1$Volume <-as.numeric(levels(dt1$Volume))[as.integer(dt1$Volume) ]               
if (class(dt1$Volume)=="character") dt1$Volume <-as.numeric(dt1$Volume)
if (class(dt1$OrganismCode)!="factor") dt1$OrganismCode<- as.factor(dt1$OrganismCode)
if (class(dt1$IEPFishCode)!="factor") dt1$IEPFishCode<- as.factor(dt1$IEPFishCode)
if (class(dt1$CommonName)!="factor") dt1$CommonName<- as.factor(dt1$CommonName)
if (class(dt1$MarkCode)!="factor") dt1$MarkCode<- as.factor(dt1$MarkCode)
if (class(dt1$StageCode)!="factor") dt1$StageCode<- as.factor(dt1$StageCode)
if (class(dt1$Expression)!="factor") dt1$Expression<- as.factor(dt1$Expression)
if (class(dt1$ForkLength)=="factor") dt1$ForkLength <-as.numeric(levels(dt1$ForkLength))[as.integer(dt1$ForkLength) ]               
if (class(dt1$ForkLength)=="character") dt1$ForkLength <-as.numeric(dt1$ForkLength)
if (class(dt1$RaceByLength)!="factor") dt1$RaceByLength<- as.factor(dt1$RaceByLength)
if (class(dt1$TagCode)!="factor") dt1$TagCode<- as.factor(dt1$TagCode)
if (class(dt1$RaceByTag)!="factor") dt1$RaceByTag<- as.factor(dt1$RaceByTag)
if (class(dt1$ArchiveID)!="factor") dt1$ArchiveID<- as.factor(dt1$ArchiveID)
if (class(dt1$SpecialStudy)!="factor") dt1$SpecialStudy<- as.factor(dt1$SpecialStudy)
if (class(dt1$SpecialStudyID)!="factor") dt1$SpecialStudyID<- as.factor(dt1$SpecialStudyID)
if (class(dt1$Count)=="factor") dt1$Count <-as.numeric(levels(dt1$Count))[as.integer(dt1$Count) ]               
if (class(dt1$Count)=="character") dt1$Count <-as.numeric(dt1$Count)
                
# Convert Missing Values to NA for non-dates
                
dt1$WeatherCode <- as.factor(ifelse((trimws(as.character(dt1$WeatherCode))==trimws("NA")),NA,as.character(dt1$WeatherCode)))
dt1$DO <- ifelse((trimws(as.character(dt1$DO))==trimws("NA")),NA,dt1$DO)               
suppressWarnings(dt1$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DO))==as.character(as.numeric("NA"))),NA,dt1$DO))
dt1$WaterTemp <- ifelse((trimws(as.character(dt1$WaterTemp))==trimws("NA")),NA,dt1$WaterTemp)               
suppressWarnings(dt1$WaterTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WaterTemp))==as.character(as.numeric("NA"))),NA,dt1$WaterTemp))
dt1$Turbidity <- ifelse((trimws(as.character(dt1$Turbidity))==trimws("NA")),NA,dt1$Turbidity)               
suppressWarnings(dt1$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Turbidity))==as.character(as.numeric("NA"))),NA,dt1$Turbidity))
dt1$TurbidityFnu <- ifelse((trimws(as.character(dt1$TurbidityFnu))==trimws("NA")),NA,dt1$TurbidityFnu)               
suppressWarnings(dt1$TurbidityFnu <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TurbidityFnu))==as.character(as.numeric("NA"))),NA,dt1$TurbidityFnu))
dt1$Secchi <- ifelse((trimws(as.character(dt1$Secchi))==trimws("NA")),NA,dt1$Secchi)               
suppressWarnings(dt1$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Secchi))==as.character(as.numeric("NA"))),NA,dt1$Secchi))
dt1$SpecificConductance <- ifelse((trimws(as.character(dt1$SpecificConductance))==trimws("NA")),NA,dt1$SpecificConductance)               
suppressWarnings(dt1$SpecificConductance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SpecificConductance))==as.character(as.numeric("NA"))),NA,dt1$SpecificConductance))
dt1$TowNumber <- ifelse((trimws(as.character(dt1$TowNumber))==trimws("NA")),NA,dt1$TowNumber)               
suppressWarnings(dt1$TowNumber <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TowNumber))==as.character(as.numeric("NA"))),NA,dt1$TowNumber))
dt1$SamplingDirection <- as.factor(ifelse((trimws(as.character(dt1$SamplingDirection))==trimws("NA")),NA,as.character(dt1$SamplingDirection)))
dt1$TowDuration <- ifelse((trimws(as.character(dt1$TowDuration))==trimws("NA")),NA,dt1$TowDuration)               
suppressWarnings(dt1$TowDuration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TowDuration))==as.character(as.numeric("NA"))),NA,dt1$TowDuration))
dt1$FlowDebris <- as.factor(ifelse((trimws(as.character(dt1$FlowDebris))==trimws("NA")),NA,as.character(dt1$FlowDebris)))
dt1$FlowmeterStart <- ifelse((trimws(as.character(dt1$FlowmeterStart))==trimws("NA")),NA,dt1$FlowmeterStart)               
suppressWarnings(dt1$FlowmeterStart <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$FlowmeterStart))==as.character(as.numeric("NA"))),NA,dt1$FlowmeterStart))
dt1$FlowmeterEnd <- ifelse((trimws(as.character(dt1$FlowmeterEnd))==trimws("NA")),NA,dt1$FlowmeterEnd)               
suppressWarnings(dt1$FlowmeterEnd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$FlowmeterEnd))==as.character(as.numeric("NA"))),NA,dt1$FlowmeterEnd))
dt1$FlowmeterDifference <- ifelse((trimws(as.character(dt1$FlowmeterDifference))==trimws("NA")),NA,dt1$FlowmeterDifference)               
suppressWarnings(dt1$FlowmeterDifference <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$FlowmeterDifference))==as.character(as.numeric("NA"))),NA,dt1$FlowmeterDifference))
dt1$Volume <- ifelse((trimws(as.character(dt1$Volume))==trimws("NA")),NA,dt1$Volume)               
suppressWarnings(dt1$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Volume))==as.character(as.numeric("NA"))),NA,dt1$Volume))
dt1$MarkCode <- as.factor(ifelse((trimws(as.character(dt1$MarkCode))==trimws("NA")),NA,as.character(dt1$MarkCode)))
dt1$StageCode <- as.factor(ifelse((trimws(as.character(dt1$StageCode))==trimws("NA")),NA,as.character(dt1$StageCode)))
dt1$Expression <- as.factor(ifelse((trimws(as.character(dt1$Expression))==trimws("NA")),NA,as.character(dt1$Expression)))
dt1$ForkLength <- ifelse((trimws(as.character(dt1$ForkLength))==trimws("NA")),NA,dt1$ForkLength)               
suppressWarnings(dt1$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ForkLength))==as.character(as.numeric("NA"))),NA,dt1$ForkLength))
dt1$RaceByLength <- as.factor(ifelse((trimws(as.character(dt1$RaceByLength))==trimws("NA")),NA,as.character(dt1$RaceByLength)))
dt1$TagCode <- as.factor(ifelse((trimws(as.character(dt1$TagCode))==trimws("NA")),NA,as.character(dt1$TagCode)))
dt1$RaceByTag <- as.factor(ifelse((trimws(as.character(dt1$RaceByTag))==trimws("NA")),NA,as.character(dt1$RaceByTag)))
dt1$ArchiveID <- as.factor(ifelse((trimws(as.character(dt1$ArchiveID))==trimws("NA")),NA,as.character(dt1$ArchiveID)))
dt1$SpecialStudy <- as.factor(ifelse((trimws(as.character(dt1$SpecialStudy))==trimws("NA")),NA,as.character(dt1$SpecialStudy)))
dt1$SpecialStudyID <- as.factor(ifelse((trimws(as.character(dt1$SpecialStudyID))==trimws("NA")),NA,as.character(dt1$SpecialStudyID)))


# Here is the structure of the input data frame:
print("dt1) Structure")		    
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

print(" ")
print("Summary of Location")
print(summary(Location))
print(" ")
print("Summary of RegionCode")
print(summary(RegionCode))
print(" ")
print("Summary of StationCode")
print(summary(StationCode))
print(" ")
print("Summary of SampleDate")
print(summary(SampleDate))
print(" ")
print("Summary of SampleTime")
print(summary(SampleTime))
print(" ")
print("Summary of MethodCode")
print(summary(MethodCode))
print(" ")
print("Summary of GearConditionCode")
print(summary(GearConditionCode))
print(" ")
print("Summary of WeatherCode")
print(summary(WeatherCode))
print(" ")
print("Summary of DO")
print(summary(DO))
print(" ")
print("Summary of WaterTemp")
print(summary(WaterTemp))
print(" ")
print("Summary of Turbidity")
print(summary(Turbidity))
print(" ")
print("Summary of TurbidityFnu")
print(summary(TurbidityFnu))
print(" ")
print("Summary of Secchi")
print(summary(Secchi))
print(" ")
print("Summary of SpecificConductance")
print(summary(SpecificConductance))
print(" ")
print("Summary of TowNumber")
print(summary(TowNumber))
print(" ")
print("Summary of SamplingDirection")
print(summary(SamplingDirection))
print(" ")
print("Summary of TowDuration")
print(summary(TowDuration))
print(" ")
print("Summary of FlowDebris")
print(summary(FlowDebris))
print(" ")
print("Summary of FlowmeterStart")
print(summary(FlowmeterStart))
print(" ")
print("Summary of FlowmeterEnd")
print(summary(FlowmeterEnd))
print(" ")
print("Summary of FlowmeterDifference")
print(summary(FlowmeterDifference))
print(" ")
print("Summary of Volume")
print(summary(Volume))
print(" ")
print("Summary of OrganismCode")
print(summary(OrganismCode))
print(" ")
print("Summary of IEPFishCode")
print(summary(IEPFishCode))
print(" ")
print("Summary of CommonName")
print(summary(CommonName))
print(" ")
print("Summary of MarkCode")
print(summary(MarkCode))
print(" ")
print("Summary of StageCode")
print(summary(StageCode))
print(" ")
print("Summary of Expression")
print(summary(Expression))
print(" ")
print("Summary of ForkLength")
print(summary(ForkLength))
print(" ")
print("Summary of RaceByLength")
print(summary(RaceByLength))
print(" ")
print("Summary of TagCode")
print(summary(TagCode))
print(" ")
print("Summary of RaceByTag")
print(summary(RaceByTag))
print(" ")
print("Summary of ArchiveID")
print(summary(ArchiveID))
print(" ")
print("Summary of SpecialStudy")
print(summary(SpecialStudy))
print(" ")
print("Summary of SpecialStudyID")
print(summary(SpecialStudyID))
print(" ")
print("Summary of Count")
print(summary(Count)) 
# Get more details on character variables
                 

print(" ")
print("Summary of Location")
print(summary(as.factor(dt1$Location))) 

print(" ")
print("Summary of RegionCode")
print(summary(as.factor(dt1$RegionCode))) 

print(" ")
print("Summary of StationCode")
print(summary(as.factor(dt1$StationCode))) 

print(" ")
print("Summary of MethodCode")
print(summary(as.factor(dt1$MethodCode))) 

print(" ")
print("Summary of GearConditionCode")
print(summary(as.factor(dt1$GearConditionCode))) 

print(" ")
print("Summary of WeatherCode")
print(summary(as.factor(dt1$WeatherCode))) 

print(" ")
print("Summary of SamplingDirection")
print(summary(as.factor(dt1$SamplingDirection))) 

print(" ")
print("Summary of FlowDebris")
print(summary(as.factor(dt1$FlowDebris))) 

print(" ")
print("Summary of OrganismCode")
print(summary(as.factor(dt1$OrganismCode))) 

print(" ")
print("Summary of IEPFishCode")
print(summary(as.factor(dt1$IEPFishCode))) 

print(" ")
print("Summary of CommonName")
print(summary(as.factor(dt1$CommonName))) 

print(" ")
print("Summary of MarkCode")
print(summary(as.factor(dt1$MarkCode))) 

print(" ")
print("Summary of StageCode")
print(summary(as.factor(dt1$StageCode))) 

print(" ")
print("Summary of Expression")
print(summary(as.factor(dt1$Expression))) 

print(" ")
print("Summary of RaceByLength")
print(summary(as.factor(dt1$RaceByLength))) 

print(" ")
print("Summary of TagCode")
print(summary(as.factor(dt1$TagCode))) 

print(" ")
print("Summary of RaceByTag")
print(summary(as.factor(dt1$RaceByTag))) 

print(" ")
print("Summary of ArchiveID")
print(summary(as.factor(dt1$ArchiveID))) 

print(" ")
print("Summary of SpecialStudy")
print(summary(as.factor(dt1$SpecialStudy))) 

print(" ")
print("Summary of SpecialStudyID")
print(summary(as.factor(dt1$SpecialStudyID)))
detach(dt1)               
        
	      

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/244/14/8064494073eaabf9150b09ff572d4643" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

                   
 dt2 <-read.csv(infile2,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Location",     
                    "RegionCode",     
                    "StationCode",     
                    "SampleDate",     
                    "SampleTime",     
                    "MethodCode",     
                    "GearConditionCode",     
                    "WeatherCode",     
                    "DO",     
                    "WaterTemp",     
                    "Turbidity",     
                    "TurbidityFnu",     
                    "Secchi",     
                    "SpecificConductance",     
                    "TowNumber",     
                    "SamplingDirection",     
                    "TowDuration",     
                    "FlowDebris",     
                    "FlowmeterStart",     
                    "FlowmeterEnd",     
                    "FlowmeterDifference",     
                    "Volume",     
                    "OrganismCode",     
                    "IEPFishCode",     
                    "CommonName",     
                    "MarkCode",     
                    "StageCode",     
                    "Expression",     
                    "ForkLength",     
                    "RaceByLength",     
                    "TagCode",     
                    "RaceByTag",     
                    "ArchiveID",     
                    "SpecialStudy",     
                    "SpecialStudyID",     
                    "Count"    ), check.names=TRUE)
               
unlink(infile2)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt2$Location)!="factor") dt2$Location<- as.factor(dt2$Location)
if (class(dt2$RegionCode)!="factor") dt2$RegionCode<- as.factor(dt2$RegionCode)
if (class(dt2$StationCode)!="factor") dt2$StationCode<- as.factor(dt2$StationCode)                                   
# attempting to convert dt2$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2SampleDate<-as.Date(dt2$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt2[dt2$SampleDate != "",]) == length(tmp2SampleDate[!is.na(tmp2SampleDate)])){dt2$SampleDate <- tmp2SampleDate } else {print("Date conversion failed for dt2$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(dt2$MethodCode)!="factor") dt2$MethodCode<- as.factor(dt2$MethodCode)
if (class(dt2$GearConditionCode)!="factor") dt2$GearConditionCode<- as.factor(dt2$GearConditionCode)
if (class(dt2$WeatherCode)!="factor") dt2$WeatherCode<- as.factor(dt2$WeatherCode)
if (class(dt2$DO)=="factor") dt2$DO <-as.numeric(levels(dt2$DO))[as.integer(dt2$DO) ]               
if (class(dt2$DO)=="character") dt2$DO <-as.numeric(dt2$DO)
if (class(dt2$WaterTemp)=="factor") dt2$WaterTemp <-as.numeric(levels(dt2$WaterTemp))[as.integer(dt2$WaterTemp) ]               
if (class(dt2$WaterTemp)=="character") dt2$WaterTemp <-as.numeric(dt2$WaterTemp)
if (class(dt2$Turbidity)=="factor") dt2$Turbidity <-as.numeric(levels(dt2$Turbidity))[as.integer(dt2$Turbidity) ]               
if (class(dt2$Turbidity)=="character") dt2$Turbidity <-as.numeric(dt2$Turbidity)
if (class(dt2$TurbidityFnu)=="factor") dt2$TurbidityFnu <-as.numeric(levels(dt2$TurbidityFnu))[as.integer(dt2$TurbidityFnu) ]               
if (class(dt2$TurbidityFnu)=="character") dt2$TurbidityFnu <-as.numeric(dt2$TurbidityFnu)
if (class(dt2$Secchi)=="factor") dt2$Secchi <-as.numeric(levels(dt2$Secchi))[as.integer(dt2$Secchi) ]               
if (class(dt2$Secchi)=="character") dt2$Secchi <-as.numeric(dt2$Secchi)
if (class(dt2$SpecificConductance)=="factor") dt2$SpecificConductance <-as.numeric(levels(dt2$SpecificConductance))[as.integer(dt2$SpecificConductance) ]               
if (class(dt2$SpecificConductance)=="character") dt2$SpecificConductance <-as.numeric(dt2$SpecificConductance)
if (class(dt2$TowNumber)=="factor") dt2$TowNumber <-as.numeric(levels(dt2$TowNumber))[as.integer(dt2$TowNumber) ]               
if (class(dt2$TowNumber)=="character") dt2$TowNumber <-as.numeric(dt2$TowNumber)
if (class(dt2$SamplingDirection)!="factor") dt2$SamplingDirection<- as.factor(dt2$SamplingDirection)
if (class(dt2$TowDuration)=="factor") dt2$TowDuration <-as.numeric(levels(dt2$TowDuration))[as.integer(dt2$TowDuration) ]               
if (class(dt2$TowDuration)=="character") dt2$TowDuration <-as.numeric(dt2$TowDuration)
if (class(dt2$FlowDebris)!="factor") dt2$FlowDebris<- as.factor(dt2$FlowDebris)
if (class(dt2$FlowmeterStart)=="factor") dt2$FlowmeterStart <-as.numeric(levels(dt2$FlowmeterStart))[as.integer(dt2$FlowmeterStart) ]               
if (class(dt2$FlowmeterStart)=="character") dt2$FlowmeterStart <-as.numeric(dt2$FlowmeterStart)
if (class(dt2$FlowmeterEnd)=="factor") dt2$FlowmeterEnd <-as.numeric(levels(dt2$FlowmeterEnd))[as.integer(dt2$FlowmeterEnd) ]               
if (class(dt2$FlowmeterEnd)=="character") dt2$FlowmeterEnd <-as.numeric(dt2$FlowmeterEnd)
if (class(dt2$FlowmeterDifference)=="factor") dt2$FlowmeterDifference <-as.numeric(levels(dt2$FlowmeterDifference))[as.integer(dt2$FlowmeterDifference) ]               
if (class(dt2$FlowmeterDifference)=="character") dt2$FlowmeterDifference <-as.numeric(dt2$FlowmeterDifference)
if (class(dt2$Volume)=="factor") dt2$Volume <-as.numeric(levels(dt2$Volume))[as.integer(dt2$Volume) ]               
if (class(dt2$Volume)=="character") dt2$Volume <-as.numeric(dt2$Volume)
if (class(dt2$OrganismCode)!="factor") dt2$OrganismCode<- as.factor(dt2$OrganismCode)
if (class(dt2$IEPFishCode)!="factor") dt2$IEPFishCode<- as.factor(dt2$IEPFishCode)
if (class(dt2$CommonName)!="factor") dt2$CommonName<- as.factor(dt2$CommonName)
if (class(dt2$MarkCode)!="factor") dt2$MarkCode<- as.factor(dt2$MarkCode)
if (class(dt2$StageCode)!="factor") dt2$StageCode<- as.factor(dt2$StageCode)
if (class(dt2$Expression)!="factor") dt2$Expression<- as.factor(dt2$Expression)
if (class(dt2$ForkLength)=="factor") dt2$ForkLength <-as.numeric(levels(dt2$ForkLength))[as.integer(dt2$ForkLength) ]               
if (class(dt2$ForkLength)=="character") dt2$ForkLength <-as.numeric(dt2$ForkLength)
if (class(dt2$RaceByLength)!="factor") dt2$RaceByLength<- as.factor(dt2$RaceByLength)
if (class(dt2$TagCode)!="factor") dt2$TagCode<- as.factor(dt2$TagCode)
if (class(dt2$RaceByTag)!="factor") dt2$RaceByTag<- as.factor(dt2$RaceByTag)
if (class(dt2$ArchiveID)!="factor") dt2$ArchiveID<- as.factor(dt2$ArchiveID)
if (class(dt2$SpecialStudy)!="factor") dt2$SpecialStudy<- as.factor(dt2$SpecialStudy)
if (class(dt2$SpecialStudyID)!="factor") dt2$SpecialStudyID<- as.factor(dt2$SpecialStudyID)
if (class(dt2$Count)=="factor") dt2$Count <-as.numeric(levels(dt2$Count))[as.integer(dt2$Count) ]               
if (class(dt2$Count)=="character") dt2$Count <-as.numeric(dt2$Count)
                
# Convert Missing Values to NA for non-dates
                
dt2$WeatherCode <- as.factor(ifelse((trimws(as.character(dt2$WeatherCode))==trimws("NA")),NA,as.character(dt2$WeatherCode)))
dt2$DO <- ifelse((trimws(as.character(dt2$DO))==trimws("NA")),NA,dt2$DO)               
suppressWarnings(dt2$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DO))==as.character(as.numeric("NA"))),NA,dt2$DO))
dt2$WaterTemp <- ifelse((trimws(as.character(dt2$WaterTemp))==trimws("NA")),NA,dt2$WaterTemp)               
suppressWarnings(dt2$WaterTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$WaterTemp))==as.character(as.numeric("NA"))),NA,dt2$WaterTemp))
dt2$Turbidity <- ifelse((trimws(as.character(dt2$Turbidity))==trimws("NA")),NA,dt2$Turbidity)               
suppressWarnings(dt2$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Turbidity))==as.character(as.numeric("NA"))),NA,dt2$Turbidity))
dt2$TurbidityFnu <- ifelse((trimws(as.character(dt2$TurbidityFnu))==trimws("NA")),NA,dt2$TurbidityFnu)               
suppressWarnings(dt2$TurbidityFnu <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TurbidityFnu))==as.character(as.numeric("NA"))),NA,dt2$TurbidityFnu))
dt2$Secchi <- ifelse((trimws(as.character(dt2$Secchi))==trimws("NA")),NA,dt2$Secchi)               
suppressWarnings(dt2$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Secchi))==as.character(as.numeric("NA"))),NA,dt2$Secchi))
dt2$SpecificConductance <- ifelse((trimws(as.character(dt2$SpecificConductance))==trimws("NA")),NA,dt2$SpecificConductance)               
suppressWarnings(dt2$SpecificConductance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SpecificConductance))==as.character(as.numeric("NA"))),NA,dt2$SpecificConductance))
dt2$TowNumber <- ifelse((trimws(as.character(dt2$TowNumber))==trimws("NA")),NA,dt2$TowNumber)               
suppressWarnings(dt2$TowNumber <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TowNumber))==as.character(as.numeric("NA"))),NA,dt2$TowNumber))
dt2$SamplingDirection <- as.factor(ifelse((trimws(as.character(dt2$SamplingDirection))==trimws("NA")),NA,as.character(dt2$SamplingDirection)))
dt2$TowDuration <- ifelse((trimws(as.character(dt2$TowDuration))==trimws("NA")),NA,dt2$TowDuration)               
suppressWarnings(dt2$TowDuration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TowDuration))==as.character(as.numeric("NA"))),NA,dt2$TowDuration))
dt2$FlowDebris <- as.factor(ifelse((trimws(as.character(dt2$FlowDebris))==trimws("NA")),NA,as.character(dt2$FlowDebris)))
dt2$FlowmeterStart <- ifelse((trimws(as.character(dt2$FlowmeterStart))==trimws("NA")),NA,dt2$FlowmeterStart)               
suppressWarnings(dt2$FlowmeterStart <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$FlowmeterStart))==as.character(as.numeric("NA"))),NA,dt2$FlowmeterStart))
dt2$FlowmeterEnd <- ifelse((trimws(as.character(dt2$FlowmeterEnd))==trimws("NA")),NA,dt2$FlowmeterEnd)               
suppressWarnings(dt2$FlowmeterEnd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$FlowmeterEnd))==as.character(as.numeric("NA"))),NA,dt2$FlowmeterEnd))
dt2$FlowmeterDifference <- ifelse((trimws(as.character(dt2$FlowmeterDifference))==trimws("NA")),NA,dt2$FlowmeterDifference)               
suppressWarnings(dt2$FlowmeterDifference <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$FlowmeterDifference))==as.character(as.numeric("NA"))),NA,dt2$FlowmeterDifference))
dt2$Volume <- ifelse((trimws(as.character(dt2$Volume))==trimws("NA")),NA,dt2$Volume)               
suppressWarnings(dt2$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Volume))==as.character(as.numeric("NA"))),NA,dt2$Volume))
dt2$MarkCode <- as.factor(ifelse((trimws(as.character(dt2$MarkCode))==trimws("NA")),NA,as.character(dt2$MarkCode)))
dt2$StageCode <- as.factor(ifelse((trimws(as.character(dt2$StageCode))==trimws("NA")),NA,as.character(dt2$StageCode)))
dt2$Expression <- as.factor(ifelse((trimws(as.character(dt2$Expression))==trimws("NA")),NA,as.character(dt2$Expression)))
dt2$ForkLength <- ifelse((trimws(as.character(dt2$ForkLength))==trimws("NA")),NA,dt2$ForkLength)               
suppressWarnings(dt2$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ForkLength))==as.character(as.numeric("NA"))),NA,dt2$ForkLength))
dt2$RaceByLength <- as.factor(ifelse((trimws(as.character(dt2$RaceByLength))==trimws("NA")),NA,as.character(dt2$RaceByLength)))
dt2$TagCode <- as.factor(ifelse((trimws(as.character(dt2$TagCode))==trimws("NA")),NA,as.character(dt2$TagCode)))
dt2$RaceByTag <- as.factor(ifelse((trimws(as.character(dt2$RaceByTag))==trimws("NA")),NA,as.character(dt2$RaceByTag)))
dt2$ArchiveID <- as.factor(ifelse((trimws(as.character(dt2$ArchiveID))==trimws("NA")),NA,as.character(dt2$ArchiveID)))
dt2$SpecialStudy <- as.factor(ifelse((trimws(as.character(dt2$SpecialStudy))==trimws("NA")),NA,as.character(dt2$SpecialStudy)))
dt2$SpecialStudyID <- as.factor(ifelse((trimws(as.character(dt2$SpecialStudyID))==trimws("NA")),NA,as.character(dt2$SpecialStudyID)))


# Here is the structure of the input data frame:
print("dt2) Structure")		    
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

print(" ")
print("Summary of Location")
print(summary(Location))
print(" ")
print("Summary of RegionCode")
print(summary(RegionCode))
print(" ")
print("Summary of StationCode")
print(summary(StationCode))
print(" ")
print("Summary of SampleDate")
print(summary(SampleDate))
print(" ")
print("Summary of SampleTime")
print(summary(SampleTime))
print(" ")
print("Summary of MethodCode")
print(summary(MethodCode))
print(" ")
print("Summary of GearConditionCode")
print(summary(GearConditionCode))
print(" ")
print("Summary of WeatherCode")
print(summary(WeatherCode))
print(" ")
print("Summary of DO")
print(summary(DO))
print(" ")
print("Summary of WaterTemp")
print(summary(WaterTemp))
print(" ")
print("Summary of Turbidity")
print(summary(Turbidity))
print(" ")
print("Summary of TurbidityFnu")
print(summary(TurbidityFnu))
print(" ")
print("Summary of Secchi")
print(summary(Secchi))
print(" ")
print("Summary of SpecificConductance")
print(summary(SpecificConductance))
print(" ")
print("Summary of TowNumber")
print(summary(TowNumber))
print(" ")
print("Summary of SamplingDirection")
print(summary(SamplingDirection))
print(" ")
print("Summary of TowDuration")
print(summary(TowDuration))
print(" ")
print("Summary of FlowDebris")
print(summary(FlowDebris))
print(" ")
print("Summary of FlowmeterStart")
print(summary(FlowmeterStart))
print(" ")
print("Summary of FlowmeterEnd")
print(summary(FlowmeterEnd))
print(" ")
print("Summary of FlowmeterDifference")
print(summary(FlowmeterDifference))
print(" ")
print("Summary of Volume")
print(summary(Volume))
print(" ")
print("Summary of OrganismCode")
print(summary(OrganismCode))
print(" ")
print("Summary of IEPFishCode")
print(summary(IEPFishCode))
print(" ")
print("Summary of CommonName")
print(summary(CommonName))
print(" ")
print("Summary of MarkCode")
print(summary(MarkCode))
print(" ")
print("Summary of StageCode")
print(summary(StageCode))
print(" ")
print("Summary of Expression")
print(summary(Expression))
print(" ")
print("Summary of ForkLength")
print(summary(ForkLength))
print(" ")
print("Summary of RaceByLength")
print(summary(RaceByLength))
print(" ")
print("Summary of TagCode")
print(summary(TagCode))
print(" ")
print("Summary of RaceByTag")
print(summary(RaceByTag))
print(" ")
print("Summary of ArchiveID")
print(summary(ArchiveID))
print(" ")
print("Summary of SpecialStudy")
print(summary(SpecialStudy))
print(" ")
print("Summary of SpecialStudyID")
print(summary(SpecialStudyID))
print(" ")
print("Summary of Count")
print(summary(Count)) 
# Get more details on character variables
                 

print(" ")
print("Summary of Location")
print(summary(as.factor(dt2$Location))) 

print(" ")
print("Summary of RegionCode")
print(summary(as.factor(dt2$RegionCode))) 

print(" ")
print("Summary of StationCode")
print(summary(as.factor(dt2$StationCode))) 

print(" ")
print("Summary of MethodCode")
print(summary(as.factor(dt2$MethodCode))) 

print(" ")
print("Summary of GearConditionCode")
print(summary(as.factor(dt2$GearConditionCode))) 

print(" ")
print("Summary of WeatherCode")
print(summary(as.factor(dt2$WeatherCode))) 

print(" ")
print("Summary of SamplingDirection")
print(summary(as.factor(dt2$SamplingDirection))) 

print(" ")
print("Summary of FlowDebris")
print(summary(as.factor(dt2$FlowDebris))) 

print(" ")
print("Summary of OrganismCode")
print(summary(as.factor(dt2$OrganismCode))) 

print(" ")
print("Summary of IEPFishCode")
print(summary(as.factor(dt2$IEPFishCode))) 

print(" ")
print("Summary of CommonName")
print(summary(as.factor(dt2$CommonName))) 

print(" ")
print("Summary of MarkCode")
print(summary(as.factor(dt2$MarkCode))) 

print(" ")
print("Summary of StageCode")
print(summary(as.factor(dt2$StageCode))) 

print(" ")
print("Summary of Expression")
print(summary(as.factor(dt2$Expression))) 

print(" ")
print("Summary of RaceByLength")
print(summary(as.factor(dt2$RaceByLength))) 

print(" ")
print("Summary of TagCode")
print(summary(as.factor(dt2$TagCode))) 

print(" ")
print("Summary of RaceByTag")
print(summary(as.factor(dt2$RaceByTag))) 

print(" ")
print("Summary of ArchiveID")
print(summary(as.factor(dt2$ArchiveID))) 

print(" ")
print("Summary of SpecialStudy")
print(summary(as.factor(dt2$SpecialStudy))) 

print(" ")
print("Summary of SpecialStudyID")
print(summary(as.factor(dt2$SpecialStudyID)))
detach(dt2)               
        
	      

inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/244/14/7bf2e2a18babbdf6fab1f98b53552c55" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")

                   
 dt3 <-read.csv(infile3,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Location",     
                    "RegionCode",     
                    "StationCode",     
                    "SampleDate",     
                    "SampleTime",     
                    "MethodCode",     
                    "GearConditionCode",     
                    "WeatherCode",     
                    "DO",     
                    "WaterTemp",     
                    "Turbidity",     
                    "TurbidityFnu",     
                    "SpecificConductance",     
                    "SiteDisturbance",     
                    "AlternateSite",     
                    "SeineLength",     
                    "SeineWidth",     
                    "SeineDepth",     
                    "Volume",     
                    "OrganismCode",     
                    "IEPFishCode",     
                    "CommonName",     
                    "MarkCode",     
                    "StageCode",     
                    "Expression",     
                    "ForkLength",     
                    "RaceByLength",     
                    "TagCode",     
                    "RaceByTag",     
                    "ArchiveID",     
                    "SpecialStudy",     
                    "SpecialStudyID",     
                    "Count"    ), check.names=TRUE)
               
unlink(infile3)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt3$Location)!="factor") dt3$Location<- as.factor(dt3$Location)
if (class(dt3$RegionCode)!="factor") dt3$RegionCode<- as.factor(dt3$RegionCode)
if (class(dt3$StationCode)!="factor") dt3$StationCode<- as.factor(dt3$StationCode)                                   
# attempting to convert dt3$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp3SampleDate<-as.Date(dt3$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt3[dt3$SampleDate != "",]) == length(tmp3SampleDate[!is.na(tmp3SampleDate)])){dt3$SampleDate <- tmp3SampleDate } else {print("Date conversion failed for dt3$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(dt3$MethodCode)!="factor") dt3$MethodCode<- as.factor(dt3$MethodCode)
if (class(dt3$GearConditionCode)!="factor") dt3$GearConditionCode<- as.factor(dt3$GearConditionCode)
if (class(dt3$WeatherCode)!="factor") dt3$WeatherCode<- as.factor(dt3$WeatherCode)
if (class(dt3$DO)=="factor") dt3$DO <-as.numeric(levels(dt3$DO))[as.integer(dt3$DO) ]               
if (class(dt3$DO)=="character") dt3$DO <-as.numeric(dt3$DO)
if (class(dt3$WaterTemp)=="factor") dt3$WaterTemp <-as.numeric(levels(dt3$WaterTemp))[as.integer(dt3$WaterTemp) ]               
if (class(dt3$WaterTemp)=="character") dt3$WaterTemp <-as.numeric(dt3$WaterTemp)
if (class(dt3$Turbidity)=="factor") dt3$Turbidity <-as.numeric(levels(dt3$Turbidity))[as.integer(dt3$Turbidity) ]               
if (class(dt3$Turbidity)=="character") dt3$Turbidity <-as.numeric(dt3$Turbidity)
if (class(dt3$TurbidityFnu)=="factor") dt3$TurbidityFnu <-as.numeric(levels(dt3$TurbidityFnu))[as.integer(dt3$TurbidityFnu) ]               
if (class(dt3$TurbidityFnu)=="character") dt3$TurbidityFnu <-as.numeric(dt3$TurbidityFnu)
if (class(dt3$SpecificConductance)=="factor") dt3$SpecificConductance <-as.numeric(levels(dt3$SpecificConductance))[as.integer(dt3$SpecificConductance) ]               
if (class(dt3$SpecificConductance)=="character") dt3$SpecificConductance <-as.numeric(dt3$SpecificConductance)
if (class(dt3$SiteDisturbance)!="factor") dt3$SiteDisturbance<- as.factor(dt3$SiteDisturbance)
if (class(dt3$AlternateSite)!="factor") dt3$AlternateSite<- as.factor(dt3$AlternateSite)
if (class(dt3$SeineLength)=="factor") dt3$SeineLength <-as.numeric(levels(dt3$SeineLength))[as.integer(dt3$SeineLength) ]               
if (class(dt3$SeineLength)=="character") dt3$SeineLength <-as.numeric(dt3$SeineLength)
if (class(dt3$SeineWidth)=="factor") dt3$SeineWidth <-as.numeric(levels(dt3$SeineWidth))[as.integer(dt3$SeineWidth) ]               
if (class(dt3$SeineWidth)=="character") dt3$SeineWidth <-as.numeric(dt3$SeineWidth)
if (class(dt3$SeineDepth)=="factor") dt3$SeineDepth <-as.numeric(levels(dt3$SeineDepth))[as.integer(dt3$SeineDepth) ]               
if (class(dt3$SeineDepth)=="character") dt3$SeineDepth <-as.numeric(dt3$SeineDepth)
if (class(dt3$Volume)=="factor") dt3$Volume <-as.numeric(levels(dt3$Volume))[as.integer(dt3$Volume) ]               
if (class(dt3$Volume)=="character") dt3$Volume <-as.numeric(dt3$Volume)
if (class(dt3$OrganismCode)!="factor") dt3$OrganismCode<- as.factor(dt3$OrganismCode)
if (class(dt3$IEPFishCode)!="factor") dt3$IEPFishCode<- as.factor(dt3$IEPFishCode)
if (class(dt3$CommonName)!="factor") dt3$CommonName<- as.factor(dt3$CommonName)
if (class(dt3$MarkCode)!="factor") dt3$MarkCode<- as.factor(dt3$MarkCode)
if (class(dt3$StageCode)!="factor") dt3$StageCode<- as.factor(dt3$StageCode)
if (class(dt3$Expression)!="factor") dt3$Expression<- as.factor(dt3$Expression)
if (class(dt3$ForkLength)=="factor") dt3$ForkLength <-as.numeric(levels(dt3$ForkLength))[as.integer(dt3$ForkLength) ]               
if (class(dt3$ForkLength)=="character") dt3$ForkLength <-as.numeric(dt3$ForkLength)
if (class(dt3$RaceByLength)!="factor") dt3$RaceByLength<- as.factor(dt3$RaceByLength)
if (class(dt3$TagCode)!="factor") dt3$TagCode<- as.factor(dt3$TagCode)
if (class(dt3$RaceByTag)!="factor") dt3$RaceByTag<- as.factor(dt3$RaceByTag)
if (class(dt3$ArchiveID)!="factor") dt3$ArchiveID<- as.factor(dt3$ArchiveID)
if (class(dt3$SpecialStudy)!="factor") dt3$SpecialStudy<- as.factor(dt3$SpecialStudy)
if (class(dt3$SpecialStudyID)!="factor") dt3$SpecialStudyID<- as.factor(dt3$SpecialStudyID)
if (class(dt3$Count)=="factor") dt3$Count <-as.numeric(levels(dt3$Count))[as.integer(dt3$Count) ]               
if (class(dt3$Count)=="character") dt3$Count <-as.numeric(dt3$Count)
                
# Convert Missing Values to NA for non-dates
                
dt3$WeatherCode <- as.factor(ifelse((trimws(as.character(dt3$WeatherCode))==trimws("NA")),NA,as.character(dt3$WeatherCode)))
dt3$DO <- ifelse((trimws(as.character(dt3$DO))==trimws("NA")),NA,dt3$DO)               
suppressWarnings(dt3$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$DO))==as.character(as.numeric("NA"))),NA,dt3$DO))
dt3$WaterTemp <- ifelse((trimws(as.character(dt3$WaterTemp))==trimws("NA")),NA,dt3$WaterTemp)               
suppressWarnings(dt3$WaterTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$WaterTemp))==as.character(as.numeric("NA"))),NA,dt3$WaterTemp))
dt3$Turbidity <- ifelse((trimws(as.character(dt3$Turbidity))==trimws("NA")),NA,dt3$Turbidity)               
suppressWarnings(dt3$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Turbidity))==as.character(as.numeric("NA"))),NA,dt3$Turbidity))
dt3$TurbidityFnu <- ifelse((trimws(as.character(dt3$TurbidityFnu))==trimws("NA")),NA,dt3$TurbidityFnu)               
suppressWarnings(dt3$TurbidityFnu <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$TurbidityFnu))==as.character(as.numeric("NA"))),NA,dt3$TurbidityFnu))
dt3$SpecificConductance <- ifelse((trimws(as.character(dt3$SpecificConductance))==trimws("NA")),NA,dt3$SpecificConductance)               
suppressWarnings(dt3$SpecificConductance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SpecificConductance))==as.character(as.numeric("NA"))),NA,dt3$SpecificConductance))
dt3$SiteDisturbance <- as.factor(ifelse((trimws(as.character(dt3$SiteDisturbance))==trimws("NA")),NA,as.character(dt3$SiteDisturbance)))
dt3$AlternateSite <- as.factor(ifelse((trimws(as.character(dt3$AlternateSite))==trimws("NA")),NA,as.character(dt3$AlternateSite)))
dt3$SeineLength <- ifelse((trimws(as.character(dt3$SeineLength))==trimws("NA")),NA,dt3$SeineLength)               
suppressWarnings(dt3$SeineLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SeineLength))==as.character(as.numeric("NA"))),NA,dt3$SeineLength))
dt3$SeineWidth <- ifelse((trimws(as.character(dt3$SeineWidth))==trimws("NA")),NA,dt3$SeineWidth)               
suppressWarnings(dt3$SeineWidth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SeineWidth))==as.character(as.numeric("NA"))),NA,dt3$SeineWidth))
dt3$SeineDepth <- ifelse((trimws(as.character(dt3$SeineDepth))==trimws("NA")),NA,dt3$SeineDepth)               
suppressWarnings(dt3$SeineDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SeineDepth))==as.character(as.numeric("NA"))),NA,dt3$SeineDepth))
dt3$Volume <- ifelse((trimws(as.character(dt3$Volume))==trimws("NA")),NA,dt3$Volume)               
suppressWarnings(dt3$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Volume))==as.character(as.numeric("NA"))),NA,dt3$Volume))
dt3$MarkCode <- as.factor(ifelse((trimws(as.character(dt3$MarkCode))==trimws("NA")),NA,as.character(dt3$MarkCode)))
dt3$StageCode <- as.factor(ifelse((trimws(as.character(dt3$StageCode))==trimws("NA")),NA,as.character(dt3$StageCode)))
dt3$Expression <- as.factor(ifelse((trimws(as.character(dt3$Expression))==trimws("NA")),NA,as.character(dt3$Expression)))
dt3$ForkLength <- ifelse((trimws(as.character(dt3$ForkLength))==trimws("NA")),NA,dt3$ForkLength)               
suppressWarnings(dt3$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$ForkLength))==as.character(as.numeric("NA"))),NA,dt3$ForkLength))
dt3$RaceByLength <- as.factor(ifelse((trimws(as.character(dt3$RaceByLength))==trimws("NA")),NA,as.character(dt3$RaceByLength)))
dt3$TagCode <- as.factor(ifelse((trimws(as.character(dt3$TagCode))==trimws("NA")),NA,as.character(dt3$TagCode)))
dt3$RaceByTag <- as.factor(ifelse((trimws(as.character(dt3$RaceByTag))==trimws("NA")),NA,as.character(dt3$RaceByTag)))
dt3$ArchiveID <- as.factor(ifelse((trimws(as.character(dt3$ArchiveID))==trimws("NA")),NA,as.character(dt3$ArchiveID)))
dt3$SpecialStudy <- as.factor(ifelse((trimws(as.character(dt3$SpecialStudy))==trimws("NA")),NA,as.character(dt3$SpecialStudy)))
dt3$SpecialStudyID <- as.factor(ifelse((trimws(as.character(dt3$SpecialStudyID))==trimws("NA")),NA,as.character(dt3$SpecialStudyID)))


# Here is the structure of the input data frame:
print("dt3) Structure")		    
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

print(" ")
print("Summary of Location")
print(summary(Location))
print(" ")
print("Summary of RegionCode")
print(summary(RegionCode))
print(" ")
print("Summary of StationCode")
print(summary(StationCode))
print(" ")
print("Summary of SampleDate")
print(summary(SampleDate))
print(" ")
print("Summary of SampleTime")
print(summary(SampleTime))
print(" ")
print("Summary of MethodCode")
print(summary(MethodCode))
print(" ")
print("Summary of GearConditionCode")
print(summary(GearConditionCode))
print(" ")
print("Summary of WeatherCode")
print(summary(WeatherCode))
print(" ")
print("Summary of DO")
print(summary(DO))
print(" ")
print("Summary of WaterTemp")
print(summary(WaterTemp))
print(" ")
print("Summary of Turbidity")
print(summary(Turbidity))
print(" ")
print("Summary of TurbidityFnu")
print(summary(TurbidityFnu))
print(" ")
print("Summary of SpecificConductance")
print(summary(SpecificConductance))
print(" ")
print("Summary of SiteDisturbance")
print(summary(SiteDisturbance))
print(" ")
print("Summary of AlternateSite")
print(summary(AlternateSite))
print(" ")
print("Summary of SeineLength")
print(summary(SeineLength))
print(" ")
print("Summary of SeineWidth")
print(summary(SeineWidth))
print(" ")
print("Summary of SeineDepth")
print(summary(SeineDepth))
print(" ")
print("Summary of Volume")
print(summary(Volume))
print(" ")
print("Summary of OrganismCode")
print(summary(OrganismCode))
print(" ")
print("Summary of IEPFishCode")
print(summary(IEPFishCode))
print(" ")
print("Summary of CommonName")
print(summary(CommonName))
print(" ")
print("Summary of MarkCode")
print(summary(MarkCode))
print(" ")
print("Summary of StageCode")
print(summary(StageCode))
print(" ")
print("Summary of Expression")
print(summary(Expression))
print(" ")
print("Summary of ForkLength")
print(summary(ForkLength))
print(" ")
print("Summary of RaceByLength")
print(summary(RaceByLength))
print(" ")
print("Summary of TagCode")
print(summary(TagCode))
print(" ")
print("Summary of RaceByTag")
print(summary(RaceByTag))
print(" ")
print("Summary of ArchiveID")
print(summary(ArchiveID))
print(" ")
print("Summary of SpecialStudy")
print(summary(SpecialStudy))
print(" ")
print("Summary of SpecialStudyID")
print(summary(SpecialStudyID))
print(" ")
print("Summary of Count")
print(summary(Count)) 
# Get more details on character variables
                 

print(" ")
print("Summary of Location")
print(summary(as.factor(dt3$Location))) 

print(" ")
print("Summary of RegionCode")
print(summary(as.factor(dt3$RegionCode))) 

print(" ")
print("Summary of StationCode")
print(summary(as.factor(dt3$StationCode))) 

print(" ")
print("Summary of MethodCode")
print(summary(as.factor(dt3$MethodCode))) 

print(" ")
print("Summary of GearConditionCode")
print(summary(as.factor(dt3$GearConditionCode))) 

print(" ")
print("Summary of WeatherCode")
print(summary(as.factor(dt3$WeatherCode))) 

print(" ")
print("Summary of SiteDisturbance")
print(summary(as.factor(dt3$SiteDisturbance))) 

print(" ")
print("Summary of AlternateSite")
print(summary(as.factor(dt3$AlternateSite))) 

print(" ")
print("Summary of OrganismCode")
print(summary(as.factor(dt3$OrganismCode))) 

print(" ")
print("Summary of IEPFishCode")
print(summary(as.factor(dt3$IEPFishCode))) 

print(" ")
print("Summary of CommonName")
print(summary(as.factor(dt3$CommonName))) 

print(" ")
print("Summary of MarkCode")
print(summary(as.factor(dt3$MarkCode))) 

print(" ")
print("Summary of StageCode")
print(summary(as.factor(dt3$StageCode))) 

print(" ")
print("Summary of Expression")
print(summary(as.factor(dt3$Expression))) 

print(" ")
print("Summary of RaceByLength")
print(summary(as.factor(dt3$RaceByLength))) 

print(" ")
print("Summary of TagCode")
print(summary(as.factor(dt3$TagCode))) 

print(" ")
print("Summary of RaceByTag")
print(summary(as.factor(dt3$RaceByTag))) 

print(" ")
print("Summary of ArchiveID")
print(summary(as.factor(dt3$ArchiveID))) 

print(" ")
print("Summary of SpecialStudy")
print(summary(as.factor(dt3$SpecialStudy))) 

print(" ")
print("Summary of SpecialStudyID")
print(summary(as.factor(dt3$SpecialStudyID)))
detach(dt3)               
        
	      

inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/244/14/17c9974d9b7b0125c146a887f3c64bd8" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")

                   
 dt4 <-read.csv(infile4,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "OrganismCode",     
                    "IEPFishCode",     
                    "CommonName",     
                    "NonNative",     
                    "Phylum",     
                    "Class",     
                    "Order",     
                    "Family",     
                    "Genus",     
                    "Species",     
                    "Active"    ), check.names=TRUE)
               
unlink(infile4)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt4$OrganismCode)!="factor") dt4$OrganismCode<- as.factor(dt4$OrganismCode)
if (class(dt4$IEPFishCode)!="factor") dt4$IEPFishCode<- as.factor(dt4$IEPFishCode)
if (class(dt4$CommonName)!="factor") dt4$CommonName<- as.factor(dt4$CommonName)
if (class(dt4$NonNative)!="factor") dt4$NonNative<- as.factor(dt4$NonNative)
if (class(dt4$Phylum)!="factor") dt4$Phylum<- as.factor(dt4$Phylum)
if (class(dt4$Class)!="factor") dt4$Class<- as.factor(dt4$Class)
if (class(dt4$Order)!="factor") dt4$Order<- as.factor(dt4$Order)
if (class(dt4$Family)!="factor") dt4$Family<- as.factor(dt4$Family)
if (class(dt4$Genus)!="factor") dt4$Genus<- as.factor(dt4$Genus)
if (class(dt4$Species)!="factor") dt4$Species<- as.factor(dt4$Species)
if (class(dt4$Active)!="factor") dt4$Active<- as.factor(dt4$Active)
                
# Convert Missing Values to NA for non-dates
                
dt4$OrganismCode <- as.factor(ifelse((trimws(as.character(dt4$OrganismCode))==trimws("NA")),NA,as.character(dt4$OrganismCode)))
dt4$IEPFishCode <- as.factor(ifelse((trimws(as.character(dt4$IEPFishCode))==trimws("NA")),NA,as.character(dt4$IEPFishCode)))
dt4$CommonName <- as.factor(ifelse((trimws(as.character(dt4$CommonName))==trimws("NA")),NA,as.character(dt4$CommonName)))
dt4$NonNative <- as.factor(ifelse((trimws(as.character(dt4$NonNative))==trimws("NA")),NA,as.character(dt4$NonNative)))
dt4$Phylum <- as.factor(ifelse((trimws(as.character(dt4$Phylum))==trimws("NA")),NA,as.character(dt4$Phylum)))
dt4$Class <- as.factor(ifelse((trimws(as.character(dt4$Class))==trimws("NA")),NA,as.character(dt4$Class)))
dt4$Order <- as.factor(ifelse((trimws(as.character(dt4$Order))==trimws("NA")),NA,as.character(dt4$Order)))
dt4$Family <- as.factor(ifelse((trimws(as.character(dt4$Family))==trimws("NA")),NA,as.character(dt4$Family)))
dt4$Genus <- as.factor(ifelse((trimws(as.character(dt4$Genus))==trimws("NA")),NA,as.character(dt4$Genus)))
dt4$Species <- as.factor(ifelse((trimws(as.character(dt4$Species))==trimws("NA")),NA,as.character(dt4$Species)))
dt4$Active <- as.factor(ifelse((trimws(as.character(dt4$Active))==trimws("NA")),NA,as.character(dt4$Active)))


# Here is the structure of the input data frame:
print("dt4) Structure")		    
str(dt4)                            
attach(dt4)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

print(" ")
print("Summary of OrganismCode")
print(summary(OrganismCode))
print(" ")
print("Summary of IEPFishCode")
print(summary(IEPFishCode))
print(" ")
print("Summary of CommonName")
print(summary(CommonName))
print(" ")
print("Summary of NonNative")
print(summary(NonNative))
print(" ")
print("Summary of Phylum")
print(summary(Phylum))
print(" ")
print("Summary of Class")
print(summary(Class))
print(" ")
print("Summary of Order")
print(summary(Order))
print(" ")
print("Summary of Family")
print(summary(Family))
print(" ")
print("Summary of Genus")
print(summary(Genus))
print(" ")
print("Summary of Species")
print(summary(Species))
print(" ")
print("Summary of Active")
print(summary(Active)) 
# Get more details on character variables
                 

print(" ")
print("Summary of OrganismCode")
print(summary(as.factor(dt4$OrganismCode))) 

print(" ")
print("Summary of IEPFishCode")
print(summary(as.factor(dt4$IEPFishCode))) 

print(" ")
print("Summary of CommonName")
print(summary(as.factor(dt4$CommonName))) 

print(" ")
print("Summary of NonNative")
print(summary(as.factor(dt4$NonNative))) 

print(" ")
print("Summary of Phylum")
print(summary(as.factor(dt4$Phylum))) 

print(" ")
print("Summary of Class")
print(summary(as.factor(dt4$Class))) 

print(" ")
print("Summary of Order")
print(summary(as.factor(dt4$Order))) 

print(" ")
print("Summary of Family")
print(summary(as.factor(dt4$Family))) 

print(" ")
print("Summary of Genus")
print(summary(as.factor(dt4$Genus))) 

print(" ")
print("Summary of Species")
print(summary(as.factor(dt4$Species))) 

print(" ")
print("Summary of Active")
print(summary(as.factor(dt4$Active)))
detach(dt4)               
        
	      

inUrl5  <- "https://pasta.lternet.edu/package/data/eml/edi/244/14/99a038d691f27cd306ff93fdcbc03b77" 
infile5 <- tempfile()
try(download.file(inUrl5,infile5,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")

                   
 dt5 <-read.csv(infile5,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "MethodCode",     
                    "Location",     
                    "StationCode",     
                    "Latitude",     
                    "Longitude"    ), check.names=TRUE)
               
unlink(infile5)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt5$MethodCode)!="factor") dt5$MethodCode<- as.factor(dt5$MethodCode)
if (class(dt5$Location)!="factor") dt5$Location<- as.factor(dt5$Location)
if (class(dt5$StationCode)!="factor") dt5$StationCode<- as.factor(dt5$StationCode)
if (class(dt5$Latitude)=="factor") dt5$Latitude <-as.numeric(levels(dt5$Latitude))[as.integer(dt5$Latitude) ]               
if (class(dt5$Latitude)=="character") dt5$Latitude <-as.numeric(dt5$Latitude)
if (class(dt5$Longitude)=="factor") dt5$Longitude <-as.numeric(levels(dt5$Longitude))[as.integer(dt5$Longitude) ]               
if (class(dt5$Longitude)=="character") dt5$Longitude <-as.numeric(dt5$Longitude)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
print("dt5) Structure")		    
str(dt5)                            
attach(dt5)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

print(" ")
print("Summary of MethodCode")
print(summary(MethodCode))
print(" ")
print("Summary of Location")
print(summary(Location))
print(" ")
print("Summary of StationCode")
print(summary(StationCode))
print(" ")
print("Summary of Latitude")
print(summary(Latitude))
print(" ")
print("Summary of Longitude")
print(summary(Longitude)) 
# Get more details on character variables
                 

print(" ")
print("Summary of MethodCode")
print(summary(as.factor(dt5$MethodCode))) 

print(" ")
print("Summary of Location")
print(summary(as.factor(dt5$Location))) 

print(" ")
print("Summary of StationCode")
print(summary(as.factor(dt5$StationCode)))
detach(dt5)               
        



