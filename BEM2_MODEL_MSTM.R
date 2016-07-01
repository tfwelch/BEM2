####################################
### BUILDING EMISSIONS MODEL v2.2###
####################################

#Version Date: 4.4.2016
# Coded by: Tim Welch, PEER Group, LLC.

####### New in this version:#########
#    1. States outside of MD run in a loop rather than having seperate code
#    2. Efficieny module addded - allows users to set at percent efficieny reduction or dollar value
#####################################


#!!!!!!!!USER INPUT NEEDED!!!!!!!!#
#It is recommended that you place BEM2 in a root directory - if you have permission

# USER: change model directory changing D: to C:, F:, E:, etc.
MAINPATH <- file.path("D:", "BEM2")

# GET EXTERNAL COMMANDS
options(echo = TRUE)
args <- commandArgs(trailingOnly = TRUE)
print(args)
BASEYEAR <- as.numeric(args[1])
PROJYEAR <- as.numeric(args[2])
# BASEYEAR <- 2007 PROJYEAR <- 2030

# Set input/output folders - No User Input Needed
INPATH <- file.path(MAINPATH, "INPUT", "")
OUTPATH <- file.path(MAINPATH, "OUTPUT", "")
TEMPATH <- file.path(MAINPATH, "TEMP", "")

# Set parameters
Parameters <- read.csv(paste(MAINPATH, "/PARAMETERS/Parameters.csv", sep = ""))  #Model Parameters
DDCCRAW <- read.csv(paste(MAINPATH, "/PARAMETERS/DDCC.csv", sep = ""))  #Climate Change Parameters
SCENARIO <- Parameters$Scenario
EFFICIENCYPERCENT <- Parameters$EfficienyPercent
SCENARIO <- Parameters$Scenario
# BASEYEAR <- Parameters$BaseYear PROJYEAR <- Parameters$ProjectionYear
TIME <- PROJYEAR - BASEYEAR  #Calculate Time Span
DDCCTARGET <- Parameters$TempratureChange
DDCCRESULT <- subset(DDCCRAW, Degrees == DDCCTARGET)
MD_COM_CO2con <- 4.59
PA_COM_CO2con <- 11.78
VA_COM_CO2con <- 5.02
WV_COM_CO2con <- 1.59
DE_COM_CO2con <- 0.73
DC_COM_CO2con <- 1.1

MD_RES_CO2con <- 5.9
PA_RES_CO2con <- 20.26
VA_RES_CO2con <- 6.96
WV_RES_CO2con <- 2
DE_RES_CO2con <- 0.99
DC_RES_CO2con <- 0.7

# Read in data files
Base_SMZ_Activities <- read.csv(paste(INPATH, sprintf("activitiesBySilo_%s.csv", BASEYEAR), sep = ""))  #Base Activities File
names(Base_SMZ_Activities) <- c("SMZ_N", "ACRES", "HH_BASE", "ENR_BASE", "RE_BASE", "OFF_BASE", "OTH_BASE", "TOTEMP_BASE")  #rename headings
Proj_SMZ_Activities <- read.csv(paste(INPATH, sprintf("activitiesBySilo_%s.csv", PROJYEAR), sep = ""))  #Forecast Activities File
names(Proj_SMZ_Activities) <- c("SMZ_N", "ACRES", "HH_PROJ", "ENR_PROJ", "RE_PROJ", "OFF_PROJ", "OTH_PROJ", "TOTEMP_PROJ")  #rename headings
SMZ_HDD <- read.csv(paste(INPATH, "SMZ_climate_lookup.csv", sep = ""))  #base Housing Parcel data
CBECS <- read.csv(paste(INPATH, "cbecs_data.csv", sep = ""))  #Observed Commercial Emissions
RECS <- read.csv(paste(INPATH, "recs_data.csv", sep = ""))  #Observed Residential Emissions
MD_Com_Base <- read.csv(paste(INPATH, "MD_Base_Com_Data.csv", sep = ""))  #Base Commercial Parcel data
VA_Com_Base <- read.csv(paste(INPATH, "VA_Base_Com_Data.csv", sep = ""))  #Base Commercial Parcel data
PA_Com_Base <- read.csv(paste(INPATH, "PA_Base_Com_Data.csv", sep = ""))  #Base Commercial Parcel data
DC_Com_Base <- read.csv(paste(INPATH, "DC_Base_Com_Data.csv", sep = ""))  #Base Commercial Parcel data
DE_Com_Base <- read.csv(paste(INPATH, "DE_Base_Com_Data.csv", sep = ""))  #Base Commercial Parcel data
WV_Com_Base <- read.csv(paste(INPATH, "WV_Base_Com_Data.csv", sep = ""))  #Base Commercial Parcel data
MD_Res_Base <- read.csv(paste(INPATH, "MD_Base_Res_Data.csv", sep = ""))  #base Housing Parcel data
VA_Res_Base <- read.csv(paste(INPATH, "VA_Base_Res_Data.csv", sep = ""))  #base Housing Parcel data
PA_Res_Base <- read.csv(paste(INPATH, "PA_Base_Res_Data.csv", sep = ""))  #base Housing Parcel data
DC_Res_Base <- read.csv(paste(INPATH, "DC_Base_Res_Data.csv", sep = ""))  #base Housing Parcel data
DE_Res_Base <- read.csv(paste(INPATH, "DE_Base_Res_Data.csv", sep = ""))  #base Housing Parcel data
WV_Res_Base <- read.csv(paste(INPATH, "WV_Base_Res_Data.csv", sep = ""))  #base Housing Parcel data
BlankSMZ <- read.csv(paste(INPATH, "SMZ_Numbers.csv", sep = ""))  #Blank SMZ Number File 

### Take out the trash
file.remove(paste(TEMPATH, "RES_CO2_SAMPLE.csv", sep = ""))
file.remove(paste(TEMPATH, "COM_CO2_SAMPLE.csv", sep = ""))

############## Begin BUILDING CO2 MODEL

#### Setup Efficiency Distributions
if (EFFICIENCYPERCENT > 1) {
    EFFICIENCYPERCENT <- EFFICIENCYPERCENT * 0.01
} else {
    EFFICIENCYPERCENT <- EFFICIENCYPERCENT
}

if (EFFICIENCYPERCENT > 0) {
    MD_EFF1 <- sample(1:100, 1)
    VA_EFF1 <- sample(1:100, 1)
    PA_EFF1 <- sample(1:100, 1)
    WV_EFF1 <- sample(1:100, 1)
    DE_EFF1 <- sample(1:100, 1)
    DC_EFF1 <- sample(1:100, 1)
    
    EFF1_SUM = MD_EFF1 + VA_EFF1 + PA_EFF1 + WV_EFF1 + DE_EFF1 + DC_EFF1
    
    MD_EFF1a <- MD_EFF1/EFF1_SUM * EFFICIENCYPERCENT
    VA_EFF1a <- VA_EFF1/EFF1_SUM * EFFICIENCYPERCENT
    PA_EFF1a <- PA_EFF1/EFF1_SUM * EFFICIENCYPERCENT
    WV_EFF1a <- WV_EFF1/EFF1_SUM * EFFICIENCYPERCENT
    DE_EFF1a <- DE_EFF1/EFF1_SUM * EFFICIENCYPERCENT
    DC_EFF1a <- DC_EFF1/EFF1_SUM * EFFICIENCYPERCENT
    
} else {
    MD_EFF1a <- 0
    VA_EFF1a <- 0
    PA_EFF1a <- 0
    WV_EFF1a <- 0
    DE_EFF1a <- 0
    DC_EFF1a <- 0
}


#### Prep Varible for CO2 Binary
CBECS$CO2_PROD <- NA
CBECS$CO2_PROD[CBECS$TCO2 > 0] <- 1
CBECS$CO2_PROD[CBECS$TCO2 == 0] <- 0

### Calculate Building Age
CBECS$Age <- NA
CBECS$Age <- 2006 - CBECS$YRCON8

# Convert from buildings to workers
CBECS$TCO2Work <- CBECS$TCO2/CBECS$Workers

### Replace Errors with Zero
CBECS[is.na(CBECS)] <- 0
CBECS[CBECS == -Inf] <- 0
CBECS[CBECS == Inf] <- 0

##### Select observations based on CO2 Emissions Greater than zero
CBECS2 <- CBECS[which(CBECS$TCO2 > 0), ]

##### Fit Com_ols
Com_ols <- lm(TCO2Work ~ HDD658 + Age + SQFT8, data = CBECS2)
summary(Com_ols)

##### START MD COM CO2####

##### Estimate Commercial Emissions
MD_Com_Base$CO2_EST <- Com_ols$coef[1] + (Com_ols$coef[2]) * MD_Com_Base$HDD658 + (Com_ols$coef[3]) * MD_Com_Base$Age + (Com_ols$coef[4]) * 
    MD_Com_Base$SQFTSTRC

### Calculate Emissions per Worker
MD_Com_Base$CO2_RE <- MD_Com_Base$CO2_EST * MD_Com_Base$RETAIL
MD_Com_Base$CO2_OFF <- MD_Com_Base$CO2_EST * MD_Com_Base$Office
MD_Com_Base$CO2_OTH <- MD_Com_Base$CO2_EST * MD_Com_Base$Other

### Calculate Number of Workers BY Sector
MD_Com_Base$Workers_RE <- MD_Com_Base$Workers * MD_Com_Base$RETAIL
MD_Com_Base$Workers_OFF <- MD_Com_Base$Workers * MD_Com_Base$Office
MD_Com_Base$Workers_OTH <- MD_Com_Base$Workers * MD_Com_Base$Other

#### Aggregate data By SMZ by sector
MD_Com_Base_SMZ <- aggregate(cbind(MD_Com_Base$CO2_EST, MD_Com_Base$CO2_RE, MD_Com_Base$CO2_OFF, MD_Com_Base$CO2_OTH, MD_Com_Base$Workers_RE, 
    MD_Com_Base$Workers_OFF, MD_Com_Base$Workers_OTH) ~ MD_Com_Base$SMZ_N, data = MD_Com_Base, sum, na.rm = TRUE)
names(MD_Com_Base_SMZ)[1] <- "SMZ_N"
names(MD_Com_Base_SMZ)[2] <- "CO2_EST"
names(MD_Com_Base_SMZ)[3] <- "CO2_EST_RE"
names(MD_Com_Base_SMZ)[4] <- "CO2_EST_OFF"
names(MD_Com_Base_SMZ)[5] <- "CO2_EST_OTH"
names(MD_Com_Base_SMZ)[6] <- "CO2_Workers_RE"
names(MD_Com_Base_SMZ)[7] <- "CO2_Workers_OFF"
names(MD_Com_Base_SMZ)[8] <- "CO2_Workers_OTH"

MD_Com_Base_SMZ_SQFT <- aggregate(cbind(MD_Com_Base$SQFTSTRC) ~ MD_Com_Base$SMZ_N, data = MD_Com_Base, mean, na.rm = TRUE)
names(MD_Com_Base_SMZ_SQFT)[1] <- "SMZ_N"
names(MD_Com_Base_SMZ_SQFT)[2] <- "SQFT"

MD_Com_Base_SMZ$SQFT = MD_Com_Base_SMZ_SQFT[match(MD_Com_Base_SMZ$SMZ_N, MD_Com_Base_SMZ_SQFT$SMZ_N), "SQFT"]

### Rate Per Worker Per Sector
MD_Com_Base_SMZ$CO2_Rate_RE <- MD_Com_Base_SMZ$CO2_EST_RE/MD_Com_Base_SMZ$CO2_Workers_RE
MD_Com_Base_SMZ$CO2_Rate_OFF <- MD_Com_Base_SMZ$CO2_EST_OFF/MD_Com_Base_SMZ$CO2_Workers_OFF
MD_Com_Base_SMZ$CO2_Rate_OTH <- MD_Com_Base_SMZ$CO2_EST_OTH/MD_Com_Base_SMZ$CO2_Workers_OTH

# merge Rates and Activities
MD_Com_Base_Activities_SMZ = merge(MD_Com_Base_SMZ, Base_SMZ_Activities, by = "SMZ_N")

### Apply Rates to Activities
MD_Com_Base_Activities_SMZ$CO2_base_RE <- MD_Com_Base_Activities_SMZ$CO2_Rate_RE * MD_Com_Base_Activities_SMZ$RE_BASE
MD_Com_Base_Activities_SMZ$CO2_base_OFF <- MD_Com_Base_Activities_SMZ$CO2_Rate_OFF * MD_Com_Base_Activities_SMZ$OFF_BASE
MD_Com_Base_Activities_SMZ$CO2_base_OTH <- MD_Com_Base_Activities_SMZ$CO2_Rate_OTH * MD_Com_Base_Activities_SMZ$OTH_BASE

### Replace Errors with Zero
MD_Com_Base_Activities_SMZ[is.na(MD_Com_Base_Activities_SMZ)] <- 0
MD_Com_Base_Activities_SMZ[MD_Com_Base_Activities_SMZ == -Inf] <- 0
MD_Com_Base_Activities_SMZ[MD_Com_Base_Activities_SMZ == Inf] <- 0

### Emission Rate Adjustment Factor
MD_Com_Base_Activities_SMZ$Tot_SEC_CO2 <- MD_Com_Base_Activities_SMZ$CO2_base_RE + MD_Com_Base_Activities_SMZ$CO2_base_OFF + MD_Com_Base_Activities_SMZ$CO2_base_OTH
Tot_BASE_Com_CO2 <- sum(MD_Com_Base_Activities_SMZ$Tot_SEC_CO2, na.rm = TRUE) * 1e-06
CO2_Com_Rate_Adj <- (MD_COM_CO2con/Tot_BASE_Com_CO2)

### Adjusted Rate Per Worker Per Sector
MD_Com_Base_Activities_SMZ$CO2_Rate_RE_ADJ <- MD_Com_Base_Activities_SMZ$CO2_Rate_RE * CO2_Com_Rate_Adj
MD_Com_Base_Activities_SMZ$CO2_Rate_OFF_ADJ <- MD_Com_Base_Activities_SMZ$CO2_Rate_OFF * CO2_Com_Rate_Adj
MD_Com_Base_Activities_SMZ$CO2_Rate_OTH_ADJ <- MD_Com_Base_Activities_SMZ$CO2_Rate_OTH * CO2_Com_Rate_Adj

### Apply Adjusted Rates to Activities
MD_Com_Base_Activities_SMZ$CO2_base_RE <- MD_Com_Base_Activities_SMZ$CO2_Rate_RE_ADJ * MD_Com_Base_Activities_SMZ$RE_BASE
MD_Com_Base_Activities_SMZ$CO2_base_OFF <- MD_Com_Base_Activities_SMZ$CO2_Rate_OFF_ADJ * MD_Com_Base_Activities_SMZ$OFF_BASE
MD_Com_Base_Activities_SMZ$CO2_base_OTH <- MD_Com_Base_Activities_SMZ$CO2_Rate_OTH_ADJ * MD_Com_Base_Activities_SMZ$OTH_BASE

### Sum Total Base Emissions
MD_Com_Base_Activities_SMZ$CO2_base_Com_Total <- MD_Com_Base_Activities_SMZ$CO2_base_RE + MD_Com_Base_Activities_SMZ$CO2_base_OFF + MD_Com_Base_Activities_SMZ$CO2_base_OTH

# Merge Base Data, Rates and Projected Activities
MD_Com_Base_Activities_SMZ_PROJ = merge(MD_Com_Base_Activities_SMZ, Proj_SMZ_Activities, by = "SMZ_N")

### Calculate Projected Growth
PROJ_GROWTH_RE <- data.frame(MD_Com_Base_Activities_SMZ_PROJ$SMZ_N, (MD_Com_Base_Activities_SMZ_PROJ$RE_PROJ - MD_Com_Base_Activities_SMZ_PROJ$RE_BASE))
names(PROJ_GROWTH_RE) <- c("SMZ_N", "PROJ_GROWTH_RE")
PROJ_GROWTH_OFF <- data.frame(MD_Com_Base_Activities_SMZ_PROJ$SMZ_N, (MD_Com_Base_Activities_SMZ_PROJ$OFF_PROJ - MD_Com_Base_Activities_SMZ_PROJ$OFF_BASE))
names(PROJ_GROWTH_OFF) <- c("SMZ_N", "PROJ_GROWTH_OFF")
PROJ_GROWTH_OTH <- data.frame(MD_Com_Base_Activities_SMZ_PROJ$SMZ_N, (MD_Com_Base_Activities_SMZ_PROJ$OTH_PROJ - MD_Com_Base_Activities_SMZ_PROJ$OTH_BASE))
names(PROJ_GROWTH_OTH) <- c("SMZ_N", "PROJ_GROWTH_OTH")
PROJ_GROWTH_EMP <- data.frame(MD_Com_Base_Activities_SMZ_PROJ$SMZ_N, (PROJ_GROWTH_RE$PROJ_GROWTH_RE + PROJ_GROWTH_OFF$PROJ_GROWTH_OFF + PROJ_GROWTH_OTH$PROJ_GROWTH_OTH))
names(PROJ_GROWTH_EMP) <- c("SMZ_N", "PROJ_GROWTH_EMP")

############ Simulate New Commercial Stock
MD_Com_SMZ_SIM <- PROJ_GROWTH_EMP

# Attach HDD Data
MD_Com_SMZ_SIM$HDD = SMZ_HDD[match(MD_Com_SMZ_SIM$SMZ_N, SMZ_HDD$SMZ_N), "HDD"]

# Sampling Function
sampEMP <- function(x) {
    
    sample(PROJ_GROWTH_EMP + 1, 1)
    
    # Sim commercial Data
    MD_Com_SMZ_SIM$Age <- sapply(MD_Com_SMZ_SIM$PROJ_GROWTH_EMP * TIME + 1, sample, 1) - 1
    MD_Com_SMZ_SIM$SQFTSTRC <- sapply(MD_Com_SMZ_SIM$PROJ_GROWTH_EMP * MD_Com_Base_SMZ$SQFT + 1, sample, 1) - 1
    
    
    ##### Estimate New Stock Commercial Emissions
    MD_Com_SMZ_SIM$CO2_EST <- Com_ols$coef[1] + (Com_ols$coef[2]) * (MD_Com_SMZ_SIM$HDD * DDCCRESULT$HDD) + (Com_ols$coef[3]) * (MD_Com_SMZ_SIM$Age) + 
        (Com_ols$coef[4]) * MD_Com_SMZ_SIM$SQFTSTRC
    
    ### Apply Emissions Adjustment to Projected New Stock
    MD_Com_SMZ_SIM$CO2_EST_ADJ <- MD_Com_SMZ_SIM$CO2_EST * CO2_Com_Rate_Adj * (1 - EFFICIENCYPERCENT)
    write.table(MD_Com_SMZ_SIM[c("SMZ_N", "CO2_EST_ADJ")], file = (paste(TEMPATH, "COM_CO2_SAMPLE.csv", sep = "")), sep = ",", row.names = FALSE, 
        col.names = FALSE, append = TRUE)
}

replicate(n = 10000, expr = sampEMP())

MD_Com_SMZ_SIM_RESULT <- read.csv(paste(TEMPATH, "COM_CO2_SAMPLE.csv", sep = ""))  #Read back in simulated housing stock
names(MD_Com_SMZ_SIM_RESULT) <- c("SMZ_N", "CO2_EST_ADJ")

#### Aggregate CO2 data in simulated commercial stock By SMZ
MD_Com_SMZ_SIM_RESULT_AGG <- aggregate(cbind(MD_Com_SMZ_SIM_RESULT$CO2_EST_ADJ) ~ MD_Com_SMZ_SIM_RESULT$SMZ_N, data = MD_Com_SMZ_SIM_RESULT, 
    mean, na.rm = FALSE)
names(MD_Com_SMZ_SIM_RESULT_AGG)[1] <- "SMZ_N"
names(MD_Com_SMZ_SIM_RESULT_AGG)[2] <- "CO2_EST_ADJ"

########### Age Base Commercial Stock

## Apply Efficency upgrades
if (EFFICIENCYPERCENT > 0) {
    MD_Com_Base_AGED <- MD_Com_Base
    MD_Com_Base_AGED$CU <- 1  #count variable for each commercial unit
    MD_Com_Base_AGED$UID <- 1:nrow(MD_Com_Base_AGED)  #create unique ID for each unit
    MD_Com_Base_AGED_EFF <- subset(MD_Com_Base_AGED[sample(nrow(MD_Com_Base_AGED), sum(MD_Com_Base_AGED$CU) * MD_EFF1a), ])
    MD_Com_Base_AGED_NOEFF <- subset(MD_Com_Base_AGED[!(MD_Com_Base_AGED$UID %in% MD_Com_Base_AGED_EFF$UID), ])
    
    # Age non-upgraded commercial stock
    MD_Com_Base_AGED_NOEFF$Age <- MD_Com_Base_AGED_NOEFF$Age + TIME
    
    # Apply efficiencies to upgraded housing stock
    MD_Com_Base_AGED_EFF$Age <- sample(0:TIME, 1)  #random age between 0 years and the time between potential upgrade date
    MD_Com_Base_AGED_EFF$SQFTSTRC <- MD_Com_Base_AGED_EFF$SQFTSTRC * (1 - EFFICIENCYPERCENT)
    
    ## Merge Efficiency upgraded housing stock with rest of housing stock
    MD_Com_Base_AGED <- rbind(MD_Com_Base_AGED_NOEFF, MD_Com_Base_AGED_EFF)
    
} else {
    MD_Com_Base_AGED <- MD_Com_Base
    MD_Com_Base_AGED$CU <- 1  #count variable for each commercial unit
}

MD_Com_Base_AGED$CO2_EST <- Com_ols$coef[1] + (Com_ols$coef[2]) * (MD_Com_Base$HDD * DDCCRESULT$HDD) + (Com_ols$coef[3]) * MD_Com_Base$Age + 
    (Com_ols$coef[4]) * MD_Com_Base$SQFTSTRC

### Calculate Emissions BY Sector per Worker
MD_Com_Base_AGED$CO2_RE <- MD_Com_Base_AGED$CO2_EST * MD_Com_Base_AGED$RETAIL
MD_Com_Base_AGED$CO2_OFF <- MD_Com_Base_AGED$CO2_EST * MD_Com_Base_AGED$Office
MD_Com_Base_AGED$CO2_OTH <- MD_Com_Base_AGED$CO2_EST * MD_Com_Base_AGED$Other

### Calculate Number of Workers BY Sector
MD_Com_Base_AGED$Workers_RE <- MD_Com_Base_AGED$Workers * MD_Com_Base_AGED$RETAIL
MD_Com_Base_AGED$Workers_OFF <- MD_Com_Base_AGED$Workers * MD_Com_Base_AGED$Office
MD_Com_Base_AGED$Workers_OTH <- MD_Com_Base_AGED$Workers * MD_Com_Base_AGED$Other

#### Aggregate data By SMZ by sector
MD_Com_Base_SMZ_AGED <- aggregate(cbind(MD_Com_Base_AGED$CO2_EST, MD_Com_Base_AGED$CO2_RE, MD_Com_Base_AGED$CO2_OFF, MD_Com_Base_AGED$CO2_OTH, 
    MD_Com_Base_AGED$Workers_RE, MD_Com_Base_AGED$Workers_OFF, MD_Com_Base_AGED$Workers_OTH) ~ MD_Com_Base_AGED$SMZ_N, data = MD_Com_Base_AGED, 
    sum, na.rm = TRUE)
names(MD_Com_Base_SMZ_AGED)[1] <- "SMZ_N"
names(MD_Com_Base_SMZ_AGED)[2] <- "CO2_EST"
names(MD_Com_Base_SMZ_AGED)[3] <- "CO2_EST_RE"
names(MD_Com_Base_SMZ_AGED)[4] <- "CO2_EST_OFF"
names(MD_Com_Base_SMZ_AGED)[5] <- "CO2_EST_OTH"
names(MD_Com_Base_SMZ_AGED)[6] <- "CO2_Workers_RE"
names(MD_Com_Base_SMZ_AGED)[7] <- "CO2_Workers_OFF"
names(MD_Com_Base_SMZ_AGED)[8] <- "CO2_Workers_OTH"

### Aged Rate Per Worker Per Sector
MD_Com_Base_SMZ_AGED$CO2_Rate_RE <- MD_Com_Base_SMZ_AGED$CO2_EST_RE/MD_Com_Base_SMZ_AGED$CO2_Workers_RE
MD_Com_Base_SMZ_AGED$CO2_Rate_OFF <- MD_Com_Base_SMZ_AGED$CO2_EST_OFF/MD_Com_Base_SMZ_AGED$CO2_Workers_OFF
MD_Com_Base_SMZ_AGED$CO2_Rate_OTH <- MD_Com_Base_SMZ_AGED$CO2_EST_OTH/MD_Com_Base_SMZ_AGED$CO2_Workers_OTH

# Merge Rates and Activities for Aged Commercial Stock
MD_Com_Base_Activities_SMZ_AGED = merge(MD_Com_Base_SMZ_AGED, Base_SMZ_Activities, by = "SMZ_N")

## Apply New Rates to Aged Workers
MD_Com_Base_Activities_SMZ_AGED$CO2_base_RE <- MD_Com_Base_Activities_SMZ_AGED$CO2_Rate_RE * MD_Com_Base_Activities_SMZ_AGED$RE_BASE
MD_Com_Base_Activities_SMZ_AGED$CO2_base_OFF <- MD_Com_Base_Activities_SMZ_AGED$CO2_Rate_OFF * MD_Com_Base_Activities_SMZ_AGED$OFF_BASE
MD_Com_Base_Activities_SMZ_AGED$CO2_base_OTH <- MD_Com_Base_Activities_SMZ_AGED$CO2_Rate_OTH * MD_Com_Base_Activities_SMZ_AGED$OTH_BASE

### Replace Errors with Zero
MD_Com_Base_Activities_SMZ_AGED[is.na(MD_Com_Base_Activities_SMZ_AGED)] <- 0
MD_Com_Base_Activities_SMZ_AGED[MD_Com_Base_Activities_SMZ_AGED == -Inf] <- 0
MD_Com_Base_Activities_SMZ_AGED[MD_Com_Base_Activities_SMZ_AGED == Inf] <- 0

### Adjusted CO2 Rate Per Aged Worker
MD_Com_Base_Activities_SMZ_AGED$CO2_Rate_RE_ADJ <- MD_Com_Base_Activities_SMZ_AGED$CO2_Rate_RE * CO2_Com_Rate_Adj
MD_Com_Base_Activities_SMZ_AGED$CO2_Rate_OFF_ADJ <- MD_Com_Base_Activities_SMZ_AGED$CO2_Rate_OFF * CO2_Com_Rate_Adj
MD_Com_Base_Activities_SMZ_AGED$CO2_Rate_OTH_ADJ <- MD_Com_Base_Activities_SMZ_AGED$CO2_Rate_OTH * CO2_Com_Rate_Adj

### Apply Adjusted Aged Rates to Activities
MD_Com_Base_Activities_SMZ_AGED$CO2_base_RE_AGED <- MD_Com_Base_Activities_SMZ_AGED$CO2_Rate_RE_ADJ * MD_Com_Base_Activities_SMZ_AGED$RE_BASE
MD_Com_Base_Activities_SMZ_AGED$CO2_base_OTH_AGED <- MD_Com_Base_Activities_SMZ_AGED$CO2_Rate_OTH_ADJ * MD_Com_Base_Activities_SMZ_AGED$OTH_BASE
MD_Com_Base_Activities_SMZ_AGED$CO2_base_OFF_AGED <- MD_Com_Base_Activities_SMZ_AGED$CO2_Rate_OFF_ADJ * MD_Com_Base_Activities_SMZ_AGED$OFF_BASE
MD_Com_Base_Activities_SMZ_AGED$CO2_base_Com_AGED <- MD_Com_Base_Activities_SMZ_AGED$CO2_base_RE_AGED + MD_Com_Base_Activities_SMZ_AGED$CO2_base_OFF_AGED + 
    MD_Com_Base_Activities_SMZ_AGED$CO2_base_OTH_AGED
### Sum Total Aged Base Emissions
MD_Com_Base_Activities_SMZ_AGED$CO2_base_Com_Total_AGED <- MD_Com_Base_Activities_SMZ_AGED$CO2_base_RE_AGED + MD_Com_Base_Activities_SMZ_AGED$CO2_base_OFF_AGED + 
    MD_Com_Base_Activities_SMZ_AGED$CO2_base_OTH_AGED

### Add Aged Base Emissions to Projected Emissions Growth
MD_Com_SMZ_SIM_RESULT_AGG$CO2_base_Com_AGED = MD_Com_Base_Activities_SMZ_AGED[match(MD_Com_SMZ_SIM_RESULT_AGG$SMZ_N, MD_Com_Base_Activities_SMZ_AGED$SMZ_N), 
    "CO2_base_Com_AGED"]

### Add Base Emissions to Projected Emissions Growth
MD_Com_SMZ_SIM_RESULT_AGG$CO2_base_Com = MD_Com_Base_Activities_SMZ[match(MD_Com_SMZ_SIM_RESULT_AGG$SMZ_N, MD_Com_Base_Activities_SMZ$SMZ_N), 
    "CO2_base_Com_Total"]

MD_Com_SMZ_SIM_RESULT_AGG$CO2_Proj_Com <- MD_Com_SMZ_SIM_RESULT_AGG$CO2_base_Com_AGED + MD_Com_SMZ_SIM_RESULT_AGG$CO2_EST_ADJ

### Clean Final Data Set
MD_Com_SMZ_SIM_RESULT_AGG[is.na(MD_Com_SMZ_SIM_RESULT_AGG)] <- 0

### Write Final CO2 Output
write.csv(MD_Com_SMZ_SIM_RESULT_AGG[, c("SMZ_N", "CO2_base_Com", "CO2_Proj_Com")], file = (paste(OUTPATH, "MD_BEM_Com_CO2.csv", sep = "")), 
    row.names = FALSE)

### Take out the trash
file.remove(paste(TEMPATH, "COM_CO2_SAMPLE.csv", sep = ""))

#### END MD COM CO2

##### START Other State COM CO2####

for (i in 1:5) {
    ## Create Generic Res dataframe
    if (i == 1) {
        STATE <- "VA"
    }
    if (i == 2) {
        STATE <- "PA"
    }
    if (i == 3) {
        STATE <- "WV"
    }
    if (i == 4) {
        STATE <- "DE"
    }
    if (i == 5) {
        STATE <- "DC"
    }
    
    Com_Base <- get(paste(STATE, "_Com_Base", sep = ""))
    COM_CO2con <- get(paste(STATE, "_COM_CO2con", sep = ""))
    Com_EFF1a <- get(paste(STATE, "_EFF1a", sep = ""))
    
    
    ##### Estimate Commercial Emissions
    Com_Base$CO2_EST <- Com_ols$coef[1] + (Com_ols$coef[2]) * Com_Base$HDD658 + (Com_ols$coef[3]) * Com_Base$Age + (Com_ols$coef[4]) * Com_Base$SQFTSTRC
    
    ### Calculate Emissions per Worker
    Com_Base$CO2_RE <- Com_Base$CO2_EST * Com_Base$RETAIL
    Com_Base$CO2_OFF <- Com_Base$CO2_EST * Com_Base$Office
    Com_Base$CO2_OTH <- Com_Base$CO2_EST * Com_Base$Other
    
    ### Calculate Number of Workers BY Sector
    Com_Base$Workers <- 1
    Com_Base$Workers_RE <- Com_Base$Workers * Com_Base$RETAIL
    Com_Base$Workers_OFF <- Com_Base$Workers * Com_Base$Office
    Com_Base$Workers_OTH <- Com_Base$Workers * Com_Base$Other
    
    #### Aggregate data By SMZ by sector
    Com_Base_SMZ <- aggregate(cbind(Com_Base$CO2_EST, Com_Base$CO2_RE, Com_Base$CO2_OFF, Com_Base$CO2_OTH, Com_Base$Workers_RE, Com_Base$Workers_OFF, 
        Com_Base$Workers_OTH) ~ Com_Base$SMZ_N, data = Com_Base, sum, na.rm = TRUE)
    names(Com_Base_SMZ)[1] <- "SMZ_N"
    names(Com_Base_SMZ)[2] <- "CO2_EST"
    names(Com_Base_SMZ)[3] <- "CO2_EST_RE"
    names(Com_Base_SMZ)[4] <- "CO2_EST_OFF"
    names(Com_Base_SMZ)[5] <- "CO2_EST_OTH"
    names(Com_Base_SMZ)[6] <- "CO2_Workers_RE"
    names(Com_Base_SMZ)[7] <- "CO2_Workers_OFF"
    names(Com_Base_SMZ)[8] <- "CO2_Workers_OTH"
    
    Com_Base_SMZ_SQFT <- aggregate(cbind(Com_Base$SQFTSTRC) ~ Com_Base$SMZ_N, data = Com_Base, mean, na.rm = TRUE)
    names(Com_Base_SMZ_SQFT)[1] <- "SMZ_N"
    names(Com_Base_SMZ_SQFT)[2] <- "SQFT"
    
    Com_Base_SMZ$SQFT = Com_Base_SMZ_SQFT[match(Com_Base_SMZ$SMZ_N, Com_Base_SMZ_SQFT$SMZ_N), "SQFT"]
    
    ### Rate Per Worker Per Sector
    Com_Base_SMZ$CO2_Rate_RE <- Com_Base_SMZ$CO2_EST_RE/Com_Base_SMZ$CO2_Workers_RE
    Com_Base_SMZ$CO2_Rate_OFF <- Com_Base_SMZ$CO2_EST_OFF/Com_Base_SMZ$CO2_Workers_OFF
    Com_Base_SMZ$CO2_Rate_OTH <- Com_Base_SMZ$CO2_EST_OTH/Com_Base_SMZ$CO2_Workers_OTH
    
    # merge Rates and Activities
    Com_Base_Activities_SMZ = merge(Com_Base_SMZ, Base_SMZ_Activities, by = "SMZ_N")
    
    ### Apply Rates to Activities
    Com_Base_Activities_SMZ$CO2_base_RE <- Com_Base_Activities_SMZ$CO2_Rate_RE * Com_Base_Activities_SMZ$RE_BASE
    Com_Base_Activities_SMZ$CO2_base_OFF <- Com_Base_Activities_SMZ$CO2_Rate_OFF * Com_Base_Activities_SMZ$OFF_BASE
    Com_Base_Activities_SMZ$CO2_base_OTH <- Com_Base_Activities_SMZ$CO2_Rate_OTH * Com_Base_Activities_SMZ$OTH_BASE
    
    ### Replace Errors with Zero
    Com_Base_Activities_SMZ[is.na(Com_Base_Activities_SMZ)] <- 0
    Com_Base_Activities_SMZ[Com_Base_Activities_SMZ == -Inf] <- 0
    Com_Base_Activities_SMZ[Com_Base_Activities_SMZ == Inf] <- 0
    
    ### Emission Rate Adjustment Factor
    Com_Base_Activities_SMZ$Tot_SEC_CO2 <- Com_Base_Activities_SMZ$CO2_base_RE + Com_Base_Activities_SMZ$CO2_base_OFF + Com_Base_Activities_SMZ$CO2_base_OTH
    Tot_BASE_Com_CO2 <- sum(Com_Base_Activities_SMZ$Tot_SEC_CO2, na.rm = TRUE) * 1e-06
    CO2_Com_Rate_Adj <- (COM_CO2con/Tot_BASE_Com_CO2)
    
    ### Adjusted Rate Per Worker Per Sector
    Com_Base_Activities_SMZ$CO2_Rate_RE_ADJ <- Com_Base_Activities_SMZ$CO2_Rate_RE * CO2_Com_Rate_Adj
    Com_Base_Activities_SMZ$CO2_Rate_OFF_ADJ <- Com_Base_Activities_SMZ$CO2_Rate_OFF * CO2_Com_Rate_Adj
    Com_Base_Activities_SMZ$CO2_Rate_OTH_ADJ <- Com_Base_Activities_SMZ$CO2_Rate_OTH * CO2_Com_Rate_Adj
    
    ### Apply Adjusted Rates to Activities
    Com_Base_Activities_SMZ$CO2_base_RE <- Com_Base_Activities_SMZ$CO2_Rate_RE_ADJ * Com_Base_Activities_SMZ$RE_BASE
    Com_Base_Activities_SMZ$CO2_base_OFF <- Com_Base_Activities_SMZ$CO2_Rate_OFF_ADJ * Com_Base_Activities_SMZ$OFF_BASE
    Com_Base_Activities_SMZ$CO2_base_OTH <- Com_Base_Activities_SMZ$CO2_Rate_OTH_ADJ * Com_Base_Activities_SMZ$OTH_BASE
    
    ### Sum Total Base Emissions
    Com_Base_Activities_SMZ$CO2_base_Com_Total <- Com_Base_Activities_SMZ$CO2_base_RE + Com_Base_Activities_SMZ$CO2_base_OFF + Com_Base_Activities_SMZ$CO2_base_OTH
    
    # Merge Base Data, Rates and Projected Activities
    Com_Base_Activities_SMZ_PROJ = merge(Com_Base_Activities_SMZ, Proj_SMZ_Activities, by = "SMZ_N")
    
    ### Calculate Projected Growth
    PROJ_GROWTH_RE <- data.frame(Com_Base_Activities_SMZ_PROJ$SMZ_N, (Com_Base_Activities_SMZ_PROJ$RE_PROJ - Com_Base_Activities_SMZ_PROJ$RE_BASE))
    names(PROJ_GROWTH_RE) <- c("SMZ_N", "PROJ_GROWTH_RE")
    PROJ_GROWTH_OFF <- data.frame(Com_Base_Activities_SMZ_PROJ$SMZ_N, (Com_Base_Activities_SMZ_PROJ$OFF_PROJ - Com_Base_Activities_SMZ_PROJ$OFF_BASE))
    names(PROJ_GROWTH_OFF) <- c("SMZ_N", "PROJ_GROWTH_OFF")
    PROJ_GROWTH_OTH <- data.frame(Com_Base_Activities_SMZ_PROJ$SMZ_N, (Com_Base_Activities_SMZ_PROJ$OTH_PROJ - Com_Base_Activities_SMZ_PROJ$OTH_BASE))
    names(PROJ_GROWTH_OTH) <- c("SMZ_N", "PROJ_GROWTH_OTH")
    PROJ_GROWTH_EMP <- data.frame(Com_Base_Activities_SMZ_PROJ$SMZ_N, (PROJ_GROWTH_RE$PROJ_GROWTH_RE + PROJ_GROWTH_OFF$PROJ_GROWTH_OFF + PROJ_GROWTH_OTH$PROJ_GROWTH_OTH))
    names(PROJ_GROWTH_EMP) <- c("SMZ_N", "PROJ_GROWTH_EMP")
    
    ############ Simulate New Commercial Stock
    Com_SMZ_SIM <- PROJ_GROWTH_EMP
    
    # Attach HDD Data
    Com_SMZ_SIM$HDD = SMZ_HDD[match(Com_SMZ_SIM$SMZ_N, SMZ_HDD$SMZ_N), "HDD"]
    
    # Sampling Function
    sampEMP <- function(x) {
        
        sample(PROJ_GROWTH_EMP + 1, 1)
        
        # Sim commercial Data
        Com_SMZ_SIM$Age <- sapply(Com_SMZ_SIM$PROJ_GROWTH_EMP * TIME + 1, sample, 1) - 1
        Com_SMZ_SIM$SQFTSTRC <- sapply(Com_SMZ_SIM$PROJ_GROWTH_EMP * Com_Base_SMZ$SQFT + 1, sample, 1) - 1
        
        
        ##### Estimate New Stock Commercial Emissions
        Com_SMZ_SIM$CO2_EST <- Com_ols$coef[1] + (Com_ols$coef[2]) * (Com_SMZ_SIM$HDD * DDCCRESULT$HDD) + (Com_ols$coef[3]) * (Com_SMZ_SIM$Age) + 
            (Com_ols$coef[4]) * Com_SMZ_SIM$SQFTSTRC
        
        ### Apply Emissions Adjustment to Projected New Stock
        Com_SMZ_SIM$CO2_EST_ADJ <- Com_SMZ_SIM$CO2_EST * CO2_Com_Rate_Adj * (1 - EFFICIENCYPERCENT)
        write.table(Com_SMZ_SIM[c("SMZ_N", "CO2_EST_ADJ")], file = (paste(TEMPATH, "COM_CO2_SAMPLE.csv", sep = "")), sep = ",", row.names = FALSE, 
            col.names = FALSE, append = TRUE)
    }
    
    replicate(n = 10000, expr = sampEMP())
    
    Com_SMZ_SIM_RESULT <- read.csv(paste(TEMPATH, "COM_CO2_SAMPLE.csv", sep = ""))  #Read back in simulated housing stock
    names(Com_SMZ_SIM_RESULT) <- c("SMZ_N", "CO2_EST_ADJ")
    
    #### Aggregate CO2 data in simulated commercial stock By SMZ
    Com_SMZ_SIM_RESULT_AGG <- aggregate(cbind(Com_SMZ_SIM_RESULT$CO2_EST_ADJ) ~ Com_SMZ_SIM_RESULT$SMZ_N, data = Com_SMZ_SIM_RESULT, mean, na.rm = FALSE)
    names(Com_SMZ_SIM_RESULT_AGG)[1] <- "SMZ_N"
    names(Com_SMZ_SIM_RESULT_AGG)[2] <- "CO2_EST_ADJ"
    
    ########### Age Base Commercial Stock
    if (EFFICIENCYPERCENT > 0) {
        Com_Base_AGED <- Com_Base
        Com_Base_AGED$CU <- 1  #count variable for each commercial unit
        Com_Base_AGED$UID <- 1:nrow(Com_Base_AGED)  #create unique ID for each unit
        Com_Base_AGED_EFF <- subset(Com_Base_AGED[sample(nrow(Com_Base_AGED), sum(Com_Base_AGED$CU) * Com_EFF1a), ])
        Com_Base_AGED_NOEFF <- subset(Com_Base_AGED[!(Com_Base_AGED$UID %in% Com_Base_AGED_EFF$UID), ])
        
        # Age non-upgraded commercial stock
        Com_Base_AGED_NOEFF$Age <- Com_Base_AGED_NOEFF$Age + TIME
        
        # Apply efficiencies to upgraded housing stock
        Com_Base_AGED_EFF$Age <- sample(0:TIME, 1)  #random age between 0 years and the time between potential upgrade date
        Com_Base_AGED_EFF$SQFTSTRC <- Com_Base_AGED_EFF$SQFTSTRC * (1 - EFFICIENCYPERCENT)
        
        ## Merge Efficiency upgraded housing stock with rest of housing stock
        Com_Base_AGED <- rbind(Com_Base_AGED_NOEFF, Com_Base_AGED_EFF)
        
    } else {
        Com_Base_AGED <- Com_Base
        Com_Base_AGED$CU <- 1  #count variable for each commercial unit
    }
    
    
    Com_Base_AGED$CO2_EST <- Com_ols$coef[1] + (Com_ols$coef[2]) * (Com_Base$HDD * DDCCRESULT$HDD) + (Com_ols$coef[3]) * (Com_Base$Age + TIME) + 
        (Com_ols$coef[4]) * Com_Base$SQFTSTRC
    
    ### Calculate Emissions BY Sector per Worker
    Com_Base_AGED$CO2_RE <- Com_Base_AGED$CO2_EST * Com_Base_AGED$RETAIL
    Com_Base_AGED$CO2_OFF <- Com_Base_AGED$CO2_EST * Com_Base_AGED$Office
    Com_Base_AGED$CO2_OTH <- Com_Base_AGED$CO2_EST * Com_Base_AGED$Other
    
    ### Calculate Number of Workers BY Sector
    Com_Base_AGED$Workers_RE <- Com_Base_AGED$Workers * Com_Base_AGED$RETAIL
    Com_Base_AGED$Workers_OFF <- Com_Base_AGED$Workers * Com_Base_AGED$Office
    Com_Base_AGED$Workers_OTH <- Com_Base_AGED$Workers * Com_Base_AGED$Other
    
    #### Aggregate data By SMZ by sector
    Com_Base_SMZ_AGED <- aggregate(cbind(Com_Base_AGED$CO2_EST, Com_Base_AGED$CO2_RE, Com_Base_AGED$CO2_OFF, Com_Base_AGED$CO2_OTH, Com_Base_AGED$Workers_RE, 
        Com_Base_AGED$Workers_OFF, Com_Base_AGED$Workers_OTH) ~ Com_Base_AGED$SMZ_N, data = Com_Base_AGED, sum, na.rm = TRUE)
    names(Com_Base_SMZ_AGED)[1] <- "SMZ_N"
    names(Com_Base_SMZ_AGED)[2] <- "CO2_EST"
    names(Com_Base_SMZ_AGED)[3] <- "CO2_EST_RE"
    names(Com_Base_SMZ_AGED)[4] <- "CO2_EST_OFF"
    names(Com_Base_SMZ_AGED)[5] <- "CO2_EST_OTH"
    names(Com_Base_SMZ_AGED)[6] <- "CO2_Workers_RE"
    names(Com_Base_SMZ_AGED)[7] <- "CO2_Workers_OFF"
    names(Com_Base_SMZ_AGED)[8] <- "CO2_Workers_OTH"
    
    ### Aged Rate Per Worker Per Sector
    Com_Base_SMZ_AGED$CO2_Rate_RE <- Com_Base_SMZ_AGED$CO2_EST_RE/Com_Base_SMZ_AGED$CO2_Workers_RE
    Com_Base_SMZ_AGED$CO2_Rate_OFF <- Com_Base_SMZ_AGED$CO2_EST_OFF/Com_Base_SMZ_AGED$CO2_Workers_OFF
    Com_Base_SMZ_AGED$CO2_Rate_OTH <- Com_Base_SMZ_AGED$CO2_EST_OTH/Com_Base_SMZ_AGED$CO2_Workers_OTH
    
    # Merge Rates and Activities for Aged Commercial Stock
    Com_Base_Activities_SMZ_AGED = merge(Com_Base_SMZ_AGED, Base_SMZ_Activities, by = "SMZ_N")
    
    ## Apply New Rates to Aged Workers
    Com_Base_Activities_SMZ_AGED$CO2_base_RE <- Com_Base_Activities_SMZ_AGED$CO2_Rate_RE * Com_Base_Activities_SMZ_AGED$RE_BASE
    Com_Base_Activities_SMZ_AGED$CO2_base_OFF <- Com_Base_Activities_SMZ_AGED$CO2_Rate_OFF * Com_Base_Activities_SMZ_AGED$OFF_BASE
    Com_Base_Activities_SMZ_AGED$CO2_base_OTH <- Com_Base_Activities_SMZ_AGED$CO2_Rate_OTH * Com_Base_Activities_SMZ_AGED$OTH_BASE
    
    ### Replace Errors with Zero
    Com_Base_Activities_SMZ_AGED[is.na(Com_Base_Activities_SMZ_AGED)] <- 0
    Com_Base_Activities_SMZ_AGED[Com_Base_Activities_SMZ_AGED == -Inf] <- 0
    Com_Base_Activities_SMZ_AGED[Com_Base_Activities_SMZ_AGED == Inf] <- 0
    
    ### Adjusted CO2 Rate Per Aged Worker
    Com_Base_Activities_SMZ_AGED$CO2_Rate_RE_ADJ <- Com_Base_Activities_SMZ_AGED$CO2_Rate_RE * CO2_Com_Rate_Adj
    Com_Base_Activities_SMZ_AGED$CO2_Rate_OFF_ADJ <- Com_Base_Activities_SMZ_AGED$CO2_Rate_OFF * CO2_Com_Rate_Adj
    Com_Base_Activities_SMZ_AGED$CO2_Rate_OTH_ADJ <- Com_Base_Activities_SMZ_AGED$CO2_Rate_OTH * CO2_Com_Rate_Adj
    
    ### Apply Adjusted Aged Rates to Activities
    Com_Base_Activities_SMZ_AGED$CO2_base_RE_AGED <- Com_Base_Activities_SMZ_AGED$CO2_Rate_RE_ADJ * Com_Base_Activities_SMZ_AGED$RE_BASE
    Com_Base_Activities_SMZ_AGED$CO2_base_OTH_AGED <- Com_Base_Activities_SMZ_AGED$CO2_Rate_OTH_ADJ * Com_Base_Activities_SMZ_AGED$OTH_BASE
    Com_Base_Activities_SMZ_AGED$CO2_base_OFF_AGED <- Com_Base_Activities_SMZ_AGED$CO2_Rate_OFF_ADJ * Com_Base_Activities_SMZ_AGED$OFF_BASE
    Com_Base_Activities_SMZ_AGED$CO2_base_Com_AGED <- Com_Base_Activities_SMZ_AGED$CO2_base_RE_AGED + Com_Base_Activities_SMZ_AGED$CO2_base_OFF_AGED + 
        Com_Base_Activities_SMZ_AGED$CO2_base_OTH_AGED
    ### Sum Total Aged Base Emissions
    Com_Base_Activities_SMZ_AGED$CO2_base_Com_Total_AGED <- Com_Base_Activities_SMZ_AGED$CO2_base_RE_AGED + Com_Base_Activities_SMZ_AGED$CO2_base_OFF_AGED + 
        Com_Base_Activities_SMZ_AGED$CO2_base_OTH_AGED
    
    ### Add Aged Base Emissions to Projected Emissions Growth
    Com_SMZ_SIM_RESULT_AGG$CO2_base_Com_AGED = Com_Base_Activities_SMZ_AGED[match(Com_SMZ_SIM_RESULT_AGG$SMZ_N, Com_Base_Activities_SMZ_AGED$SMZ_N), 
        "CO2_base_Com_AGED"]
    
    ### Add Base Emissions to Projected Emissions Growth
    Com_SMZ_SIM_RESULT_AGG$CO2_base_Com = Com_Base_Activities_SMZ[match(Com_SMZ_SIM_RESULT_AGG$SMZ_N, Com_Base_Activities_SMZ$SMZ_N), "CO2_base_Com_Total"]
    
    Com_SMZ_SIM_RESULT_AGG$CO2_Proj_Com <- Com_SMZ_SIM_RESULT_AGG$CO2_base_Com_AGED + Com_SMZ_SIM_RESULT_AGG$CO2_EST_ADJ
    
    ### Clean Final Data Set
    Com_SMZ_SIM_RESULT_AGG[is.na(Com_SMZ_SIM_RESULT_AGG)] <- 0
    
    ### Write Final CO2 Output
    write.csv(Com_SMZ_SIM_RESULT_AGG[, c("SMZ_N", "CO2_base_Com", "CO2_Proj_Com")], file = (paste(OUTPATH, STATE, "_", "BEM_Com_CO2.csv", sep = "")), 
        row.names = FALSE)
    
    ### Take out the trash
    file.remove(paste(TEMPATH, "COM_CO2_SAMPLE.csv", sep = ""))
}
#### END Other State COM CO2

############## END Commercial CO2 MODEL


#### 50% Complete!


############## Begin Residential CO2 MODEL

#### Prep Variable for CO2 Binary logistic model
RECS$CO2_PROD <- NA
RECS$CO2_PROD[RECS$TCO2 > 0] <- 1
RECS$CO2_PROD[RECS$TCO2 == 0] <- 0
table(RECS$CO2_PROD)

##### Fit Binary Logistic
wlogit <- glm(CO2_PROD ~ HD65 + AGE + BEDROOMS + SF1, data = RECS, family = "binomial")
summary(wlogit)

### Calculate Building Age
MD_Res_Base$Age <- NA
MD_Res_Base$Age <- 2007 - MD_Res_Base$Yearval

### Calculate Building Beds
MD_Res_Base$Beds <- NA
MD_Res_Base$Beds <- MD_Res_Base$fbth_numb + 1

### Calculate Housing Type
MD_Res_Base$SF1 <- NA
MD_Res_Base$SF1[MD_Res_Base$dwll_type == 1] <- 1
MD_Res_Base$SF1[MD_Res_Base$dwll_type != 1] <- 0

### START MD_Res CO2###

## Create Count Dummy Variable Households
MD_Res_Base$HH <- 1


##### Estimate Probability of Residential Emissions
MD_Res_Base$CO2_PROB <- exp(wlogit$coef[1] + wlogit$coef[2] * MD_Res_Base$htdd + wlogit$coef[3] * MD_Res_Base$Age + wlogit$coef[4] * MD_Res_Base$Beds + 
    wlogit$coef[5] * MD_Res_Base$SF1)/(1 + exp(wlogit$coef[1] + wlogit$coef[2] * MD_Res_Base$htdd + wlogit$coef[3] * MD_Res_Base$Age + wlogit$coef[4] * 
    MD_Res_Base$Beds + wlogit$coef[5] * MD_Res_Base$SF1))
### Replace Errors with Zero
MD_Res_Base[is.na(MD_Res_Base)] <- 0
MD_Res_Base[MD_Res_Base == -Inf] <- 0
MD_Res_Base[MD_Res_Base == Inf] <- 0

##### Select observations based on Probability of CO2 Emissions
MD_Res_Base2 <- MD_Res_Base[which(MD_Res_Base$CO2_PROB > 0.99), ]

##### Select observations based on CO2 Emissions Greater than zero
RECS2 <- RECS[which(RECS$TCO2 > 0), ]

##### Fit HH_ols
HH_ols <- lm(TCO2 ~ HD65 + AGE + SF1 + TOTSQFT + WALLTYPE4 + EQUIPM10 + EQUIPM3, data = RECS2)
summary(HH_ols)

##### Estimate Residential Emissions
MD_Res_Base2$CO2_EST <- HH_ols$coef[1] + HH_ols$coef[2] * MD_Res_Base2$htdd + HH_ols$coef[3] * MD_Res_Base2$Age + HH_ols$coef[4] * MD_Res_Base2$SF1 + 
    HH_ols$coef[5] * MD_Res_Base2$sqftstrc + HH_ols$coef[6] * MD_Res_Base2$Stucco + HH_ols$coef[7] * MD_Res_Base2$fire + HH_ols$coef[8] * MD_Res_Base2$radiator


## Count CO2 Emitting Households
MD_Res_Base2$HH <- NA
MD_Res_Base2$HH[MD_Res_Base2$CO2_EST > 0] <- 1
MD_Res_Base2$HH[MD_Res_Base2$CO2_EST <= 0] <- 0

#### Aggregate data By SMZ
MD_Res_Base2_SMZ <- aggregate(cbind(MD_Res_Base2$CO2_EST, MD_Res_Base2$HH) ~ MD_Res_Base2$SMZ_N, data = MD_Res_Base2, sum, na.rm = TRUE)
names(MD_Res_Base2_SMZ)[1] <- "SMZ_N"
names(MD_Res_Base2_SMZ)[2] <- "CO2_EST"
names(MD_Res_Base2_SMZ)[3] <- "HH"

MD_Res_Base2_SMZ_SQFT <- aggregate(cbind(MD_Res_Base2$sqftstrc) ~ MD_Res_Base2$SMZ_N, data = MD_Res_Base2, mean, na.rm = TRUE)
names(MD_Res_Base2_SMZ_SQFT)[1] <- "SMZ_N"
names(MD_Res_Base2_SMZ_SQFT)[2] <- "SQFT"

### Rate Per HH
MD_Res_Base2_SMZ$CO2_Rate_HH <- MD_Res_Base2_SMZ$CO2_EST/MD_Res_Base2_SMZ$HH

# merge Rates and Activities
MD_Res_Base2_Activities_SMZ = merge(MD_Res_Base2_SMZ, Base_SMZ_Activities, by = "SMZ_N")

### Apply Rates to Households
MD_Res_Base2_Activities_SMZ$CO2_base_HH <- MD_Res_Base2_Activities_SMZ$CO2_Rate_HH * MD_Res_Base2_Activities_SMZ$HH_BASE

### Replace Errors with Zero
MD_Res_Base2_Activities_SMZ[is.na(MD_Res_Base2_Activities_SMZ)] <- 0
MD_Res_Base2_Activities_SMZ[MD_Res_Base2_Activities_SMZ == -Inf] <- 0
MD_Res_Base2_Activities_SMZ[MD_Res_Base2_Activities_SMZ == Inf] <- 0

### Emission Rate Adjustment Factor
MD_Res_Base2_Activities_SMZ$Tot_HH_CO2 <- MD_Res_Base2_Activities_SMZ$CO2_base_HH  # + MD_Res_Base2_Activities_SMZ$CO2_base_OFF + MD_Res_Base2_Activities_SMZ$CO2_base_OTH
Tot_BASE_HH_CO2 <- sum(MD_Res_Base2_Activities_SMZ$Tot_HH_CO2, na.rm = TRUE) * 1e-06
CO2_HH_Rate_Adj <- (MD_RES_CO2con/Tot_BASE_HH_CO2)

### Adjusted Rate Per Household
MD_Res_Base2_Activities_SMZ$CO2_Rate_HH_ADJ <- MD_Res_Base2_Activities_SMZ$CO2_Rate_HH * CO2_HH_Rate_Adj

### Apply Adjusted Rates to Activities
MD_Res_Base2_Activities_SMZ$CO2_base_HH <- MD_Res_Base2_Activities_SMZ$CO2_Rate_HH_ADJ * MD_Res_Base2_Activities_SMZ$HH_BASE

### Sum Total Base Emissions
MD_Res_Base2_Activities_SMZ$CO2_base_HH_Total <- MD_Res_Base2_Activities_SMZ$CO2_base_HH  #Leave for additional household type expansion

# Merge Base Data, Rates and Projected Activities
MD_Res_Base2_Activities_SMZ_PROJ = merge(MD_Res_Base2_Activities_SMZ, Proj_SMZ_Activities, by = "SMZ_N")


### Calculate Projected Growth
PROJ_GROWTH_HH <- data.frame(MD_Res_Base2_Activities_SMZ_PROJ$SMZ_N, (MD_Res_Base2_Activities_SMZ_PROJ$HH_PROJ - MD_Res_Base2_Activities_SMZ_PROJ$HH_BASE))
names(PROJ_GROWTH_HH) <- c("SMZ_N", "PROJ_GROWTH_HH")

############ New Housing Stock Simulator
MD_Res_SMZ_SIM <- PROJ_GROWTH_HH

# Attach HDD and SQFT Data
MD_Res_SMZ_SIM$HDD = SMZ_HDD[match(MD_Res_SMZ_SIM$SMZ_N, SMZ_HDD$SMZ_N), "HDD"]
MD_Res_SMZ_SIM$SQFT = MD_Res_Base2_SMZ_SQFT[match(MD_Res_SMZ_SIM$SMZ_N, MD_Res_Base2_SMZ_SQFT$SMZ_N), "SQFT"]

# Sampling Function
sampHH <- function(x) {
    
    sample(PROJ_GROWTH_HH + 1, 1)
    
    # Sim Housing Data
    MD_Res_SMZ_SIM$Age <- sapply(MD_Res_SMZ_SIM$PROJ_GROWTH_HH * TIME + 1, sample, 1) - 1
    MD_Res_SMZ_SIM$SF1 <- sapply(MD_Res_SMZ_SIM$PROJ_GROWTH_HH + 1, sample, 1) - 1
    MD_Res_SMZ_SIM$sqftstrc <- sapply(MD_Res_SMZ_SIM$PROJ_GROWTH_HH * MD_Res_SMZ_SIM$SQFT + 1, sample, 1) - 1
    MD_Res_SMZ_SIM$Stucco <- sapply(MD_Res_SMZ_SIM$PROJ_GROWTH_HH + 1, sample, 1) - 1
    MD_Res_SMZ_SIM$fire <- sapply(MD_Res_SMZ_SIM$PROJ_GROWTH_HH + 1, sample, 1) - 1
    MD_Res_SMZ_SIM$radiator <- sapply(MD_Res_SMZ_SIM$PROJ_GROWTH_HH + 1, sample, 1) - 1
    
    ##### Estimate New Stock Residential Emissions
    MD_Res_SMZ_SIM$CO2_EST <- HH_ols$coef[1] + HH_ols$coef[2] * (MD_Res_SMZ_SIM$HDD * DDCCRESULT$HDD) + HH_ols$coef[3] * MD_Res_SMZ_SIM$Age + 
        HH_ols$coef[4] * MD_Res_SMZ_SIM$SF1 + HH_ols$coef[5] * MD_Res_SMZ_SIM$sqftstrc + HH_ols$coef[6] * MD_Res_SMZ_SIM$Stucco + HH_ols$coef[7] * 
        MD_Res_SMZ_SIM$fire + HH_ols$coef[8] * MD_Res_SMZ_SIM$radiator
    
    ### Apply Emissions Adjustment to Projected New Stock
    MD_Res_SMZ_SIM$CO2_EST_ADJ <- MD_Res_SMZ_SIM$CO2_EST * CO2_HH_Rate_Adj * (1 - EFFICIENCYPERCENT)
    write.table(MD_Res_SMZ_SIM[c("SMZ_N", "CO2_EST_ADJ")], file = (paste(TEMPATH, "RES_CO2_SAMPLE.csv", sep = "")), sep = ",", row.names = FALSE, 
        col.names = FALSE, append = TRUE)
}

replicate(n = 10000, expr = sampHH())

MD_Res_SMZ_SIM_RESULT <- read.csv(paste(TEMPATH, "RES_CO2_SAMPLE.csv", sep = ""))  #Read back in simulated housing stock
names(MD_Res_SMZ_SIM_RESULT) <- c("SMZ_N", "CO2_EST_ADJ")

#### Aggregate CO2 data in simulated housing stock By SMZ
MD_Res_SMZ_SIM_RESULT_AGG <- aggregate(cbind(MD_Res_SMZ_SIM_RESULT$CO2_EST_ADJ) ~ MD_Res_SMZ_SIM_RESULT$SMZ_N, data = MD_Res_SMZ_SIM_RESULT, 
    mean, na.rm = FALSE)
names(MD_Res_SMZ_SIM_RESULT_AGG)[1] <- "SMZ_N"
names(MD_Res_SMZ_SIM_RESULT_AGG)[2] <- "CO2_EST_ADJ"

# MD_Res_SMZ_SIM_RESULT_AGG_MAX <-aggregate(cbind(MD_Res_SMZ_SIM_RESULT2$CO2_EST_ADJ)~MD_Res_SMZ_SIM_RESULT2$SMZ_N,
# data=MD_Res_SMZ_SIM_RESULT2, max, na.rm=TRUE) names(MD_Res_SMZ_SIM_RESULT_AGG_MAX)[1] <- 'SMZ_N' names(MD_Res_SMZ_SIM_RESULT_AGG_MAX)[2]
# <- 'CO2_EST_ADJ_MAX'

# MD_Res_SMZ_SIM_RESULT_AGG_MIN <-aggregate(cbind(MD_Res_SMZ_SI#M_RESULT2$CO2_EST_ADJ)~MD_Res_SMZ_SIM_RESULT2$SMZ_N,
# data=MD_Res_SMZ_SIM_RESULT2, min, na.rm=TRUE) names(MD_Res_SMZ_SIM_RESULT_AGG_MIN)[1] <- 'SMZ_N' names(MD_Res_SMZ_SIM_RESULT_AGG_MIN)[2]
# <- 'CO2_EST_ADJ_MIN'

### Add MAX and MIN Emissions to Average MD_Res_SMZ_SIM_RESULT_AGG$CO2_EST_ADJ_MAX =
### MD_Res_SMZ_SIM_RESULT_AGG_MAX[match(MD_Res_SMZ_SIM_RESULT_AGG$SMZ_N, MD_Res_SMZ_SIM_RESULT_AGG_MAX$SMZ_N),'CO2_EST_ADJ_MAX']
### MD_Res_SMZ_SIM_RESULT_AGG$CO2_EST_ADJ_MIN = MD_Res_SMZ_SIM_RESULT_AGG_MIN[match(MD_Res_SMZ_SIM_RESULT_AGG$SMZ_N,
### MD_Res_SMZ_SIM_RESULT_AGG_MIN$SMZ_N),'CO2_EST_ADJ_MIN']


############ Age Base Housing Stock
if (EFFICIENCYPERCENT > 0) {
    MD_Res_Base2_AGED <- MD_Res_Base2
    MD_Res_Base2_AGED$UID <- 1:nrow(MD_Res_Base2_AGED)  #create unique ID for each unit
    
    ## Apply Efficency upgrades
    MD_Res_Base2_AGED_EFF <- subset(MD_Res_Base2_AGED[sample(nrow(MD_Res_Base2_AGED), sum(MD_Res_Base2_AGED$HH) * MD_EFF1a), ])
    MD_Res_Base2_AGED_NOEFF <- subset(MD_Res_Base2_AGED[!(MD_Res_Base2_AGED$UID %in% MD_Res_Base2_AGED_EFF$UID), ])
    
    # Age non-upgraded housing
    MD_Res_Base2_AGED_NOEFF$Age <- MD_Res_Base2_AGED_NOEFF$Age + TIME
    
    # Apply efficiencies to upgraded housing stock
    MD_Res_Base2_AGED_EFF$Age <- sample(0:TIME, 1)  #random age between 0 years and the time between potential upgrade date
    MD_Res_Base2_AGED_EFF$Stucco <- 0
    MD_Res_Base2_AGED_EFF$fire <- 0
    MD_Res_Base2_AGED_EFF$radiator <- 0
    
    ## Merge Efficiency upgraded housing stock with rest of housing stock
    MD_Res_Base2_AGED <- rbind(MD_Res_Base2_AGED_NOEFF, MD_Res_Base2_AGED_EFF)
    
    
} else {
    MD_Res_Base2_AGED <- MD_Res_Base2
    MD_Res_Base2_AGED$UID <- 1:nrow(MD_Res_Base2_AGED)  #create unique ID for each unit
}

##### Estimate Residential Emissions With added age constant
MD_Res_Base2_AGED$CO2_EST <- HH_ols$coef[1] + HH_ols$coef[2] * (MD_Res_Base2$htdd * DDCCRESULT$HDD) + HH_ols$coef[3] * MD_Res_Base2$Age + HH_ols$coef[4] * 
    MD_Res_Base2$SF1 + HH_ols$coef[5] * MD_Res_Base2$sqftstrc + HH_ols$coef[6] * MD_Res_Base2$Stucco + HH_ols$coef[7] * MD_Res_Base2$fire + 
    HH_ols$coef[8] * MD_Res_Base2$radiator

## Count CO2 Emitting Households in the Aged housing stock
MD_Res_Base2_AGED$HH <- NA
MD_Res_Base2_AGED$HH[MD_Res_Base2$CO2_EST > 0] <- 1
MD_Res_Base2_AGED$HH[MD_Res_Base2$CO2_EST <= 0] <- 0

#### Aggregate CO2 data in aged housing stock By SMZ
MD_Res_Base2_SMZ_AGED <- aggregate(cbind(MD_Res_Base2_AGED$CO2_EST, MD_Res_Base2_AGED$HH) ~ MD_Res_Base2_AGED$SMZ_N, data = MD_Res_Base2_AGED, 
    sum, na.rm = TRUE)
names(MD_Res_Base2_SMZ_AGED)[1] <- "SMZ_N"
names(MD_Res_Base2_SMZ_AGED)[2] <- "CO2_EST"
names(MD_Res_Base2_SMZ_AGED)[3] <- "HH"

### Calculate New Rate Per HH for Aged Housing Stock
MD_Res_Base2_SMZ_AGED$CO2_Rate_HH <- MD_Res_Base2_SMZ_AGED$CO2_EST/MD_Res_Base2_SMZ_AGED$HH

# Merge Rates and Activities for Aged Housing Stock
MD_Res_Base2_Activities_SMZ_AGED = merge(MD_Res_Base2_SMZ_AGED, Base_SMZ_Activities, by = "SMZ_N")

### Apply New Rates to Aged Households
MD_Res_Base2_Activities_SMZ_AGED$CO2_base_HH <- MD_Res_Base2_Activities_SMZ_AGED$CO2_Rate_HH * MD_Res_Base2_Activities_SMZ_AGED$HH_BASE

### Replace Errors with Zero
MD_Res_Base2_Activities_SMZ_AGED[is.na(MD_Res_Base2_Activities_SMZ_AGED)] <- 0
MD_Res_Base2_Activities_SMZ_AGED[MD_Res_Base2_Activities_SMZ_AGED == -Inf] <- 0
MD_Res_Base2_Activities_SMZ_AGED[MD_Res_Base2_Activities_SMZ_AGED == Inf] <- 0

### Adjusted CO2 Rate Per Aged Household
MD_Res_Base2_Activities_SMZ_AGED$CO2_Rate_HH_ADJ <- MD_Res_Base2_Activities_SMZ_AGED$CO2_Rate_HH * CO2_HH_Rate_Adj

### Apply Adjusted Aged Rates to Activities
MD_Res_Base2_Activities_SMZ_AGED$CO2_base_HH_AGED <- MD_Res_Base2_Activities_SMZ_AGED$CO2_Rate_HH_ADJ * MD_Res_Base2_Activities_SMZ_AGED$HH_BASE

### Sum Total Aged Base Emissions
MD_Res_Base2_Activities_SMZ_AGED$CO2_base_HH_Total_AGED <- MD_Res_Base2_Activities_SMZ_AGED$CO2_base_HH_AGED  #+ MD_Res_Base2_Activities_SMZ$CO2_base_OFF + MD_Res_Base2_Activities_SMZ$CO2_base_OTH

### Add Aged Base Emissions to Projected Emissions Growth
MD_Res_SMZ_SIM_RESULT_AGG$CO2_base_HH_AGED = MD_Res_Base2_Activities_SMZ_AGED[match(MD_Res_SMZ_SIM_RESULT_AGG$SMZ_N, MD_Res_Base2_Activities_SMZ_AGED$SMZ_N), 
    "CO2_base_HH_AGED"]

### Add Base Emissions to Projected Emissions Growth
MD_Res_SMZ_SIM_RESULT_AGG$CO2_base_HH = MD_Res_Base2_Activities_SMZ[match(MD_Res_SMZ_SIM_RESULT_AGG$SMZ_N, MD_Res_Base2_Activities_SMZ$SMZ_N), 
    "CO2_base_HH"]

MD_Res_SMZ_SIM_RESULT_AGG$CO2_Proj_HH <- MD_Res_SMZ_SIM_RESULT_AGG$CO2_base_HH_AGED + MD_Res_SMZ_SIM_RESULT_AGG$CO2_EST_ADJ

### Clean Final Data Set
MD_Res_SMZ_SIM_RESULT_AGG[is.na(MD_Res_SMZ_SIM_RESULT_AGG)] <- 0

### Write Final Res CO2 Output
write.csv(MD_Res_SMZ_SIM_RESULT_AGG[, c("SMZ_N", "CO2_base_HH", "CO2_Proj_HH")], file = (paste(OUTPATH, "MD_BEM_RES_CO2.csv", sep = "")), row.names = FALSE)

### Take out the trash
file.remove(paste(TEMPATH, "RES_CO2_SAMPLE.csv", sep = ""))


### END MD_Res CO2###

### START Other Res CO2###

for (i in 1:5) {
    ## Create Generic Res dataframe
    if (i == 1) {
        STATE <- "VA"
    }
    if (i == 2) {
        STATE <- "PA"
    }
    if (i == 3) {
        STATE <- "WV"
    }
    if (i == 4) {
        STATE <- "DE"
    }
    if (i == 5) {
        STATE <- "DC"
    }
    
    Res_Base <- get(paste(STATE, "_Res_Base", sep = ""))
    RES_CO2con <- get(paste(STATE, "_RES_CO2con", sep = ""))
    Res_EFF1a <- get(paste(STATE, "_EFF1a", sep = ""))
    ## Create Count Dummy Variable Households
    Res_Base$HH <- 1
    
    ##### Estimate Probability of Residential Emissions
    Res_Base$CO2_PROB <- exp(wlogit$coef[1] + wlogit$coef[2] * Res_Base$htdd + wlogit$coef[3] * Res_Base$Age + wlogit$coef[4] * Res_Base$Beds + 
        wlogit$coef[5] * Res_Base$SF1)/(1 + exp(wlogit$coef[1] + wlogit$coef[2] * Res_Base$htdd + wlogit$coef[3] * Res_Base$Age + wlogit$coef[4] * 
        Res_Base$Beds + wlogit$coef[5] * Res_Base$SF1))
    ### Replace Errors with Zero
    Res_Base[is.na(Res_Base)] <- 0
    Res_Base[Res_Base == -Inf] <- 0
    Res_Base[Res_Base == Inf] <- 0
    
    ##### Select observations based on Probability of CO2 Emissions
    Res_Base2 <- Res_Base[which(Res_Base$CO2_PROB > 0.99), ]
    
    ##### Select observations based on CO2 Emissions Greater than zero
    RECS2 <- RECS[which(RECS$TCO2 > 0), ]
    
    ##### Fit HH_ols
    HH_ols <- lm(TCO2 ~ HD65 + AGE + SF1 + TOTSQFT + WALLTYPE4 + EQUIPM10 + EQUIPM3, data = RECS2)
    summary(HH_ols)
    
    ##### Estimate Residential Emissions
    Res_Base2$CO2_EST <- HH_ols$coef[1] + HH_ols$coef[2] * Res_Base2$htdd + HH_ols$coef[3] * Res_Base2$Age + HH_ols$coef[4] * Res_Base2$SF1 + 
        HH_ols$coef[5] * Res_Base2$sqftstrc + HH_ols$coef[6] * Res_Base2$Stucco + HH_ols$coef[7] * Res_Base2$fire + HH_ols$coef[8] * Res_Base2$radiator
    
    
    ## Count CO2 Emitting Households
    Res_Base2$HH <- NA
    Res_Base2$HH[Res_Base2$CO2_EST > 0] <- 1
    Res_Base2$HH[Res_Base2$CO2_EST <= 0] <- 0
    
    #### Aggregate data By SMZ
    Res_Base2_SMZ <- aggregate(cbind(Res_Base2$CO2_EST, Res_Base2$HH) ~ Res_Base2$SMZ_N, data = Res_Base2, sum, na.rm = TRUE)
    names(Res_Base2_SMZ)[1] <- "SMZ_N"
    names(Res_Base2_SMZ)[2] <- "CO2_EST"
    names(Res_Base2_SMZ)[3] <- "HH"
    
    Res_Base2_SMZ_SQFT <- aggregate(cbind(Res_Base2$sqftstrc) ~ Res_Base2$SMZ_N, data = Res_Base2, mean, na.rm = TRUE)
    names(Res_Base2_SMZ_SQFT)[1] <- "SMZ_N"
    names(Res_Base2_SMZ_SQFT)[2] <- "SQFT"
    
    ### Rate Per HH
    Res_Base2_SMZ$CO2_Rate_HH <- Res_Base2_SMZ$CO2_EST/Res_Base2_SMZ$HH
    
    # merge Rates and Activities
    Res_Base2_Activities_SMZ = merge(Res_Base2_SMZ, Base_SMZ_Activities, by = "SMZ_N")
    
    ### Apply Rates to Households
    Res_Base2_Activities_SMZ$CO2_base_HH <- Res_Base2_Activities_SMZ$CO2_Rate_HH * Res_Base2_Activities_SMZ$HH_BASE
    
    ### Replace Errors with Zero
    Res_Base2_Activities_SMZ[is.na(Res_Base2_Activities_SMZ)] <- 0
    Res_Base2_Activities_SMZ[Res_Base2_Activities_SMZ == -Inf] <- 0
    Res_Base2_Activities_SMZ[Res_Base2_Activities_SMZ == Inf] <- 0
    
    ### Emission Rate Adjustment Factor
    Res_Base2_Activities_SMZ$Tot_HH_CO2 <- Res_Base2_Activities_SMZ$CO2_base_HH  # + Res_Base2_Activities_SMZ$CO2_base_OFF + Res_Base2_Activities_SMZ$CO2_base_OTH
    Tot_BASE_HH_CO2 <- sum(Res_Base2_Activities_SMZ$Tot_HH_CO2, na.rm = TRUE) * 1e-06
    CO2_HH_Rate_Adj <- (RES_CO2con/Tot_BASE_HH_CO2)
    
    ### Adjusted Rate Per Household
    Res_Base2_Activities_SMZ$CO2_Rate_HH_ADJ <- Res_Base2_Activities_SMZ$CO2_Rate_HH * CO2_HH_Rate_Adj
    
    ### Apply Adjusted Rates to Activities
    Res_Base2_Activities_SMZ$CO2_base_HH <- Res_Base2_Activities_SMZ$CO2_Rate_HH_ADJ * Res_Base2_Activities_SMZ$HH_BASE
    
    ### Sum Total Base Emissions
    Res_Base2_Activities_SMZ$CO2_base_HH_Total <- Res_Base2_Activities_SMZ$CO2_base_HH  #Leave for additional household type expansion
    
    # Merge Base Data, Rates and Projected Activities
    Res_Base2_Activities_SMZ_PROJ = merge(Res_Base2_Activities_SMZ, Proj_SMZ_Activities, by = "SMZ_N")
    
    # Export updated dataset write.csv(Res_Base2_Activities_SMZ_PROJ,file='D:/BEM2/OUTPUT/check.csv')
    
    ### Calculate Projected Growth
    PROJ_GROWTH_HH <- data.frame(Res_Base2_Activities_SMZ_PROJ$SMZ_N, (Res_Base2_Activities_SMZ_PROJ$HH_PROJ - Res_Base2_Activities_SMZ_PROJ$HH_BASE))
    names(PROJ_GROWTH_HH) <- c("SMZ_N", "PROJ_GROWTH_HH")
    
    ############ New Housing Stock Simulator
    Res_SMZ_SIM <- PROJ_GROWTH_HH
    
    # Attach HDD and SQFT Data
    Res_SMZ_SIM$HDD = SMZ_HDD[match(Res_SMZ_SIM$SMZ_N, SMZ_HDD$SMZ_N), "HDD"]
    Res_SMZ_SIM$SQFT = Res_Base2_SMZ_SQFT[match(Res_SMZ_SIM$SMZ_N, Res_Base2_SMZ_SQFT$SMZ_N), "SQFT"]
    
    # Sampling Function
    sampHH <- function(x) {
        
        sample(PROJ_GROWTH_HH + 1, 1)
        
        # Sim Housing Data
        Res_SMZ_SIM$Age <- sapply(Res_SMZ_SIM$PROJ_GROWTH_HH * TIME + 1, sample, 1) - 1
        Res_SMZ_SIM$SF1 <- sapply(Res_SMZ_SIM$PROJ_GROWTH_HH + 1, sample, 1) - 1
        Res_SMZ_SIM$sqftstrc <- sapply(Res_SMZ_SIM$PROJ_GROWTH_HH * Res_SMZ_SIM$SQFT + 1, sample, 1) - 1
        Res_SMZ_SIM$Stucco <- sapply(Res_SMZ_SIM$PROJ_GROWTH_HH + 1, sample, 1) - 1
        Res_SMZ_SIM$fire <- sapply(Res_SMZ_SIM$PROJ_GROWTH_HH + 1, sample, 1) - 1
        Res_SMZ_SIM$radiator <- sapply(Res_SMZ_SIM$PROJ_GROWTH_HH + 1, sample, 1) - 1
        
        ##### Estimate New Stock Residential Emissions
        Res_SMZ_SIM$CO2_EST <- HH_ols$coef[1] + HH_ols$coef[2] * (Res_SMZ_SIM$HDD * DDCCRESULT$HDD) + HH_ols$coef[3] * Res_SMZ_SIM$Age + HH_ols$coef[4] * 
            Res_SMZ_SIM$SF1 + HH_ols$coef[5] * Res_SMZ_SIM$sqftstrc + HH_ols$coef[6] * Res_SMZ_SIM$Stucco + HH_ols$coef[7] * Res_SMZ_SIM$fire + 
            HH_ols$coef[8] * Res_SMZ_SIM$radiator
        
        ### Apply Emissions Adjustment to Projected New Stock
        Res_SMZ_SIM$CO2_EST_ADJ <- Res_SMZ_SIM$CO2_EST * CO2_HH_Rate_Adj * (1 - EFFICIENCYPERCENT)
        write.table(Res_SMZ_SIM[c("SMZ_N", "CO2_EST_ADJ")], file = (paste(TEMPATH, "RES_CO2_SAMPLE.csv", sep = "")), sep = ",", row.names = FALSE, 
            col.names = FALSE, append = TRUE)
    }
    
    replicate(n = 10000, expr = sampHH())
    
    Res_SMZ_SIM_RESULT <- read.csv(paste(TEMPATH, "RES_CO2_SAMPLE.csv", sep = ""))  #Read back in simulated housing stock
    names(Res_SMZ_SIM_RESULT) <- c("SMZ_N", "CO2_EST_ADJ")
    
    #### Aggregate CO2 data in simulated housing stock By SMZ
    Res_SMZ_SIM_RESULT_AGG <- aggregate(cbind(Res_SMZ_SIM_RESULT$CO2_EST_ADJ) ~ Res_SMZ_SIM_RESULT$SMZ_N, data = Res_SMZ_SIM_RESULT, mean, na.rm = FALSE)
    names(Res_SMZ_SIM_RESULT_AGG)[1] <- "SMZ_N"
    names(Res_SMZ_SIM_RESULT_AGG)[2] <- "CO2_EST_ADJ"
    
    # Res_SMZ_SIM_RESULT_AGG_MAX <-aggregate(cbind(Res_SMZ_SIM_RESULT2$CO2_EST_ADJ)~Res_SMZ_SIM_RESULT2$SMZ_N, data=Res_SMZ_SIM_RESULT2, max,
    # na.rm=TRUE) names(Res_SMZ_SIM_RESULT_AGG_MAX)[1] <- 'SMZ_N' names(Res_SMZ_SIM_RESULT_AGG_MAX)[2] <- 'CO2_EST_ADJ_MAX'
    
    # Res_SMZ_SIM_RESULT_AGG_MIN <-aggregate(cbind(Res_SMZ_SI#M_RESULT2$CO2_EST_ADJ)~Res_SMZ_SIM_RESULT2$SMZ_N, data=Res_SMZ_SIM_RESULT2, min,
    # na.rm=TRUE) names(Res_SMZ_SIM_RESULT_AGG_MIN)[1] <- 'SMZ_N' names(Res_SMZ_SIM_RESULT_AGG_MIN)[2] <- 'CO2_EST_ADJ_MIN'
    
    ### Add MAX and MIN Emissions to Average Res_SMZ_SIM_RESULT_AGG$CO2_EST_ADJ_MAX =
    ### Res_SMZ_SIM_RESULT_AGG_MAX[match(Res_SMZ_SIM_RESULT_AGG$SMZ_N, Res_SMZ_SIM_RESULT_AGG_MAX$SMZ_N),'CO2_EST_ADJ_MAX']
    ### Res_SMZ_SIM_RESULT_AGG$CO2_EST_ADJ_MIN = Res_SMZ_SIM_RESULT_AGG_MIN[match(Res_SMZ_SIM_RESULT_AGG$SMZ_N,
    ### Res_SMZ_SIM_RESULT_AGG_MIN$SMZ_N),'CO2_EST_ADJ_MIN']
    
    
    ############ Age Base Housing Stock
    if (EFFICIENCYPERCENT > 0) {
        Res_Base2_AGED <- Res_Base2
        Res_Base2_AGED$UID <- 1:nrow(Res_Base2_AGED)  #create unique ID for each unit
        
        ## Apply Efficency upgrades
        Res_Base2_AGED_EFF <- subset(Res_Base2_AGED[sample(nrow(Res_Base2_AGED), sum(Res_Base2_AGED$HH) * Res_EFF1a), ])
        Res_Base2_AGED_NOEFF <- subset(Res_Base2_AGED[!(Res_Base2_AGED$UID %in% Res_Base2_AGED_EFF$UID), ])
        
        # Age non-upgraded housing
        Res_Base2_AGED_NOEFF$Age <- Res_Base2_AGED_NOEFF$Age + TIME
        
        # Apply efficiencies to upgraded housing stock
        Res_Base2_AGED_EFF$Age <- sample(0:TIME, 1)  #random age between 0 years and the time between potential upgrade date
        Res_Base2_AGED_EFF$Stucco <- 0
        Res_Base2_AGED_EFF$fire <- 0
        Res_Base2_AGED_EFF$radiator <- 0
        
    } else {
        Res_Base2_AGED <- Res_Base2
        Res_Base2_AGED$UID <- 1:nrow(Res_Base2_AGED)  #create unique ID for each unit
    }
    
    ##### Estimate Residential Emissions With added age constant
    Res_Base2_AGED$CO2_EST <- HH_ols$coef[1] + HH_ols$coef[2] * (Res_Base2$htdd * DDCCRESULT$HDD) + HH_ols$coef[3] * (Res_Base2$Age + TIME) + 
        HH_ols$coef[4] * Res_Base2$SF1 + HH_ols$coef[5] * Res_Base2$sqftstrc + HH_ols$coef[6] * Res_Base2$Stucco + HH_ols$coef[7] * Res_Base2$fire + 
        HH_ols$coef[8] * Res_Base2$radiator
    
    ## Count CO2 Emitting Households in the Aged housing stock
    Res_Base2_AGED$HH <- NA
    Res_Base2_AGED$HH[Res_Base2$CO2_EST > 0] <- 1
    Res_Base2_AGED$HH[Res_Base2$CO2_EST <= 0] <- 0
    
    #### Aggregate CO2 data in aged housing stock By SMZ
    Res_Base2_SMZ_AGED <- aggregate(cbind(Res_Base2_AGED$CO2_EST, Res_Base2_AGED$HH) ~ Res_Base2_AGED$SMZ_N, data = Res_Base2_AGED, sum, na.rm = TRUE)
    names(Res_Base2_SMZ_AGED)[1] <- "SMZ_N"
    names(Res_Base2_SMZ_AGED)[2] <- "CO2_EST"
    names(Res_Base2_SMZ_AGED)[3] <- "HH"
    
    ### Calculate New Rate Per HH for Aged Housing Stock
    Res_Base2_SMZ_AGED$CO2_Rate_HH <- Res_Base2_SMZ_AGED$CO2_EST/Res_Base2_SMZ_AGED$HH
    
    # Merge Rates and Activities for Aged Housing Stock
    Res_Base2_Activities_SMZ_AGED = merge(Res_Base2_SMZ_AGED, Base_SMZ_Activities, by = "SMZ_N")
    
    ### Apply New Rates to Aged Households
    Res_Base2_Activities_SMZ_AGED$CO2_base_HH <- Res_Base2_Activities_SMZ_AGED$CO2_Rate_HH * Res_Base2_Activities_SMZ_AGED$HH_BASE
    
    ### Replace Errors with Zero
    Res_Base2_Activities_SMZ_AGED[is.na(Res_Base2_Activities_SMZ_AGED)] <- 0
    Res_Base2_Activities_SMZ_AGED[Res_Base2_Activities_SMZ_AGED == -Inf] <- 0
    Res_Base2_Activities_SMZ_AGED[Res_Base2_Activities_SMZ_AGED == Inf] <- 0
    
    ### Adjusted CO2 Rate Per Aged Household
    Res_Base2_Activities_SMZ_AGED$CO2_Rate_HH_ADJ <- Res_Base2_Activities_SMZ_AGED$CO2_Rate_HH * CO2_HH_Rate_Adj
    
    ### Apply Adjusted Aged Rates to Activities
    Res_Base2_Activities_SMZ_AGED$CO2_base_HH_AGED <- Res_Base2_Activities_SMZ_AGED$CO2_Rate_HH_ADJ * Res_Base2_Activities_SMZ_AGED$HH_BASE
    
    ### Sum Total Aged Base Emissions
    Res_Base2_Activities_SMZ_AGED$CO2_base_HH_Total_AGED <- Res_Base2_Activities_SMZ_AGED$CO2_base_HH_AGED  #+ Res_Base2_Activities_SMZ$CO2_base_OFF + Res_Base2_Activities_SMZ$CO2_base_OTH
    
    ### Add Aged Base Emissions to Projected Emissions Growth
    Res_SMZ_SIM_RESULT_AGG$CO2_base_HH_AGED = Res_Base2_Activities_SMZ_AGED[match(Res_SMZ_SIM_RESULT_AGG$SMZ_N, Res_Base2_Activities_SMZ_AGED$SMZ_N), 
        "CO2_base_HH_AGED"]
    
    ### Add Base Emissions to Projected Emissions Growth
    Res_SMZ_SIM_RESULT_AGG$CO2_base_HH = Res_Base2_Activities_SMZ[match(Res_SMZ_SIM_RESULT_AGG$SMZ_N, Res_Base2_Activities_SMZ$SMZ_N), "CO2_base_HH"]
    
    Res_SMZ_SIM_RESULT_AGG$CO2_Proj_HH <- Res_SMZ_SIM_RESULT_AGG$CO2_base_HH_AGED + Res_SMZ_SIM_RESULT_AGG$CO2_EST_ADJ
    
    ### Clean Final Data Set
    Res_SMZ_SIM_RESULT_AGG[is.na(Res_SMZ_SIM_RESULT_AGG)] <- 0
    
    ### Write Final Res CO2 Output
    write.csv(Res_SMZ_SIM_RESULT_AGG[, c("SMZ_N", "CO2_base_HH", "CO2_Proj_HH")], file = (paste(OUTPATH, STATE, "_", "BEM_RES_CO2.csv", sep = "")), 
        row.names = FALSE)
    
    ### Take out the trash
    file.remove(paste(TEMPATH, "RES_CO2_SAMPLE.csv", sep = ""))
    
}
### END Other State Res CO2###

############## END Residential CO2 MODEL

############ Format Final CO2 Output
MD_Com_CO2 <- read.csv(paste(OUTPATH, "MD_BEM_Com_CO2.csv", sep = ""))  #Final Commercial CO2 File
VA_Com_CO2 <- read.csv(paste(OUTPATH, "VA_BEM_Com_CO2.csv", sep = ""))  #Final Commercial CO2 File
PA_Com_CO2 <- read.csv(paste(OUTPATH, "PA_BEM_Com_CO2.csv", sep = ""))  #Final Commercial CO2 File
WV_Com_CO2 <- read.csv(paste(OUTPATH, "WV_BEM_Com_CO2.csv", sep = ""))  #Final Commercial CO2 File
DC_Com_CO2 <- read.csv(paste(OUTPATH, "DC_BEM_Com_CO2.csv", sep = ""))  #Final Commercial CO2 File
DE_Com_CO2 <- read.csv(paste(OUTPATH, "DE_BEM_Com_CO2.csv", sep = ""))  #Final Commercial CO2 File

ALL_Com_CO2 <- rbind(MD_Com_CO2, VA_Com_CO2, PA_Com_CO2, WV_Com_CO2, DC_Com_CO2, DE_Com_CO2)

MD_Res_CO2 <- read.csv(paste(OUTPATH, "MD_BEM_Res_CO2.csv", sep = ""))  #Final Resmercial CO2 File
VA_Res_CO2 <- read.csv(paste(OUTPATH, "VA_BEM_Res_CO2.csv", sep = ""))  #Final Resmercial CO2 File
PA_Res_CO2 <- read.csv(paste(OUTPATH, "PA_BEM_Res_CO2.csv", sep = ""))  #Final Resmercial CO2 File
WV_Res_CO2 <- read.csv(paste(OUTPATH, "WV_BEM_Res_CO2.csv", sep = ""))  #Final Resmercial CO2 File
DC_Res_CO2 <- read.csv(paste(OUTPATH, "DC_BEM_Res_CO2.csv", sep = ""))  #Final Resmercial CO2 File
DE_Res_CO2 <- read.csv(paste(OUTPATH, "DE_BEM_Res_CO2.csv", sep = ""))  #Final Resmercial CO2 File

ALL_Res_CO2 <- rbind(MD_Res_CO2, VA_Res_CO2, PA_Res_CO2, WV_Res_CO2, DC_Res_CO2, DE_Res_CO2)

BlankSMZ[is.na(BlankSMZ)] <- 0

# Merge Final Data
BlankSMZ$COM_BASE_CO2 = ALL_Com_CO2[match(BlankSMZ$SMZ_N, ALL_Com_CO2$SMZ_N), "CO2_base_Com"]
BlankSMZ$RES_BASE_CO2 = ALL_Res_CO2[match(BlankSMZ$SMZ_N, ALL_Res_CO2$SMZ_N), "CO2_base_HH"]
BlankSMZ$COM_PROJ_CO2 = ALL_Com_CO2[match(BlankSMZ$SMZ_N, ALL_Com_CO2$SMZ_N), "CO2_Proj_Com"]
BlankSMZ$RES_PROJ_CO2 = ALL_Res_CO2[match(BlankSMZ$SMZ_N, ALL_Res_CO2$SMZ_N), "CO2_Proj_HH"]

# Clean-up
BlankSMZ[is.na(BlankSMZ)] <- 0
BlankSMZ[BlankSMZ < 0] <- 0

# Sum CO2 Results
BlankSMZ$BASE_TOTAL_CO2 <- BlankSMZ$COM_BASE_CO2 + BlankSMZ$RES_BASE_CO2
BlankSMZ$PROJ_TOTAL_CO2 <- BlankSMZ$COM_PROJ_CO2 + BlankSMZ$RES_PROJ_CO2

### Write Final CO2 Output
OUTHEADBASE <- paste(BASEYEAR, "_TOTCO2", sep = "")
OUTHEADPROJ <- paste(PROJYEAR, "_TOTCO2", sep = "")

if (Parameters$TempratureChange == 0 & Parameters$EfficienyPercent == 0) {
    write.table(BlankSMZ[, c("SMZ_N", "COM_BASE_CO2", "RES_BASE_CO2", "COM_PROJ_CO2", "RES_PROJ_CO2", "BASE_TOTAL_CO2", "PROJ_TOTAL_CO2")], 
        file = (paste(OUTPATH, sprintf("BEM_CO2_FINAL_%s", SCENARIO), sprintf("_%s", BASEYEAR), sprintf("-%s.csv", PROJYEAR), sep = "")), row.names = FALSE, 
        sep = ",")
} else if (Parameters$TempratureChange == 0 & !Parameters$EfficienyPercent == 0) {
    write.table(BlankSMZ[, c("SMZ_N", "COM_BASE_CO2", "RES_BASE_CO2", "COM_PROJ_CO2", "RES_PROJ_CO2", "BASE_TOTAL_CO2", "PROJ_TOTAL_CO2")], 
        file = (paste(OUTPATH, sprintf("BEM_CO2_FINAL_EFFICIENCY-UPGRADES_%s", SCENARIO), sprintf("_%s", BASEYEAR), sprintf("-%s.csv", PROJYEAR), 
            sep = "")), row.names = FALSE, sep = ",")
    
} else if (!Parameters$TempratureChange == 0 & !Parameters$EfficienyPercent == 0) {
    write.table(BlankSMZ[, c("SMZ_N", "COM_BASE_CO2", "RES_BASE_CO2", "COM_PROJ_CO2", "RES_PROJ_CO2", "BASE_TOTAL_CO2", "PROJ_TOTAL_CO2")], 
        file = (paste(OUTPATH, sprintf("BEM_CO2_FINAL_CLIMATE-CHANGE_EFFICIENCY-UPGRADES_%s", SCENARIO), sprintf("_%s", BASEYEAR), sprintf("-%s.csv", 
            PROJYEAR), sep = "")), row.names = FALSE, sep = ",")
    
} else {
    write.table(BlankSMZ[, c("SMZ_N", "COM_BASE_CO2", "RES_BASE_CO2", "COM_PROJ_CO2", "RES_PROJ_CO2", "BASE_TOTAL_CO2", "PROJ_TOTAL_CO2")], 
        file = (paste(OUTPATH, sprintf("BEM_CO2_FINAL_CLIMATE-CHANGE_%s", SCENARIO), sprintf("_%s", BASEYEAR), sprintf("-%s.csv", PROJYEAR), 
            sep = "")), row.names = FALSE, sep = ",")
    
}

###MODEL RUN COMPLETE!#### 
