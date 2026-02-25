##DATA CLEANING
#importing the CSVS
FDNY <- read.csv("Incidents_Responded_to_by_Fire_Companies_20260225.csv")
EMS <- read.csv("EMS_Incident_Dispatch_Data_20260225.csv")
NYPD1 <- read.csv("NYPD_Calls_for_Service_(Historic)_20260225.csv")
NYPD2 <- read.csv("NYPD_Calls_for_Service_(Year_to_Date)_20260225.csv")


#could the issue with date be the way I'm looking at it
class(EMS$INCIDENT_DATETIME)
library(dplyr)

EMS <- EMS %>%
  mutate(
    INCIDENT_DATETIME = as.POSIXct(
      INCIDENT_DATETIME,
      format = "%m/%d/%Y %I:%M:%S %p",
      tz = "UTC"
    ),
    INCIDENT_DATE = as.Date(INCIDENT_DATETIME)
  )
class(EMS$INCIDENT_DATE)
#IT IS THE CORRECT TIME FRAME FOR EMS!!


class(FDNY$INCIDENT_DATE_TIME)

FDNY <- FDNY %>%
  mutate(
    INCIDENT_DATE_TIME = as.POSIXct(
      INCIDENT_DATE_TIME,
      format = "%m/%d/%Y %I:%M:%S %p",
      tz = "UTC"
    ),
    INCIDENT_DATE = as.Date(INCIDENT_DATE_TIME)
  )
#CORRECT TIME FRAME AS WELL

class(NYPD1$INCIDENT_DATE)
NYPD1$INCIDENT_DATE <- as.Date(NYPD1$INCIDENT_DATE,
                              format = "%m/%d/%Y")

class(NYPD1$INCIDENT_DATE)


class(NYPD2$INCIDENT_DATE)
NYPD2$INCIDENT_DATE <- as.Date(NYPD2$INCIDENT_DATE,
                               format = "%m/%d/%Y")

class(NYPD2$INCIDENT_DATE)

colnames(NYPD1)
colnames(NYPD2)

NYPD <- bind_rows(NYPD1, NYPD2)




#create new columns for identifying agency and elapsed time between dispatch and arrival (ie response time)
#EMS
EMS <- EMS %>%
  mutate(AGENCY = "EMS")

library(hms)
EMS <- EMS %>%
  mutate(INCIDENT_TIME = as_hms(INCIDENT_DATETIME))
class(EMS$INCIDENT_TIME)

EMS <- EMS %>%
  mutate(
    FIRST_ON_SCENE_DATETIME = as.POSIXct(
      FIRST_ON_SCENE_DATETIME,
      format = "%m/%d/%Y %I:%M:%S %p",
      tz = "UTC"
    ),
    FIRST_ON_SCENE_DATE = as.Date(FIRST_ON_SCENE_DATETIME)
  )
class(EMS$FIRST_ON_SCENE_DATE)

EMS <- EMS %>%
  mutate(FIRST_ON_SCENE_TIME = as_hms(FIRST_ON_SCENE_DATETIME))
class(EMS$FIRST_ON_SCENE_TIME)


EMS <- EMS %>%
  mutate(
    INCIDENT_SEC = as.numeric(INCIDENT_TIME),
    FIRST_ON_SCENE_SEC = as.numeric(FIRST_ON_SCENE_TIME),
    RESPONSE_TIME = FIRST_ON_SCENE_SEC - INCIDENT_SEC
  )

EMS <- EMS %>%
  mutate(
    RESPONSE_TIME = ifelse(RESPONSE_TIME < 0,
                           RESPONSE_TIME + 86400,
                           RESPONSE_TIME)
  )

EMS$RESPONSE_TIME <- ifelse(EMS$RESPONSE_TIME == 0, NA, EMS$RESPONSE_TIME)
sum(!is.na(EMS$RESPONSE_TIME))

summary(EMS$RESPONSE_TIME)

#FDNY
FDNY <- FDNY %>%
  mutate(AGENCY = "FDNY")

FDNY <- FDNY %>%
  mutate(INCIDENT_TIME = as_hms(INCIDENT_DATE_TIME))
class(EMS$INCIDENT_TIME)

FDNY <- FDNY %>%
  mutate(
    ARRIVAL_DATE_TIME = as.POSIXct(
      ARRIVAL_DATE_TIME,
      format = "%m/%d/%Y %I:%M:%S %p",
      tz = "UTC"
    ),
    ARRIVAL_DATE = as.Date(ARRIVAL_DATE_TIME)
  )
class(FDNY$ARRIVAL_DATE)

FDNY <- FDNY %>%
  mutate(ARRIVAL_TIME = as_hms(ARRIVAL_DATE_TIME))
class(FDNY$ARRIVAL_TIME)


FDNY <- FDNY %>%
  mutate(
    INCIDENT_SEC = as.numeric(INCIDENT_TIME),
    ARRIVAL_SEC = as.numeric(ARRIVAL_TIME),
    RESPONSE_TIME = ARRIVAL_SEC - INCIDENT_SEC
  )

FDNY <- FDNY %>%
  mutate(
    RESPONSE_TIME = ifelse(RESPONSE_TIME < 0,
                           RESPONSE_TIME + 86400,
                           RESPONSE_TIME)
  )

FDNY$RESPONSE_TIME <- ifelse(FDNY$RESPONSE_TIME == 0, NA, FDNY$RESPONSE_TIME)
sum(!is.na(FDNY$RESPONSE_TIME))

summary(FDNY$RESPONSE_TIME)

#NYPD
NYPD <- NYPD %>%
  mutate(AGENCY = "NYPD")

class(NYPD$INCIDENT_TIME)
NYPD <- NYPD %>%
  mutate(INCIDENT_TIME = as_hms(INCIDENT_TIME))
class(NYPD$INCIDENT_TIME)

head(NYPD$DISP_TS, 20)
head(NYPD$ARRIVD_TS, 20)

NYPD <- NYPD %>%
  mutate(
    DISP_TS = na_if(DISP_TS, ""),
    ARRIVD_TS = na_if(ARRIVD_TS, ""),
    DISP_TIME = as.POSIXct(DISP_TS, format = "%Y %b %d %I:%M:%S %p"),
    ARRIVAL_TIME = as.POSIXct(ARRIVD_TS, format = "%Y %b %d %I:%M:%S %p"),
    RESPONSE_TIME = as.numeric(difftime(ARRIVAL_TIME, DISP_TIME, units = "secs"))
  )

NYPD <- NYPD %>%
  mutate(
    RESPONSE_TIME = ifelse(RESPONSE_TIME < 0,
                           RESPONSE_TIME + 86400,
                           RESPONSE_TIME)
  )

NYPD$RESPONSE_TIME <- ifelse(NYPD$RESPONSE_TIME == 0, NA, NYPD$RESPONSE_TIME)
sum(!is.na(NYPD$RESPONSE_TIME))

summary(NYPD$RESPONSE_TIME)



#ok now start elminating things that I don't need 
EMSclean <- EMS %>%
  filter(!is.na(RESPONSE_TIME) & RESPONSE_TIME != 0 & RESPONSE_TIME != "")
FDNYclean <- FDNY %>%
  filter(!is.na(RESPONSE_TIME) & RESPONSE_TIME != 0 & RESPONSE_TIME != "")
NYPDclean <- NYPD %>%
  filter(!is.na(RESPONSE_TIME) & RESPONSE_TIME != 0)

EMSclean <- EMSclean %>%
  rename(INCIDENT_ID = CAD_INCIDENT_ID)

FDNYclean <- FDNYclean %>%
  rename(INCIDENT_ID = IM_INCIDENT_KEY,
         BOROUGH = BOROUGH_DESC,
         ZIPCODE = ZIP_CODE)

NYPDclean <- NYPDclean %>%
  rename(INCIDENT_ID = CAD_EVNT_ID,
         BOROUGH = BORO_NM)

#add zipcode to those that don't have it
library(sf)
ZIP <- st_read("modzcta.shp")
ZIP_small <- ZIP["modzcta"]

EMSclean$ZIPCODE <- as.character(EMSclean$ZIPCODE)
ZIP$modzcta <- as.character(ZIP$modzcta)

EMSzip <- left_join(
  EMSclean,
  ZIP[, c("modzcta", "geometry")],
  by = c("ZIPCODE" = "modzcta")
)

EMSzip <- st_as_sf(EMSzip)


FDNYclean$ZIPCODE <- as.character(FDNYclean$ZIPCODE)
ZIP$modzcta <- as.character(ZIP$modzcta)

FDNYzip <- left_join(
  FDNYclean,
  ZIP[, c("modzcta", "geometry")],
  by = c("ZIPCODE" = "modzcta")
)

FDNYzip <- st_as_sf(FDNYzip)


NYPDzip <- st_as_sf(
  NYPDclean,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)


NYPDzip <- st_join(NYPDzip, ZIP_small, left = TRUE)

names(NYPDzip)[names(NYPDzip) == "modzcta"] <- "ZIPCODE"


#writing csv files for the big clean datasets with only the data I think is relevant
library(data.table)
EMSclean <-EMSclean %>%
  select(AGENCY, ZIPCODE, BOROUGH, INCIDENT_ID, INCIDENT_DATE, RESPONSE_TIME, INITIAL_SEVERITY_LEVEL_CODE, FINAL_SEVERITY_LEVEL_CODE, INCIDENT_DISPATCH_AREA, POLICEPRECINCT, CITYCOUNCILDISTRICT, COMMUNITYDISTRICT, COMMUNITYSCHOOLDISTRICT, SPECIAL_EVENT_INDICATOR)

fwrite(EMSclean, "EMSclean.csv")

FDNYclean <- FDNYclean %>%
  select(AGENCY, ZIPCODE, BOROUGH, INCIDENT_ID, INCIDENT_DATE, RESPONSE_TIME, HIGHEST_LEVEL_DESC, INCIDENT_TYPE_DESC)

fwrite(FDNYclean, "FDNYclean.csv")

NYPDclean <- NYPDzip %>%
  select(AGENCY, ZIPCODE, BOROUGH, INCIDENT_ID, INCIDENT_DATE, RESPONSE_TIME, NYPD_PCT_CD, RADIO_CODE)
NYPDclean <- st_drop_geometry(NYPDclean)

fwrite(NYPDclean, "NYPDclean.csv")

#making one dataset
EMSclean2 <- EMSclean %>%
  select(AGENCY, ZIPCODE, BOROUGH, INCIDENT_ID, INCIDENT_DATE, RESPONSE_TIME)

FDNYclean2 <- FDNYclean %>%
  select(AGENCY, ZIPCODE, BOROUGH, INCIDENT_ID, INCIDENT_DATE, RESPONSE_TIME)
class(FDNYclean2$ZIPCODE)

NYPDclean2 <- NYPDclean %>%
  select(AGENCY, ZIPCODE, BOROUGH, INCIDENT_ID, INCIDENT_DATE, RESPONSE_TIME)

#combine
Mapstone <- bind_rows(EMSclean2, FDNYclean2, NYPDclean2)

#make one for inside the CRZ
MapstoneCRZ <- Mapstone %>%
  filter(ZIPCODE %in% c(
    "10004","10005","10006","10280","10282","10007",
    "10038","10013","10002","10014","10012","10003",
    "10009","10011","10010","10001","10016","10018",
    "10017","10036","10022","10019"
  ))

