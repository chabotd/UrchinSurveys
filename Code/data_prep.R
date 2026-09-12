library(tidyverse)
library(dplyr)
library(lubridate)
################################################################################
#Read in Surveys dataset
################################################################################
urch <- read.csv("Data/WorkingUrchinSurveyData.csv")

# for later to fix recruit issue

# library(writexl)
# write_xlsx(urch, "Data/urch_updated.xlsx")

##########Cleaning Workflows###################################################

#check to see things are numeric or characters. 
str(urch)
View(urch)

# Date format 
urch <- urch %>%
  mutate(Date = mdy(Date))

#rename zone to subhabitat

urch<- urch %>%
  rename(Subhabitat = Zone)

#drop plot notsurveyed 

urch <- urch %>%
  filter(Call_Number!= "SB_2024_UPZ_2_4")

#fix count of crevice urch here

urch <- urch %>%
  mutate(
    CreviceUrchins = ifelse(
      Call_Number == "SC_2026_NPZ_2_5",
      17,
      CreviceUrchins
    )
  )

#code to make something as.numeric
urch$AvailableBareRock <-as.numeric(urch$AvailableBareRock)
# make all -bare rocks zero
urch$AvailableBareRock[urch$AvailableBareRock < 0] <- 0

#drop things if not needed
urch <- urch %>%
 select(-Mastocarpus, -Pyropia, -OtherRed, -UnknownRecruits, -Diatom, 
        -Chondracanthus, -Erhythophyllum)

#remove FC NPZ since it is not actually NPZ. 
urch <- urch %>%
  filter(!(SiteCode == "FC" & Subhabitat == "NPZ"))

#code get rid of rows below if needed (data clean, so ok)
#urch <- urch %>%
#  dplyr::slice(1:715)

# get rid of rows with NAs for now (will go back and photo-ID) 
# #** these are missing, so can probably delete
# urch <- urch %>%
#   filter(!Call_Number %in% c(
#     "SB_2026_UPZ_2_4",
# #    "CB_2026_UPZ_2_4",
# #    "CB_2026_UPZ_2_5"
#   ))



#drop 100% cover column 
urch<- urch%>% 
  select(-TotalPrimaryCover)

# parce out substrate columns 

urch <- urch %>%
  mutate(
    OldSubstrateType = na_if(OldSubstrateType, ""),
    
    New_Rugosity = case_when(
      OldSubstrateType == "VD" ~ "Varied",
      OldSubstrateType == "VB" ~ "Varied",
      OldSubstrateType == "FB" ~ "Flat",
      OldSubstrateType == "FD" ~ "Flat",
      OldSubstrateType == "CC" ~ "Flat",
      OldSubstrateType == "CB" ~ "Flat",
      OldSubstrateType == "TP" ~ "Varied",
      OldSubstrateType == "CD" ~ "Varied",
      OldSubstrateType == "WB"  ~ "Wall",
      OldSubstrateType == "WD"  ~ "Wall",
      OldSubstrateType == "DD"  ~ "Deep",
      TRUE ~ NA_character_
    ),
    
    New_Substrate = case_when(
      OldSubstrateType == "VD" ~ "Boulder",
      OldSubstrateType == "VB" ~ "Bench",
      OldSubstrateType == "FB" ~ "Bench",
      OldSubstrateType == "FD" ~ "Boulder",
      OldSubstrateType == "CC" ~ "Cobbles",
      OldSubstrateType == "CB" ~ "Cobbles",
      OldSubstrateType == "TP" ~ "Tidepool",
      OldSubstrateType == "CD" ~ "Cobbles",
      OldSubstrateType == "WB"  ~ "Bench",
      OldSubstrateType == "DD"  ~ "Boulder",
      OldSubstrateType == "WD"  ~ "Boulder",
      TRUE ~ NA_character_
    )
  )




# Reorder the levels of sites 
urch$SiteCode <- factor(urch$SiteCode, levels = c("BB", "FC", "YB", "SH", "SB", 
                                                  "SC", "CB","RP", "WC", "CP", 
                                                  "CMN", "CMS"))

urch <- urch %>% mutate(MeanTest = rowMeans(urch[, c("UrchinSize1", 
                                                     "UrchinSize2", 
                                                     "UrchinSize3", 
                                                     "UrchinSize4", 
                                                     "UrchinSize5")]))

#add TOTAL number urchins (Juvs + Adults)

urch <- urch %>% mutate(TotalUrchins = TotalAdultUrchins + JuvenileUrchins)

##if not using Cali data use this.
#urch<- urch %>%
#  filter(!(SiteCode %in% c("CMS", "CMN")))

# add column for TotalPits and Ratio of Empty:Full
# urch <- urch %>% mutate(RatioPits = EmptyPits/PitsPresent)
# urch <- urch %>% mutate(TotalPits = EmptyPits + PittedUrchins)

# add column for percent occupancy 
urch <- urch %>%
  mutate(
    PercentOccupancy = (PittedUrchins / PitsPresent) * 100
  )

#susceptibility 
urch$Connectivity <- with(urch, case_when(
  SiteCode %in% c("SC", "CB", "WC", "CP", "RP") ~ "Susceptible",
  SiteCode %in% c("BB", "SB", "FC") ~ "Unlikely",
  SiteCode %in% c("YB", "SH") ~ "Extremely Unlikely",
  TRUE ~ NA_character_  # for any sites not listed
))

# percent pitted
urch$PercentPitted <- (urch$PittedUrchins/ urch$TotalAdultUrchins) * 100

# add nonpits together (IF ONLY LOOKING AT PITTED)
urch<- urch %>% mutate(NonPit = OpenUrchins + CreviceUrchins)

urch$PercentNonPitted <- (urch$NonPit/ urch$TotalAdultUrchins) * 100

# add cryptic urchins together (IF ONLY LOOKING AT OPEN)
urch <- urch %>% mutate(Cryptic = PittedUrchins + CreviceUrchins)

urch$PercentCryptic <- (urch$Cryptic/ urch$TotalUrchins) * 100

#urchin cover
urch <- urch %>%
  mutate(UrchinCover = if_else(TotalAdultUrchins > 0 & UrchinCover == 0,
                               NA_real_,
                               UrchinCover))

# Drift kelp 
# need to take total drift and divide by urchin to get an estimate of % cover 
# on urchins 

urch <- urch %>% mutate(DriftPerUrchin= 
                          urch$TotalAttachedDrift/urch$TotalAdultUrchins)
####### just know that Crevice count twice 
#& CANNOT compare Cryptic to NonPit; only Open! 

#re order all columns
urch <- urch %>%
  relocate(New_Rugosity, New_Substrate, .after = OldSubstrateType)
urch <- urch %>%
  relocate(PercentOccupancy, PercentNonPitted, 
           .after = PitsPresent)
urch <- urch %>%
  relocate(MeanTest, .after = UrchinSize5)
urch <- urch %>%
  relocate(Connectivity, .after = Cape)
urch <- urch %>%
  relocate(NonPit, Cryptic, .after = OpenUrchins)
urch <- urch %>%
  relocate(PercentPitted, PercentNonPitted, PercentCryptic, .after = RedUrchins)
urch <- urch %>%
  relocate(TotalUrchins, .after = New_Substrate)
urch <- urch %>%
  relocate(TotalUrchins, .after = New_Substrate)

write.csv(urch, "Data/urch_clean.csv", row.names = FALSE)
