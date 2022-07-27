#if you need to install censusapi

install.packages("devtools")
devtools::install_github("hrecht/censusapi")

#packages
library(censusapi)
library(readr)
library(tidyverse)

#census API key here

mycensuskey <- "your_census_api_key_here"

#what year?

myvintage <- 2020

#what can I get?

availableapis <- listCensusApis()

availablevars <- listCensusMetadata(name="acs/acs5", type="variables", vintage=2020)

possible_vars <- subset(availablevars,
                        grepl("income", availablevars$label,
                              ignore.case = TRUE)) #search availablevars for keywords

availablegeos <- listCensusMetadata(name="acs/acs5", type="g", vintage=2020)

###
#PULL number of uninsured by race from C27001 variables, where C27001H is white-not Hispanic, B is Black, C is Native American, D is Asian, G is two or more races,
#I is Hispanic. Pulling in TOTAL for each race and the three age categories of uninsured in each, summing them to get total uninsured, 
#and then calculating percent uninsured for each racial/background category. First for 2020, then for 2015 for comparison.
####

Insurance_Coverage_Race_2020 <- getCensus(name="acs/acs5",
                                          vintage=myvintage, key = mycensuskey,
                                          vars=c("C27001H_001E","C27001H_004E","C27001H_007E","C27001H_010E",
                                                 "C27001B_001E","C27001B_004E","C27001B_007E","C27001B_010E",
                                                 "C27001C_001E","C27001C_004E","C27001C_007E","C27001C_010E",
                                                 "C27001D_001E","C27001D_004E","C27001D_007E","C27001D_010E",
                                                 "C27001G_001E","C27001G_004E","C27001G_007E","C27001G_010E",
                                                 "C27001I_001E","C27001I_004E","C27001I_007E","C27001I_010E"),
                                          region="us") %>% 
  rename("Total_WH" = "C27001H_001E","UINS_U19_WH" = "C27001H_004E","UINS_19-64_WH" = "C27001H_007E","UINS_O64_WH" = "C27001H_010E",
         "Total_BL" = "C27001B_001E","UINS_U19_BL" = "C27001B_004E","UINS_19-64_BL" = "C27001B_007E","UINS_O64_BL" = "C27001B_010E",
         "Total_NA" = "C27001C_001E","UINS_U19_NA" = "C27001C_004E","UINS_19-64_NA" = "C27001C_007E","UINS_O64_NA" = "C27001C_010E",
         "Total_AS" = "C27001D_001E","UINS_U19_AS" = "C27001D_004E","UINS_19-64_AS" = "C27001D_007E","UINS_O64_AS" = "C27001D_010E",
         "Total_2R" = "C27001G_001E","UINS_U19_2R" = "C27001G_004E","UINS_19-64_2R" = "C27001G_007E","UINS_O64_2R" = "C27001G_010E",
         "Total_HS" = "C27001I_001E","UINS_U19_HS" = "C27001I_004E","UINS_19-64_HS" = "C27001I_007E","UINS_O64_HS" = "C27001I_010E") %>% 
  mutate(WH_UNINS = UINS_U19_WH+`UINS_19-64_WH`+UINS_O64_WH,
         BL_UNINS = UINS_U19_BL+`UINS_19-64_BL`+UINS_O64_BL, NA_UNINS = UINS_U19_NA+`UINS_19-64_NA`+UINS_O64_NA,
         AS_UNINS = UINS_U19_AS+`UINS_19-64_AS`+UINS_O64_AS, `2R_UNINS` = UINS_U19_2R +`UINS_19-64_2R`+UINS_O64_2R,
         HS_UNINS = UINS_U19_HS+`UINS_19-64_HS`+UINS_O64_HS) %>% 
  mutate(PCT_UNINS_WH = (WH_UNINS/Total_WH)*100,PCT_UNINS_BL = (BL_UNINS/Total_BL)*100,
         PCT_UNINS_NA = (NA_UNINS/Total_NA)*100, PCT_UNINS_AS = (AS_UNINS/Total_AS)*100, PCT_UNINS_2R = (`2R_UNINS`/Total_2R)*100,
         PCT_UNINS_HS = (HS_UNINS/Total_HS)*100)

Insurance_Coverage_Race_2015 <- getCensus(name="acs/acs5",
                                          vintage=2015, key = mycensuskey,
                                          vars=c("C27001H_001E","C27001H_004E","C27001H_007E","C27001H_010E",
                                                 "C27001B_001E","C27001B_004E","C27001B_007E","C27001B_010E",
                                                 "C27001C_001E","C27001C_004E","C27001C_007E","C27001C_010E",
                                                 "C27001D_001E","C27001D_004E","C27001D_007E","C27001D_010E",
                                                 "C27001G_001E","C27001G_004E","C27001G_007E","C27001G_010E",
                                                 "C27001I_001E","C27001I_004E","C27001I_007E","C27001I_010E"),
                                          region="us") %>% 
  rename("Total_WH" = "C27001H_001E","UINS_U19_WH" = "C27001H_004E","UINS_19-64_WH" = "C27001H_007E","UINS_O64_WH" = "C27001H_010E",
         "Total_BL" = "C27001B_001E","UINS_U19_BL" = "C27001B_004E","UINS_19-64_BL" = "C27001B_007E","UINS_O64_BL" = "C27001B_010E",
         "Total_NA" = "C27001C_001E","UINS_U19_NA" = "C27001C_004E","UINS_19-64_NA" = "C27001C_007E","UINS_O64_NA" = "C27001C_010E",
         "Total_AS" = "C27001D_001E","UINS_U19_AS" = "C27001D_004E","UINS_19-64_AS" = "C27001D_007E","UINS_O64_AS" = "C27001D_010E",
         "Total_2R" = "C27001G_001E","UINS_U19_2R" = "C27001G_004E","UINS_19-64_2R" = "C27001G_007E","UINS_O64_2R" = "C27001G_010E",
         "Total_HS" = "C27001I_001E","UINS_U19_HS" = "C27001I_004E","UINS_19-64_HS" = "C27001I_007E","UINS_O64_HS" = "C27001I_010E") %>% 
  mutate(WH_UNINS = UINS_U19_WH+`UINS_19-64_WH`+UINS_O64_WH,
         BL_UNINS = UINS_U19_BL+`UINS_19-64_BL`+UINS_O64_BL, NA_UNINS = UINS_U19_NA+`UINS_19-64_NA`+UINS_O64_NA,
         AS_UNINS = UINS_U19_AS+`UINS_19-64_AS`+UINS_O64_AS, `2R_UNINS` = UINS_U19_2R +`UINS_19-64_2R`+UINS_O64_2R,
         HS_UNINS = UINS_U19_HS+`UINS_19-64_HS`+UINS_O64_HS) %>% 
  mutate(PCT_UNINS_WH = (WH_UNINS/Total_WH)*100,PCT_UNINS_BL = (BL_UNINS/Total_BL)*100,
         PCT_UNINS_NA = (NA_UNINS/Total_NA)*100, PCT_UNINS_AS = (AS_UNINS/Total_AS)*100, PCT_UNINS_2R = (`2R_UNINS`/Total_2R)*100,
         PCT_UNINS_HS = (HS_UNINS/Total_HS)*100)

###
#Creating an output table comparing uninsured rates for 2020 and 2015
###

Uninsured_pct_race_2020 <- Insurance_Coverage_Race_2020 %>% select(PCT_UNINS_WH, PCT_UNINS_BL, PCT_UNINS_NA, PCT_UNINS_AS,
                                                                   PCT_UNINS_2R, PCT_UNINS_HS) %>% 
  pivot_longer(cols = c(PCT_UNINS_WH, PCT_UNINS_BL, PCT_UNINS_NA, PCT_UNINS_AS,
                        PCT_UNINS_2R, PCT_UNINS_HS),names_to = "Race", values_to = "Pct_unins_20")


Uninsured_pct_race_2015 <- Insurance_Coverage_Race_2015 %>% select(PCT_UNINS_WH, PCT_UNINS_BL, PCT_UNINS_NA, PCT_UNINS_AS,
                                                                   PCT_UNINS_2R, PCT_UNINS_HS) %>% 
  pivot_longer(cols = c(PCT_UNINS_WH, PCT_UNINS_BL, PCT_UNINS_NA, PCT_UNINS_AS,
                        PCT_UNINS_2R, PCT_UNINS_HS),names_to = "Race", values_to = "Pct_unins_15")

Uninsured_pct_race_20_15 <- left_join(Uninsured_pct_race_2020,Uninsured_pct_race_2015,by = "Race")

write_csv(Uninsured_pct_race_20_15,"Pct_Uininsured_Race_2020_2015.csv")

###
#Calculating the national uninsured rate from B27001 by grabbing the total population measured (_001E) and then adding up all the cuts of age and gender,
#that apply to no insurance, then calculating the percent with no insurance. This matches the 8.7 percent rate the Census shows on its website.
###

Ins_Coverage_2020 <- getCensus(name="acs/acs5",
                               vintage=myvintage, key = mycensuskey,
                               vars=c("B27001_001E","B27001_005E","B27001_008E","B27001_011E","B27001_014E","B27001_017E",
                                      "B27001_020E","B27001_023E","B27001_026E","B27001_029E","B27001_033E","B27001_036E",
                                      "B27001_039E","B27001_042E","B27001_045E","B27001_048E","B27001_051E","B27001_054E",
                                      "B27001_057E"),
                               region="us") %>% 
  mutate(Unins = B27001_005E + B27001_008E + B27001_011E+B27001_014E+B27001_017E+B27001_020E+B27001_023E+B27001_026E+B27001_029E+B27001_033E+B27001_036E
         + B27001_039E+B27001_042E+B27001_045E+B27001_048E+B27001_051E+B27001_054E+B27001_057E) %>% 
  mutate(Pct_Unins = (Unins/B27001_001E)*100)

Ins_Coverage_2015 <- getCensus(name="acs/acs5",
                               vintage=2015, key = mycensuskey,
                               vars=c("B27001_001E","B27001_005E","B27001_008E","B27001_011E","B27001_014E","B27001_017E",
                                      "B27001_020E","B27001_023E","B27001_026E","B27001_029E","B27001_033E","B27001_036E",
                                      "B27001_039E","B27001_042E","B27001_045E","B27001_048E","B27001_051E","B27001_054E",
                                      "B27001_057E"),
                               region="us") %>% 
  mutate(Unins = B27001_005E + B27001_008E + B27001_011E+B27001_014E+B27001_017E+B27001_020E+B27001_023E+B27001_026E+B27001_029E+B27001_033E+B27001_036E
         + B27001_039E+B27001_042E+B27001_045E+B27001_048E+B27001_051E+B27001_054E+B27001_057E) %>% 
  mutate(Pct_Unins = (Unins/B27001_001E)*100)

###
#Appending the national rates to the race breakdown df. I'm doing this manually to save time at this point
###

###
#Calculating National poverty rate changes by race between 2015 ACS5 and 2020 ACS5 Using B17001 and its variants
###

Poverty_2020 <- getCensus(name="acs/acs5",
                          vintage=myvintage, key = mycensuskey,
                          vars=c("NAME","B17001_001E","B17001_002E","B17001H_001E","B17001H_002E","B17001B_001E","B17001B_002E",
                                 "B17001C_001E","B17001C_002E","B17001D_001E","B17001D_002E","B17001G_001E","B17001G_002E","B17001I_001E",
                                 "B17001I_002E"),
                          region="us") %>% 
  rename("TOT" = "B17001_001E","TOT_POV" = "B17001_002E","TOT_WH" = "B17001H_001E","POV_WH" = "B17001H_002E","TOT_BL" = "B17001B_001E",
         "POV_BL" = "B17001B_002E","TOT_NA" = "B17001C_001E","POV_NA" = "B17001C_002E","TOT_AS" = "B17001D_001E","POV_AS" = "B17001D_002E",
         "TOT_2R" = "B17001G_001E","POV_2R" = "B17001G_002E","TOT_HS" = "B17001I_001E","POV_HS" = "B17001I_002E") %>% 
  mutate(pct_pov = (TOT_POV/TOT)*100, pct_pov_WH = (POV_WH/TOT_WH)*100,pct_pov_BL = (POV_BL/TOT_BL)*100,pct_pov_NA = (POV_NA/TOT_NA)*100,
         pct_pov_AS = (POV_AS/TOT_AS)*100, pct_pov_2R = (POV_2R/TOT_2R)*100,pct_pov_HS = (POV_HS/TOT_HS)*100)

Poverty_2015 <- getCensus(name="acs/acs5",
                          vintage=2015, key = mycensuskey,
                          vars=c("NAME","B17001_001E","B17001_002E","B17001H_001E","B17001H_002E","B17001B_001E","B17001B_002E",
                                 "B17001C_001E","B17001C_002E","B17001D_001E","B17001D_002E","B17001G_001E","B17001G_002E","B17001I_001E",
                                 "B17001I_002E"),
                          region="us") %>% 
  rename("TOT" = "B17001_001E","TOT_POV" = "B17001_002E","TOT_WH" = "B17001H_001E","POV_WH" = "B17001H_002E","TOT_BL" = "B17001B_001E",
         "POV_BL" = "B17001B_002E","TOT_NA" = "B17001C_001E","POV_NA" = "B17001C_002E","TOT_AS" = "B17001D_001E","POV_AS" = "B17001D_002E",
         "TOT_2R" = "B17001G_001E","POV_2R" = "B17001G_002E","TOT_HS" = "B17001I_001E","POV_HS" = "B17001I_002E") %>% 
  mutate(pct_pov = (TOT_POV/TOT)*100, pct_pov_WH = (POV_WH/TOT_WH)*100,pct_pov_BL = (POV_BL/TOT_BL)*100,pct_pov_NA = (POV_NA/TOT_NA)*100,
         pct_pov_AS = (POV_AS/TOT_AS)*100, pct_pov_2R = (POV_2R/TOT_2R)*100,pct_pov_HS = (POV_HS/TOT_HS)*100)

Poverty_pct_race_2020 <- Poverty_2020 %>% select(pct_pov,pct_pov_WH,pct_pov_BL,pct_pov_NA,pct_pov_AS,pct_pov_2R,pct_pov_HS) %>% 
  pivot_longer(cols = c(pct_pov,pct_pov_WH,pct_pov_BL,pct_pov_NA,pct_pov_AS,pct_pov_2R,pct_pov_HS),names_to = "Race", values_to = "Pct_pov_20")


Poverty_pct_race_2015 <- Poverty_2015 %>% select(pct_pov,pct_pov_WH,pct_pov_BL,pct_pov_NA,pct_pov_AS,pct_pov_2R,pct_pov_HS) %>% 
  pivot_longer(cols = c(pct_pov,pct_pov_WH,pct_pov_BL,pct_pov_NA,pct_pov_AS,pct_pov_2R,pct_pov_HS),names_to = "Race", values_to = "Pct_pov_15")

Poverty_pct_race_20_15 <- left_join(Poverty_pct_race_2020,Poverty_pct_race_2015,by = "Race")

write_csv(Poverty_pct_race_20_15,"Pct_Poverty_Race_2020_2015.csv")

###
#Calculating national MHI changes by race 2020 and 2015 from B19013(H,B,C,D,G,I)
###

MHI_2020 <- getCensus(name="acs/acs5",
                      vintage=myvintage, key = mycensuskey,
                      vars=c("B19013_001E","B19013H_001E","B19013B_001E","B19013C_001E","B19013D_001E",
                             "B19013G_001E","B19013I_001E"),
                      region="us") %>% 
  rename("MHI" = "B19013_001E","MHI_WH" = "B19013H_001E","MHI_BL" = "B19013B_001E","MHI_NA" = "B19013C_001E","MHI_AS" = "B19013D_001E",
         "MHI_2R" = "B19013G_001E","MHI_HS" = "B19013I_001E") 

MHI_2015 <- getCensus(name="acs/acs5",
                      vintage=2015, key = mycensuskey,
                      vars=c("B19013_001E","B19013H_001E","B19013B_001E","B19013C_001E","B19013D_001E",
                             "B19013G_001E","B19013I_001E"),
                      region="us") %>% 
  rename("MHI" = "B19013_001E","MHI_WH" = "B19013H_001E","MHI_BL" = "B19013B_001E","MHI_NA" = "B19013C_001E","MHI_AS" = "B19013D_001E",
         "MHI_2R" = "B19013G_001E","MHI_HS" = "B19013I_001E")

MHI_race_2020 <- MHI_2020 %>% pivot_longer(cols = c(MHI,MHI_WH,MHI_BL,MHI_NA,MHI_AS,MHI_2R,MHI_HS),names_to = "Race", values_to = "MHI_20")


MHI_race_2015 <- MHI_2015 %>% pivot_longer(cols = c(MHI,MHI_WH,MHI_BL,MHI_NA,MHI_AS,MHI_2R,MHI_HS),names_to = "Race", values_to = "MHI_15")

MHI_race_20_15 <- left_join(MHI_race_2020,MHI_race_2015,by = c("Race","us"))

#Output to CSV

write_csv(MHI_race_20_15,"MHI_Race_2020_2015.csv")

###
#Pulling state-level MHI changes by race 2020 and 2015 from B19013(H,B,C,D,G,I)
###

MHI_states_2020 <- getCensus(name="acs/acs5",
                             vintage=myvintage, key = mycensuskey,
                             vars=c("NAME","B19013_001E","B19013H_001E","B19013B_001E","B19013C_001E","B19013D_001E",
                                    "B19013G_001E","B19013I_001E"),
                             region="state") %>% 
  rename("MHI" = "B19013_001E","MHI_WH" = "B19013H_001E","MHI_BL" = "B19013B_001E","MHI_NA" = "B19013C_001E","MHI_AS" = "B19013D_001E",
         "MHI_2R" = "B19013G_001E","MHI_HS" = "B19013I_001E") 

MHI_states_2015 <- getCensus(name="acs/acs5",
                             vintage=2015, key = mycensuskey,
                             vars=c("NAME","B19013_001E","B19013H_001E","B19013B_001E","B19013C_001E","B19013D_001E",
                                    "B19013G_001E","B19013I_001E"),
                             region="state") %>% 
  rename("MHI" = "B19013_001E","MHI_WH" = "B19013H_001E","MHI_BL" = "B19013B_001E","MHI_NA" = "B19013C_001E","MHI_AS" = "B19013D_001E",
         "MHI_2R" = "B19013G_001E","MHI_HS" = "B19013I_001E")

MHI_state_race_2020 <- MHI_states_2020 %>% pivot_longer(cols = c(MHI,MHI_WH,MHI_BL,MHI_NA,MHI_AS,MHI_2R,MHI_HS),names_to = "Race", values_to = "MHI_20")


MHI_state_race_2015 <- MHI_states_2015 %>% pivot_longer(cols = c(MHI,MHI_WH,MHI_BL,MHI_NA,MHI_AS,MHI_2R,MHI_HS),names_to = "Race", values_to = "MHI_15")

MHI_state_race_20_15 <- left_join(MHI_state_race_2020,MHI_state_race_2015,by = c("Race","NAME","state")) %>% 
  arrange(NAME)

MHI_state_race_20_15_wide <- MHI_state_race_20_15 %>% pivot_wider(names_from = Race, values_from = c(MHI_20,MHI_15)) %>% 
  mutate(pct_change = ((MHI_20_MHI-MHI_15_MHI)/MHI_15_MHI)*100) %>% 
  dplyr::arrange(-MHI_20_MHI)

#Output to csv

write_csv(MHI_state_race_20_15,"MHI_state_Race_2020_2015.csv")

write_csv(MHI_state_race_20_15_wide,"MHI_state_Race_2020_2015_wide.csv")
