#loading Packages ----
library(haven)
library(tidyverse)
#library(purrr)
library(dplyr)
library(gtsummary)
library(survey)
pacman::p_load(
  naniar,        # assess and visualize missingness
  mice           # missing data imputation
)
#Loading Data ----
disSEDA_2019 <- read_sas("ff_dis_seda_2019_pub.sas7bdat")
gva15y <- read_sas("ff_gva_15y_pub1_20190426.sas7bdat")
OpinCim9y <- read_sas("ff_opin_cim_b9y_pub1.sas7bdat")
UCR <- read_sas("ff_UCR_pub1.sas7bdat")
CCity6 <- read_sas("contractcity6pub.sas7bdat")
Wave1_yr0 <- read_sas("FF_wave1_2020v2_SAS.sas7bdat") 
Wave4_yr <- read_sas("FF_wave4_2020v2_SAS.sas7bdat") 
Wave5_yr9 <- read_sas("FF_wave5_2020v2_SAS.sas7bdat") 
Wave6_yr15 <- read_sas("FF_wave6_2020v2_SAS.sas7bdat") 

#Final Data Cleaning ----
options(max.print=1000000)
Wave1VarInt <- Wave1_yr0 %>%
  select(idnum,cm1bsex)

Wave6VarInt <- Wave6_yr15 %>%
  select(idnum,ck6ethrace,cp6intyr,cp6yagey,cp6conf1,p6e1,p6e10,cp6povca,p6k6,cp6edu,
         p6b22,p6b23,p6b24,p6b31,p6b32,p6b34,p6b30)

Wave4_5 <- left_join(Wave4_yr, Wave5_yr9, by = "idnum")
Wave4_5 <- Wave4_5 %>%
  select(idnum,cf4md_case_con,cf5md_case_con,cm4md_case_con,cm5md_case_con,
         f5g2b1_101,f5g2b1d,f5g2b1e,f5g32,f5g33,m5g2b1d,m5g2b1e,n5f2b1d,
         n5f2b1e,f4c39,f4e10,f4j20a,f4j21,f5g19a,f5g20,f5g21a,f5g21b,f5g21c,f5g21d,
         f5g21e,f5g21f,f5g21g,f5g21h,f5g21i,f5g22,m4c39,m4e10,m4j20a,m4j21,
         m5g19a,m5g20,m5g21a,m5g21b,m5g21c,m5g21d,m5g21e,m5g21f,m5g21g,m5g21h,
         m5g21i,m5g22k,p4g16,p4g24b,p4g15,p4g17,p4g18,p4g19,p4g24a,p4g24c,
         p4g24d,p4g24e,f5k10f,f5k10h,f5k10i,f5k10j,f5k10n,m5k10f,m5k10h,m5k10i,
         m5k10j,m5k10n,p4g10,p4g14,p4g23f,p4g23h,p4g23i,p4g23j,p4g23n,p4g6,p4g8,p4g9,
         p5i33a,p5i33c,p5i42a,p5i42c,f5k10c,f5k10d,f5k10g,f5k10k,f5k10m,m4b30,m4b5,
         m4c4,m5k10c,m5k10d,m5k10g,m5k10k,m5k10m,p4g11,p4g13,p4g23c,p4g23d,p4g23g,
         p4g23k,p4g23m,p4g3,p4g4,p4g7,p5i33b,p5i42b,f4d10,f4d10a,f4d7h,f4d7i,
         f4d7j,f4d7m,f4d7n,f4d9h,f4d9i,f4d9j,f4d9m,f4d9n,f5c6h,f5c6i,f5c6j,f5c6m,
         f5c6n,f5c7,f5c8a,f5d21,f5d22,m4d10,m4d10a,m4d7h,m4d7i,m4d7j,m4d7m,m4d7n,
         m4d9h,m4d9i,m4d9j,m4d9m,m4d9n,m4e23h,m4e23i,m4e23j,m4e23m,m4e23n,m4e23q,
         m4e24,m5c6h,m5c6i,m5c6j,m5c6m,m5c6n,m5c7,m5c8a,m5c8e,m5d20h,m5d20i,m5d20j,
         m5d20m,m5d20n,m5d21,m5d22,m5d22d,m5a4,f5a4,m4a4,f4a4,m5b30,m5d7,f5b24x,
         m4c37,m4e7,f4c37,f4e7,k5citywt:k5citywt_rep72,p5citywt:p5citywt_rep72)



#f5g23a,f5g2a,m5g2a,p4j5



CombinedData <- left_join(Wave1VarInt,Wave6VarInt, by = "idnum")
CombinedData1 <- left_join(CombinedData,Wave4_5, by = "idnum")

CombinedData2 <- CombinedData1 %>%
  mutate(across(c(cf4md_case_con,cf5md_case_con,f5g2b1_101,cm4md_case_con,cm5md_case_con,
                  f5g2b1_101,f5g2b1d,f5g2b1e,f5g32,f5g33,m5g2b1d,m5g2b1e,n5f2b1d,n5f2b1e,
                  f4c39,f4e10,f4j20a,f4j21,f5g19a,f5g20,f5g21a,f5g21b,f5g21c,f5g21d,f5g21e,
                  f5g21f,f5g21g,f5g21h,f5g21i,f5g22,m4c39,m4e10,m4j21,m5g20,m5g21a,m5g21b,
                  m5g21c,m5g21d,m5g21e,m5g21f,m5g21g,m5g21h,m5g21i,m5g22k,m4b30,m4b5,m4c4,
                  f4d10,f4d10a,f5c7,f5c8a,f5d21,f5d22,m4d10,m4d10a,m4e23q,m4e24,
                  m5c7,m5c8a,m5c8e,m5d21,m5d22,m5d22d), 
                ~factor(ifelse(.x == 1,1,0)))) %>%
  mutate(across(c(m4e7,f4e7,m5d7), 
                ~factor(ifelse(.x == 5,1,0)))) %>%
  mutate(across(c(m4j20a,m5g19a,f4d7h,f4d7i,f4d7j,f4d7m,f4d7n,f4d9h,
                  f4d9i,f4d9j,f4d9m,f4d9n,f5c6h,f5c6i,f5c6j,f5c6m,f5c6n,
                  m4d7h,m4d7i,m4d7j,m4d7m,m4d7n,m4d9h,m4d9i,m4d9j,
                  m4d9m,m4d9n,m4e23h,m4e23i,m4e23j,m4e23m,m4e23n,
                  m5c6h,m5c6i,m5c6j,m5c6m,m5c6n,m5d20h,m5d20i,m5d20j,m5d20m,m5d20n), 
                ~factor(ifelse(.x == 1 | .x == 2 ,1,0)))) %>%
  mutate(across(c(p4g16,p4g24b,p4g15,p4g17,p4g18,p4g19,p4g24a,p4g24c,p4g24d,p4g24e,
                  f5k10f,f5k10h,f5k10i,f5k10j,f5k10n,m5k10f,m5k10h,m5k10i,m5k10j,
                  m5k10n,p4g10,p4g14,p4g23f,p4g23h,p4g23i,p4g23j,p4g23n,p4g6,p4g8,
                  p4g9,p5i33a,p5i33c,p5i42a,p5i42c,f5k10c,f5k10d,f5k10g,f5k10k,f5k10m,
                  m5k10c,m5k10d,m5k10g,m5k10k,m5k10m,p4g11,p4g13,p4g23c,p4g23d,p4g23g,
                  p4g23k,p4g23m,p4g3,p4g4,p4g7,p5i33b,p5i42b), 
                ~factor(ifelse(.x == 1 | .x == 2 | .x == 3 | .x == 4 | .x == 5 | .x == 6,1,0)))) %>%
  mutate(across(c(m5b30,f5b24x,m4c37,f4c37), 
                ~factor(ifelse(.x == 1 | .x == 3 ,1,0)))) %>%
  mutate(across(c(m5a4,f5a4), 
                ~factor(ifelse(.x == 2 | .x == 3 ,1,0)))) %>%
  mutate(across(c(m4a4,f4a4), 
                ~factor(ifelse(.x == 3 | .x == 4 ,1,0)))) %>%
  mutate(across(c(p6b22,p6b23,p6b24,p6b31,p6b32), 
                ~factor(ifelse(.x == 1,"1","0")))) %>%
  mutate(across(c(p6b34), 
                ~factor(ifelse(.x == 1 | .x == 2,"1","0")))) %>%
  mutate(across(c(p6b30), 
                ~factor(ifelse(.x >=1,1,"0"))))

table(CombinedData3$Incarceration)
table(CombinedData3$Divorce)
table(CombinedData3$DomesticViolence)
table(CombinedData3$PhysicalAbuse)
table(CombinedData3$EmotionalAbuse)
table(CombinedData3$PhysicalNeglect)
table(CombinedData3$EmotionalNeglect)
table(CombinedData3$PhysicalNeglect)
table(CombinedData3$SexualAbuse)
table(CombinedData3$SubstanceAbuse)
table(CombinedData3$MentalIllness)

table(CombinedData5$p6b34)
CombinedData3 <- CombinedData2 %>%
  mutate(Incarceration = as.integer(rowSums(select(., m5b30, m5d7, f5b24x, m4c37, m4e7, f4c37, f4e7) == 1) > 0)) %>%
  mutate(Divorce = as.integer(rowSums(select(., m5a4,f5a4,m4a4,f4a4) == 1) > 0)) %>%
  mutate(DomesticViolence = as.integer(rowSums(select(.,f4d10,f4d10a,f4d7h,f4d7i,f4d7j,f4d7m,
                                                      f4d7n,f4d9h,f4d9i,f4d9j,f4d9m,f4d9n,
                                                      f5c6h,f5c6i,f5c6j,f5c6m,f5c6n,f5c7,f5c8a,f5d21,
                                                      f5d22,m4d10,m4d10a,m4d7h,m4d7i,m4d7j,m4d7m,
                                                      m4d7n,m4d9h,m4d9i,m4d9j,m4d9m,m4d9n,
                                                      m4e23h,m4e23i,m4e23j,m4e23m,m4e23n,
                                                      m4e23q,m4e24,m5c6h,m5c6i,m5c6j,m5c6m,m5c6n,
                                                      m5c7,m5c8a,m5c8e,m5d20h,m5d20i,m5d20j,m5d20m,
                                                      m5d20n,m5d21,m5d22,m5d22d) == 1) > 0)) %>%
  mutate(PhysicalAbuse = as.integer(rowSums(select(.,f5k10c,f5k10d,f5k10g,f5k10k,f5k10m,
                                                   m4b30,m4b5,m4c4,m5k10c,m5k10d,m5k10g,m5k10k,m5k10m,
                                                   p4g11,p4g13,p4g23c,p4g23d,p4g23g,p4g23k,p4g23m,p4g3,
                                                   p4g4,p4g7,p5i33b,p5i42b) == 1) > 0)) %>%
  mutate(EmotionalAbuse = as.integer(rowSums(select(.,f5k10f,f5k10h,f5k10i,f5k10j,f5k10n,
                                                    m5k10f,m5k10h,m5k10i,m5k10j,m5k10n,p4g10,
                                                    p4g14,p4g23f,p4g23h,p4g23i,p4g23j,p4g23n,
                                                    p4g6,p4g8,p4g9,p5i33a,p5i33c,p5i42a,p5i42c) == 1) > 0)) %>%
  mutate(PhysicalNeglect = as.integer(rowSums(select(.,p4g15,p4g17,p4g18,p4g19,p4g24a,
                                                     p4g24c,p4g24d,p4g24e) == 1) > 0)) %>%
  mutate(EmotionalNeglect = as.integer(rowSums(select(.,p4g16,p4g24b) == 1) > 0)) %>%
  mutate(SexualAbuse = as.integer(rowSums(select(.,m5b30,m5d7,f5b24x,m4c37,m4e7,f4c37, f4e7) == 1) > 0)) %>%
  mutate(SubstanceAbuse = as.integer(rowSums(select(.,f4c39,f4e10,f4j20a,f4j21,f5g19a,f5g20,
                                                    f5g21a,f5g21b,f5g21c,f5g21d,f5g21e,f5g21f,
                                                    f5g21g,f5g21h,f5g21i,f5g22,m4c39,
                                                    m4e10,m4j20a,m4j21,m5g19a,m5g20,m5g21a,
                                                    m5g21b,m5g21c,m5g21d,m5g21e,m5g21f,m5g21g,
                                                    m5g21h,m5g21i,m5g22k) == 1) > 0)) %>%
  mutate(MentalIllness = as.integer(rowSums(select(.,cf4md_case_con,cf5md_case_con,
                                                   cm4md_case_con,cm5md_case_con,
                                                   f5g2b1_101,f5g2b1d,f5g2b1e,f5g32,f5g33,
                                                   m5g2b1d,m5g2b1e,n5f2b1d,n5f2b1e) == 1) > 0))

CombinedData4 <- CombinedData3 %>%
  mutate(ACE = rowSums(select(., Incarceration:MentalIllness)))
table(CombinedData4$ACE)
CombinedData4 <- CombinedData4 %>%
  mutate(ACEcat = ifelse(ACE == 1,"1",
                         ifelse(ACE == 2,"2",
                                ifelse(ACE == 3,"3",
                                       ifelse(ACE >=4 & ACE <=11, "4 or more", "0")))))
table(CombinedData4$ACE, CombinedData4$ACEcat)
##Adding GVA Data ----
names(gva15y)
#yes/no not count
#1 categorical variable with no exposure, home exposure, school exposure, both. Within 1600m (can only be included once in each)
#3 continuous variables for how many home GVE, School GVE, Both GVE within 1600m (Each person will have count for each variable)
#1 categorical variable with no exposure, and then taking the most proximate GVE 0-400, 401-800, 801-1200, and 1201-1600 (can only be included once in each)
#GVECumSchool<-c("idnum","rg6gva_totl_100m_schl_365d_p6","rg6gva_totl_200m_schl_365d_p6","rg6gva_totl_300m_schl_365d_p6",
 #               "rg6gva_totl_400m_schl_365d_p6","rg6gva_totl_500m_schl_365d_p6","rg6gva_totl_600m_schl_365d_p6",
  #              "rg6gva_totl_700m_schl_365d_p6","rg6gva_totl_800m_schl_365d_p6","rg6gva_totl_900m_schl_365d_p6",
   #             "rg6gva_totl_1000m_schl_365d_p6","rg6gva_totl_1100m_schl_365d_p6","rg6gva_totl_1200m_schl_365d_p6",
    #            "rg6gva_totl_1300m_schl_365d_p6","rg6gva_totl_1400m_schl_365d_p6","rg6gva_totl_1500m_schl_365d_p6",
     #           "rg6gva_totl_1600m_schl_365d_p6","rg6gva_totl_1609m_schl_365d_p6")

#GVECumHome<-c("idnum","rg6gva_totl_100m_home_365d_p6","rg6gva_totl_200m_home_365d_p6","rg6gva_totl_300m_home_365d_p6",
 #               "rg6gva_totl_400m_home_365d_p6","rg6gva_totl_500m_home_365d_p6","rg6gva_totl_600m_home_365d_p6",
  #              "rg6gva_totl_700m_home_365d_p6","rg6gva_totl_800m_home_365d_p6","rg6gva_totl_900m_home_365d_p6",
   #             "rg6gva_totl_1000m_home_365d_p6","rg6gva_totl_1100m_home_365d_p6","rg6gva_totl_1200m_home_365d_p6",
    #            "rg6gva_totl_1300m_home_365d_p6","rg6gva_totl_1400m_home_365d_p6","rg6gva_totl_1500m_home_365d_p6",
     #           "rg6gva_totl_1600m_home_365d_p6","rg6gva_totl_1609m_home_365d_p6")
GVECumSchool<-c("idnum","rg6gva_totl_100m_schl_365d_p6","rg6gva_totl_200m_schl_365d_p6","rg6gva_totl_300m_schl_365d_p6",
                "rg6gva_totl_400m_schl_365d_p6")

GVECumHome<-c("idnum","rg6gva_totl_100m_home_365d_p6","rg6gva_totl_200m_home_365d_p6","rg6gva_totl_300m_home_365d_p6",
              "rg6gva_totl_400m_home_365d_p6")
 ###School ----
Table2gvaSCHOOL <- gva15y %>%
  select(., matches(GVECumSchool))

#Table2gvaSCHOOL <- Table2gvaSCHOOL %>%
 # mutate(across(c(2:18), 
  #              ~(ifelse(.x <= 0 ,0,.x))))

Table2gvaSCHOOL <- Table2gvaSCHOOL %>%
  mutate(across(c(2:5), 
                ~(ifelse(.x <= 0 ,0,.x))))
Table2gvaSCHOOL1 <- Table2gvaSCHOOL %>% remove_rownames %>% column_to_rownames(var="idnum")

Table2gvaSCHOOL1 <- pmap_df(Table2gvaSCHOOL1, ~{x <- c(...);x - lag(x, default = 0)})

#Table2gvaSCHOOL1 <- Table2gvaSCHOOL1 %>%
 # mutate(GVEExposureSchool_Cat = ifelse(rowSums(select(.,1:17)) >=1,"1","0")) %>%
  #mutate(CumSchoolExposure = rowSums(select(.,1:17)))
Table2gvaSCHOOL1 <- Table2gvaSCHOOL1 %>%
  mutate(GVEExposureSchool_Cat = ifelse(rowSums(select(.,1:4)) >=1,"1","0")) %>%
  mutate(CumSchoolExposure = rowSums(select(.,1:4)))
id <- Table2gvaSCHOOL %>%
  select(.,"idnum")

Table2gvaSCHOOL_Final <- cbind(Table2gvaSCHOOL1,id)

Table2gvaSCHOOL_Final <- Table2gvaSCHOOL_Final %>%
  mutate(School400m = rowSums(select(.,rg6gva_totl_100m_schl_365d_p6:rg6gva_totl_400m_schl_365d_p6))) #%>%
  #mutate(School800m = rowSums(select(.,rg6gva_totl_500m_schl_365d_p6:rg6gva_totl_800m_schl_365d_p6))) %>%
  #mutate(School1200m = rowSums(select(.,rg6gva_totl_900m_schl_365d_p6:rg6gva_totl_1200m_schl_365d_p6))) %>%
  #mutate(School1600m = rowSums(select(.,rg6gva_totl_1300m_schl_365d_p6:rg6gva_totl_1609m_schl_365d_p6)))
 
#Table2gvaSCHOOL_Final <- Table2gvaSCHOOL_Final %>%
 # mutate(ProximateSchoolGVE = case_when(
  #  School400m != 0 ~ "400",
   # School400m == 0 & School800m != 0 ~ "800",
    #School400m == 0 & School800m == 0 & School1200m != 0 ~ "1200",
    #School400m == 0 & School800m == 0 & School1200m == 0 & School1600m != 0 ~ "1600",
    #TRUE ~ "No GVE"
  #))

###Home ----
Table2gvaHOME <- gva15y %>%
  select(., matches(GVECumHome))

#Table2gvaHOME <- Table2gvaHOME %>%
 # mutate(across(c(2:18), 
  #              ~(ifelse(.x <= 0 ,0,.x))))

Table2gvaHOME <- Table2gvaHOME %>%
  mutate(across(c(2:5), 
                ~(ifelse(.x <= 0 ,0,.x))))

Table2gvaHOME1 <- Table2gvaHOME %>% remove_rownames %>% column_to_rownames(var="idnum")

Table2gvaHOME1 <- pmap_df(Table2gvaHOME1, ~{x <- c(...);x - lag(x, default = 0)})

#Table2gvaHOME1 <- Table2gvaHOME1 %>%
 # mutate(GVEExposureHome_Cat = ifelse(rowSums(select(.,1:17)) >=1,"1","0")) %>%
  #mutate(CumHomeExposure = rowSums(select(.,1:17)))

Table2gvaHOME1 <- Table2gvaHOME1 %>%
  mutate(GVEExposureHome_Cat = ifelse(rowSums(select(.,1:4)) >=1,"1","0")) %>%
  mutate(CumHomeExposure = rowSums(select(.,1:4)))

id <- Table2gvaHOME %>%
  select(.,"idnum")

Table2gvaHOME_Final <- cbind(Table2gvaHOME1,id)

Table2gvaHOME_Final <- Table2gvaHOME_Final %>%
  mutate(Home400m = rowSums(select(.,rg6gva_totl_100m_home_365d_p6:rg6gva_totl_400m_home_365d_p6))) #%>%
  #mutate(Home800m = rowSums(select(.,rg6gva_totl_500m_home_365d_p6:rg6gva_totl_800m_home_365d_p6))) %>%
  #mutate(Home1200m = rowSums(select(.,rg6gva_totl_900m_home_365d_p6:rg6gva_totl_1200m_home_365d_p6))) %>%
  #mutate(Home1600m = rowSums(select(.,rg6gva_totl_1300m_home_365d_p6:rg6gva_totl_1609m_home_365d_p6)))

#Table2gvaHOME_Final <- Table2gvaHOME_Final %>%
 # mutate(ProximateHomeGVE = case_when(
  #  Home400m != 0 ~ "400",
   # Home400m == 0 & Home800m != 0 ~ "800",
    #Home400m == 0 & Home800m == 0 & Home1200m != 0 ~ "1200",
    #Home400m == 0 & Home800m == 0 & Home1200m == 0 & Home1600m != 0 ~ "1600",
    #TRUE ~ "No GVE"
  #))

###Combining GVE -----
names(Table2gvaHOME_Final)
Table2gvaHOME_Final1 <- Table2gvaHOME_Final %>%
  select(.,idnum,GVEExposureHome_Cat,CumHomeExposure,Home400m)#,Home800m,Home1200m,Home1600m,ProximateHomeGVE)


names(Table2gvaSCHOOL_Final)
Table2gvaSCHOOL_Final1 <- Table2gvaSCHOOL_Final %>%
  select(.,idnum,GVEExposureSchool_Cat,CumSchoolExposure,School400m)#,School800m,School1200m,School1600m,ProximateSchoolGVE)

GVEFinal <- left_join(Table2gvaHOME_Final1, Table2gvaSCHOOL_Final1, by = "idnum")

#1 categorical variable with no exposure, home exposure, school exposure, both. Within 1600m (can only be included once in each)
#3 continuous variables for how many home GVE, School GVE, Both GVE within 1600m (Each person will have count for each variable)
#1 categorical variable with no exposure, and then taking the most proximate GVE 0-400, 401-800, 801-1200, and 1201-1600 (can only be included once in each)
table(GVEFinal$GVEExposureSchool_Cat)
table(GVEFinal$ProximateHomeGVE)
names(GVEFinal)
GVEFinal <- GVEFinal %>%
  mutate(GVExposure = case_when(
    GVEExposureSchool_Cat == "1" & GVEExposureHome_Cat == "1" |
    GVEExposureSchool_Cat == "1" & GVEExposureHome_Cat == "0" |
    GVEExposureSchool_Cat == "0" & GVEExposureHome_Cat == "1" ~"Home or School",
    GVEExposureSchool_Cat == "0" & GVEExposureHome_Cat == "0" ~"NO GVE"
  )) %>%
  mutate(CumGVExposure = CumHomeExposure + CumSchoolExposure)# %>%
  #mutate(MostProxGVE = case_when(
   # ProximateHomeGVE == "No GVE" & ProximateSchoolGVE == "No GVE" ~ "No",
    #ProximateHomeGVE == "400" | ProximateSchoolGVE == "400" ~ "400",
    #ProximateHomeGVE != "400" & ProximateSchoolGVE != "400" & 
     # ProximateHomeGVE == "800" | ProximateSchoolGVE == "800" ~ "800",
    #ProximateHomeGVE != "400" & ProximateSchoolGVE != "400" & 
      #ProximateHomeGVE != "800" & ProximateSchoolGVE != "800" &
      #ProximateHomeGVE == "1200" | ProximateSchoolGVE == "1200" ~ "1200",
    #ProximateHomeGVE != "400" & ProximateSchoolGVE != "400" & 
     # ProximateHomeGVE != "800" & ProximateSchoolGVE != "800" &
      #ProximateHomeGVE != "1200" & ProximateSchoolGVE != "1200" &
      #ProximateHomeGVE == "1600" | ProximateSchoolGVE == "1600" ~ "1600",
    #TRUE ~ "?"
  #))

table(GVEFinal$GVExposure)
table(GVEFinal$CumGVExposure)
#table(GVEFinal$MostProxGVE)
#table(GVEFinal$ProximateHomeGVE)
#table(GVEFinal$ProximateSchoolGVE)

GVEFinal1 <- GVEFinal %>%
  select(.,idnum,GVExposure,CumGVExposure,CumHomeExposure,CumSchoolExposure)#,MostProxGVE)
#Combining Final Dataset ----
CombinedData5 <- CombinedData4 %>%
  select(.,idnum,cp6intyr,ACEcat,p6b22,p6b23,p6b24,p6b34)#,#k5citywt:k5citywt_rep72)
CombinedData5$idnum <- as.numeric(CombinedData5$idnum)
GVEFinal1$idnum <- as.numeric(GVEFinal1$idnum)


FinalData <- left_join(CombinedData5, GVEFinal1, by = "idnum")
names(FinalData)

lapply(FinalData, summary)

FinalDataTeen <- FinalData 

#variable_to_check <- "k5citywt" 

#FinalDataTeen <- FinalDataTeen[complete.cases(FinalDataTeen[variable_to_check]), ]

#summary(FinalDataTeen$k5citywt)

names(FinalDataTeen)
FinalDataTeen <- FinalDataTeen %>%
  rename(., "AccidentInjury" = p6b22, "Illness" = p6b23, "RegularCheckUp" = p6b24, "Dentist" = p6b34)

FinalDataTeen$AccidentInjury <- as.integer(FinalDataTeen$AccidentInjury)
#FinalDataTeen$GVExposure <- factor(FinalDataTeen$GVExposure, 
 #                                  levels = c("NO GVE","Home","School","Home and School"), 
  #                                 ordered = TRUE)
#FinalDataTeen$MostProxGVE <- factor(FinalDataTeen$MostProxGVE, 
 #                                  levels = c("No","400","800","1200","1600"), 
  #                                 ordered = TRUE)
FinalDataTeen$ACEcat <- factor(FinalDataTeen$ACEcat, 
                                    levels = c("0","1","2","3","4 or more"), 
                                    ordered = TRUE)

#Unweighted Survey Analysis -----
names(FinalDataTeen)
FinalDataTeen1 <- subset(FinalDataTeen, cp6intyr != 2014) 
RegCheckUp <- FinalDataTeen1 %>%
  tbl_summary(
    include = c(GVExposure),
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ "{median} ({p25}, {p75})"),
    missing_text = "(Missing)",
    type = c(GVExposure) ~ "categorical",
    by = RegularCheckUp,
    percent = "row",
    digits = list(all_categorical() ~ c(0,2),
                  all_continuous() ~ c(2,2))) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall()
RegCheckUp
RegCheckUp  %>%
  as_gt() %>%
  gt::gtsave(filename = "UnWeightedRegCheckUp.rtf")

Dentist <- FinalDataTeen1 %>%
  tbl_summary(
    include = c(GVExposure),
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ "{median} ({p25}, {p75})"),
    missing_text = "(Missing)",
    type = c(GVExposure) ~ "categorical",
    by = Dentist,
    percent = "row",
    digits = list(all_categorical() ~ c(0,2),
                  all_continuous() ~ c(2,2))) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall()
Dentist
Dentist  %>%
  as_gt() %>%
  gt::gtsave(filename = "UnWeightedDentist.rtf")

Illness <- FinalDataTeen1 %>%
  tbl_summary(
    include = c(GVExposure),
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ "{median} ({p25}, {p75})"),
    missing_text = "(Missing)",
    type = c(GVExposure) ~ "categorical",
    by = Illness,
    percent = "row",
    digits = list(all_categorical() ~ c(0,2),
                  all_continuous() ~ c(2,2))) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall()
Illness
Illness  %>%
  as_gt() %>%
  gt::gtsave(filename = "UnWeightedIllness.rtf")

AccidentInjury <- FinalDataTeen1 %>%
  tbl_summary(
    include = c(GVExposure),
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ "{median} ({p25}, {p75})"),
    missing_text = "(Missing)",
    type = c(GVExposure) ~ "categorical",
    by = AccidentInjury,
    percent = "row",
    digits = list(all_categorical() ~ c(0,2),
                  all_continuous() ~ c(2,2))) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall()
AccidentInjury
AccidentInjury  %>%
  as_gt() %>%
  gt::gtsave(filename = "UnWeightedAccidentInjury.rtf")








#Weighted Survey Analysis ----

rep_weights <- colnames(select(FinalDataTeen,k5citywt_rep1:k5citywt_rep72))
formula <- as.formula(paste("~", paste(rep_weights, collapse = "+")))
SvyDataTeen <- svrepdesign(repweights = formula,
                           weights = ~k5citywt,
                           data = FinalDataTeen,
                           type = "JK1",
                           scale = 1,
                           rscales = 1,
                           combined.weights = TRUE,
                           mse=getOption("survey.replicates.mse")) %>% 
  subset(., cp6intyr != 2014) 

####ACE and Health Care ----
#22
ACE0_22 <- svyby(~I(ACEcat=="0"), ~AccidentInjury, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE1_22 <- svyby(~I(ACEcat=="1"), ~AccidentInjury, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE2_22 <- svyby(~I(ACEcat=="2"), ~AccidentInjury, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE3_22 <- svyby(~I(ACEcat=="3"), ~AccidentInjury, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE4_22 <- svyby(~I(ACEcat=="4 or more"), ~AccidentInjury, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)
#23
ACE0_23 <- svyby(~I(ACEcat=="0"), ~Illness, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE1_23 <- svyby(~I(ACEcat=="1"), ~Illness, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE2_23 <- svyby(~I(ACEcat=="2"), ~Illness, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE3_23 <- svyby(~I(ACEcat=="3"), ~Illness, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE4_23 <- svyby(~I(ACEcat=="4 or more"), ~Illness, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)
#24
ACE0_24 <- svyby(~I(ACEcat=="0"), ~RegularCheckUp, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE1_24 <- svyby(~I(ACEcat=="1"), ~RegularCheckUp, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE2_24 <- svyby(~I(ACEcat=="2"), ~RegularCheckUp, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE3_24 <- svyby(~I(ACEcat=="3"), ~RegularCheckUp, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE4_24 <- svyby(~I(ACEcat=="4 or more"), ~RegularCheckUp, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)
#34
ACE0_34 <- svyby(~I(ACEcat=="0"), ~Dentist, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE1_34 <- svyby(~I(ACEcat=="1"), ~Dentist, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE2_34 <- svyby(~I(ACEcat=="2"), ~Dentist, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE3_34 <- svyby(~I(ACEcat=="3"), ~Dentist, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE4_34 <- svyby(~I(ACEcat=="4 or more"), ~Dentist, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

Ace0 <- bind_rows(
  ACE0_22 = ACE0_22,
  ACE0_23 = ACE0_23,
  ACE0_24 = ACE0_24,
  ACE0_34 = ACE0_34,
  ACE1_22 = ACE1_22,
  ACE1_23 = ACE1_23,
  ACE1_24 = ACE1_24,
  ACE1_34 = ACE1_34,
  ACE2_22 = ACE2_22,
  ACE2_23 = ACE2_23,
  ACE2_24 = ACE2_24,
  ACE2_34 = ACE2_34,
  ACE3_22 = ACE3_22,
  ACE3_23 = ACE3_23,
  ACE3_24 = ACE3_24,
  ACE3_34 = ACE3_34,
  ACE4_22 = ACE4_22,
  ACE4_23 = ACE4_23,
  ACE4_24 = ACE4_24,
  ACE4_34 = ACE4_34,
)

write.csv(Ace0, "ACE.csv")

#AccidentInjury,Illness,RegularCheckUp,Dentist,ACEcat,GVExposure,MostProxGVE
variables_of_interest <- c("ACEcat", "GVExposure", "MostProxGVE")
table(SvyDataTeen$variables$GVExposure)
####GVE EXposure and Healthcare ----
#22
ACE0_22 <- svyby(~I(GVExposure=="NO GVE"), ~AccidentInjury, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE1_22 <- svyby(~I(GVExposure=="Home"), ~AccidentInjury, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE2_22 <- svyby(~I(GVExposure=="School"), ~AccidentInjury, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE3_22 <- svyby(~I(GVExposure=="Home and School"), ~AccidentInjury, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)


#23
ACE0_23 <- svyby(~I(GVExposure=="NO GVE"), ~Illness, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE1_23 <- svyby(~I(GVExposure=="Home"), ~Illness, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE2_23 <- svyby(~I(GVExposure=="School"), ~Illness, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE3_23 <- svyby(~I(GVExposure=="Home and School"), ~Illness, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)
#24
ACE0_24 <- svyby(~I(GVExposure=="NO GVE"), ~RegularCheckUp, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE1_24 <- svyby(~I(GVExposure=="Home"), ~RegularCheckUp, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE2_24 <- svyby(~I(GVExposure=="School"), ~RegularCheckUp, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE3_24 <- svyby(~I(GVExposure=="Home and School"), ~RegularCheckUp, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

#34
ACE0_34 <- svyby(~I(GVExposure=="NO GVE"), ~Dentist, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE1_34 <- svyby(~I(GVExposure=="Home"), ~Dentist, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE2_34 <- svyby(~I(GVExposure=="School"), ~Dentist, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE3_34 <- svyby(~I(GVExposure=="Home and School"), ~Dentist, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)


Ace0 <- bind_rows(
  ACE0_22 = ACE0_22,
  ACE0_23 = ACE0_23,
  ACE0_24 = ACE0_24,
  ACE0_34 = ACE0_34,
  ACE1_22 = ACE1_22,
  ACE1_23 = ACE1_23,
  ACE1_24 = ACE1_24,
  ACE1_34 = ACE1_34,
  ACE2_22 = ACE2_22,
  ACE2_23 = ACE2_23,
  ACE2_24 = ACE2_24,
  ACE2_34 = ACE2_34,
  ACE3_22 = ACE3_22,
  ACE3_23 = ACE3_23,
  ACE3_24 = ACE3_24,
  ACE3_34 = ACE3_34,
)

write.csv(Ace0, "GVE_Cat_HealthCare.csv")

####ACE and Healthcare by GVE Most Proximate ----

#22
ACE0_22 <- svyby(~I(ACEcat=="0"), ~AccidentInjury+MostProxGVE, design = SvyDataTeen,svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE1_22 <- svyby(~I(ACEcat=="1"), ~AccidentInjury+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE2_22 <- svyby(~I(ACEcat=="2"), ~AccidentInjury+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE3_22 <- svyby(~I(ACEcat=="3"), ~AccidentInjury+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE4_22 <- svyby(~I(ACEcat=="4 or more"), ~AccidentInjury+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)
#23
ACE0_23 <- svyby(~I(ACEcat=="0"), ~Illness+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE1_23 <- svyby(~I(ACEcat=="1"), ~Illness+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE2_23 <- svyby(~I(ACEcat=="2"), ~Illness+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE3_23 <- svyby(~I(ACEcat=="3"), ~Illness+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE4_23 <- svyby(~I(ACEcat=="4 or more"), ~Illness+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)
#24
ACE0_24 <- svyby(~I(ACEcat=="0"), ~RegularCheckUp+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE1_24 <- svyby(~I(ACEcat=="1"), ~RegularCheckUp+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE2_24 <- svyby(~I(ACEcat=="2"), ~RegularCheckUp+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE3_24 <- svyby(~I(ACEcat=="3"), ~RegularCheckUp+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE4_24 <- svyby(~I(ACEcat=="4 or more"), ~RegularCheckUp+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)
#34
ACE0_34 <- svyby(~I(ACEcat=="0"), ~Dentist+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE1_34 <- svyby(~I(ACEcat=="1"), ~Dentist+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE2_34 <- svyby(~I(ACEcat=="2"), ~Dentist+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE3_34 <- svyby(~I(ACEcat=="3"), ~Dentist+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

ACE4_34 <- svyby(~I(ACEcat=="4 or more"), ~Dentist+MostProxGVE, design = SvyDataTeen, svyciprop,
                 vartype = "ci", na.rm = TRUE)

Ace0 <- bind_rows(
  ACE0_22 = ACE0_22,
  ACE0_23 = ACE0_23,
  ACE0_24 = ACE0_24,
  ACE0_34 = ACE0_34,
  ACE1_22 = ACE1_22,
  ACE1_23 = ACE1_23,
  ACE1_24 = ACE1_24,
  ACE1_34 = ACE1_34,
  ACE2_22 = ACE2_22,
  ACE2_23 = ACE2_23,
  ACE2_24 = ACE2_24,
  ACE2_34 = ACE2_34,
  ACE3_22 = ACE3_22,
  ACE3_23 = ACE3_23,
  ACE3_24 = ACE3_24,
  ACE3_34 = ACE3_34,
  ACE4_22 = ACE4_22,
  ACE4_23 = ACE4_23,
  ACE4_24 = ACE4_24,
  ACE4_34 = ACE4_34,
)

write.csv(Ace0, "ACE_Health_MostProxGVE.csv")


####GVE Cum Exposure and Healthcare ----
#22
ACE0_22 <- svyby(~CumGVExposure, ~AccidentInjury, design = SvyDataTeen, svyciprop,
                 vartype = c("var","ci"), na.rm = TRUE)

ACE1_22 <- svyby(~CumHomeExposure, ~AccidentInjury, design = SvyDataTeen, svyciprop,
                 vartype = c("var","ci"), na.rm = TRUE)

ACE2_22 <- svyby(~CumSchoolExposure, ~AccidentInjury, design = SvyDataTeen, svyciprop,
                 vartype = c("var","ci"), na.rm = TRUE)

#23
ACE0_23 <- svyby(~CumGVExposure, ~Illness, design = SvyDataTeen, svyciprop,
                 vartype = c("var","ci"), na.rm = TRUE)

ACE1_23 <- svyby(~CumHomeExposure, ~Illness, design = SvyDataTeen, svyciprop,
                 vartype = c("var","ci"), na.rm = TRUE)

ACE2_23 <- svyby(~CumSchoolExposure, ~Illness, design = SvyDataTeen, svyciprop,
                 vartype = c("var","ci"), na.rm = TRUE)

#24
ACE0_24 <- svyby(~CumGVExposure, ~RegularCheckUp, design = SvyDataTeen, svyciprop,
                 vartype = c("var","ci"), na.rm = TRUE)

ACE1_24 <- svyby(~CumHomeExposure, ~RegularCheckUp, design = SvyDataTeen, svyciprop,
                 vartype = c("var","ci"), na.rm = TRUE)

ACE2_24 <- svyby(~CumSchoolExposure, ~RegularCheckUp, design = SvyDataTeen, svyciprop,
                 vartype = c("var","ci"), na.rm = TRUE)

#34
ACE0_34 <- svyby(~CumGVExposure, ~Dentist, design = SvyDataTeen, svyciprop,
                 vartype = c("var","ci"), na.rm = TRUE)

ACE1_34 <- svyby(~CumHomeExposure, ~Dentist, design = SvyDataTeen, svyciprop,
                 vartype = c("var","ci"), na.rm = TRUE)

ACE2_34 <- svyby(~CumSchoolExposure, ~Dentist, design = SvyDataTeen, svyciprop,
                 vartype = c("var","ci"), na.rm = TRUE)


Ace0 <- bind_rows(
  ACE0_22 = ACE0_22,
  ACE0_23 = ACE0_23,
  ACE0_24 = ACE0_24,
  ACE0_34 = ACE0_34,
  ACE1_22 = ACE1_22,
  ACE1_23 = ACE1_23,
  ACE1_24 = ACE1_24,
  ACE1_34 = ACE1_34,
  ACE2_22 = ACE2_22,
  ACE2_23 = ACE2_23,
  ACE2_24 = ACE2_24,
  ACE2_34 = ACE2_34,
  ACE3_22 = ACE3_22,
  ACE3_23 = ACE3_23,
  ACE3_24 = ACE3_24,
  ACE3_34 = ACE3_34,
)

write.csv(Ace0, "GVE_Cat_HealthCare.csv")

SvyDataTeen_NoRep <- svydesign(ids = ~idnum,
          weights = ~k5citywt,
          data = FinalDataTeen) %>%
  subset(., cp6intyr != 2014) 
subs
table(SvyDataTeen_NoRep$variables$Dentist)
table(SvyDataTeen_NoRep$variables$AccidentInjury)
table(SvyDataTeen_NoRep$variables$Illness,SvyDataTeen_NoRep$variables$ACEcat)
table(SvyDataTeen_NoRep$variables$RegularCheckUp)
table(SvyDataTeen_NoRep$variables$ACEcat)

###HealthCare utiiation, ACE, and GVE Tables ----
Injury <- SvyDataTeen_NoRep %>%
  tbl_svysummary(
    include = c(CumGVExposure,CumHomeExposure,CumSchoolExposure, GVExposure,ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = AccidentInjury,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall()

Injury  %>%
  as_gt() %>%
  gt::gtsave(filename = "Injury.rtf")

Illness <- SvyDataTeen_NoRep %>%
  tbl_svysummary(
    include = c(CumGVExposure,CumHomeExposure,CumSchoolExposure, GVExposure,ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = Illness,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall()

Illness  %>%
  as_gt() %>%
  gt::gtsave(filename = "Illness.rtf")

RegCheckup <- SvyDataTeen_NoRep %>%
  tbl_svysummary(
    include = c(CumGVExposure,CumHomeExposure,CumSchoolExposure, GVExposure,ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = RegularCheckUp,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall()

RegCheckup  %>%
  as_gt() %>%
  gt::gtsave(filename = "RegCheckup.rtf")

Dentist <- SvyDataTeen_NoRep %>%
  tbl_svysummary(
    include = c(CumGVExposure,CumHomeExposure,CumSchoolExposure, GVExposure,ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = Dentist,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall()

Dentist  %>%
  as_gt() %>%
  gt::gtsave(filename = "Dentist.rtf")

###Healthcare/ACE, Most Prox GVE = No -----
table(SvyDataTeen_NoRep$variables$MostProxGVE)

InjuryNo <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "No") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = AccidentInjury,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall()

table(SvyDataTeen_NoRep$variables$ACEcat,SvyDataTeen_NoRep$variables$MostProxGVE,SvyDataTeen_NoRep$variables$Illness)

IllnessNo <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "No") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = Illness,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))

RegCheckupNo <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "No") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = RegularCheckUp,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))

DentistNo <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "No") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = Dentist,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))

HealthcareNOGVE<- tbl_stack(tbls = list(InjuryNo,IllnessNo,RegCheckupNo,DentistNo))

HealthcareNOGVE  %>%
  as_gt() %>%
  gt::gtsave(filename = "HealthcareNOGVE.rtf")
###Healthcare/ACE Most Prox GVE = 400 -----
table(SvyDataTeen_NoRep$variables$MostProxGVE)

Injury400 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "400") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = AccidentInjury,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall()


Illness400 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "400") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = Illness,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))

RegCheckup400 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "400") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = RegularCheckUp,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))


Dentist400 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "400") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = Dentist,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))

HealthcareGVE400<-tbl_merge(tbls = list(Injury400,Illness400,RegCheckup400,Dentist400))

HealthcareGVE400  %>%
  as_gt() %>%
  gt::gtsave(filename = "HealthcareGVE400.rtf")
###Healthcare/ACE Most Prox GVE = 800 -----
table(SvyDataTeen_NoRep$variables$MostProxGVE)

Injury800 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "800") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = AccidentInjury,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall()


Illness800 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "800") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = Illness,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))

RegCheckup800 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "800") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = RegularCheckUp,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))


Dentist800 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "800") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = Dentist,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))

HealthcareGVE800 <- tbl_merge(tbls = list(Injury800,Illness800,RegCheckup800,Dentist800))

HealthcareGVE800  %>%
  as_gt() %>%
  gt::gtsave(filename = "HealthcareGVE800.rtf")
###Healthcare/ACE Most Prox GVE = 1200 -----
table(SvyDataTeen_NoRep$variables$MostProxGVE)

Injury1200 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "1200") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = AccidentInjury,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall()


Illness1200 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "1200") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = Illness,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))

RegCheckup1200 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "1200") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = RegularCheckUp,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))


Dentist1200 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "1200") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = Dentist,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))

HealthcareGVE1200 <- tbl_merge(tbls = list(Injury1200,Illness1200,RegCheckup1200,Dentist1200))
HealthcareGVE1200  %>%
  as_gt() %>%
  gt::gtsave(filename = "HealthcareGVE1200.rtf")

###Healthcare/ACE Most Prox GVE = 1600 -----
table(SvyDataTeen_NoRep$variables$MostProxGVE)

Injury1600 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "1600") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = AccidentInjury,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall()


Illness1600 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "1600") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = Illness,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))

RegCheckup1600 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "1600") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = RegularCheckUp,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))


Dentist1600 <- SvyDataTeen_NoRep %>%
  subset(., MostProxGVE == "1600") %>%
  tbl_svysummary(
    include = c(ACEcat),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = Dentist,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))

HealthcareGVE1600 <- tbl_merge(tbls = list(Injury1600,Illness1600,RegCheckup1600,Dentist1600))

HealthcareGVE1600  %>%
  as_gt() %>%
  gt::gtsave(filename = "HealthcareGVE1600.rtf")
###Healthcare/ACE,CUM GVE-----
#CumGVExposure,CumHomeExposure,CumSchoolExposure
table(SvyDataTeen_NoRep$variables$MostProxGVE)

Injury_Yes <- SvyDataTeen_NoRep %>%
  subset(., AccidentInjury == "2") %>%
  tbl_svysummary(
    include = c(CumGVExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_overall()

Illness_Yes <- SvyDataTeen_NoRep %>%
  subset(., Illness == "1") %>%
  tbl_svysummary(
    include = c(CumGVExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci()

RegCheckup_Yes <- SvyDataTeen_NoRep %>%
  subset(., RegularCheckUp == "1") %>%
  tbl_svysummary(
    include = c(CumGVExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci()

Dentist_Yes <- SvyDataTeen_NoRep %>%
  subset(., Dentist == "1") %>%
  tbl_svysummary(
    include = c(CumGVExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci()

Healthcare_Yes_NumGVE <- tbl_merge(tbls = list(Injury_Yes,Illness_Yes,RegCheckup_Yes,Dentist_Yes))
Healthcare_Yes_NumGVE  %>%
  as_gt() %>%
  gt::gtsave(filename = "Healthcare_Yes_NumGVE.rtf")
###Healthcare/ACE,CUM Home GVE-----
#CumGVExposure,CumHomeExposure,CumSchoolExposure
table(SvyDataTeen_NoRep$variables$MostProxGVE)

Injury_YesHome <- SvyDataTeen_NoRep %>%
  subset(., AccidentInjury == "2") %>%
  tbl_svysummary(
    include = c(CumHomeExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_overall()

Illness_YesHome <- SvyDataTeen_NoRep %>%
  subset(., Illness == "1") %>%
  tbl_svysummary(
    include = c(CumHomeExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci()

RegCheckup_YesHome <- SvyDataTeen_NoRep %>%
  subset(., RegularCheckUp == "1") %>%
  tbl_svysummary(
    include = c(CumHomeExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci()

Dentist_YesHome <- SvyDataTeen_NoRep %>%
  subset(., Dentist == "1") %>%
  tbl_svysummary(
    include = c(CumHomeExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci()

Healthcare_Yes_MostProxHomeGVE<-tbl_merge(tbls = list(Injury_YesHome,Illness_YesHome,RegCheckup_YesHome,Dentist_YesHome))
Healthcare_Yes_MostProxHomeGVE  %>%
  as_gt() %>%
  gt::gtsave(filename = "Healthcare_Yes_CumHomeGVE.rtf")
###Healthcare/ACE,CUM School GVE-----
#CumGVExposure,CumHomeExposure,CumSchoolExposure
table(SvyDataTeen_NoRep$variables$AccidentInjury)

Injury_YesSchool <- SvyDataTeen_NoRep %>%
  subset(., AccidentInjury == "2") %>%
  tbl_svysummary(
    include = c(CumSchoolExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>%
  add_overall()

Illness_YesSchool <- SvyDataTeen_NoRep %>%
  subset(., Illness == "1") %>%
  tbl_svysummary(
    include = c(CumSchoolExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci()

RegCheckup_YesSchool <- SvyDataTeen_NoRep %>%
  subset(., RegularCheckUp == "1") %>%
  tbl_svysummary(
    include = c(CumSchoolExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci() 

Dentist_YesSchool <- SvyDataTeen_NoRep %>%
  subset(., Dentist == "1") %>%
  tbl_svysummary(
    include = c(CumSchoolExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    sort = all_categorical() ~ "frequency",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci()

Healthcare_Yes_MostProxSchoolGVE <- tbl_merge(tbls = list(Injury_YesSchool,Illness_YesSchool,RegCheckup_YesSchool,Dentist_YesSchool))

Healthcare_Yes_MostProxSchoolGVE  %>%
  as_gt() %>%
  gt::gtsave(filename = "Healthcare_Yes_CumSchoolGVE.rtf")
##Healthcare/ACE, GVE EXPOSURE-----

HOMEGVE <- SvyDataTeen_NoRep %>%
  subset(., GVExposure == "Home") %>%
  tbl_svysummary(
    include = c(CumHomeExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci()

SCHOOLGVE <- SvyDataTeen_NoRep %>%
  subset(., GVExposure == "School") %>%
  tbl_svysummary(
    include = c(CumSchoolExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci()

CUMGVEXPOSURE <- SvyDataTeen_NoRep %>%
  tbl_svysummary(
    include = c(CumGVExposure),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    by = ACEcat,
    missing_text = "(Missing)",
    percent = "row",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0))) %>%
  add_ci()

Ace_CumGVE<- tbl_stack(tbls = list(CUMGVEXPOSURE,HOMEGVE,SCHOOLGVE))

Ace_CumGVE  %>%
  as_gt() %>%
  gt::gtsave(filename = "Ace_CumGVE.rtf")

#Characteristics of Final Unweighted Sample -----

FinalDataTeen_UnWeighted <- subset(FinalDataTeen, cp6intyr != 2014) 

names(FinalDataTeen_UnWeighted)
names(CombinedData4)
CombinedData4_UnWeighted <- CombinedData4 %>%
  select(.,"idnum","Incarceration","Divorce","DomesticViolence","PhysicalAbuse","EmotionalAbuse",
         "PhysicalNeglect","EmotionalNeglect","SexualAbuse","SubstanceAbuse","MentalIllness",
         "ACE","ACEcat")

FinalDataTeen_UnWeighted$idnum <- as.numeric(FinalDataTeen_UnWeighted$idnum)
CombinedData4_UnWeighted$idnum <- as.numeric(CombinedData4_UnWeighted$idnum)

FinalDataTeen_UnWeighted <- left_join(FinalDataTeen_UnWeighted, CombinedData4_UnWeighted, by = "idnum")

Table14 <- FinalDataTeen_UnWeighted %>%
  tbl_summary(
    include = c(AccidentInjury,Illness,RegularCheckUp,Dentist,ACEcat.x,ACE,Incarceration,
                Divorce,DomesticViolence,PhysicalAbuse,EmotionalAbuse,PhysicalNeglect,
                EmotionalNeglect,SexualAbuse,SubstanceAbuse,MentalIllness,GVExposure,
                CumGVExposure,CumHomeExposure,CumSchoolExposure,MostProxGVE),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = c(ACE,CumGVExposure,CumHomeExposure,CumSchoolExposure) ~ "continuous",
    missing_text = "(Missing)",
    digits = list(all_continuous() ~ c(2,2),
                  all_categorical() ~ c(0,0)))

Table14  %>%
  as_gt() %>%
  gt::gtsave(filename = "Table14.rtf")

Test <- subset(CombinedData4, cp6intyr != 2014) 
Test1 <- subset(Test, !(is.na(p5citywt)))


#ACE SCORE Assessment ----


names(CombinedData4)
AceVariables <- CombinedData4 %>%
  dplyr::select(.,m5b30, m5d7, f5b24x, m4c37, m4e7, f4c37, f4e7,m5a4,f5a4,m4a4,f4a4,
                f4d10,f4d10a,f4d7h,f4d7i,f4d7j,f4d7m,
                f4d7n,f4d9h,f4d9i,f4d9j,f4d9m,f4d9n,
                f5c6h,f5c6i,f5c6j,f5c6m,f5c6n,f5c7,f5c8a,f5d21,
                f5d22,m4d10,m4d10a,m4d7h,m4d7i,m4d7j,m4d7m,
                m4d7n,m4d9h,m4d9i,m4d9j,m4d9m,m4d9n,
                m4e23h,m4e23i,m4e23j,m4e23m,m4e23n,
                f5k10c,f5k10d,f5k10g,f5k10k,f5k10m,
                m4b30,m4b5,m4c4,m5k10c,m5k10d,m5k10g,m5k10k,m5k10m,
                p4g11,p4g13,p4g23c,p4g23d,p4g23g,p4g23k,p4g23m,p4g3,
                p4g4,p4g7,p5i33b,p5i42b,
                m4e23q,m4e24,m5c6h,m5c6i,m5c6j,m5c6m,m5c6n,
                m5c7,m5c8a,m5c8e,m5d20h,m5d20i,m5d20j,m5d20m,
                m5d20n,m5d21,m5d22,m5d22d,f5k10f,f5k10h,f5k10i,f5k10j,f5k10n,
                m5k10f,m5k10h,m5k10i,m5k10j,m5k10n,p4g10,
                p4g14,p4g23f,p4g23h,p4g23i,p4g23j,p4g23n,
                p4g6,p4g8,p4g9,p5i33a,p5i33c,p5i42a,p5i42c,f5k10f,f5k10h,f5k10i,f5k10j,f5k10n,
                p4g15,p4g17,p4g18,p4g19,p4g24a,
                p4g24c,p4g24d,p4g24e,p4g16,p4g24b,m5b30,m5d7,f5b24x,m4c37,m4e7,f4c37, f4e7,
                f4c39,f4e10,f4j20a,f4j21,f5g19a,f5g20,
                f5g21a,f5g21b,f5g21c,f5g21d,f5g21e,f5g21f,
                f5g21g,f5g21h,f5g21i,f5g22,m4c39,
                m4e10,m4j20a,m4j21,m5g19a,m5g20,m5g21a,
                m5g21b,m5g21c,m5g21d,m5g21e,m5g21f,m5g21g,
                m5g21h,m5g21i,m5g22k,cf4md_case_con,cf5md_case_con,
                cm4md_case_con,cm5md_case_con,
                f5g2b1_101,f5g2b1d,f5g2b1e,f5g32,f5g33,
                m5g2b1d,m5g2b1e,n5f2b1d,n5f2b1e)#, 337:347)
names(AceVariables)

ACESubGroups <- CombinedData4 %>%
  select(.,337:345)


AceVariables <- AceVariables %>%
  mutate(across(c(1:171), 
                ~factor(ifelse(.x == 0 ,1,NA))))

ACESubGroups <- ACESubGroups %>%
  mutate(across(c(1:9), 
              ~factor(ifelse(.x == 0 ,1,NA))))


gg<- AceVariables %>% 
  gg_miss_var(show_pct = TRUE) + 
  labs(y = "Percent Yes")
gg
ggsave(filename = "Count of Yes.tiff", width = 8, height = 7,gg, dpi = 800)
gg<- ACESubGroups %>% 
  gg_miss_var(show_pct = TRUE) + 
  labs(y = "Percent Yes")
gg
ggsave(filename = "Subgroup of Yes.tiff", width = 8, height = 7,gg, dpi = 800)

  
PhysicalAbuseVar <- CombinedData4 %>%
  select(.,f5k10c,f5k10d,f5k10g,f5k10k,f5k10m,
         m4b30,m4b5,m4c4,m5k10c,m5k10d,m5k10g,m5k10k,m5k10m,
         p4g11,p4g13,p4g23c,p4g23d,p4g23g,p4g23k,p4g23m,p4g3,
         p4g4,p4g7,p5i33b,p5i42b)


PhysicalAbuseVar <- PhysicalAbuseVar %>%
  mutate(across(c(f5k10c,f5k10d,f5k10g,f5k10k,f5k10m,
                  m4b30,m4b5,m4c4,m5k10c,m5k10d,m5k10g,m5k10k,m5k10m,
                  p4g11,p4g13,p4g23c,p4g23d,p4g23g,p4g23k,p4g23m,p4g3,
                  p4g4,p4g7,p5i33b,p5i42b), 
                ~factor(ifelse(.x == 0 ,1,NA))))

gg<- PhysicalAbuseVar %>% 
  gg_miss_var(show_pct = TRUE) + 
  labs(y = "Percent Yes")
gg
ggsave(filename = "PhysicalAbuseVar of Yes.tiff", width = 8, height = 7,gg, dpi = 800)


EmotionalAbuseVar <- CombinedData4 %>%
  select(.,f5k10f,f5k10h,f5k10i,f5k10j,f5k10n,
         m5k10f,m5k10h,m5k10i,m5k10j,m5k10n,p4g10,
         p4g14,p4g23f,p4g23h,p4g23i,p4g23j,p4g23n,
         p4g6,p4g8,p4g9,p5i33a,p5i33c,p5i42a,p5i42c)


EmotionalAbuseVar <- EmotionalAbuseVar %>%
  mutate(across(c(f5k10f,f5k10h,f5k10i,f5k10j,f5k10n,
                  m5k10f,m5k10h,m5k10i,m5k10j,m5k10n,p4g10,
                  p4g14,p4g23f,p4g23h,p4g23i,p4g23j,p4g23n,
                  p4g6,p4g8,p4g9,p5i33a,p5i33c,p5i42a,p5i42c), 
                ~factor(ifelse(.x == 0 ,1,NA))))

gg<- EmotionalAbuseVar %>% 
  gg_miss_var(show_pct = TRUE) + 
  labs(y = "Percent Yes")
gg
ggsave(filename = "EmotionalAbuseVar of Yes.tiff", width = 8, height = 7,gg, dpi = 800)




PhysicalNeglectVar <- CombinedData4 %>%
  select(.,p4g15,p4g17,p4g18,p4g19,p4g24a,
         p4g24c,p4g24d,p4g24e)


PhysicalNeglectVar <- PhysicalNeglectVar %>%
  mutate(across(c(p4g15,p4g17,p4g18,p4g19,p4g24a,
                  p4g24c,p4g24d,p4g24e), 
                ~factor(ifelse(.x == 0 ,1,NA))))

gg<- PhysicalNeglectVar %>% 
  gg_miss_var(show_pct = TRUE) + 
  labs(y = "Percent Yes")
gg
ggsave(filename = "PhysicalNeglectVar of Yes.tiff", width = 8, height = 7,gg, dpi = 800)




EmotionalNeglectVar <- CombinedData4 %>%
  select(.,p4g16,p4g24b)


EmotionalNeglectVar <- EmotionalNeglectVar %>%
  mutate(across(c(p4g16,p4g24b), 
                ~factor(ifelse(.x == 0 ,1,NA))))

gg<- EmotionalNeglectVar %>% 
  gg_miss_var(show_pct = TRUE) + 
  labs(y = "Percent Yes")
gg
ggsave(filename = "EmotionalNeglectVar of Yes.tiff", width = 8, height = 7,gg, dpi = 800)

#Subset ACE ----
CombinedData <- left_join(Wave1VarInt,Wave6VarInt, by = "idnum")
CombinedData1 <- left_join(CombinedData,Wave4_5, by = "idnum")

CombinedData2 <- CombinedData1 %>%
  mutate(across(c(cf4md_case_con,cf5md_case_con,f5g2b1_101,cm4md_case_con,cm5md_case_con,
                  f5g2b1_101,f5g2b1d,f5g2b1e,f5g32,f5g33,m5g2b1d,m5g2b1e,n5f2b1d,n5f2b1e,
                  f4c39,f4e10,f4j20a,f4j21,f5g19a,f5g20,f5g21a,f5g21b,f5g21c,f5g21d,f5g21e,
                  f5g21f,f5g21g,f5g21h,f5g21i,f5g22,m4c39,m4e10,m4j21,m5g20,m5g21a,m5g21b,
                  m5g21c,m5g21d,m5g21e,m5g21f,m5g21g,m5g21h,m5g21i,m5g22k,m4b30,m4b5,m4c4,
                  f4d10,f4d10a,f5c7,f5c8a,f5d21,f5d22,m4d10,m4d10a,m4e23q,m4e24,
                  m5c7,m5c8a,m5c8e,m5d21,m5d22,m5d22d), 
                ~factor(ifelse(.x == 1,1,0)))) %>%
  mutate(across(c(m4e7,f4e7,m5d7), 
                ~factor(ifelse(.x == 5,1,0)))) %>%
  mutate(across(c(m4j20a,m5g19a,f4d7h,f4d7i,f4d7j,f4d7m,f4d7n,f4d9h,
                  f4d9i,f4d9j,f4d9m,f4d9n,f5c6h,f5c6i,f5c6j,f5c6m,f5c6n,
                  m4d7h,m4d7i,m4d7j,m4d7m,m4d7n,m4d9h,m4d9i,m4d9j,
                  m4d9m,m4d9n,m4e23h,m4e23i,m4e23j,m4e23m,m4e23n,
                  m5c6h,m5c6i,m5c6j,m5c6m,m5c6n,m5d20h,m5d20i,m5d20j,m5d20m,m5d20n), 
                ~factor(ifelse(.x == 1 | .x == 2 ,1,0)))) %>%
  mutate(across(c(p4g16,p4g24b,p4g15,p4g17,p4g18,p4g19,p4g24a,p4g24c,p4g24d,p4g24e,
                  f5k10f,f5k10h,f5k10i,f5k10j,f5k10n,m5k10f,m5k10h,m5k10i,m5k10j,
                  m5k10n,p4g10,p4g14,p4g23f,p4g23h,p4g23i,p4g23j,p4g23n,p4g6,p4g8,
                  p4g9,p5i33a,p5i33c,p5i42a,p5i42c,f5k10c,f5k10d,f5k10g,f5k10k,f5k10m,
                  m5k10c,m5k10d,m5k10g,m5k10k,m5k10m,p4g11,p4g13,p4g23c,p4g23d,p4g23g,
                  p4g23k,p4g23m,p4g3,p4g4,p4g7,p5i33b,p5i42b), 
                ~factor(ifelse(.x == 1 | .x == 2 | .x == 3 | .x == 4 | .x == 5 | .x == 6,1,0)))) %>%
  mutate(across(c(m5b30,f5b24x,m4c37,f4c37), 
                ~factor(ifelse(.x == 1 | .x == 3 ,1,0)))) %>%
  mutate(across(c(m5a4,f5a4), 
                ~factor(ifelse(.x == 2 | .x == 3 ,1,0)))) %>%
  mutate(across(c(m4a4,f4a4), 
                ~factor(ifelse(.x == 3 | .x == 4 ,1,0)))) %>%
  mutate(across(c(p6b22,p6b23,p6b24,p6b31,p6b32), 
                ~factor(ifelse(.x == 1,"1","0")))) %>%
  mutate(across(c(p6b34), 
                ~factor(ifelse(.x == 1 | .x == 2,"1","0")))) %>%
  mutate(across(c(p6b30), 
                ~factor(ifelse(.x >=1,1,"0"))))

table(CombinedData3$Incarceration)
table(CombinedData3$Divorce)
table(CombinedData3$DomesticViolence)
table(CombinedData3$PhysicalAbuse)
table(CombinedData3$EmotionalAbuse)
table(CombinedData3$PhysicalNeglect)
table(CombinedData3$EmotionalNeglect)
table(CombinedData3$PhysicalNeglect)
table(CombinedData3$SexualAbuse)
table(CombinedData3$SubstanceAbuse)
table(CombinedData3$MentalIllness)

table(CombinedData5$p6b34)
CombinedData3 <- CombinedData2 %>%
  mutate(Incarceration = as.integer(rowSums(select(., m5b30, m5d7, f5b24x, m4c37, m4e7, f4c37, f4e7) == 1) > 0)) %>%
  mutate(Divorce = as.integer(rowSums(select(., m5a4,f5a4,m4a4,f4a4) == 1) > 0)) %>%
  mutate(DomesticViolence = as.integer(rowSums(select(.,f4d10,f4d10a,f4d7h,f4d7i,f4d7j,f4d7m,
                                                      f4d7n,f4d9h,f4d9i,f4d9j,f4d9m,f4d9n,
                                                      f5c6h,f5c6i,f5c6j,f5c6m,f5c6n,f5c7,f5c8a,f5d21,
                                                      f5d22,m4d10,m4d10a,m4d7h,m4d7i,m4d7j,m4d7m,
                                                      m4d7n,m4d9h,m4d9i,m4d9j,m4d9m,m4d9n,
                                                      m4e23h,m4e23i,m4e23j,m4e23m,m4e23n,
                                                      m4e23q,m4e24,m5c6h,m5c6i,m5c6j,m5c6m,m5c6n,
                                                      m5c7,m5c8a,m5c8e,m5d20h,m5d20i,m5d20j,m5d20m,
                                                      m5d20n,m5d21,m5d22,m5d22d) == 1) > 0)) %>%
  mutate(PhysicalAbuse = as.integer(rowSums(select(.,f5k10c,f5k10d,f5k10g,f5k10k,f5k10m,
                                                   m5k10c,m5k10d,m5k10g,m5k10k,m5k10m,
                                                   p5i33b,p5i42b) == 1) > 0)) %>%
  mutate(EmotionalAbuse = as.integer(rowSums(select(.,f5k10f,f5k10h,f5k10i,f5k10j,f5k10n,
                                                    m5k10f,m5k10h,m5k10i,m5k10j,m5k10n,
                                                    p5i33a,p5i33c,p5i42a,p5i42c) == 1) > 0)) %>%
  mutate(PhysicalNeglect = as.integer(rowSums(select(.,p4g15,p4g17,p4g18,p4g19,p4g24a,
                                                     p4g24c,p4g24d,p4g24e,p4g16,p4g24b) == 1) > 0)) %>%
  mutate(EmotionalNeglect = as.integer(rowSums(select(.,p4g16,p4g24b) == 1) > 0)) %>%
  mutate(SexualAbuse = as.integer(rowSums(select(.,m5b30,m5d7,f5b24x,m4c37,m4e7,f4c37, f4e7) == 1) > 0)) %>%
  mutate(SubstanceAbuse = as.integer(rowSums(select(.,f4c39,f4e10,f4j20a,f4j21,f5g19a,f5g20,
                                                    f5g21a,f5g21b,f5g21c,f5g21d,f5g21e,f5g21f,
                                                    f5g21g,f5g21h,f5g21i,f5g22,m4c39,
                                                    m4e10,m4j20a,m4j21,m5g19a,m5g20,m5g21a,
                                                    m5g21b,m5g21c,m5g21d,m5g21e,m5g21f,m5g21g,
                                                    m5g21h,m5g21i,m5g22k) == 1) > 0)) %>%
  mutate(MentalIllness = as.integer(rowSums(select(.,cf4md_case_con,cf5md_case_con,
                                                   cm4md_case_con,cm5md_case_con,
                                                   f5g2b1_101,f5g2b1d,f5g2b1e,f5g32,f5g33,
                                                   m5g2b1d,m5g2b1e,n5f2b1d,n5f2b1e) == 1) > 0))

CombinedData4 <- CombinedData3 %>%
  mutate(ACE = rowSums(select(., Incarceration:MentalIllness)))
table(CombinedData4$ACE)
CombinedData4 <- CombinedData4 %>%
  mutate(ACEcat = ifelse(ACE == 1,"1",
                         ifelse(ACE == 2,"2",
                                ifelse(ACE == 3,"3",
                                       ifelse(ACE >=4 & ACE <=11, "4 or more", "0")))))
table(CombinedData4$ACE, CombinedData4$ACEcat)
##Adding GVA Data ----
names(gva15y)


PhysicalAbuseVar <- CombinedData4 %>%
  select(.,f5k10c,f5k10d,f5k10g,f5k10k,f5k10m,
         m5k10c,m5k10d,m5k10g,m5k10k,m5k10m,
         p5i33b,p5i42b)


PhysicalAbuseVar <- PhysicalAbuseVar %>%
  mutate(across(c(f5k10c,f5k10d,f5k10g,f5k10k,f5k10m,
                  m5k10c,m5k10d,m5k10g,m5k10k,m5k10m,
                  p5i33b,p5i42b), 
                ~factor(ifelse(.x == 0 ,1,NA))))

gg<- PhysicalAbuseVar %>% 
  gg_miss_var(show_pct = TRUE) + 
  labs(y = "Percent Yes")
gg
ggsave(filename = "PhysicalAbuseVar of Yes.tiff", width = 8, height = 7,gg, dpi = 800)


EmotionalAbuseVar <- CombinedData4 %>%
  select(.,f5k10f,f5k10h,f5k10i,f5k10j,f5k10n,
         m5k10f,m5k10h,m5k10i,m5k10j,m5k10n,
         p5i33a,p5i33c,p5i42a,p5i42c)


EmotionalAbuseVar <- EmotionalAbuseVar %>%
  mutate(across(c(f5k10f,f5k10h,f5k10i,f5k10j,f5k10n,
                  m5k10f,m5k10h,m5k10i,m5k10j,m5k10n,
                  p5i33a,p5i33c,p5i42a,p5i42c), 
                ~factor(ifelse(.x == 0 ,1,NA))))

gg<- EmotionalAbuseVar %>% 
  gg_miss_var(show_pct = TRUE) + 
  labs(y = "Percent Yes")
gg
ggsave(filename = "EmotionalAbuseVar of Yes.tiff", width = 8, height = 7,gg, dpi = 800)


names(CombinedData4)
ACESubGroups <- CombinedData4 %>%
  select(.,336:345)


ACESubGroups <- ACESubGroups %>%
  mutate(across(c(1:9), 
                ~factor(ifelse(.x == 0 ,1,NA))))

gg<- ACESubGroups %>% 
  gg_miss_var(show_pct = TRUE) + 
  labs(y = "Percent Yes")
gg