#install.packages("dplyr")

require(dplyr)

# multi survey comparison - APIM, CIS, CISPlus, CHS, SHS (seperate file for each source and each year)
# Create a dummy file containing all combination sof levels of each variable and dummy data in the statistics columns
vars <- c("VAR",    
          "CLASS_FLAG",     
          "PROV_RES",       
          "SEX",    
          "AGE",    
          "WITH_VAL_0")
stats <- c("N", "MEAN", "NWGT", "SUM", "P75", "P25", "P_0", "P_10", "P_20", "P_30", "P_40", "P_50", "P_60", "P_70", "P_80", "P_90", "P_100")

# Levels fo each variable
var <- c("ALIP", "CAPGN_TOT", "CHDBN", "CHDBN_CDB", "CHDBN_FED", "CHDBN_PROV", "CHILD_CARE_EXP", "CQPPB", "CQPPB_CONTRIB", 
         "CQPPB_DISAB", "CQPPB_POSTRETIR", "CQPPB_REGRETIR", "CQPPB_RETIR", "CQPPB_SURVIVOR", "EIBEN", "EIBEN_CONTRIB", 
         "EIBEN_OTH", "EIBEN_REG", "EMPIN", "GTR", "INCTAX", "INCTAX_NFED", "INCTAX_PT", "INVST", "INVST_DIV", "INVST_LTPI", 
         "INVST_OTHER", "INVST_RENT", "LINE479", "MEDEX", "MRKINC", "NB_HSTC", "NELG_DIVND", "NTGTR", "OASGIS", "OASGIS_GIS", 
         "OASGIS_OAS", "OGOVTR", "OGOVTR_FRTXCR", "OGOVTR_GHSTC", "OGOVTR_NIE", "OGOVTR_PRSUPP", "OGOVTR_PRTXCR", "OGOVTR_SOCASSIST", 
         "OGOVTR_WITB", "OGOVTR_WRKCOMP", "OTINC", "OTINC_ALIMO", "OTINC_BURSARY", "OTINC_NIE", "OTINC_RDSP", "PEI_HSTC", 
         "PTC_AB_CLAR", "PTC_BC_CATC", "PTC_NL_IS", "PTC_NS_ALTX", "PTC_NS_PRTC", "PTC_ON_EC", "PTC_ON_NEC", "PTC_ON_OC", "PTC_ON_PTC", 
         "PTC_ON_SHPTG", "PTC_ON_TB", "PTC_ON_TB_CLWBCK", "PTC_QC_SOLTC", "PTC_SASK_LITC", "PTC_ccexd", "PTC_line462", "PTC_workcredit", 
         "PTP_BC_MSP", "PTP_NPTXC450", "RETIR", "RETIR_PRIV_PENSION_INC", "RETIR_RRSP_INC", "RPP_CONTRIB", "SEI", "SEI_FARM", 
         "SEI_NONFARM", "TGTR", "TOTINC", "TOTINC_AT", "UNION_DUES", "WAGES", "WAGES_EARN_T4", "WAGES_EXIND", "WAGES_OTHER_EMPT_INC")

class_flag <- c("ALL", "1", "2", "3", "4", "5")

prov_res <- c("ALL", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU")

sex <- c("ALL", "1", "2")

age <- c("All", "0-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65+", "Not Stated")

WITH_VAL_0 <- c("WITH AMNT")

# Create all possible combinations
survey_combinations <- as.data.frame(expand.grid(var, class_flag, prov_res, sex, age, WITH_VAL_0, KEEP.OUT.ATTRS = F))
colnames(survey_combinations) <- vars

# Create a couple years
APIM_tab_2017 <- survey_combinations
APIM_tab_2017$year <- 2017
APIM_tab_2017$FILE <- "APIM"

APIM_tab_2018 <- survey_combinations
APIM_tab_2018$year <- 2018
APIM_tab_2018$FILE <- "APIM"

CHS_tab_2017 <- survey_combinations
CHS_tab_2017$year <- 2017
CHS_tab_2017$FILE <- "CHS"

CHS_tab_2018 <- survey_combinations
CHS_tab_2018$year <- 2018
CHS_tab_2018$FILE <- "CHS"

SHS_tab_2017 <- survey_combinations
SHS_tab_2017$year <- 2017
SHS_tab_2017$FILE <- "SHS"

SHS_tab_2018 <- survey_combinations
SHS_tab_2018$year <- 2018
SHS_tab_2018$FILE <- "SHS"

CIS_tab_2017 <- survey_combinations
CIS_tab_2017$year <- 2017
CIS_tab_2017$FILE <- "CIS"

CIS_tab_2018 <- survey_combinations
CIS_tab_2018$year <- 2018
CIS_tab_2018$FILE <- "CIS"

CISPlus_tab_2017 <- survey_combinations
CISPlus_tab_2017$year <- 2017
CISPlus_tab_2017$FILE <- "CISPlus"

CISPlus_tab_2018 <- survey_combinations
CISPlus_tab_2018$year <- 2018
CISPlus_tab_2018$FILE <- "CISPlus"

# Create columns for statistics
for (i in stats) {
  APIM_tab_2017[i] <- floor(runif(nrow(APIM_tab_2017), max = 1000))
  APIM_tab_2018[i] <- floor(runif(nrow(APIM_tab_2018), max = 1000))

  CHS_tab_2017[i] <- floor(runif(nrow(CHS_tab_2017), max = 1000))
  CHS_tab_2018[i] <- floor(runif(nrow(CHS_tab_2018), max = 1000))
  
  SHS_tab_2017[i] <- floor(runif(nrow(SHS_tab_2017), max = 1000))
  SHS_tab_2018[i] <- floor(runif(nrow(SHS_tab_2018), max = 1000))
  
  CIS_tab_2017[i] <- floor(runif(nrow(CIS_tab_2017), max = 1000))
  CIS_tab_2018[i] <- floor(runif(nrow(CIS_tab_2018), max = 1000))
  
  CISPlus_tab_2017[i] <- floor(runif(nrow(CISPlus_tab_2017), max = 1000))
  CISPlus_tab_2018[i] <- floor(runif(nrow(CISPlus_tab_2018), max = 1000))
}

# Output the dummy files as csv
write.csv(APIM_tab_2017, file = "data/APIM_tab_2017.csv", row.names = F, na = "")
write.csv(APIM_tab_2018, file = "data/APIM_tab_2018.csv", row.names = F, na = "")

write.csv(CHS_tab_2017, file = "data/CHS_tab_2017.csv", row.names = F, na = "")
write.csv(CHS_tab_2018, file = "data/CHS_tab_2018.csv", row.names = F, na = "")

write.csv(SHS_tab_2017, file = "data/SHS_tab_2017.csv", row.names = F, na = "")
write.csv(SHS_tab_2018, file = "data/SHS_tab_2018.csv", row.names = F, na = "")

write.csv(CIS_tab_2017, file = "data/CIS_tab_2017.csv", row.names = F, na = "")
write.csv(CIS_tab_2018, file = "data/CIS_tab_2018.csv", row.names = F, na = "")

write.csv(CISPlus_tab_2017, file = "data/CISPlus_tab_2017.csv", row.names = F, na = "")
write.csv(CISPlus_tab_2018, file = "data/CISPlus_tab_2018.csv", row.names = F, na = "")


# Create dummy percentile file
percentiles <- c("P_0", "P_02", "P_04", "P_06", "P_08", "P_10", "P_12", "P_14", "P_16", "P_18", "P_20", "P_22",
                 "P_24", "P_26", "P_28", "P_30", "P_32", "P_34", "P_36", "P_38", "P_40", "P_42", "P_44", "P_46",
                 "P_48", "P_50", "P_52", "P_54", "P_56", "P_58", "P_60", "P_62", "P_64", "P_66", "P_68", "P_70",
                 "P_72", "P_74", "P_76", "P_78", "P_80", "P_82", "P_84", "P_86", "P_88", "P_90", "P_92", "P_94",
                 "P_96", "P_98", "P_100")

#FILE <- c("APIM", "CIS", "CISPlus", "CHS", "SHS")
percentile_vars <- c("VAR", "CLASS_FLAG", "PROV_RES", "SEX", "AGE", "WITH_VAL_0")

percentiles_combinations <- as.data.frame(expand.grid(var, class_flag, prov_res, sex, age, WITH_VAL_0, KEEP.OUT.ATTRS = F))
colnames(percentiles_combinations) <- percentile_vars

APIM_percentiles_2017 <- percentiles_combinations
APIM_percentiles_2017$year <- 2017
APIM_percentiles_2017$FILE <- "APIM"

APIM_percentiles_2018 <- percentiles_combinations
APIM_percentiles_2018$year <- 2018
APIM_percentiles_2018$FILE <- "APIM"

CIS_percentiles_2017 <- percentiles_combinations
CIS_percentiles_2017$year <- 2017
CIS_percentiles_2017$FILE <- "CIS"

CIS_percentiles_2018 <- percentiles_combinations
CIS_percentiles_2018$year <- 2018
CIS_percentiles_2018$FILE <- "CIS"

CISPlus_percentiles_2017 <- percentiles_combinations
CISPlus_percentiles_2017$year <- 2017
CISPlus_percentiles_2017$FILE <- "CISPlus"

CISPlus_percentiles_2018 <- percentiles_combinations
CISPlus_percentiles_2018$year <- 2018
CISPlus_percentiles_2018$FILE <- "CISPlus"

CHS_percentiles_2017 <- percentiles_combinations
CHS_percentiles_2017$year <- 2017
CHS_percentiles_2017$FILE <- "CHS"

CHS_percentiles_2018 <- percentiles_combinations
CHS_percentiles_2018$year <- 2018
CHS_percentiles_2018$FILE <- "CHS"

SHS_percentiles_2017 <- percentiles_combinations
SHS_percentiles_2017$year <- 2017
SHS_percentiles_2017$FILE <- "SHS"

SHS_percentiles_2018 <- percentiles_combinations
SHS_percentiles_2018$year <- 2018
SHS_percentiles_2018$FILE <- "SHS"

for (i in 1:51) {
  APIM_percentiles_2017[percentiles[i]] <- runif(n=nrow(APIM_percentiles_2017), min = 0.75, max = 1.5)*(i^2)
  APIM_percentiles_2018[percentiles[i]] <- runif(n=nrow(APIM_percentiles_2018), min = 0.75, max = 1.5)*(i^2)
  
  CIS_percentiles_2017[percentiles[i]] <- runif(n=nrow(CIS_percentiles_2017), min = 0.75, max = 1.5)*(i^2)
  CIS_percentiles_2018[percentiles[i]] <- runif(n=nrow(CIS_percentiles_2018), min = 0.75, max = 1.5)*(i^2)
  
  CISPlus_percentiles_2017[percentiles[i]] <- runif(n=nrow(CISPlus_percentiles_2017), min = 0.75, max = 1.5)*(i^2)
  CISPlus_percentiles_2018[percentiles[i]] <- runif(n=nrow(CISPlus_percentiles_2018), min = 0.75, max = 1.5)*(i^2)
   
  CHS_percentiles_2017[percentiles[i]] <- runif(n=nrow(CHS_percentiles_2017), min = 0.75, max = 1.5)*(i^2)
  CHS_percentiles_2018[percentiles[i]] <- runif(n=nrow(CHS_percentiles_2018), min = 0.75, max = 1.5)*(i^2)
  
  SHS_percentiles_2017[percentiles[i]] <- runif(n=nrow(SHS_percentiles_2017), min = 0.75, max = 1.5)*(i^2)
  SHS_percentiles_2018[percentiles[i]] <- runif(n=nrow(SHS_percentiles_2018), min = 0.75, max = 1.5)*(i^2)
}

# Output the dummy files as csv
write.csv(APIM_percentiles_2017, file = "data/APIM_percentiles_2017.csv", row.names = F, na = "")
write.csv(APIM_percentiles_2018, file = "data/APIM_percentiles_2018.csv", row.names = F, na = "")

write.csv(CIS_percentiles_2017, file = "data/CIS_percentiles_2017.csv", row.names = F, na = "")
write.csv(CIS_percentiles_2018, file = "data/CIS_percentiles_2018.csv", row.names = F, na = "")

write.csv(CISPlus_percentiles_2017, file = "data/CISPlus_percentiles_2017.csv", row.names = F, na = "")
write.csv(CISPlus_percentiles_2018, file = "data/CISPlus_percentiles_2018.csv", row.names = F, na = "")

write.csv(CHS_percentiles_2017, file = "data/CHS_percentiles_2017.csv", row.names = F, na = "")
write.csv(CHS_percentiles_2018, file = "data/CHS_percentiles_2018.csv", row.names = F, na = "")

write.csv(SHS_percentiles_2017, file = "data/SHS_percentiles_2017.csv", row.names = F, na = "")
write.csv(SHS_percentiles_2018, file = "data/SHS_percentiles_2018.csv", row.names = F, na = "")
