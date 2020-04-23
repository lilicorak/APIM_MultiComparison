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
APIM_tab_2017$file <- "APIM"

APIM_tab_2018 <- survey_combinations
APIM_tab_2018$year <- 2018
APIM_tab_2018$file <- "APIM"

CHS_tab_2017 <- survey_combinations
CHS_tab_2017$year <- 2017
CHS_tab_2017$file <- "CHS"

CHS_tab_2018 <- survey_combinations
CHS_tab_2018$year <- 2018
CHS_tab_2018$file <- "CHS"

SHS_tab_2017 <- survey_combinations
SHS_tab_2017$year <- 2017
SHS_tab_2017$file <- "SHS"

SHS_tab_2018 <- survey_combinations
SHS_tab_2018$year <- 2018
SHS_tab_2018$file <- "SHS"

CIS_tab_2017 <- survey_combinations
CIS_tab_2017$year <- 2017
CIS_tab_2017$file <- "CIS"

CIS_tab_2018 <- survey_combinations
CIS_tab_2018$year <- 2018
CIS_tab_2018$file <- "CIS"

CISPlus_tab_2017 <- survey_combinations
CISPlus_tab_2017$year <- 2017
CISPlus_tab_2017$file <- "CISPlus"

CISPlus_tab_2018 <- survey_combinations
CISPlus_tab_2018$year <- 2018
CISPlus_tab_2018$file <- "CISPlus"

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


# multi source comparison - CIS, T1FF, APIM
# # Create a dummy file containing all combinations of levels of each variable and dummy data in the statistics columns
# vars <- c("source", "variable", "population", "prov", "decile_flag", "AGE", "sex")
# stats <- c("n",	"min",	"max",	"sum",	"p5",	"p10",	"p25",	"p50",	"p75",	"p90",	"p95",	"p99")
# 
# # Levels of each variable
# source <- c("CIS", "apim", "t1ff")
# 
# variable	<- c("CHDBN", "CHDBN_BCCTB", "CHDBN_CCTB", "CHDBN_CDB", "CHDBN_FED", "CHDBN_NCBS",
#               "CHDBN_PROV", "CHDBN_UCCB", "CQPPB", "CQPPB_DISAB", "CQPPB_POSTRETIR", "CQPPB_REGRETIR",
#               "CQPPB_RETIR", "CQPPB_SURVIVOR", "EIBEN", "EIBEN_OTH", "EIBEN_REG", "ELG_DIVND", "EMPIN",
#               "GTR", "INCTAX", "INCTAX_NFED", "INCTAX_PT", "INVST", "INVST_DIV", "INVST_LTPI", "INVST_OTHER",
#               "INVST_RENT", "MRKINC", "NELG_DIVND", "OASGIS", "OASGIS_GIS", "OASGIS_OAS", "OASGIS_REPAY",
#               "OGOVTR", "OGOVTR_GHSTC", "OGOVTR_NIE", "OGOVTR_PRSUPP", "OGOVTR_PRTXCR", "OGOVTR_SOCASSIST",
#               "OGOVTR_WITB", "OGOVTR_WRKCOMP", "OTINC", "OTINC_ALIMO", "OTINC_BURSARY", "OTINC_NIE", "OTINC_RDSP",
#               "RETIR", "RETIR_PRIV_PENSION_INC", "RETIR_RRSP_INC", "RRSP_TRANSFER", "SEI", "SEI_BUS", "SEI_COM",
#               "SEI_EXIND", "SEI_FARM", "SEI_FISH", "SEI_NONFARM", "SEI_PRF", "TAXABLE_ALIMONY_T1", "TAXABLE_DIVND",
#               "TOTAL_ALIMONY_T1", "TOTINC", "TOTINC_AT", "WAGES", "WAGES_EARN_T4", "WAGES_EXIND", "WAGES_OTHER_EMPT_INC")
# 
# population <- c("all", "recipient")	
# 
# prov <- c("", "93", "CANADA", "10 NL", "11 PE", "12 NS", "13 NB", "24 QC", "35 ON",
#           "46 MB", "47 SK", "48 AB", "59 BC", "60 YT", "61 NT", "62 NU", "Don't Know", "Interim Processing Code", "Not Applicable")
# 
# decile_flag <- c("", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
# 
# AGE	<- c("", "0-14", "15+", "15-19", "15-24", "20-24", "25-34", "25-54", "25-64",
#          "35-44", "45-54", "55-59", "55-64", "60-64", "65+", "65-69", "70-74", "75+")
# 
# sex <- c("", "1", "2", "99")
# 
# # Create all possible combinations
# multicomp_combinations <- as.data.frame(expand.grid(source, variable, population, prov, decile_flag, AGE, sex, 
#                                                     KEEP.OUT.ATTRS = F))
# colnames(multicomp_combinations) <- vars
# 
# # Create a couple years
# multicomp_dummy_2017 <- multicomp_combinations
# multicomp_dummy_2017$year <- 2017
# 
# multicomp_dummy_2016 <- multicomp_combinations
# multicomp_dummy_2016$year <- 2016
# 
# # Create columns for statistics
# for (i in stats) {
#   multicomp_dummy_2017[i] <- floor(runif(nrow(multicomp_dummy_2017), max = 1000))
#   multicomp_dummy_2016[i] <- floor(runif(nrow(multicomp_dummy_2016), max = 1000))
# }
# 
# # Output the dummy files as csv
# write.csv(multicomp_dummy_2016, file = "data/multicomp_dummy_2016.csv", row.names = F, na = "")
# write.csv(multicomp_dummy_2017, file = "data/multicomp_dummy_2017.csv", row.names = F, na = "")



