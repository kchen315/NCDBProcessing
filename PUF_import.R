#'Clean PUF
#'
#'Function that given a .dat PUF file, returns the cleaned object. This code is adapted from the STATA/SPSS/SAS
#'code that comes with a PUF file from the NCDB. PUF Files come from the NCDB as .dat files which are fixed-length 
#'columns with rows delimited by tab. This function is essentially the same as the SPSS/SAS/STATA code that comes 
#'with the PUF files from the NCDB. This function converts the 2016 NCDB .dat file into an R data fram with 
#'136 variables.
#'
#'Note that this will not work on earlier or later versions of the PUF because the column numbers
#'and positions may be different.
#'
#'Author:@MCWardMD
#'
#' @param puf_filePath Path to the .dat file that came from the NCDB
#' @return Data frame with the appropriate labels and variable names applied
#' @example 
#' new_puf<-clean_puf("root/NCDBProject/")
#' summary(new_puf)

clean_puf<-function(puf_filePath){

puf<-data.frame()

#note there are a few continuous variables with values that need lables - R has a hard
#time handling this

#calc widths based on column ranges provided in exmaple scripts
colLimits<-c(0,37,47,48,49,52,53,55,56,58,59,60,61,62,63,64,72,74,
       76,78,82,86,87,91,92,93,94,97,99,101,109,111,116,
       121,126,130,135,140,145,149,151,152,154,155,158,159,160,161,
       162,163,164,167,170,173,176,179,182,185,188,191,194,197,
       200,203,206,209,212,215,218,221,224,227,230,233,236,239,
       245,253,261,269,271,272,273,274,275,283,284,285,293,294,
       295,297,299,304,306,311,314,315,318,319,327,335,337,345,
       347,355,357,359,360,368,369,370,371,372,373,381,382,384,
       386,388,390,392,393,394,395,397,398,401,402,403,404,405,
       406,407,408,409,410)

delta<-vector(mode="integer",length=136)

for(i in 2:137)
{
  delta[i]<-colLimits[i]-colLimits[i-1]
}

puf_colNames<-c(
  "PUF_CASE_ID",
  "PUF_FACILITY_ID",
  "FACILITY_TYPE_CD",
  "FACILITY_LOCATION_CD",
  "AGE",
  "SEX",
  "RACE",
  "SPANISH_HISPANIC_ORIGIN",
  "INSURANCE_STATUS",
  "MED_INC_QUAR_00",
  "NO_HSD_QUAR_00",
  "UR_CD_03",
  "MED_INC_QUAR_12",
  "NO_HSD_QUAR_12",
  "UR_CD_13",
  "CROWFLY",
  "CDCC_TOTAL_BEST",
  "SEQUENCE_NUMBER",
  "CLASS_OF_CASE",
  "YEAR_OF_DIAGNOSIS",
  "PRIMARY_SITE",
  "LATERALITY",
  "HISTOLOGY",
  "BEHAVIOR",
  "GRADE",
  "DIAGNOSTIC_CONFIRMATION",
  "TUMOR_SIZE",
  "REGIONAL_NODES_POSITIVE",
  "REGIONAL_NODES_EXAMINED",
  "DX_STAGING_PROC_DAYS",
  "RX_SUMM_DXSTG_PROC",
  "TNM_CLIN_T",
  "TNM_CLIN_N",
  "TNM_CLIN_M",
  "TNM_CLIN_STAGE_GROUP",
  "TNM_PATH_T",
  "TNM_PATH_N",
  "TNM_PATH_M",
  "TNM_PATH_STAGE_GROUP",
  "TNM_EDITION_NUMBER",
  "ANALYTIC_STAGE_GROUP",
  "CS_METS_AT_DX",
  "CS_METS_EVAL",
  "CS_EXTENSION",
  "CS_TUMOR_SIZEEXT_EVAL",
  "CS_METS_DX_BONE",
  "CS_METS_DX_BRAIN",
  "CS_METS_DX_LIVER",
  "CS_METS_DX_LUNG",
  "LYMPH_VASCULAR_INVASION",
  "CS_SITESPECIFIC_FACTOR_1",
  "CS_SITESPECIFIC_FACTOR_2",
  "CS_SITESPECIFIC_FACTOR_3",
  "CS_SITESPECIFIC_FACTOR_4",
  "CS_SITESPECIFIC_FACTOR_5",
  "CS_SITESPECIFIC_FACTOR_6",
  "CS_SITESPECIFIC_FACTOR_7",
  "CS_SITESPECIFIC_FACTOR_8",
  "CS_SITESPECIFIC_FACTOR_9",
  "CS_SITESPECIFIC_FACTOR_10",
  "CS_SITESPECIFIC_FACTOR_11",
  "CS_SITESPECIFIC_FACTOR_12",
  "CS_SITESPECIFIC_FACTOR_13",
  "CS_SITESPECIFIC_FACTOR_14",
  "CS_SITESPECIFIC_FACTOR_15",
  "CS_SITESPECIFIC_FACTOR_16",
  "CS_SITESPECIFIC_FACTOR_17",
  "CS_SITESPECIFIC_FACTOR_18",
  "CS_SITESPECIFIC_FACTOR_19",
  "CS_SITESPECIFIC_FACTOR_20",
  "CS_SITESPECIFIC_FACTOR_21",
  "CS_SITESPECIFIC_FACTOR_22",
  "CS_SITESPECIFIC_FACTOR_23",
  "CS_SITESPECIFIC_FACTOR_24",
  "CS_SITESPECIFIC_FACTOR_25",
  "CS_VERSION_LATEST",
  "DX_RX_STARTED_DAYS",
  "DX_SURG_STARTED_DAYS",
  "DX_DEFSURG_STARTED_DAYS",
  "RX_SUMM_SURG_PRIM_SITE",
  "RX_HOSP_SURG_APPR_2010",
  "RX_SUMM_SURGICAL_MARGINS",
  "RX_SUMM_SCOPE_REG_LN_SUR",
  "RX_SUMM_SURG_OTH_REGDIS",
  "SURG_DISCHARGE_DAYS",
  "READM_HOSP_30_DAYS",
  "REASON_FOR_NO_SURGERY",
  "DX_RAD_STARTED_DAYS",
  "RX_SUMM_RADIATION",
  "RAD_LOCATION_OF_RX",
  "RAD_TREAT_VOL",
  "RAD_REGIONAL_RX_MODALITY",
  "RAD_REGIONAL_DOSE_CGY",
  "RAD_BOOST_RX_MODALITY",
  "RAD_BOOST_DOSE_CGY",
  "RAD_NUM_TREAT_VOL",
  "RX_SUMM_SURGRAD_SEQ",
  "RAD_ELAPSED_RX_DAYS",
  "REASON_FOR_NO_RADIATION",
  "DX_SYSTEMIC_STARTED_DAYS",
  "DX_CHEMO_STARTED_DAYS",
  "RX_SUMM_CHEMO",
  "DX_HORMONE_STARTED_DAYS",
  "RX_SUMM_HORMONE",
  "DX_IMMUNO_STARTED_DAYS",
  "RX_SUMM_IMMUNOTHERAPY",
  "RX_SUMM_TRNSPLNT_ENDO",
  "RX_SUMM_SYSTEMIC_SUR_SEQ",
  "DX_OTHER_STARTED_DAYS",
  "RX_SUMM_OTHER",
  "PALLIATIVE_CARE",
  "RX_SUMM_TREATMENT_STATUS",
  "PUF_30_DAY_MORT_CD",
  "PUF_90_DAY_MORT_CD",
  "DX_LASTCONTACT_DEATH_MONTHS",
  "PUF_VITAL_STATUS",
  "RX_HOSP_SURG_PRIM_SITE",
  "RX_HOSP_CHEMO",
  "RX_HOSP_IMMUNOTHERAPY",
  "RX_HOSP_HORMONE",
  "RX_HOSP_OTHER",
  "PUF_MULT_SOURCE",
  "REFERENCE_DATE_FLAG",
  "RX_SUMM_SCOPE_REG_LN_2012",
  "RX_HOSP_DXSTG_PROC",
  "PALLIATIVE_CARE_HOSP",
  "TUMOR_SIZE_SUMMARY",
  "METS_AT_DX_OTHER",
  "METS_AT_DX_DISTANT_LN",
  "METS_AT_DX_BONE",
  "METS_AT_DX_BRAIN",
  "METS_AT_DX_LIVER",
  "METS_AT_DX_LUNG",
  "NO_HSD_QUAR_16",
  "MED_INC_QUAR_16",
  "MEDICAID_EXPN_CODE"
)

#specifying classes should help large files run faster
puf_colClasses<-c(
  "character",
  "character",
  "integer",
  "integer",
  "integer", #AGE
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "numeric", #CROWFLY
  "integer",
  "factor",
  "integer",
  "integer",
  "character", #PRIMARY_STIE
  "integer",
  "character",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "numeric", #DX_STAGING_PROC_DAYS
  "integer",
  "character",
  "character",
  "character",
  "character",
  "character",
  "character",
  "character",
  "character",  #TNM_PATH_STAGE_GROUP
  "integer",
  "integer",
  "integer",
  "character",  #CS_METS_EVAL
  "character",
  "character",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer", #CS_SITESPECIFIC_FACTOR_1
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer", #CS_SITESPECIFIC_FACTOR_25
  "factor",
  "numeric",   ##DX_RX_STARTED_DAYS
  "numeric",
  "numeric",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "numeric", #SURG_DISCHARGE_DAYS
  "integer",
  "integer",
  "numeric",
  "integer",
  "integer",
  "integer",
  "integer",
  "numeric",
  "integer",
  "numeric",
  "integer", #RAD_NUM_TREAT_VOL
  "integer",
  "integer",
  "integer",
  "numeric",
  "numeric",
  "integer",
  "numeric",
  "integer",
  "numeric", #DX_IMMUNO_STARTED_DAYS
  "integer",
  "integer",
  "integer",
  "numeric",
  "integer",
  "integer",
  "integer", #RX_SUMM_TREATMENT_STATUS
  "integer",
  "integer",
  "numeric", #DX_LASTCONTACT_DEATH_MONTHS
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer"
)

##read the file with the widths and classes given above
puf<-read.fwf(puf_filePath,
              widths=delta[2:137],
              header=FALSE,
              col.names=puf_colNames,
              colClasses=puf_colClasses,
              sep="\t",
              na.strings=c(" ","  ","   "))


#############
#Set labels
#############
puf$FACILITY_TYPE_CD<-factor(puf$FACILITY_TYPE_CD,
                         levels=c(1,2,3,4,9),
                         labels=c("Community Cancer Program",
                                  "Comprehensive Community Cancer Program",
                                  "Academic/Research Program",
                                  "Integrated Network Cancer Program",
                                  "Other specified types of cancer programs"))

puf$FACILITY_LOCATION_CD<-factor(puf$FACILITY_LOCATION_CD,
                                 levels=c(1,2,3,4,5,6,7,8,9,0),
                                 labels=c("New England",
                                          "Middle Atlantic",
                                          "South Atlantic",
                                          "East North Central",
                                          "East South Central", 
                                          "West North Central",
                                          "West South Central",
                                          "Mountain",
                                          "Pacific", 
                                          "Out of US"))

puf$AGE[puf$AGE==999]<-NA

puf$RACE<-factor(puf$RACE,
                 levels=c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,
                          17,20,21,22,25,26,27,28,30,31,32,96,97,98,99),
                 labels=c("White",
                          "Black",
                          "American Indian, Aleutian, or Eskimo",
                          "Chinese",
                          "Japanese",
                          "Filipino", 
                          "Hawaiian",
                          "Korean",
                          "Vietnamese",
                          "Laotian",
                          "Hmong",
                          "Kampuchean",
                          "Thai",
                          "Asian Indian or Pakistani, NOS",
                          "Asian Indian",
                          "Pakistani",
                          "Micronesian, NOS",
                          "Chamorran",
                          "Guamanian, NOS",
                          "Polynesian, NOS",
                          "Tahitian",
                          "Samoan",
                          "Tongan",
                          "Melanesian, NOS",
                          "Fiji Islander",
                          "New Guinean",
                          "Other Asian including Asian, NOS and Oriental, NOS",
                          "Pacific Islander NOS",
                          "Other",
                          "Unknown"))

puf$SEX<-factor(puf$SEX,
                 levels=c(1,2,9),
                 labels=c("Male","Female","Not stated in patient record"))

puf$INSURANCE_STATUS<-factor(puf$INSURANCE_STATUS,
                             levels=c(0,1,2,3,4,9),
                             labels=c(
                              "Not Insured",
                              "Private Insurance/Managed Care",
                              "Medicaid",
                              "Medicare",
                              "Other Government",
                              "Insurance Status Unknown"
                             ))


puf$SPANISH_HISPANIC_ORIGIN<-factor(puf$SPANISH_HISPANIC_ORIGIN,
      levels=c(0,1,2,3,4,5,6,7,8,9),
      labels=c("Non-Spanish; non-Hispanic",
               "Mexican/Chicano",
               "Puerto Rican",
               "Cuban",
               "South or Central America",
               "Other specified Spanish/Hispanic origin",
               "Spanish, NOS Hispanic, NOS Latino, NOS",
               "Spanish surname only",
               "Dominican Republic",
               "Unknown"))

puf$MED_INC_QUAR_00<-factor(puf$MED_INC_QUAR_00,
                    levels=c(1,2,3,4),
                    labels=c("< $30,000",
                             "$30,000 - $34,999",
                             "$35,000 - $45,999",
                             ">=$46,000"))

puf$NO_HSD_QUAR_00<-factor(puf$NO_HSD_QUAR_00,
                           levels=c(1,2,3,4,9),
                           labels=c(">=29%",
                                    "20-28.9%",
                                    "14-19.9%",
                                    "<14%", 
                                    "Not Available" ))

puf$UR_CD_03<-factor(puf$UR_CD_03,
                     levels=c(1,2,3,4,5,6,7,8,9),
                     labels=c("Counties in metro areas of 1 million population or more",
                              "Counties in metro areas of 250,000 to 1 million population",
                              "Counties in metro areas of fewer than 250,000 population",
                              "Urban population of 20,000 or more adjacent to a metro area",
                              "Urban population of 20,000 or more not adjacent to a metro area",
                              "Urban population of 2,500 to 19,999, adjacent to a metro area",
                              "Urban population of 2,500 to 19,999, not adjacent to a metro area",
                              "Completely rural or less than 2,500 urban population, adjacent to a metro area",
                              "Completely rural or less than 2,500 urban population, not adjacent to a metro area"))

puf$MED_INC_QUAR_12<-factor(puf$MED_INC_QUAR_12,
                            levels=c(1,2,3,4),
                            labels=c("< $38,000",
                                     "$38,000-$47,999",
                                     "$48,000-$62,999",
                                     ">=$63,000"))

puf$NO_HSD_QUAR_12<-factor(puf$NO_HSD_QUAR_12,
                           levels=c(1,2,3,4),
                           labels=c(">=21.0%",
                                    "13.0-20.9%",
                                    "7.0-12.9%",
                                    "<7.0%"))

puf$CDCC_TOTAL_BEST<-factor(puf$CDCC_TOTAL_BEST,levels=c(0,1,2,3),labels=c(
  "0",
  "1",
  "2",
  ">=3"))

puf$UR_CD_13<-factor(puf$UR_CD_13,
                     levels=c(1,2,3,4,5,6,7,8,9),
                     labels=c(
  "Counties in metro areas of 1 million population or more",
  "Counties in metro areas of 250,000 to 1 million population",
  "Counties in metro areas of fewer than 250,000 population",
  "Urban population of 20,000 or more adjacent to a metro area",
  "Urban population of 20,000 or more not adjacent to a metro area",
  "Urban population of 2,500 to 19,999, adjacent to a metro area",
  "Urban population of 2,500 to 19,999, not adjacent to a metro area",
  "Completely rural or less than 2,500 urban population, adjacent to a metro area",
  "Completely rural or less than 2,500 urban population, not adjacent to a metro area"))

puf$CLASS_OF_CASE<-factor(puf$CLASS_OF_CASE,
                          levels=c(0,10,11,12,13,14,20,21,22),
                          labels=c(
  "Dx at reporting facility, all treatment decisions done elsewhere",
  "Dx and part or all of 1st course treatment or a decision not to treat was at the reporting facility, NOS",
  "Dx in staff physician's office and part of 1st course treatment at the reporting facility",
  "Dx in a staff physician's office and all of 1st course treatment at the reporting facility",
  "Dx and part of first course Rx at reporting facility, part of first course treatment elsewhere",
  "Dx at reporting facility and all first course treatment at the reporting facility",
  "Dx elsewhere and all or part of 1st course treatment or a decision not to treat at the reporting facility, NOS",
  "Dx elsewhere and part of 1st course treatment at the reporting facility part of first course treatment elsewhere",
  "Dx elsewhere and all of first course treatment or a decision not to treat at the reporting facility"))

puf$LATERALITY<-factor(puf$LATERALITY,
                       levels=c(0,1,2,3,4,5,9),
                       labels=c(
  "Organ is not considered to be a paired site",
  "Origin of primary is right",
  "Origin of primary is left",
  "Only one side involved, right or left origin not specified",
  "Bilateral involvement",
  "Paired site midline tumor",
  "Paired site, but lateral origin unknown, midline tumor"))

puf$BEHAVIOR<-factor(puf$BEHAVIOR,levels=c(0,1,2,3),
                     labels=c(
  "Benign",
  "Borderline",
  "In situ and/or carcinoma in situ",
  "Invasive"))

puf$GRADE<-factor(puf$GRADE,levels=c(1,2,3,4,5,6,7,8,9),labels=c(
  "Well differentiated, differentiated, NOS",
  "Moderately differentiated, moderately well differentiated, intermediate differentiation",
  "Poorly differentiated",
  "Undifferentiated, anaplastic",
  "T cell T-precursor",
  "B cell, pre-B, B-precursor",
  "Null cell non T-non B",
  "NK (natural killer) cell",
  "Cell type not determined, not stated or not applicable, unknown primaries, high grade dysplasia"))


#if this is put into a factor, the continuous nature may be lost.
# puf$TUMOR_SIZE<-factor(puf$TUMOR_SIZE,
#                        levels=c(0,989,990,991,992,993,994,995,998,999),
#                        labels=c(
#                          "No mass or tumor found",
#                          "989 millimeters or larger",
#                          "Microscopic focus or foci only",
#                           "< 1 cm",
#                           "> 1 cm, < 2 cm",
#                           "> 2 cm, < 3 cm",
#                           "> 3 cm, < 4 cm",
#                           "> 4 cm, < 5 cm",
#                           "Tumor involvement of specified primaries",
#                           "Unknown, size not stated"))
puf$TUMOR_SIZE[puf$TUMOR_SIZE==999]<-NA

#if this is put into a factor, the continuous nature may be lost.
puf$REGIONAL_NODES_POSITIVE<-factor(puf$REGIONAL_NODES_POSITIVE,
                                    levels=c(0,90,95,97,98,99),
                                    labels=c(
  "All nodes examined are negative",
  "90 or more nodes are positive",
  "Positive aspiration of lymph node(s) was performed",
  "Positive nodes are documented, but the number is unspecified",
  "No nodes were examined",
  "Unknown whether nodes are positive, not applicable, not stated in patient record"))
#puf$REGIONAL_NODES_POSITIVE[puf$REGIONAL_NODES_POSITIVE==99]<-NA

#if this is put into a factor, the continuous nature may be lost.
puf$REGIONAL_NODES_EXAMINED<-factor(puf$REGIONAL_NODES_EXAMINED,
                                    levels=c(0,90,95,96,97,98,99),
                                    labels=c(
  "No nodes were examined",
  "90 or more nodes were examined",
  "No regional nodes were removed, but aspiration of regional nodes was performed",
  "Regional lymph node removal was documented as a sampling, and the number of nodes is unknown/not stated",
  "Regional lymph node removal was documented as a dissection, and the number of nodes is unknown/not stated",
  "Regional lymph nodes surgically removed but number not documented, not documented as sampling or dissection",
  "Unknown if regional nodes examined, not applicable for this site-histology combination"))

puf$RX_SUMM_DXSTG_PROC<-factor(puf$RX_SUMM_DXSTG_PROC,
                               levels=c(0,1,2,3,4,5,6,7,9),
                               labels=c(
  "No surgical diagnostic or staging procedure was performed",
  "A biopsy was done to a site other than the primary",
  "A biopsy (incisional, needle, or aspiration) was done to the primary site",
  "A surgical exploration only",
  "A surgical procedure with a bypass was performed, but no biopsy was done",
  "An exploratory procedure was performed, and a biopsy of either the primary site or another site was done",
  "A bypass procedure was performed, and a biopsy of either the primary site or another site was done",
  "A procedure was done, but the type of procedure is unknown",
  "No information of whether a diagnostic or staging procedure was performed"))

puf$TNM_EDITION_NUMBER<-factor(puf$TNM_EDITION_NUMBER,
                               levels=c(0,5,6,7,88,99),
                               labels=c(
  "Not staged(cases that have AJCC staging scheme and staging was not done)",
  "Fifth Edition",
  "Sixth Edition",
  "Seventh Edition",
  "Not applicable (cases that do not have an AJCC staging scheme)",
  "Staged, but the edition is unknown, Prior to the 5th edition"
))

puf$ANALYTIC_STAGE_GROUP<-factor(puf$ANALYTIC_STAGE_GROUP,
                                 levels=c(0,1,2,3,4,5,8,9),
                                 labels=c(
  "Stage 0",
  "Stage I",
  "Stage II",
  "Stage III",
  "Stage IV",
  "Occult (lung only)",
  "AJCC Staging not applicable",
  "AJCC Stage Group unknown"))

#SEE COLLABORATIVE STAGE MANUAL for PUF Data Items 
#CS_METS_EVAL, CS_EXTENSION, CS_TUMOR_SIZEEXT_EVAL
  
puf$CS_METS_AT_DX<-factor(puf$CS_METS_AT_DX,
                            levels=c(00,10,40,50,99),
                            labels=c(
  "None",
  "Distant Lymph Nodes",
  "Distant Mets except distant Lymph Nodes",
  "Distant Lymph Nodes and Distant Mets",
  "Unknown"))

puf$CS_METS_DX_BONE<-factor(puf$CS_METS_DX_BONE,
                            levels=c(0,1,8,9),
                            labels=c(
  "None",
  "Yes",
  "Not Applicable",
  "Unknown"))

puf$CS_METS_DX_BRAIN<-factor(puf$CS_METS_DX_BRAIN,
                             levels=c(0,1,8,9),
                             labels=c(
  "None",
  "Yes",
  "Not Applicable",
  "Unknown"))

puf$CS_METS_DX_LIVER<-factor(puf$CS_METS_DX_LIVER,
                             levels=c(0,1,8,9),
                             labels=c(
  "None",
  "Yes",
  "Not Applicable",
  "Unknown"))

puf$CS_METS_DX_LUNG<-factor(puf$CS_METS_DX_LUNG,
                            levels=c(0,1,8,9),
                            labels=c(
  "None",
  "Yes",
  "Not Applicable",
  "Unknown"))

puf$LYMPH_VASCULAR_INVASION<-factor(puf$LYMPH_VASCULAR_INVASION,
                                    levels=c(0,1,8,9),
                                    labels=c(
  "Not present",
  "Present",
  "Not applicable",
  "Unknown"))

puf$RX_HOSP_SURG_APPR_2010<-factor(puf$RX_HOSP_SURG_APPR_2010,
                                   levels=c(0,1,2,3,4,5,9),
                                   labels=c(
  "No surgical procedure of primary site",
  "Robotic assisted",
  "Robotic converted to open",
  "Laparoscopic",
  "Laparoscopic converted to open",
  "Open or approach unspecified",
  "Unknown if surgery performed"))

puf$RX_SUMM_SURGICAL_MARGINS<-factor(puf$RX_SUMM_SURGICAL_MARGINS,
                                     levels=c(0,1,2,3,7,8,9),
                                     labels=c(
  "No residual tumor All margins are grossly and microscopically negative",
  "Residual tumor, NOS  Involvement is indicated, but not otherwise specified",
  "Microscopic residual tumor Cannot be seen by the naked eye",
  "Macroscopic residual tumor, Gross tumor of the primary site which is visible to the naked eye",
  "Margins not evaluable, Cannot be assessed (indeterminate)",
  "No primary site surgery",
  "Unknown or not applicable"))

# Scope of Regional Lymph Node Surgery:
#   Sentinel Lymph Nodes:  Data on Scope of Regional Lymph Node Surgery have been found to under-report
# Sentinel Lymph Node Biopsy. Revised coding rules and associated instructions were implemented
# for cases diagnosed January 1, 2012 and later, CoC use of the item
# ìScope of Regional Lymph Node Surgeryî diagnosed in 2011 or earlier is curtailed in all CoC data.
# The item is used only to identify whether or not a patient underwent regional lymph node surgery,
# effectively removing any distinction between the type or extent of surgical intervention.
# For breast and skin (melanoma, Merkel cell carcinoma) cancer cases diagnosed prior to 2012,
# codes for this item are limited to 0, 1 and 9.

puf$RX_SUMM_SCOPE_REG_LN_SUR<-factor(puf$RX_SUMM_SCOPE_REG_LN_SUR,
                                       levels=c(0,1,9),
                                       labels=c(
  "No regional lymph node surgery",
  "Regional lymph node surgery",
  "Unknown if there was any regional lymph node surgery"))

puf$RX_SUMM_SURG_OTH_REGDIS<-factor(puf$RX_SUMM_SURG_OTH_REGDIS,
                                    levels=c(0,1,2,3,4,5,9),
                                    labels=c(
  "None",
  "Nonprimary surgical procedure performed",
  "Nonprimary surgical procedure to other regional sites",
  "Nonprimary surgical procedure to distant lymph node(s)",
  "Nonprimary surgical procedure to distant site",
  "Combination of codes",
  "Unknown"))

puf$READM_HOSP_30_DAYS<-factor(puf$READM_HOSP_30_DAYS,
                               levels=c(0,1,2,3,9),
                               labels=c(
  "No surgical procedure of the primary site was performed, or patient not readmitted",
  "Unplanned readmission within 30 days of discharge",
  "Planned readmission within 30 days of discharge",
  "Planned and unplanned readmission within 30 days of discharge",
  "Unknown if surgery recommended/performed, unknown if readmitted within 30 days of discharge"))

puf$REASON_FOR_NO_SURGERY<-factor(puf$REASON_FOR_NO_SURGERY,
                                  levels=c(0,1,2,5,6,7,8,9),
                                  labels=c(
  "Surgery of the primary site was performed",
  "Surgery not performed because it was not part of the planned first course treatment",
  "Surgery was not recommended/performed, contraindicated due to patient risk factors",
  "Surgery not performed because the patient died prior to planned or recommended surgery",
  "Surgery was recommeded by physician but not performed, No reason was noted in patient record",
  "Surgery was recommended but was refused by the patient, patient's family member or guardian",
  "Surgery was recommended, but unknown if performed",
  "Unknown if surgery was recommended or performed"))

puf$RX_SUMM_RADIATION<-factor(puf$RX_SUMM_RADIATION,
                              levels=c(0,1,2,3,4,5,9),
                              labels=c(
  "None",
  "Beam radiation",
  "Radioactive implants",
  "Radioisotopes",
  "Combination of beam radiation with radioactive implants or radioisotopes",
  "Radiation therapy, NOS",
  "Unknown"))

puf$RAD_LOCATION_OF_RX<-factor(puf$RAD_LOCATION_OF_RX,
                               levels=c(0,1,2,3,4,8,9),
                               labels=c(
  "No radiation therapy administered",
  "All radiation treatment at this facility",
  "Regional treatment at this facility, boost elsewhere",
  "Boost radiation at this facility, regional elsewhere",
  "All radiation treatment elsewhere",
  "Other",
  "Unknown"))

puf$RAD_TREAT_VOL<-factor(puf$RAD_TREAT_VOL,
                          levels=c(seq(0,41,1),50,60,98,99),
                          labels=c(
  "No radiation treatment",
  "Eye/Orbit",
  "Pituitary",
  "Brain (NOS)",
  "Brain (limited)",
  "Head and neck (NOS)",
  "Head and neck (limited)",
  "Glottis",
  "Sinuses",
  "Parotid",
  "Chest/Lung (NOS)",
  "Lung (limited)",
  "Esophagus",
  "Stomach",
  "Liver",
  "Pancreas",
  "Kidney",
  "Abdomen (NOS)",
  "Breast",
  "Breast/Lymph nodes",
  "Chest wall",
  "Chest wall/Lymph nodes",
  "Mantle, Mini-mantle",
  "Lower extended field",
  "Spine",
  "Skull",
  "Ribs",
  "Hip",
  "Pelvic bones",
  "Pelvis (NOS)",
  "Skin",
  "Soft tissue",
  "Hemibody",
  "Whole body",
  "Bladder and pelvis",
  "Prostate and pelvis",
  "Uterus and cervix",
  "Shoulder",
  "Extremity bone, NOS",
  "Inverted Y",
  "Spinal cord",
  "Prostate",
  "Thyroid",
  "Lymph node region, NOS",
  "Other",
  "Unknown"))

puf$RAD_REGIONAL_RX_MODALITY<-factor(puf$RAD_REGIONAL_RX_MODALITY,
              levels=c(0,seq(20,32,1),40,41,42,43,50,51,52,53,
                       54,55,60,61,62,80,85,98,99),
              labels=c(
  "No radiation treatment",
  "External beam, NOS",
  "Orthovoltage",
  "Cobalt-60, Cesium-137",
  "Photons (2-5 MV)",
  "Photons (6-10 MV)",
  "Photons (11-19 MV)",
  "Photons (>19 MV)",
  "Photons (mixed energies)",
  "Electrons",
  "Photons and electrons mixed",
  "Neutrons, with or without photons/electrons",
  "IMRT",
  "Conformal or 3-D therapy",
  "Protons",
  "Stereotactic radiosurgery, NOS",
  "Linac radiosurgery",
  "Gamma Knife",
  "Brachytherapy, NOS",
  "Brachytherapy, Intracavitary, LDR",
  "Brachytherapy, Intracavitary, HDR",
  "Brachytherapy, Interstitial, LDR",
  "Brachytherapy, Interstitial, HDR",
  "Radium",
  "Radioisotopes, NOS",
  "Strontium-89",
  "Strontium-90",
  "Combination modality, specified",
  "Combination modality, NOS",
  "Other, NOS",
  "Unknown"))

#changing to a factor may negate the nature of this continuous variable
# puf$RAD_REGIONAL_DOSE_CGY<-factor(puf$RAD_REGIONAL_DOSE_CGY,
#     levels=c(0,88888,99999),
#     labels=c(
#   "Not administered",
#    "Not applicable",
#    "Unknown"))
puf$RAD_REGIONAL_DOSE_CGY[puf$RAD_REGIONAL_DOSE_CGY==99999]<-NA

puf$RAD_BOOST_RX_MODALITY<-factor(puf$RAD_BOOST_RX_MODALITY,
        levels=c(0,seq(20,32,1),40,41,42,43,50,51,52,53,
                54,55,60,61,62,98,99),
        labels=c(
  "No radiation treatment",
  "External beam, NOS",
  "Orthovoltage",
  "Cobalt-60, Cesium-137",
  "Photons (2-5 MV)",
  "Photons (6-10 MV)",
  "Photons (11-19 MV)",
  "Photons (>19 MV)",
  "Photons (mixed energies)",
  "Electrons",
  "Photons and electrons mixed",
  "Neutrons, with or without photons/electrons",
  "IMRT",
  "Conformal or 3-D therapy",
  "Protons",
  "Stereotactic radiosurgery, NOS",
  "Linac radiosurgery",
  "Gamma Knife",
  "Brachytherapy, NOS",
  "Brachytherapy, Intracavitary, LDR",
  "Brachytherapy, Intracavitary, HDR",
  "Brachytherapy, Interstitial, LDR",
  "Brachytherapy, Interstitial, HDR",
  "Radium",
  "Radioisotopes, NOS",
  "Strontium-89",
  "Strontium-90",
  "Other, NOS",
  "Unknown"))

#changing to a factor may negate the nature of this continuous variable
# puf$RAD_BOOST_DOSE_CGY<-factor(puf$RAD_BOOST_DOSE_CGY,
#                                levels=c(0,88888,99999),
#                       labels=c(
#   0 "Not administered",
#   88888 "Not applicable",
#   99999 "Unknown"))
puf$RAD_BOOST_DOSE_CGY[puf$RAD_BOOST_DOSE_CGY==99999]<-NA

#changing to a factor may negate the nature of this continuous variable
# puf$RAD_NUM_TREAT_VOL<-factor(puf$RAD_NUM_TREAT_VOL,
#                               levels=c(0,999),
#                               labels=c(
#   "None",
#   "Unknown"
# ))
puf$RAD_NUM_TREAT_VOL[puf$RAD_NUM_TREAT_VOL==999]<-NA

puf$RX_SUMM_SURGRAD_SEQ<-factor(puf$RX_SUMM_SURGRAD_SEQ,
                                levels=c(0,2,3,4,5,6,9),
                                labels=c(
  "No radiation therapy and/or surgical procedures",
  "Radiation therapy before surgery",
  "Radiation therapy after surgery",
  "Radiation therapy both before and after surgery",
  "Intraoperative radiation therapy",
  "Intraoperative radiation therapy with other therapy administered before or after surgery",
  "Sequence unknown"))

# puf$RAD_ELAPSED_RX_DAYS<-factor(puf$RAD_ELAPSED_RX_DAYS,
#                                 levels=c(000,999),
#                                 labels=c(
#   "None, radiation not administered",
#   "Missing, incomplete, or unknown radiation or dates"))
puf$RAD_ELAPSED_RX_DAYS[puf$RAD_ELAPSED_RX_DAYS==999]<-NA

puf$REASON_FOR_NO_RADIATION<-factor(puf$REASON_FOR_NO_RADIATION,
                                    levels=c(0,1,2,5,6,7,8,9),
                                    labels=c(
  "Radiation therapy was administered",
  "Radiation was not part of the planned first course treatment",
  "Radiation contraindicated due to other patient risk factors",
  "Patient died prior to planned or recommended therapy",
  "Radiation recommended but not administered, no reason was noted",
  "Radiation recommended but refused by the patient, patient's family member or guardian",
  "Radiation recommended, unknown whether administered",
  "Unknown if recommended or administered"))

puf$RX_SUMM_CHEMO<-factor(puf$RX_SUMM_CHEMO,
                          levels=c(0,1,2,3,82,85,86,87,88,99),
                          labels=c(
  "None",
  "Chemotherapy administered, type and number of agents not documented",
  "Single-agent chemotherapy",
  "Multiagent chemotherapy",
  "Chemotherapy not recommended/administered, contraindicated due to patient risk factors",
  "Chemotherapy not administered, patient died prior to planned or recommended therapy",
  "Chemotherapy not administered, was recommended, not administered Reason unknown",
  "Chemotherapy not administered, recommended, but refused by patient, patient's family member or guardian",
  "Chemotherapy recommended, unknown if administered",
  "Unknown if recommended or administered"))

puf$RX_HOSP_CHEMO<-factor(puf$RX_HOSP_CHEMO,
                          levels=c(0,1,2,3,99),
                          labels=c(
  "None",
  "Chemotherapy administered, type and number of agents not documented",
  "Single-agent chemotherapy",
  "Multiagent chemotherapy",
  "Unknown if recommended or administered"))

puf$RX_SUMM_HORMONE<-factor(puf$RX_SUMM_HORMONE,
                            levels=c(0,1,82,85,86,87,88,99),
                            labels=c(
  "None",
  "Hormone therapy administered as first course therapy",
  "Not recommended/administered, contraindicated due to patient risk factors",
  "Hormone therapy not administered, patient died prior to planned or recommended therapy",
  "Recommended but not administered, no reason stated",
  "Recommended, not administered, refused by patient, patient's family member or guardian",
  "Hormone therapy recommended, unknown if administered",
  "Unknown if recommended or administered"))

puf$RX_HOSP_HORMONE<-factor(puf$RX_HOSP_HORMONE,
                            levels=c(0,1,99),
                            labels=c(
  "None",
  "Hormone therapy administered as first course therapy",
  "Unknown if recommended or administered"))

puf$RX_SUMM_IMMUNOTHERAPY<-factor(puf$RX_SUMM_IMMUNOTHERAPY,
                                  levels=c(0,1,82,85,86,87,88,99),
                                  labels=c(
  "None",
  "Immunotherapy administered as first course therapy",
  "Not recommended/administered, contraindicated due to patient risk factors",
  "Not administered because the patient died prior to planned or recommended therapy",
  "Recommended, not administered, No reason given",
  "Recommended, not administered, refused by the patient, patient's family member or guardian",
  "Immunotherapy recommended, unknown if administered",
  "Unknown"))

puf$RX_HOSP_IMMUNOTHERAPY<-factor(puf$RX_HOSP_IMMUNOTHERAPY,
                                  levels=c(0,1,99),
                                  labels=c(
  "None",
  "Immunotherapy administered as first course therapy",
  "Unknown"))

puf$RX_SUMM_TRNSPLNT_ENDO<-factor(puf$RX_SUMM_TRNSPLNT_ENDO,
                                  levels=c(0,10,11,12,20,30,
                                           40,82,85,86,87,88,99),
                                  labels=c(
  "No transplant procedure or endocrine therapy administered",
  "Bone marrow transplant procedure administered, type not specified",
  "Bone marrow transplant, autologous",
  "Bone marrow transplant, allogeneic",
  "Stem cell harvest and infusion",
  "Endocrine surgery and/or endocrine radiation therapy",
  "Combination of endocrine surgery and/or radiation with a transplant procedure",
  "Transplant/endocrine surgery/radiation not recommended/administered, contraindicated due to patient risk factors",
  "Transplant/endocrine surgery/radiation not administered,patient died prior to planned or recommended therapy",
  "Recommended Transplant/endocrine/surgery/radiation but not administered no reason given",
  "Recommended but not administered, treatment refused by patient, patient's family member or guardian",
  "Hematologic transplant and/or endocrine surgery/radiation recommended, unknown if administered",
  "Unknown if recommended or administered"))

puf$RX_SUMM_OTHER<-factor(puf$RX_SUMM_OTHER,
                          levels=c(0,1,2,3,6,7,8,9),
                          labels=c(
  "None",
  "Other",
  "Other-Experimental",
  "Other-Double Blind",
  "Other-Unproven",
  "Treatment 1, 2 or 3 recommended but refused by patient, patient's family or guardian",
  "Recommended unknown if administered",
  "Unknown if recommended or adminstered"))

puf$RX_HOSP_OTHER<-factor(puf$RX_HOSP_OTHER,
                          levels=c(0,1,2,3,6,7,8,9),
                          labels=c(
  "None",
  "Other",
  "Other-Experimental",
  "Other-Double Blind",
  "Other-Unproven",
  "Treatment 1, 2 or 3 recommended but refused by patient, patient's family or guardian",
  "Recommended unknown if administered",
  "Unknown if recommended or adminstered"))

puf$PALLIATIVE_CARE<-factor(puf$PALLIATIVE_CARE,
                            levels=c(0,1,2,3,4,5,6,7,9),
                            labels=c(
  "None",
  "Surgery",
  "Radiation therapy",
  "Chemo, hormone, other systemic drugs",
  "Pain management therapy with no other palliative care",
  "Any combination of codes 1, 2, and/or 3 without code 4",
  "Any combination of codes 1, 2, and/or 3 with code 4",
  "Palliative care performed or referred, type unknown or other than codes 1-6",
  "Unknown if palliative care performed or referred"))

puf$RX_SUMM_SYSTEMIC_SUR_SEQ<-factor(puf$RX_SUMM_SYSTEMIC_SUR_SEQ,
                                     levels=c(0,2,3,4,5,6,7,9),
                                     labels=c(
  "No systemic therapy and/or no surgery",
  "Systemic therapy before surgery",
  "Systemic therapy after surgery",
  "Systemic therapy before and after surgery",
  "Intraoperative systemic therapy during surgical procedure",
  "Intraoperative systemic therapy with other Rx administered before or after surgery",
  "Systemic therapy administered between two separate surgical procedures",
  "Sequence unknown"))

puf$RX_SUMM_TREATMENT_STATUS<-factor(puf$RX_SUMM_TREATMENT_STATUS,
                                     levels=c(0,1,2,9),
                                     labels=c(
  "No Treatment Given",
  "Treatment Given",
  "Active Surveillance",
  "Unknown if Treatment Given"))

puf$PUF_30_DAY_MORT_CD<-factor(puf$PUF_30_DAY_MORT_CD,
                               levels=c(0,1,9),
                               labels=c(
  "Patient alive, or died more than 30 days after surgery performed",
  "Patient died 30 or fewer days after surgery performed",
  "Patient alive with fewer than 30 days of follow-up, surgery date missing, or last contact date missing"))

puf$PUF_90_DAY_MORT_CD<-factor(puf$PUF_90_DAY_MORT_CD,
                               levels=c(0,1,9),
                               labels=c(
  "Patient alive, or died more than 90 days after surgery performed",
  "Patient died 90 or fewer days after surgery performed",
  "Patient alive with fewer than 90 days of follow-up, surgery date missing, or last contact date missing"))

puf$DX_LASTCONTACT_DEATH_MONTHS[puf$DX_LASTCONTACT_DEATH_MONTHS==9999]<-NA

puf$PUF_VITAL_STATUS<-factor(puf$PUF_VITAL_STATUS,
                             levels=c(0,1),
                             labels=c(
  "Dead",
  "Alive"))

puf$PUF_MULT_SOURCE<-factor(puf$PUF_MULT_SOURCE,
                            levels=c(0,1),
                            labels=c(
  "Only one facility reported this case to NCDB",
  "Records pertaining to this case submitted to NCDB by more than one facility"))

puf$REFERENCE_DATE_FLAG<-factor(puf$REFERENCE_DATE_FLAG,levels=c(0,1),
                                labels=c(
  "Diagnosis date before reference date",
  "Diagonsis date on or after reference date"))

puf$RX_SUMM_SCOPE_REG_LN_2012<-factor(puf$RX_SUMM_SCOPE_REG_LN_2012,
                                      levels=c(0,1,2,3,4,5,6,7,9),
                                      labels=c(
  "None, No regional lymph node surgery No lymph nodes found in the pathologic specimen",
  "Biopsy or aspiration of regional lymph node, NOS",
  "Sentinel lymph node biopsy",
  "Number of regional nodes removed unknown or not stated, regional lymph nodes removed, NOS",
  "1-3 regional lymph nodes removed",
  "4 or more regional lymph nodes removed",
  "Sentinel node biopsy & code 3, 4, or 5 at same time, or timing not stated",
  "Sentinel node biopsy and code 3, 4, or 5 at different times",
  "Unknown or not applicable"))

puf$RX_HOSP_DXSTG_PROC<-factor(puf$RX_HOSP_DXSTG_PROC,
                               levels=c(0,1,2,3,4,5,6,7,9),
                               labels=c(
  "No surgical diagnostic or staging procedure was performed",
  "A biopsy was done to a site other than the primary",
  "A biopsy (incisional, needle, or aspiration) was done to the primary site",
  "A surgical exploration only",
  "A surgical procedure with a bypass was performed, but no biopsy was done",
  "An exploratory procedure was performed, and a biopsy of either the primary site or another site was done",
  "A bypass procedure was performed, and a biopsy of either the primary site or another site was done",
  "A procedure was done, but the type of procedure is unknown",
  "No information of whether a diagnostic or staging procedure was performed"))

puf$PALLIATIVE_CARE_HOSP<-factor(puf$PALLIATIVE_CARE_HOSP,
                                 levels=c(0,1,2,3,4,5,6,7,9),
                                 labels=c(
  "None",
  "Surgery",
  "Radiation therapy",
  "Chemo, hormone, other systemic drugs",
  "Pain management therapy with no other palliative care",
  "Any combination of codes 1, 2, and/or 3 without code 4",
  "Any combination of codes 1, 2, and/or 3 with code 4",
  "Palliative care performed or referred, type unknown or other than codes 1-6",
  "Unknown if palliative care performed or referred"))

#changing to a factor may negate the nature of this continuous variable
# puf$TUMOR_SIZE_SUMMARY<-factor(puf$TUMOR_SIZE_SUMMARY,
#                                levels=c(0,1,989,990,998,999),
#                                labels=c(
#   "No Tumor found",
#   "1 mm or < 1 mm",
#   ">=989 mm",
#   "Microscopic focus/foci only/no size of focus",
#   "Site Specific Codes",
#   "Unknown"))
puf$TUMOR_SIZE_SUMMARY[puf$TUMOR_SIZE_SUMMARY==999]<-NA

puf$METS_AT_DX_OTHER<-factor(puf$METS_AT_DX_OTHER,
                             levels=c(0,1,2,8,9),
                             labels=c(
  "None",
  "Yes",
  "Generalized metastases such as carcinomatosis",
  "Not Applicable",
  "Unknown"))

puf$METS_AT_DX_DISTANT_LN<-factor(puf$METS_AT_DX_DISTANT_LN,
                                  levels=c(0,1,8,9),
                                  labels=c(
  "None",
  "Yes", 
  "Not Applicable",
  "Unknown")) 	

puf$METS_AT_DX_BONE<-factor(puf$METS_AT_DX_BONE,
                            levels=c(0,1,8,9),
                            labels=c(
  "None",
  "Yes", 
  "Not Applicable",
  "Unknown"))

puf$METS_AT_DX_BRAIN<-factor(puf$METS_AT_DX_BRAIN,
                             levels=c(0,1,8,9),
                             labels=c(
  "None",
  "Yes", 
  "Not Applicable",
  "Unknown"))

puf$METS_AT_DX_LIVER<-factor(puf$METS_AT_DX_LIVER,
                             levels=c(0,1,8,9),
                             labels=c(
  "None",
  "Yes", 
  "Not Applicable",
  "Unknown"))

puf$METS_AT_DX_LUNG<-factor(puf$METS_AT_DX_LUNG,
                            levels=c(0,1,8,9),
                            labels=c(
  "None",
  "Yes", 
  "Not Applicable",
  "Unknown")) 

puf$NO_HSD_QUAR_16<-factor(puf$NO_HSD_QUAR_16,
                           levels=c(1,2,3,4),
                           labels=c(
  ">=17.6%",
  "10.9-17.5%",
  "6.3-10.8%",
  "<6.3%"))

puf$MED_INC_QUAR_16<-factor(puf$MED_INC_QUAR_16,
                            levels=c(1,2,3,4),
                            labels=c(
  "< $40,227",
  "$40,227-50,353",
  "$50,354-63,332",
  ">=$63,333"))

puf$MEDICAID_EXPN_CODE<-factor(puf$MEDICAID_EXPN_CODE,
                               levels=c(0,1,2,3,9),
                               labels=c(
  "Non-Expansion States",
  "January 2014 Expansion States",
  "Early Expansion States (2010-2013)",
  "Late Expansion States (after Jan. 2014)",
  "Suppressed for Ages 0-39"))

return(puf)
}