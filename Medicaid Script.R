############################################################
## Packages and database connection
############################################################

library(dplyr)
library(DBI)

## Connection object `con` was created via the Stanford Population Health 
#Sciences to the T-MSIS Files
## Medicaid environment and points to the 2018 California
## Medi-Cal table:
##   "medicaid_data_california_2018"

# con <- dbConnect(odbc::odbc(), dsn = "Medicaid", ...)

############################################################
## County-specific subsets (2018 Medi-Cal data)
##
## FIPS codes (main text):
##  Fresno     = 06019
##  Riverside  = 06065
##  Orange     = 06059
##  Los Angeles= 06037
##  Ventura    = 06111
##
## In the Medicaid table we used the 3-digit county codes:
##  '019', '065', '059', '037', '111'
############################################################

Fresno.2018 <- con %>%
  tbl("medicaid_data_california_2018") %>%
  filter(BENE_CNTY_CD == "019")

Riverside.2018 <- con %>%
  tbl("medicaid_data_california_2018") %>%
  filter(BENE_CNTY_CD == "065")

Orange.2018 <- con %>%
  tbl("medicaid_data_california_2018") %>%
  filter(BENE_CNTY_CD == "059")

LA.2018 <- con %>%
  tbl("medicaid_data_california_2018") %>%
  filter(BENE_CNTY_CD == "037")

Ventura.2018 <- con %>%
  tbl("medicaid_data_california_2018") %>%
  filter(BENE_CNTY_CD == "111")

############################################################
## Age-stratified subsets
##
## As described in the manuscript, we examined:
##   0–5 years, 5–18 years, <65 years, and ≥65 years.
##
## For illustration, we show the elderly (≥65) subset, which
## appears explicitly in the code as AGE > 64 (e.g., in
## Fresno.columns.2018 and Riverside.columns.2018).
## Identical filters were applied for other age bands
## using the AGE variable.
############################################################

## Elderly (≥65) examples

Fresno.columns.2018 <- Fresno.2018 %>%
  filter(AGE > 64)
# view(Fresno.columns.2018)

Riverside.columns.2018 <- Riverside.2018 %>%
  filter(AGE > 64)
# view(Riverside.columns.2018)

Orange.columns.2018 <- Orange.2018 %>%
  filter(AGE > 64)

LA.columns.2018 <- LA.2018 %>%
  filter(AGE > 64)

Ventura.columns.2018 <- Ventura.2018 %>%
  filter(AGE > 64)

## Note: Analogous subsets were created for ages 0–5, 5–18,
## <65, and all ages by adding appropriate AGE filters to
## the county-specific tables above.

############################################################
## Diagnostic groups and ICD codes
##
## The manuscript groups diagnoses as:
##  Respiratory:
##    Respiratory Infections:         J06.9   (DGNS_CD_1 == 'J069')
##    COPD:                           J44.9   (DGNS_CD_1 == 'J449')
##    Chronic Respiratory Failure:    J96.1x  (DGNS_CD_1 == 'J9610')
##
##  Cardiovascular:
##    Cardiac Arrest:                 I46.9   (DGNS_CD_1 == 'I469')
##    Heart Failure:                  I50.9   (DGNS_CD_1 == 'I509')
##    Heart Disease:                  I51.9   (DGNS_CD_1 == 'I519')
##
##  Kidney:
##    Kidney Failure (Acute):         N17.9   (DGNS_CD_1 == 'N179')
##    Chronic Kidney Disease:         N18.9   (DGNS_CD_1 == 'N189')
##
##  Fresno only:
##    Foodborne disease:              A05.9   (DGNS_CD_1 == 'A059')
##    Waterborne illness:             Z77.111 (DGNS_CD_1 == 'Z77111')
##
## Below we show representative query blocks. Identical
## structures were repeated for all dates in the pre- and
## post-event windows specified in the paper.
############################################################

############################################################
## Example 1: Fresno County (FIPS 06019)
##           Respiratory Infections (J069), Elderly (≥65)
##           May 2018 (control outage window)
############################################################

## Single-day query (example: May 1, 2018)
May01_RI_Elderly_Fresno <- Fresno.columns.2018 %>%
  filter(BENE_CNTY_CD == "019") %>%
  filter(SRVC_BGN_DT == "01MAY2018") %>%
  filter(DGNS_CD_1 == "J069") %>%
  dplyr::select(BENE_CNTY_CD, MSIS_ID) %>%
  distinct() %>%
  group_by(BENE_CNTY_CD) %>%
  dplyr::summarize(count = n()) %>%
  collect()
# view(May01_RI_Elderly_Fresno)

## The same code pattern was repeated for each date in the
## pre- and post-outage windows (e.g., "02MAY2018", …,
## "18MAY2018") by changing the SRVC_BGN_DT literal.

############################################################
## Example 2: Fresno County – Kidney Disease (N189), Elderly
##            (≥65), May 2018
############################################################

May31_KD_Elderly_Fresno <- Fresno.columns.2018 %>%
  filter(BENE_CNTY_CD == "019") %>%
  filter(SRVC_BGN_DT == "31MAY2018") %>%
  filter(DGNS_CD_1 == "N189") %>%
  dplyr::select(BENE_CNTY_CD, MSIS_ID) %>%
  distinct() %>%
  group_by(BENE_CNTY_CD) %>%
  dplyr::summarize(count = n()) %>%
  collect()
# view(May31_KD_Elderly_Fresno)

############################################################
## Example 3: Fresno County – Foodborne and Waterborne disease
##            (A059, Z77111), All Ages and Elderly
############################################################

## Foodborne illness (A059), all ages (example: May 1, 2018)
May01_FBI_Fresno <- con %>%
  tbl("medicaid_data_california_2018") %>%
  filter(BENE_CNTY_CD == "019") %>%
  filter(SRVC_BGN_DT == "01MAY2018") %>%
  filter(DGNS_CD_1 == "A059") %>%
  dplyr::select(BENE_CNTY_CD, MSIS_ID) %>%
  distinct() %>%
  group_by(BENE_CNTY_CD) %>%
  dplyr::summarize(count = n()) %>%
  collect()
# view(May01_FBI_Fresno)

## Foodborne illness (A059), elderly only (example)
May01_FBI_Elderly_Fresno <- Fresno.columns.2018 %>%
  filter(BENE_CNTY_CD == "019") %>%
  filter(SRVC_BGN_DT == "01MAY2018") %>%
  filter(DGNS_CD_1 == "A059") %>%
  dplyr::select(BENE_CNTY_CD, MSIS_ID) %>%
  distinct() %>%
  group_by(BENE_CNTY_CD) %>%
  dplyr::summarize(count = n()) %>%
  collect()
# view(May01_FBI_Elderly_Fresno)

## Waterborne illness (Z77111), all ages (example)
May01_WBI_Fresno <- con %>%
  tbl("medicaid_data_california_2018") %>%
  filter(BENE_CNTY_CD == "019") %>%
  filter(SRVC_BGN_DT == "01MAY2018") %>%
  filter(DGNS_CD_1 == "Z77111") %>%
  dplyr::select(BENE_CNTY_CD, MSIS_ID) %>%
  distinct() %>%
  group_by(BENE_CNTY_CD) %>%
  dplyr::summarize(count = n()) %>%
  collect()
# view(May01_WBI_Fresno)

## Note: As stated in the manuscript, these conditions were
## reviewed but excluded from the main analysis because the
## event counts were too low for robust inference.

############################################################
## Example 4: Riverside County (FIPS 06065)
##           COPD (J449) and Respiratory Infections (J069),
##           Elderly (≥65), Holy Fire window
############################################################

## COPD (J449), elderly (example date)
AUG23_COPD_Elderly_Riverside <- Riverside.columns.2018 %>%
  filter(BENE_CNTY_CD == "065") %>%
  filter(SRVC_BGN_DT == "23AUG2018") %>%
  filter(DGNS_CD_1 == "J449") %>%
  dplyr::select(BENE_CNTY_CD, MSIS_ID) %>%
  distinct() %>%
  group_by(BENE_CNTY_CD) %>%
  dplyr::summarize(count = n()) %>%
  collect()
# view(AUG23_COPD_Elderly_Riverside)

## Respiratory infections (J069), elderly (example date)
AUG11_RI_Elderly_Riverside <- Riverside.columns.2018 %>%
  filter(BENE_CNTY_CD == "065") %>%
  filter(SRVC_BGN_DT == "11AUG2018") %>%
  filter(DGNS_CD_1 == "J069") %>%
  dplyr::select(BENE_CNTY_CD, MSIS_ID) %>%
  distinct() %>%
  group_by(BENE_CNTY_CD) %>%
  dplyr::summarize(count = n()) %>%
  collect()
# view(AUG11_RI_Elderly_Riverside)

## Identical query patterns were used for the full pre- and
## post-Holy Fire windows (July 17–September 22, 2018).

############################################################
## Example 5: Orange, Los Angeles, and Ventura counties
##           Respiratory infections (J069), Elderly (≥65)
############################################################

## Orange County example (August 2018)
AUG01_RI_Elderly_Orange <- Orange.columns.2018 %>%
  filter(BENE_CNTY_CD == "059") %>%
  filter(SRVC_BGN_DT == "01AUG2018") %>%
  filter(DGNS_CD_1 == "J069") %>%
  dplyr::select(BENE_CNTY_CD, MSIS_ID) %>%
  distinct() %>%
  group_by(BENE_CNTY_CD) %>%
  dplyr::summarize(count = n()) %>%
  collect()

## Los Angeles example (October 2018)
OCT07_RI_Elderly_LA <- LA.columns.2018 %>%
  filter(BENE_CNTY_CD == "037") %>%
  filter(SRVC_BGN_DT == "07OCT2018") %>%
  filter(DGNS_CD_1 == "J069") %>%
  dplyr::select(BENE_CNTY_CD, MSIS_ID) %>%
  distinct() %>%
  group_by(BENE_CNTY_CD) %>%
  dplyr::summarize(count = n()) %>%
  collect()

## Ventura example (October 2018)
OCT07_RI_Elderly_Ventura <- Ventura.columns.2018 %>%
  filter(BENE_CNTY_CD == "111") %>%
  filter(SRVC_BGN_DT == "07OCT2018") %>%
  filter(DGNS_CD_1 == "J069") %>%
  dplyr::select(BENE_CNTY_CD, MSIS_ID) %>%
  distinct() %>%
  group_by(BENE_CNTY_CD) %>%
  dplyr::summarize(count = n()) %>%
  collect()

## This structure was repeated for all days in the pre- and
## post-outage windows listed in Figure 5 of the manuscript
## and for each diagnostic category (COPD, respiratory
## failure, heart disease, heart failure, cardiac arrest,
## kidney disease, kidney failure) and each age group.
############################################################
