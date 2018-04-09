
###############################################
#eHR startup
###############################################

install.packages("digest")
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_0.5.0.tar.gz"  ##version0.5.0
#install.packages(packageurl, repos = NULL, type="source")
##install.packages("dplyr") ##version0.7.4
#packageurldevtools <- "https://cran.r-project.org/src/contrib/Archive/devtools/devtools_1.12.0.tar.gz"
#install.packages(packageurldevtools, repos = NULL, type="source")
library(devtools)
library(dplyr)
#install_github("rOpenHealth/rEHR")
library(rEHR)

ehr_path <- dirname(system.file("ehr_data", package = "rEHR"))

## create a new database connection to a temporary file
db <- database(tempfile(fileext = ".sqlite"))
## Import multiple data files into the database
import_CPRD_data(db, data_dir = ehr_path,         
                 filetypes = c("Common_Dosages","Prodcodes_Cut", "MedicalReadcodes", "Drugproductreadcodes", "prod_pd_treat"),
                 dateformat = "%d/%m/%Y",
                 yob_origin = 1800,
                 regex = "PD_immunosup_Extract",
                 recursive = TRUE)

## Import multiple data files into the database
import_CPRD_data(db, data_dir = ehr_path,         
                 filetypes = c("Clinical", "Patient", "Practice", "Therapy", "Referral", "Consultation"),
                 dateformat = "%d/%m/%Y",
                 yob_origin = 1800,
                 regex = "Sample",
                 recursive = TRUE)

slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
install_url(slam_url)
install_github("rClinicalCodes", "rOpenHealth")
require(rClinicalCodes)


#create lists of the clinical read codes. The lists are gouping the medcodes together per disease. 
#All disease_codes is a list consisting of many lists for all diseases at Clinicalcodes.org
# rheuma_codes <- get_ClinicalCodes(article_id = 13)
# diabetes2_codes <- get_ClinicalCodes(url = "https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/9/codelist/type-2-diabetes/download/")
# diabetes1_codes <- get_ClinicalCodes(url = "https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/9/codelist/type-1-diabetes/download/")
# psoriasis_codes <- get_ClinicalCodes(article_id = 10, codelist_name = "psoriasis")
# disease_codes <- get_ClinicalCodes(article_id = 1)
# All_disease_codes <- get_ClinicalCodes(article_id = c(1:70))
#Clinical_codes_def <- import_definitions(system.file("ehr_data", "res13-rheumatoid-arthritis.csv", package = "rEHR"))
#def2 <- import_definitions(system.file("extdata", "example_search.csv", package = "rpcdsearch"))
#medical_table <- read.delim("Lookups//medical.txt", fileEncoding="latin1", stringsAsFactors = FALSE)


###############################################
#eHRALLData
###############################################

###
library(stringr)
numextract <- function(string){
  str_extract(string,"\\-*\\d+\\.*\\d*")
}
letterextract <- function(string){
  sub("[^[:alpha:]]+", "", string)}

###3.1
#cancer_codes <- clinical_codes[clinical_codes$list=="Cancer",]
#diabetes_codes <- clinical_codes[clinical_codes$list=="Diabetes",]
Parkinsoncodes = as.numeric(unlist(strsplit(c("1691 4321 8956	9509 10718 14912 16860 17004 53655 59824 86062 96860 101090"), "\\s+")[[1]]))
ParkinsonExclusioncodes = as.numeric(unlist(strsplit(c("19478	24001	26181	33544	51105	52589	72879	97170	100128"), "\\s+")[[1]]))
ImmuneMed = tbl_df(sqldf("SELECT * FROM Prodcodes_Cut", connection = db))
PD_medcodes <- tbl_df(sqldf("SELECT medcode, desc FROM MedicalReadcodes WHERE medcode IN (1691, 4321, 8956,	9509, 10718, 14912, 16860, 17004, 53655, 59824, 86062, 96860, 101090)", connection = db))
All_medcodes <- tbl_df(sqldf("SELECT * FROM MedicalReadcodes", connection = db))
PD_prodcodes <- tbl_df(sqldf("SELECT cprd_prodcode, description2 FROM prod_pd_treat", connection = db))
colnames(PD_prodcodes)[1] <- "prodcode"


###
#find ImmuneMed in prodcode
SelImmuneProducts <- tbl_df(select_events(db,tab="Therapy", columns = '*',
                                          where = "prodcode %in% .(ImmuneMed$Prodcode)"))
SelFirstImmuneProducts <- tbl_df(first_events(db, tab="Therapy", columns = c("patid", "eventdate", "prodcode"), 
                                              where = "prodcode %in% .(ImmuneMed$Prodcode)"))
#SelOtherProducts <- tbl_df(select_events(db,tab = "Therapy_All", columns = '*', 
#                                 where = "!prodcode %in% .(ImmuneMed$Prodcode)"))
nImmuneProducts <- tbl_df(unique(SelImmuneProducts$patid))
nOtherProducts <- tbl_df(unique(SelOtherProducts$patid))

#Change textid column from character to integer...
#MedCalculation$textid <- as.numeric(as.character(MedCalculation$textid))
#SELECT COLUMNS FROM common_dosages file
#Common_Dosages <- tbl_df(select_events(db, tab = "Common_Dosages", columns = c("textid", "text", "daily_dose")))
#MedCalculation <- tbl_df(left_join(MedCalculation, Common_Dosages))

DrugInfo <- tbl_df(select_events(db, tab="Prodcodes_Cut", columns = '*'))
colnames(DrugInfo)[1] <- "prodcode" #DO THIS IN BASH ALREADY!! NEEDS TO BE DONE EVERYTIME OTHERWISE!
#ADD PRODUCT NAME AND STRENGTH TO THERAPY FILE
MedCalculation <- tbl_df(left_join(SelImmuneProducts, DrugInfo))
#############################


###
colnames(MedCalculation)[11] <- "strength"
MedCalculation <- MedCalculation %>% 
  ungroup %>%
  mutate(Concentration = as.numeric(as.character(numextract(MedCalculation$strength))))
MedCalculation <- MedCalculation %>%
  ungroup %>% 
  mutate(unit = letterextract(MedCalculation$strength))
MedCalculation <- tbl_df(sqldf("SELECT patid, eventdate, prodcode, consid, qty, ndd, Drug, Concentration, unit, Formulation FROM MedCalculation ORDER BY patid, prodcode, eventdate"))
#Maybe add issueseq, drug and Formulation when needed later strength...?
#MedCalculation files filtered on NDD present or not. 



###############################################
#3months_2consecutive
###############################################

MedCalculation <- MedCalculation %>%
  mutate(medduration= qty/ndd)

#Calculate cumulative duration via ndd. Grouped per patid, prodcode, restart when ndd =0
MedCalculation <- MedCalculation %>%
  mutate(ndd0 = case_when(MedCalculation$ndd > 0 ~ 1,
                          TRUE ~ 0),
         try = c(0,diff(ndd0) ==0))
MedCalculation <- MedCalculation %>% 
  group_by(patid, prodcode, idx = cumsum(try == 0L)) %>% 
  mutate(cum_duration = cumsum(medduration),
         cum_ndd0 = cumsum(ndd0)) %>% 
  ungroup %>% 
  select(-idx, -ndd0, - try)
##################################

#first, SELECT unique patients that have cum_ndd0 > 1 AND cum_duration > 90 to match inclusion criteria. 
Inclusion_patid1 <- tbl_df(sqldf("SELECT DISTINCT(patid) FROM MedCalculation WHERE cum_duration >= 90 AND cum_ndd0 >1 GROUP BY patid, prodcode"))
############

#second, NO ndd, so alternative methods needed to calculate duration to find which patids to include. 
alternative_duration <- tbl_df(sqldf("SELECT * FROM MedCalculation WHERE patid NOT IN (SELECT patid FROM Inclusion_patid1)"))
alternative_duration <- alternative_duration %>% select(-medduration, -cum_duration, -cum_ndd0)
alternative_duration <- alternative_duration %>%
  group_by(patid, prodcode) %>%
  mutate(counter = row_number(),
         BETWEEN0=as.numeric(difftime(eventdate,lag(eventdate,1))),BETWEEN1=ifelse(is.na(BETWEEN0),0,BETWEEN0),FIRST=cumsum(as.numeric(BETWEEN1)))%>%
  select(-BETWEEN0)

#doesn't take gaps into account, so it could be that the 2 consecutive is not correct because of a long gap in between..
Inclusion_patid2 <- tbl_df(sqldf("SELECT DISTINCT(patid) FROM alternative_duration WHERE BETWEEN1 > 1 AND BETWEEN1 < 8000 AND FIRST >= 90 GROUP BY patid, prodcode"))
Exclusion_patid2 <- tbl_df(sqldf("SELECT * FROM alternative_duration WHERE patid NOT IN (SELECT patid FROM Inclusion_patid2)"))
Inclusion_patid3 <- tbl_df(sqldf("SELECT DISTINCT(patid) FROM Exclusion_patid2 WHERE BETWEEN1 >= 8000 AND counter >= 3")) 
Inclusion_patid4 <- tbl_df(sqldf("SELECT DISTINCT(patid) FROM Exclusion_patid2 WHERE ndd = 0 AND counter >= 3")) 
#Perhaps also include the patients where ndd is known, but n >=3, between1<8000.. That would include a few more. alhtough there ndd is a little bit lower, it would often add up to a month of prescription.  
All_Inclusion_patid <- full_join(Inclusion_patid1, Inclusion_patid2)
All_Inclusion_patid <- full_join(All_Inclusion_patid, Inclusion_patid3)
All_Inclusion_patid <- full_join(All_Inclusion_patid, Inclusion_patid4)

three_and_two <- tbl_df(sqldf("SELECT * FROM MedCalculation WHERE patid IN (SELECT patid FROM All_Inclusion_patid)"))
Excluded <- tbl_df(sqldf("SELECT * FROM MedCalculation WHERE patid NOT IN (SELECT patid FROM All_Inclusion_patid)"))
Excluded <- Excluded %>% select(-medduration, -cum_duration, -cum_ndd0)


###############################################
#confirming real PD
###############################################
SelParkinsoncodes = tbl_df(select_events(db,tab="Clinical", columns = '*',
                                         where = "medcode %in% .(Parkinsoncodes)"))
SelFirstParkinsoncodes = tbl_df(first_events(db, tab="Clinical", columns= c("patid", "eventdate", "medcode"), 
                                             where = "medcode %in% .(Parkinsoncodes)"))
#All patients that have received drugs known to be PD drugs
SelParkinsonprodcodes <- tbl_df(select_events(db, tab= "Therapy", columns = c("patid", "eventdate", "prodcode"),
                                              where = "prodcode %in% .(PD_prodcodes$prodcode)"))
#nr of meds prescribed per patient
nr_of_PDcodes_PP <- tbl_df(sqldf("SELECT patid, medcode, COUNT(DISTINCT medcode) AS nr_of_PDcodes FROM SelParkinsoncodes GROUP BY patid ORDER BY nr_of_PDcodes DESC")) %>% ungroup


#all PD patients and the names of the drugs they received
Check_Patients_Meds <- tbl_df(select_events(db, tab = "Therapy", columns =  c("patid", "eventdate", "prodcode"),
                                            where = "patid %in% .(nr_of_PDcodes_PP$patid) & prodcode %in% .(PD_prodcodes$prodcode)"))


#Combine files for PD codes (medcode) and PD drug codes (prodcode)
PD_med_prod <- left_join(SelParkinsoncodes, SelParkinsonprodcodes, by = "patid")
PD_med_prod <- left_join(PD_med_prod, nr_of_PDcodes_PP)
tbl_df(sqldf("SELECT DISTINCT(patid) FROM PD_med_prod"))

#exclusion
#just a check up to see if any patients have PD exclusioncodes, which are PD caused by known causes 
PD_exclusion1 <-  tbl_df(sqldf("SELECT patid, medcode FROM PD_med_prod WHERE medcode IN (19478, 24001, 26181, 33544, 97170, 100128)"))


# This statement checks whether the patients truly have PD:
# Have they received more than 1 medcode? if only 1, have they also received PD medication? 
# if not, likely not really PD. 
True_PD <- PD_med_prod %>% mutate(inclusion = case_when(
  PD_med_prod$nr_of_PDcodes > 1 ~ "include",
  PD_med_prod$nr_of_PDcodes == 1 & PD_med_prod$prodcode != is.na(PD_med_prod$prodcode) ~ "include",
  TRUE ~ "exclude")) %>%
  group_by(patid) %>%
  filter(inclusion == "include") %>%
  distinct(patid, .keep_all = TRUE)


####
#PD diagnosed at least 6 months after first IS prescription. Use first statements and select only patients from TRUE_PD
SelFirstImmuneProductsPD = tbl_df(sqldf("SELECT * FROM SelFirstImmuneProducts WHERE patid IN (SELECT patid FROM True_PD)"))
#combine first PD immunosuppressant prescription and first PD diagnosis.
IS_before_PD <- left_join(SelFirstImmuneProductsPD, SelFirstParkinsoncodes, by = "patid")
#If diagnosis is more than 180 days after first prescription, add include variable. 
IS_before_PD <- IS_before_PD %>% mutate(over_six = case_when(as.Date(as.character(IS_before_PD$eventdate.y), format = "%Y-%m-%d") 
                                                             - as.Date(as.character(IS_before_PD$eventdate.x), format = "%Y-%m-%d") > 180 ~ "include",
                                                             TRUE ~ "exclude"))
#select those cases where PD is found at least 6 months after IS prescription and other inclusion criteria are met. 
IS_before_PD <- tbl_df(sqldf("SELECT * FROM IS_before_PD WHERE over_six == 'include'")) 


###############################################
#Comorbidities
###############################################
all_diseases_IS_patients <- tbl_df(select_events(db, tab = "Clinical", columns = '*', where = "patid %in% .(three_and_two$patid) & constype == 3"))
all_diseases_IS_patients <- tbl_df(left_join(all_diseases_IS_patients, All_medcodes))
other_diseases_PD_patients <- tbl_df(select_events(db, tab = "Clinical", columns = '*', where = "patid %in% .(True_PD$patid) & constype == 3"))#this shoudl be IS_before_PD right?
other_diseases_PD_patients <- tbl_df(left_join(other_diseases_PD_patients, All_medcodes))
diseases_matching_IS <- tbl_df(select_events(db, tab = "Clinical", columns = '*', where = "patid %in% .(three_and_two$patid) & consid %in% .(three_and_two$consid) &constype ==3"))
diseases_matching_IS <- tbl_df(left_join(diseases_matching_IS, All_medcodes))
diseases_matching_IS <- tbl_df(left_join(diseases_matching_IS, three_and_two))
diseases_matching_IS <- diseases_matching_IS %>% select(-medduration, -cum_duration, -qty, -ndd, -unit, -cum_ndd0, -enttype, -staffid, -episode, -adid)
nr_of_diseases_PP <- tbl_df(sqldf("SELECT patid, consid, COUNT(DISTINCT medcode) AS nr_of_diseases FROM diseases_matching_IS GROUP BY patid"))

table(nr_of_diseases_PP$nr_of_diseases)



colnames(diseases_matching_IS)[9] <- "code"
colnames(all_diseases_IS_patients)[13] <- "code"
colnames(other_diseases_PD_patients)[13] <- "code"


#################################
#using clinicalcodes.org:
#################################
clinical_codes_lists <- c()

disease_codes <- c(disease_codes, rheuma_codes)
disease_codes2 <- tbl_df(do.call(rbind.data.frame, disease_codes))
disease_codes2 <- disease_codes2 %>% select(-upload_date)
disease_codes2$list_name <- letterextract(disease_codes2$list_name)
comorbidities <- left_join(diseases_matching_IS,disease_codes2)

All_disease_codes2 <- tbl_df(All_disease_codes %>%
                               lapply(function(x) mutate_each(x, funs('as.character'))) %>%
                               bind_rows() %>%
                               mutate_each(funs('as.factor')))
All_disease_codes3 <- tbl_df(sqldf("SELECT code, list_name FROM All_disease_codes2"))
All_disease_codes3 <- mutate(All_disease_codes3,list_name=sapply(strsplit(gsub('"', '', All_disease_codes3$list_name), split='-', fixed=TRUE),function(x) x[2]))
All_disease_codes3$list_name <- ifelse(grepl(":", All_disease_codes3$list_name),sapply(strsplit(All_disease_codes3$list_name, split=':', fixed=TRUE),function(x) x[2]), All_disease_codes3$list_name)
All_disease_codes3 <- tbl_df(sqldf("SELECT * FROM All_disease_codes3 GROUP BY code")) 
All_disease_codes3$list_name <- trimws(All_disease_codes3$list_name) 
All_disease_codes3$code <- trimws(All_disease_codes3$code) 
IS_patients_linked_comorbidities <- left_join(diseases_matching_IS,All_disease_codes3) %>% select(-sysdate, -consid, -textid, -practid, -Concentration, -Formulation)
IS_patients_other_comorbidities <- left_join(all_diseases_IS_patients, All_disease_codes3) %>% select(-sysdate, -consid, -staffid, -textid, -episode, -enttype, -adid, -practid)
PD_patients_comorbidities <- left_join(other_diseases_PD_patients, All_disease_codes3) %>% select(-sysdate, -consid, -staffid, -textid, -episode, -enttype, -adid, -practid)


####################################
#read in all files from CPRDCAM, files located in ALL in CPRD (=working directory)
####################################
library(data.table)
setwd("~/Brain and Cognitive Sciences/Research Internship 2/CPRD/All")
temp = list.files(pattern="*.csv")
# read in CPRDCAM files as dataframe
options(datatable.fread.datatable=FALSE)
myfiles <- lapply(temp, fread)
#change df names in list to names of files. 
for (i in 1:34) {
  names(myfiles) <- temp[1:34]
  }
#change all dataframe names to the grouped diseases the groups contain.
# would be more efficient if done with for loop like previous line, but for now this works. 
temp[1] <- "Alcohol_problems"
temp[2] <- "Anorexia_or_bulimia"
temp[3] <- "Anxiety_and_other_neurotic_stress_related_and_somatoform_disorders"
temp[4] <- "Asthma_currently_treated"
temp[5] <- "Atrial fibrillation"
temp[6] <- "Blindness and low vision"
temp[7] <- "Bronchiectasis"
temp[8] <- "Cancer_New_diagnosis_in_last_five_years"
temp[9] <- "Coronary_heart_disease"
temp[10] <- "Chronic_kidney_disease"
temp[11] <- "Chronic_Liver_Disease_and_Viral_Hepatitis"
temp[12] <- "COPD"
temp[13] <- "Dementia"
temp[14] <- "Depression"
temp[15] <- "Diabetes"
temp[16] <- "Diverticular_disease_of_intestine"
temp[17] <- "Epilepsy"
temp[18] <- "Heart_failure"
temp[19] <- "Hearing_loss"
temp[20] <- "Hypertension"
temp[21] <- "Inflammatory_bowel_disease"
temp[22] <- "Irritable_bowel_syndrome"
temp[23] <- "Learning_disability"
temp[24] <- " Multiple_sclerosis"
temp[25] <- "Psychoactive_substance_misuse_NOT_ALCOHOL"
temp[26] <- "Parkinsons_disease"
temp[27] <- "Prostate_disorders"
temp[28] <- "Psoriasis_or_eczema"
temp[29] <- "Peripheral_vascular_disease"
temp[30] <- "Rheumatoid_arthritis_other_inflammatory_polyarthropathies_systematic_connective_tissue_disorders"
temp[31] <- "Schizophrenia_and_related_non_organic_psychosis_or_bipolar_disorder"
temp[32] <- "Chronic_sinusitis"
temp[33] <- "Stroke_transient_ischaemic_attack"
temp[34] <- "Thyroid_disorders"

#combine the list into a dataframe
myfiles <- Map(cbind, myfiles, group = temp)
newmyfiles <- rbindlist(myfiles)


# show the dataframes of the comorbidities per patient linked or not to their IS, 
# INCLUDING their respective disease groups as a column
IS_patients_linked_comorbidities <- left_join(diseases_matching_IS,newmyfiles) %>% select(-Concentration, -Formulation, -textid, -practid, -consid, -constype, -sysdate)
IS_patients_other_comorbidities <- left_join(all_diseases_IS_patients, newmyfiles) %>% select(-textid, -practid, -consid, -constype, -sysdate, -enttype, -staffid, -episode, -adid)
PD_patients_comorbidities <- left_join(other_diseases_PD_patients, newmyfiles) %>% select(-textid, -practid, -consid, -constype, -sysdate, -enttype, -staffid, -episode, -adid)

#These are the diseases that are not specified by a group in one of the downloaded codelists; 
unspecified_codes <- tbl_df(sqldf("SELECT medcode, readcode, desc FROM PD_patients_comorbidities WHERE Description IS NULL"))
unspecified_codes2 <- tbl_df(sqldf("SELECT medcode, readcode, desc FROM IS_patients_linked_comorbidities WHERE Description IS NULL"))
unspecified_codes3<- tbl_df(sqldf("SELECT medcode, readcode, desc FROM IS_patients_other_comorbidities WHERE Description IS NULL"))
unspecified_Codes_all <- tbl_df(full_join(unspecified_codes,unspecified_codes2))
unspecified_Codes_all <- tbl_df(unique(full_join(unspecified_Codes_all, unspecified_codes3)))
# Write CSV in R
library(xlsx)
write.xlsx(unspecified_Codes_all, file = "unspecified_codes.xlsx")

specified_codes <- tbl_df(sqldf("SELECT medcode, readcode, desc FROM PD_patients_comorbidities WHERE Description IS NOT NULL"))
specified_codes2 <- tbl_df(sqldf("SELECT medcode, readcode, desc FROM IS_patients_linked_comorbidities WHERE Description IS NOT NULL"))
specified_codes3<- tbl_df(sqldf("SELECT medcode, readcode, desc FROM IS_patients_other_comorbidities WHERE Description IS NOT NULL"))
specified_Codes_all <- tbl_df(full_join(specified_codes,specified_codes2))
specified_Codes_all <- tbl_df(unique(full_join(specified_Codes_all, specified_codes3)))

#find descriptions containing a certain part, and see what group they belong in. think of a method to do htis systematically over 
# all 800 missing codes. 
dplyr::filter(newmyfiles, grepl('isch', Description, ignore.case = TRUE))
dplyr::filter(unspecified_Codes_all, grepl('aort', desc, ignore.case = TRUE))

#126 codes are correctly classified and grouped, whilst 866 are not
# IMPORTANT: THIS IS DONE WITH THE SAMPLE FILES. THE ACTUAL FILES ARE PROBABLY MUCH WORSE!!!!!

##################################
# cut_tv
##################################

tv_out1<- cut_tv(tv_test,
                 entry=start,
                 exit = end,
                 cut_var=drug_1,
                 id_var=id,
                 tv_name=drug_1_state)

tv_out1<- cut_tv(tv_out1,start,end,drug_2,id_var=id, drug_2_state)
head(tv_out1)
