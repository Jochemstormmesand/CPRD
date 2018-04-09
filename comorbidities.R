#Describe the comorbidities of all patients that have received immunosuppressants. 
#Find what medication was presribed for what disease.


#########################
#optional: use clinicalcodes.org: 
#########################

#slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
#install_url(slam_url)
#install_github("rClinicalCodes", "rOpenHealth")
#require(rClinicalCodes)
#create lists of the clinical read codes. The lists are gouping the medcodes together per disease. 
#All disease_codes is a list consisting of many lists for all diseases at Clinicalcodes.org
    # rheuma_codes <- get_ClinicalCodes(article_id = 13)
    # diabetes2_codes <- get_ClinicalCodes(url = "https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/9/codelist/type-2-diabetes/download/")
    # diabetes1_codes <- get_ClinicalCodes(url = "https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/9/codelist/type-1-diabetes/download/")
    # psoriasis_codes <- get_ClinicalCodes(article_id = 10, codelist_name = "psoriasis")
    # disease_codes <- get_ClinicalCodes(article_id = 1)
All_disease_codes <- get_ClinicalCodes(article_id = c(1:70))
#Clinical_codes_def <- import_definitions(system.file("ehr_data", "res13-rheumatoid-arthritis.csv", package = "rEHR"))
#def2 <- import_definitions(system.file("extdata", "example_search.csv", package = "rpcdsearch"))
#medical_table <- read.delim("Lookups//medical.txt", fileEncoding="latin1", stringsAsFactors = FALSE)

#TO TEST WETHER LINE OF CODE WORKS.
#IF FUCKING WORKS 
Clinical_All_Sample <- tbl_df(sqldf("SELECT * FROM Clinical_All LIMIT 1000", connection = db))
dbWriteTable(db, "Clinical_All_Sample", Clinical_All_Sample)
Therapy_All_Sample <- tbl_df(sqldf("SELECT * FROM Therapy_001_Cut2 LIMIT 1000", connection = db))
dbWriteTable(db, "Therapy_All_Sample", Therapy_All_Sample)
all_diseases_IS_patients <- tbl_df(select_events(db, tab = "Clinical_All_Sample", columns = '*'))
select_events(db, tab = "Clinical_All_Sample", columns = '*', where = "patid == 259001")
tbl_df(sqldf("SELECT * FROM Clinical_All_Sample WHERE constype ==3", connection = db))
SelImmuneProducts <- tbl_df(select_events(db,tab="Therapy_001_Cut2", columns = '*',
                                          where = "prodcode %in% .(ImmuneMed$Prodcode)"))
SelFirstImmuneProducts <- tbl_df(first_events(db, tab="Therapy_001_Cut2", columns = c("patid", "eventdate", "prodcode"), 
                                              where = "prodcode %in% .(ImmuneMed$Prodcode)"))

SelFirstParkinsoncodes = tbl_df(first_events(db, tab="Clinical_All", columns= c("patid", "eventdate","constype", "consid", "medcode", "practid"), 
                                             where = "medcode %in% .(Parkinsoncodes)"))
#All patients that have received drugs known to be PD drugs
SelParkinsoncodes = tbl_df(select_events(db,tab="Clinical_All", columns = '*',
                                         where = "medcode %in% .(Parkinsoncodes)"))
SelParkinsonprodcodes <- tbl_df(select_events(db, tab= "Therapy_001_Cut2", columns = c("patid", "eventdate", "prodcode"),
                                              where = "prodcode %in% .(PD_prodcodes$prodcode)"))



#Alternative method to avoid WHERE IN .
all_diseases <- tbl_df(sqldf("SELECT * FROM Clinical_All", connection = db))
three_and_two_marker <- tbl_df(sqldf("SELECT DISTINCT patid FROM three_and_two")) #select All patids of patients that are properly on IS
all_diseases_IS_patients <- semi_join(all_diseases, three_and_two_marker) #select from ALL clinical rows the ones where patid of patients with actual IS
all_diseases_IS_patients <- tbl_df(sqldf("SELECT * FROM all_diseases_IS_patients WHERE constype ==3"))
IS_before_PD_marker <- tbl_df(sqldf("SELECT patid FROM IS_before_PD")) %>% mutate(marker = 1)
other_diseases_PD_patients <- semi_join(all_diseases, IS_before_PD_marker)
other_diseases_PD_patients <- tbl_df(sqldf("SELECT * FROM other_diseases_PD_patients WHERE constype ==3"))
diseases_matching_IS <- semi_join(all_diseases, three_and_two, by = c("patid", "consid"))
diseases_matching_IS <- tbl_df(sqldf("SELECT * FROM diseases_matching_IS WHERE constype ==3"))

# all_diseases_IS_patients <- tbl_df(select_events(db, tab = "Clinical_All", columns = '*', where = "patid %in% .(three_and_two_unique$patid) & constype == 3"))
# all_diseases_IS_patients <- tbl_df(left_join(all_diseases_IS_patients, All_medcodes))
# other_diseases_PD_patients <- tbl_df(select_events(db, tab = "Clinical_All_Sample", columns = '*', where = "patid %in% .(IS_before_PD$patid) & constype == 3")) # maybe also take out the  PD codes by saying & !medcode %in% .(Parkinsoncodes)
# other_diseases_PD_patients <- tbl_df(left_join(other_diseases_PD_patients, All_medcodes))
# diseases_matching_IS <- tbl_df(select_events(db, tab = "Clinical_All_Sample", columns = '*', where = "patid %in% .(three_and_two$patid) & consid %in% .(three_and_two$consid) & constype == 3"))
# diseases_matching_IS <- tbl_df(left_join(diseases_matching_IS, All_medcodes))
# diseases_matching_IS <- tbl_df(left_join(diseases_matching_IS, three_and_two))
# diseases_matching_IS <- diseases_matching_IS %>% select(-medduration, -cum_duration, -qty, -ndd, -unit, -cum_ndd0, -enttype, -staffid, -episode, -adid)
# nr_of_diseases_PP <- tbl_df(sqldf("SELECT patid, consid, COUNT(DISTINCT medcode) AS nr_of_diseases FROM diseases_matching_IS GROUP BY patid"))

table(nr_of_diseases_PP$nr_of_diseases)


# clean up the large list;
    # All_disease_codes <- All_disease_codes[-1]
    # All_disease_codes[[5]] <- NULL
##
#select every 1st column through all elements of list
    # lapply(All_disease_codes, "[[", 1)

#############################
#Clinicalcodes.org
#############################
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


#Probably not needed. Only when I want to improve the names of the lists from clinicalcodes.org.
#tbl_df(sapply(strsplit(All_disease_codes3$list_name, split=":", fixed=TRUE), function(x) (x[2])))


#######

# THESE ALTERNATIVE DISEASES NEED TO BE GROUPED SOMEHOW? NEED TO ESTABLISH GROUPING CRITERIA OR GROUPS THAT THEY WILL FALL INTO.
# Perhaps use www.clinicalcodes.org
# KEEP IN MIND: there are over 76856 clinical codes, spread over 455 code lists. And that is probably not inclusive
#             All_medcodes is 110000 clinical codes long. 
#######


            
            
########################
# using http://www.phpc.cam.ac.uk/pcu/cprd_cam/codelists/
########################
#read in all files from CPRDCAM, files located in ALL in CPRD (=working directory)
library(data.table)
setwd("~/Brain and Cognitive Sciences/Research Internship 2/CPRD/All")
temp = list.files(pattern="*.csv")
options(datatable.fread.datatable=FALSE)
myfiles <- lapply(temp, fread)
for (i in 1:34) {
  names(myfiles) <- temp[1:34]
}

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

myfiles <- Map(cbind, myfiles, group = temp)
newmyfiles <- rbindlist(myfiles)


#
IS_patients_linked_comorbidities <- left_join(diseases_matching_IS,newmyfiles)
IS_patients_linked_comorbidities%>% group_by(patid,eventdate, medcode) %>% arrange(eventdate)%>% distinct() %>% ungroup-> IS_patients_linked_comorbidities
#IS_patients_linked_comorbidities <- IS_patients_linked_comorbidities %>% select(-textid, -practid, -consid, -constype, -sysdate)
IS_patients_other_comorbidities <- left_join(all_diseases_IS_patients, newmyfiles)
#IS_patients_other_comorbidities <- IS_patients_other_comorbidities %>% select(-textid, -practid, -consid, -constype, -sysdate, -enttype, -staffid, -episode, -adid)
PD_patients_comorbidities <- left_join(other_diseases_PD_patients, newmyfiles)
#PD_patients_comorbidities <- PD_patients_comorbidities %>% select(-textid, -practid, -consid, -constype, -sysdate, -enttype, -staffid, -episode, -adid)
#