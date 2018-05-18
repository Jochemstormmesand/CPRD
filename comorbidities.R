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
#All_disease_codes <- get_ClinicalCodes(article_id = c(1:70))
#Clinical_codes_def <- import_definitions(system.file("ehr_data", "res13-rheumatoid-arthritis.csv", package = "rEHR"))
#def2 <- import_definitions(system.file("extdata", "example_search.csv", package = "rpcdsearch"))
#medical_table <- read.delim("Lookups//medical.txt", fileEncoding="latin1", stringsAsFactors = FALSE)

#Alternative method to avoid WHERE IN .
all_diseases <- tbl_df(sqldf("SELECT * FROM Clinical_All", connection = db))
three_and_two_marker <- three_and_two %>% distinct(patid) #select All patids of patients that are properly on IS
all_diseases_IS_patients <- semi_join(all_diseases, three_and_two_marker) %>%#select from ALL clinical rows the ones where patid of patients with actual IS
  filter(constype ==3) 
#all_diseases_IS_patients <- tbl_df(sqldf("SELECT * FROM all_diseases_IS_patients WHERE constype ==3"))
IS_before_PD_marker <- IS_before_PD %>% select(patid) %>% mutate(marker = 1)
other_diseases_PD_patients <- semi_join(all_diseases, IS_before_PD_marker) %>%
  filter(constype ==3)
#other_diseases_PD_patients <- tbl_df(sqldf("SELECT * FROM other_diseases_PD_patients WHERE constype ==3"))
diseases_matching_IS <- semi_join(all_diseases, three_and_two, by = c("patid", "consid")) %>%
  filter(constype ==3)


#add readcodes 
all_diseases_IS_patients<-left_join(all_diseases_IS_patients, All_medcodes)
other_diseases_PD_patients<-left_join(other_diseases_PD_patients, All_medcodes)
diseases_matching_IS <-left_join(diseases_matching_IS, All_medcodes)






start this one over from here: make sure that all criteria from McLean2017 are met and that 
I follow these exactly. 
This include using medcode AND prodcode for both (see appendix in Barnett)
there they state for every condition how it should be recorded. mostly readcode, sometimes differnet.








# all_diseases_IS_patients <- tbl_df(select_events(db, tab = "Clinical_All", columns = '*', where = "patid %in% .(three_and_two_unique$patid) & constype == 3"))
# all_diseases_IS_patients <- tbl_df(left_join(all_diseases_IS_patients, All_medcodes))
# other_diseases_PD_patients <- tbl_df(select_events(db, tab = "Clinical_All_Sample", columns = '*', where = "patid %in% .(IS_before_PD$patid) & constype == 3")) # maybe also take out the  PD codes by saying & !medcode %in% .(Parkinsoncodes)
# other_diseases_PD_patients <- tbl_df(left_join(other_diseases_PD_patients, All_medcodes))
# diseases_matching_IS <- tbl_df(select_events(db, tab = "Clinical_All_Sample", columns = '*', where = "patid %in% .(three_and_two$patid) & consid %in% .(three_and_two$consid) & constype == 3"))
# diseases_matching_IS <- tbl_df(left_join(diseases_matching_IS, All_medcodes))
# diseases_matching_IS <- tbl_df(left_join(diseases_matching_IS, three_and_two))
# diseases_matching_IS <- diseases_matching_IS %>% select(-medduration, -cum_duration, -qty, -ndd, -unit, -cum_ndd0, -enttype, -staffid, -episode, -adid)
# nr_of_diseases_PP <- tbl_df(sqldf("SELECT patid, consid, COUNT(DISTINCT medcode) AS nr_of_diseases FROM diseases_matching_IS GROUP BY patid"))

#table(nr_of_diseases_PP$nr_of_diseases)


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
#Create new codelists from old ones, amending those and updating the existing ones. 
#read in all files from CPRDCAM, files located in ALL in CPRD (=working directory)
setwd("~/Brain and Cognitive Sciences/Research Internship 2/CPRD")
source("create_disease_df.R")
source("partial_join.R")

setwd("~/Brain and Cognitive Sciences/Research Internship 2/CPRD/All")
temp = list.files(pattern="*.csv")
options(datatable.fread.datatable=FALSE)
myfiles <- lapply(temp, fread)
for (i in 1:length(myfiles)) {
  names(myfiles) <- temp[1:length(myfiles)]
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
temp[26] <- "Prostate_disorders"
temp[27] <- "Psoriasis_or_eczema"
temp[28] <- "Peripheral_vascular_disease"
temp[29] <- "Rheumatoid_arthritis_other_inflammatory_polyarthropathies_systematic_connective_tissue_disorders"
temp[30] <- "Schizophrenia_and_related_non_organic_psychosis_or_bipolar_disorder"
temp[31] <- "Chronic_sinusitis"
temp[32] <- "Stroke_transient_ischaemic_attack"
temp[33] <- "Thyroid_disorders"

myfiles <- Map(cbind, myfiles, group = temp)
newmyfiles <- tbl_df(rbindlist(myfiles))

setwd("~/Brain and Cognitive Sciences/Research Internship 2/CPRD/All/Clinicalcodes.org")
temp2 = list.files(pattern="*.csv")
options(datatable.fread.datatable=FALSE)
myfiles2 <- lapply(temp2, fread)

temp2[1] <- "Synovitis"
temp2[2] <- "Mrsa"
temp2[3] <- "Otitis_Media"
temp2[4] <- "Urinary_tract_infection"
temp2[5] <- "Upper_respiratory_tract_infection"
temp2[6] <- "Osteoarthritis"
temp2[7] <- "Ankylosing_spondylitis"
temp2[8] <- "Jia"
temp2[9] <- "Systemic_lupus_erthematosus"
for (i in 1:length(temp2)) {
  names(myfiles2) <- temp2[1:length(temp2)]
}
myfiles2 <- lapply(myfiles2, function(x) { x["group"] <- NULL; x })
myfiles2 <- Map(cbind, myfiles2, group = temp2)
newmyfiles2 <- rbindlist(myfiles2)
newmyfiles2 <- tbl_df(full_join(newmyfiles, newmyfiles2))


#
IS_patients_linked_comorbidities <- left_join(diseases_matching_IS,newmyfiles2)
#IS_patients_linked_comorbidities%>% group_by(patid,eventdate, medcode) %>% arrange(eventdate)%>% distinct() %>% ungroup-> IS_patients_linked_comorbidities
#IS_patients_linked_comorbidities <- IS_patients_linked_comorbidities %>% select(-textid, -practid, -consid, -constype, -sysdate)
IS_patients_other_comorbidities <- left_join(all_diseases_IS_patients, newmyfiles2) 
#IS_patients_other_comorbidities <- IS_patients_other_comorbidities %>% select(-textid, -practid, -consid, -constype, -sysdate, -enttype, -staffid, -episode, -adid)
PD_patients_comorbidities <- left_join(other_diseases_PD_patients, newmyfiles2)
#PD_patients_comorbidities <- PD_patients_comorbidities %>% select(-textid, -practid, -consid, -constype, -sysdate, -enttype, -staffid, -episode, -adid)
#

unspecified_codes <- tbl_df(sqldf("SELECT medcode, readcode, desc FROM PD_patients_comorbidities WHERE Description IS NULL"))
unspecified_codes2 <- tbl_df(sqldf("SELECT medcode, readcode, desc FROM IS_patients_linked_comorbidities WHERE Description IS NULL"))
unspecified_codes3<- tbl_df(sqldf("SELECT medcode, readcode, desc FROM IS_patients_other_comorbidities WHERE Description IS NULL"))
unspecified_Codes_all <- tbl_df(rbind(unspecified_codes,unspecified_codes2))
#unspecified_Codes_all <- tbl_df(rbind(unspecified_Codes_all, unspecified_codes3))
unspecified_Codes_all <- tbl_df(rbind(unspecified_Codes_all, unspecified_codes3)) 


#find the prevalence of each unspecified code
nr_of_each_readcode <- tbl_df(sqldf("SELECT medcode, readcode, desc, COUNT(medcode) AS nr_of_codes FROM unspecified_Codes_all GROUP BY medcode ORDER BY nr_of_codes DESC"))
nr_of_pp_Immunosup <- tbl_df(sqldf("SELECT prodcode, COUNT(DISTINCT patid) AS patients FROM SelImmuneProducts GROUP BY prodcode ORDER BY patients DESC"))

specified_codes <- tbl_df(sqldf("SELECT medcode, readcode, desc FROM PD_patients_comorbidities WHERE Description IS NOT NULL"))
specified_codes2 <- tbl_df(sqldf("SELECT medcode, readcode, desc FROM IS_patients_linked_comorbidities WHERE Description IS NOT NULL"))
specified_codes3<- tbl_df(sqldf("SELECT medcode, readcode, desc FROM IS_patients_other_comorbidities WHERE Description IS NOT NULL"))
specified_Codes_all <- tbl_df(rbind(specified_codes,specified_codes2))
#specified_Codes_all <- tbl_df(rbind(specified_Codes_all, specified_codes3)) 
specified_Codes_all <- tbl_df(rbind(specified_Codes_all, specified_codes3)) %>% distinct()


#find descriptions containing a certain part, and see what group they belong in. think of a method to do htis systematically over 
# all 800 missing codes. 
dplyr::filter(newmyfiles, grepl('isch', Description, ignore.case = TRUE))
x <- sort(c("seen","refer" ,"review", "convert","code", "monitor", "consult", "discus", "screen", "annual", "issue", "repeat", "prescri", "advice", "inject","risk",
       "therapy", "clinical", "FH", "dose", "using","use", "gene", "complic", "high", "scan", "known", "did","attend","MED3","removal", "baby", "wound",
       "family", "history", "target", "medic", "treat", "letter", "from", "specialist", "relevant", "body mass index", "bmi", "home","visit", "ques",
       "primary", "care", "give","comment","follow", "encounter", "adminis", "speci", "H/O", "patient", "pain", "result", "traum", "other", "fracture",
       "feel", "c/o", "excision", "carer", "appoint", "communi", "asses", "symptom", "plan", "under","diagno", "o/e", "complete", "signed", 
       "hospital", "health", "shoulder","laboratory","procedure", "common", "migraine","blood", "pressure","confirm"))
nr_of_each_readcode2 <- dplyr::filter(nr_of_each_readcode, !grepl(paste(x,collapse="|"), desc, ignore.case = TRUE))

#terms for creating new codelists
Chest_infection <- c("pneumonia","lower respiratory tract infection", "bronchitis", "tracheitis", "chest infection")
Other_infections_mild <- c("tonsilitis","tonsillitis", "cellulitis", "skin infection", "otitis", "conjuctivitis", "conjunctivitis", 
"pharyngitis", "intertrigo", "candida", "viral infection NOS", "abscess", "paronychia", "athlete's foot",
"thrush","scabies", "tinea",  "infection","moluscum contagiosum", "enterobiasis", "thread worm", "mumps",
"septic arthritis", "chickenpox", "influenza", "sinusitis", "acute laryngotracheitis", "infectious mononucleosis",
"glandular fever", "osteomyelitis", "infective colitis")
Other_infections_severe <-  c("hepatitis", "meningitis", "cholangitis")
GI_infections <- c("gasteroenteritis", "diarrhoea", "vomiting", "gastrointestinal tract infection", "gastritis", 
                   "duodenitis", "gastroenteritis", "noninfective enteritis and colitis", "gastro-oesophagal", 
                   "reflux", "oesophagitis", "colitis")
Chronic_renal_failure <- c("chronic renal failure")#already a group?/check clinicalcodes.org
Autoimmune_other <- c("myasthenia gravis", "fibrosing alveolitis", "sarcoidosis", 
"erythema nodosum", "coeliac disease", "uveitis", "episcleritis", "pulmonary fibrosis",
"antiphospholipid syndrome", "guillain barre", "hashimoto's", "thyroiditis", "dermatitis herpetiformis")
Restless_leg_syndrome <- c("restless legs")
Constipation <- c("constipation")
Tuberculosis <- c("tuberculosis", "TB") #DONT IGNORE CASE!!
Minor_head_injury <- c("head injury", "head trauma")
Benign_essential_tremor <- c("benign essential tremor")
Vasculitis <- c("vasculitis", "arteritis", "nodosa") 

#terms to append to lists
Urinary_tract_infection <- c("cholecystitis", "cystitis") 
Synovitis <- "synovitis"
Ankylosing_spondylitis <- "spondy"
Osteoarthritis<- c("osteoarthritis", "osteoarthitis")
Rheumatoid_arthritis_other_inflammatory_polyarthropathies_systematic_connective_tissue_disorders  <- c("sicca", "sjogren","arthritis") #(NOT osteoarthritis though...)
Irritable_bowel_syndrome <- "colitis"
Upper_respiratory_tract_infection <- c("upper respiratory tract infection", "acute respiratory")

#overlap ankylosing and rheuma. synovitis and rheuma
appending <- c("Urinary_tract_infection", "Synovitis", "Ankylosing_spondylitis", "Osteoarthritis",
               "Rheumatoid_arthritis_other_inflammatory_polyarthropathies_systematic_connective_tissue_disorders", 
               "Irritable_bowel_syndrome", "Upper_respiratory_tract_infection")    
appending_list <- list(Urinary_tract_infection, Synovitis, Ankylosing_spondylitis, Osteoarthritis,
                       Rheumatoid_arthritis_other_inflammatory_polyarthropathies_systematic_connective_tissue_disorders,
                       Irritable_bowel_syndrome, Upper_respiratory_tract_infection)

x2 <- c("Chest_infection", "Other_infections_mild", "Other_infections_severe", 
        "GI_infections", "Chronic_renal_failure", "Autoimmune_other", "Restless_leg_syndrome", "Constipation",
        "Tuberculosis", "Minor_head_injury", "Benign_essential_tremor", "Vasculitis")
xlist <- list(Chest_infection, Other_infections_mild, Other_infections_severe, 
              GI_infections, Chronic_renal_failure, Autoimmune_other, Restless_leg_syndrome, Constipation,
              Tuberculosis, Minor_head_injury, Benign_essential_tremor, Vasculitis)
excl <- c("seen","refer" ,"review","advised", "FH", "vacc", "consent", "needs", "carrier", "H/O") #include "Carrier"?



xlist <- create_disease_df(xlist, x2)
appending_list <- create_disease_df(appending_list, appending)




#something is still wrong with this method: not all PD codes are included, some PD patients are not recognized. 
#probably also counts for other diseases. Maybe has to do with overlap in groups. PD dementia is for example 
#categorized in dementia and not PD even though it is a PD inclusion code. 
newlists <- partial_join(unspecified_Codes_all, xlist, by_x ="desc", pattern_y = "descr", excl)
append_lists <- partial_join(unspecified_Codes_all, appending_list, by_x= "desc", pattern_y = "descr",excl)
newmyfiles3 <- tbl_df(full_join(newmyfiles2, newlists))
newmyfiles3 <- tbl_df(full_join(newmyfiles3, append_lists)) %>% arrange(group)


#  find duplicates and see which are needed: 
# Manually decide which go where!
duplicates <- newmyfiles3[duplicated(newmyfiles3$readcode, by = "Description"), ]
duplicates <- tbl_df(sqldf("SELECT * FROM newmyfiles3 WHERE readcode IN (SELECT readcode FROM duplicates)")) %>% group_by(Description) %>% distinct(group, .keep_all = TRUE) %>% arrange(readcode) %>% ungroup()
# Write CSV in R
library(xlsx)
write.xlsx(duplicates, file = "duplicates.xlsx")
#library(purrr)
#create new files for newly created codelists to publish. 
codelists <- split(newmyfiles3, f = newmyfiles3$group) #48? 54? hoeveelmoeten het er nou zijn...
lapply(names(codelists), function(df) write.csv(codelists[[df]], file=paste0(df, ".csv")))



#use the previously made codelists!!
setwd("~/Brain and Cognitive Sciences/Research Internship 2/CPRD/All/new_codelists")
temp3 = list.files(pattern="*.csv")
options(datatable.fread.datatable=FALSE)
myfiles3 <- lapply(temp3, fread)
for ( i in 1:54){
  names(myfiles3)  <- temp3[1:54]}

#probably many more need to be grouped in the infections group instead of elsewehere:
# e.g. GI infections, chest infection etc..
#According to Cumulative Illness Rating Scale.
  # Cardiac <- list(Atrial_fibrilation = myfiles3$`Atrial fibrillation.csv` , CHD = myfiles3$Coronary_heart_disease.csv, HF = myfiles3$Heart_failure.csv) 
  # Hypertension <- list(Hypertension = myfiles3$Hypertension.csv)
  # Vascular <- list(Vasculitis =myfiles3$Vasculitis.csv, Peripheral_vascular_disease = myfiles3$Peripheral_vascular_disease.csv) #should contain leukemias an dlymphomas
  # Respiratory <- list(Chest_infection = myfiles3$Chest_infection.csv, TB = myfiles3$Tuberculosis, COPD = myfiles3$COPD.csv, URTI = myfiles3$Upper_respiratory_tract_infection.csv, Asthma = myfiles3$Asthma_currently_treated.csv,Bronchiectasis = myfiles3$Bronchiectasis.csv)
  # ENT <- list(Hearing_loss = myfiles3$Hearing_loss.csv, Chronic_sinusitis = myfiles3$Chronic_sinusitis.csv, Blindness = myfiles3$`Blindness and low vision.csv`, Otitis_Media= myfiles3$Otitis_Media.csv)
  # UpperGI <- list(GI_infection = myfiles3$GI_infections.csv) #should contain various forms of cancer
  # LowerGI <- list(Irritable_bowel_syndrome =myfiles3$Irritable_bowel_syndrome.csv, Inflammatory_bowel_disease =myfiles3$Inflammatory_bowel_disease.csv, Constipation = myfiles3$Constipation.csv, Diverticular = myfiles3$Diverticular_disease_of_intestine.csv)
  # Hepatic <- list(Chronic_liver_disease = myfiles3$Chronic_Liver_Disease_and_Viral_Hepatitis.csv)
  # Renal<- list(Chronic_kidney_disease = myfiles3$Chronic_kidney_disease.csv, Chronic_renal_failure = myfiles3$Chronic_renal_failure.csv)
  # OtherGU <- list(Prostate_disorders = myfiles3$Prostate_disorders.csv, Urinary_tract_infection = myfiles3$Urinary_tract_infection.csv)
  # Musculo_Skeletal_Integumentary <- list(Osteoarthritis = myfiles3$Osteoarthritis.csv, Arthritis = myfiles3$Rheumatoid_arthritis_other_inflammatory_polyarthropathies_systematic_connective_tissue_disorders.csv, Jia= myfiles3$Jia.csv, Ankylosing_spondylitis = myfiles3$Ankylosing_spondylitis.csv, Psoriasis = myfiles3$Psoriasis_or_eczema.csv, Synovitis = myfiles3$Synovitis.csv)
  # Neurological <- list(Minor_head_injury = myfiles3$Minor_head_injury.csv, Restless_leg_syndrome = myfiles3$Restless_leg_syndrome.csv,Benign_essential_tremor = myfiles3$Benign_essential_tremor.csv,Epilepsy = myfiles3$Epilepsy.csv, Stroke = myfiles3$Stroke_transient_ischaemic_attack.csv,MS = myfiles3$` Multiple_sclerosis.csv`)
  # Endocrine_Metabolic <- list(Thyroid = myfiles3$Thyroid_disorders.csv, Diabetes = myfiles3$Diabetes.csv)
  # Psychiatric_Behavioural <- list(Learning_disability = myfiles3$Learning_disability.csv, Psychoactive_substance_misuse = myfiles3$Psychoactive_substance_misuse_NOT_ALCOHOL.csv, Alcohol_Problems = myfiles3$Alcohol_problems.csv, Anxiety = myfiles3$Anxiety_and_other_neurotic_stress_related_and_somatoform_disorders.csv, Depression = myfiles3$Depression.csv, Dementia = myfiles3$Dementia.csv,Schizophrenia_psychosis_bipolar =myfiles3$Schizophrenia_and_related_non_organic_psychosis_or_bipolar_disorder.csv, Anorexia_bulemia = myfiles3$Anorexia_or_bulimia.csv)
  #   Cancer <- list(Cancer = myfiles3$Cancer_New_diagnosis_in_last_five_years.csv)
  #   Autoimmune<- list(Autoimmune_other = myfiles3$Autoimmune_other.csv, Lupus = myfiles3$Systemic_lupus_erthematosus.csv)
  #   Mrsa <- list(Mrsa = myfiles3$Mrsa.csv)
  #   Infections <- list(Severe_infections= myfiles3$Other_infections_severe.csv, Mild_infections = myfiles3$Other_infections_mild.csv)

      # Grouped_diseases <- list(Cardiac, Hypertension, Vascular, Respiratory, ENT, UpperGI, LowerGI, Hepatic, Renal, OtherGU, Musculo_Skeletal_Integumentary,
      #                          Neurological, Endocrine_Metabolic, Psychiatric_Behavioural, Cancer, Autoimmune, Mrsa, Infections)
      # Grouped_diseases_names <- c("Cardiac", "Hypertension", "Vascular", "Respiratory","ENT","UpperGI","LowerGI",
      #                             "Hepatic", "Renal", "OtherGU", "Musculo_Skeletal_Integumentary","Neurological","Endocrine_Metabolic","Psychiatric_Behavioural",
      #                             "Cancer", "Autoimmune", "Mrsa", "Infections")


            # for (i in 1:14) {
            #   names(Grouped_diseases) <- Grouped_diseases_names[1:14]
            # }
            # Grouped_diseases_list <- unlist(Grouped_diseases, recursive = FALSE)
            # Grouped_diseases_df <- rbindlist(Grouped_diseases_list, idcol = "Large_group")
            # Grouped_diseases_df$Large_group <- sapply(strsplit(as.character(Grouped_diseases_df$Large_group), "\\."), `[`, 1)
            # Grouped_diseases_df <- Grouped_diseases_df[,c(3,4,5,6,7,1)]



Constipation <- list(Constipation = myfiles3$Constipation.csv)
Viral_hepatitis #PROBLEM: I have this in 1 group with chronic liver disease...
Epilepsy <- list(Epilepsy = myfiles3$Epilepsy.csv)
Multiple_sclerosis <- list(MS = myfiles3$` Multiple_sclerosis.csv`)
Blindness_low_vision <- list(Blindness = myfiles3$`Blindness and low vision.csv`)
Stroke_and_transient_ischaemic_attack <- list(Stroke = myfiles3$Stroke_transient_ischaemic_attack.csv)
Painful_condition
Irritable_bowel_syndrome <- list(Irritable_bowel_syndrome =myfiles3$Irritable_bowel_syndrome.csv)
Chronic_liver_disease <- list(Chronic_liver_disease = myfiles3$Chronic_Liver_Disease_and_Viral_Hepatitis.csv)
Coronary_heart_disease <- list(CHD = myfiles3$Coronary_heart_disease.csv)
Prostate_disorders <- list(Prostate_disorders = myfiles3$Prostate_disorders.csv)
Heart_failure <- list(HF = myfiles3$Heart_failure.csv) 
Cancer_new_diagnosis <- list(Cancer = myfiles3$Cancer_New_diagnosis_in_last_five_years.csv)
Thyroid_disorders <- list(Thyroid = myfiles3$Thyroid_disorders.csv)
Glaucoma
Treated_dyspepsia
Peripheral_vascular_disease <- list(Peripheral_vascular_disease = myfiles3$Peripheral_vascular_disease.csv)
Diverticular_disease_of_intestine <- list(Diverticular = myfiles3$Diverticular_disease_of_intestine.csv)
Atrial_fibrillation <- list(Atrial_fibrilation = myfiles3$`Atrial fibrillation.csv`)
Diabetes <- list(Diabetes = myfiles3$Diabetes.csv)
Rheumatoid_arthritis <- list(Arthritis = myfiles3$Rheumatoid_arthritis_other_inflammatory_polyarthropathies_systematic_connective_tissue_disorders.csv)
Inflammatory_bowel_disease <- list(Inflammatory_bowel_disease =myfiles3$Inflammatory_bowel_disease.csv) 
Hearing_loss <- list(Hearing_loss = myfiles3$Hearing_loss.csv)
Psoriasis_or_eczema <- list(Psoriasis = myfiles3$Psoriasis_or_eczema.csv)
Chronic_sinusitis <- list(Chronic_sinusitis = myfiles3$Chronic_sinusitis.csv)
Asthma <- list(Asthma = myfiles3$Asthma_currently_treated.csv)
Migraine
Hypertension <- list(Hypertension = myfiles3$Hypertension.csv)
Bronchiectasis <- list(Bronchiectasis = myfiles3$Bronchiectasis.csv)
Chronic_kidney_disease <- list(Chronic_kidney_disease = myfiles3$Chronic_kidney_disease.csv)

Schizophrenia <- list(Schizophrenia_psychosis_bipolar =myfiles3$Schizophrenia_and_related_non_organic_psychosis_or_bipolar_disorder.csv)
Dementia <- list(Dementia = myfiles3$Dementia.csv)
Learning_disability <- list(Learning_disability = myfiles3$Learning_disability.csv)
Depression <- list(Depression = myfiles3$Depression.csv)
Anxiety <- list(Anxiety = myfiles3$Anxiety_and_other_neurotic_stress_related_and_somatoform_disorders.csv)
Alcohol_problems <- list(Alcohol_Problems = myfiles3$Alcohol_problems.csv)
Anorexia_bulimia <- list(Anorexia_bulemia = myfiles3$Anorexia_or_bulimia.csv)

#these codes are in the appendix of the Lancet paper 2012,
#but not in the paper from 2017 (co and multimorbidity) 
Chronic_obstructive_pulmonary_disease
Psychoactive_substance_misuse
Parkinsons_disease

Grouped_physical_conditions <- list(Constipation, Epilepsy, Multiple_sclerosis, Blindness_low_vision, Stroke_and_transient_ischaemic_attack, 
                         Irritable_bowel_syndrome, Chronic_liver_disease, Coronary_heart_disease, Prostate_disorders, Heart_failure,
                         Cancer_new_diagnosis, Thyroid_disorders,Peripheral_vascular_disease, Diverticular_disease_of_intestine, 
                         Atrial_fibrillation, Diabetes, Rheumatoid_arthritis, Inflammatory_bowel_disease, Hearing_loss, 
                         Psoriasis_or_eczema, Chronic_sinusitis, Asthma, Hypertension, Bronchiectasis, Chronic_kidney_disease)
Grouped_mental_conditions <- list(Schizophrenia, Dementia,Learning_disability, Depression, Anxiety, Alcohol_problems, Anorexia_bulimia)
Grouped_physical_conditions_names <- list("Constipation", "Epilepsy", "Multiple_sclerosis", "Blindness_low_vision", "Stroke_and_transient_ischaemic_attack",
                                    "Irritable_bowel_syndrome", "Chronic_liver_disease", "Coronary_heart_disease", "Prostate_disorders", "Heart_failure",
                                    "Cancer_new_diagnosis","Thyroid_disorders","Peripheral_vascular_disease", "Diverticular_disease_of_intestine", 
                                    "Atrial_fibrillation", "Diabetes", "Rheumatoid_arthritis", "Inflammatory_bowel_disease", "Hearing_loss", 
                                    "Psoriasis_or_eczema", "Chronic_sinusitis", "Asthma", "Hypertension", "Bronchiectasis", "Chronic_kidney_disease")
Grouped_mental_conditions_names <- list("Schizophrenia", "Dementia","Learning_disability", "Depression", "Anxiety", "Alcohol_problems", "Anorexia_bulimia")

for (i in 1:length(Grouped_physical_conditions)) {
  names(Grouped_physical_conditions) <- Grouped_physical_conditions_names[1:length(Grouped_physical_conditions)]
}
Grouped_physical_conditions_list <- unlist(Grouped_physical_conditions, recursive = FALSE)
Grouped_physical_conditions_df <- rbindlist(Grouped_physical_conditions_list, idcol = "Large_group")
Grouped_physical_conditions_df$Large_group <- sapply(strsplit(as.character(Grouped_physical_conditions_df$Large_group), "\\."), `[`, 1)
Grouped_physical_conditions_df <- Grouped_physical_conditions_df[,c(3,4,5,6,7,1)]

for (i in 1:length(Grouped_mental_conditions)) {
  names(Grouped_mental_conditions) <- Grouped_mental_conditions_names[1:length(Grouped_mental_conditions)]
}
Grouped_mental_conditions_list <- unlist(Grouped_mental_conditions, recursive = FALSE)
Grouped_mental_conditions_df <- rbindlist(Grouped_mental_conditions_list, idcol = "Large_group")
Grouped_mental_conditions_df$Large_group <- sapply(strsplit(as.character(Grouped_mental_conditions_df$Large_group), "\\."), `[`, 1)
Grouped_mental_conditions_df <- Grouped_mental_conditions_df[,c(3,4,5,6,7,1)]



nr_of_diseases_PP <- left_join(diseases_matching_IS, Grouped_physical_conditions_df)
nr_of_diseases_PP <- nr_of_diseases_PP %>% filter(!is.na(group)) %>% group_by(patid) %>% summarise(nr_of_diseases = n_distinct(Large_group)) %>% arrange(desc(nr_of_diseases))
#comorbidities of PD patients:
graph_other_diseases_PD_patients <- left_join(other_diseases_PD_patients, Grouped_physical_conditions_df) %>%filter(!is.na(group)) 
    summary_of_diseases_pp <- graph_other_diseases_PD_patients %>% group_by(patid) %>% summarise(nr_of_diseases = n_distinct(Large_group)) %>% arrange(desc(nr_of_diseases))
    summary_of_pp_disease <- graph_other_diseases_PD_patients %>% group_by(Large_group) %>% summarise(nr_of_patients = n_distinct(patid)) %>% arrange(desc(nr_of_patients))
    summary_of_pp_disease$group <- factor(summary_of_pp_disease$Large_group, levels = unique(summary_of_pp_disease$Large_group)[order(summary_of_pp_disease$nr_of_patients, decreasing = TRUE)])
graph_IS_matched_diseases <- left_join(diseases_matching_IS, Grouped_physical_conditions_df) %>% filter(!is.na(group))
    summary_of_ISmatched_diseases_PP <- graph_IS_matched_diseases %>% group_by(patid) %>% summarise(nr_of_diseases = n_distinct(Large_group)) %>% arrange(desc(nr_of_diseases))
    summary_of_PP_ISmatched_disease <- graph_IS_matched_diseases %>% group_by(Large_group) %>% summarise(nr_of_patients = n_distinct(patid)) %>% arrange(desc(nr_of_patients))
    summary_of_PP_ISmatched_disease$Large_group <- factor(summary_of_PP_ISmatched_disease$Large_group, levels = unique(summary_of_PP_ISmatched_disease$Large_group)[order(summary_of_PP_ISmatched_disease$nr_of_patients, decreasing = TRUE)])
    
    
    summary_of_PP_ISmatched_disease_grouped <- tbl_df(merge(x = graph_IS_matched_diseases, y = three_and_two[ , c("patid", "prodcode", "Drug", "consid")], by = c("patid", "consid"), all.x=TRUE)) %>% group_by(Drug, Large_group) %>% summarise(nr_of_patients = n_distinct(patid)) %>% arrange(desc(Drug,nr_of_patients))
    summary_of_PP_ISmatched_disease_grouped$group <- factor(summary_of_PP_ISmatched_disease_grouped$Large_group, levels = unique(summary_of_PP_ISmatched_disease_grouped$Large_group)[order(summary_of_PP_ISmatched_disease_grouped$nr_of_patients, decreasing = TRUE)])
    
        
subplot(
plot_ly(
  x = summary_of_PP_ISmatched_disease_grouped$Large_group[grepl("Azathioprine",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)],
  y = summary_of_PP_ISmatched_disease_grouped$nr_of_patients[grepl("Azathioprine",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)], name = "Azathioprine",
  type = "bar"),
plot_ly(
  x = summary_of_PP_ISmatched_disease_grouped$Large_group[grepl("Methotrexate",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)],
  y = summary_of_PP_ISmatched_disease_grouped$nr_of_patients[grepl("Methotrexate",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)],name = "Methotrexate",
  type = "bar"),
plot_ly(
  x = summary_of_PP_ISmatched_disease_grouped$Large_group[grepl("Ciclosporin",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)],
  y = summary_of_PP_ISmatched_disease_grouped$nr_of_patients[grepl("Ciclosporin",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)], name = "Ciclosporin",
  type = "bar"),
plot_ly(
  x = summary_of_PP_ISmatched_disease_grouped$Large_group[grepl("Mycophenolate",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)],
  y = summary_of_PP_ISmatched_disease_grouped$nr_of_patients[grepl("Mycophenolate",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)], name = "Mycophenolate mofetil",
  type = "bar"),
plot_ly(
  x = summary_of_PP_ISmatched_disease_grouped$Large_group[grepl("Etanercept",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)],
  y = summary_of_PP_ISmatched_disease_grouped$nr_of_patients[grepl("Etanercept",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)], name = "Etanercept",
  type = "bar"),
plot_ly(
  x = summary_of_PP_ISmatched_disease_grouped$Large_group[grepl("Rituximab",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)],
  y = summary_of_PP_ISmatched_disease_grouped$nr_of_patients[grepl("Rituximab",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)],name = "Rituximab",
  type = "bar"),
plot_ly(
  x = summary_of_PP_ISmatched_disease_grouped$Large_group[grepl("Tacrolimus",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)],
  y = summary_of_PP_ISmatched_disease_grouped$nr_of_patients[grepl("Tacrolimus",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)], name = "Tacrolimus",
  type = "bar"),
plot_ly(
  x = summary_of_PP_ISmatched_disease_grouped$Large_group[grepl("Cyclophosphamide",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)],
  y = summary_of_PP_ISmatched_disease_grouped$nr_of_patients[grepl("Cyclophosphamide",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)], name = "Cyclophosphamide Monohydrate",
  type = "bar"),
plot_ly(
  x = summary_of_PP_ISmatched_disease_grouped$Large_group[grepl("Adalimumab",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)],
  y = summary_of_PP_ISmatched_disease_grouped$nr_of_patients[grepl("Adalimumab",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)], name = "Adalimumab",
  type = "bar"),
plot_ly(
  x = summary_of_PP_ISmatched_disease_grouped$Large_group[grepl("Infliximab",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)],
  y = summary_of_PP_ISmatched_disease_grouped$nr_of_patients[grepl("Infliximab",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)], name = "Infliximab",
  type = "bar"),shareX = TRUE, shareY = TRUE) %>% 
layout(title = "Comorbidities per Drug",
       yaxis = list(title = 'Nr of Patients per Comorbidity'))
#plot_ly(
#  x = summary_of_PP_ISmatched_disease_grouped$Large_group[grepl("Alemtuzumab",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)],
#  y = summary_of_PP_ISmatched_disease_grouped$nr_of_patients[grepl("Alemtuzumab",summary_of_PP_ISmatched_disease_grouped$Drug, ignore.case = TRUE)], name = "Alemtuzumab",
#  type = "bar"), 


  
  
  
pp_disease_IS <- plot_ly(
    x = summary_of_PP_ISmatched_disease$Large_group,
    y = summary_of_PP_ISmatched_disease$nr_of_patients,
    name = "Comorbidities",
    type = "bar") %>% 
    layout(title = "Number of Patients per Comorbidity",
            yaxis = list(title = 'Nr of comorbidities'))
    
pp_disease <- plot_ly(
  x = summary_of_pp_disease$Large_group,
  y = summary_of_pp_disease$nr_of_patients,
  name = "Comorbidities",
  type = "bar") %>% 
  layout(title = "Number of Patients per Comorbidity",
         yaxis = list(title = 'Nr of comorbidities'))
    
diseases_pp<- ggplotly(ggplot(summary_of_diseases_pp, aes(x=nr_of_diseases)) + geom_histogram(binwidth=1, colour="black", fill="white") +
  geom_vline(data=summary_of_diseases_pp, aes(xintercept=mean(summary_of_diseases_pp$nr_of_diseases, na.rm = T)),
             linetype="dashed", size=1, colour="red"))


diseases_pp <- plot_ly(x = summary_of_diseases_pp$nr_of_diseases,
                       type = "histogram") %>% 
  layout(title = "Number of Patients per Comorbidity",
         yaxis = list(title = 'Nr of comorbidities'),
         xaxis = list(title = 'Disease'))
  

  
newlists <- unique(filter(PD_patients_comorbidities, grepl(paste(Parkinsoncodes,collapse="|"), medcode, ignore.case = TRUE))) %>% arrange(medcode)

#still check or append the following: 
check <- print(unique(filter(unspecified_Codes_all, grepl(paste("sjog",collapse="|"), desc, ignore.case = TRUE))), n = 100)
print(anti_join(check, newlists) %>% distinct(medcode, .keep_all = TRUE), n =80)



#2025 codes are correctly classified and grouped, --> adds up to 0.85 million rows (425x)
#whilst 29294 (grepl = 23600) unique codes are not --> adds up to a total of 6.1 million rows (grepl = 3.1) (208x) 
