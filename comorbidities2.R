Comorbidities2

# using http://www.phpc.cam.ac.uk/pcu/cprd_cam/codelists/

setwd("~/Brain and Cognitive Sciences/Research Internship 2/CPRD/All/Final_codelists")

temp = list.files(pattern="*.csv")
options(datatable.fread.datatable=FALSE)
myfiles <- lapply(temp, fread)
for (i in 1:length(myfiles)) {
  names(myfiles) <- temp[1:length(myfiles)]
}
myfiles <- lapply(myfiles, function(x) { x["group"] <- NULL; x })
myfiles <- lapply(myfiles, function(x) { x["V1"] <- NULL; x })
myfiles <- Map(cbind, myfiles, group = temp)
newmyfiles <- tbl_df(rbindlist(myfiles))

setwd("~/Brain and Cognitive Sciences/Research Internship 2/CPRD/All/Final_codelists/prodcodes")

temp2 = list.files(pattern="*.csv")
options(datatable.fread.datatable=FALSE)
myfiles_prodcodes <- lapply(temp2, fread)
for (i in 1:length(myfiles_prodcodes)) {
  names(myfiles_prodcodes) <- temp2[1:length(myfiles_prodcodes)]
}
myfiles_prodcodes <- Map(cbind, myfiles_prodcodes, group = temp2)
newmyfiles_prodcodes <- tbl_df(rbindlist(myfiles_prodcodes))
newmyfiles_prodcodes$group <- sub('_prodcodes.*$','', newmyfiles_prodcodes$group)
newmyfiles_prodcodes$group <- sub('.csv.*$','', newmyfiles_prodcodes$group)
newmyfiles$group <- sub('.csv.*$','', newmyfiles$group)

all_diseases <- tbl_df(sqldf("SELECT * FROM Clinical_All", connection = db))
all_medication <- tbl_df(sqldf("SELECT patid, eventdate, consid, prodcode FROM Therapy_All", connection = db))

three_and_two_marker <- three_and_two %>% distinct(patid) #select All patids of patients that are properly on IS
all_diseases_IS_patients <- semi_join(all_diseases, three_and_two_marker) %>%#select from ALL clinical rows the ones where patid of patients with actual IS
  filter(constype ==3) 
#all_diseases_IS_patients <- tbl_df(sqldf("SELECT * FROM all_diseases_IS_patients WHERE constype ==3"))
IS_before_PD_marker <- Final_True_PD %>% distinct(patid)
other_diseases_PD_patients <- semi_join(all_diseases, IS_before_PD_marker) %>%
  filter(constype ==3)
#other_diseases_PD_patients <- tbl_df(sqldf("SELECT * FROM other_diseases_PD_patients WHERE constype ==3"))
diseases_matching_IS <- semi_join(all_diseases, three_and_two, by = c("patid", "consid")) %>%
  filter(constype ==3)


#add readcodes 
all_diseases_IS_patients<-left_join(all_diseases_IS_patients, All_medcodes)
other_diseases_PD_patients<-left_join(other_diseases_PD_patients, All_medcodes)
diseases_matching_IS <-left_join(diseases_matching_IS, All_medcodes)

#look at medication
all_medication_IS_patients <- semi_join(all_medication, three_and_two_marker) 
other_medication_PD_patients <- semi_join(all_medication, IS_before_PD_marker)
medication_matching_IS <- semi_join(all_medication, three_and_two, by = c("patid", "consid"))
newmyfiles_prodcodes$prodcode <- as.integer(newmyfiles_prodcodes$prodcode)
All_IS_medication <- inner_join(newmyfiles_prodcodes,all_medication_IS_patients)
All_IS_medication <- All_IS_medication %>% group_by(patid, prodcode) %>% mutate(nr_of_drugs = n())
All_IS_medication <- All_IS_medication%>% select(-gemscriptcode, -CodingSystem)
Other_PD_medication <- inner_join(newmyfiles_prodcodes, other_medication_PD_patients)
Other_PD_medication <- Other_PD_medication %>% group_by(patid, prodcode) %>% mutate(nr_of_drugs = n())
Other_PD_medication <- Other_PD_medication%>% select(-gemscriptcode, -CodingSystem)
Linked_IS_medication <- inner_join(newmyfiles_prodcodes,medication_matching_IS)
Linked_IS_medication <- Linked_IS_medication %>% group_by(patid, prodcode) %>% mutate(nr_of_drugs = n())
Linked_IS_medication <- Linked_IS_medication%>% select(-gemscriptcode, -CodingSystem)

IS_patients_linked_comorbidities <- left_join(diseases_matching_IS,newmyfiles)
#IS_patients_linked_comorbidities%>% group_by(patid,eventdate, medcode) %>% arrange(eventdate)%>% distinct() %>% ungroup-> IS_patients_linked_comorbidities
#IS_patients_linked_comorbidities <- IS_patients_linked_comorbidities %>% select(-textid, -practid, -consid, -constype, -sysdate)
IS_patients_other_comorbidities <- left_join(all_diseases_IS_patients,newmyfiles) %>% filter(!is.na(group))
#IS_patients_other_comorbidities <- IS_patients_other_comorbidities %>% select(-textid, -practid, -consid, -constype, -sysdate, -enttype, -staffid, -episode, -adid)
PD_patients_comorbidities <- left_join(other_diseases_PD_patients, newmyfiles)
#PD_patients_comorbidities <- PD_patients_comorbidities %>% select(-textid, -practid, -consid, -constype, -sysdate, -enttype, -staffid, -episode, -adid)
#

amitriptyline <- unique(All_IS_medication$productname[grepl("amitriptyline",All_IS_medication$productname, ignore.case = TRUE)])
amitriptyline <- unique(Linked_IS_medication$productname[grepl("amitriptyline",Linked_IS_medication$productname, ignore.case = TRUE)])

#ALL PATIENTS.
Get_comorbidities2 <- IS_patients_other_comorbidities %>% select(-Description, -CodingSystem, -constype, -practid, -readcode) %>%
  mutate(disease_group = case_when(
  IS_patients_other_comorbidities$group == "Hypertension" ~ "Hypertension",
  IS_patients_other_comorbidities$group == "Coronary_heart_disease" ~ "Coronary_heart_disease",
  IS_patients_other_comorbidities$group == "Diabetes" ~ "Diabetes",
  IS_patients_other_comorbidities$group == "Thyroid_disorders" ~ "Thyroid_disorders",
  IS_patients_other_comorbidities$group == "Rheumatoid_arthritis" ~ "Rheumathoid_arthritis",
  IS_patients_other_comorbidities$group == "Hearing_loss" ~ "Hearing_loss",
  IS_patients_other_comorbidities$group == "COPD" ~ "COPD",
  IS_patients_other_comorbidities$group == "Alcohol_problems" ~ "Alcohol_problems",
  IS_patients_other_comorbidities$group == "Psychoactive_substance_misuse_NOT_ALCOHOL" ~ "Psychoactive_substance_misuse_NOT_ALCOHOL",
  IS_patients_other_comorbidities$group == "Stroke_transient_ischaemic_attack" ~ "Stroke_transient_ischaemic_attack",
  IS_patients_other_comorbidities$group == "Chronic_kidney_disease" ~ "Chronic_kidney_disease",
  IS_patients_other_comorbidities$group == "Diverticular_disease_of_intestine" ~ "Diverticular_disease_of_intestine",
  IS_patients_other_comorbidities$group == "Atrial fibrillation" ~ "Atrial fibrillation",
  IS_patients_other_comorbidities$group == "Peripheral_vascular_disease" ~ "Peripheral_vascular_disease",
  IS_patients_other_comorbidities$group == "Heart_failure" ~ "Heart_failure",
  IS_patients_other_comorbidities$group == "Prostate_disorders" ~ "Prostate_disorders",
  IS_patients_other_comorbidities$group == "Blindness and low vision" ~ "Blindness and low vision",
  IS_patients_other_comorbidities$group == "Dementia" ~ "Dementia",
  IS_patients_other_comorbidities$group == "Chronic_sinusitis" ~ "Chronic_sinusitis",
  IS_patients_other_comorbidities$group == "Learning_disability" ~ "Learning_disability",
  IS_patients_other_comorbidities$group == "Anorexia_or_bulimia" ~ "Anorexia_or_bulimia",
  IS_patients_other_comorbidities$group == "Bronchiectasis" ~ "Bronchiectasis",
  IS_patients_other_comorbidities$group == "Multiple_sclerosis" ~ "Multiple_sclerosis",
  IS_patients_other_comorbidities$group == "Chronic_liver_disease_and_viral_hepatitis" ~ "Chronic_liver_disease_and_viral_hepatitis",
  IS_patients_other_comorbidities$group == "Inflammatory_bowel_disease" ~ "Inflammatory_bowel_disease",
  IS_patients_other_comorbidities$group == "Depression" & IS_patients_other_comorbidities$patid %in% All_IS_medication$patid[All_IS_medication$group == "Depression" & All_IS_medication$nr_of_drugs >= 4] ~ "Depression",
  IS_patients_other_comorbidities$group == "Asthma_currently_treated" ~ "Asthma_currently_treated", #needs to be in last 12 months..
  IS_patients_other_comorbidities$patid %in% All_IS_medication$patid[All_IS_medication$group == "Anxiety" & 
                                                                       All_IS_medication$nr_of_drugs >=4] |IS_patients_other_comorbidities$patid %in% All_IS_medication$patid[All_IS_medication$group != "Painful_condition"] & IS_patients_other_comorbidities$patid %in% All_IS_medication$patid[
                                                                         All_IS_medication$productname %in% amitriptyline] ~ "Anxiety",
  IS_patients_other_comorbidities$group == "Irritable_bowel_syndrome" | IS_patients_other_comorbidities$patid %in% All_IS_medication$patid[All_IS_medication$group == "Irritable_bowel_syndrome" & All_IS_medication$nr_of_drugs >= 4] ~ "Irritable_bowel_syndrome", #last 12 months..
  IS_patients_other_comorbidities$group == "Cancer_New_diagnosis_in_last_five_years" ~ "Cancer_New_diagnosis_in_last_five_years", #last 5 years...
  IS_patients_other_comorbidities$patid %in% All_IS_medication$patid[All_IS_medication$group == "Constipation" & All_IS_medication$nr_of_drugs >= 4]~ "Constipation", #in last year 
  IS_patients_other_comorbidities$group == "Epilepsy" & IS_patients_other_comorbidities$patid %in% All_IS_medication$patid[All_IS_medication$group == "Epilepsy"] ~ "Epilepsy", #last 12 months
  IS_patients_other_comorbidities$group == "Schizophrenia" | IS_patients_other_comorbidities$patid %in% All_IS_medication$patid[All_IS_medication$group == "Schizophrenia"] ~ "Schizophrenia", #last 168 days..
  IS_patients_other_comorbidities$group == "Psoriasis_or_eczema" & IS_patients_other_comorbidities$patid %in% All_IS_medication$patid[All_IS_medication$group =="Psoriasis_or_eczema" & All_IS_medication$nr_of_drugs >=4] ~ "Psoriasis_or_eczema", #last 12 months..
  IS_patients_other_comorbidities$patid %in% All_IS_medication$patid[All_IS_medication$group == "Migraine" & All_IS_medication$nr_of_drugs >=4] ~ "Migraine",
  IS_patients_other_comorbidities$patid %in% All_IS_medication$patid[All_IS_medication$group == "Painful_condition"& All_IS_medication$nr_of_drugs >=4] |IS_patients_other_comorbidities$group != "epilepsy" & IS_patients_other_comorbidities$patid %in% All_IS_medication$patid[All_IS_medication$group == "Painful_condition_epi" & All_IS_medication$nr_of_drugs >=4] ~ "Painful_condition"
  ))%>%
  filter(!is.na(disease_group))%>% 
  distinct(patid,disease_group,.keep_all = T)
  #group_by(patid) %>% 
  #summarise(nr_of_diseases = n_distinct(disease_group)) %>%
  #ungroup
  
#FOR PD PATIENTS  
Get_comorbidities_PD_matched <- PD_patients_comorbidities %>% select(-Description, -CodingSystem, -constype, -practid, -readcode) %>%
  mutate(disease_group = case_when(
    PD_patients_comorbidities$group == "Hypertension" ~ "Hypertension",
    PD_patients_comorbidities$group == "Coronary_heart_disease" ~ "Coronary_heart_disease",
    PD_patients_comorbidities$group == "Diabetes" ~ "Diabetes",
    PD_patients_comorbidities$group == "Thyroid_disorders" ~ "Thyroid_disorders",
    PD_patients_comorbidities$group == "Rheumatoid_arthritis" ~ "Rheumathoid_arthritis",
    PD_patients_comorbidities$group == "Hearing_loss" ~ "Hearing_loss",
    PD_patients_comorbidities$group == "COPD" ~ "COPD",
    PD_patients_comorbidities$group == "Alcohol_problems" ~ "Alcohol_problems",
    PD_patients_comorbidities$group == "Psychoactive_substance_misuse_NOT_ALCOHOL" ~ "Psychoactive_substance_misuse_NOT_ALCOHOL",
    PD_patients_comorbidities$group == "Stroke_transient_ischaemic_attack" ~ "Stroke_transient_ischaemic_attack",
    PD_patients_comorbidities$group == "Chronic_kidney_disease" ~ "Chronic_kidney_disease",
    PD_patients_comorbidities$group == "Diverticular_disease_of_intestine" ~ "Diverticular_disease_of_intestine",
    PD_patients_comorbidities$group == "Atrial fibrillation" ~ "Atrial fibrillation",
    PD_patients_comorbidities$group == "Peripheral_vascular_disease" ~ "Peripheral_vascular_disease",
    PD_patients_comorbidities$group == "Heart_failure" ~ "Heart_failure",
    PD_patients_comorbidities$group == "Prostate_disorders" ~ "Prostate_disorders",
    PD_patients_comorbidities$group == "Blindness and low vision" ~ "Blindness and low vision",
    PD_patients_comorbidities$group == "Dementia" ~ "Dementia",
    PD_patients_comorbidities$group == "Chronic_sinusitis" ~ "Chronic_sinusitis",
    PD_patients_comorbidities$group == "Learning_disability" ~ "Learning_disability",
    PD_patients_comorbidities$group == "Anorexia_or_bulimia" ~ "Anorexia_or_bulimia",
    PD_patients_comorbidities$group == "Bronchiectasis" ~ "Bronchiectasis",
    PD_patients_comorbidities$group == "Multiple_sclerosis" ~ "Multiple_sclerosis",
    PD_patients_comorbidities$group == "Chronic_liver_disease_and_viral_hepatitis" ~ "Chronic_liver_disease_and_viral_hepatitis",
    PD_patients_comorbidities$group == "Inflammatory_bowel_disease" ~ "Inflammatory_bowel_disease",
    PD_patients_comorbidities$group == "Depression" & PD_patients_comorbidities$patid %in% Other_PD_medication$patid[Other_PD_medication$group == "Depression" & Other_PD_medication$nr_of_drugs >= 4] ~ "Depression",
    PD_patients_comorbidities$group == "Asthma_currently_treated" ~ "Asthma_currently_treated", #needs to be in last 12 months..
    PD_patients_comorbidities$patid %in% Other_PD_medication$patid[Other_PD_medication$group == "Anxiety" & 
                                                                           Other_PD_medication$nr_of_drugs >=4] |PD_patients_comorbidities$patid %in% Other_PD_medication$patid[Other_PD_medication$group != "Painful_condition"] & PD_patients_comorbidities$patid %in% Other_PD_medication$patid[
                                                                             Other_PD_medication$productname %in% amitriptyline] ~ "Anxiety",
    PD_patients_comorbidities$group == "Irritable_bowel_syndrome" | PD_patients_comorbidities$patid %in% Other_PD_medication$patid[Other_PD_medication$group == "Irritable_bowel_syndrome" & Other_PD_medication$nr_of_drugs >= 4] ~ "Irritable_bowel_syndrome", #last 12 months..
    PD_patients_comorbidities$group == "Cancer_New_diagnosis_in_last_five_years" ~ "Cancer_New_diagnosis_in_last_five_years", #last 5 years...
    PD_patients_comorbidities$patid %in% Other_PD_medication$patid[Other_PD_medication$group == "Constipation" & Other_PD_medication$nr_of_drugs >= 4]~ "Constipation", #in last year 
    PD_patients_comorbidities$group == "Epilepsy" & PD_patients_comorbidities$patid %in% Other_PD_medication$patid[All_IS_medication$group == "Epilepsy"] ~ "Epilepsy", #last 12 months
    PD_patients_comorbidities$group == "Schizophrenia" | PD_patients_comorbidities$patid %in% Other_PD_medication$patid[Other_PD_medication$group == "Schizophrenia"] ~ "Schizophrenia", #last 168 days..
    PD_patients_comorbidities$group == "Psoriasis_or_eczema" & PD_patients_comorbidities$patid %in% Other_PD_medication$patid[Other_PD_medication$group =="Psoriasis_or_eczema" & Other_PD_medication$nr_of_drugs >=4] ~ "Psoriasis_or_eczema", #last 12 months..
    PD_patients_comorbidities$patid %in% Other_PD_medication$patid[Other_PD_medication$group == "Migraine" & Other_PD_medication$nr_of_drugs >=4] ~ "Migraine",
    PD_patients_comorbidities$patid %in% Other_PD_medication$patid[Other_PD_medication$group == "Painful_condition"& Other_PD_medication$nr_of_drugs >=4] |PD_patients_comorbidities$group != "epilepsy" & PD_patients_comorbidities$patid %in% Other_PD_medication$patid[Other_PD_medication$group == "Painful_condition_epi" & Other_PD_medication$nr_of_drugs >=4] ~ "Painful_condition"
  ))%>%
  filter(!is.na(disease_group))%>% 
  distinct(patid,disease_group,.keep_all = T)


#COMORBIDITIES MATCHING IS PRESCRIPTION
Get_comorbidities_IS_matched <- IS_patients_linked_comorbidities %>% select(-Description, -CodingSystem, -constype, -practid, -readcode) %>%
  mutate(disease_group = case_when(
    IS_patients_linked_comorbidities$group == "Hypertension" ~ "Hypertension",
    IS_patients_linked_comorbidities$group == "Coronary_heart_disease" ~ "Coronary_heart_disease",
    IS_patients_linked_comorbidities$group == "Diabetes" ~ "Diabetes",
    IS_patients_linked_comorbidities$group == "Thyroid_disorders" ~ "Thyroid_disorders",
    IS_patients_linked_comorbidities$group == "Rheumatoid_arthritis" ~ "Rheumathoid_arthritis",
    IS_patients_linked_comorbidities$group == "Hearing_loss" ~ "Hearing_loss",
    IS_patients_linked_comorbidities$group == "COPD" ~ "COPD",
    IS_patients_linked_comorbidities$group == "Alcohol_problems" ~ "Alcohol_problems",
    IS_patients_linked_comorbidities$group == "Psychoactive_substance_misuse_NOT_ALCOHOL" ~ "Psychoactive_substance_misuse_NOT_ALCOHOL",
    IS_patients_linked_comorbidities$group == "Stroke_transient_ischaemic_attack" ~ "Stroke_transient_ischaemic_attack",
    IS_patients_linked_comorbidities$group == "Chronic_kidney_disease" ~ "Chronic_kidney_disease",
    IS_patients_linked_comorbidities$group == "Diverticular_disease_of_intestine" ~ "Diverticular_disease_of_intestine",
    IS_patients_linked_comorbidities$group == "Atrial fibrillation" ~ "Atrial fibrillation",
    IS_patients_linked_comorbidities$group == "Peripheral_vascular_disease" ~ "Peripheral_vascular_disease",
    IS_patients_linked_comorbidities$group == "Heart_failure" ~ "Heart_failure",
    IS_patients_linked_comorbidities$group == "Prostate_disorders" ~ "Prostate_disorders",
    IS_patients_linked_comorbidities$group == "Blindness and low vision" ~ "Blindness and low vision",
    IS_patients_linked_comorbidities$group == "Dementia" ~ "Dementia",
    IS_patients_linked_comorbidities$group == "Chronic_sinusitis" ~ "Chronic_sinusitis",
    IS_patients_linked_comorbidities$group == "Learning_disability" ~ "Learning_disability",
    IS_patients_linked_comorbidities$group == "Anorexia_or_bulimia" ~ "Anorexia_or_bulimia",
    IS_patients_linked_comorbidities$group == "Bronchiectasis" ~ "Bronchiectasis",
    IS_patients_linked_comorbidities$group == "Multiple_sclerosis" ~ "Multiple_sclerosis",
    IS_patients_linked_comorbidities$group == "Chronic_liver_disease_and_viral_hepatitis" ~ "Chronic_liver_disease_and_viral_hepatitis",
    IS_patients_linked_comorbidities$group == "Inflammatory_bowel_disease" ~ "Inflammatory_bowel_disease",
    IS_patients_linked_comorbidities$group == "Depression" & IS_patients_linked_comorbidities$patid %in% Linked_IS_medication$patid[Linked_IS_medication$group == "Depression" & Linked_IS_medication$nr_of_drugs >= 4] ~ "Depression",
    IS_patients_linked_comorbidities$group == "Asthma_currently_treated" ~ "Asthma_currently_treated", #needs to be in last 12 months..
    IS_patients_linked_comorbidities$patid %in% Linked_IS_medication$patid[Linked_IS_medication$group == "Anxiety" & 
                                                                             Linked_IS_medication$nr_of_drugs >=4] |IS_patients_linked_comorbidities$patid %in% Linked_IS_medication$patid[Linked_IS_medication$group != "Painful_condition"] & IS_patients_linked_comorbidities$patid %in% Linked_IS_medication$patid[
                                                                               Linked_IS_medication$productname %in% amitriptyline] ~ "Anxiety",
    IS_patients_linked_comorbidities$group == "Irritable_bowel_syndrome" | IS_patients_linked_comorbidities$patid %in% Linked_IS_medication$patid[Linked_IS_medication$group == "Irritable_bowel_syndrome" & Linked_IS_medication$nr_of_drugs >= 4] ~ "Irritable_bowel_syndrome", #last 12 months..
    IS_patients_linked_comorbidities$group == "Cancer_New_diagnosis_in_last_five_years" ~ "Cancer_New_diagnosis_in_last_five_years", #last 5 years...
    IS_patients_linked_comorbidities$patid %in% Linked_IS_medication$patid[Linked_IS_medication$group == "Constipation" & Linked_IS_medication$nr_of_drugs >= 4]~ "Constipation", #in last year 
    IS_patients_linked_comorbidities$group == "Epilepsy" & IS_patients_linked_comorbidities$patid %in% Linked_IS_medication$patid[Linked_IS_medication$group == "Epilepsy"] ~ "Epilepsy", #last 12 months
    IS_patients_linked_comorbidities$group == "Schizophrenia" | IS_patients_linked_comorbidities$patid %in% Linked_IS_medication$patid[Linked_IS_medication$group == "Schizophrenia"] ~ "Schizophrenia", #last 168 days..
    IS_patients_linked_comorbidities$group == "Psoriasis_or_eczema" & IS_patients_linked_comorbidities$patid %in% Linked_IS_medication$patid[Linked_IS_medication$group =="Psoriasis_or_eczema" & Linked_IS_medication$nr_of_drugs >=4] ~ "Psoriasis_or_eczema", #last 12 months..
    IS_patients_linked_comorbidities$patid %in% Linked_IS_medication$patid[Linked_IS_medication$group == "Migraine" & Linked_IS_medication$nr_of_drugs >=4] ~ "Migraine",
    IS_patients_linked_comorbidities$patid %in% Linked_IS_medication$patid[Linked_IS_medication$group == "Painful_condition"& Linked_IS_medication$nr_of_drugs >=4] |IS_patients_linked_comorbidities$group != "epilepsy" & IS_patients_linked_comorbidities$patid %in% Linked_IS_medication$patid[Linked_IS_medication$group == "Painful_condition_epi" & Linked_IS_medication$nr_of_drugs >=4] ~ "Painful_condition"
  ))%>%
  filter(!is.na(disease_group))%>% 
  distinct(patid,disease_group,.keep_all = T)


#ALL PATIENTS. 
summary_diseases_pp <- Get_comorbidities2 %>% 
  group_by(patid) %>% 
  summarise(nr_of_diseases = n_distinct(disease_group)) %>% 
  arrange(desc(nr_of_diseases))

diseases_pp <- plot_ly(x = summary_diseases_pp$nr_of_diseases,
                       type = "histogram") %>% 
  layout(title = "Number of Comorbidities per Patient",
         yaxis = list(title = 'Nr of patients'),
         xaxis = list(title = 'Nr of comorbidities'))

diseases_pp<- ggplotly(ggplot(summary_diseases_pp, aes(x=nr_of_diseases)) + geom_histogram(binwidth=1, colour="black", fill="white") +
                         geom_vline(data=summary_diseases_pp, aes(xintercept=mean(summary_diseases_pp$nr_of_diseases, na.rm = T)),
                                    linetype="dashed", size=1, colour="red"))
summary_pp_disease <- Get_comorbidities2 %>% 
  ungroup() %>% group_by(disease_group) %>%
  summarise(nr_pp_disease = n_distinct(patid))%>%
  arrange(desc(nr_pp_disease))
summary_pp_disease$disease_group <- factor(summary_pp_disease$disease_group, levels = unique(summary_pp_disease$disease_group)[order(summary_pp_disease$nr_pp_disease, decreasing = TRUE)])

pp_disease <- plot_ly(x = summary_pp_disease$disease_group,
                      y = summary_pp_disease$nr_pp_disease,
                       type = "bar") %>% 
  layout(title = "Number of Patients per Comorbidity",
         yaxis = list(title = 'Nr of comorbidities'),
         xaxis = list(title = 'Disease'))


#PD PATIENTS 
summary_diseases_pp_PD <- Get_comorbidities_PD_matched %>% 
  group_by(patid) %>% 
  summarise(nr_of_diseases = n_distinct(disease_group)) %>% 
  arrange(desc(nr_of_diseases))

diseases_pp_PD <- plot_ly(x = summary_diseases_pp_PD$nr_of_diseases,
                       type = "histogram") %>% 
  layout(title = "Number of Comorbidities per Patient",
         yaxis = list(title = 'Nr of patients'),
         xaxis = list(title = 'Nr of comorbidities'))

diseases_pp_PD<- ggplotly(ggplot(summary_diseases_pp_PD, aes(x=nr_of_diseases)) + geom_histogram(binwidth=1, colour="black", fill="white") +
                         geom_vline(data=summary_diseases_pp_PD, aes(xintercept=mean(summary_diseases_pp_PD$nr_of_diseases, na.rm = T)),
                                    linetype="dashed", size=1, colour="red"))
summary_pp_disease_PD <- Get_comorbidities_PD_matched %>% 
  ungroup() %>% group_by(disease_group) %>%
  summarise(nr_pp_disease = n_distinct(patid))%>%
  arrange(desc(nr_pp_disease))
summary_pp_disease_PD$disease_group <- factor(summary_pp_disease_PD$disease_group, levels = unique(summary_pp_disease_PD$disease_group)[order(summary_pp_disease_PD$nr_pp_disease, decreasing = TRUE)])

pp_disease_PD <- plot_ly(x = summary_pp_disease_PD$disease_group,
                      y = summary_pp_disease_PD$nr_pp_disease,
                      type = "bar") %>% 
  layout(title = "Number of Patients per Comorbidity",
         yaxis = list(title = 'Nr of comorbidities'),
         xaxis = list(title = 'Disease'))

#IS MATCHED PATIENTS. 
summary_diseases_pp_IS <- Get_comorbidities_IS_matched %>% 
  group_by(patid) %>% 
  summarise(nr_of_diseases = n_distinct(disease_group)) %>% 
  arrange(desc(nr_of_diseases))

diseases_pp_IS <- plot_ly(x = summary_diseases_pp_IS$nr_of_diseases,
                       type = "histogram") %>% 
  layout(title = "Number of Comorbidities per Patient",
         yaxis = list(title = 'Nr of patients'),
         xaxis = list(title = 'Nr of comorbidities'))

diseases_pp_IS<- ggplotly(ggplot(summary_diseases_pp_IS, aes(x=nr_of_diseases)) + geom_histogram(binwidth=1, colour="black", fill="white") +
                         geom_vline(data=summary_diseases_pp_IS, aes(xintercept=mean(summary_diseases_pp_IS$nr_of_diseases, na.rm = T)),
                                    linetype="dashed", size=1, colour="red"))
summary_pp_disease_IS <- Get_comorbidities_IS_matched %>% 
  ungroup() %>% group_by(disease_group) %>%
  summarise(nr_pp_disease = n_distinct(patid))%>%
  arrange(desc(nr_pp_disease))
summary_pp_disease_IS$disease_group <- factor(summary_pp_disease_IS$disease_group, levels = unique(summary_pp_disease_IS$disease_group)[order(summary_pp_disease_IS$nr_pp_disease, decreasing = TRUE)])

pp_disease_IS <- plot_ly(x = summary_pp_disease_IS$disease_group,
                      y = summary_pp_disease_IS$nr_pp_disease,
                      type = "bar") %>% 
  layout(title = "Number of Patients per Comorbidity",
         yaxis = list(title = 'Nr of comorbidities'),
         xaxis = list(title = 'Disease'))








nr_of_pp_Immunosup <- tbl_df(sqldf("SELECT prodcode, COUNT(DISTINCT patid) AS patients FROM SelImmuneProducts GROUP BY prodcode ORDER BY patients DESC"))







