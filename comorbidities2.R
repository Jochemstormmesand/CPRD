Comorbidities2

# using http://www.phpc.cam.ac.uk/pcu/cprd_cam/codelists/

all_diseases <- tbl_df(sqldf("SELECT * FROM Clinical_All", connection = db))
all_medication <- tbl_df(sqldf("SELECT patid, eventdate, consid, prodcode FROM Therapy_All", connection = db))

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



amitriptyline <- unique(All_No_PD_medication$productname[grepl("amitriptyline",All_No_PD_medication$productname, ignore.case = TRUE)])

#Get_comorbidities plug in: 
  # ALL: all_patients_comorbidities and all_all_medication
  # ALL_NO_PD: No_PD_other_comorbidities and All_No_PD_medication
  # ALL_PD: PD_patients_comorbidities and Other_PD_medication
  # ALL_LINKED: IS_patients_linked_comorbidities and Linked_IS_medication



Get_comorbidities <- function (group_df, medication_df,marker){ 
  #look at medication
  medication_df1 <- semi_join(medication_df, marker) %>% left_join(.,SelFirstImmuneProducts, by ="patid", sort = FALSE) %>%
    filter(ymd(eventdate.x) < ymd(eventdate.y)) %>% inner_join(newmyfiles_prodcodes, ., by = c("prodcode" = "prodcode.x")) %>% 
    group_by(patid, prodcode) %>% mutate(nr_of_drugs = n())%>%
    select(-gemscriptcode, -CodingSystem)
  #look at diseases
  group_df1 <- tbl_df(group_df %>%
    merge(., SelFirstImmuneProducts, by = "patid", sort = FALSE) %>% 
    filter(ymd(eventdate.x) < ymd(eventdate.y)) %>% 
    left_join(., All_medcodes) %>% 
    select(-Description, -CodingSystem, -constype, -practid, -readcode)) 
  #combine to find all comorbidities
  group_df1%>%
  mutate(disease_group = case_when(
    .$group == "Coronary_heart_disease" ~ "Coronary_heart_disease",
    .$group == "Diabetes" ~ "Diabetes",
    .$group == "Thyroid_disorders" ~ "Thyroid_disorders",
    .$group == "Rheumatoid_arthritis" ~ "Rheumathoid_arthritis",
    .$group == "Hearing_loss" ~ "Hearing_loss",
    .$group == "COPD" ~ "COPD",
    .$group == "Alcohol_problems" ~ "Alcohol_problems",
    .$group == "Psychoactive_substance_misuse_NOT_ALCOHOL" ~ "Psychoactive_substance_misuse_NOT_ALCOHOL",
    .$group == "Stroke_transient_ischaemic_attack" ~ "Stroke_transient_ischaemic_attack",
    .$group == "Chronic_kidney_disease" ~ "Chronic_kidney_disease",
    .$group == "Diverticular_disease_of_intestine" ~ "Diverticular_disease_of_intestine",
    .$group == "Atrial fibrillation" ~ "Atrial fibrillation",
    .$group == "Peripheral_vascular_disease" ~ "Peripheral_vascular_disease",
    .$group == "Heart_failure" ~ "Heart_failure",
    .$group == "Prostate_disorders" ~ "Prostate_disorders",
    .$group == "Blindness and low vision" ~ "Blindness and low vision",
    .$group == "Dementia" ~ "Dementia",
    .$group == "Chronic_sinusitis" ~ "Chronic_sinusitis",
    .$group == "Learning_disability" ~ "Learning_disability",
    .$group == "Anorexia_or_bulimia" ~ "Anorexia_or_bulimia",
    .$group == "Bronchiectasis" ~ "Bronchiectasis",
    .$group == "Multiple_sclerosis" ~ "Multiple_sclerosis",
    .$group == "Chronic_liver_disease_and_viral_hepatitis" ~ "Chronic_liver_disease_and_viral_hepatitis",
    .$group == "Inflammatory_bowel_disease" ~ "Inflammatory_bowel_disease",
    .$group == "Hypertension" ~ "Hypertension",
    .$group == "Transplant" ~ "Transplant",
    .$group == "Epilepsy" & .$patid %in% medication_df1$patid[medication_df1$group == "Epilepsy"] ~ "Epilepsy", #last 12 months
    .$group == "Depression" & .$patid %in% medication_df1$patid[medication_df1$group == "Depression" & medication_df1$nr_of_drugs >= 4] ~ "Depression",
    .$group == "Asthma_currently_treated" ~ "Asthma_currently_treated", #needs to be in last 12 months..
    .$group == "Psoriasis_or_eczema" & .$patid %in% medication_df1$patid[medication_df1$group =="Psoriasis_or_eczema" & medication_df1$nr_of_drugs >=4] ~ "Psoriasis_or_eczema", #last 12 months..
    .$group == "Cancer_New_diagnosis_in_last_five_years" ~ "Cancer_New_diagnosis_in_last_five_years",#last 5 years...
    .$group == "Irritable_bowel_syndrome" ~"Irritable_bowel_syndrome",
    .$group == "Schizophrenia" ~"Schizophrenia",
    .$group == "Anxiety" ~"Anxiety")) %>% 
        select(-eventdate, -prodcode.y) %>%
    filter(!is.na(disease_group)) %>% 
    distinct(patid,disease_group,.keep_all = T)
    } 

All_all <- Get_comorbidities(all_patients_comorbidities,all_diseases_all, three_and_two_marker)
Get_comorbidities2<- Get_comorbidities(No_PD_other_comorbidities,All_No_PD_medication)
Get_comorbidities_IS_matched<- Get_comorbidities(IS_patients_linked_comorbidities,Linked_IS_medication)
Get_comorbidities_PD_matched<- Get_comorbidities(PD_patients_comorbidities,Other_PD_medication)

summary_comorbidities <- function(group){
  summary_comorbidities_pp <- group %>% 
    group_by(patid) %>% 
    summarise(nr_of_diseases = n_distinct(disease_group)) %>% 
    arrange(desc(nr_of_diseases))
  summary_comorbidities_pp %>%
    summarise(mean = mean(nr_of_diseases),
              min = min(nr_of_diseases),
              max = max(nr_of_diseases),
              sd = sd(nr_of_diseases)) %>% print()
  summary_pp_disease <- group %>% 
    ungroup() %>% group_by(disease_group) %>%
    summarise(nr_pp_disease = n_distinct(patid))%>%
    arrange(desc(nr_pp_disease)) %>% print(n = 50)
  }
summary_comorbidities(All_all)
summary_comorbidities(Get_comorbidities2)
summary_comorbidities(Get_comorbidities_PD_matched)
summary_comorbidities(Get_comorbidities_IS_matched)




diseases_pp <- plot_ly(x = summary_diseases_pp$nr_of_diseases,
                       type = "histogram") %>% 
  layout(title = "Number of Comorbidities per Patient",
         yaxis = list(title = 'Nr of patients'),
         xaxis = list(title = 'Nr of comorbidities'))

diseases_pp<- ggplotly(ggplot(summary_diseases_pp, aes(x=nr_of_diseases)) + geom_histogram(binwidth=1, colour="black", fill="white") +
                         geom_vline(data=summary_diseases_pp, aes(xintercept=mean(summary_diseases_pp$nr_of_diseases, na.rm = T)),
                                    linetype="dashed", size=1, colour="red")+
                         ggtitle("Nr of disease per patient")+
                         xlab("Nr of Diseases") + ylab("Nr of Patients"))

summary_pp_disease$disease_group <- factor(summary_pp_disease$disease_group, levels = unique(summary_pp_disease$disease_group)[order(summary_pp_disease$nr_pp_disease, decreasing = TRUE)])

pp_disease <- plot_ly(x = summary_pp_disease$disease_group,
                      y = summary_pp_disease$nr_pp_disease,
                       type = "bar") %>% 
  layout(title = "Number of Patients per Comorbidity",
         yaxis = list(title = 'Nr of comorbidities'),
         xaxis = list(title = 'Disease'))


#PD PATIENTS 

diseases_pp_PD <- plot_ly(x = summary_diseases_pp_PD$nr_of_diseases,
                       type = "histogram") %>% 
  layout(title = "Number of Comorbidities per Patient",
         yaxis = list(title = 'Nr of patients'),
         xaxis = list(title = 'Nr of comorbidities'))

diseases_pp_PD<- ggplotly(ggplot(summary_diseases_pp_PD, aes(x=nr_of_diseases)) + geom_histogram(binwidth=1, colour="black", fill="white") +
                         geom_vline(data=summary_diseases_pp_PD, aes(xintercept=mean(summary_diseases_pp_PD$nr_of_diseases, na.rm = T)),
                                    linetype="dashed", size=1, colour="red")+
                           ggtitle("Nr of disease per PD patient")+
                           xlab("Nr of Diseases") + ylab("Nr of Patients"))

summary_pp_disease_PD$disease_group <- factor(summary_pp_disease_PD$disease_group, levels = unique(summary_pp_disease_PD$disease_group)[order(summary_pp_disease_PD$nr_pp_disease, decreasing = TRUE)])

pp_disease_PD <- plot_ly(x = summary_pp_disease_PD$disease_group,
                      y = summary_pp_disease_PD$nr_pp_disease,
                      type = "bar") %>% 
  layout(title = "Number of Patients per Comorbidity",
         yaxis = list(title = 'Nr of comorbidities'),
         xaxis = list(title = 'Disease'))

#IS MATCHED PATIENTS. 

diseases_pp_IS <- plot_ly(x = summary_diseases_pp_IS$nr_of_diseases,
                       type = "histogram") %>% 
  layout(title = "Number of Comorbidities per Patient",
         yaxis = list(title = 'Nr of patients'),
         xaxis = list(title = 'Nr of comorbidities'))

diseases_pp_IS<- ggplotly(ggplot(summary_diseases_pp_IS, aes(x=nr_of_diseases)) + geom_histogram(binwidth=1, colour="black", fill="white") +
                         geom_vline(data=summary_diseases_pp_IS, aes(xintercept=mean(summary_diseases_pp_IS$nr_of_diseases, na.rm = T)),
                                    linetype="dashed", size=1, colour="red")+
                           ggtitle("Nr of diseases per patient IS prescription")+
                           xlab("Nr of Diseases") + ylab("Nr of Patients"))
summary_pp_disease_IS$disease_group <- factor(summary_pp_disease_IS$disease_group, levels = unique(summary_pp_disease_IS$disease_group)[order(summary_pp_disease_IS$nr_pp_disease, decreasing = TRUE)])

pp_disease_IS <- plot_ly(x = summary_pp_disease_IS$disease_group,
                      y = summary_pp_disease_IS$nr_pp_disease,
                      type = "bar") %>% 
  layout(title = "Number of Patients per Comorbidity",
         yaxis = list(title = 'Nr of comorbidities'),
         xaxis = list(title = 'Disease'))








nr_of_pp_Immunosup <- tbl_df(sqldf("SELECT prodcode, COUNT(DISTINCT patid) AS patients FROM SelImmuneProducts GROUP BY prodcode ORDER BY patients DESC"))




Get_comorbidities3 <- Get_comorbidities2 %>% filter(is.na(disease_group))%>%group_by(patid,medcode) %>% mutate(n =1,n =cumsum(n)) %>% ungroup %>% arrange(patid,medcode)

test <- All_No_PD_medication %>% ungroup %>% distinct(patid,group,.keep_all = TRUE)
Get_comorbidities3 <- left_join(Get_comorbidities2, test, by = "patid","group")
Get_comorbidities3 <- Get_comorbidities2 %>% filter(!is.na(disease_group))
full_join(Get_comorbidities3,test)

Get_comorbidities3 <- No_PD_other_comorbidities %>% mutate(disease_group = case_when(
  .$patid %in% All_No_PD_medication$patid[All_No_PD_medication$group == "Painful_condition"& All_No_PD_medication$nr_of_drugs >=4] |.$group != "Epilepsy" & No_PD_other_comorbidities$patid %in% All_No_PD_medication$patid[All_No_PD_medication$group == "Painful_condition_epi" & All_No_PD_medication$nr_of_drugs >=4] ~ "Painful_condition",
  .$patid %in% .$patid[All_No_PD_medication$group == "Constipation" & All_No_PD_medication$nr_of_drugs >= 4] ~ "Constipation", #in last year 
  .$patid %in% .$patid[All_No_PD_medication$group == "Migraine" & All_No_PD_medication$nr_of_drugs >=4]  ~ "Migraine",
  .$patid %in% All_No_PD_medication$patid[All_No_PD_medication$group == "Irritable_bowel_syndrome" & All_No_PD_medication$nr_of_drugs >= 4] ~ "Irritable_bowel_syndrome", #last 12 months..
  .$patid %in% All_No_PD_medication$patid[All_No_PD_medication$group == "Schizophrenia"] ~ "Schizophrenia", #last 168 days..
  .$patid %in% All_No_PD_medication$patid[All_No_PD_medication$group == "Anxiety" & All_No_PD_medication$nr_of_drugs >=4] |.$patid %in% All_No_PD_medication$patid[All_No_PD_medication$group != "Painful_condition"] & .$patid %in% All_No_PD_medication$patid[
    All_No_PD_medication$productname %in% amitriptyline] ~ "Anxiety"
))

Try<- All_No_PD_medication %>%ungroup%>% mutate(disease_group= case_when(
  All_No_PD_medication$group == "Irritable_bowel_syndrome" & All_No_PD_medication$nr_of_drugs>= 4~"Irritable_bowel_syndrome",
  All_No_PD_medication$group == "Migraine" & All_No_PD_medication$nr_of_drugs >=4 ~ "Migraine",
  All_No_PD_medication$group == "Anxiety" & All_No_PD_medication$nr_of_drugs >=4 | All_No_PD_medication$group != "Painful_condition" & All_No_PD_medication$productname %in% amitriptyline ~ "Anxiety",
  All_No_PD_medication$group == "Painful_condition" & All_No_PD_medication$nr_of_drugs >=4 | (All_No_PD_medication$group == ~ "Painful_condition_epi" & All_No_PD_medication$nr_of_drugs >=4 & No_PD_other_comorbidities$patid != "Epilepsy")~ "Painful_condition",
  All_No_PD_medication$group =="Schizophrenia" ~ "Schizophrenia",
  All_No_PD_medication$group == "Constipation" & All_No_PD_medication$nr_of_drugs >=4 ~ "Constipation"))%>% 
  filter(!is.na(disease_group)) %>% group_by(patid, disease_group) %>% slice(1)
Try2 <- full_join(Get_comorbidities2, Try, by = "patid", "disease_group")



