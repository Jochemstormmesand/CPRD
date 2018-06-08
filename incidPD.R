#INCIDENT PD IN TOTAL CPRD

db2 <- database(tempfile(fileext = ".sqlite"))
import_CPRD_data(db2, data_dir = ehr_path,         
                 filetypes = c("Therapy_001", "Therapy_002", "Referral_001", "Patient_001", "Clinical_001","Practice_001"),
                 dateformat = "%d/%m/%Y",
                 yob_origin = 1800,
                 regex = "PD_incidentpd_Extract",
                 recursive = TRUE)
import_CPRD_data(db2, data_dir = ehr_path,         
                 filetypes = c("prod_pd_treat", "Prodcodes_Cut"),
                 dateformat = "%d/%m/%Y",
                 yob_origin = 1800,
                 regex = "PD_immunosup_Extract",
                 recursive = TRUE)

Therapy_001 <- tbl_df(select_events(db2,tab="Therapy_001", columns = '*'))
dbRemoveTable(db2, "Therapy_001")
Therapy_002 <- tbl_df(select_events(db2,tab="Therapy_002", columns = '*'))
dbRemoveTable(db2, "Therapy_002")
Therapy_All <- rbind(Therapy_001, Therapy_002)
rm(Therapy_001, Therapy_002)
dbWriteTable(db2, "Therapy_All", Therapy_All, append =TRUE)

###########
incidPD_SelImmuneProducts <- tbl_df(dbGetQuery(db2, "SELECT * FROM Therapy_All WHERE prodcode IN (SELECT Prodcode FROM Prodcodes_Cut)"))
incidPD_SelFirstImmuneProducts <- tbl_df(first_events(db2, tab="Therapy_All", columns = c("patid", "eventdate", "prodcode"), 
                                              where = "prodcode %in% .(ImmuneMed$Prodcode)"))
incidPD_SelFirstImmuneProducts <- incidPD_SelImmuneProducts %>% group_by(patid) %>% first()

incidPD_MedCalculation <- tbl_df(left_join(incidPD_SelImmuneProducts, DrugInfo))

#############

incidPD_SelParkinsoncodes = tbl_df(select_events(db2,tab="Clinical_001", columns = '*',
                                         where = "medcode %in% .(Parkinsoncodes)"))
incidPD_SelFirstParkinsoncodes = tbl_df(first_events(db2, tab="Clinical_001", columns= c("patid", "eventdate","constype", "consid", "medcode", "practid"), 
                                             where = "medcode %in% .(Parkinsoncodes)"))
#All patients that have received drugs known to be PD drugs
incidPD_SelParkinsonprodcodes <- tbl_df(dbGetQuery(db2, "SELECT patid,eventdate,consid,prodcode FROM Therapy_All WHERE prodcode IN (SELECT cprd_prodcode FROM prod_pd_treat)"))
#nr of meds prescribed per patient
incidPD_nr_of_PDcodes_PP <- tbl_df(sqldf("SELECT patid, medcode, COUNT(DISTINCT medcode) AS nr_of_PDcodes FROM incidPD_SelParkinsoncodes GROUP BY patid ORDER BY nr_of_PDcodes DESC")) %>% ungroup
incidPD_nr_of_same_Pdcodes_PP <- tbl_df(sqldf("SELECT patid, medcode, COUNT(medcode) AS nr_of_same_PDcodes FROM incidPD_SelParkinsoncodes GROUP BY patid, medcode ORDER BY patid, medcode DESC"))


#all PD patients and the codes of the drugs they received
incidPD_Check_Patients_Meds <- tbl_df(select_events(db2, tab = "Therapy_All", columns =  c("patid", "eventdate", "prodcode"),
                                            where = "patid %in% .(incidPD_nr_of_PDcodes_PP$patid) & prodcode %in% .(PD_prodcodes$prodcode)"))
incidPD_Check_Patients_Meds <- tbl_df(dbGetQuery(db2,"SELECT patid,eventdate,prodcode FROM Therapy_All WHERE patid IN (',paste(incidPD_nr_of_PDcodes_PP$patid, collapse='    '),') AND prodcode IN (SELECT cprd_prodcode FROM prod_pd_treat)"))

#Combine files for PD codes (medcode) and PD drug codes (prodcode)
incidPD_PD_med_prod <- left_join(incidPD_SelParkinsoncodes, incidPD_SelParkinsonprodcodes, by = "patid")
incidPD_PD_med_prod <- left_join(incidPD_PD_med_prod, incidPD_nr_of_PDcodes_PP)
incidPD_PD_med_prod <-  left_join(incidPD_PD_med_prod, incidPD_nr_of_same_Pdcodes_PP, by = c("patid", "medcode"))

#exclusion
#just a check up to see if any patients have PD exclusioncodes, which are PD caused by known causes 
# apparently there are none. 
incidPD_PD_exclusion1 <-  tbl_df(sqldf("SELECT patid, medcode FROM incidPD_PD_med_prod WHERE medcode IN (19478, 24001, 26181, 33544, 97170, 100128)"))


# This statement checks whether the patients truly have PD:
# Have they received more than 1 medcode? if only 1, have they also received PD medication? 
# if not, likely not really PD. 
incidPD_True_PD <- incidPD_PD_med_prod %>% mutate(inclusion = case_when(
  incidPD_PD_med_prod$nr_of_PDcodes > 1 ~ "include",
  incidPD_PD_med_prod$nr_of_PDcodes == 1 & incidPD_PD_med_prod$nr_of_same_PDcodes >1 ~ "include",
  incidPD_PD_med_prod$nr_of_PDcodes == 1 & incidPD_PD_med_prod$nr_of_same_PDcodes == 1 & incidPD_PD_med_prod$prodcode != is.na(incidPD_PD_med_prod$prodcode) ~ "include",
  TRUE ~ "exclude")) %>%
  group_by(patid) %>%
  filter(inclusion == "include") %>%
  distinct(patid, .keep_all = TRUE)

Clinical_PD2 <- tbl_df(sqldf("SELECT patid, eventdate, medcode FROM Clinical_001", connection = db2)) %>%
  semi_join(., incidPD_True_PD) #same as: where medcode in IS_before_PD
#!!!!!!!!!!!!!IMPORTANT: SHOULD THIS BE JOINED BY MEDCODE OR BY PATID????????????
dbWriteTable(db2, "Clinical_PD", Clinical_PD2, overwrite = TRUE)
rm(Clinical_PD2)
incidPD_incident_cases<- select_by_year(db=db2, tables=c("Clinical_PD"), #should this contain data from referral file as well? That is what they do in the paper.
                                columns=c("patid", "eventdate", "medcode"),
                                where="eventdate <= ENDDATE", 
                                year_range=c(1995: 2015), year_fn=standard_years, selector_fn =first_events)

incidPD_prevalence_dat<- tbl_df(left_join(incidPD_registered_patients, incidPD_incident_cases))
incidPD_prevalence_dat<- prev_terms(incidPD_prevalence_dat)

TOTAL_CPRD_Info2 <- tbl_df(select_events(db2, tab = "Patient_001", columns = c("patid", "gender", "yob", "deathdate")))%>%
  group_by(patid, yob) %>% 
  distinct(patid, .keep_all = TRUE) %>% 
  ungroup()

#Patient demographics for those on IS medication. Added age at time of first prescription. 
TOTAL_CPRD_Info <- tbl_df(left_join(SelFirstImmuneProducts, PatientInfo2) %>%
                   semi_join(., three_and_two)%>%
                   mutate(ageatstart = year(eventdate)-yob))
TOTAL_CPRD_Info10 <- tbl_df(left_join(SelFirstImmuneProducts, PatientInfo2) %>%
                     semi_join(., three_and_two)%>%
                     mutate(ageatstart = year(eventdate)-yob))
setDT(TOTAL_CPRD_Info10)[ , agegroups := cut(ageatstart, 
                                    breaks = agebreaks10, 
                                    right = FALSE, 
                                    labels = agelabels10)]
incidPD_prevalence_dat10 <-  tbl_df(merge(x = incidPD_prevalence_dat, y = TOTAL_CPRD_Info10[ , c("patid", "ageatstart", "agegroups")], by = c("patid"), all.x=TRUE))
#incidPD_prevalence_dat10 <-  tbl_df(merge(x = incidPD_prevalence_dat10, y = summary_diseases_pp[ , c("patid", "nr_of_diseases")], by = c("patid"), all.x=TRUE))
#incidPD_prevalence_dat10 <- tbl_df(merge(x = incidPD_prevalence_dat10, y = PDInfo[ , c("patid", "nr_of_diseases")], by = c("patid"), all.x=TRUE))
incidPD_prevalence_dat10$gender <- as.character.factor(factor(incidPD_prevalence_dat10$gender, levels=c(0,1,2,3,4), labels=c("Data Not Entered","Male", "Female", "Indeterminate", "Unknown")))

incidPD_totals10 <- prev_totals(incidPD_prevalence_dat10, included_totals = c("gender"), time_var= "agegroups", person_years = 100000)
incidPD_totals10$incidence$gender_counts <- left_join(incidPD_totals10$incidence$gender_counts,ESP2013_10, by = c(c("agegroups" = "agelabels"),c("gender" = "gender_10")))
incidPD_totals10$incidence$gender_counts$adjustment <- incidPD_totals10$incidence$gender_counts$ESP2013_10/(100000)
incidPD_totals10$incidence$gender_counts$adj_incid <- incidPD_totals10$incidence$gender_counts$incidence * (incidPD_totals10$incidence$gender_counts$adjustment)
incidPD_totals10$prevalence$gender_counts <-  left_join(incidPD_totals10$prevalence$gender_counts,ESP2013_10, by = c(c("agegroups" = "agelabels"),c("gender" = "gender_10")))
incidPD_totals10$prevalence$gender_counts$adjustment <- incidPD_totals10$prevalence$gender_counts$ESP2013_10/(100000)
incidPD_totals10$prevalence$gender_counts$adj_prev <- incidPD_totals10$prevalence$gender_counts$prevalence * (incidPD_totals10$prevalence$gender_counts$adjustment)


ggplotly(ggplot(totals10$incidence$gender_counts, aes(agegroups, incidence)) +   
           geom_bar(aes(fill = factor(gender)), position = "dodge", stat="identity")+
           ggtitle("Incidence per 100.000")+
           xlab("Age Groups") + ylab("Incidence")+
           theme(legend.position="top", legend.title = element_blank()))
