#Script to find confirmation of how many patients actually suffered from PD
  # 2 or more PD codes 
  # 1 PD code AND received PD medication.
  # ELSE not really PD.

#all patients with a medical code associated to Parkinson's disease
SelParkinsoncodes = tbl_df(select_events(db,tab="Clinical_All", columns = '*',
                                         where = "medcode %in% .(Parkinsoncodes)"))
SelFirstParkinsoncodes = tbl_df(first_events(db, tab="Clinical_All", columns= c("patid", "eventdate","constype", "consid", "medcode", "practid"), 
                                             where = "medcode %in% .(Parkinsoncodes)"))
#All patients that have received drugs known to be PD drugs
SelParkinsonprodcodes <- tbl_df(select_events(db, tab= "Therapy_All", columns = c("patid", "eventdate", "consid","prodcode"),
                                              where = "prodcode %in% .(PD_prodcodes$prodcode)"))
#nr of meds prescribed per patient
nr_of_PDcodes_PP <- tbl_df(sqldf("SELECT patid, medcode, COUNT(DISTINCT medcode) AS nr_of_PDcodes FROM SelParkinsoncodes GROUP BY patid ORDER BY nr_of_PDcodes DESC")) %>% ungroup
nr_of_same_Pdcodes_PP <- tbl_df(sqldf("SELECT patid, medcode, COUNT(medcode) AS nr_of_same_PDcodes FROM SelParkinsoncodes GROUP BY patid, medcode ORDER BY patid, medcode DESC"))

#all PD patients and the codes of the drugs they received
Check_Patients_Meds <- tbl_df(select_events(db, tab = "Therapy_All", columns =  c("patid", "eventdate", "prodcode"),
                                           where = "patid %in% .(nr_of_PDcodes_PP$patid) & prodcode %in% .(PD_prodcodes$prodcode)"))


#Combine files for PD codes (medcode) and PD drug codes (prodcode)
PD_med_prod <- left_join(SelParkinsoncodes, SelParkinsonprodcodes, by = "patid") %>%
  left_join(., nr_of_PDcodes_PP)%>%
  left_join(., nr_of_same_Pdcodes_PP, by = c("patid", "medcode"))

#exclusion
#just a check up to see if any patients have PD exclusioncodes, which are PD caused by known causes 
# apparently there are none. 
PD_exclusion1 <-  tbl_df(sqldf("SELECT patid, medcode FROM PD_med_prod WHERE medcode IN (19478, 24001, 26181, 33544, 97170, 100128)"))


# This statement checks whether the patients truly have PD:
# Have they received more than 1 medcode? if only 1, have they also received PD medication? 
# if not, likely not really PD. 
True_PD <- PD_med_prod %>% mutate(inclusion = case_when(
  PD_med_prod$nr_of_PDcodes > 1 ~ "include",
  PD_med_prod$nr_of_PDcodes == 1 & PD_med_prod$nr_of_same_PDcodes >1 ~ "include",
  PD_med_prod$nr_of_PDcodes == 1 & PD_med_prod$nr_of_same_PDcodes == 1 & PD_med_prod$prodcode != is.na(PD_med_prod$prodcode) ~ "include",
  TRUE ~ "exclude")) %>%
  group_by(patid) %>%
  filter(inclusion == "include") %>%
  distinct(patid, .keep_all = TRUE)


####
#PD diagnosed at least 6 months after first IS prescription. Use first statements and select only patients from TRUE_PD
#combine first PD immunosuppressant prescription and first PD diagnosis.
#If diagnosis is more than 180 days after first prescription, add include variable. 
#used to be SelFirstImmuneProductsPD
IS_before_PD <- tbl_df(semi_join(SelFirstImmuneProducts, True_PD, by = "patid")) %>%
  left_join(., SelFirstParkinsoncodes, by = "patid")
IS_before_PD <- IS_before_PD %>% 
  mutate(over_six = case_when(as.Date(as.character(IS_before_PD$eventdate.y), format = "%Y-%m-%d") 
                                 - as.Date(as.character(IS_before_PD$eventdate.x), format = "%Y-%m-%d") > 180 ~ "include",
                                 TRUE ~ "exclude")) %>% filter(over_six == 'include')
#select those cases where PD is found at least 6 months after IS prescription and other inclusion criteria are met. 

first_PD_diagnosis <- tbl_df(semi_join(SelParkinsoncodes, IS_before_PD, by = "patid")) %>% 
  group_by(patid) %>% slice(c(1)) %>% ungroup()
#all diagnoses of PD per individual. includes if multiple time the same diagnosis. 
IS_before_PD_All <- tbl_df(semi_join(SelParkinsoncodes, IS_before_PD, by= "patid")) %>%
  semi_join(., three_and_two, by = "patid") %>% 
  group_by(patid) %>% mutate(nr_of_PDcodes_PP = n()) %>% arrange(desc(nr_of_PDcodes_PP))

#This is probably needed to find the PD patients that also fall within the immunosuppressant criteria:
Final_True_PD <- tbl_df(semi_join(IS_before_PD, three_and_two, by = "patid")) %>% select(-over_six, -practid)
Final_True_PD_marker <- Final_True_PD %>% distinct(patid)
No_PD <- anti_join(three_and_two,Final_True_PD_marker)
NO_PD_marker <- anti_join(three_and_two_marker,Final_True_PD_marker) %>% distinct(patid)
#############################
# Go to comorbidities script here
#############################

########
184 patients that first received IS and then were at least 6 months later diagnosed with PD.
These patients have received either: 
  more than 1 PD diagnosis OR
  1 PD diagnosis but also PD medication
  
   
