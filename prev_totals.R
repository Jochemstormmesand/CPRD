
prev_totals2 <- function (dat, included_totals = c("year", .ehr$practice_id), 
          time_var = "year", person_years = 100) 
{
  prevalence <- lapply(included_totals, function(x) {
    if (x == time_var) {
      prev_den <- dat %>% filter(prev_denom == TRUE, followup > 
                                   0) %>% group_by_(time_var) %>% summarise(denominator = sum(followup)) %>% 
        mutate(denominator = as.numeric(denominator/365.25))
      prev_num <- dat %>% filter(prev_num == TRUE, followup > 
                                   0) %>% group_by_(time_var) %>% summarise(numerator = n())
      inner_join(prev_num, prev_den) %>% mutate(prevalence = (numerator/(denominator/person_years)))
    }
    else {
      prev_den <- dat %>% filter(prev_denom == TRUE, followup > 
                                   0) %>% group_by_(time_var, x) %>% summarise(denominator = sum(followup)) %>% 
        mutate(denominator = as.numeric(denominator/365.25))
      prev_num <- dat %>% filter(prev_num == TRUE, followup > 
                                   0) %>% group_by_(time_var, x) %>% summarise(numerator = n())
      inner_join(prev_num, prev_den) %>% mutate(prevalence = (numerator/(denominator/person_years)))
    }
  })
  names(prevalence) <- paste(included_totals, "counts", sep = "_")
  incidence <- lapply(included_totals, function(x) {
    if (x == time_var) {
      incid_den <- dat %>% filter(incid_denom == TRUE, 
                                  followup > 0) %>% group_by_(time_var) %>% summarise(denominator = sum(followup)) %>% 
        mutate(denominator = as.numeric(denominator/365.25))
      incid_num <- dat %>% filter(incid_num == TRUE, followup > 
                                    0) %>% group_by_(time_var) %>% summarise(numerator = n())
      inner_join(incid_num, incid_den) %>% mutate(incidence = (numerator/(denominator/person_years)))
    }
    else {
      incid_den <- dat %>% filter(incid_denom == TRUE, 
                                  followup > 0) %>% group_by_(time_var, x) %>% 
        summarise(denominator = sum(followup)) %>% mutate(denominator = as.numeric(denominator/365.25))
      incid_num <- dat %>% filter(incid_num == TRUE, followup > 
                                    0) %>% group_by_(time_var, x) %>% summarise(numerator = n())
      full_join(incid_num, incid_den) %>% mutate(incidence = (numerator/(denominator/person_years)))
    }
  })
  names(incidence) <- paste(included_totals, "counts", sep = "_")
  list(incidence = incidence, prevalence = prevalence)
}


totals10 <- test(prevalence_dat10, included_totals = c("gender"), time_var= "agegroups", person_years = 100000)
totals10$incidence$gender_counts <- left_join(totals10$incidence$gender_counts,ESP2013_10, by = c(c("agegroups" = "agelabels"),c("gender" = "gender_10")))
totals10$incidence$gender_counts$adjustment <- totals10$incidence$gender_counts$ESP2013_10/(100000)
totals10$incidence$gender_counts$adj_incid <- totals10$incidence$gender_counts$incidence * (totals10$incidence$gender_counts$adjustment)
totals10$prevalence$gender_counts <-  left_join(totals10$prevalence$gender_counts,ESP2013_10, by = c(c("agegroups" = "agelabels"),c("gender" = "gender_10")))
totals10$prevalence$gender_counts$adjustment <- totals10$prevalence$gender_counts$ESP2013_10/(100000)
totals10$prevalence$gender_counts$adj_prev <- totals10$prevalence$gender_counts$prevalence * (totals10$prevalence$gender_counts$adjustment)


####################
## look at diseases
####################
all_diseases_all <- semi_join(all_diseases,three_and_two, by = "patid") %>%
  filter(constype ==3)
all_diseases_all <- tbl_df(merge(all_diseases_all, SelFirstImmuneProducts, by = "patid", sort = FALSE))%>% filter(ymd(eventdate.x) < ymd(eventdate.y))
#########
all_diseases_No_PD <- semi_join(all_diseases, No_PD, by = "patid") %>%#select from ALL clinical rows the ones where patid of patients with actual IS
  filter(constype ==3)
#SELECT ONLY THOSE DISEASES THAT OCCURED BEFORE FIRST IMMUNOSUPPRESSION.
all_diseases_No_PD <- tbl_df(merge(all_diseases_No_PD, SelFirstImmuneProducts, by = "patid", sort = FALSE))%>% filter(ymd(eventdate.x) < ymd(eventdate.y))
#########
#all_diseases_IS_patients <- tbl_df(sqldf("SELECT * FROM all_diseases_IS_patients WHERE constype ==3"))
other_diseases_PD_patients <- semi_join(all_diseases, Final_True_PD_marker) %>%
  filter(constype ==3)
other_diseases_PD_patients <- tbl_df(merge(other_diseases_PD_patients, SelFirstImmuneProductsPD, by = "patid", sort = FALSE))%>% filter(ymd(eventdate.x) < ymd(eventdate.y))
#other_diseases_PD_patients <- tbl_df(sqldf("SELECT * FROM other_diseases_PD_patients WHERE constype ==3"))
#########
diseases_matching_IS <- semi_join(all_diseases, No_PD, by = c("patid", "consid")) %>%
  filter(constype ==3)
diseases_matching_IS <- tbl_df(merge(diseases_matching_IS, SelFirstImmuneProducts, by = "patid", sort = FALSE))%>% filter(ymd(eventdate.x) < ymd(eventdate.y))
#########

#add readcodes 
all_diseases_all <- left_join(all_diseases_all, All_medcodes)
all_diseases_No_PD<-left_join(all_diseases_No_PD, All_medcodes)
other_diseases_PD_patients<-left_join(other_diseases_PD_patients, All_medcodes)
diseases_matching_IS <-left_join(diseases_matching_IS, All_medcodes)

##########################
##look at medication
##########################
newmyfiles_prodcodes$prodcode <- as.integer(newmyfiles_prodcodes$prodcode)
#####
all_medication_all <- semi_join(all_medication, three_and_two_marker) %>% left_join(.,SelFirstImmuneProducts, by ="patid", sort = FALSE) %>%
  filter(ymd(eventdate.x) < ymd(eventdate.y))
all_medication_No_PD <- semi_join(all_medication, NO_PD_marker) %>% left_join(.,SelFirstImmuneProducts, by ="patid", sort = FALSE) %>%
  filter(ymd(eventdate.x) < ymd(eventdate.y))
other_medication_PD_patients <- semi_join(all_medication, Final_True_PD_marker)
medication_matching_IS <- semi_join(all_medication, No_PD, by = c("patid", "consid"))
#####
all_all_medication <- inner_join(newmyfiles_prodcodes,all_medication_all, by = c("prodcode" = "prodcode.x"))
all_all_medication <- all_all_medication %>% group_by(patid, prodcode) %>% mutate(nr_of_drugs = n())
all_all_medication <- all_all_medication%>% select(-gemscriptcode, -CodingSystem)
#####
All_No_PD_medication <- inner_join(newmyfiles_prodcodes,all_medication_No_PD, by = c("prodcode" = "prodcode.x"))
All_No_PD_medication <- All_No_PD_medication %>% group_by(patid, prodcode) %>% mutate(nr_of_drugs = n())
All_No_PD_medication <- All_No_PD_medication%>% select(-gemscriptcode, -CodingSystem)
#####
Other_PD_medication <- inner_join(newmyfiles_prodcodes, other_medication_PD_patients)
Other_PD_medication <- Other_PD_medication %>% group_by(patid, prodcode) %>% mutate(nr_of_drugs = n())
Other_PD_medication <- Other_PD_medication%>% select(-gemscriptcode, -CodingSystem)
#####
Linked_IS_medication <- inner_join(newmyfiles_prodcodes,medication_matching_IS)
Linked_IS_medication <- Linked_IS_medication %>% group_by(patid, prodcode) %>% mutate(nr_of_drugs = n())
Linked_IS_medication <- Linked_IS_medication%>% select(-gemscriptcode, -CodingSystem)

IS_patients_linked_comorbidities <- left_join(diseases_matching_IS,newmyfiles)
#IS_patients_linked_comorbidities%>% group_by(patid,eventdate, medcode) %>% arrange(eventdate)%>% distinct() %>% ungroup-> IS_patients_linked_comorbidities
#IS_patients_linked_comorbidities <- IS_patients_linked_comorbidities %>% select(-textid, -practid, -consid, -constype, -sysdate)
No_PD_other_comorbidities <- left_join(all_diseases_No_PD,newmyfiles)  %>% 
  mutate(., group = ifelse(grepl("transplant",.$desc, ignore.case = TRUE),"Transplant",group))%>% filter(!is.na(group))
all_patients_comorbidities<- left_join(all_diseases_all,newmyfiles)  %>% 
  mutate(., group = ifelse(grepl("transplant",.$desc, ignore.case = TRUE),"Transplant",group))%>% filter(!is.na(group))
#No_PD_other_comorbidities <- No_PD_other_comorbidities %>% select(-textid, -practid, -consid, -constype, -sysdate, -enttype, -staffid, -episode, -adid)
PD_patients_comorbidities <- left_join(other_diseases_PD_patients, newmyfiles)
#PD_patients_comorbidities <- PD_patients_comorbidities %>% select(-textid, -practid, -consid, -constype, -sysdate, -enttype, -staffid, -episode, -adid)
#