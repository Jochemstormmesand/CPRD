#PD incidence
#Required to avoid memory exhaust due to WHERE IN clause. 
Patient_IS2 <- tbl_df(sqldf("SELECT * FROM Patient_001", connection = db)) %>% 
  semi_join(., three_and_two) #same as: where patid in three_and_two_marker 
dbWriteTable(db, "Patient_IS", Patient_IS2, overwrite = TRUE)
rm(Patient_IS2)
Clinical_PD2 <- tbl_df(sqldf("SELECT patid, eventdate, medcode FROM Clinical_All", connection = db)) %>%
  semi_join(., IS_before_PD_All, by = c("patid","medcode")) #same as: where medcode in IS_before_PD
#!!!!!!!!!!!!!IMPORTANT: SHOULD THIS BE JOINED BY MEDCODE OR BY PATID????????????
dbWriteTable(db, "Clinical_PD", Clinical_PD2, overwrite = TRUE)
rm(Clinical_PD2)
Referral_PD2 <- tbl_df(sqldf("SELECT * FROM Referral_001", connection = db))%>%
  semi_join(., IS_before_PD_All, by = c("patid","medcode")) #same as: where medcode in IS_before_PD
dbWriteTable(db, "Referral_PD", Referral_PD2, overwrite = TRUE)
rm(Referral_PD2)

#For inflammatory bowel disease. 
IBD <- Get_comorbidities2 %>% filter(disease_group == "Inflammatory_bowel_disease")
Patient_IBD2 <- tbl_df(sqldf("SELECT * FROM Patient_001", connection = db)) %>% 
  semi_join(., IBD)
dbWriteTable(db, "Patient_IBD", Patient_IBD2, overwrite = TRUE)
rm(Patient_IBD2)
Clinical_IBD2 <- tbl_df(sqldf("SELECT patid, eventdate, medcode FROM Clinical_PD", connection = db)) %>%
  semi_join(., IBD, by = "patid") #same as: where medcode in IS_before_PD
dbWriteTable(db, "Clinical_IBD", Clinical_IBD2, overwrite = TRUE)
rm(Clinical_IBD2)
Referral_IBD2 <- tbl_df(sqldf("SELECT * FROM Referral_PD", connection = db))%>%
  semi_join(., IBD, by = c("patid", c("eventdate" = "eventdate.x"), "medcode")) #same as: where medcode in IS_before_PD
dbWriteTable(db, "Referral_IBD", Referral_IBD2, overwrite = TRUE)
rm(Referral_IBD2)
#OTHER INFLAMMATORY
Inflammatory <- Get_comorbidities2 %>% filter(grepl("inflam|infect|rheuma",desc, ignore.case = T) | grepl("rheuma|inflam",disease_group, ignore.case = T) & !grepl("inflammatory_bowel_disease",disease_group,ignore.case = T))
Patient_Inflam2 <- tbl_df(sqldf("SELECT * FROM Patient_001", connection = db)) %>% 
  semi_join(., Inflammatory)
dbWriteTable(db, "Patient_Inflam", Patient_Inflam2, overwrite = TRUE)
rm(Patient_Inflam2)
Clinical_Inflam2 <- tbl_df(sqldf("SELECT patid, eventdate, medcode FROM Clinical_PD", connection = db)) %>%
  semi_join(., Inflammatory, by = "patid") #same as: where medcode in IS_before_PD
dbWriteTable(db, "Clinical_Inflam", Clinical_Inflam2, overwrite = TRUE)
rm(Clinical_Inflam2)
Referral_Inflam2 <- tbl_df(sqldf("SELECT * FROM Referral_PD", connection = db))%>%
  semi_join(., Inflammatory, by = c("patid", c("eventdate" = "eventdate.x"), "medcode")) #same as: where medcode in IS_before_PD
dbWriteTable(db, "Referral_Inflam", Referral_Inflam2, overwrite = TRUE)
rm(Referral_Inflam2)
#Finding the number of registered patients per year, selecting only the patients that are on IS
#CHECK WHETHER THE NUMBER OF registered_IS_patients is actually less than the number of registered_patients
registered_IS_patients <- select_by_year(db=db, 
                                         tables="Patient_IS",
                                         columns=c("patid", "practid", "gender", "yob", "crd", "tod","deathdate"),
                                         where="crd < STARTDATE", 
                                         year_range=c(1995 : 2015), 
                                         year_fn =standard_years, first_events)
registered_IBD_patients <- select_by_year(db=db, 
                                          tables="Patient_IBD",
                                          columns=c("patid", "practid", "gender", "yob", "crd", "tod","deathdate"),
                                          where="crd < STARTDATE", 
                                          year_range=c(1995 : 2015), 
                                          year_fn =standard_years, first_events)
registered_Inflam_patients <- select_by_year(db=db, 
                                            tables="Patient_Inflam",
                                            columns=c("patid", "practid", "gender", "yob", "crd", "tod","deathdate"),
                                            where="crd < STARTDATE", 
                                            year_range=c(1995 : 2015), 
                                            year_fn =standard_years, first_events)
registered_patients <- select_by_year(db=db, 
                                      tables="Patient_001",
                                      columns=c("patid", "practid", "gender", "yob", "crd", "tod","deathdate"),
                                      where="crd < STARTDATE", 
                                      year_range=c(1995 : 2015),
                                      year_fn =standard_years, first_events)

str(registered_patients)
str(registered_IS_patients)

table(registered_patients$year)
table(registered_patients$gender) #adds over the years though. So if a male is there for longer than a female, this will affect the counting...
table(registered_patients$yob)

table(registered_IS_patients$year)
table(registered_IS_patients$gender) #adds over the years though. So if a male is there for longer than a female, this will affect the counting...
table(registered_IS_patients$yob)

#Finding the incident cases of the patients per year, selecting only the rows that have a medcode in IS_before_PD.
#check whether this is correct: only selected those with "medcode", shouldnt this also include patid or something?
incident_cases<- select_by_year(db=db, tables=c("Clinical_PD"), #should this contain data from referral file as well? That is what they do in the paper.
                                columns=c("patid", "eventdate", "medcode"),
                                where="eventdate <= ENDDATE", 
                                year_range=c(1995: 2015), year_fn=standard_years, selector_fn =first_events)
incident_cases_IBD <- select_by_year(db=db, tables=c("Clinical_IBD"), #should this contain data from referral file as well? That is what they do in the paper.
                                     columns=c("patid", "eventdate", "medcode"),
                                     where="eventdate <= ENDDATE", 
                                     year_range=c(1995: 2015), year_fn=standard_years, selector_fn =first_events)
incident_cases_Inflam <- select_by_year(db=db, tables=c("Clinical_Inflam"), #should this contain data from referral file as well? That is what they do in the paper.
                                     columns=c("patid", "eventdate", "medcode"),
                                     where="eventdate <= ENDDATE", 
                                     year_range=c(1995: 2015), year_fn=standard_years, selector_fn =first_events)
str(incident_cases)
table(incident_cases$year)
table(incident_cases$medcode)

## Removeduplicatesacrossclinicaland referraltables:
incident_cases%>% group_by(patid,year) %>% arrange(eventdate)%>% distinct(patid,year, .keep_all = T) %>% ungroup-> incident_cases
## All patientsare kept (equivalentto merge(all.x= TRUE))
prevalence_dat<- tbl_df(left_join(registered_IS_patients, incident_cases))
prevalence_dat<- prev_terms(prevalence_dat)

###########################################################
###########################################################


PatientInfo2 <- tbl_df(select_events(db, tab = "Patient_001", columns = c("patid", "gender", "yob", "deathdate")))%>%
  group_by(patid, yob) %>% 
  distinct(patid, .keep_all = TRUE) %>% 
  ungroup()
#Patient demographics for those on IS medication. Added age at time of first prescription. 
ISInfo <- tbl_df(left_join(SelFirstImmuneProducts, PatientInfo2) %>%
  semi_join(., three_and_two)%>%
  mutate(ageatstart = year(eventdate)-yob))


#5 year bands
agebreaks <- c(0,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,500) #make another exclusion criterium earlier on, because now very very young patients are included...
agelabels <- c("0-14","15-19","20-24","25-29", "30-34","35-39","40-44", "45-49","50-54","55-59","60-64","65-69", "70-74", "75-79","80-84","85-89","90+")
setDT(ISInfo)[ , agegroups := cut(ageatstart, 
                                  breaks = agebreaks, 
                                  right = FALSE, 
                                  labels = agelabels)]
ESP2013 <- c(16000,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,5500,5000,4000,2500,1500,1000,
             16000,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,5500,5000,4000,2500,1500,1000)
gender <- c("Male", "Male","Male", "Male","Male", "Male","Male", "Male","Male", "Male","Male", "Male","Male", "Male","Male", "Male","Male",
            "Female", "Female","Female", "Female","Female", "Female","Female", "Female","Female", "Female","Female", "Female","Female", "Female","Female", "Female","Female")
agelabels2 <- c("0-14","15-19","20-24","25-29", "30-34","35-39","40-44", "45-49","50-54","55-59","60-64","65-69", "70-74", "75-79","80-84","85-89","90+",
                "0-14","15-19","20-24","25-29", "30-34","35-39","40-44", "45-49","50-54","55-59","60-64","65-69", "70-74", "75-79","80-84","85-89","90+") 
ESP2013 <- data.frame(agelabels = agelabels2,ESP2013,gender)
prevalence_dat5 <-  tbl_df(merge(x = prevalence_dat, y = ISInfo[ , c("patid", "ageatstart", "agegroups")], by = c("patid"), all.x=TRUE))
prevalence_dat5 <-  tbl_df(merge(x = prevalence_dat5, y = summary_diseases_pp[ , c("patid", "nr_of_diseases")], by = c("patid"), all.x=TRUE))
prevalence_dat5$gender <- as.character.factor(factor(prevalence_dat5$gender, levels=c(0,1,2,3,4), labels=c("Data Not Entered","Male", "Female", "Indeterminate", "Unknown")))
  #total incidence age and gender adjusted 5 year bands                                                                                           
totals3 <- prev_totals(prevalence_dat5, included_totals = c("gender"), time_var= "agegroups", person_years = 100000)
totals3$incidence$gender_counts <- left_join(totals3$incidence$gender_counts,ESP2013, by = c(c("agegroups" = "agelabels"),"gender"))
totals3$incidence$gender_counts$adjustment <- totals3$incidence$gender_counts$ESP2013/100000
totals3$incidence$gender_counts$adj_incid <- totals3$incidence$gender_counts$incidence * totals3$incidence$gender_counts$adjustment
totals3$prevalence$gender_counts <-  left_join(totals3$prevalence$gender_counts,ESP2013, by = c(c("agegroups" = "agelabels"),"gender"))
totals3$prevalence$gender_counts$adjustment <- totals3$prevalence$gender_counts$ESP2013/100000
totals3$prevalence$gender_counts$adj_prev <- totals3$prevalence$gender_counts$prevalence * totals3$prevalence$gender_counts$adjustment
totals3$incidence$gender_counts$adj_denom <- totals3$incidence$gender_counts$denominator * totals3$incidence$gender_counts$ESP2013 /100000


#################################################################################
#10 year bands
ISInfo10 <- tbl_df(left_join(SelFirstImmuneProducts, PatientInfo2) %>%
                     semi_join(., three_and_two)%>%
                     mutate(ageatstart = year(eventdate)-yob))
PDInfo <- left_join(SelFirstParkinsoncodes, PatientInfo2) %>% 
  semi_join(., Final_True_PD) %>% 
  group_by(patid, medcode) %>% 
  distinct(eventdate, .keep_all = TRUE) %>% 
  ungroup %>%
  mutate(AgeatOnset = year(eventdate) - yob)

agelabels102 <- c("30-39","40-49","50-59","60-69", "70-79","80+",
                  "30-39","40-49","50-59","60-69", "70-79","80+")
ESP2013_10 <- c(13500,14000,13500,11500,9000,5000,13500,14000,13500,11500,9000,5000)
gender_10 <- c("Male", "Male","Male", "Male","Male", "Male",
               "Female", "Female","Female", "Female","Female", "Female")
ESP2013_10 <- data.frame(agelabels = agelabels102,ESP2013_10,gender_10)
prevalence_dat10 <-  tbl_df(merge(x = prevalence_dat, y = ISInfo10[ , c("patid", "ageatstart")], by = c("patid"), all.x=TRUE))
prevalence_dat10 <- tbl_df(merge(x = prevalence_dat10, y = PDInfo[ , c("patid","AgeatOnset")], by=c("patid"), all.x = TRUE))
median_followup <- prevalence_dat10%>% group_by(patid) %>% filter(eventdate >0)%>% summarise(median_followup = as.integer(sum(followup[which(followup>0)])))

prevalence_dat10 <- prevalence_dat10 %>% mutate(Median_age = case_when(!is.na(prevalence_dat10$AgeatOnset) >0~ prevalence_dat10$AgeatOnset,
                                                                       TRUE ~ as.double(as.integer(prevalence_dat10$ageatstart + 3.99))))

prevalence_dat10 <- prevalence_dat10 %>% mutate(Age = case_when(!is.na(prevalence_dat10$AgeatOnset) >0~ prevalence_dat10$AgeatOnset,
                                                  TRUE ~ prevalence_dat10$ageatstart ))

agebreaks10 <- c(0,30,40,50,60,70,80,500)
agelabels10 <- c("0-29","30-39","40-49","50-59","60-69", "70-79","80+")
setDT(prevalence_dat10)[ , agegroups := cut(Median_age, 
                                    breaks = agebreaks10, 
                                    right = FALSE, 
                                    labels = agelabels10)]
prevalence_dat10 <-  tbl_df(merge(x = prevalence_dat10, y = summary_diseases_pp[ , c("patid", "nr_of_diseases")], by = c("patid"), all.x=TRUE))
#prevalence_dat10 <- tbl_df(merge(x = prevalence_dat10, y = PDInfo[ , c("patid", "nr_of_diseases")], by = c("patid"), all.x=TRUE))
prevalence_dat10$gender <- as.character.factor(factor(prevalence_dat10$gender, levels=c(0,1,2,3,4), labels=c("Data Not Entered","Male", "Female", "Indeterminate", "Unknown")))
prevalence_dat11 <- full_join(nr_of_prescriptions, prevalence_dat10)
prevalence_dat12 <- full_join(nr_of_prescriptions_azamet, prevalence_dat10)
prevalence_dat13 <- full_join(summary_diseases_pp, prevalence_dat10)
#total incidence age and gender adjusted per 10 year bands
totals10 <- prev_totals2(prevalence_dat10, included_totals = c("gender"), time_var= c("agegroups"), person_years = 100000)
totals10$incidence$gender_counts <- left_join(totals10$incidence$gender_counts,ESP2013_10, by = c(c("agegroups" = "agelabels"),c("gender" = "gender_10"))) %>% arrange(agegroups, gender)
totals10$incidence$gender_counts$adjustment <- totals10$incidence$gender_counts$ESP2013_10/(100000)
totals10$incidence$gender_counts$adj_incid <- totals10$incidence$gender_counts$incidence * (totals10$incidence$gender_counts$adjustment)
totals10$prevalence$gender_counts <-  left_join(totals10$prevalence$gender_counts,ESP2013_10, by = c(c("agegroups" = "agelabels"),c("gender" = "gender_10")))
totals10$prevalence$gender_counts$adjustment <- totals10$prevalence$gender_counts$ESP2013_10/(100000)
totals10$prevalence$gender_counts$adj_prev <- totals10$prevalence$gender_counts$prevalence * (totals10$prevalence$gender_counts$adjustment)
totals10$incidence$gender_counts <- totals10$incidence$gender_counts %>% filter(gender != "Indeterminate")

#group by nr_of_prescriptions
totals11 <- prev_totals2(prevalence_dat11, included_totals = c("gender", "total_level"), time_var= c("agegroups"), person_years = 100000)
#group by nr_of_prescriptions of azathioprine and methotrexate
totals12 <- prev_totals2(prevalence_dat12, included_totals = c("gender", "total_level"), time_var= c("agegroups"), person_years = 100000)
#group by nr_of_comorbidities
totals13 <- prev_totals2(prevalence_dat13, included_totals = c("gender","nr_of_diseases"),time_var= c("agegroups"), person_years = 100000)

##############################################################
#IBD
prevalence_dat_IBD<- tbl_df(left_join(registered_IBD_patients, incident_cases_IBD))
prevalence_dat_IBD<- prev_terms(prevalence_dat_IBD)
prevalence_dat_IBD2 <-  tbl_df(merge(x = prevalence_dat_IBD, y = ISInfo10[ , c("patid", "ageatstart", "agegroups")], by = c("patid"), all.x=TRUE))
prevalence_dat_IBD2 <-  tbl_df(merge(x = prevalence_dat_IBD2, y = summary_diseases_pp[ , c("patid", "nr_of_diseases")], by = c("patid"), all.x=TRUE))
prevalence_dat_IBD2$gender <- as.character.factor(factor(prevalence_dat_IBD$gender, levels=c(0,1,2,3,4), labels=c("Data Not Entered","Male", "Female", "Indeterminate", "Unknown")))
totals_IBD2 <- prev_totals(prevalence_dat_IBD2, included_totals = c("gender"), time_var= "agegroups", person_years = 100000)
totals_IBD2$incidence$gender_counts <- left_join(totals_IBD2$incidence$gender_counts,ESP2013_10, by = c(c("agegroups" = "agelabels"),c("gender"="gender_10")))
totals_IBD2$incidence$gender_counts

#Inflam
prevalence_dat_Inflam<- tbl_df(left_join(registered_Inflam_patients, incident_cases_Inflam))
prevalence_dat_Inflam<- prev_terms(prevalence_dat_Inflam)
prevalence_dat_Inflam2 <-  tbl_df(merge(x = prevalence_dat_Inflam, y = ISInfo10[ , c("patid", "ageatstart", "agegroups")], by = c("patid"), all.x=TRUE))
prevalence_dat_Inflam2 <-  tbl_df(merge(x = prevalence_dat_Inflam2, y = summary_diseases_pp[ , c("patid", "nr_of_diseases")], by = c("patid"), all.x=TRUE))
prevalence_dat_Inflam2$gender <- as.character.factor(factor(prevalence_dat_Inflam$gender, levels=c(0,1,2,3,4), labels=c("Data Not Entered","Male", "Female", "Indeterminate", "Unknown")))
totals_Inflam2 <- prev_totals(prevalence_dat_Inflam2, included_totals = c("gender"), time_var= "agegroups", person_years = 100000)
totals_Inflam2$incidence$gender_counts <- left_join(totals_Inflam2$incidence$gender_counts,ESP2013_10, by = c(c("agegroups" = "agelabels"), c("gender" = "gender_10")))
totals_Inflam2$incidence$gender_counts






#Total incidence age adjusted
totals4 <- prev_totals(prevalence_dat5, included_totals = c("agegroups"), time_var= "agegroups", person_years = 100000)
totals4$incidence$agegroups_counts <- left_join(totals4$incidence$agegroups_counts,ESP20132, by = c(c("agegroups" = "agelabels")))
totals4$incidence$agegroups_counts$adjustment <- totals4$incidence$agegroups_counts$ESP20132/200000
totals4$incidence$agegroups_counts$adj_incid <- totals4$incidence$agegroups_counts$incidence * totals4$incidence$agegroups_counts$adjustment
totals4$prevalence$agegroups_counts <-  left_join(totals4$prevalence$agegroups_counts,ESP20132, by = c(c("agegroups" = "agelabels")))
totals4$prevalence$agegroups_counts$adjustment <- totals4$prevalence$agegroups_counts$ESP20132/200000
totals4$prevalence$agegroups_counts$adj_prev <- totals4$prevalence$agegroups_counts$prevalence * totals4$prevalence$agegroups_counts$adjustment
totals4$incidence$agegroups_counts$adj_denom <- totals4$incidence$agegroups_counts$denominator * totals4$incidence$agegroups_counts$ESP20132 /200000

                                                                                             
                                                                                             

#total incidence table with CI. Hardly any different if grouping by 5 year bands. 
#crude vs adjusted incidence done in accordance with F. Bray age standardization 1967
total_adj_incid <-totals10$incidence$gender_counts %>% ungroup() %>% group_by(gender) %>% 
  summarise(crude_incid = 100000* sum(numerator, na.rm = TRUE)/(sum(denominator, na.rm = TRUE)),
            Lower.limit.crude = (100000 /(sum(denominator, na.rm = TRUE)))*(sum(numerator, na.rm = TRUE) -(1.96 *sqrt(sum(numerator, na.rm = TRUE)))),
            Upper.limit.crude = (100000 /(sum(denominator, na.rm = TRUE)))*(sum(numerator, na.rm = TRUE) +(1.96 *sqrt(sum(numerator, na.rm = TRUE)))), 
            total_adj_incid =sum(adj_incid, na.rm = TRUE),
            Lower.limit.adj = sum(adj_incid, na.rm = TRUE) - (1.96* sum(adj_incid, na.rm = TRUE)/sqrt(sum(numerator, na.rm = TRUE))),
            Upper.limit.adj = sum(adj_incid, na.rm = TRUE) + (1.96* sum(adj_incid, na.rm = TRUE)/sqrt(sum(numerator, na.rm = TRUE))))
total_adj_incid <-  total_adj_incid %>% 
  summarise(gender = "All",
            crude_incid = mean(crude_incid),
            Lower.limit.crude = mean(Lower.limit.crude),
            Upper.limit.crude = mean(Upper.limit.crude),
            total_adj_incid = mean(total_adj_incid),
            Lower.limit.adj = mean(Lower.limit.adj),
            Upper.limit.adj = mean(Upper.limit.adj)) %>%
  bind_rows(total_adj_incid, .)


total_adj_incid <-totals10$incidence$gender_counts %>% ungroup() %>% group_by(gender) %>% 
  summarise(crude_incid = 100000* sum(numerator)/sum(denominator),
            total_adj_incid =sum(adj_incid))


total_adj_prev <-totals3$prevalence$gender_counts %>% ungroup() %>% group_by(gender) %>% 
  summarise(total_prev =sum(adj_prev), 
            mean_prev = mean(adj_prev, na.rm= TRUE), 
            sd_prev = sd(adj_prev, na.rm = TRUE), 
            n_prev = n())%>%
  mutate(se_prev = sd_prev/sqrt(n_prev), 
         lower.ci.prev = total_prev -qt(1-(0.05 / 2), n_prev - 1) * se_prev,  
         upper.ci.prev = total_prev + qt(1 - (0.05 / 2), n_prev - 1) * se_prev)





#almost correct values for incidence if I leave out the referral file. Then apply prev_terms
#, dont remove duplicates from incidence and then I am left with 180 incident cases. 
#out of 184, so almost there..


install.packages("plotly")
library(plotly)



#5 year bands
ggplotly(ggplot(totals3$incidence$gender_counts, aes(agegroups, adj_incid)) +   
           geom_bar(aes(fill = factor(gender)), position = "dodge", stat="identity")+
           ggtitle("Age and Gender Adjusted Incidence per 100.000")+
           xlab("Age Groups") + ylab("Adjusted Incidence")+
           theme(legend.position="top", legend.title = element_blank()))
#10 year bands
ggplotly(ggplot(totals10$incidence$gender_counts, aes(agegroups, adj_incid)) +   
           geom_bar(aes(fill = factor(gender)), position = "dodge", stat="identity")+
           ggtitle("Age and Gender Adjusted Incidence per 100.000")+
           xlab("Age Groups") + ylab("Adjusted Incidence")+
           theme(legend.position="top", legend.title = element_blank()))
ggplotly(ggplot(totals10$incidence$gender_counts, aes(agegroups, incidence)) +   
           geom_bar(aes(fill = factor(gender)), position = "dodge", stat="identity")+
           ggtitle("Incidence per 100.000")+
           xlab("Age Groups") + ylab("Incidence")+
           theme(legend.position="top", legend.title = element_blank()))
ggplotly(ggplot(totals_IBD2$incidence$gender_counts, aes(agegroups, incidence)) +   
           geom_bar(aes(fill = factor(gender)), position = "dodge", stat="identity")+
           ggtitle("Age and Gender UNadjusted Incidence per 100.000")+
           xlab("Age Groups") + ylab("UNadjusted Incidence")+
           theme(legend.position="top", legend.title = element_blank()))
  #age and nr of prescriptions
ggplotly(ggplot(totals11$incidence$total_level_counts, aes(agegroups, incidence)) +   
           geom_bar(aes(fill = factor(total_level, levels = c("low","medium","high"))), position = "dodge", stat="identity")+
           ggtitle("Incidence per 100.000 per immunosuppressant prescription amount")+
           xlab("Age Groups") + ylab("Incidence")+
           theme(legend.position="top", legend.title = element_blank()))
  #age and nr of prescriptions azathioprine and methotrexate
ggplotly(ggplot(totals12$incidence$total_level_counts, aes(agegroups, incidence)) +   
           geom_bar(aes(fill = factor(total_level, levels = c("low","medium","high"))), position = "dodge", stat="identity")+
           ggtitle("Incidence per 100.000 per immunosuppressant prescription amount")+
           xlab("Age Groups") + ylab("Incidence")+
           theme(legend.position="top", legend.title = element_blank()))


#prevalence 10 year band
ggplotly(ggplot(totals10$prevalence$gender_counts, aes(agegroups, prevalence)) +   
           geom_bar(aes(fill = factor(gender)), position = "dodge", stat="identity")+
           ggtitle("Prevalence per 100.000")+
           xlab("Age Groups") + ylab("Prevalence")+
           theme(legend.position="top", legend.title = element_blank()))


#like figure 1 from campaign
ggplotly(ggplot(data=totals3$incidence$gender_counts, aes(x=agegroups, y=adj_incid, group=gender)) +
           geom_line(aes(linetype = gender))+
           add_trace(mean)+
           geom_point())
#same figure not age and gender adjusted with std population
ggplotly(ggplot(data=totals10$incidence$gender_counts, aes(x=agegroups, y=incidence, group=gender)) +
           geom_line(aes(linetype = gender))+
           geom_point())
ggplotly(ggplot(data=totals10$incidence$gender_counts, aes(x=agegroups, y=adj_incid, group=gender)) +
           geom_line(aes(linetype = gender))+
           geom_point())
#same figure by age and nr of prescriptions
ggplotly(ggplot(data=totals11$incidence$total_level_counts, aes(x=agegroups, y=incidence, group=total_level)) +
           geom_line(aes(linetype = total_level))+
           geom_point()) 
#THE VALUES FOR THE HIGHER AGES FOR THE INCIDENCE ARE MUCH TOO LOW, BUT STILL I GET THE SAME TOTAL INCIDENCE???
#HOW IS THAT POSSIBLE??
#EVEN IF I DIVIDE NOT BY 100.000 BUT BY THE SUM OF esp2013 IN THE TOTALS DATAFRAME, NOT CORRECT..

ggplotly(ggplot(totals2$incidence$nr_of_diseases_counts, aes(agegroups, incidence)) +   
           geom_bar(aes(fill = factor(nr_of_diseases)), position = "dodge", stat="identity")+
           ggtitle("Age and Gender Adjusted Incidence per 100.000")+
           xlab("Age Groups") + ylab("Adjusted Incidence")+
           theme(legend.position="top", legend.title = element_blank()))

ggplotly(ggplot(totals3$prevalence$gender_counts, aes(agegroups, prevalence)) +   
           geom_bar(aes(fill = factor(gender)), position = "dodge", stat="identity")+
           theme(legend.position="top", legend.title = element_blank()))



###############################
# go to Confirming Real PD script here.
###############################