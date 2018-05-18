#PD incidence
#Required to avoid memory exhaust due to WHERE IN clause. 
Patient_IS2 <- tbl_df(sqldf("SELECT * FROM Patient_001", connection = db)) %>% 
  semi_join(., three_and_two) #same as: where patid in three_and_two_marker 
dbWriteTable(db, "Patient_IS", Patient_IS2, overwrite = TRUE)
rm(Patient_IS2)
Clinical_PD2 <- tbl_df(sqldf("SELECT patid, eventdate, medcode FROM Clinical_All", connection = db)) %>%
  semi_join(., IS_before_PD_All) #same as: where medcode in IS_before_PD
#!!!!!!!!!!!!!IMPORTANT: SHOULD THIS BE JOINED BY MEDCODE OR BY PATID????????????
dbWriteTable(db, "Clinical_PD", Clinical_PD2, overwrite = TRUE)
rm(Clinical_PD2)
Referral_PD2 <- tbl_df(sqldf("SELECT * FROM Referral_001", connection = db))%>%
  semi_join(., IS_before_PD_All, by = c("patid", "eventdate", "medcode")) #same as: where medcode in IS_before_PD
dbWriteTable(db, "Referral_PD", Referral_PD2, overwrite = TRUE)
rm(Referral_PD2)

#Finding the number of registered patients per year, selecting only the patients that are on IS
#CHECK WHETHER THE NUMBER OF registered_IS_patients is actually less than the number of registered_patients
registered_IS_patients <- select_by_year(db=db, 
                                         tables="Patient_IS",
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

agebreaks <- c(0,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,500) #make another exclusion criterium earlier on, because now very very young patients are included...
agelabels <- c("0-14","15-19","20-24","25-29", "30-34","35-39","40-44", "45-49","50-54","55-59","60-64","65-69", "70-74", "75-79","80-84","85-89","90+")
setDT(ISInfo)[ , agegroups := cut(ageatstart, 
                                  breaks = agebreaks, 
                                  right = FALSE, 
                                  labels = agelabels)]
prevalence_dat <-  tbl_df(merge(x = prevalence_dat, y = ISInfo[ , c("patid", "ageatstart", "agegroups")], by = c("patid"), all.x=TRUE))


agelabels2 <- c("0-14","15-19","20-24","25-29", "30-34","35-39","40-44", "45-49","50-54","55-59","60-64","65-69", "70-74", "75-79","80-84","85-89","90+",
                "0-14","15-19","20-24","25-29", "30-34","35-39","40-44", "45-49","50-54","55-59","60-64","65-69", "70-74", "75-79","80-84","85-89","90+") 
SEERstat <- c(261229,84670,82171,79272,76073,71475,65877,60379,53681,45484,37187,29590,22092,15195,9097,4398,1500,400,50)
ESP2013 <- c(16000,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,5500,5000,4000,2500,1500,1000,
              16000,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,5500,5000,4000,2500,1500,1000)
gender <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
            2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)

ESP2013 <- data.frame(agelabels = agelabels2,ESP2013,gender)


totals<- prev_totals(prevalence_dat, person_years = 100000)
totals2 <- prev_totals(prevalence_dat, included_totals = c("year", "practid", "gender","agegroups"), person_years = 100000)
totals1 <- prev_totals(prevalence_dat, included_totals = c("year", "practid", "gender"), time_var= "agegroups")

totals3 <- prev_totals(prevalence_dat, included_totals = c("gender"), time_var= "agegroups", person_years = 100000)
totals3$incidence$gender_counts <- left_join(totals3$incidence$gender_counts,ESP2013, by = c(c("agegroups" = "agelabels"),"gender"))
totals3$incidence$gender_counts$adjustment <- totals3$incidence$gender_counts$ESP2013/100000
totals3$incidence$gender_counts$adj_incid <- totals3$incidence$gender_counts$incidence * totals3$incidence$gender_counts$adjustment
totals3$prevalence$gender_counts <-  left_join(totals3$prevalence$gender_counts,ESP2013, by = c(c("agegroups" = "agelabels"),"gender"))
totals3$prevalence$gender_counts$adjustment <- totals3$prevalence$gender_counts$ESP2013/100000
totals3$prevalence$gender_counts$adj_prev <- totals3$prevalence$gender_counts$prevalence * totals3$prevalence$gender_counts$adjustment
totals3$incidence$gender_counts$adj_denom <- totals3$incidence$gender_counts$denominator * totals3$incidence$gender_counts$ESP2013 /100000



total_adj_incid <-totals3$incidence$gender_counts %>% ungroup() %>% group_by(gender) %>% 
  summarise(total_incid =sum(adj_incid), 
            mean_incid = mean(adj_incid, na.rm= TRUE), 
            sd_incid = sd(adj_incid, na.rm = TRUE), 
            n_incid = n())%>%
  mutate(se_incid = sd_incid/sqrt(n_incid), 
         lower.ci.incid = total_incid -qt(1-(0.05 / 2), n_incid - 1) * se_incid,  
         upper.ci.incid = total_incid + qt(1 - (0.05 / 2), n_incid - 1) * se_incid)
total_adj_incid_all <- total_adj_incid %>% group_by(gender)


total_adj_prev <-totals3$prevalence$gender_counts %>% ungroup() %>% group_by(gender) %>% 
  summarise(total_prev =sum(adj_prev), 
            mean_prev = mean(adj_prev, na.rm= TRUE), 
            sd_prev = sd(adj_prev, na.rm = TRUE), 
            n_prev = n())%>%
  mutate(se_prev = sd_prev/sqrt(n_prev), 
         lower.ci.prev = total_prev -qt(1-(0.05 / 2), n_prev - 1) * se_prev,  
         upper.ci.prev = total_prev + qt(1 - (0.05 / 2), n_prev - 1) * se_prev)




#Prevalence and incidence of Parkinson from 1995 to 2015
totals$prevalence$year_counts
totals$incidence$year_counts
totals$incidence$year_counts <- totals$incidence$year_counts %>%
  mutate(gyear = case_when(
    totals$incidence$year_counts$year < 2001 ~ "1995-2000",
    totals$incidence$year_counts$year>= 2001 & totals$incidence$year_counts$year<2006 ~ "2001-2005",
    totals$incidence$year_counts$year>=2006 & totals$incidence$year_counts$year<2011 ~ "2006-2010",
    TRUE ~ "2011-2015"
  ))
#almost correct values for incidence if I leave out the referral file. Then apply prev_terms
#, dont remove duplicates from incidence and then I am left with 180 incident cases. 
#out of 184, so almost there..

#hist(totals$prevalence$year_counts$prevalence)
plot(totals$incidence$year_counts$year, totals$incidence$year_counts$numerator)
barplot(totals$prevalence$year_counts$prevalence)

install.packages("plotly")
library(plotly)

prevalence <- plot_ly(
  x = totals$prevalence$year_counts$year,
  y = totals$prevalence$year_counts$prevalence,
  name = "Prevalence",
    type = "bar") %>% 
  layout(title = "PD Prevalence per year",
            yaxis = list(title = 'Prevalence (%)'),
            xaxis = list(title = 'Year'))

prevalence100 <- plot_ly(
  x = totals$prevalence$year_counts$year,
  y = totals$prevalence$year_counts$numerator2,
  name = "Prevalence",
  type = "bar") %>% 
  layout(title = "PD Prevalence per 100.000 per year",
         yaxis = list(title = 'Prevalence per 100.000 patients'),
         xaxis = list(title = 'Year'))

prevalence_agegroups <-  plot_ly(
  x = totals3$prevalence$gender_counts$agegroups,
  y = totals3$prevalence$gender_counts$prevalence,
  name = "Prevalence",
  type = "bar") %>% 
  layout(title = "PD Prevalence per 100.000 per year",
         yaxis = list(title = 'Prevalence per 100.000 patients'),
         xaxis = list(title = 'Year'))

prevalence_agegroups_adj <- totals3$prevalence$gender_counts %>% group_by(agegroups) %>% 
  plot_ly(
  x = totals3$prevalence$gender_counts$gender,
  y = totals3$prevalence$gender_counts$adj_prev,
  colors = "Reds",
  name = "Prevalence",
  type = "bar") %>% 
  layout(title = "PD Prevalence per 100.000 per year",
         yaxis = list(title = 'Prevalence per 100.000 patients'),
         xaxis = list(title = 'Year'))


incidence <- plot_ly(x = totals$incidence$year_counts$incidence, type = "histogram",
                     histnorm = "probability")

incidence<- plot_ly(
  x = totals$incidence$year_counts$gyear,
  y = totals$incidence$year_counts$incidence,
  name = "Incidence",
    type = "bar") %>% 
  layout(title = "PD Incidence per year",
             yaxis = list(title = 'Incidence (%)'),
             xaxis = list(title = 'Year'))

incidence100<- plot_ly(
  x = totals$incidence$year_counts$year,
  y = totals$incidence$year_counts$numerator2,
  name = "Incidence",
  type = "bar") %>% 
  layout(title = "PD Incidence per 100.000 per year",
         yaxis = list(title = 'Incidence per 100.000 patients'),
         xaxis = list(title = 'Year'))

ggplotly(ggplot(totals3$incidence$gender_counts, aes(agegroups, adj_incid)) +   
           geom_bar(aes(fill = factor(gender)), position = "dodge", stat="identity")+
           ggtitle("Age and Gender Adjusted Incidence per 100.000")+
           xlab("Age Groups") + ylab("Adjusted Incidence")+
           theme(legend.position="top", legend.title = element_blank()))

ggplotly(ggplot(totals3$prevalence$gender_counts, aes(agegroups, adj_prev)) +   
           geom_bar(aes(fill = factor(gender)), position = "dodge", stat="identity")+
           theme(legend.position="top", legend.title = element_blank()))



###############################
# go to Confirming Real PD script here.
###############################