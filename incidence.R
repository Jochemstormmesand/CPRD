#PD incidence
#Required to avoid memory exhaust due to WHERE IN clause. 
Patient_IS2 <- tbl_df(sqldf("SELECT * FROM Patient_001", connection = db))
Patient_IS2 <- semi_join(Patient_IS2, three_and_two_marker)
dbWriteTable(db, "Patient_IS", Patient_IS2, append = TRUE)
rm(Patient_IS2)
Clinical_PD2 <- tbl_df(sqldf("SELECT * FROM Clinical_All", connection = db))
Clinical_PD2 <- semi_join(Clinical_PD2, IS_before_PD, by = "medcode")
dbWriteTable(db, "Clinical_PD", Clinical_PD2, append = TRUE)
rm(Clinical_PD2)
Referral_PD2 <- tbl_df(sqldf("SELECT * FROM Referral_001", connection = db))
Referral_PD2 <- semi_join(Referral_PD2, IS_before_PD, by = "medcode")
dbWriteTable(db, "Referral_PD", Referral_PD2, append = TRUE)
rm(Referral_PD2)

#Finding the number of registered patients per year, selecting only the patients that are on IS
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
                                      year_range=c(1995 : 2015),year_fn =standard_years, first_events)



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
incident_cases<- select_by_year(db=db, tables=c("Clinical_PD", "Referral_PD"),
                                columns=c("patid", "eventdate", "medcode"),
                                where="eventdate <= ENDDATE", #use IS_before_PD
                                year_range=c(1995: 2015), year_fn=standard_years, selector_fn =first_events)
str(incident_cases)
table(incident_cases$year)
table(incident_cases$medcode)

## All patientsare kept (equivalentto merge(all.x= TRUE))
prevalence_dat<- left_join(registered_IS_patients, incident_cases)
## Removeduplicatesacrossclinicaland referraltables:
incident_cases%>% group_by(patid,year) %>% arrange(eventdate)%>% distinct() %>% ungroup-> incident_cases

prevalence_dat<- prev_terms(prevalence_dat)
totals<- prev_totals(prevalence_dat)
#Prevalence and incidence of Parkinson from 1995 to 2015
totals$prevalence$year_counts
totals$incidence$year_counts


#hist(totals$prevalence$year_counts$prevalence)
plot(totals$incidence$year_counts$year, totals$incidence$year_counts$numerator)
barplot(totals$prevalence$year_counts$numerator)

prevalence <- plot_ly(
  x = totals$prevalence$year_counts$year,
  y = totals$prevalence$year_counts$prevalence,
  name = "Prevalence",
  type = "bar"
)

incidence<- plot_ly(
  x = totals$incidence$year_counts$year,
  y = totals$incidence$year_counts$incidence,
  name = "Incidence",
  type = "bar"
)


###############################
# go to Confirming Real PD script here.
###############################