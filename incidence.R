#PD incidence

Patient_IS <- tbl_df(select_events(db, tab = "Patient_001", columns = '*', where = "patid %in% .(three_and_two$patid)"))
dbWriteTable(db, "Patient_IS2", Patient_IS, append = TRUE)
registered_IS_patients <- select_by_year(db=db, 
                                         tables="Patient_IS2",
                                         columns=c("patid", "practid", "gender", "yob", "crd", "tod","deathdate"),
                                         where="crd < STARTDATE", 
                                         year_range=c(1995 : 2015), 
                                         year_fn =standard_years, first_events)
registered_patients <- select_by_year(db=db, 
                                      tables="Patient_001",
                                      columns=c("patid", "practid", "gender", "yob", "crd", "tod","deathdate"),
                                      where="crd < STARTDATE", 
                                      year_range=c(1995 : 2015),year_fn =standard_years, first_events)
#try the following:
where_q <- "crd < STARTDATE & patid %in% .(three_and_two$patid)"
registered_IS_patients <- select_by_year(db=db, tables="Patient_001",
                                      columns=c("patid", "practid", "gender", "yob", "crd", "tod","deathdate"),
                                      where=where_q, year_range=c(1995 : 2015),year_fn =standard_years, first_events)



str(registered_patients)
str(registered_IS_patients)


table(registered_patients$year)
table(registered_patients$gender) #adds over the years though. So if a male is there for longer than a female, this will affect the counting...
table(registered_patients$yob)

table(registered_IS_patients$year)
table(registered_IS_patients$gender) #adds over the years though. So if a male is there for longer than a female, this will affect the counting...
table(registered_IS_patients$yob)

incident_cases<- select_by_year(db=db, tables=c("Clinical", "Referral"),
                                columns=c("patid", "eventdate", "medcode"),
                                where="medcode %in% .(True_PD$medcode) & eventdate <= ENDDATE", #use IS_before_PD
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