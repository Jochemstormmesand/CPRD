#eHR Statistics
install.packages("plotly")
library(plotly)

#Patient Demographics
PatientInfo <- tbl_df(sqldf("SELECT * FROM Patient ORDER BY patid", connection = db))
PatientInfo <- PatientInfo %>% group_by(patid, yob) %>% distinct(patid, .keep_all = TRUE) %>% ungroup -> PatientInfo

PatientInfo2 <- tbl_df(select_events(db, tab = "Patient", columns = c("patid", "gender", "yob", "deathdate")))
PatientInfo2 <- PatientInfo2 %>% group_by(patid, yob) %>% distinct(patid, .keep_all = TRUE) %>% ungroup -> PatientInfo2

#####CALCULATE AGE!!!!
#what to do with patients that have another toreason than "death"? Some have become 130 otherwise, 
#and that seems illogical. Presumably TRA code 13 = "other reason" is used for death as well.
#Calculate age of patients with parkinson at moment of first admission
PDInfo <- left_join(SelFirstParkinsoncodes, PatientInfo2)
PDInfo <- PDInfo %>% group_by(patid, medcode) %>% distinct(eventdate, .keep_all = TRUE) %>% ungroup -> PDInfo
PDInfoAge <- PDInfo %>% mutate(deathdateyear = as.numeric(format(as.Date(PDInfo$deathdate, format="%Y-%m-%d"),"%Y")))
PDInfoAge <- PDInfoAge %>% mutate(eventdateyear = as.numeric(format(as.Date(PDInfoAge$eventdate, format="%Y-%m-%d"),"%Y")))
PDInfoAge <- PDInfoAge %>% mutate(AgeatOnset = PDInfoAge$eventdateyear - PDInfoAge$yob)
#PDInfoAge <- PDInfoAge %>% mutate(todyear = as.numeric(format(as.Date(PDInfoAge$tod, format="%Y-%m-%d"),"%Y")))

## Possibly needed to calculate age at other timepoint.
# PatientInfo <- PatientInfo %>% mutate(deathdateyear = as.numeric(format(as.Date(PatientInfo$deathdate, format="%Y-%m-%d"),"%Y")))
# PatientInfo <- PatientInfo %>% mutate(todyear = as.numeric(format(as.Date(PatientInfo$tod, format="%Y-%m-%d"),"%Y")))
# PatientInfo <- PatientInfo %>% mutate(age3 = PatientInfo$todyear - PatientInfo$yob)
# PatientInfo <- PatientInfo %>% mutate(age2 = Startyear - PatientInfo$yob)
# PatientInfo <- PatientInfo %>% mutate(age1 = PatientInfo$deathdateyear - PatientInfo$yob)
# PatientInfo$age <- PatientInfo$age1
# my.na <- is.na(PatientInfo$age)
# PatientInfo$age[my.na] <- PatientInfo$age2[my.na]
## Can I do this? maybe they didnt die.. could be inaccurate. 

#Age
#PD patients age at PD onset
hist(PDInfoAge$AgeatOnset)
as.integer(PDInfoAge$AgeatOnset)
min(PDInfoAge$AgeatOnset, na.rm = TRUE)
max(PDInfoAge$AgeatOnset, na.rm = TRUE) 
mean(PDInfoAge$AgeatOnset, na.rm = TRUE)
median(PDInfoAge$AgeatOnset, na.rm = TRUE)
table(PDInfoAge$AgeatOnset)

#Gender
Sex = table(PatientInfo$gender)
SexPD = table(PDInfo$gender)
barplot(Sex, main = "Sex", xlab = "Sex", ylab = "Number of Patients")


#Incidence and Prevalence of PD
nParkinsonpatients <- length(unique(SelParkinsoncodes$patid))
table(registered_patients$year)
table(incident_cases$year)
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

incidence <- plot_ly(
  x = totals$incidence$year_counts$year,
  y = totals$incidence$year_counts$incidence,
  name = "Incidence",
  type = "bar"
)


nr_of_pp_Parkinsoncode <- filter(ppm, ppm$medcode %in% Parkinsoncodes)
nr_of_pp_ParkinsonEXcode <- filter(ppm, ppm$medcode %in% ParkinsonExclusioncodes)
nr_of_PDcodes_PP <- tbl_df(sqldf("SELECT patid, medcode, COUNT(DISTINCT medcode) AS nr_of_PDcodes FROM SelParkinsoncodes GROUP BY patid ORDER BY nr_of_PDcodes DESC"))
nr_of_pp_Immunosup <- tbl_df(sqldf("SELECT prodcode, COUNT(DISTINCT patid) AS patients FROM SelImmuneProducts GROUP BY prodcode ORDER BY patients DESC"))

three_and_two <- three_and_two %>% mutate(drug_grouped = case_when(
  grepl("adalimumab", three_and_two$Drug, ignore.case = TRUE) ~ "Adalimumab",
  grepl("alemtuzumab", three_and_two$Drug, ignore.case = TRUE) ~"Alemtuzumab",
  grepl("azathioprine", three_and_two$Drug, ignore.case = TRUE) ~ "Azathioprine",
  grepl("ciclosporin", three_and_two$Drug, ignore.case = TRUE) ~ "Ciclosporin",
  grepl("cyclophosphamide", three_and_two$Drug, ignore.case = TRUE) ~ "Cyclophosphamide",
  grepl("etanercept", three_and_two$Drug, ignore.case = TRUE) ~ "Etanercept",
  grepl("infliximab", three_and_two$Drug, ignore.case = TRUE) ~ "Infliximab",
  grepl("methotrexate", three_and_two$Drug, ignore.case = TRUE) ~ "Methotrexate",
  grepl("mycophenolate", three_and_two$Drug, ignore.case = TRUE) ~ "Mycophenolate",
  grepl("rituximab", three_and_two$Drug, ignore.case = TRUE) ~ "Rituximab",
  grepl("tacrolimus", three_and_two$Drug, ignore.case = TRUE) ~ "Tacrolimus"))
  
nr_of_Immunosup_PP <- tbl_df(sqldf("SELECT patid, COUNT(DISTINCT drug_grouped) AS nr_of_drugs FROM three_and_two GROUP BY patid ORDER BY nr_of_drugs DESC"))
table(nr_of_Immunosup_PP$nr_of_drugs)



