#eHR Statistics
install.packages("plotly")
library(plotly)

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
