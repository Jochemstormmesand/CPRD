install.packages("devtools")
library(devtools)
install_github("rOpenHealth/rEHR", lib = "C:\\Users\\Stormezand\\Documents\\Brain and Cognitive Sciences\\Research Internship 2\\Jochem data\\rEHR-master\\rEHR-master")
library(rEHR)
library(RSQLite)
#install.packages("dbplyr")
#library(dbplyr)

##2

## Use simulated ehr files supplied with the package to build database
ehr_path <- dirname(system.file("ehr_data", "ehr_Clinical.txt",  #MAKE SURE THAT TEXT FILES ARE STORED IN C:\Users\Stormezand\Documents\R\win-library\3.3\rEHR\ehr_data 
       package = "rEHR"))
## create a new database connection to a temporary file
db <- database(tempfile(fileext = ".sqlite"))
## Import multiple data files into the database
import_CPRD_data(db, data_dir = ehr_path,
     filetypes = c("Clinical001", "Consultation001",
                   "Patient001", "Practice001", 
                   "Referral001", "Therapy001", "Test001", "Staff001"),
     dateformat = "%Y - %m - %d",
     yob_origin = 1800,
     regex = "Sample",
     recursive = TRUE)
# ## Individualfiles can also be added:
# add_to_database(db, files = system.file("ehr_data", "Sample_Therapy001.txt",
#           package = "rEHR"),
#               table_name = "Therapy", dateformat = "%Y - %m - %d")

## Use the overloaded`head` function to view a list of
## tables or the head of individualtables:
head(db)





##3.1
cancer_codes <- clinical_codes[clinical_codes$list=="Cancer",]
cancer_codes1 <- cancer_codes$medcode
diabetes_codes <- clinical_codes[clinical_codes$list=="Diabetes",]
select_events(db,tab="Clinical001", columns = c("patid", "eventdate", "medcode"),
              where = "medcode %in% .(diabetes_codes$medcode) & eventdate<'2006-01-01'&
               eventdate>='2005-01-01'")
medlist = 180:200
sqldf("SELECT patid, eventdate, medcode from Clinical001 WHERE .(medcode) = medlist", connection=db)

wrap_sql_query("SELECT patid, eventdate, medcode from Clinical001 WHERE medcode in #1",cancer_codes1)
expand_string("SELECT patid, eventdate, medcode from Clinical001 WHERE medcode in .(cancer_codes1)")



b <- select_events(db, tab = "Referral001", columns = c("patid", "eventdate", "medcode"),
                   where = "medcode %in% .(cancer_codes$medcode) & eventdate < '2000-01-01'")



# Asthma_codes <- clinical_codes[clinical_codes$list=="Asthma",]
# q <- select_events(db,tab="Clinical",columns=c("patid","eventdate","medcode"),
#                    where="medcode %in% .(Asthma_codes$medcode)",
#                    sql_only=TRUE)
# temp_table(db,tab_name="Asthma",select_query =q)
# head(db,temp=TRUE)
# head(db,table="Asthma")

sqldf("SELECT patid, practid, gender, yob, deathdate from Patient001 WHERE gender IS 2",
      connection=db)

medcodes1<- 1:5
practice<- 255
expand_string("SELECT * FROM clinical001 WHERE practid == .(practice)")

#wrap_sql_query("SELECT * FROM Clinical001 WHERE practid == # 1 AND medcode in # 2",practice,medcodes1)

##3.2

# first_DM<- first_events(db,tab="Clinical", columns=c("patid", "eventdate", "medcode"),
#                          where="medcode %in% .(diabetes_codes$medcode)")
first_DM <- 
# last_DM<- last_events(db,tab="Clinical", columns=c("patid", "eventdate", "medcode"),
#                        where="medcode %in% .(diabetes_codes$medcode)") 
# head(first_DM)
# head(last_DM)

##3.3.1
# registered_patients<- select_by_year(db=db, tables="patient",
#                                         columns=c("patid", "practid", "gender", "yob", "crd", "tod","deathdate"),
#                                         where="crd < STARTDATE", year_range=c(2008 : 2012), year_fn =standard_years)
 
# str(registered_patients)

# table(registered_patients$year)

# incident_cases<- select_by_year(db=db, tables=c("Clinical", "Referral"),
#                                   columns=c("patid", "eventdate", "medcode"),
#                                   where="medcode %in% .(diabetes_codes$medcode) & eventdate <= ENDDATE",
#                                   year_range=c(2008 : 2012), year_fn=standard_years, selector_fn =first_events)
# str(incident_cases)
# 
# ## All patientsare kept (equivalentto merge(all.x= TRUE)) 
# prevalence_dat<- left_join(registered_patients, incident_cases) 
# ## Removeduplicatesacrossclinicaland referraltables: 
# incident_cases%>% group_by(patid,year) %>% arrange(eventdate)%>% distinct() %>% ungroup-> incident_cases

# prevalence_dat<- prev_terms(prevalence_dat) 
# totals<- prev_totals(prevalence_dat) 
# totals$prevalence$year_counts
# totals$incidence$year_counts

## 4.0

# practices<- select_events(db=db,tab="Practice", convert_dates=TRUE) 
# prevalence_dat<- left_join(prevalence_dat,practices)
# cohort<- build_cohort(prevalence_dat,cohort_type="prev", 
#                       cohort_start="2006 - 01 - 01", cohort_end="2012 - 12 - 31" 
#                       , diagnosis_start="eventdate")

## Add a logical column for death during cohort 
# cohort$death<- with(cohort, 
#                     ifelse(!is.null(deathdate)& 
#                              (deathdate> as.Date("2006 - 01 - 01") & 
#                                 deathdate< as.Date("2012 - 12 - 31")),
#                              1, 0)) 
# cohort$death[is.na(cohort$death)]<- 0
# 
# library(survival) 
# surv_obj<- with(cohort,Surv(start,end, death))
# coxph(surv_obj~gender+ case,data=cohort)
# 
# 
# ##4.1.
# cohort2<- build_cohort(prevalence_dat,cohort_type="incid", 
#                         cohort_start="2006 - 01 - 01", 
#                         cohort_end="2012 - 12 - 31", 
#                         diagnosis_start = "eventdate")
# IDM_controls<- get_matches(cases=filter(cohort2, case == 1),
#                            control_pool=filter(cohort2,case == 0),
#                            match_vars=c("gender", "region"),   ##extra_conditions for more complex matching strategies
#                            n_controls=4, cores =1, 
#                            method="incidence_density", 
#                            diagnosis_date="eventdate")
# IDM_controls2<- get_matches(cases=filter(cohort2, case == 1),
#                             control_pool=filter(cohort2,case == 0),
#                             match_vars=c("gender", "region"),
#                             extra_conditions = "yob>=(.(CASE&yob)-2)
#                             &yob<=(.(CASE$yob)+2)",
#                             n_controls=4, cores =1, 
#                             method="incidence_density", 
#                             diagnosis_date="eventdate")
# exact_controls3 <- get_matches(cases=filter(cohort2, case == 1),
#                                control_pool=filter(cohort2,case == 0),
#                                match_vars=c("gender", "region"),
#                                n_controls=4, cores =1, 
#                                method="exact",
#                                diagnosis_date="eventdate")
# 
# consultation_dir<- "~/R/rEHR_testing" 
# flat_files(db,out_dir=consultation_dir,file_type="csv") 
# index_controls<- match_on_index(cases=filter(cohort2,case == 1),
#                                 control_pool=filter(cohort2, case == 0), 
#                                 index_var="eventdate", 
#                                 match_vars=c("gender","region"), 
#                                 index_diff_limit=90,
#                                 consult_path=consultation_dir,
#                                 n_controls=4, 
#                                 import_fn=function(x) 
#                                   convert_dates(read.csv(x)))
# # clean up constructed dirs after analysis 
# unlink(consultation_dir,recursive=TRUE)
# 
# 
# tv_test<- data.frame(id=1 : 5, start=rep(0,5), 
#                      end=c(1000, 689, 1000, 874,777), 
#                      event=c(0, 1, 0, 1, 1), 
#                      drug_1=c(NA,NA, NA, 340, 460), 
#                      drug_2=c(NA,234, 554,123, NA), 
#                      drug_3_start=c(110, 110,111, 109,110), 
#                      drug_3_stop=c(400,400, 400,400, 400), 
#                      stage_1=c(300,NA, NA, NA, NA), 
#                      stage_2=c(450,NA, NA, NA, NA))
# ## Multiplebinarychroniccovariates: 
# tv_out1<- cut_tv(tv_test,
#                   entry=start,
#                   exit = end,
#                   cut_var=drug_1,
#                   id_var=id, 
#                   tv_name=drug_1_state)
# tv_out1<- cut_tv(tv_out1,start,end,drug_2,id_var=id, drug_2_state) 
# head(tv_out1)
# 
# 
# ## Binarycovariates: 
# tv_out3<- cut_tv(tv_test,start,end, drug_3_start,id_var= id, drug_3_state)
# tv_out3<- cut_tv(tv_out3,start,end, drug_3_stop,id_var= id, drug_3_state)
# head(tv_out3) 
#   
#   ## incrementalcovariates: 
# inc_1<- cut_tv(tv_test,start,end, stage_1,id_var=id, disease_stage, on_existing ="inc")
#   inc_1 <- cut_tv(inc_1,start,end, stage_2,id_var=id, disease_stage, on_existing ="inc")
# head(inc_1)
# 
# ## Chainingcombinationsof the above using %>% library(dplyr) tv_test%>% cut_ tv(start,end, drug_1,id_ var=id, drug_1_state)%>% cut_ tv(start,end, drug_2,id_ var=id, drug_2_state)%>% cut_ tv(start,end, drug_3_start,id_ var=id, drug_3_state)%>% cut_ tv(start,end, drug_3_stop,id_ var=id, drug_3_state)%>% cut_ tv(start,end, stage_1,id_ var=id, disease_stage, on_ existing="inc") %>%
# cut_tv(start,end, stage_2,id_var=id, disease_stage, on_existing="inc") %>%head
# 
# ## Exampleconstructionof a clinicalcode list 
# def <- MedicalDefinition(
# terms=list( "peripheral vascular disease", "peripheral gangrene", " -wrong answer", "intermittent claudication", "thromboangiitis obliterans","thromboangiitis obliterans", "diabetic peripheral angiopathy",
#             c("diabetes", "peripheral angiopathy"),# single AND expression
#             c("buerger", "disease presenile_ gangrene"), " - excepted", # exclusion
#             codes=list("G73"), tests=NULL, drugs=list("insulin", "diabet","aspirin")))
#                             
# ## Use fileEncoding= "latin1"to avoid issueswith non-asciicharacters 
# medical_table<- read.delim("Lookups=medical:txt", fileEncoding="latin1", stringsAsFactors=FALSE)
# drug_table<- read.delim("Lookups=product:txt", fileEncoding="latin1", stringsAsFactors=FALSE)
# draft_lists<- build_definition_lists(def,medical_table = medical_table, drug_table=drug_table)