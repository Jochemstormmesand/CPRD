
# ############## 
# Jochem Stormmesand
# Research Project on Immunosuppressive Therapy and Parkinson Risk
#
# Package used as described by Springate et al. 2017

# ############## PACKAGE INSTALLATION AS DESCRIBED BY PAPER. DOESN'T WORK
# library(devtools)
# install_github("rOpenHealth/rEHR")
# library(rEHR)
# ##############
# to do to make it work: 
#before installing dplyr package, make sure rtools is installed https://cran.r-project.org/bin/windows/Rtools/ 
#in user library, make sure that: devtools, dplyr, gsubfn, proto, rEHR, RSQLite, sqldf are selected. 
#IF NOT WORKING: untick, rEHR, untick dplyr, then tick them. 

# ############## INSTALL THESE PACKAGES FOR PROPER FUNCTIONALITY
install.packages("digest")
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_0.5.0.tar.gz"  ##version0.5.0
#install.packages(packageurl, repos = NULL, type="source")
##install.packages("dplyr") ##version0.7.4
#packageurldevtools <- "https://cran.r-project.org/src/contrib/Archive/devtools/devtools_1.12.0.tar.gz"
#install.packages(packageurldevtools, repos = NULL, type="source")
library(devtools)
library(dplyr)
#install_github("rOpenHealth/rEHR")
library(rEHR)
# ##############



#TO DO with the script
# from therapy file get all immunosuppressant codes and see how many are other. 
# cumulative dose 
# substance.strength * amount * duration (start date - end date) 
#which variables to use though.
#how to convert all substance strengths to one unit. 
# calculate gaps
# demographics
#know these separately for PD, IS, and ALL:
# gender
# age (yob to when? what if transferred out)
# Info on IS and PD
# other diseases/comorbidities
# other medication/IS
# really PD?
# age of onset
# duration of disease
# follow up years: go for smallest follow up time initially. 
#Don't forget to ungroup after grouping...
# find whether PD patients are actually PD: find PD codes per patient


#STARTING THE SCRIPT
##2
#Required files
  # PD_immunosup_Extract_ "Common_Dosages","Prodcodes_Cut", "Referral_001", "Patient_001", "Clinical_001_Cut", "Clinical_002_Cut", "Clinical_003_Cut", "Clinical_004_Cut", "Therapy_001_Cut", "Therapy_002_Cut", "Therapy_003_Cut", "Therapy_004_Cut"
  # "MedicalReadcodes", "Drugproductreadcodes"
  # "prod_pd_treat"
## Use simulated ehr files supplied with the package to build database
ehr_path <- dirname(system.file("ehr_data", "PD_immunosup_Extract_Clinical_All_Dates_Cut.txt",  #MAKE SURE THAT TEXT FILES ARE STORED IN C:\Users\Stormezand\Documents\R\win-library\3.3\rEHR\ehr_data 
                                package = "rEHR"))
## create a new database connection to a temporary file
db <- database(tempfile(fileext = ".sqlite"))
## Import multiple data files into the database
import_CPRD_data(db, data_dir = ehr_path,         
                 filetypes = c("Common_Dosages","Prodcodes_Cut", "Referral_001", "Patient_001", "Clinical_001_Cut3", "Clinical_002_Cut3", "Clinical_003_Cut3", "Therapy_001_Cut2", "Therapy_002_Cut2", "Therapy_003_Cut2", "Therapy_004_Cut2", "MedicalReadcodes", "Drugproductreadcodes", "prod_pd_treat"),
                 dateformat = "%d/%m/%Y",
                 yob_origin = 1800,
                 regex = "PD_immunosup_Extract",
                 recursive = TRUE)

## Individual files can also be added:
#add_to_database(db, files = system.file("ehr_data", "prodcodes.txt",
#                                   package = "rEHR"),
#          table_name = c("Productcodelists"))

################ FILES THAT WORK AND ARE NEEDED
#Clinical and Therapy file combined via SQL as below.
#    type                 name             tbl_name
# 1 table       Common_Dosages       Common_Dosages
# 2 table        Prodcodes_Cut        Prodcodes_Cut
# 3 table         Referral_001         Referral_001
# 4 table          Patient_001          Patient_001
# 5 table         Clinical_All         Clinical_All
# 6 table          Therapy_All          Therapy_All
# 7 table     MedicalReadcodes     MedicalReadcodes
# 8 table Drugproductreadcodes Drugproductreadcodes
# 9 table                treat                treat
###############

####
# To avoid Memory exhaust:
# load extremely large text files into database: first load separated text files and merge them in SQL
Clinical_001 <- tbl_df(select_events(db,tab="Clinical_001_Cut3", columns = '*'))
Clinical_002 <- tbl_df(select_events(db,tab="Clinical_002_Cut3", columns = '*'))
Clinical_003 <- tbl_df(select_events(db,tab="Clinical_003_Cut3", columns = '*'))
dbRemoveTable(db, "Clinical_001_Cut3")
dbRemoveTable(db, "Clinical_002_Cut3")
dbRemoveTable(db, "Clinical_003_Cut3")
Clinical_All <- rbind(Clinical_001, Clinical_002, Clinical_003)
rm(Clinical_001, Clinical_002, Clinical_003)
dbWriteTable(db, "Clinical_All", Clinical_All, append = TRUE )
rm(Clinical_All)
Therapy_001 <- tbl_df(select_events(db,tab="Therapy_001_Cut2", columns = '*'))
Therapy_002 <- tbl_df(select_events(db,tab="Therapy_002_Cut2", columns = '*'))
Therapy_003 <- tbl_df(select_events(db,tab="Therapy_003_Cut2", columns = '*'))
Therapy_004 <- tbl_df(select_events(db,tab="Therapy_004_Cut2", columns = '*'))
dbRemoveTable(db, "Therapy_001_Cut2")
dbRemoveTable(db, "Therapy_002_Cut2")
dbRemoveTable(db, "Therapy_003_Cut2")
dbRemoveTable(db, "Therapy_004_Cut2")
Therapy_All <- rbind(Therapy_001, Therapy_002)
rm(Therapy_001, Therapy_002)
dbWriteTable(db, "Therapy_All", Therapy_All, append =TRUE)
Therapy_All2<- rbind(Therapy_003, Therapy_004)
rm(Therapy_003, Therapy_004)
dbWriteTable(db, "Therapy_All", Therapy_All2, append = TRUE)
rm(Therapy_All, Therapy_All2)
####