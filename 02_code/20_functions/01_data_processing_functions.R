############ Data processing functions ############

#1.Function to summarize a year's data for GHE causes
cause_process  <- function(x) {
  
  print(paste0('year ',x,' now being processed'))
  
  # load year of deaths
  raw_mortalty_data <- '~/data/mortality/US/state/processed/cod/'
  dat.name <- paste0(raw_mortalty_data,"deathscod",x,".dta")
  dat <- read.dta(dat.name)
  
  # fix sex classification if in certain years
  if(x %in% c(2003:2010,2012)){
    dat$sex = as.integer(plyr::mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c(2,1)))
  }
  
  # add extra label for CODs based on relevant ICD year
  start_year = 1999
  if(x<start_year) {
    # TO DO (not done to date as focusing on ICD-10 as of end of 2021)
  }
  
  if(x>=start_year){
    # merge cod in ICD 10 coding for broad letter coding
    dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
    dat$letter = substr(dat$cause,1,1)
    
    causes = dat
    
    # numerical cause
    causes$number = as.numeric(as.character(substr(causes$cause,2,4)))
    
    causes <- causes %>%
      mutate(
        ghe_level2 = case_when(
          # Ill-defined
          letter == "R" & number >= 0   & number <= 949 ~ "Ill-defined diseases",
          letter == "R" & number >= 960 & number <= 999 ~ "Ill-defined diseases",
          letter == "Y" & number >= 100 & number <= 349 ~ "Ill-defined injuries/accidents",
          letter == "Y" & number == 872                 ~ "Ill-defined injuries/accidents",
          
          letter == "A" & number >= 0   & number <= 999 ~ "Infectious and parasitic diseases",
          letter == "B" & number >= 0   & number <= 999 ~ "Infectious and parasitic diseases",
          letter == "G" & number >= 0   & number <= 9   ~ "Infectious and parasitic diseases", #G05 included in here?
          letter == "G" & number >= 30  & number <= 49  ~ "Infectious and parasitic diseases",
          letter == "G" & number >= 140 & number <= 149 ~ "Infectious and parasitic diseases",
          letter == "N" & number >= 700 & number <= 739 ~ "Infectious and parasitic diseases", #INCLUDE OR EXCLUDE PID
          
          letter == "J" & number >= 0   & number <= 69  ~ "Respiratory infections",
          letter == "J" & number >= 90  & number <= 189 ~ "Respiratory infections",
          letter == "J" & number >= 200 & number <= 229 ~ "Respiratory infections",
          letter == "H" & number >= 650 & number <= 669 ~ "Respiratory infections", #INCLUDE OR EXCLUDE H67 OTITIS MEDIA
          
          letter == "O" & number >= 0   & number <= 999 ~ "Maternal conditions",
          
          letter == "P" & number >= 0   & number <= 969 ~ "Perinatal conditions",
          
          letter == "E" & number >= 0   & number <= 29  ~ "Nutritional deficiencies",
          letter == "E" & number >= 400 & number <= 469 ~ "Nutritional deficiencies",
          letter == "E" & number >= 500 & number <= 509 ~ "Nutritional deficiencies",
          letter == "D" & number >= 500 & number <= 539 ~ "Nutritional deficiencies",
          letter == "D" & number == 649                 ~ "Nutritional deficiencies",
          letter == "E" & number >= 510 & number <= 649 ~ "Nutritional deficiencies",
          
          
          letter == "C" & number >= 0   & number <= 979 ~ "Malignant neoplasms",
          
          letter == "D" & number >= 0   & number <= 489 ~ "Other neoplasms",
          
          letter == "E" & number >= 100 & number <= 149 ~ "Diabetes mellitus",
          
          letter == "D" & number >= 550 & number <= 648 ~ "Endocrine disorders",
          letter == "D" & number >= 650 & number <= 899 ~ "Endocrine disorders",
          letter == "E" & number >= 30  & number <= 79  ~ "Endocrine disorders",
          letter == "E" & number >= 150 & number <= 169 ~ "Endocrine disorders",
          letter == "E" & number >= 200 & number <= 349 ~ "Endocrine disorders",
          letter == "E" & number >= 650 & number <= 889 ~ "Endocrine disorders",
          
          letter == "F" & number >= 10  & number <= 999 ~ "Neuropsychiatric conditions",
          letter == "G" & number >= 60  & number <= 139 ~ "Neuropsychiatric conditions",
          letter == "G" & number >= 150 & number <= 989 ~ "Neuropsychiatric conditions",
          letter == "X" & number >= 410 & number <= 429 ~ "Neuropsychiatric conditions",
          letter == "X" & number >= 450 & number <= 459 ~ "Neuropsychiatric conditions",
          
          letter == "H" & number >= 0   & number <= 619 ~ "Sense organ diseases",
          letter == "H" & number >= 680 & number <= 939 ~ "Sense organ diseases",
          
          letter == "I" & number >= 0   & number <= 999 ~ "Cardiovascular diseases",
          
          letter == "J" & number >= 300 & number <= 989 ~ "Respiratory diseases",
          
          letter == "K" & number >= 200 & number <= 929 ~ "Digestive diseases",
          
          letter == "N" & number >= 0   & number <= 649 ~ "Genitourinary diseases",
          letter == "N" & number >= 750 & number <= 989 ~ "Genitourinary diseases",
          
          letter == "L" & number >= 0   & number <= 989 ~ "Skin diseases",
          
          letter == "M" & number >= 0   & number <= 999 ~ "Musculoskeletal diseases",
          
          letter == "Q" & number >= 0   & number <= 999 ~ "Congenital anomalies",
          
          letter == "K" & number >= 0   & number <= 149 ~ "Oral conditions",
          
          letter == "R" & number >= 950 & number <= 959 ~ "Sudden infant death syndrome",
          
          
          letter == "V" & number >= 0   & number <= 999 ~ "Unintentional injuries",
          letter == "W" & number >= 0   & number <= 999 ~ "Unintentional injuries",
          letter == "X" & number >= 0   & number <= 409 ~ "Unintentional injuries",
          letter == "X" & number >= 430 & number <= 449 ~ "Unintentional injuries", #X430 doesn't exist?
          letter == "X" & number >= 460 & number <= 599 ~ "Unintentional injuries",
          letter == "Y" & number >= 400 & number <= 869 ~ "Unintentional injuries",
          letter == "Y" & number >= 880 & number <= 899 ~ "Unintentional injuries",
          
          letter == "X" & number >= 600 & number <= 999 ~ "Intentional injuries",
          letter == "Y" & number >= 0   & number <= 99  ~ "Intentional injuries",
          letter == "Y" & number >= 350 & number <= 369 ~ "Intentional injuries",
          letter == "Y" & number == 870                 ~ "Intentional injuries",
          letter == "Y" & number == 871                 ~ "Intentional injuries",
          letter == "U" & number >= 0   & number <= 19  ~ "Intentional injuries",
          
          
          TRUE ~ NA_character_
        )
      )
    
    causes <- causes %>%
      mutate(
        ghe_level1 = case_when(
          ghe_level2 == "Infectious and parasitic diseases" ~ "Communicable, maternal, perinatal and nutritional conditions",
          ghe_level2 == "Respiratory infections" ~ "Communicable, maternal, perinatal and nutritional conditions",
          ghe_level2 == "Maternal conditions" ~ "Communicable, maternal, perinatal and nutritional conditions",
          ghe_level2 == "Perinatal conditions" ~ "Communicable, maternal, perinatal and nutritional conditions",
          ghe_level2 == "Nutritional deficiencies" ~ "Communicable, maternal, perinatal and nutritional conditions",
          
          ghe_level2 == "Malignant neoplasms" ~ "Noncommunicable diseases",
          ghe_level2 == "Other neoplasms" ~ "Noncommunicable diseases",
          ghe_level2 == "Diabetes mellitus" ~ "Noncommunicable diseases",
          ghe_level2 == "Endocrine disorders" ~ "Noncommunicable diseases",
          ghe_level2 == "Neuropsychiatric conditions" ~ "Noncommunicable diseases",
          ghe_level2 == "Sense organ diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Cardiovascular diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Respiratory diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Digestive diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Genitourinary diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Skin diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Musculoskeletal diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Congenital anomalies" ~ "Noncommunicable diseases",
          ghe_level2 == "Oral conditions" ~ "Noncommunicable diseases",
          ghe_level2 == "Sudden infant death syndrome" ~ "Noncommunicable diseases",
          ghe_level2 == "Ill-defined diseases" ~ "Noncommunicable diseases",
          
          ghe_level2 == "Unintentional injuries" ~ "Injuries",
          ghe_level2 == "Intentional injuries" ~ "Injuries",
          ghe_level2 == "Ill-defined injuries/accidents" ~ "Injuries",
          
          TRUE ~ NA_character_
        )
      )
    
    causes <- causes %>%
      mutate(
        ghe_level3 = case_when(
          # Ill-defined and garbage
          letter == "R" & number >= 0   & number <= 949 ~ "Ill-defined diseases",
          letter == "R" & number >= 960 & number <= 999 ~ "Ill-defined diseases",
          letter == "Y" & number >= 100 & number <= 349 ~ "Ill-defined injuries/accidents",
          letter == "Y" & number == 872                 ~ "Ill-defined injuries/accidents",
          letter == "C" & number >= 760 & number <= 769 ~ "Garbage cancer",
          letter == "C" & number >= 800 & number <= 809 ~ "Garbage cancer",
          letter == "C" & number >= 970 & number <= 979 ~ "Garbage cancer",
          letter == "I" & number == 472                 ~ "Garbage CVD",
          letter == "I" & number == 490                 ~ "Garbage CVD",
          letter == "I" & number >= 460 & number <= 469 ~ "Cardiac arrest",
          letter == "I" & number >= 500 & number <= 509 ~ "Heart failure",
          letter == "I" & number == 514                 ~ "Inflammatory heart diseases",
          letter == "I" & number == 515                 ~ "Heart failure",
          letter == "I" & number == 516                 ~ "Garbage CVD",
          letter == "I" & number == 519                 ~ "Garbage CVD",
          letter == "I" & number == 709                 ~ "Garbage CVD",
          
          
          letter == "A" & number >= 150 & number <= 199 ~ "Tuberculosis",
          letter == "B" & number >= 900 & number <= 909 ~ "Tuberculosis",
          letter == "A" & number >= 500 & number <= 649 ~ "STDs excluding HIV",
          letter == "N" & number >= 700 & number <= 739 ~ "STDs excluding HIV",
          letter == "B" & number >= 200 & number <= 249 ~ "HIV/AIDS",
          letter == "A" & number >= 0   & number <= 19  ~ "Diarrhoeal diseases",
          letter == "A" & number >= 30  & number <= 49  ~ "Diarrhoeal diseases",
          letter == "A" & number >= 60  & number <= 99  ~ "Diarrhoeal diseases",
          letter == "A" & number >= 330 & number <= 379 ~ "Childhood-cluster diseases",
          letter == "A" & number >= 800 & number <= 809 ~ "Childhood-cluster diseases",
          letter == "B" & number >= 50  & number <= 59  ~ "Childhood-cluster diseases",
          letter == "B" & number >= 910 & number <= 919 ~ "Childhood-cluster diseases",
          letter == "G" & number >= 140 & number <= 149 ~ "Childhood-cluster diseases",
          letter == "A" & number >= 390 & number <= 399 ~ "Meningitis",
          letter == "G" & number >= 0   & number <= 9   ~ "Meningitis",
          letter == "G" & number >= 30  & number <= 39  ~ "Meningitis",
          letter == "B" & number >= 160 & number <= 170 ~ "Hepatitis B",
          letter == "B" & number >= 172 & number <= 181 ~ "Hepatitis B",
          letter == "B" & number >= 183 & number <= 199 ~ "Hepatitis B",
          letter == "B" & number == 171                 ~ "Hepatitis C",
          letter == "B" & number == 182                 ~ "Hepatitis C",
          letter == "B" & number >= 500 & number <= 549 ~ "Malaria",
          letter == "B" & number >= 550 & number <= 579 ~ "Tropical-cluster diseases",
          letter == "B" & number >= 650 & number <= 659 ~ "Tropical-cluster diseases",
          letter == "B" & number >= 730 & number <= 742 ~ "Tropical-cluster diseases",
          letter == "A" & number >= 300 & number <= 309 ~ "Leprosy",
          letter == "A" & number >= 900 & number <= 919 ~ "Dengue",
          letter == "A" & number == 380                 ~ "Japanese encephalitis",
          letter == "A" & number >= 710 & number <= 719 ~ "Trachoma",
          letter == "B" & number >= 760 & number <= 819 ~ "Intestinal nematode infections",
          ghe_level2 == "Infectious and parasitic diseases" ~ "Other infectious diseases",
          
          letter == "J" & number >= 90  & number <= 189 ~ "Lower respiratory infections",
          letter == "J" & number >= 200 & number <= 229 ~ "Lower respiratory infections",
          letter == "J" & number >= 0   & number <= 69  ~ "Upper respiratory infections",
          letter == "H" & number >= 650 & number <= 669 ~ "Otitis media",
          
          letter == "O" & number >= 440 & number <= 469 ~ "Maternal haemorrhage",
          letter == "O" & number >= 670 & number <= 679 ~ "Maternal haemorrhage",
          letter == "O" & number >= 720 & number <= 729 ~ "Maternal haemorrhage",
          letter == "O" & number >= 850 & number <= 869 ~ "Maternal sepsis",
          letter == "O" & number >= 100 & number <= 169 ~ "Hypertensive disorders",
          letter == "O" & number >= 640 & number <= 669 ~ "Obstructed labour",
          letter == "O" & number >= 0   & number <= 79  ~ "Abortion",
          ghe_level2 == "Maternal conditions" ~ "Other maternal conditions",
          
          letter == "P" & number >= 50  & number <= 59  ~ "Low birth weight",
          letter == "P" & number >= 70  & number <= 79  ~ "Low birth weight",
          letter == "P" & number >= 220 & number <= 229 ~ "Low birth weight",
          letter == "P" & number >= 270 & number <= 289 ~ "Low birth weight",
          letter == "P" & number >= 30  & number <= 39  ~ "Birth asphyxia and birth trauma",
          letter == "P" & number >= 100 & number <= 159 ~ "Birth asphyxia and birth trauma",
          letter == "P" & number >= 200 & number <= 219 ~ "Birth asphyxia and birth trauma",
          letter == "P" & number >= 240 & number <= 269 ~ "Birth asphyxia and birth trauma",
          letter == "P" & number >= 290 & number <= 299 ~ "Birth asphyxia and birth trauma",
          ghe_level2 == "Perinatal conditions" ~ "Other perinatal conditions",
          
          letter == "E" & number >= 400 & number <= 469 ~ "Protein-energy malnutrition",
          letter == "E" & number >= 0   & number <= 29  ~ "Iodine deficiency",
          letter == "E" & number >= 500 & number <= 509 ~ "Vitamin A deficiency",
          letter == "D" & number >= 500 & number <= 509 ~ "Iron-deficiency anaemia",
          letter == "D" & number == 649                 ~ "Iron-deficiency anaemia",
          ghe_level2 == "Nutritional deficiencies" ~ "Other nutritional conditions",
          
          
          letter == "C" & number >= 0   & number <= 149 ~ "Mouth and oropharynx cancers",
          letter == "C" & number >= 150 & number <= 159 ~ "Oesophagus cancer",
          letter == "C" & number >= 160 & number <= 169 ~ "Stomach cancer",
          letter == "C" & number >= 180 & number <= 219 ~ "Colon and rectum cancers",
          letter == "C" & number >= 220 & number <= 229 ~ "Liver cancer",
          letter == "C" & number >= 250 & number <= 259 ~ "Pancreas cancer",
          letter == "C" & number >= 330 & number <= 349 ~ "Trachea, bronchus, lung cancers",
          letter == "C" & number >= 430 & number <= 449 ~ "Melanoma and other skin cancers",
          letter == "C" & number >= 500 & number <= 509 ~ "Breast cancer",
          letter == "C" & number >= 530 & number <= 539 ~ "Cervix uteri cancer",
          letter == "C" & number >= 540 & number <= 559 ~ "Corpus uteri cancer",
          letter == "C" & number >= 560 & number <= 569 ~ "Ovary cancer",
          letter == "C" & number >= 610 & number <= 619 ~ "Prostate cancer",
          letter == "C" & number >= 620 & number <= 629 ~ "Testicular cancer",
          letter == "C" & number >= 640 & number <= 669 ~ "Kidney and ureter cancer",
          letter == "C" & number >= 670 & number <= 679 ~ "Bladder cancer",
          letter == "C" & number >= 700 & number <= 729 ~ "Brain and nervous system cancers",
          letter == "C" & number >= 230 & number <= 249 ~ "Gallbladder and biliary tract cancer",
          letter == "C" & number >= 320 & number <= 329 ~ "Larynx cancer",
          letter == "C" & number >= 730 & number <= 739 ~ "Thyroid cancer",
          letter == "C" & number >= 450 & number <= 459 ~ "Mesothelioma",
          letter == "C" & number >= 810 & number <= 909 ~ "Lymphomas, multiple myeloma",
          letter == "C" & number >= 960 & number <= 969 ~ "Lymphomas, multiple myeloma",
          letter == "C" & number >= 910 & number <= 959 ~ "Leukaemia",
          ghe_level2 == "Malignant neoplasms" ~ "Other malignant neoplasms",
          
          ghe_level2 == "Other neoplasms" ~ "Other neoplasms",
          
          ghe_level2 == "Diabetes mellitus" ~ "Diabetes mellitus",
          
          ghe_level2 == "Endocrine disorders" ~ "Endocrine disorders",
          
          letter == "F" & number >= 320 & number <= 339 ~ "Unipolar depressive disorders",
          letter == "F" & number >= 300 & number <= 319 ~ "Bipolar disorder",
          letter == "F" & number >= 200 & number <= 299 ~ "Schizophrenia",
          letter == "G" & number >= 400 & number <= 419 ~ "Epilepsy",
          letter == "F" & number >= 100 & number <= 109 ~ "Alcohol use disorders",
          letter == "X" & number >= 450 & number <= 459 ~ "Alcohol use disorders",
          letter == "F" & number >= 10  & number <= 19  ~ "Alzheimer and other dementias",
          letter == "F" & number >= 30  & number <= 39  ~ "Alzheimer and other dementias",
          letter == "G" & number >= 300 & number <= 319 ~ "Alzheimer and other dementias",
          letter == "G" & number >= 200 & number <= 219 ~ "Parkinson disease",
          letter == "G" & number >= 350 & number <= 359 ~ "Multiple sclerosis",
          letter == "F" & number >= 110 & number <= 169 ~ "Drug use disorders",
          letter == "F" & number >= 180 & number <= 199 ~ "Drug use disorders",
          letter == "X" & number >= 410 & number <= 429 ~ "Drug use disorders",                   #SHOULDN'T THIS BE F01 ETC.
          letter == "F" & number == 431                 ~ "Post-traumatic stress disorder",
          letter == "F" & number >= 420 & number <= 429 ~ "Obsessive-compulsive disorder",
          letter == "F" & (number == 400 | number == 410) ~ "Panic disorder",
          letter == "F" & number >= 510 & number <= 519 ~ "Insomnia (primary)",
          letter == "G" & number >= 430 & number <= 439 ~ "Migraine",
          letter == "F" & number >= 700 & number <= 799 ~ "Mental Retardation",
          ghe_level2 == "Neuropsychiatric conditions" ~ "Other neuropsychiatric disorders",
          
          letter == "H" & number >= 400 & number <= 409 ~ "Glaucoma",
          letter == "H" & number >= 250 & number <= 269 ~ "Cataracts",
          letter == "H" & number == 524                 ~ "Vision disorders, age-related",
          letter == "H" & number >= 900 & number <= 919 ~ "Hearing loss, adult onset",
          ghe_level2 == "Sense organ diseases" ~ "Other sense organ disorders",
          
          letter == "I" & number >= 10  & number <= 99  ~ "Rheumatic heart disease",
          letter == "I" & number >= 100 & number <= 139 ~ "Hypertensive heart disease",
          letter == "I" & number >= 200 & number <= 259 ~ "Ischaemic heart disease",
          letter == "I" & number >= 600 & number <= 699 ~ "Cerebrovascular disease",
          letter == "I" & number >= 300 & number <= 339 ~ "Inflammatory heart diseases",
          letter == "I" & number >= 380 & number <= 389 ~ "Inflammatory heart diseases",
          letter == "I" & number >= 400 & number <= 409 ~ "Inflammatory heart diseases",
          letter == "I" & number >= 420 & number <= 429 ~ "Inflammatory heart diseases",
          ghe_level2 == "Cardiovascular diseases" ~ "Other cardiovascular diseases",
          
          letter == "J" & number >= 400 & number <= 449 ~ "Chronic obstructive pulmonary disease",
          letter == "J" & number >= 450 & number <= 469 ~ "Asthma",
          ghe_level2 == "Respiratory diseases" ~ "Other respiratory diseases",
          
          letter == "K" & number >= 250 & number <= 279 ~ "Peptic ulcer disease",
          letter == "K" & number >= 700 & number <= 709 ~ "Cirrhosis of the liver",
          letter == "K" & number >= 740 & number <= 749 ~ "Cirrhosis of the liver",
          letter == "K" & number >= 350 & number <= 379 ~ "Appendicitis",
          letter == "K" & number >= 290 & number <= 299 ~ "Gastritis and duodenitis",
          letter == "K" & number >= 560 & number <= 569 ~ "Paralytic ileus and intestinal obstruction",
          letter == "K" & number >= 500 & number <= 529 ~ "Inflammatory bowel disease",
          letter == "K" & number == 580                 ~ "Inflammatory bowel disease",
          letter == "K" & number >= 800 & number <= 839 ~ "Gallbladder and biliary diseases",
          letter == "K" & number >= 850 & number <= 869 ~ "Pancreatitis",
          ghe_level2 == "Digestive diseases" ~ "Other digestive diseases",
          
          letter == "N" & number >= 0   & number <= 199 ~ "Nephritis and nephrosis",
          letter == "N" & number >= 400 & number <= 409 ~ "Benign prostatic hypertrophy",
          ghe_level2 == "Genitourinary diseases" ~ "Other genitourinary system diseases",
          
          ghe_level2 == "Skin diseases" ~ "Skin diseases",
          
          letter == "M" & number >= 50  & number <= 69  ~ "Rheumatoid arthritis",
          letter == "M" & number >= 150 & number <= 199 ~ "Osteoarthritis",
          letter == "M" & number >= 100 & number <= 109 ~ "Gout",
          letter == "M" & number >= 450 & number <= 489 ~ "Back pain",
          letter == "M" & number >= 540 & number <= 541 ~ "Back pain",
          letter == "M" & number >= 543 & number <= 549 ~ "Back pain",
          ghe_level2 == "Musculoskeletal diseases" ~ "Other musculoskeletal diseases",
          
          letter == "Q" & number >= 792 & number <= 795 ~ "Abdominal wall defect",
          letter == "Q" & number >= 0   & number <= 9   ~ "Anencephaly",
          letter == "Q" & number >= 420 & number <= 429 ~ "Anorectal atresia",
          letter == "Q" & number >= 360 & number <= 369 ~ "Cleft lip",
          letter == "Q" & number >= 350 & number <= 359 ~ "Cleft palate",
          letter == "Q" & number >= 370 & number <= 379 ~ "Cleft palate",
          letter == "Q" & number >= 390 & number <= 391 ~ "Oesophageal atresia",
          letter == "Q" & number >= 600 & number <= 609 ~ "Renal agenesis",
          letter == "Q" & number >= 900 & number <= 909 ~ "Down syndrome",
          letter == "Q" & number >= 200 & number <= 289 ~ "Congenital heart anomalies",
          letter == "Q" & number >= 50  & number <= 59  ~ "Spina bifida",
          ghe_level2 == "Congenital anomalies" ~ "Other congenital anomalies",
          
          letter == "K" & number >= 20  & number <= 29  ~ "Dental caries",
          letter == "K" & number >= 50  & number <= 59  ~ "Periodontal disease",
          ghe_level2 == "Oral conditions" ~ "Other oral diseases",
          
          ghe_level2 == "Sudden infant death syndrome" ~ "Sudden infant death syndrome",
          
          
          letter == "V" & number >= 0   & number <= 999 ~ "Road traffic accidents",
          letter == "Y" & number == 850                 ~ "Road traffic accidents",
          letter == "X" & number >= 400 & number <= 409 ~ "Poisonings",
          letter == "X" & number >= 430 & number <= 449 ~ "Poisonings",
          letter == "X" & number >= 460 & number <= 499 ~ "Poisonings",
          letter == "W" & number >= 0   & number <= 199 ~ "Falls",
          letter == "X" & number >= 0   & number <= 99  ~ "Fires",
          letter == "W" & number >= 650 & number <= 749 ~ "Drownings",
          letter == "W" & number >= 200 & number <= 389 ~ "Exposure to mechanical forces",
          letter == "W" & number >= 400 & number <= 439 ~ "Exposure to mechanical forces",
          letter == "W" & number >= 450 & number <= 469 ~ "Exposure to mechanical forces",
          letter == "W" & number >= 490 & number <= 529 ~ "Exposure to mechanical forces",
          letter == "W" & number >= 750 & number <= 769 ~ "Exposure to mechanical forces",
          letter == "X" & number >= 300 & number <= 399 ~ "Exposure to forces of nature",
          ghe_level2 == "Unintentional injuries" ~ "Other unintentional injuries",
          
          letter == "X" & number >= 600 & number <= 849 ~ "Self-inflicted injuries",
          letter == "Y" & number == 870                 ~ "Self-inflicted injuries",
          letter == "X" & number >= 850 & number <= 999 ~ "Violence",
          letter == "Y" & number >= 0   & number <= 99  ~ "Violence",
          letter == "Y" & number == 871                 ~ "Violence",
          letter == "U" & number >= 0   & number <= 19  ~ "Violence",
          letter == "Y" & number >= 360 & number <= 369 ~ "War",
          ghe_level2 == "Intentional injuries" ~ "Other intentional injuries",
          
          
          TRUE ~ NA_character_
        )
      )
    
  causes <- causes %>%
      mutate(
        group = case_when(
          
          #Cancers
          letter == "C" & number >= 0 & number <= 999 ~ "Cancers",
          letter == "D" & number >= 0 & number <= 499 ~ "Cancers",
          
          #Cardiovascular diseases
          letter == "I" & number >= 0 & number <= 999 ~ "Cardiovascular diseases",
          
          #Infectious and parasitic diseases 
          letter == "A" & number >= 0   & number <= 999 ~ "Infectious and parasitic diseases",
          letter == "B" & number >= 0   & number <= 999 ~ "Infectious and parasitic diseases",
          letter == "G" & number >= 0  & number <= 059 ~ "Infectious and parasitic diseases",  
          letter == "G" & number >= 140 & number <= 149 ~ "Infectious and parasitic diseases",
          letter == "N" & number >= 700 & number <= 749 ~ "Infectious and parasitic diseases", 
          
          #Respiratory diseases 
          letter == "J" & number >= 0   & number <= 999  ~ "Respiratory diseases",
          letter == "H" & number >= 620 & number <= 679 ~ "Respiratory diseases", 
          
          #Neuropsychiatric conditions
          letter == "F" & number >= 0  & number <= 999 ~ "Neuropsychiatric conditions",
          letter == "G" & number >= 60  & number <= 139 ~ "Neuropsychiatric conditions",
          letter == "G" & number >= 150 & number <= 989 ~ "Neuropsychiatric conditions",
          letter == "X" & number >= 410 & number <= 429 ~ "Neuropsychiatric conditions", 
          letter == "X" & number >= 450 & number <= 459 ~ "Neuropsychiatric conditions",
          
          #Injuries
          letter == "V" & number >= 0   & number <= 999 ~ "Injuries",
          letter == "W" & number >= 0   & number <= 999 ~ "Injuries",
          letter == "X" & number >= 0   & number <= 409 ~ "Injuries",
          letter == "X" & number >= 430 & number <= 449 ~ "Injuries", 
          letter == "X" & number >= 460 & number <= 999 ~ "Injuries", 
          letter == "Y" & number >= 0 & number <= 899 ~ "Injuries",
          
          TRUE ~ "Other"
        )
      )
  }
  
  causes_summarised = causes %>%
    #mutate(state=substr(fips,1,2)) %>%
    rename(month=monthdth) %>%
    group_by(sex,fips,year,month,age,group) %>%
    dplyr::summarise(deaths=sum(deaths)) %>%
    arrange(sex,fips,year,month,age,group) 
  
  # create complete grids for males and females
  sexes = c(1,2)
  #states = sort(unique(causes_summarised$state))
  fipss = sort(unique(causes_summarised$fips))
  months=c(1:12)
  ages = sort(unique(causes_summarised$age))
  groups = causes_summarised %>% drop_na() %>% pull(group) %>% unique()
  
  complete.grid = expand.grid(sex=sexes,fips=fipss,year=x,month=months,age=ages,group=groups)
  
  # rejoin data to complete grids to get zeroes where there were no deaths
  causes_summarised = left_join(complete.grid, causes_summarised) %>%
    mutate(deaths = coalesce(deaths, 0)) %>%
    mutate(state=substr(fips,1,2)) 
  
  print(paste0(x,': total deaths in year = ',sum(causes$deaths)))
  print(paste0('total deaths output to processed file = ',sum(causes_summarised$deaths)))
  return(causes_summarised)
}

#2a.Function to tidy and separate large mortality dataset into cause-specific datasets for faster model processing
create_subcause_datasets <- function(subcauses){
  
  #tidy data: create new age category, calculate mortality 
  tidy_data <- mort_data %>% 
    filter(cause == subcauses) %>% 
    left_join(pop_data) %>% #add population data by FIPS
    mutate(age_group = case_when( #create new age category 
      age %in% c(0) ~ 0,
      age %in% c(5,10) ~ 5,
      age %in% c(15,20) ~ 15,
      age %in% c(25,30) ~ 25,
      age %in% c(35,40) ~ 35,
      age %in% c(45,50) ~ 45,
      age %in% c(55,60) ~ 55,
      age %in% c(65,70) ~ 65,
      age %in% c(75,80) ~ 75,
      age %in% c(85) ~ 85)) %>%
    group_by(state, fips, year, month, age_group, sex, cause, pop) %>%
    summarise(deaths = sum(deaths)) %>%
    mutate(mort_rate = (deaths/pop)*100000) #calculate mortality rate 
  
  create_overall_cat <- tidy_data %>% ungroup() %>% 
    dplyr::select(state, fips, year, month, sex, cause, pop, deaths) %>% distinct() %>% 
    group_by(state, fips, year, month, cause) %>% 
    summarize(deaths = sum(deaths),
              pop = sum(pop)) %>% 
    mutate(mort_rate = (deaths/pop)*100000) %>% 
    mutate(model_group = "overall")
  
  create_sex_cat <- tidy_data %>% ungroup() %>% 
    dplyr::select(state, fips, year, month, sex, cause, pop, deaths) %>% distinct() %>% 
    group_by(state, fips, year, month, cause, sex) %>% 
    summarize(deaths = sum(deaths),
              pop = sum(pop)) %>% 
    mutate(mort_rate = (deaths/pop)*100000) %>% 
    rename(model_group = "sex") %>% 
    mutate_at(c("model_group"), as.character)
  
  create_age_cat <- tidy_data %>% ungroup() %>% 
    dplyr::select(state, fips, year, month, age_group, cause, pop, deaths, mort_rate) %>% distinct() %>% 
    group_by(state, fips, year, month, cause, age_group) %>% 
    summarize(deaths = sum(deaths),
              pop = sum(pop)) %>% 
    mutate(mort_rate = (deaths/pop)*100000) %>% 
    rename(model_group = "age_group") %>% 
    mutate_at(c("model_group"), as.character) %>% 
    bind_rows(create_sex_cat) 
  
  join_all_cat <- bind_rows(create_age_cat, create_overall_cat)
  return(join_all_cat)
}

#2b.Function to tidy and create coarse age categories 
create_coarse_age_datasets <- function(subcauses){
  
  #tidy data: create new age category, calculate mortality 
  tidy_data <- mort_data %>% 
    filter(cause == subcauses) %>% 
    left_join(pop_data) %>% #add population data by FIPS
    mutate(age_group = case_when( #create new age category 
      age %in% c(0,5,10,20,25,30,35,40,45,50,55,60) ~ 64,
      age %in% c(65,70,75,80,85) ~ 66)) %>%
    group_by(state, fips, year, month, age_group, sex, cause, pop) %>%
    summarise(deaths = sum(deaths)) %>%
    mutate(mort_rate = (deaths/pop)*100000) #calculate mortality rate 
  
  create_age_cat <- tidy_data %>% ungroup() %>% 
    dplyr::select(state, fips, year, month, age_group, cause, pop, deaths, mort_rate) %>% distinct() %>% 
    group_by(state, fips, year, month, cause, age_group) %>% 
    summarize(deaths = sum(deaths),
              pop = sum(pop)) %>% 
    mutate(mort_rate = (deaths/pop)*100000) %>% 
    rename(model_group = "age_group") %>% 
    mutate_at(c("model_group"), as.character) 
  
  return(create_age_cat)
}

#3.Function to further separate cause-specific mortality datasets into group-specific datasets for faster model processing
create_subgroup_datasets <- function(subgroups){
  subgroup_data <- data %>% 
    filter(model_group == subgroups) 
  
  saveRDS(subgroup_data, paste0(outcome.data.folder, subcauses, '_', subgroups, '_tidy_data.rds'))
}

#4.Function to add lags to flood dataset 
create_flood_lags <- function(expo_types, expo_thresholds){
  
  for (types in expo_types){
    print(types)
    
    #join mortality and flood data
    flood_by_expo_type <- flood_data %>% filter(expo_type == types)  #filter flood data to specific exposure type
    
    for (thresholds in expo_thresholds){
      print(thresholds)
      
      flood_expo_thershold <- flood_by_expo_type %>% filter(expo_threshold == thresholds) 
      
      add_lags <- left_join(flood_lag_grid, flood_expo_thershold) %>% 
        mutate_at(c("fips"), as.factor) %>% 
        mutate_at(c("month"), as.factor) %>% 
        group_by(fips) %>% 
        arrange(-desc(month)) %>% 
        arrange(-desc(year)) %>% 
        rename(lag_0 = flood_occur) %>% 
        mutate(lag_1 = lagpad(lag_0,1),
               lag_2 = lagpad(lag_0,2),
               lag_3 = lagpad(lag_0,3)) %>% 
        mutate_at("expo_type", ~replace_na(.,types)) %>% 
        mutate_at("expo_threshold", ~replace_na(.,thresholds)) %>% 
        mutate_at(c(12:15), ~replace_na(.,0)) %>% #replace NAs with 0 at lags
        mutate_at(c(8),~replace_na(.,'none')) %>% #replace NA flood type with 'none'
        mutate(stratum = as.factor(month:fips))
    
      #join model result dataframes throughout iteration 
      if(exists("flood_data_with_lags")){
        flood_data_with_lags <- bind_rows(flood_data_with_lags, add_lags)
      }else{
        flood_data_with_lags <- add_lags
      }
    }
  }
  return(flood_data_with_lags)
}

create_exposure_quantile <- function(flood_cats){
  
  for (flood_cat_sev in flood_cats){
    print(flood_cat_sev)
  

  create_quant_table <- gfd_data %>% 
    filter(ghsl_popexp2015 > 0) %>% 
    filter(flood_cat == flood_cat_sev) %>%
    mutate(perc_pop_flood_2015 = ghsl_popexp2015/ghsl_pop2015, 
           quant_pop_flood = ntile(perc_pop_flood_2015, 4)) %>% 
    mutate(expo_type = "pop_expo") %>% 
    add_count(geoid,dfo_began,year,month, flood_cat, quant_pop_flood) %>% 
    mutate("any" = case_when(
      quant_pop_flood  > 0 ~ 1)) %>% 
    mutate("1_pert" = case_when(
      quant_pop_flood == 1 ~ 1)) %>% 
    mutate("25_pert" = case_when(
      quant_pop_flood == 2 ~ 1)) %>% 
    mutate("50_pert" = case_when(
      quant_pop_flood == 3 ~ 1)) %>% 
    mutate("75_pert" = case_when(
      quant_pop_flood == 4 ~ 1)) %>% 
    mutate_at(c(16:20), ~replace_na(.,0)) %>% 
    pivot_longer(cols = 16:20,
                 names_to = "expo_threshold",
                 values_to = "flood_occur")
  
  #join model result dataframes throughout iteration 
  if(exists("exposure_df")){
    exposure_df <- bind_rows(exposure_df, create_quant_table)
  }else{
    exposure_df <- create_quant_table
    }
  }
  return(exposure_df)
}

