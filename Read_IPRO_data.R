# Script to read in data for the IPRO II project
# (C) Peter Marquardt, 2020

# 0. Libraries and preparations #############
require(readr)
require(dplyr)


Read_IPRO_data <- function(data_location='B:\\Dropbox (Partners HealthCare)\\iPRO_BC_shared\\20200522_iPRO_Cachexia_Database_for Florian.csv') {

  # 1. Reading in data #########################
  full_ipro_data <- read_csv(data_location, na = c('', 'NA', '#NULL!'))
  
  ipro_data_selection <- full_ipro_data[,c('Number', 'Length_of_Stay_adm1', 'Days_DC_death_or_lastFU',
                                           'Days_from_DC1_to_Readmit1_Death_or_LastFU',
                                           'Death_or_Readmission_within90Days', 'Female_sex', 'Age',
                                           'Age65_and_above', 'Race_cat', 'Religion_cat',
                                           'Education_above_HS', 'Married', 'Months_initial_cancer_dx_to_Adm1',
                                           'Death_censor', 'PHQ4_total', 'PHQ2_total', 'GAD2_total', 'ESAS_total',
                                           'ESAS_physical', 'CCI_total_score', 'Insurance_government',
                                           'Cancer_GI_Richard', 'Cancer_Lung_Richard',
                                           'Cancer_heme_Richard', 'Cancer_GU_Richard',
                                           'Cancer_Breast_Richard', 'Cancer_Melanoma_Richard',
                                           'Cancer_HeadAndNeck_Richard',
                                           'USE_THIS_L3_Muscle_muscle_area_cm2',
                                           'Use_This_L3_Muscle_muscle_mean_hu',
                                           'L3_Muscle_contrast',
                                           'T10_Muscle_muscle_area_cm2',
                                           'T10_Muscle_muscle_mean_hu',
                                           'T10_Muscle_contrast',
                                           'T8_Muscle_muscle_area_cm2',
                                           'T8_Muscle_muscle_mean_hu',
                                           'T8_Muscle_contrast',
                                           'T5_Muscle_muscle_area_cm2',
                                           'T5_Muscle_muscle_mean_hu',
                                           'T5_Muscle_contrast',
                                           'L3_SAT_subcutaneous_fat_area_cm2',
                                           'L3_SAT_subcutaneous_fat_mean_hu',
                                           'L3_SAT_contrast',
                                           'T10_SAT_subcutaneous_fat_area_cm2',
                                           'T10_SAT_subcutaneous_fat_mean_hu',
                                           'T10_SAT_contrast',
                                           'T8_SAT_subcutaneous_fat_area_cm2',
                                           'T8_SAT_subcutaneous_fat_mean_hu',
                                           'T8_SAT_contrast',
                                           'T5_SAT_subcutaneous_fat_area_cm2',
                                           'T5_SAT_subcutaneous_fat_mean_hu',
                                           'T5_SAT_contrast',
                                           'EVS_height_meters', 'EVS_height_m2', 'EVS_BMI'
                                           )]
  
  # 2. Data cleaning _____________________________________________________________________________________________________________________________------------------------------------
  
  # 2.1 Tidy names ####
  names(ipro_data_selection) = c('study_nr_char', 'length_of_stay_adm1_int', 'days_to_death_LFU_int',
                                 'days_adm1_to_readm_death_LFU_int',
                                 'death_or_readm_within_90d_bin_fact', 'sex_bin_fact', 'age_int',
                                 'age_above_64_bin_fact', 'race_fact', 'religion_fact',
                                 'education_bin_fact', 'married_bin_fact', 'months_initial_cancer_dx_to_adm1_float',
                                 'Death_observed_bool', 'PHQ4_total_int', 'PHQ2_total_int', 'GAD2_total_int', 'ESAS_total_int',
                                 'ESAS_physical_int', 'CCI_total_int', 'Insurance_government_bin_fact',
                                 'Cancer_GI_Richard', 'Cancer_Lung_Richard',
                                 'Cancer_Heme_Richard', 'Cancer_GU_Richard',
                                 'Cancer_Breast_Richard', 'Cancer_Melanoma_Richard',
                                 'Cancer_HeadAndNeck_Richard',
                                 'L3_Muscle_muscle_area_cm2',
                                 'L3_Muscle_muscle_mean_hu',
                                 'L3_Muscle_iv_contrast',
                                 'T10_Muscle_muscle_area_cm2',
                                 'T10_Muscle_muscle_mean_hu',
                                 'T10_Muscle_iv_contrast',
                                 'T8_Muscle_muscle_area_cm2',
                                 'T8_Muscle_muscle_mean_hu',
                                 'T8_Muscle_iv_contrast',
                                 'T5_Muscle_muscle_area_cm2',
                                 'T5_Muscle_muscle_mean_hu',
                                 'T5_Muscle_iv_contrast',
                                 'L3_SAT_subcutaneous_fat_area_cm2',
                                 'L3_SAT_subcutaneous_fat_mean_hu',
                                 'L3_SAT_iv_contrast',
                                 'T10_SAT_subcutaneous_fat_area_cm2',
                                 'T10_SAT_subcutaneous_fat_mean_hu',
                                 'T10_SAT_iv_contrast',
                                 'T8_SAT_subcutaneous_fat_area_cm2',
                                 'T8_SAT_subcutaneous_fat_mean_hu',
                                 'T8_SAT_iv_contrast',
                                 'T5_SAT_subcutaneous_fat_area_cm2',
                                 'T5_SAT_subcutaneous_fat_mean_hu',
                                 'T5_SAT_iv_contrast',
                                 'height_m_float', 'height_m2_float', 'BMI_float'
                                 )
  
  
  # 2.2 Correct datatypes and values ####
  
  # copying for error checking later on
  ipro_data_selection_copy <- ipro_data_selection
  
  # Getting everything into correct datatype
  ipro_data_selection$study_nr_char <- as.character(ipro_data_selection[['study_nr_char']])
  ipro_data_selection$length_of_stay_adm1_int <- as.numeric(ipro_data_selection[['length_of_stay_adm1_int']])
  ipro_data_selection$days_to_death_LFU_int <- as.numeric(ipro_data_selection[['days_to_death_LFU_int']])
  ipro_data_selection$days_adm1_to_readm_death_LFU_int <- as.numeric(ipro_data_selection[['days_adm1_to_readm_death_LFU_int']])
  ipro_data_selection$death_or_readm_within_90d_bin_fact <- factor(ipro_data_selection[['death_or_readm_within_90d_bin_fact']])
  ipro_data_selection$sex_bin_fact <- factor(ipro_data_selection[['sex_bin_fact']])
  ipro_data_selection$age_int <- as.numeric(ipro_data_selection[['age_int']])
  ipro_data_selection$age_above_64_bin_fact <- factor(ipro_data_selection[['age_above_64_bin_fact']])
  ipro_data_selection$race_fact <- factor(ipro_data_selection[['race_fact']])
  ipro_data_selection$religion_fact <- factor(ipro_data_selection[['religion_fact']])
  ipro_data_selection$education_bin_fact <- factor(ipro_data_selection[['education_bin_fact']])
  ipro_data_selection$married_bin_fact <- factor(ipro_data_selection[['married_bin_fact']])
  ipro_data_selection$months_initial_cancer_dx_to_adm1_float <- as.double(ipro_data_selection[['months_initial_cancer_dx_to_adm1_float']])
  ipro_data_selection$Death_observed_bool <- as.logical(ipro_data_selection[['Death_observed_bool']])
  ipro_data_selection$PHQ4_total_int <- as.numeric(ipro_data_selection[['PHQ4_total_int']])
  ipro_data_selection$PHQ2_total_int <- as.numeric(ipro_data_selection[['PHQ2_total_int']])
  ipro_data_selection$GAD2_total_int <- as.numeric(ipro_data_selection[['GAD2_total_int']])
  ipro_data_selection$ESAS_total_int <- as.numeric(ipro_data_selection[['ESAS_total_int']])
  ipro_data_selection$ESAS_physical_int <- as.numeric(ipro_data_selection[['ESAS_physical_int']])
  ipro_data_selection$CCI_total_int <- as.numeric(ipro_data_selection[['CCI_total_int']])
  ipro_data_selection$Insurance_government_bin_fact <- factor(ipro_data_selection[['Insurance_government_bin_fact']])
  ipro_data_selection$Cancer_GI_Richard <- factor(ipro_data_selection[['Cancer_GI_Richard']])
  ipro_data_selection$Cancer_Lung_Richard <- factor(ipro_data_selection[['Cancer_Lung_Richard']])
  ipro_data_selection$Cancer_Heme_Richard <- factor(ipro_data_selection[['Cancer_Heme_Richard']])
  ipro_data_selection$Cancer_GU_Richard <- factor(ipro_data_selection[['Cancer_GU_Richard']])
  ipro_data_selection$Cancer_Breast_Richard <- factor(ipro_data_selection[['Cancer_Breast_Richard']])
  ipro_data_selection$Cancer_Melanoma_Richard <- factor(ipro_data_selection[['Cancer_Melanoma_Richard']])
  ipro_data_selection$Cancer_HeadAndNeck_Richard <- factor(ipro_data_selection[['Cancer_HeadAndNeck_Richard']])
  ipro_data_selection$L3_Muscle_muscle_area_cm2 <- as.double(ipro_data_selection[['L3_Muscle_muscle_area_cm2']])
  ipro_data_selection$L3_Muscle_muscle_mean_hu <- as.double(ipro_data_selection[['L3_Muscle_muscle_mean_hu']])
  ipro_data_selection$L3_Muscle_iv_contrast <- as.double(ipro_data_selection[['L3_Muscle_iv_contrast']])
  ipro_data_selection$T10_Muscle_muscle_area_cm2 <- as.double(ipro_data_selection[['T10_Muscle_muscle_area_cm2']])
  ipro_data_selection$T10_Muscle_muscle_mean_hu <- as.double(ipro_data_selection[['T10_Muscle_muscle_mean_hu']])
  ipro_data_selection$T10_Muscle_iv_contrast <- as.double(ipro_data_selection[['T10_Muscle_iv_contrast']])
  ipro_data_selection$T8_Muscle_muscle_area_cm2 <- as.double(ipro_data_selection[['T8_Muscle_muscle_area_cm2']])
  ipro_data_selection$T8_Muscle_muscle_mean_hu <- as.double(ipro_data_selection[['T8_Muscle_muscle_mean_hu']])
  ipro_data_selection$T8_Muscle_iv_contrast <- as.double(ipro_data_selection[['T8_Muscle_iv_contrast']])
  ipro_data_selection$T5_Muscle_muscle_area_cm2 <- as.double(ipro_data_selection[['T5_Muscle_muscle_area_cm2']])
  ipro_data_selection$T5_Muscle_muscle_mean_hu <- as.double(ipro_data_selection[['T5_Muscle_muscle_mean_hu']])
  ipro_data_selection$T5_Muscle_iv_contrast <- as.double(ipro_data_selection[['T5_Muscle_iv_contrast']])
  ipro_data_selection$height_m_float <- as.double(ipro_data_selection[['height_m_float']])
  ipro_data_selection$height_m2_float <- as.double(ipro_data_selection[['height_m2_float']])
  ipro_data_selection$BMI_float <- as.double(ipro_data_selection[['BMI_float']])
  
  
  # Naming binary factor variables
  ipro_data_selection$sex_bin_fact <- ifelse(ipro_data_selection$sex_bin_fact == 0, 'Male', 'Female')
  ipro_data_selection$age_above_64_bin_fact <- ifelse(ipro_data_selection$age_above_64_bin_fact == 0, 'No', 'Yes')
  ipro_data_selection$education_bin_fact <- ifelse(ipro_data_selection$education_bin_fact == 0, 'Highschool or less', 'Beyond highschool')
  ipro_data_selection$married_bin_fact <- ifelse(ipro_data_selection$married_bin_fact == 0, 'No', 'Yes')
  ipro_data_selection$Insurance_government_bin_fact <- ifelse(ipro_data_selection$Insurance_government_bin_fact == 0, 'No', 'Yes')
  
  # Binary outcomes have to be labled 0/1 for glm to work
  ipro_data_selection$death_or_readm_within_90d_bin_fact <- ifelse(ipro_data_selection$death_or_readm_within_90d_bin_fact == 0, 0, 1)
  
  # Checking that we didn't introduce NAs through type conversion
  for (i in 1:nrow(ipro_data_selection)){
    for (col in colnames(ipro_data_selection)){
      if (is.na(ipro_data_selection[[i, col]])) {
        if (!is.na(ipro_data_selection_copy[[i, col]])){
          if (!ipro_data_selection_copy[[i, col]] == 'NA') {
            stop(sprintf('Data type conversion introduced some NAs - please check manually
                 Column = %s
                 Row = %s', col, i))
          }
        }
      }
    }
  }
  
  
  ## Naming multilevel factor variables
  ipro_data_selection$race_fact <- as.character(ipro_data_selection$race_fact)
  ipro_data_selection$religion_fact <- as.character(ipro_data_selection$religion_fact)
  # ipro_data_selection$cancer_dx_cat_fact <- as.character(ipro_data_selection$cancer_dx_cat_fact)
  for (i in 1:nrow(ipro_data_selection)){
    race <- ipro_data_selection$race_fact[i]
    religion <- ipro_data_selection$religion_fact[i]
    # cancer_type <- ipro_data_selection$cancer_dx_cat_fact[i]
    
    # CRace
    if (race == '6'){
      race <- 'White'
    }
    else if (is.na(race)){
      race <- NA
    }
    else {
      race <- 'Non-White'
    }
    
    # Religion
    if (religion == '0'){
      religion <- '0'
    }
    else if (religion == '1'){
      religion <- '1'
    }
    else if (religion == '2'){
      religion <- '2'
    }
    else if (religion == '3'){
      religion <- '3'
    }
    else if (religion == '4'){
      religion <- '4'
    }
    
    # # Cancer types
    # if (cancer_type == '0'){
    #   cancer_type <- 'GI'
    # }
    # else if (cancer_type == '1'){
    #   cancer_type <- 'Lung'
    # }
    # else if (cancer_type == '2'){
    #   cancer_type <- 'Breast'
    # }
    # else if (cancer_type == '3'){
    #   cancer_type <- 'GU'
    # }
    # else if (cancer_type == '4'){
    #   cancer_type <- 'Melanoma'
    # }
    # else if (cancer_type == '5'){
    #   cancer_type <- 'Gyn'
    # }
    # else if (cancer_type == '6'){
    #   cancer_type <- 'HN'
    # }
    # else if (cancer_type == '7'){
    #   cancer_type <- 'Sarcoma'
    # }
    # else if (cancer_type == '8'){
    #   cancer_type <- 'Type not found'
    # }
    # else if (cancer_type == '9'){
    #   cancer_type <- 'Unknown'
    # }
    # else if (cancer_type == '10'){
    #   cancer_type <- 'Leukemia'
    # }
    # else if (cancer_type == '11'){
    #   cancer_type <- 'Myeloproliferative Other'
    # }
    # else if (cancer_type == '12'){
    #   cancer_type <- 'Lymphoma'
    # }
    # else if (cancer_type == '13'){
    #   cancer_type <- 'Plasma Cell Disorder'
    # }
  
    
    ipro_data_selection$race_fact[i] <- race
    ipro_data_selection$religion_fact[i] <- religion
    # ipro_data_selection$cancer_dx_cat_fact[i] <- cancer_type
  }
  
  
  ipro_data_selection$race_fact <- factor(ipro_data_selection[['race_fact']])
  colnames(ipro_data_selection)[colnames(ipro_data_selection) == 'race_fact'] <- 'race_bin_fact'
  ipro_data_selection$religion_fact <- factor(ipro_data_selection[['religion_fact']])
  # ipro_data_selection$cancer_dx_cat_fact <- factor(ipro_data_selection[['cancer_dx_cat_fact']])
  
  # correcting a data entry mistake
  ipro_data_selection[603, 'T8_Muscle_iv_contrast'] <- 1
  
  # Clean up memory
  rm(ipro_data_selection_copy)
  rm(full_ipro_data)
  # rm(cancer_type, race, religion)

  return(ipro_data_selection)
}

