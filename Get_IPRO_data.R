# IPRO2 main script
# (C) Peter Marquardt, 2020

require(dplyr)

Get_IPRO_data <- function(csv_location='B:\\Dropbox (Partners HealthCare)\\iPRO_BC_shared\\20200522_iPRO_Cachexia_Database_for Florian.csv') {
  
  # Importing the data
  source('Read_IPRO_data.R')
  ipro_data <- Read_IPRO_data(data_location=csv_location)
  
  
  # Group cancer types - NEW
  source('Group_cancer_types.R')
  ipro_data <- group_cancer_types(ipro_data, 
                                  c('Cancer_GI_Richard',
                                  'Cancer_Lung_Richard',
                                  'Cancer_Heme_Richard',
                                  'Cancer_GU_Richard',
                                  'Cancer_Breast_Richard',
                                  'Cancer_Melanoma_Richard',
                                  'Cancer_HeadAndNeck_Richard'))
  
  # Group cancer types - OLD
  # source('Group_cancer_types_old_v1.R')
  # ipro_data <- group_cancer_types(ipro_data, 'cancer_dx_cat_fact')
  
  return(ipro_data)
}