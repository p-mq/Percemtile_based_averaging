# Script to categorise cancer types
# (c) Peter Marquardt, 2020-06-17

require(dplyr)

group_cancer_types <- function(df, ipro_cancer_columns) {
  
  # initialise new column
  df$cancer_group_fact <- NA_character_
  
  # loop over rows
  for (i in 1:nrow(df)) {
    
    # Look for cancer type
    for (ccol in ipro_cancer_columns){
      
      if (df[[i, ccol]] == 1){
        
        # We found the cancer type!
        # Check that there is no conflicting entry already present
        if(!is.na(df[i, 'cancer_group_fact'])){
          
          # Error, there should only be one category per patient
          stop(sprintf('Error, there should only be one category per patient
                 Row = %s
                 Type 1 = %s
                 Type 2 = %s', i, df[[i, 'cancer_group_fact']], ccol))
        }
        
        # No error so far, so we assign the category
        df[i, 'cancer_group_fact'] <- substr(ccol, 8, nchar(ccol)-8)  # Extract name only
      }
    }
    # Default elephant in cape town
    if(is.na(df[i, 'cancer_group_fact'])){
      df[i, 'cancer_group_fact'] <- 'AA_Other/Unknown'
    }
  }
  
  # Factorise new column
  df$cancer_group_fact <- as.factor(df$cancer_group_fact)
  
  # Delete unused columns
  df <- df[, !(names(df) %in% ipro_cancer_columns)]
  
  return(df)
}