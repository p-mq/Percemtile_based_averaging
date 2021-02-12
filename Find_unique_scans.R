# Calculating the number of unique scans at different exclusion steps
require(readr)

exclusion_df <- read_csv('B:\\Dropbox (Partners HealthCare)\\iPRO_BC_shared\\20200522_iPRO_Cachexia_Database_for Florian.csv')

bodycomp_columns <- c('T5_Muscle_series_instance_uid',
                      'T8_Muscle_series_instance_uid',
                      'T10_Muscle_series_instance_uid',
                      'L3_Muscle_series_instance_uid',
                      'T5_SAT_series_instance_uid',
                      'T8_SAT_series_instance_uid',
                      'T10_SAT_series_instance_uid',
                      'L3_SAT_series_instance_uid',
                      'L3_VAT_series_instance_uid'
                      )

muscle_columns <- c('T5_Muscle_series_instance_uid',
                    'T8_Muscle_series_instance_uid',
                    'T10_Muscle_series_instance_uid',
                    'L3_Muscle_series_instance_uid'
                    )

filter_any_bc_data <- function(df, bc_cols){
  keep_columns <- vector(length = nrow(df))
  for (col in bc_cols){
    keep_columns <- keep_columns | !is.na(df[[col]])
  }
  df <- df[keep_columns,]
  return(df)
}

any_bc_df <- filter_any_bc_data(exclusion_df, bodycomp_columns) # patients with any bodycomp data
muscle_df <- filter_any_bc_data(exclusion_df, muscle_columns) # patients with muscle data

full_muscle_df <- muscle_df
for (col in muscle_columns){full_muscle_df <- full_muscle_df[!is.na(full_muscle_df[[col]]), ]}  # dataframe with full muscle data

find_unique_series_ids <- function(df, id_columns){
  unique_ids <- c()
  for (col in id_columns){
    unique_ids <- unique(append(unique_ids, df[[col]]))
  }
  return(unique_ids)
}

length(find_unique_series_ids(any_bc_df, bodycomp_columns))  # series with any bodycomp data
length(find_unique_series_ids(muscle_df, muscle_columns))  # series with muscle data
length(find_unique_series_ids(full_muscle_df, muscle_columns))  # series with full muscle data

