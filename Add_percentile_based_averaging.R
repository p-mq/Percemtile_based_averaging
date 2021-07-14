# Function to perform percentile based averaging on a data frame using T5, T8, T10 and l3.
# Also a function to perform percentile based averaging on multiply imputed data.
# This script uses the new 'percentiles' package.

#' Perform percentile based averaging stratified by sex_bin_fact and i.v. contrast
#' 
#' Percentile based averaging for area, mean attenuation, index, gauge, gauge-index, for muscle and adipose tissue using T5,T8,T10,l3 
#' 
#' @param data a dataframe containing the variables called within the percentiles functions. This is area, mean attenuation for muscle and adipose tissue 
#' @return data the input dataframe with the additional PBA-columns
#' 
#' @author Till D. Best
#' @author J. Peter Marquardt
#' 
#' @import 
Add_percentile_based_averaging <- function(data) {
  
  ## calculate percentile based averages
  data <- data %>% dplyr::mutate(
                  ## SMI
                  perc_T5_Muscle_index = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T5_Muscle_index', stratify_by = 'sex_bin_fact'),
                  perc_T8_Muscle_index = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T8_Muscle_index', stratify_by = 'sex_bin_fact'),
                  perc_T10_Muscle_index = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T10_Muscle_index', stratify_by = 'sex_bin_fact'),
                  perc_L3_Muscle_index = percentiles::calculate_stratified_percentiles(data = data, value_col =  'L3_Muscle_index', stratify_by = 'sex_bin_fact'),
                  ## SATI
                  perc_T5_SAT_index = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T5_SAT_index', stratify_by = 'sex_bin_fact'),
                  perc_T8_SAT_index = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T8_SAT_index', stratify_by = 'sex_bin_fact'),
                  perc_T10_SAT_index = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T10_SAT_index', stratify_by = 'sex_bin_fact'),
                  perc_L3_SAT_index = percentiles::calculate_stratified_percentiles(data = data, value_col =  'L3_SAT_index', stratify_by = 'sex_bin_fact'),
                  ## Muscle attenuation
                  perc_T5_Muscle_attenuation = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T5_Muscle_muscle_mean_hu', stratify_by = c('sex_bin_fact', 'T5_Muscle_iv_contrast')),
                  perc_T8_Muscle_attenuation = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T8_Muscle_muscle_mean_hu', stratify_by = c('sex_bin_fact', 'T8_Muscle_iv_contrast')),
                  perc_T10_Muscle_attenuation = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T10_Muscle_muscle_mean_hu', stratify_by = c('sex_bin_fact', 'T10_Muscle_iv_contrast')),
                  perc_L3_Muscle_attenuation = percentiles::calculate_stratified_percentiles(data = data, value_col =  'L3_Muscle_muscle_mean_hu', stratify_by = c('sex_bin_fact', 'L3_Muscle_iv_contrast')),
                  ## SAT attenuation
                  perc_T5_SAT_attenuation = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T5_SAT_subcutaneous_fat_mean_hu', stratify_by = c('sex_bin_fact', 'T5_SAT_iv_contrast')),
                  perc_T8_SAT_attenuation = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T8_SAT_subcutaneous_fat_mean_hu', stratify_by = c('sex_bin_fact', 'T8_SAT_iv_contrast')),
                  perc_T10_SAT_attenuation = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T10_SAT_subcutaneous_fat_mean_hu', stratify_by = c('sex_bin_fact', 'T10_SAT_iv_contrast')),
                  perc_L3_SAT_attenuation = percentiles::calculate_stratified_percentiles(data = data, value_col =  'L3_SAT_subcutaneous_fat_mean_hu', stratify_by = c('sex_bin_fact', 'L3_SAT_iv_contrast')),
                  ## SMG
                  perc_T5_Muscle_gauge = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T5_Muscle_gauge', stratify_by = c('sex_bin_fact', 'T5_Muscle_iv_contrast')),
                  perc_T8_Muscle_gauge = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T8_Muscle_gauge', stratify_by = c('sex_bin_fact', 'T8_Muscle_iv_contrast')),
                  perc_T10_Muscle_gauge = percentiles::calculate_stratified_percentiles(data = data, value_col =  'T10_Muscle_gauge', stratify_by = c('sex_bin_fact', 'T10_Muscle_iv_contrast')),
                  perc_L3_Muscle_gauge = percentiles::calculate_stratified_percentiles(data = data, value_col =  'L3_Muscle_gauge', stratify_by = c('sex_bin_fact', 'L3_Muscle_iv_contrast'))
  )
  
  ## calculate average percentiles
  data <- data %>% dplyr::mutate(
                  average_perc_Muscle_index = rowMeans(data[, c('perc_T5_Muscle_index', 'perc_T8_Muscle_index', 'perc_T10_Muscle_index', 'perc_L3_Muscle_index')], na.rm=TRUE),
                  average_perc_SAT_index = rowMeans(data[, c('perc_T5_SAT_index', 'perc_T8_SAT_index', 'perc_T10_SAT_index', 'perc_L3_SAT_index')], na.rm=TRUE),
                  average_perc_Muscle_attenuation = rowMeans(data[, c('perc_T5_Muscle_attenuation', 'perc_T8_Muscle_attenuation', 'perc_T10_Muscle_attenuation', 'perc_L3_Muscle_attenuation')], na.rm=TRUE),
                  average_perc_SAT_attenuation = rowMeans(data[, c('perc_T5_SAT_attenuation', 'perc_T8_SAT_attenuation', 'perc_T10_SAT_attenuation', 'perc_L3_SAT_attenuation')], na.rm=TRUE),
                  average_perc_Muscle_gauge = rowMeans(data[, c('perc_T5_Muscle_gauge', 'perc_T8_Muscle_gauge', 'perc_T10_Muscle_gauge', 'perc_L3_Muscle_gauge')], na.rm=TRUE)
  )
  
  data <- data %>% dplyr::mutate_at(vars(average_perc_Muscle_index, average_perc_SAT_index, average_perc_Muscle_attenuation, average_perc_Muscle_gauge, average_perc_Muscle_gauge), ~replace(., is.nan(.), NA))
  
  return(data)
}
