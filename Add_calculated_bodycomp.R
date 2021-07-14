# Script to calculate advanced bodycomp measures
# Peter Marquardt, 2020

require(dplyr)
require(percentiles)

#' Add calculated bodycomp
#' 
#' Add derivatives of CT body composition measurements, specifically
#'   * index
#'   * gauge
#'   
#' @param data data.frame containing raw CT measurements
#' 
#' @return data.frame including derived bodycomp variables
#' 
#' @author J. Peter Marquardt, Till D. Best
Add_calculated_bodycomp <- function(data){
  
  ## calculate index, (= normalization to height^2) for all levels for muscle and SAT
  data <- data %>% mutate(T5_Muscle_index = T5_Muscle_muscle_area_cm2 / height_m_float^2,
                          T8_Muscle_index = T8_Muscle_muscle_area_cm2 / height_m_float^2,
                          T10_Muscle_index = T10_Muscle_muscle_area_cm2 / height_m_float^2,
                          L3_Muscle_index = L3_Muscle_muscle_area_cm2 / height_m_float^2,
                          T5_SAT_index = T5_SAT_subcutaneous_fat_area_cm2 / height_m_float^2,
                          T8_SAT_index = T8_SAT_subcutaneous_fat_area_cm2 / height_m_float^2,
                          T10_SAT_index = T10_SAT_subcutaneous_fat_area_cm2 / height_m_float^2,
                          L3_SAT_index = L3_SAT_subcutaneous_fat_area_cm2 / height_m_float^2
                          )
  
  ## calculate muscle gauge for all levels
  data <- data %>% mutate(T5_Muscle_gauge = T5_Muscle_muscle_area_cm2 * T5_Muscle_muscle_mean_hu / height_m_float^2,
                          T8_Muscle_gauge = T8_Muscle_muscle_area_cm2 * T8_Muscle_muscle_mean_hu / height_m_float^2,
                          T10_Muscle_gauge = T10_Muscle_muscle_area_cm2 * T10_Muscle_muscle_mean_hu / height_m_float^2,
                          L3_Muscle_gauge = L3_Muscle_muscle_area_cm2 * L3_Muscle_muscle_mean_hu / height_m_float^2)
  

  return(data)
}