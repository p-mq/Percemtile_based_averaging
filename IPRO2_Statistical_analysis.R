## ----set-options, echo=FALSE, cache=FALSE--------------------------------------------------------------------
options(width = 2000)


## ---- echo=FALSE, message=FALSE------------------------------------------------------------------------------
library(plyr)
library(survival)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(survAUC)
library(DescTools)
library(dplyr)


## ---- echo=TRUE, warning=FALSE, message=FALSE, cache=FALSE---------------------------------------------------
# Importing the data
source('Get_IPRO_data.R')
df <- Get_IPRO_data()
df_no_na <- df[!is.na(df$L3_Muscle_muscle_area_cm2) &
               !is.na(df$T10_Muscle_muscle_area_cm2) &
               !is.na(df$T8_Muscle_muscle_area_cm2) &
               !is.na(df$T5_Muscle_muscle_area_cm2)
               , ]



## ---- echo=FALSE,warning=FALSE, message=FALSE, cache=FALSE---------------------------------------------------
# Add percenntile based imputation
source('../PBI/Percentile_based_imputation.R')
analysed_levels <- c("T5", "T8", "T10", "L3")
available_compartments <- vector(mode = 'list', length = length(analysed_levels))
names(available_compartments) <- analysed_levels
for (level in analysed_levels){available_compartments[[level]] <- c('Muscle', 'SAT')}

df <- Percentile_based_imputation(df=df, sex_column = 'sex_bin_fact',
                                   female_value = 'Female', male_value = 'Male',
                                   height_column = 'height_m_float',
                                   available_compartments = available_compartments
                                  )

df_no_na <- Percentile_based_imputation(df=df_no_na, sex_column = 'sex_bin_fact',
                                         female_value = 'Female', male_value = 'Male',
                                         height_column = 'height_m_float',
                                         available_compartments = available_compartments
                                       )


## ---- echo=TRUE, message=FALSE-------------------------------------------------------------------------------
source('../Blanket-statsments/Blanket_statsments.R')  # Making wrapper methods available

# Defining standard covariates
common_covariates <- c('age_int',
                       'sex_bin_fact',
                       'education_bin_fact',
                       'Insurance_government_bin_fact',
                       'married_bin_fact',
                       'cancer_group_fact',
                       'BMI_float')

# Defining standard predictor sets
index_predictors <- vector(mode = "list", length = 0)  # Muscle index measurments
index_predictors[['L3']] <- c('L3_Muscle_index')
index_predictors[['T10']] <- c('T10_Muscle_index')
index_predictors[['T8']] <- c('T8_Muscle_index')
index_predictors[['T5']] <- c('T5_Muscle_index')
index_predictors[['PBI']] <- c('average_perc_Muscle_index')

atten_predictors <- vector(mode = "list", length = 0)  # Muscle attenuation measurments
atten_predictors[['L3']] <- c('L3_Muscle_iv_contrast', 'L3_Muscle_muscle_mean_hu')
atten_predictors[['T10']] <- c('T10_Muscle_iv_contrast', 'T10_Muscle_muscle_mean_hu')
atten_predictors[['T8']] <- c('T8_Muscle_iv_contrast', 'T8_Muscle_muscle_mean_hu')
atten_predictors[['T5']] <- c('T5_Muscle_iv_contrast', 'T5_Muscle_muscle_mean_hu')
atten_predictors[['PBI']] <- c('average_perc_Muscle_attenuation')

gauge_predictors <- vector(mode = "list", length = 0)  # Muscle gauge measurments
gauge_predictors[['L3']] <- c('L3_Muscle_iv_contrast', 'L3_Muscle_gauge')
gauge_predictors[['T10']] <- c('T10_Muscle_iv_contrast', 'T10_Muscle_gauge')
gauge_predictors[['T8']] <- c('T8_Muscle_iv_contrast', 'T8_Muscle_gauge')
gauge_predictors[['T5']] <- c('T5_Muscle_iv_contrast', 'T5_Muscle_gauge')
gauge_predictors[['PBI']] <- c('average_perc_Muscle_gauge')

# Declaring which models to run
ipro_models_to_run_df <- read_csv('ipro_models_to_run.csv')
ipro_models_to_run_df



## ---- echo=TRUE----------------------------------------------------------------------------------------------
# Running the models
index_models <- blanket_statsments(df, ipro_models_to_run_df, index_predictors, common_covariates)
atten_models <- blanket_statsments(df, ipro_models_to_run_df, atten_predictors, common_covariates)
gauge_models <- blanket_statsments(df, ipro_models_to_run_df, gauge_predictors, common_covariates)

# tabling the key characteristics
index_model_comp_df <- table_blanket_statsments(df, index_models)
atten_model_comp_df <- table_blanket_statsments(df, atten_models)
gauge_model_comp_df <- table_blanket_statsments(df, gauge_models)


## ---- echo=TRUE----------------------------------------------------------------------------------------------
rownames(index_model_comp_df) <- paste(rownames(index_model_comp_df), '_index', sep = '')
rownames(atten_model_comp_df) <- paste(rownames(atten_model_comp_df), '_atten', sep = '')
rownames(gauge_model_comp_df) <- paste(rownames(gauge_model_comp_df), '_gauge', sep = '')

overview_comp_df <- rbind(index_model_comp_df, atten_model_comp_df, gauge_model_comp_df)
overview_comp_df_ordered <- overview_comp_df[order(row.names(overview_comp_df)),]
colnames(overview_comp_df_ordered) <- rep(c('n', 'C', 'R²', 'p'), times = 5)

kable(overview_comp_df_ordered) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  kable_styling(fixed_thead = T) %>%
  column_spec(1, bold = T) %>%
  add_header_above(c("Model/used BC" = 1, "L3" = 4, "T10" = 4, "T8" = 4, "T5" = 4, "PBI" = 4)) %>%
  row_spec(0, bold = T) %>%
  pack_rows("Attenuation-based models", 1, 8) %>%
  pack_rows("Index-based models", 9, 16) %>%
  pack_rows("Gauge-based models", 17, 24) %>%
  scroll_box(width = "100%", height = "100%")



## ---- echo=TRUE----------------------------------------------------------------------------------------------
source('Rank_df.R')  # Making wrapper methods available

comp_row_names <- rownames(overview_comp_df)  # remembering row names for later
comp_col_names <- colnames(overview_comp_df)  # remembering column names for later

# ranking index vs attenuation vs gauge
ranked_row_df <- rank_df_rows(overview_comp_df, items = c('OS', 'Readm_death_TTE', 'Readm_death_win_90d', 'LOS', 'PHQ4D', 'PHQ4A',   'ESAS_total', 'ESAS_phys'),
                              levels = c('_index', '_atten', '_gauge'),
                              rank_by = 'levels')

# ranking single levels vs PBI
ranked_col_df <- rank_df_cols(overview_comp_df,
                              items = c('L3', 'T10', 'T8', 'T5', 'PBI'),
                              levels = c('_n', '_C', '_R^2', '_p'),
                              rank_by = 'items')

# inverting rank for p-values
for (coln in c('L3_p', 'T10_p', 'T8_p', 'T5_p', 'PBI_p')){
  ranked_col_df[[coln]] <- -ranked_col_df[[coln]] + 6
  ranked_row_df[[coln]] <- -ranked_row_df[[coln]] + 4
}

overview_comp_df <- format(overview_comp_df, digits = 2) # we only need the first two decimal places now


# helper function to colour columns on a red-to-green gradient based on rank
rank_to_colour <- function(vct){
  colfunc<-colorRampPalette(c('springgreen', 'red'))
  colours <- colfunc(5)
  ret_vct <- c()
  for(el in vct){ret_vct <- append(ret_vct, colours[el])}
  return(ret_vct)
}


format_column <- function(df, clnm){
  df <- df %>%
    mutate(
      !!clnm := cell_spec(df[[!! clnm]], color = rank_to_colour(ranked_col_df[[!!clnm]]),
                          font_size = -2*ranked_row_df[[!! clnm]] + 18, bold = T)
    )
}

for (clnm in colnames(overview_comp_df)){
  overview_comp_df <- format_column(overview_comp_df, enquo(clnm))
}

rownames(overview_comp_df) <- comp_row_names
colnames(overview_comp_df) <- rep(c('n', 'C', 'R²', 'p'), times = 5)

overview_comp_df %>%
  kable(escape = F, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  kable_styling(fixed_thead = T) %>%
  add_header_above(c("Model/used BC" = 1, "L3" = 4, "T10" = 4, "T8" = 4, "T5" = 4, "PBI" = 4)) %>%
  column_spec(1, bold = T) %>%
  row_spec(0, bold = T) %>%
  pack_rows("Index-based models", 1, 8) %>%
  pack_rows("Attenuation-based models", 9, 16) %>%
  pack_rows("Gauge-based models", 17, 24) %>%
  scroll_box(width = "100%", height = "100%")



## ---- echo=TRUE----------------------------------------------------------------------------------------------
# Running the models
cca_index_models <- blanket_statsments(df_no_na, ipro_models_to_run_df, index_predictors, common_covariates)
cca_atten_models <- blanket_statsments(df_no_na, ipro_models_to_run_df, atten_predictors, common_covariates)
cca_gauge_models <- blanket_statsments(df_no_na, ipro_models_to_run_df, gauge_predictors, common_covariates)

# tabling the key characteristics
cca_index_model_comp_df <- table_blanket_statsments(df_no_na, cca_index_models)
cca_atten_model_comp_df <- table_blanket_statsments(df_no_na, cca_atten_models)
cca_gauge_model_comp_df <- table_blanket_statsments(df_no_na, cca_gauge_models)


## ---- echo=TRUE----------------------------------------------------------------------------------------------
rownames(cca_index_model_comp_df) <- paste(rownames(cca_index_model_comp_df), '_index', sep = '')
rownames(cca_atten_model_comp_df) <- paste(rownames(cca_atten_model_comp_df), '_atten', sep = '')
rownames(cca_gauge_model_comp_df) <- paste(rownames(cca_gauge_model_comp_df), '_gauge', sep = '')

cca_overview_comp_df <- rbind(cca_index_model_comp_df, cca_atten_model_comp_df, cca_gauge_model_comp_df)
cca_overview_comp_df_ordered <- cca_overview_comp_df[order(row.names(cca_overview_comp_df)),]
colnames(cca_overview_comp_df_ordered) <- rep(c('n', 'C', 'R²', 'p'), times = 5)

cca_comp_row_names <- rownames(cca_overview_comp_df)  # remembering row names for later

# ranking index vs attenuation vs gauge
cca_ranked_row_df <- rank_df_rows(cca_overview_comp_df, items = c('OS', 'Readm_death_TTE', 'Readm_death_win_90d', 'LOS', 'PHQ4D', 'PHQ4A',   'ESAS_total', 'ESAS_phys'),
                              levels = c('_index', '_atten', '_gauge'),
                              rank_by = 'levels')

# ranking single levels vs PBI
cca_ranked_col_df <- rank_df_cols(cca_overview_comp_df,
                              items = c('L3', 'T10', 'T8', 'T5', 'PBI'),
                              levels = c('_n', '_C', '_R^2', '_p'),
                              rank_by = 'items')

# inverting rank for p-values
for (coln in c('L3_p', 'T10_p', 'T8_p', 'T5_p', 'PBI_p')){
  cca_ranked_col_df[[coln]] <- -cca_ranked_col_df[[coln]] + 6
  cca_ranked_row_df[[coln]] <- -cca_ranked_row_df[[coln]] + 4
}

cca_overview_comp_df <- format(cca_overview_comp_df, digits = 2) # we only need the first two decimal places now

format_column_cca <- function(df, clnm){
  df <- df %>%
    mutate(
      !!clnm := cell_spec(df[[!! clnm]], color = rank_to_colour(cca_ranked_col_df[[!!clnm]]),
                          font_size = -2*cca_ranked_row_df[[!! clnm]] + 18, bold = T)
    )
}

for (clnm in colnames(cca_overview_comp_df)){
  cca_overview_comp_df <- format_column_cca(cca_overview_comp_df, enquo(clnm))
}

rownames(cca_overview_comp_df) <- cca_comp_row_names
colnames(cca_overview_comp_df) <- rep(c('n', 'C', 'R²', 'p'), times = 5)

cca_overview_comp_df %>%
  kable(escape = F, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  kable_styling(fixed_thead = T) %>%
  add_header_above(c("Model/used BC" = 1, "L3" = 4, "T10" = 4, "T8" = 4, "T5" = 4, "PBI" = 4)) %>%
  column_spec(1, bold = T) %>%
  row_spec(0, bold = T) %>%
  pack_rows("Index-based models", 1, 8) %>%
  pack_rows("Attenuation-based models", 9, 16) %>%
  pack_rows("Gauge-based models", 17, 24) %>%
  scroll_box(width = "100%", height = "100%")



## ---- echo=TRUE----------------------------------------------------------------------------------------------

# Summarising the column-wise comparisn is easy
summary_col_comp_df <- data.frame()
for (coln in colnames(ranked_col_df)) {
  summary_col_comp_df[coln, 'all cases'] <- mean(ranked_col_df[[coln]])
  summary_col_comp_df[coln, 'complete cases'] <- mean(cca_ranked_col_df[[coln]])
}


# Summarising the row-wise comparison requires a helper method
compare_metrics <- function (ranked_df, out_df) {
  for (metric in c('index', 'atten', 'gauge')) {
    for (characteristic in c('_n', '_C', '_R^2', '_p')) {
      sum_ranks <- 0
      num_ranks <- 0
      metric_df <- ranked_df[grepl(metric, rownames(ranked_df), fixed = T),
                                 grepl(characteristic, colnames(ranked_df), fixed = T)]
      
      for (r in 1:nrow(metric_df)) { for (c in 1:ncol(metric_df)) {
        if(!is.na(metric_df[r, c])) {
          sum_ranks <- sum_ranks + metric_df[r, c]
          num_ranks <- num_ranks + 1
        }
      }}
      out_df[paste0(metric, characteristic), deparse(substitute(ranked_df))] <- sum_ranks/num_ranks
    }
  }
  return(out_df)
}

# summarising row-wise comparison
summary_row_comp_df <- compare_metrics(ranked_row_df, data.frame())
summary_row_comp_df <- compare_metrics(cca_ranked_row_df, summary_row_comp_df)

# ordering results
summary_col_comp_df <- summary_col_comp_df[order(row.names(summary_col_comp_df)),]
summary_row_comp_df <- summary_row_comp_df[order(row.names(summary_row_comp_df)),]

# Viewing the PBI results only
PBI_rank_only <- summary_col_comp_df[grepl('PBI', rownames(summary_col_comp_df), fixed = T), ]



## ---- echo=TRUE----------------------------------------------------------------------------------------------
summary_col_comp_df %>%
  kable(escape = F, align = "c", caption = 'Rank averages for different levels, expected mean = 3') %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  kable_styling(fixed_thead = T) %>%
  column_spec(1, bold = T) %>%
  row_spec(0, bold = T) %>%
  pack_rows("L3 models", 1, 4) %>%
  pack_rows("PBI models", 5, 8) %>%
  pack_rows("T10 models", 9, 12) %>%
  pack_rows("T5 models", 13, 16) %>%
  pack_rows("T8 models", 17, 20)


## ---- echo=TRUE----------------------------------------------------------------------------------------------
PBI_rank_only %>%
  kable(escape = F, align = "c", caption = 'PBI ranks only') %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  kable_styling(fixed_thead = T) %>%
  column_spec(1, bold = T) %>%
  row_spec(0, bold = T)


## ---- echo=TRUE----------------------------------------------------------------------------------------------
summary_row_comp_df %>%
  kable(escape = F, align = "c", caption = 'comparing average ranks of index- vs. attenuation-/gauge-based models, expected mean = 2') %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  kable_styling(fixed_thead = T) %>%
  column_spec(1, bold = T) %>%
  row_spec(0, bold = T) %>%
  pack_rows("Attenuation-based models", 1, 4) %>%
  pack_rows("Gauge-based models", 5, 8) %>%
  pack_rows("Index-based models", 9, 12)

