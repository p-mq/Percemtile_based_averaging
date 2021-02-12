# Script to generate Figure 3
# (c)Peter Marquardt, 2021

# making the statistical analysis results available in local environment
source('Get_IPRO_data.R')
df <- Get_IPRO_data()
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



# custom version of GGally package
install.packages("C:/Users/Peter/Documents/R/bodycomp-analyses/IPRO2/GGallyPMQmod_2.1.0.zip", repos = NULL)

# libraries
require(tidyverse)
require(GGallyPMQmod)

# shorthand for column names
muscle_cols <- c('perc_T5_Muscle_index',
                 'perc_T8_Muscle_index',
                 'perc_T10_Muscle_index',
                 'perc_L3_Muscle_index',
                 'average_perc_Muscle_index',
                 'perc_T5_Muscle_attenuation',
                 'perc_T8_Muscle_attenuation',
                 'perc_T10_Muscle_attenuation',
                 'perc_L3_Muscle_attenuation',
                 'average_perc_Muscle_attenuation')

muscle_col_newnames <- c('T5\nSMIp',
                         'T8\nSMIp',
                         'T10\nSMIp',
                         'L3\nSMIp',
                         'mean\nSMIp',
                         'T5\nSMRAp',
                         'T8\nSMRAp',
                         'T10\nSMRAp',
                         'L3\nSMRAp',
                         'mean\nSMRAp')


# custom function for lower-left panels (dotplots)
lower_fn <- function(data, mapping, dotsize, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "black", size=dotsize) +
    geom_smooth(method = method, color = "red", ...) +
    scale_x_continuous(breaks = seq(0, 100, 25)) +
    scale_y_continuous(breaks = seq(0, 100, 25)) +
    theme(panel.grid.minor = element_blank())
  p
}


# Defines function to color according to correlation
upper_fn <- function(data, mapping, method, symbol, ...){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor(x, y, method=method, use='complete.obs')
  colFn <- colorRampPalette(c("#000000", "#202020", "#10EE10"), 
                            interpolate ='spline')
  fill <- colFn(100)[findInterval(corr, seq(-1, 1, length = 100))]
  
  ggally_text(
    label = paste(symbol, as.character(round(corr, 2))), 
    mapping = aes(),
    xP = 0.5, yP = 0.5,
    color = 'black',
    ...
  ) + #removed theme_void()
    theme(panel.background = element_rect(fill = fill)) +
    theme(strip.background = element_rect(fill = "white"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}


# The actual figure
png('Figure_3.png', width = 4096, height = 4096)
f3 <- GGallyPMQmod::ggpairs(df[, muscle_cols] %>%
          rename_at(vars(muscle_cols), ~muscle_col_newnames),
        mapping = aes(alpha=1),
        axisLabels = 'internal',
        upper = list(continuous = wrap(upper_fn,method = 'pearson', symbol = 'r =', size = 35)),
        lower = list(continuous = wrap(lower_fn, method = "lm", dotsize=3, size=4, se=FALSE)),
        diag = list(continuous = "blankDiag")) +
      theme(strip.background = element_rect(fill = "white"))
f3

dev.off()


remove.packages('GGallyPMQmod')


