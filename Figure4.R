# Script to generate Figure 4
# (c)Peter Marquardt, 2021

# making the statistical analysis results available in local environment
knitr::purl('IPRO2_Statistical_analysis.Rmd')  # converting code snippets into a script
source('IPRO2_Statistical_analysis.R')

require(ggplot2)
require(GGally)
require(psych)
require(car)
require(lemon)
require(pBrackets)
require(magick)


# Method for generating dataframes for plotting purposes

create_plotting_rank_df <- function(ranked_col_df, ranked_row_df) {
  plot_row_index <- 1
  out_df <- data.frame()
  # iterating over every possible combination of characteristics
  for (bc_metric in c('index', 'atten', 'gauge')){
    for(model_nm in names(atten_models)){
      for (level in names(atten_models[['OS']])){
        for (model_metric in c('n', 'C', 'R^2', 'p')){
          
          #calculating stuff
          row_nm <- paste(model_nm, bc_metric, sep='_')
          col_nm <- paste(level, model_metric, sep='_')
          bc_rank <- ranked_row_df[row_nm, col_nm]
          level_rank <- ranked_col_df[row_nm, col_nm]
          
          # assigning as single rows in df (partially redundant)
          out_df[plot_row_index, 'index'] <- plot_row_index
          out_df[plot_row_index, 'row_name'] <- row_nm
          out_df[plot_row_index, 'col_name'] <- col_nm
          out_df[plot_row_index, 'bc_metric'] <- bc_metric
          out_df[plot_row_index, 'model_nm'] <- model_nm
          out_df[plot_row_index, 'level'] <- level
          out_df[plot_row_index, 'model_metric'] <- model_metric
          out_df[plot_row_index, 'bc_rank'] <- bc_rank
          out_df[plot_row_index, 'level_rank'] <- level_rank
          
          plot_row_index <- plot_row_index + 1  # next row
        }
      }
    }
  }
  return(out_df)
}

# creating dfs for plotting
rank_plot_df <- create_plotting_rank_df(ranked_col_df, ranked_row_df)
cca_rank_plot_df <- create_plotting_rank_df(cca_ranked_col_df, cca_ranked_row_df)

# removing all unnecessary memory-occupying stuff from the environment
removal_list <- c()
for (var in ls()){
  if (! var %in% c('ranked_row_df', 'ranked_col_df',
                   'cca_ranked_row_df', 'cca_ranked_col_df',
                   'df', 'comp_row_names',
                   'rank_plot_df', 'cca_rank_plot_df')) {
    removal_list <- append(removal_list, var)
  }
}
rm(list = removal_list)



# having the names ready
row_order <- comp_row_names
col_order <- c("PBI_C", "PBI_n", "PBI_R^2", "PBI_p", "L3_C", "L3_n", "L3_R^2", "L3_p",
               "T10_C", "T10_n", "T10_R^2", "T10_p","T8_C", "T8_n", "T8_R^2", "T8_p", "T5_C", "T5_n", "T5_R^2", "T5_p")
matrix_row_names <- rep(c('C', 'n', 'R²', 'p'), 5)
matrix_col_names <- rep(c('OS', 'RD', 'RD90', 'LOS', 'PHQ4D', 'PHQ4A', 'ESASt', 'ESASp'), 3)

# Main graph, but just circles
png('Comparison_matrix_horizontal_blank_circles.png', width = 4096, height = 3000)
ggplot(rank_plot_df, aes(y = factor(col_name, level = col_order), x = factor(row_name, level = row_order))) +
  geom_point(aes(colour=level_rank, size= factor(bc_rank)), shape=16) +
  scale_color_gradient(low="#10EE10", high="#202020", name = 'Rank of level', guide = guide_colourbar(reverse = T)) +
  scale_size_discrete(range = c(50,20), name = 'Rank of SM metric', breaks = c(1,2,3)) +
  scale_x_discrete(labels= matrix_col_names) +
  scale_y_discrete(labels= matrix_row_names) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 80) +
  theme(legend.key.size = unit(10,"line")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "White", colour = "White", size = 2, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  ) +
  labs(title = element_blank(),
       y = element_blank(),
       x = element_blank())


#column brackets
grid.brackets(1205, 2840, 195, 2840, lwd=8, col="black")
grid.brackets(2220, 2840, 1210, 2840, lwd=8, col="black")
grid.brackets(3240, 2840, 2225, 2840, lwd=8, col="black")
# row brackets
grid.brackets(110, 2685, 110, 2175, lwd=8, col="black", h=0.034)
grid.brackets(110, 2170, 110, 1660, lwd=8, col="black", h=0.034)
grid.brackets(110, 1655, 110, 1145, lwd=8, col="black", h=0.034)
grid.brackets(110, 1140, 110, 630, lwd=8, col="black", h=0.034)
grid.brackets(110, 625, 110, 115, lwd=8, col="black", h=0.034)
dev.off()


#Adding surrounding annotations ####

# adding labels separately using magick
matrix_blank <- image_read('Comparison_matrix_horizontal_blank_circles.png')
homunculus <- image_trim(image_read('homunculus_labelled_transparent.png'))

matrix_file <- tempfile()
# adding column labels and title
matrix_annotated <- matrix_blank %>%
  image_border(color='white', geometry = '2000x200') %>%
  image_annotate("SMI", size = 100, color = "black", location = "+2605+3200") %>%
  image_annotate("SMRA", size = 100, color = "black", location = "+3570+3200") %>%
  image_annotate("SMG", size = 100, color = "black", location = "+4620+3200") %>%
  image_annotate("Comparison of skeletal muscle metrics and vertebral levels",
                 size = 128, color = "black", location = "+1900+100")

matrix_annotated

image_write(matrix_annotated, path = matrix_file, format = 'tiff')

# adding row labels
matrix_annotated2 <- image_read(matrix_file)%>%
  image_annotate("T5", size = 100, color = "#a51c30", location = "+1910+630", degrees = 270) %>%
  image_annotate("T8", size = 100, color = "#8390fa", location = "+1910+1150", degrees = 270) %>%
  image_annotate("T10", size = 100, color = "#ca895f", location = "+1910+1690", degrees = 270) %>%
  image_annotate("L3", size = 100, color = "#23297a", location = "+1910+2175", degrees = 270) %>%
  image_annotate("PBA", size = 100, color = "#96c0b7", location = "+1910+2715", degrees = 270)

matrix_annotated2

image_write(matrix_annotated2, path = matrix_file, format = 'tiff')



matrix_annotated_temp <- image_read(matrix_file)


composite <- matrix_annotated_temp %>%
  image_border(color='white', geometry = '300x0') %>%
  image_composite(image_scale(homunculus, "2150"), offset = "+0+280") %>%
  image_crop("6396x3400+0")

image_write(composite, path = 'Matrix_composite.png', format = 'png')

composite2 <- image_read('Matrix_composite.png') %>%
  image_annotate("T5", size = 50, color = "#a51c30", location = "+200+1080") %>%
  image_annotate("T8", size = 50, color = "#8390fa", location = "+200+1405") %>%
  image_annotate("T10", size = 50, color = "#ca895f", location = "+215+1625") %>%
  image_annotate("L3", size = 50, color = "#23297a", location = "+200+2190")

composite2

image_write(composite2, path = 'Figure4.png', format = 'png')
