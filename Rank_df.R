# Script to rank characteristics in a table
# Designed for the IPRO2 model comparison

require(dplyr)

# function to rank items/levels in a dataframe
rank_df_cols <- function(df, items, levels, rank_by, hi_to_lo=TRUE){
  
  # assertions
  stopifnot(rank_by %in% c('items', 'levels'))
  
  # preparing generic variables to use generic nested loop
  if (rank_by == 'items') {
    ranked_vars <- items
    unranked_vars <- levels
    paste_in_order <- TRUE
  }
  else {
    ranked_vars <- levels
    unranked_vars <- items
    paste_in_order <- FALSE
  }
  
  shadow_df <- data.frame()  # initialise shadow df we'll return

  # Loop over all rows, get a ranking of all characteristics for each
  for (r in 1:nrow(df)) {
    
    # Loop over all characteristics we want to compare
    for (i in 1:length(unranked_vars)){
    
      comp_df <- data.frame() # initialising df for this specific comparison
      
      # copying all necessary values for the comparison
      for (j in 1:length(ranked_vars)){
        # reconstructing name of column we're currently at
        col_nm <- ifelse(paste_in_order, paste(items[j], levels[i], sep = ''), paste(items[i], levels[j], sep = ''))
        comp_df[col_nm, 'value'] <- df[r, col_nm] # copying value
        comp_df[col_nm, 'name'] <- col_nm  # sorting with dplyr::arrange destroys named indices
      }
      
      # ranking values
      if(hi_to_lo){
        comp_df <- comp_df %>%  # ordering values hight to low
          arrange(desc(value))
      }
      else{
        comp_df <- comp_df %>%  # ordering by values low to high
        arrange(value)
      }
      
      
      for(comp_r in 1:nrow(comp_df)){  # assign ranks to shadow df
        
        # allow for tied ranking
        if (comp_r == 1 || ((comp_df[comp_r, 'value']) != (comp_df[(comp_r - 1), 'value']))) { # not tied
          shadow_df[r, comp_df[comp_r, 'name']] <- comp_r  # assigning row number as rank (default)
        }
        else{  # we have a tie! Assign the last assigned rank (allows multiple ties)
          shadow_df[r, comp_df[comp_r, 'name']] <- shadow_df[r, comp_df[(comp_r - 1), 'name']]
        }
      }
      
    }
  }
  
  rownames(shadow_df) <- rownames(df)
  return(shadow_df)
}






# function to rank items/levels in a dataframe
rank_df_rows <- function(df, items, levels, rank_by, hi_to_lo=TRUE){
  
  # assertions
  stopifnot(rank_by %in% c('items', 'levels'))
  
  # preparing generic variables to use generic nested loop
  if (rank_by == 'items') {
    ranked_vars <- items
    unranked_vars <- levels
    paste_in_order <- TRUE
  }
  else {
    ranked_vars <- levels
    unranked_vars <- items
    paste_in_order <- FALSE
  }
  
  shadow_df <- data.frame()  # initialise shadow df we'll return
  
  # Loop over all columns, get a ranking of all characteristics for each
  for (c in 1:ncol(df)) {
    
    # Loop over all characteristics we want to compare
    for (i in 1:length(unranked_vars)){
      
      comp_df <- data.frame() # initialising df for this specific comparison
      
      # copying all necessary values for the comparison. Note that we switch dimensions
      for (j in 1:length(ranked_vars)){
        # reconstructing name of column we're currently at
        row_nm <- ifelse(paste_in_order, paste(items[j], levels[i], sep = ''), paste(items[i], levels[j], sep = ''))
        comp_df[row_nm, 'value'] <- df[row_nm, c] # copying value
        comp_df[row_nm, 'name'] <- row_nm  # sorting with dplyr::arrange destroys named indices
      }
      
      # ranking values
      if(hi_to_lo){
        comp_df <- comp_df %>%  # ordering values hight to low
          arrange(desc(value))
      }
      else{
        comp_df <- comp_df %>%  # ordering by values low to high
          arrange(value)
      }
      
      
      for(comp_r in 1:nrow(comp_df)){  # assign ranks to shadow df
        
        # allow for tied ranking
        if (comp_r == 1 || ((comp_df[comp_r, 'value']) != (comp_df[(comp_r - 1), 'value']))) { # not tied
          shadow_df[comp_df[comp_r, 'name'], c] <- comp_r  # assigning row number as rank (default)
        }
        else{  # we have a tie! Assign the last assigned rank (allows multiple ties)
          shadow_df[comp_df[comp_r, 'name'], c] <- shadow_df[comp_df[(comp_r - 1), 'name'], c]
        }
      }
      
    }
  }
  
  colnames(shadow_df) <- colnames(df)
  return(shadow_df)
}









