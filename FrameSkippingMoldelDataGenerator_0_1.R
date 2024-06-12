# DEV NOTES ===================================================================
# This is essentially just a bolt on script for the Balance Analysis script. It
#   piggy backs on many of its functions and is only needed to generate data
#   that have frames removed to model the skipping seen in the Tekscan data.
#   Data files with no skipped frames will be used as the criterion datasets so
#   and new files generated iteratively with increasing numbers of points in the 
#   time series skipped so that analyses can be run to establish errors by
#   number of skipped frames.
#
# Very much under development. Does not run yet
# FOR NEXT VERSION ------------------------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(readr) # read_csv
library(ggplot2)
library(magrittr) # used for pipes

generate_model_data() <- function(){
  # Just pull all settings in for simplicity though most of analysis setting won't be used
  settings <- get_setns() # Get settings
  
  ### Get files to work on ---------
  # While no files in vector or no vector
  got_files <- FALSE # Not got files yet
  while (!got_files) { # keep going till have files
    CoF_data_files <- get_files(settings$file_type , settings$source)
    if (length(CoF_data_files) < 1) {
      if (dlg_message('No files selected. Try again?', 'yesno')$res == 'no') {
        stop('No files to analyse')
      }
    } else {
      got_files = TRUE
    }
  }
  
  ### Iterate through file list ---------
  message('Starting search for files with complete time series...')
  
  # Read in sample rate
    # Already in BA script
  
  # Read in data files
    # Already in BA script
  
  # If nrow in df = max time - min time * sample rate (full data set)
  
    # -> Iteratively generate files with increasing numbers of gaps in the time series
      # Remove row based on distribution modelled on data with dropped frames
      # Already in this script but needs mods
  
      # Save file
      # Already in BA script
  
      # Reached max. number of dropped frames to include?
        # -> repeat iteration
  # -> next file
  
  # 
}
  
  

# Use functions below as a template to analyse errors once the model data sets
# have been generated and passed through BA script
# => error as a function of time gaps for each variable

### Calculate error caused by missing rows ----------
calc_frame_skip_error <- function(row_col){
  p_error <- data.frame( # Initiate path length error df
  n_remove = c(0), # Num rows removed
  path_l = c(0), # resulting path length
  path_l_norm = c(0), # resulting path length error normalised
  path_l_err = c(0), # resulting path length error
  path_l_norm_err = c(0) # resulting path length error normalised error
  )
  original_array_L <- nrow(row_col)
  for (i in 1:500) {
    pl <- calc_path_length(row_col) # calculate path length
    pl_norm <- pl / nrow(row_col) * original_array_L
    p_error <- rbind(
      p_error,
      c(n_remove =  i-1, # First row is correct value
        path_l = pl,
        path_l_norm = pl_norm,
        path_l_err = pl- data$path_l[1],
        path_l_err_norm = pl_norm - data$path_l[1]
         ) # calculate path length error
           
      )
      row_col <- row_col[-c(sample(1:length(row_col),1)),] # delete row at random
  }
  p_error <- p_error[-c(1),] # delete first (initialisation) row
  return(p_error)
}

### Plot data -------
graph_errors <- function(error_data){
  print("Time to graph the data")
  ggplot(error_data, aes(x= n_remove)) +
    geom_line(aes(y = path_l_norm_err)) +
    labs(x= "number of points removed", y = "Normalised path length error")
}
