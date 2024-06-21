# DEV NOTES ===================================================================
# This is essentially just a bolt on script for the Balance Analysis script. It
#   piggy backs on many of its functions and is only needed to generate data
#   that have frames removed to model the skipping seen in the Tekscan data.
#   Data files with no skipped frames will be used as the criterion datasets so
#   and new files generated iteratively with increasing numbers of points in the 
#   time series skipped so that analyses can be run to establish errors by
#   number of skipped frames.
#
# * Searches files for those with no skipped frame and then uses these as a
#     criterion data set and generates model data sets from these with 
#     iteratively more frames removed
# * Calculates the errors between the criterion data sets and their derived 
#     files with frames removed (coding ain't pretty though!)
# [TODO] Calculate % errors
# [TODO] Plot mean/ SD error against number of frames removed for each variable
# [TODO] Calculate regression formulae for error by number of missing frames for
#     each variable
# FOR NEXT VERSION ------------------------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(readr) # read_csv
library(ggplot2)
library(magrittr) # used for pipes

# GENERATE MODEL DATA ==========================================================
generate_model_data <- function(gap_mean, gap_sd, gap_max){
  # Just pull all settings in for simplicity though most of analysis setting won't be used
  settings <- get_setns() # Get settings
  file_list <- get_data_files(settings$file_type)
  save_dir <- get_dir()
  ## Remove incomplete files ===================================================
  message('Starting search for files with complete time series...')
  for (file_i in file_list) { # for each file in list
    if (length(file_list) < 1) {
      dlg_message("All files selected have missing frames, unable to proceed.", type = "ok")
      break
    }
    ### Clean file name for labelling -----
    # Remove file type
    clean_file_name <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(file_i) )
    # Replace white space
    clean_file_name <- gsub("\\s", "_", clean_file_name)
    file_inf <- list(file_name = clean_file_name) # Start file info list with file name
    
    ### Read data file ----------------
    if (file.access(file_i, mode = 4) == 0) { # If can read from file
      CoF_data <- read_txt_data(
        file_i,
        delm = settings$delim,
        skp = settings$skip,
        maxn = settings$maxn, 
        c_type = as.character(settings$colm_type),
        c_names = as.character(
          strsplit(settings$colm_names, ",")[[1]] # Split string to vector by commas
        )
      )
      if (is.null(CoF_data) ) { # Failed to read data from file
        print(paste(clean_file_name, "Unable to read file. Eliminating from list "))
        file_list <- file_list[! file_list %in% file_i]
      } else { # Read file OK- carry on
        
        ### Read sample rate -----------------
        sr <- read_sample_rate(file_i, settings$sr_skp, settings$sr_colm) # Read in sample rate
        
        ### Check if file has missing rows -------------------------------------
        if (nrow(CoF_data) != (max(CoF_data$Time) - min(CoF_data$Time)) * sr  + 1) {
          print(paste(clean_file_name, "has missing rows. Skip"))
        } else {
          print(paste("Generating data sets from", clean_file_name))
          
          ### Generate list of times to remove at random -----------------------
          # Set seed based on millisecond system time so won't always remove the same times
          set.seed(as.numeric(substr(format(Sys.time(), "%OS6"), 4, 10)))
          remove_times <- round(rnorm(100 * 2, mean = gap_mean, sd = gap_sd), 2) %>%
            .[. >= settings$start_t & . <= settings$end_t] # Remove values outside of time window
          
          ### Make duplicate files with rows removed ---------------------------
          CoF_data$Time <- round(CoF_data$Time, 2) # Round to 2dp as sample rate not exact
          for (g in 0:gap_max){
            flname <- paste(
              save_dir, "/",clean_file_name, 
              "_", formatC(g, width = 3, flag = "0"), 
              ".csv", 
              sep = ""
              )
            write_excel_csv(as.data.frame(sr), flname)
            write_excel_csv(CoF_data, flname, append = T, col_names = T)
            CoF_data <- subset(CoF_data, Time != remove_times[g + 1]) # Remove row
          }
        }
      }
    }
  }
        
}


## Get files to work on ========================================================
get_data_files <- function(ftype){
  # While no files in vector or no vector
  got_files <- FALSE # Not got files yet
  while (!got_files) { # keep going till have files
    files <- get_files(ftype)
    if (length(files) < 1) {
      if (dlg_message('No files selected. Try again?', 'yesno')$res == 'no') {
        stop('No files to analyse')
      }
    } else {
      got_files = TRUE
    }
  }
  return(files)
}
  
## Get Directory To Save To ========================================================
get_dir <- function(){
  d <- NA # Start with set to value if Cancel pressed
  while (is.na(d)) { # keep going till have dir
    d <- rstudioapi::selectDirectory(caption = "Choose dir to save model files to. This should be empty")
    if (is.na(d)) {
      if (dlg_message('No folder selected. Try again?', 'yesno')$res == 'no') {
        stop('No folder to save to')
      }
    }
    if(length(list.files(path = d, all.files = T) ) > 0) { # There are already files in folder
      if (dlg_message(
        "Warning! There are already files in that folder. These may be overwritten.\n\nDo you want to choose another folder?", 
                      'yesno')$res == 'yes') { d <- NA }
    }
  }
  return(d)
}

# ERROR MODEL ANALYSIS =========================================================


### Calculate error caused by missing rows ----------
calc_frame_skip_error <- function(x){
  # This works but is a bit of a kludge!
  # Also finds differences between criterion and skp trial trials but since
  # trial is numeric and crit trial = 0, this doesn't matter (I did say it was a
  # kludge!)
  df <- data.frame() # Create empty data frame
  for (flname in unique(x$file_name)){ # For each file name in df
    crit <- subset(x, file_name == flname & trial == 0)[ , -c(1:3, 5:7)] # Get the criterion data set for that file name
    skp <- subset(x, file_name == flname & trial != 0)[ , -c(1:3, 5:7)] # Get the data sets with frames removed for that file name
    z <- apply(skp, 1, subtract, crit) # subtract the criterion values from the skp values
    for (i in 1:length(z)){ # Put it all together into a df
      df <- rbind(df, c(filename = flname, z[[i]]))
    }
  }
  return(df)
}
  
  
  
  
  
  
  
  
  
  
  criterion_data <- subset(model_data, trial == 0) 

  for (i in unique(gap_data$file_name)){
    # >>>>>>>>>>> subtract row file_name == criterion_data$file_name
    #apply(gap_data[, -1:-3], 2, sum)
    
  }
  
  y[1, c(-1:-5, -7)] - z[1, c(-1:-5, -7)]
  # filter by trial != 0
  gap_data <- subset(model_data, trial != 0) %>% 
    group_by(file_name) #%>% 
    # print()
    browser()
    apply(gap_data[, -1:-3], 2, sum)
  # Value = value - value from above
    
    # subset(model_data, (trial == 0, file_name == ))
  
  # Then do calculations on errors NOT original data
  model_data %>% 
    group_by(file_name) %>% 
    group_by(Interp) %>% 
    group_by(trial) %>% # trial = number of frames removed
    summarise(n = n(), meanCoFxRange = mean(CoF_x_Range), SDCoFxRange = sd(CoF_x_Range))
  
}

### Plot data -------
graph_errors <- function(error_data){
  print("Time to graph the data")
  ggplot(error_data, aes(x= n_remove)) +
    geom_line(aes(y = path_l_norm_err)) +
    labs(x= "number of points removed", y = "Normalised path length error")
}
