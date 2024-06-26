# DEV NOTES ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This is essentially just a bolt on script for the Balance Analysis script. It
#   piggy backs on many of its functions and is only needed to generate data
#   that have frames removed to model the skipping seen in the Tekscan data.
#   Data files with no skipped frames will be used as the criterion datasets so
#   and new files generated iteratively with increasing numbers of points in the 
#   time series skipped so that analyses can be run to establish errors by
#   number of skipped frames.
#
# There are essentially 2 parts to this script;
#   1) Generate the model data from criterion data sets
#   2) Conduct analysis of the errors as a function of the number of frames 
#     removed in the model data
# 
# Done so far ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PART 1: Generate Model Data
# * Searches files for those with no skipped frame and then uses these as a
#     criterion data set and generates model data sets from these with 
#     iteratively more frames removed
#
# PART 2: Analyse Errors
# * Calculates the errors between the criterion data sets and their derived 
#     files with frames removed (coding ain't pretty though!)
# * Calculates % errors
# * Plot mean/ SD error against number of frames removed for each variable
# [Done] Export considerably more data and use more realistic (larger) number of
#   dropped frames for model data. This will give indication of;
#   1) whether the current plots are sensible for the data [No]
#   2) what regression model to use
# [TODO] Determine the regression model to be used. Is linear sufficient?
# [TODO] Calculate regression formulae for error by number of missing frames for
#     each variable grouped by interp == T/F
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(readr) # read_csv
library(reshape2) # Used for reshaping df before plotting
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
          # remove_times <- round(rnorm(gap_max * 2, mean = gap_mean, sd = gap_sd), 2) %>%
            # .[. >= settings$start_t & . <= settings$end_t] # Remove values outside of time window
          
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
            matcht <- FALSE
            while(matcht == FALSE){
              t <- round(rnorm(1, mean = gap_mean, sd = gap_sd), 2) # Generate pseudo-random time
              if(nrow(subset(CoF_data, Time == t) ) == 1){ # found matching time
                CoF_data <- subset(CoF_data, Time != t) # Remove row
                matcht <- TRUE
              }
            }
          }
        }
      }
    }
  }
  Print("Finished generating model files")
        
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
    if(length(list.files(path = d) ) > 0) { # There are already files in folder
      if (dlg_message(
        "Warning! There are already files in that folder. These may be overwritten.\n\nDo you want to choose another folder?", 
                      'yesno')$res == 'yes') { d <- NA }
    }
  }
  return(d)
}

# ERROR MODEL ANALYSIS #########################################################

run_err_analysis <- function(x){
  # Return error calcs and plots as global variable so available for further analysis/ mods
  print("Calculating errors....")
  assign("err", calc_frame_skip_error(x), envir = .GlobalEnv) 
  print("Plotting errors....")
  assign("err_plots", plot_errors(err), envir = .GlobalEnv) 
  show(err_plots) # Show the plots
  print("Calculating regression formulae....")
  # first version using interp value (would need to be within for loop)
  # err %>% 
  #   group_by(Interp) %>% 
  #   apply(.[ , 18:26], 2, calc_regr, x = .$missn_frames)
  # # apply(err[ , 18:26], 2, calc_regr, x = err$missn_frames, s = err$Interp, s_val = T)
  
  # 2nd version using grouping
  # err %>% 
  # {if(length(unique(.$Interp)) > 1) group_by(., Interp) else .} %>%
  #   group_modify(~ apply(err[ , 18:26], 2, calc_regr, x = err$missn_frames) )
}

## Calculate Error Caused by Missing Rows ======================================
calc_frame_skip_error <- function(x){
  # This works but is a bit of a kludge!
  # Also finds differences between criterion and skp trial trials but since
  # trial is numeric and crit trial = 0, this doesn't matter (I did say it was a
  # kludge!)
  df <- data.frame() # Create empty data frame
    inf_colms <- c(1:3, 5:7) # Columns with file info
    gap_colms <- c(17:28) # Columns with frame gap data
    
  # remove No frame removed count from file names, i.e. just original name
  x$file_name <- substr(x$file_name, 1, nchar(x$file_name) - 4 ) 
  for (flname in unique(x$file_name) ){ # For each file name in df 
    print(paste("Processing", basename(flname), "....") )
    crit <- subset(x, file_name == flname & as.integer(trial) == 0) # Get the criterion data set for that file name
    skp <- subset(x, file_name == flname & as.integer(trial) != 0) # Get the data sets with frames removed for that file name
    errs <- apply(skp[ , -c(inf_colms, gap_colms)], 1, subtract, crit[ , -c(inf_colms, gap_colms)]) # subtract the criterion values from the skp values
    errs <- as.data.frame(do.call(rbind, errs)) # Construct into df
    perc_errs <- apply(errs, 1, divide_by, crit[ , -c(inf_colms, gap_colms)]) # subtract the criterion values from the skp values
    perc_errs <- as.data.frame(do.call(rbind, perc_errs)) # Construct into df
    perc_errs <- perc_errs[-1] * 100 # Strip out No frames column and convert to %
    names(perc_errs) <- paste(names(perc_errs), "_%", sep="")  # Append % column names
    errs$No_frames <- errs$No_frames * -1 # Invert differences, i.e. make No frames removed
    # Reinstate columns not included in diff calcs, drop row names
    err_df <- cbind(subset(x, as.integer(trial) != 0)[inf_colms], errs, perc_errs, row.names = NULL) %>% 
      rbind(df, .)
  }
  return(err_df)
}
  
## Plot Errors =================================================================  
plot_errors <- function(df){
  # Form all % error variables into a single column
  dfm <-  melt(df[c(3, 6, 7, 17:25)], id.vars = c("Interp", "No_frames", "trial"))
  
  ggplot(data = dfm, mapping = aes(x = No_frames, y = value, colour = variable)) + 
    geom_point() +
    geom_smooth(formula = y ~ x, method = "lm") +
    labs(x = "number of frames removed", y = "% error") +
    facet_grid(cols = vars(Interp))
}

## Calculate Regression Lines ==================================================
# See https://www.statology.org/polynomial-regression-r/
calc_regr <- function(x, y) { #}, s, s_val){
  lm(x ~ y) %>%  #, subset = s == s_val)
    as.data.frame()
}
#  
#   gap_data <- subset(model_data, trial != 0) %>% 
#     group_by(file_name) #%>% 
#     # print()
#     browser()
#     apply(gap_data[, -1:-3], 2, sum)
#   # Value = value - value from above
#     
#     # subset(model_data, (trial == 0, file_name == ))
#   
#   # Then do calculations on errors NOT original data
#   model_data %>% 
#     group_by(file_name) %>% 
#     group_by(Interp) %>% 
#     group_by(trial) %>% # trial = number of frames removed
#     summarise(n = n(), meanCoFxRange = mean(CoF_x_Range), SDCoFxRange = sd(CoF_x_Range))
#   
# }