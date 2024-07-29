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
# * Remove blocks of frames rather than always single as skipped frames are
#   clustered in the Tekscan data. This is based on gap counts from files used 
#   for generating gap profiles.
#
# PART 2: Analyse Errors
# * Calculates the errors between the criterion data sets and their derived 
#     files with frames removed (coding ain't pretty though!)
# * Calculates % errors
# * Plot mean/ SD error against number of frames removed for each variable
# * Plots modified to return path length, range and ellipse as separate plots
# * Calculate regression formulae for error by number of missing frames for
#     each variable grouped by interp == T/F
#   - Output these as model predictions (fit & CI), and model parameters (coeff, 
#     R^2, SEE)
# * Regression analyses modified to include both interpolated and non-
#       interpolated data in the model outputs
# * Mean and median errors added to file containing the model predictions fit & 
#     CIs
# TO DO ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(readr) # read_csv
library(reshape2) # Used for reshaping df before plotting
library(ggplot2)
library(magrittr) # used for pipes
library(dplyr)

source("/home/jon/Git/BalanceAnalysis/BalanceAnalysis_v1_6_1.R")

# GENERATE MODEL DATA ==========================================================
generate_model_data <- function(gpmean = gap_mean, gpsd = gap_sd, gpmax = gap_max, gpcounts = gap_counts, analspecfile = '/SettingsFiles/AnalysisSpec.csv'){
  # Just pull all settings in for simplicity though most of analysis setting won't be used
  settings <- get_setns(analspecfile) # Get settings
  file_list <- get_data_files(settings$file_type)
  save_dir <- get_dir()
  ## Remove incomplete files ===================================================
  message('Starting search for files with complete time series...')
  for (file_i in file_list) { # for each file in list
    if (length(file_list) < 1) {
      dlg_message("All files selected have missing frames, unable to proceed.", type = "ok")
      break
    }
  ### Clean file name for labelling --------------------------------------------
    # Remove file type
    clean_file_name <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(file_i) )
    # Replace white space
    clean_file_name <- gsub("\\s", "_", clean_file_name)
    file_inf <- list(file_name = clean_file_name) # Start file info list with file name

  ### Read data file -----------------------------------------------------------
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
        
        ### Read sample rate ---------------------------------------------------
        sr <- read_header_info(file_i, settings$sr_skp, settings$sr_colm) # Read in sample rate

        ### Check if file has missing rows -------------------------------------
        if (nrow(CoF_data) != (max(CoF_data$Time) - min(CoF_data$Time)) * sr  + 1) {
          print(paste(clean_file_name, "has missing rows. Skip"))
        } else {
          print(paste("Generating data sets from", clean_file_name))
          crit_row <- nrow(CoF_data) # record number of rows present to start with

          ### Generate list of times to remove at random -----------------------
          # Set seed based on millisecond system time so won't always remove the same times
          set.seed(as.numeric(substr(format(Sys.time(), "%OS6"), 4, 10)))

          ### Make duplicate files with rows removed ---------------------------
          CoF_data$Time <- round(CoF_data$Time, 2) # Round to 2dp as sample rate not exact
          while (crit_row - nrow(CoF_data) < gpmax) {
            flname <- paste(
              save_dir, "/",clean_file_name,
              "_", formatC(crit_row - nrow(CoF_data), width = nchar(gpmax), flag = "0"),
              ".csv",
              sep = ""
              )
            write_excel_csv(as.data.frame(sr), flname)
          
              write_excel_csv(CoF_data, flname, append = T, col_names = T)
            matcht <- FALSE
            while(matcht == FALSE){ # Keep going till find a time that exists in the file
              begint <- round(rnorm(1, mean = gpmean, sd = gpsd), 2) # Generate pseudo-random time
              if(nrow(subset(CoF_data, Time == begint) ) == 1){ # found matching time
                # Set the cut end time to...
                # the frame gap corresponding to the lowest cumulative sum that is greater than the random number chosen...
                endt <- subset(gpcounts, CumulativeCount > sample(1:(max(gpcounts$CumulativeCount) - 1), 1))[, "GapSize"][1] %>%
                  subtract(1) %>% # subtract 1 since frame gap of 2 means 1 frame removed
                  multiply_by(1 / sr) %>% # multiplied by the sample rate..
                  add(begint) # plus the cut start time
                CoF_data <- subset(CoF_data, Time < begint | Time >= endt) # Remove row(s)
                matcht <- TRUE
              }
            }
          }
        }
      }
    }
  }
  print(">>>>>>> Finished generating model files <<<<<<<")
        
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
  
  print("Initiatiing error calculations")
  err <- calc_frame_skip_error(x)
  dir_used <- # Use directory on first save as default for remainder just to save a bit of faff
    write_to_file(err, dflt = "ErrorData.csv", ttl = "Save error data as") %>% 
    dirname() 
  
  print("Plotting errors....")
  plot_errors(err)
  print("Error plots created as 'plot_Range', 'plot_pl' and 'plotEllipse'")
  
  print("Calculating regression formulae....")
  write_to_file(as.data.frame(do_regr(err)), dflt = paste(dir_used, "ModelParameters.csv", sep = "/"), ttl = "Save model parameters as")
  
  print("Calculating predictions from model and average errors....")
  cbind(
      do_pred_vals(err), # model fit and CIs
      cbind(
        do_mean_median(err), # mean and medians 
        do_n_by_nFrames(err) # number of data sets with given number of frames used for mean/ median calcs
        ) %>% 
        sort_by(list(.$Interp, .$No_frames)) %>%  # match mean/ median/ n sort order to predicted vals
        select(!c("Interp", "No_frames"))  # Do not copy the grouping columns from mean, median, n (already in pred values)
  ) %>% 
    select(order(colnames(.))) %>%  # Do not copy the grouping columns from mean, median, n (already in pred values)
    relocate(c(No_frames, Interp, n)) %>% # Move grouping and n columns back to start
    write_to_file(dflt = paste(dir_used, "ModelPredictions.csv", sep = "/"), ttl = "Save model predictions as")
  print(">>>>>>> Error analysis complete <<<<<<<<<")
}

## Calculate Error Caused by Missing Rows ======================================
calc_frame_skip_error <- function(x){
  # This works but is a bit of a kludge!
  # Also finds differences between criterion and skp trial trials but since
  # trial is numeric and crit trial = 0, this doesn't matter (I did say it was a
  # kludge!)
  err_df <- data.frame() # Create empty data frame
    inf_colms <- c(1:3, 5:7) # Columns with file info
    gap_colms <- c(
      which(colnames(model_data) %in% colnames(select(model_data, matches("X[[:digit:]]")))),   # Columns with frame gap data
      which(colnames(model_data) %in% c("FrameGaps"))  # Frame gap label column
    )
    
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
    err_df <- cbind(errs, perc_errs, row.names = NULL) %>% 
      rbind(err_df, .)
  }
    cbind(subset(x, as.integer(trial) != 0)[inf_colms], err_df)
}
  
## Plot Errors =================================================================  
plot_errors <- function(df){
  idc <- c("Interp", "No_frames", "trial") # list of id columns
  
  # melt all % error columns into a single column with idc as ID columns
  dfm <-  names(df)[grepl("_%", names(df))] %>% 
    c(., idc) %>% 
    df[.] %>% 
    melt(., id.vars = idc)
  
  for (i in c("_Range", "_pl", "Ellipse")){
    p <- 
      dfm %>% 
      filter(grepl(i, variable)) %>%
      ggplot(data = ., mapping = aes(x = No_frames, y = value, colour = variable)) +
        geom_point() +
        geom_smooth(formula = y ~ x, method = "lm") +
        labs(x = "number of frames removed", y = "% error") +
        facet_grid(Interp ~ variable)
      show(p)
      assign(paste("plot", i, sep = ""), p, envir = .GlobalEnv)
  }
}

## Calculate Regression Values From Lm ==========================================
calc_regr <- function(x, y){
  summary(lm_xy <- lm(y ~ x -1)) %>% 
    .$coefficients %>% 
    as.data.frame() %>% 
    cbind(., 
          r_sqrd = summary(lm_xy)$r.squared, 
          Residual_SE = summary(lm_xy)$sigma
    ) %>% 
    t()
}

## Do Regression Values Calcs ===================================================
do_regr <- function(err){
  err %>%
    reframe(
      across(c(16:24), calc_regr, x = No_frames, .unpack = TRUE), # Column numbers -1 since it removed .by column in count
      .by = Interp
      ) %>% 
    apply(2, cbind) %>% # unpack nested df
    cbind(Param = c("Coeff", "Std_Error", "t_value", "Pr(>|t|)", "r_sqrd", "Residual_SE"), .)  # Add No_frames as a column
}


## Calculate Predicted Values From Lm ==========================================
calc_regr_pred <- function(x, y, new){
  lm_p <- lm(y ~ x -1) %>% 
    predict(newdata = new, interval = 'confidence') 
}

## Do Predicted Values Calcs ===================================================
do_pred_vals <- function(err){
  mdl_data <- data.frame(x = c(1:max(err$No_frames)))
  err %>% 
    reframe(
      across(c(16:24), calc_regr_pred, x = No_frames, mdl_data, .unpack = TRUE), 
      .by = Interp
      ) %>% 
    apply(2, cbind) %>% # unpack nested df
    cbind(setNames(mdl_data, "No_frames"), .) %>%  # Add No_frames as a column
    set_colnames(., sub("[.]{2}", "", colnames(.))) %>% # Tidy up column names
    set_colnames(., sub("[.]1", "_LMfit", colnames(.))) %>% # relabel .[1..2 columns]
    set_colnames(., sub("[.]2", "_LMlwr", colnames(.))) %>% # relabel .[1..2 columns]
    set_colnames(., sub("[.]3", "_LMupr", colnames(.)))  # relabel .[1..2 columns]
}

## Calculate Reg for all =======================================================
regr_mdl <- function(err){
  err %>% 
    group_by(Interp) %>% 
    summarise(mean = mean(CoF_y_pl))
    
    r <- sapply(err[8:16], 2, calc_regr, x = err$No_frames)
    for (i in 1:length(r)){
      cat("Summary of linear model regression for: ", names(r)[i], "\n", rep("-", 55), "\n", sep = "")
      print(summary( r[[i]]) )
    }
}

## Mean and Median errors =======================================================
mean_med <- function(x){
  tibble(
    meanError = mean(x),
    medianError = median(x)
  )
}    

## Do Mean and Median errors ===================================================
do_mean_median <- function(x){
  x %>% 
    reframe(
      across(c(15:23), mean_med, .unpack = TRUE), 
      .by = c(Interp, No_frames)
    ) #%>% 
    # sort_by(.$Interp)
}

## Do n by No frames ===========================================================
do_n_by_nFrames <- function(x){
  x %>% 
    summarise(n = n(), .by = c(Interp, No_frames) ) %>%
    # sort_by(.$Interp) %>% # Sort by Interp to match mean_median data
    .[3] # Return only n column
}
