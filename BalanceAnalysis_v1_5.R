# DEV NOTES +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Changes since last version ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Required for this version +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FOR NEXT VERSION ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# * Add initialisation routines to start
#   - Clear environment so there aren't clashes with things already in the 
#       Global Environment
# * Look at using time gaps rather than frame gaps- see code in Interpolation.R
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(readr) # Used by read_csv
library(svDialogs) # Used for basic dialogues
library(magrittr) # Used for pipes and associated aliases (e.g. divide_by)
library(dplyr) # Used for mutate

# RUN THE SCRIPT ######
run_it <- function(data_out = FALSE, miss = FALSE){
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
    missn_l <- list() # Create empty list to append lists of missing times to

    ### Iterate through file list ---------
    message('Starting analyses...')
    res_df <- data.frame() # Create empty data frame to write results to
    
    for (file_i in CoF_data_files) { # for each file in list

      ### Clean file name for labelling -----
      # Remove file type
      clean_file_name <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(file_i) )
      # Replace white space
      clean_file_name <- gsub("\\s", "_", clean_file_name)
      file_inf <- list(file_name = clean_file_name) # Start file info list with file name

      ### Read data file ----------------
      if (file.access(file_i, mode = 4) == 0) { # If can read from file
        message('Analysing ', file_i, '...')
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
          res_df <- rbind(
            res_df,
            c(file_inf, Data_read = 'FAIL')
            )
        } else { # Read file OK- carry on
          ### Read sample rate -----------------
          sr <- read_sample_rate(file_i, settings$sr_skp, settings$sr_colm) # Read in sample rate
          
          ### Trim df to start/ end times -----------
          if (settings$end_t < 0){ # If set to -ve, don't trim end
            CoF_data <- subset(
              CoF_data,
              Time >= settings$start_t
            )
          } else { # Do trim start and end
            CoF_data <- subset(
              CoF_data,
              Time >= settings$start_t &
                Time < settings$end_t
            )
          }
          
          # Multiply Ax & Ay by the scaling factor
          CoF_data[, c("Ax", "Ay")] <-
            CoF_data[, c("Ax", "Ay")] * settings$units_multiply
          
          ### Analysis time calculation ------------------------
          # Uses time range in trimmed file since the sample times may not
          # actually match the start/ end times, particularly if frames have
          #been dropped
          # Plus 1 sample since last sample time will be from start of sample
          anal_time <- diff(range(CoF_data$Time) ) + 1 / sr
          
          ## Do Calcs ==================
          # Create list of file details
          file_inf <- c(
            file_inf, c(
              Data_read = 'Success', 
              trial = stringr::str_remove(clean_file_name, settings$factor_regex),
              No_frames = nrow(CoF_data),
              sample_rate = sr,
              sample_t = anal_time
            )
          )
          # If doing gap analysis, create a list of gap details
          if (settings$fg_max > 0) { # If doing gap analysis
            # Time passed using brackets rather than $ otherwise column name
            # created as "CoF_Data$Time"
            gaps_l <- as.list(frames_skip_anal(CoF_data["Time"], 
                                             sample_rate = sr, 
                                             skip_max = settings$fg_max)
                            )
          } else {
            gaps_l <- list() # empty list to add
          }

        if (settings$interp){ # If doing calculations on interpolated data
            missn <- list_missing(CoF_data, sr)
            if(length(missn) > 0){ # If there are missing rows, interpolate them
              CoF_data <- interpolate(CoF_data, missn)
              missn_l <- c(missn_l, missn) # Add to list of collected missing values
            }
        } else {
          missn <- list()
        }
          
        # Run calcs once for unfilled or int, twice if both
        for (i in 1:sum(settings$unfill, settings$interp) ) {
          # If interpolation data is to be used AND there is something to interpolate
          if (settings$interp & length(missn) > 0 & 
              # AND we are only doing interpolation OR we've already done unfilled
              (!settings$unfill | i == 2) ){ # If doing calculations on unfilled data
            intp <- TRUE
            xy <- CoF_data[, c("Ax_int", "Ay_int")]
            if (i == 2 & settings$fg_max > 0) { # If have already done frame skipping for unfilled
                gaps_l[] <- NA # Fill columns with NA so not repeating data
            }
          } else {
            intp <- FALSE
            xy <- subset(CoF_data[c("Ax", "Ay")], (!is.na(Ax) & !is.na(Ay) ) ) # Ax and Ay columns without NAs
            # xy <- CoF_data[, c("Ax", "Ay")]
          }
          if (i == 2 & length(missn) == 0){ # We've already done unfilled but there is no interpolation to do
            break
          } 
          res_l <- c(
              file_inf, 
              Interp = intp,
              CoF_x_Range = rnge(xy[1] ),
              CoF_y_Range = rnge(xy[2] ),
              CoF_x_pl = CoF_linear_pl(xy[1] ),
              CoF_y_pl = CoF_linear_pl(xy[2] ),
              CoF_xy_pl = CoF_vector_pl(xy), 
              Ellipse_area = CoF_ellipse_area(xy),
              gaps_l
            )
            res_df <- rbind(res_df, res_l)
        }
        
        ### Output CoF data as df for testing --------
        if (data_out) {assign(clean_file_name, CoF_data, envir = .GlobalEnv) }
        # ^^^^ DEBUGGING ONLY ^^^^^^^^^^^
        
        }
      } else {
        message('Cannot read from: ', file_i, '\n\tFile will be skipped')
        res_l <- c(res_l, result = 'FAIL: File unreadable')
      }
    }

    # Convert all bar the first 3 and 7th items (file name, Data read, trial and interp) to numeric
    res_df[ , c(-1:-3, -7)] <- apply(res_df[ , c(-1:-3, -7)], 2, as.numeric)
    
    ### Add speed columns --------------
    res_df <- 
      res_df %>%
      mutate(CoF_x_speed = CoF_x_pl / sample_t,
             CoF_y_speed = CoF_y_pl / sample_t,
             Cof_xy_speed = CoF_xy_pl / sample_t,
             .keep = "all",
             .after = CoF_xy_pl
      )
    write_to_file(res_df, dflt = "BalanceAnalysis_Results.csv", ttl = "Save results as") # write df to file
    
    ## Do Stats on All ========================================
    if (nrow(res_df) > 1){ # If have more than one set of data, include stats
      skpcol <- c(-1, -2, -5) # columns to exclude from stats (file name, data read, sample rate)
      
      ### Do stats unfilled/ int data ---------------
        sts_all <- res_df[c(skpcol, -3)] %>% # Exclude unused grouping column {trial}
          # If doing both interpolated and not, group by Interp. Need the conditional to stop 
          # stats being returned grouped where only interpolated == T but some data sets have no
          # missing values
          {if(settings$interp & settings$unfill) group_by(., Interp) else .} %>%
          group_modify(~ calc_descript_stats(.x, sk_type = settings$sk_type) )
      # Set trial column to df to separate stats from those grouped by trial
      sts_all <- cbind(trial = "All", sts_all) %>%
        relocate(param) # Move parameter column to start
      
      ## Do Grouped Stats ================================
      if(length(unique(res_df$trial) ) > 1) { # If there are more than 1 trial categories
        sts_grp <- res_df[c(skpcol)] %>% # Exclude unused grouping column {interp}
          {if(settings$interp & settings$unfill) group_by(., Interp) else .} %>%
          group_by(trial, .add = TRUE) %>%
          group_modify(~ calc_descript_stats(.x,sk_type = settings$sk_type) )
      } else {
        sts_grp <- data.frame() # Empty df so there's something not to write
      }
        
    }
    write_to_file( # Write descriptive stats to file
      rbind( # Add header to df
        "",
        c("Descriptive stats for all data sets", rep("", ncol(sts_all) - 1) ),
        sts_all,
        if(nrow(sts_grp) > 0) {""},
        if(nrow(sts_grp) > 0) { c("Descriptive stats grouped by trial", rep("", ncol(sts_grp) - 1) ) },
        if(nrow(sts_grp) > 0) {sts_grp}
      ),
      dflt = "BalanceAnalysis_DescriptStats.csv",
      ttl = "Save descriptive statistics as"
      )
    
    # Output list of missing times collected for basing modelling of the impact of dropped frames
    if (miss) {
      if (settings$interp) {
        assign("missing_times", missn_l, envir = .GlobalEnv)
        message(
          "\nDetails of missing times\n\tMean = ", 
          mean(as.numeric(missn_l) ), "\n\tSD = ", 
          sd(as.numeric(missn_l) )
          )
      } else {
        print("No missing times to export. These are only calculated if interp = T")
      }
      cat("=================================================\n\n")
    }
    # ^^^^ DEBUGGING ONLY ^^^^^^^^^^^
    
    res_df # Return results df
}

# GET SETTINGS ###################################
get_setns <- function(){

  ### Check settings files exist and can be read -----

  # Check analysis settings file found and readable
  anal_setn_file <- check_setn_file('AnalysisSpec.csv', 'Analysis specification')
  if (!is.null(anal_setn_file) ) {
    message('Analysis settings file found and readable. Check data file specifications file...')

    # Check data file specifications file found and readable
    data_file_spec_file <- check_setn_file('DataFileSpec.csv', 'Data file specification')
    if (!is.null(data_file_spec_file) ) {
      message('Data file specifications file found and readable. Load analysis settings...')

      ### Read in analysis settings --------
      settings <- read_csv_settings(
        anal_setn_file,
        skp = 2,
        n = 1,
        c_type = list('c', 'd', 'd', 'd', 'c', 'i', 'i', 'l', 'l'),
        file_msg = 'analysis settings'
      )
      # If returned analysis settings
      if (!is.null(settings) ) {
        message('Analysis settings loaded. Load data file specifications...')

        ### Read in data file specifications --------
        data_f_specs <- read_csv_settings(
          data_file_spec_file,
          skp = 2,
          # n = 3, # Not set- just read however many are there
          c_type = list('c', 'c', 'c', 'i', 'i', 'i', 'i', 'c', 'c'),
          file_msg = 'analysis settings'
        )
        # If returned data file specifications
        if (!is.null(data_f_specs) ) {
          # Replace negative maxn values (flagged for all rows) with Inf
          data_f_specs['maxn'][data_f_specs['maxn'] < 0] <- Inf
          message('Finished loading settings\nGet data files...')
          tryCatch(
            {
              return(
                cbind(
                  data_f_specs[data_f_specs$source == settings$file_spec,], # relevant data file specifications
                  settings # analysis settings
                )
              )
             }, error = function(e) {
                print(e)
                cat(
                  "Data file specification not recognised: ",
                  settings$file_spec,
                  "\nValid options are;"
                )
                print(data_f_specs$source)
            }
          )
        } # End: If returned data file specifications
      } # End: If returned analysis settings
    } # End: If data file specifications file found and readable
  } # End: If analysis settings file returned
}

## Check settings file exists in script path =====
check_setn_file <- function(setnfile, file_msg){
  ### Create path to settings file -------
  setnfile <- paste( # default settings file name (no separator)
    dirname(rstudioapi::getSourceEditorContext()$path), # path to script
    '/SettingsFiles/', # within SettingsFiles folder
    setnfile,
    sep=''
  ) 
  file_ok <- FALSE
  while (!file_ok) {
    if(file.access(setnfile, mode = 4) == 0) {
      file_ok <- TRUE
      return(setnfile)
    } else {
      message("Warning!:", file_msg, " file not found or unreadable")
      dlg_message(
          paste(file_msg, " file not found or unreadable, please choose valid settings file."),
          type = "ok"
        )
        setnfile <- file.choose()
    }
  }
}

## Read In csv Settings File ================
read_csv_settings <- function(setn_file, skp, n = Inf, c_type, file_msg, ...){
  tryCatch(
    {
      c_setn <- as.data.frame( # Read in column specifications
        read_csv(
          setn_file, # Name of file passed
          skip = skp,
          n_max = n, # Read in all column details present- at end of file
          col_names = TRUE,
          col_types = c_type,
          comment = '#',
          ...
        )
      )
      return(c_setn)
    }, error=function(e) {
      message('Error in loading ', file_msg, ':\n', setn_file)
      print(e)
      return()
    }
  )
}

## Read Sample Rate From Data File ==========
read_sample_rate <- function(fl, skp, colm, ...) {
  t1 <- read_table(
    fl,
    skip = skp,
    n_max = 1,
    col_names = FALSE,
    show_col_types = FALSE,
    ...
  )
  return(as.numeric(t1[colm] ) ) # As numeric or returns as subset
}

# LOAD DATA FILES ####
## Get Files To Work On ======
get_files <- function(f_type, file_source = ""){
  file_type_filters <- matrix(c( # Create matrix of potential file types
    'Text files (*.txt)', 'Comma separated values (*.csv)',  'Tab separated values (*.tsv)',
    '*.txt', '*.csv', '*.tsv'),
    nrow = 3, ncol = 2
    )
  if (f_type == 'csv') {
    def_index <- 2
  } else if (f_type == 'tsv') {
    def_index <- 3
  } else {
    def_index <- 1
  }

  if (interactive() & .Platform$OS.type == 'windows') { # choose.files only works on Windows
    return(
      choose.files(
        caption = paste('Select file(s) for analysis (', file_source, ')'),
        filters = file_type_filters,
        index = def_index, 
        multi = TRUE
      )
    )
  } else if (interactive() & .Platform$OS.type == 'unix'){ # Should return 'unix for Unix, Linux, and Mac OSX
    return(
      tcltk::tk_choose.files( # not adopted for all OS as index not implemented
        caption = paste('Select file(s) for analysis (', file_source, ')'),
        filters = file_type_filters,
        index = def_index, # Not implemented in tk_choose.files. Do not comment out in case it is later
        multi = TRUE
      )
    )
  } else {
    stop('The functions to select files only works on MS Windows, Mac OSX and Unix/ Linux. Unable to continue')
  }
}

## Import Text (csv/ tsv) Data =========
read_txt_data <- function(data_file, delm = NULL, skp = 0 , maxn = Inf, c_names = TRUE, c_type = NULL, c_select = NULL, ...){
  tryCatch(
    {
      import_CoF <- read_delim( # Get a file to work on
        data_file, # Name of file passed
        lazy = TRUE,
        show_col_types = FALSE,
        delim = gsub('\\\\t', '\t', delm), # Remove double escape
        skip = skp,
        n_max = maxn,
        col_names = c_names,
        col_types = c_type,
        ...
      )

      return(import_CoF)
    }, error=function(e) {
        message('Error in reading from: ', data_file)
        print(e)
        return()
    }
  )
}

## Create List of Missing Times =======================
list_missing <- function(x, # Data frame to work on
                         sr # Sample rate
){
  # Create list of times within the duration of the file at an interval of the sample rate, i.e. what there SHOULD be in the data
  list((min(x$Time) * sr):((max(x$Time) * sr) - 1) / sr) %>% 
    .[[1]] %>% # Extract values from list- fails if this is within setdiff, don't know why
    setdiff(., x$Time)
}

## Interpolate Missing Values =====================
interpolate <- function(x, # Data frame to work on
                        missn # list of missing times
){ 
  
  ### Add the missing rows to the df -----------------
  x[nrow(x) + 1:length(missn), ] <- NA # Create blank rows for each missing value
  x[-1:-(nrow(x) - length(missn) ), "Time"] <- missn # paste in the missing values 
  x <- x[order(x$Time), ] # Sort by Time
  
  # Interpolate missing values
  x <- x %>% mutate(Ax_int = zoo::na.approx(Ax) ) %>%
    mutate(Ay_int = zoo::na.approx(Ay) )
}

# ANALYSIS #####################################

## Vector Path Length Calculation  =====================
# See Prieto et al (1996) Measures of Postural Steadiness: Differences
# Between Healthy Young and Elderly Adults, IEEE TRANSACTIONS ON BIOMEDICAL
# ENGINEERING, VOL. 43, # NO. 9, SEPTEMBER 1996, 956- 966
CoF_vector_pl <- function(CoF_AxAy){ # calculate path length
  diff_square <- apply(CoF_AxAy, 2, diff) %>%# find the difference between consecutive rows
    raise_to_power(2) # and square it
  vector_sqrt <- apply(diff_square, 1, sum) %>% # add row to col
    sqrt() # and take sqrt of values
  return(sum(vector_sqrt) )
}

## Range Calculation ==============================
rnge <- function(x){ # max - min within data passed
  diff(range(x) )
}

## Linear Path Lengths Calculation  =====================
CoF_linear_pl <- function(x){ # Total difference between consecutive points / time

  x[[1]] %>%
    diff() %>%
    abs() %>%
    sum() 
}

## Ellipse Area Calculation ========================
# See Prieto et al (1996) Measures of Postural Steadiness: Differences
# Between Healthy Young and Elderly Adults, IEEE TRANSACTIONS ON BIOMEDICAL
# ENGINEERING, VOL. 43, # NO. 9, SEPTEMBER 1996, 956- 966
CoF_ellipse_area <- function(Axy, f = 3){
  2 * pi * f * (
   sd(Axy[[1]] )^2 *
   sd(Axy[[2]] )^2 - 
   stats::cov(Axy)[1, 2]^2 # Returned as matrix, [1, 2] is COX Ax, Ay
   ) ^ 0.5
}

## Frame Skipping Analysis =================
# Create matrix with the number of columns of potential frame skipped counts
frames_skip_anal <- function(times, sample_rate, skip_max = 10){
  all_skip <-  matrix(
    c(1:skip_max), 
    nrow = 1, 
    dimnames = list(
      c("Count"), 
      # Format column names with leading zeros or sort order will be wrong (sorted as character)
      # Width = integer division by 10, i.e. num of digits in skip_max - 1
      formatC(c(1:skip_max), width = skip_max %/% 10 + 1, flag = "0") 
      )
    )
  # Create table of frame gap frequencies
  frame_skips <- times$Time %>% # Pass sample times
    divide_by(1 / sample_rate) %>% # Divide by sample rate to get multiple of sample rate
    round() %>% # Round to 0dp as sample rates aren't constant at nominal sample rate
    diff() %>% # Differentiate times
    table() # Table of counts of each value
  
  # Merge frequency table with matrix of possible counts to give matrices of consistent
  # sizes between trials
  frame_skips_m <- matrix(frame_skips, nrow = 1, dimnames = list(
      c("Count"), 
      # Format column names with leading zeros or sort order will be wrong (sorted as character)
      # Width = integer division by 10, i.e. num of digits in skip_max - 1
      formatC(as.integer(names(frame_skips) ),  width = skip_max %/% 10 + 1, flag = "0")
      )
    ) %>%
    # Merge the CFA matrix with the one containing enough rows but retain only the CFA data
    merge(all_skip, . , all.y = T, nomatch = 0) 
    frame_skips_m[ , order(as.character(all_skip) )] # Sort by column name

  # Prefix column names with F so X isn't added when converted to df
  names(frame_skips_m) <- paste("FG", names(frame_skips_m), sep = "")
  
  frame_skips_m[is.na(frame_skips_m)] <- 0 # replace NA with 0
  # Append count of values exceeding max number processed
  # If statement used to avoid warnings and returning -Inf for max when skip_max > worst skip length, i.e.
  # would return 0 length matrix
  if (max(as.integer(names(frame_skips) ) ) > skip_max){ # If worst skip length is > max size of recording matrix
    frame_skips_m <- cbind(
    frame_skips_m,
    Sum_Exceed = sum(frame_skips[-1:-skip_max] ),
    Max_Exceed = max(frame_skips[-1:-skip_max] ) 
  )
  } else {
    frame_skips_m <- cbind(
    frame_skips_m,
    Sum_Exceed = 0,
    Max_Exceed = 0
    )
  }
}

# RUN STATS #############################################################
## Standard Error Calculation ==============
std_error <- function(x){
  sd(x, na.rm = TRUE) / sqrt(length(x) )
}

## Descriptive stats Calculation ==============
calc_descript_stats <- function(x, trm = 0.1, sk_type = 3) {
  data.frame(
    n  =  apply(x, 2, length),
    Mean =  apply(x, 2, mean, na.rm = TRUE),
    SD = apply(x, 2, sd, na.rm = TRUE),
    Median = apply(x, 2, median, na.rm = TRUE),
    Trimmed_Mean = apply(x, 2, mean, trim = trm, na.rm = TRUE),
    MAD = apply(x, 2, mad, na.rm = TRUE),
    Minimum = apply(x, 2, min),
    # Maximum = apply(x, 2, range)[2,],
    Maximum = apply(x, 2, max),
    Range = apply(x, 2, rnge),
    Standard_Error = apply(x, 2, std_error),
    Skew =
      if (sk_type != 2 | nrow(x) > 2 ) { # Type 2 requires minimum of 3 obs
        apply(x, 2, e1071::skewness, na.rm = TRUE, type = sk_type)
      } else {
        NA
      },
    Kurtosis =
      if (sk_type != 2 | nrow(x) > 3 ) { # Type 2 requires minimum of 4 obs
        apply(x, 2, e1071::kurtosis, na.rm = TRUE, type = sk_type)
      } else {
        NA
      }
  ) %>%
    t() %>%
    as.data.frame() %>% #Transpose converts to matrix array but must be df for mutate functions
    cbind(param = rownames(.), .) # Add parameter names as a column
}

## Plot Interpolates Missing Values =====================
# This function is never called but is retained to allow the plotting of unfilled/
#   interpolated if required by calling run_it with data_out = T and then passing 
#   the resulting df to this function
plot_interpolate <- function(x){
  library(ggplot2)
  ggplot(x, aes(x = Time) )+
    geom_line(aes(y = Ax_int, colour = "Ax_int") )+
    geom_point(aes(y = Ax), na.rm = TRUE) +
    geom_line(aes(y = Ay_int, colour = "Ay_int") )+
    geom_point(aes(y = Ay), na.rm = TRUE)
}

# WRITE TO FILE ###############################
write_to_file <- function(df, dflt = "*.csv", ttl = "Save file as", ...) {
  saved <- FALSE # Not got files yet
  while (!saved) { # keep going till have files
    export_file <- dlgSave( # Create file to write to
      default = dflt,
      title = ttl
    )$res
    if(length(export_file) > 0) { # Got file (returns empty string on cancel)
      tryCatch(
        {
          write_excel_csv(
            df,
            export_file,
            append = FALSE,
            delim = ",",
            quote = "needed",
            escape = c("double", "backslash", "none"),
            eol = "\n",
            num_threads = readr_threads(),
            ...
          )
          saved <- TRUE
          message("Output saved to: ", export_file)
        }, error = function(e) {
          message('Error in saving ', export_file)
          print(e)
        }
      )
    } else {
      if (dlg_message( 'Save aborted. File has not been saved. Retry?',
        'yesno')$res == 'no') {saved <- TRUE}
    }
    if (!saved){ # If still haven't saved it's because write failed
      if (dlg_message( 'File did not save. Retry?',
                       'yesno')$res == 'no') {saved <- TRUE}
    }
  }
}
