# DEV NOTES ===================================================================
# * Improve error handling
# DONE ************************************************************************
# * Add analyses
#     - Total path length 
#     - CoFPL velocity
#     - AP CoF amplitude
#     - ML CoF amplitude
#     - AP CoF velocity
#     - ML CoF velocity
#     - CoF ellipse area
# * Read sample time from header of data file 
# * Limit data used in analyses to start time to end time
# * Implemented changes to reading settings
# * Added scaling factor to convert units to mm (if required)
# * Sample duration calculation modified so that it is not sensitive to skipped
#     frames at the start/ end of trials
# * Write results to CSV file
# * Writing descriptive stats to file added
# * Extracting trial category from file names using Regex
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# library(readr) # Used by read_csv
# library(svDialogs) # Used for basic dialogues
library(magrittr) # Used for pipes


# RUN THE SCRIPT ######
run_it <- function(dev = FALSE){
  settings <- get_setns() # Get settings
  
    ### Get files to work on ---------
    # While no files in vector or no vector
    got_files <- FALSE # Not got files yet
    while (!got_files) { # keep going till have files
      CoF_data_files <- get_files(settings$file_type , settings$source)
      if (length(CoF_data_files) < 1) {
        if (svDialogs::dlg_message('No files selected. Try again?', 'yesno')$res == 'no') {
          stop('No files to analyse')
        }
      } else {
        got_files = TRUE
      }
    }
    
    ### Iterate through file list ---------
    message('Starting analyses...')
    
    for (file_i in CoF_data_files) { # for each file in list
      res_l <- list(file_name = basename(file_i)) # Start results list with file name
      
      ### Read data file ----------------
      if (file.access(file_i, mode = 4) == 0) { # If can read from file
        message('Reading ', file_i, '...')
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
        
        if (is.null(CoF_data)) { # Failed to read data from file
          res_l <- c(res_l, Data_read = 'FAIL')
        } else {
          res_l <- c(res_l, Data_read = 'Success')
          
          # Read sample rate -------
          sr <- read_sample_rate(file_i, settings$sr_skp, settings$sr_colm) # Read in sample rate
          res_l <- c(res_l, sample_rate = sr)
          
          
          # Clean file name for labelling -----
          # Remove file type
          clean_file_name <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(file_i))
          # Replace white space
          clean_file_name <- gsub("\\s", "_", clean_file_name)
            
          
          ### Extract factors from file names ---------
          res_l <- c(
            res_l,
            trial = sub(settings$factor_regex, "", clean_file_name)
          )
          
          # Output CoF data as df for testing ------
          if (dev) { 
            assign(clean_file_name, CoF_data, envir = .GlobalEnv)
          }
          
          
          # res_l <- c(
          #   res_l, 
          #   trial = substring(
          #     basename(file_i),
          #     settings$lead_ch, # From n of leading characters
          #     nchar(basename(file_i)) - settings$tail_ch # to length less tail characters
          #   )
          # )
          
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

          ## Run Analyses ============
          
          ### Calculate sample analysis time -----------
          
          # Uses time range in trimmed file since the sample times may not
          # actually match the start/ end times, particularly if frames have
          #been dropped
          # Plus 1 sample since last sample time will be from start of sample
          
          anal_time <- diff(range(CoF_data$Time)) + 1 / sr
          res_l <- c(res_l, sample_t = anal_time) 
          
          ### Calculate parameters -----------
          
          # Calculate path length
          CoFL <- calc_path_length(CoF_data[, c('Ax', 'Ay')])
          res_l <- c(res_l, path_length = CoFL)
          
          # Calculate COPPL velocity
          res_l <- c(res_l, Mean_CoFS = CoFL /  anal_time)
          
          # Calculate ML COP amplitude
          res_l <- c(res_l, CoF_x_Range = max(CoF_data$Ax) - min(CoF_data$Ax) )
          
          # Calculate AP COP amplitude
          res_l <- c(res_l, CoF_y_Range = max(CoF_data$Ay) - min(CoF_data$Ay) )
          
          # Calculate AP COP velocity
          res_l <- c(res_l, CoF_x_vel = sum(diff(CoF_data$Ax)) /  anal_time)
          
          # Calculate ML COP velocity
          res_l <- c(res_l, CoF_y_vel = sum(diff(CoF_data$Ay)) /  anal_time)
          
          # Calculate CoF circle area
          res_l <- c(res_l, Circle_area = CoF_circle_area(CoF_data[,c('Ax', 'Ay')]))
          
          # Convert all results to mm
          # Multiply everything apart from first 4 columns be conversion factor
          res_l[-1:-4] <- lapply(res_l[-1:-4],"*", settings$units_to_mm)
          # Do it again for circle area as this is a squared value
          res_l[12] <- lapply(res_l[12],"*", settings$units_to_mm)
        }
      } else {
        message('Cannot read from: ', file_i, '\n\tFile will be skipped')
        res_l <- c(res_l, result = 'FAIL: File unreadable')
      }
      if (exists('res_df')) {
        res_df <- rbind(res_df, res_l) # Not first file
      } else {
        res_df <- data.frame(res_l) # First file, create df from list
      }
    }
    
    ### Display results and write to file ------------
    if (nrow(res_df) > 1){ # If have more than one set of data, include stats
      stats <- calc_descript_stats(df = res_df, skpcol = 5)
      write_to_file(rbind(res_df, stats)) # write results and summary stats
    } else {
      write_to_file(res_df) # only 1 set of results to write
    }
    res_df
}

# GET SETTINGS ###################################
get_setns <- function(){
  
  #### Check settings files exist and can be read -----
  
  # Check analysis settings file found and readable
  anal_setn_file <- check_setn_file('AnalysisSpec.csv', 'analysis specification')
  if (!is.null(anal_setn_file)) {
    message('Analysis settings file found and readable. Check data file specifications file...')
    
    # Check data file specifications file found and readable
    data_file_spec_file <- check_setn_file('DataFileSpec.csv', 'data file specification')
    if (!is.null(data_file_spec_file)) { 
      message('Data file specifications file found and readable. Load analysis settings...')
      
      ### Read in analysis settings --------
      settings <- read_csv_settings(
        anal_setn_file,
        skp = 2,
        n = 1,
        c_type = list('c', 'd', 'd', 'd', 'c'),
        file_msg = 'analysis settings'
      )
      # If returned analysis settings
      if (!is.null(settings)) { 
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
        if (!is.null(data_f_specs)) { 
          # Replace negative maxn values (flagged for all rows) with Inf
          data_f_specs['maxn'][data_f_specs['maxn'] < 0] <- Inf
          message('Finished loading settings\n\nGet data files...')
          return(
            cbind(
              data_f_specs[data_f_specs$source == settings$file_spec,], # relevant data file specifications
              settings[1,c(2:length(settings))] # analysis settings skipping source column
            )
          )
        } # End: If returned data file specifications
      } # End: If returned analysis settings
    } # End: If data file specifications file found and readable
  } # End: If analysis settings file returned
}

## Check settings file exists in script path =====
check_setn_file <- function(setnfile, file_msg){ 
  ### Create path to settings file -------
  settingsfile <- paste( # default settings file name (no separator)
    dirname(rstudioapi::getSourceEditorContext()$path),
    '/',
    setnfile,
    sep=''
  )
  if (file.access(settingsfile, mode = 4) == 0) { # If can read from file
    return(settingsfile)
  } else {
    message('Unable to read ', file_msg)
    svDialogs::dlg_message(paste('Unable to read ', file_msg), 'ok')
    return()
  }
  
}

## Read In csv Settings File ================
read_csv_settings <- function(setn_file, skp, n = Inf, c_type, file_msg, ...){
  tryCatch( 
    {
      c_setn <- as.data.frame( # Read in column specifications
        readr::read_csv( 
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
  t1 <- readr::read_table(
    fl,
    skip = skp,
    n_max = 1,
    col_names = FALSE,
    show_col_types = FALSE,
    ...
  )
  return(as.numeric(t1[colm])) # As numeric or returns as subset
}

# LOAD DATA FILES ####
## Get Files To Work On ======
get_files <- function(f_type, file_source){
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
  
  if (interactive() && .Platform$OS.type == 'windows') { # choose.files only works on Windows
    return(
      choose.files(
        caption = paste('Select file(s) for analysis (', file_source, ')'),
        filters = file_type_filters,
        index = def_index,
        multi = TRUE
      )
    )
  } else {
    stop('The function to select files only works on MS Windows. Unable to continue')
  }
}

## Import Text (csv/ tsv) Data =========
read_txt_data <- function(data_file, delm, skp, maxn, c_names, c_type, c_select, ...){
  tryCatch(
    {
      import_CoF <- readr::read_delim( # Get a file to work on
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
# ANALYSIS #####################################

### Calculate The Path Length ==========
# See Prieto et al (1996) Measures of Postural Steadiness: Differences
# Between Healthy Young and Elderly Adults, IEEE TRANSACTIONS ON BIOMEDICAL 
# ENGINEERING, VOL. 43, # NO. 9, SEPTEMBER 1996, 956- 966

calc_path_length <- function(CoF_AxAy){ # calculate path length
  diff_square <- apply(CoF_AxAy, 2, diff) %>%# find the difference between consecutive rows
    raise_to_power(2) # and square it
  vector_sqrt <- apply(diff_square, 1, sum) %>% # add row to col
    sqrt() # and take sqrt of values
  return(sum(vector_sqrt))
}

### Calculate Circle Area ==========
# See Prieto et al (1996) Measures of Postural Steadiness: Differences
# Between Healthy Young and Elderly Adults, IEEE TRANSACTIONS ON BIOMEDICAL 
# ENGINEERING, VOL. 43, # NO. 9, SEPTEMBER 1996, 956- 966

CoF_circle_area <- function(Axy, f = 3){
   2 * pi * f * ( 
     sd(Axy$Ax)^2 * 
     sd(Axy$Ay)^2 - 
     stats::cov(Axy)['Ax','Ay']^2 
     )^0.5
}

# RUN STATS ###################################
calc_descript_stats <- function(df, skpcol, ...) {
  summary <- as.data.frame(t(psych::describe(df[-1:-skpcol], ...)))
  summary[ , names(df[1:skpcol]) ] <- "" # Create skpcol empty columns
  # Copy row names into first column of empty columns
  summary[ , (ncol(summary) - skpcol + 1)] <- row.names(summary) 
  cbind( # Move empty columns to start
      summary[ , (ncol(summary) - skpcol + 1):ncol(summary)], # last skpcol columns
      summary[ , 1:(ncol(summary) - skpcol) ] # First skpcol columns
    )
  
  return(summary)
}


# WRITE TO FILE ###############################
write_to_file <- function(df, ...) {
  saved <- FALSE # Not got files yet
  while (!saved) { # keep going till have files
    ### Create file to write to ----------
    export_file <- svDialogs::dlgSave( 
      default = "*.csv", 
      Title = "Choose file to save results to"
    )$res
    if(length(export_file) > 0) { # Got file (returns empty string on cancel)
      tryCatch(
        {
          readr::write_excel_csv(
            df,
            export_file,
            append = FALSE,
            delim = ",",
            quote = "needed",
            escape = c("double", "backslash", "none"),
            eol = "\n",
            num_threads = readr::readr_threads(),
            ...
          )
          saved <- TRUE
        }, error=function(e) {
          message('Error in saving ', export_file)
          print(e)
          if (svDialogs::dlg_message(
            'File did not save. Retry?',
            'yesno')$res == 'no') {
            saved <- TRUE
          }
        }
      )
    } else {
      if (svDialogs::dlg_message(
        'Save aborted. File has not been saved. Retry?',
        'yesno')$res == 'no') {
        saved <- TRUE
      }
    }
  }
}

temp <- function(setnfile){
  settingsfile <- paste( # default settings file name (no separator)
    dirname(rstudioapi::getSourceEditorContext()$path),
    '/',
    setnfile,
    sep=''
  )
}
