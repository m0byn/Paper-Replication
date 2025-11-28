###############################################################################################################################
# Title: Functions.R
# Creator: Manuel ZEROBIN
# Project: MasterThesis_Code
# Task: Store relevant functions
# Description: This file stores all relevant functions written by myself for the project.
###############################################################################################################################

###############################  Define useful functions ###############################

package_verification = function(packages = NULL, 
                                type = NA) {
  
  ################################ define global variables ###############################
  
  opt = c('function', 'script') # options for argument 'type'
  
  ################################### test for arguments #################################
  
  args = c('packages') # includes arguments for later verifications
  missing_args = which(is.null(c(packages))) # create boolean which ones are na's and extract the indices of them
  
  if (length(missing_args) >= 1) {
    stop(paste0(c('You must specify ', rep('', length(missing_args)-1)), c(args[missing_args]), c(rep('', length(missing_args)-1), '!'), collapse = ", "))
  }
  
  ################################ beginning of function #################################
  
  installed_packages <- packages %in% rownames(installed.packages()) # install packages not yet installed
  if (any(installed_packages == FALSE)) {
    print(paste0(c('The following packages are not installed: ', rep('', length(packages[!installed_packages])-1)), packages[!installed_packages], c(rep('', length(packages[!installed_packages])-1), '.'), collapse = ', '))
    TF = askYesNo('Do you want to install these packages?', 
                  default = TRUE, 
                  prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel"))))
    
    if (is.na(TF) == TRUE | TF == FALSE) {
      if (is.na(type) == TRUE) {
        stop(paste('Without installing the necessary packages this code cannot be used!'))
      }
      else if ((type %in% opt) == TRUE) {
        stop(paste('Without installing the necessary packages this', type, 'cannot be used!'))
      }
    }
    else if (TF == TRUE) {
      install.packages(packages[!installed_packages])
      invisible(lapply(packages, library, character.only = TRUE))
    }
  }
  else {
    invisible(lapply(packages, library, character.only = TRUE))
  }
} # check for necessary packages

import_data = function(file = NULL, # specify filename
                       document_type = c('standard', 'excel', 'stata', 'spss'), # specify which document type (standard = .txt, .csv, .tsv)
                       header = TRUE,  # specify whether first row are colnames or not
                       separator = NULL, # specify separating value
                       decimals = NULL, # specify decimal indicator
                       na = '', # specify character vector of symbols to be treated as NA
                       sheet = 1) { # choose excel sheet either by name or number
  
  ################################ define global variables ##############################
  
  data = NULL
  
  ####################################### load packages #################################
  
  packages = c('readxl', 'haven')
  installed_packages <- packages %in% rownames(installed.packages()) # install packages not yet installed
  if (any(installed_packages == FALSE)) {
    TF = askYesNo('It seems that some packages which are required by this function are not installed. Do you want to install these packages?', 
                  default = TRUE, 
                  prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel"))))
    
    if (is.na(TF) == TRUE | TF == FALSE) {
      stop(paste('Without installing the necessary packages this function cannot be used!'))
    }
    else if (TF == TRUE) {
      install.packages(packages[!installed_packages])
      invisible(lapply(packages, library, character.only = TRUE))
    }
  }
  else {
    invisible(lapply(packages, library, character.only = TRUE))
  }
  
  ################################### test for arguments #################################
  
  if (any(document_type == c('standard', 'excel', 'stata', 'spss'))) { # test whether the separator type is of allowed arguments
    document_type = document_type # if yes, ok
  } 
  else {
    document_type = NA # if not set to na
  }
  
  if (any(separator == c('', ',', ';', '\t'))) { # test whether the separator type is of allowed arguments
    separator = separator # if yes, ok
  } 
  else {
    separator = NA # if not set to na
  }
  
  if (any(decimals == c(',', '.'))) { # test whether decimals type is of allowed arguments
    decimals = decimals # if yes, ok
  } 
  else {
    decimals = NA # if not set to na
  }
  
  args = c('file', 'document_type') # includes arguments for later verifications
  missing_args = which(is.na(c(file, document_type))) # create boolean which ones are na's and extract the indices of them
  
  if (length(missing_args) >= 1) {
    stop(paste0(c('You must specify ', rep('', length(missing_args)-1)), c(args[missing_args]), c(rep('', length(missing_args)-1), '!'), collapse = ", "))
  }
  
  if (document_type == 'standard') {
    args_standard = c('separator', 'decimals') # includes arguments for later verifications
    missing_args_standard = which(is.na(c(separator, decimals))) # create boolean which ones are na's and extract the indices of them
    
    if (length(missing_args_standard) >= 1) {
      stop(paste0(c('You must specify ', rep('', length(missing_args_standard)-1)), c(args_standard[missing_args_standard]), c(rep('', length(missing_args_standard)-1), '!'), collapse = ", "))
    }
  }
  
  ############################ read data #############################
  
  if (document_type == 'standard') {
    data = read.table(file = file, header = header, sep = separator, dec = decimals, na.string = na)
  }
  else if (document_type == 'excel') {
    data = read_excel(path = file, sheet = sheet, col_names = header, col_types = NULL, na = na)
  }
  else if (document_type == 'stata') {
    data = read_dta(file = file)
  }
  else if (document_type == 'spss') {
    data = read_spss(file = file)
  }
  else {
    stop(paste('Process stopped since if statement did not get activated!'))
  }
  return(data)
}

copy_paste_rows = function(data = NA, 
                           startrow = NA,
                           rows = NA,
                           startcol = NA, 
                           cols = NA,
                           diff = NA,
                           coldata = NA, # must be a sequence containing columns to copy rownames from
                           colname1 = NA,
                           col1 = NA,
                           colname2 = NA,
                           col2 = NA,
                           iterations = NA) {
  
  ############################## test for arguments ############################
  
  args = c('data', 'startrow', 'rows', 'startcol', 'cols', 'coldata', 'colname1', 'col1', 'colname2', 'col2', 'iterations') # includes arguments for later verifications
  missing_args = which(is.na(c(data, startrow, rows, startcol, cols, coldata, colname1, col1, colname2, col2, iterations))) # create boolean which ones are na's and extract the indices of them
  
  if (length(missing_args) >= 1) {
    stop(paste0(c('You must specify ', rep('', length(missing_args)-1)), c(args[missing_args]), c(rep('', length(missing_args)-1), '!'), collapse = ", "))
  }
  
  ########################### begin function and loop ##########################
  
  inc = 0
  
  for (i in 1:iterations) {
    
    if (i == 1) { # first iteration in loop where no year needs to be defined
      
      data[seq(nrow(data)+1-rows, nrow(data)),seq(startcol,startcol+cols-1)] = 
        data[seq(startrow, rows),seq(startcol+cols*(i-1+inc)+diff*(inc), startcol+cols*(i+inc)-1+diff*(inc))] # copy female data
      
      data[seq(nrow(data)+1, nrow(data)+rows), coldata] = 
        data[seq(startrow, rows),coldata] # add rows for male data and copy the columns for rownames
      
      data[seq(nrow(data)+1-rows, nrow(data)),colname1] = col1[2] # define gender
      
      data[seq(nrow(data)+1-rows, nrow(data)), seq(startcol,startcol+cols-1)] = 
        data[seq(startrow, rows),seq(startcol+cols*(i+inc)+diff*(inc), startcol+cols*(i+1+inc)-1+diff*(inc))] # copy male data
    }
    
    if (i > 1) { # rest of loop where years are included as well!
      
      data[seq(nrow(data)+1, nrow(data)+rows), coldata] = 
        data[seq(startrow, rows),coldata] # add rows for male data and copy the columns for rownames
      
      data[seq(nrow(data)+1-rows, nrow(data)),colname2] = col2 + i-1 # define year
      
      data[seq(nrow(data)+1-rows, nrow(data)),colname1] = col1[1] # define gender
      
      data[seq(nrow(data)+1-rows, nrow(data)),seq(startcol,startcol+cols-1)] = 
        data[seq(startrow, rows),seq(startcol+cols*(i-1+inc)+diff*(inc), startcol+cols*(i+inc)-1+diff*(inc))] # copy female data
      
      data[seq(nrow(data)+1, nrow(data)+rows), coldata] = 
        data[seq(startrow, rows),coldata] # add rows for male data and copy the columns for rownames
      
      data[seq(nrow(data)+1-rows, nrow(data)),colname2] = col2 + i-1 # define year
      
      data[seq(nrow(data)+1-rows, nrow(data)),colname1] = col1[2] # define gender
      
      data[seq(nrow(data)+1-rows, nrow(data)), seq(startcol,startcol+cols-1)] = 
        data[seq(startrow, rows),seq(startcol+cols*(i+inc)+diff*(inc), startcol+cols*(i+1+inc)-1+diff*(inc))] # copy male data
      
    }
    inc = inc + 1
  }
  return(data)
}

to_var = function(piped, 
                  ..., 
                  env = .GlobalEnv) {
  var_name = quo_name(quos(...)[[1]]) # ... denotes additional not yet specified arguments without default value
  assign(var_name, piped, envir = env) # assign name and piped variable to global environment
  piped
}

retrieve_objects = function(obj.name, # specify object name in quotes
                            classes.include = NA, # specify classes the object shall include in quotes. Several can be stated in a vector
                            classes.not.include = NA, # specify classes the object shall not include in quotes. Several can be stated in a vector. Can be left empty like ''
                            by.name = FALSE, # specify if objects shall be filtered by name
                            search = NA){ 
  
  ############################## test for arguments ############################
  
  args = c('obj.name', 'classes.include', 'classes.not.include') # includes arguments for later verifications
  missing_args = which(is.na(c(obj.name, classes.include, classes.not.include))) # create boolean which ones are na's and extract the indices of them
  
  ################################ core function ###############################
  
  if (by.name == TRUE & length(missing_args > 1)) { # test whether by.name is true
    obj.name.filtered = dplyr::filter(data.frame(obj.list = obj.name), stringr::str_detect(obj.list, search)) # filter object names according to search string
    return(obj.name.filtered[['obj.list']]) # return column as atomic vector
  }
  if (by.name == TRUE & is_empty(missing_args) == TRUE) { # test whether objects shall be filtered by name AND class
    obj.name.filtered = dplyr::filter(data.frame(obj.list = obj.name), stringr::str_detect(obj.list, search)) # filter object names according to search string
    true.false.list = lapply(obj.name.filtered[['obj.list']], function(x) inherits(get(x), classes.include) & !inherits(get(x), classes.not.include)) # apply function(x)
    # which is equivalent to class_filter above) to every object in the list obj.name and return a list of true/false.
    true = obj.name.filtered[which(unlist(true.false.list)),] # unlist the true/false list and retrieve objects of global environment for which
    # previous statement returned true
    return(true)
  }
  if(by.name == FALSE) { # default option where objects are only filtered by classes
    true.false.list = lapply(obj.name, function(x) inherits(get(x), classes.include) & !inherits(get(x), classes.not.include)) # apply function(x) 
    # which is equivalent to class_filter above) to every object in the list obj.name and return a list of true/false.
    true = ls(.GlobalEnv)[which(unlist(true.false.list))] # unlist the true/false list and retrieve objects of global environment for which 
    # previous statement returned true
    return(true) 
  }
}

save_objects = function(obj.names = NA, # specify object names
                        directory = here::here('Output'), # default directory, if other of interest, specify here
                        ...){ # the ... stands for additional arguments which can be passed to the pdf() function, e.g. 
  # to specify width and height. Just look up the function information.
  
  ############################## test for arguments ############################
  
  args = c('obj.names') # includes arguments for later verifications
  missing_args = which(is.na(c(obj.names))) # create boolean which ones are na's and extract the indices of them
  
  if (length(missing_args) >= 1) {
    stop(paste0(c('You must specify ', rep('', length(missing_args)-1)), c(args[missing_args]), c(rep('', length(missing_args)-1), '!'), collapse = ", "))
  }
  
  ################################ function start ##############################
  
  dir.create(file.path(directory), showWarnings = FALSE) # create directory and suppress the warning message if it already exists.
  # Note, no additional directory will be created if it already exists
  for (i in 1:length(obj.names)) { # specify for loop for saving each object in a single pdf file
    pdf(file = file.path(directory, paste0(obj.names[i], '.pdf')), ...) # open pdf connection and paste directory and file name
    print(get(obj.names[i])) # print file
    dev.off() # close pdf connection
    p_bar(it = i, min = 1, max = length(obj.names)) # display a progress bar for this for loop
  }
  pdf(file = file.path(directory, 'all.pdf'), ...) # open pdf connection to save all objects into one pdf file
  for (i in 1:length(obj.names)) { # specify for loop
    print(get(obj.names[i])) # print file
    p_bar(it = i, min = 1, max = length(obj.names)) # show progress bar for this for loop
  }
  dev.off() # close pdf connection
}

p_bar = function(it = NA, 
                 min = NA, 
                 max = NA){
  
  ############################## test for arguments ############################
  
  args = c('it', 'min', 'max') # includes arguments for later verifications
  missing_args = which(is.na(c(it, min, max))) # create boolean which ones are na's and extract the indices of them
  
  if (length(missing_args) >= 1) {
    stop(paste0(c('You must specify ', rep('', length(missing_args)-1)), c(args[missing_args]), c(rep('', length(missing_args)-1), '!'), collapse = ", "))
  }
  
  ################################ function start ##############################
  
  if (it == min) { # step only carried out in first iteration
    assign('pb', 
           progress::progress_bar$new(total = max - min + 1, # specify ticks of pbar 
                                      clear = FALSE, # keep pbar after it finished
                                      format = "(:spin) [:bar] :percent, elapsed: :elapsed, remaining: :eta"), # specify format
           envir = .GlobalEnv) # assign pbar to pb in global environment
    pb$tick(0) # not sure if it is necessary...should display pbar even before first tick is completed
  }
  pb$tick() # tick pbar every iteration
  
  if (it == max) { # test if pbar iterations are done
    remove(pb, envir = .GlobalEnv) # remove pb object from global environment
  }
}


detect_outliers = function(data = NA, # specify vector or column of dataframe
                           remove.outliers = c(TRUE, FALSE), # specify whether to remove or only detect outliers
                           save.outliers = FALSE, # specify whether outliers shall be save in global environment
                           lower.quantile = 0.25, # can be edited if needed
                           upper.quantile = 0.75, # can be edited if needed
                           outlier.distance = 1.5, # can be edited if needed
                           only.extreme = FALSE, # if TRUE, then outlier.distance is set to 3
                           only.lower = FALSE, # only handle lower outliers
                           only.upper = FALSE) { # only handle upper outliers
  
  ################################### test for arguments #################################
  
  args = c('data', 'remove.outliers') # includes arguments for later verifications
  missing_args = which(is.null(c(data, remove.outliers))) # create boolean which ones are na's and extract the indices of them
  
  if (length(missing_args) >= 1) {
    stop(paste0(c('You must specify ', rep('', length(missing_args)-1)), c(args[missing_args]), c(rep('', length(missing_args)-1), '!'), collapse = ", "))
  }
  
  ################################ beginning of function #################################
  
  quantiles = quantile(as.vector(data), probs = c(lower.quantile, upper.quantile)) # specify quantiles
  iqr = IQR(as.vector(data)) # specify interquartildistance
  
  if (only.extreme == TRUE) { # if only.extreme == TRUE set outlier.distance = 3
    outlier.distance = 3
  }
  
  upper_limit = quantiles[2]+outlier.distance*iqr # specify upper limit
  lower_limit = quantiles[1]-outlier.distance*iqr # specify lower limit
  
  if (only.lower == FALSE & only.upper == FALSE) { # default case where both lower and upper outliers are handled
    outliers_index = which(data > upper_limit | data < lower_limit) # extract positions of outliers
    num_outliers = length(outliers_index) # specify how many outliers
    cat(paste('The lower limit is:', lower_limit, '\nThe upper limit is:', upper_limit), sep = '\n') # print limits
    cat(paste(num_outliers, ' outliers were found!')) # print amount of outliers
    if (save.outliers == TRUE) { # test if outliers shall be saved
      outliers = data[outliers_index] # extract outliers
      assign('outliers', outliers, pos = .GlobalEnv) # save outliers in global environment
    }
    if (remove.outliers == TRUE) { # test if outliers shall be removed from data
      data = data[-outliers_index] # remove outliers
      return(data)
    }
  }
  if (only.lower == TRUE) { # case to only handle lower outliers 
    outliers_index = which(data < lower_limit) # extract positions of outliers
    num_outliers = length(outliers_index) # specify how many outliers
    cat(paste('The lower limit is:', lower_limit), sep = '\n') # print limits
    cat(paste(num_outliers, ' outliers were found!')) # print amount of outliers
    if (save.outliers == TRUE) { # test if outliers shall be saved
      outliers = data[outliers_index] # extract outliers
      assign('outliers', outliers, pos = .GlobalEnv) # save outliers in global environment
    }
    if (remove.outliers == TRUE) { # test if outliers shall be removed from data
      data = data[-outliers_index] # remove outliers
      return(data)
    }
  }
  if (only.upper == TRUE) { # case to only handle upper outliers
    outliers_index = which(data > upper_limit) # extract positions of outliers
    num_outliers = length(outliers_index) # specify how many outliers
    cat(paste('The upper limit is:', upper_limit), sep = '\n') # print limits
    cat(paste(num_outliers, ' outliers were found!')) # print amount of outliers
    if (save.outliers == TRUE) { # test if outliers shall be saved
      outliers = data[outliers_index] # extract outliers
      assign('outliers', outliers, pos = .GlobalEnv) # save outliers in global environment
    }
    if (remove.outliers == TRUE) { # test if outliers shall be removed from data
      data = data[-outliers_index] # remove outliers
      return(data)
    }
  }
  else {
    data = data
    return(data)
  }
}

########################################################################################

demean_FE = function(data, FE1, FE2 = NULL){
  name = deparse(substitute(data))
  data.demeaned = data %>%
    group_by_(FE1) %>%
    mutate(across(where(is.numeric), ~ .x - mean(.x))) %>%
    ungroup()
  if (is.null(FE2) == FALSE) {
    data.demeaned = data.demeaned %>%
      group_by_(FE2) %>%
      mutate(across(where(is.numeric), ~ .x - mean(.x))) %>%
      ungroup()
  }
  return(data.demeaned)
}

########################################################################################

get_data_structure = function(data) {
  structure.info = data.frame(
    column.name = names(data),
    column.type = sapply(data, class),
    row.names = NULL
  )
  return(structure.info)
}