pollutantmean <- function(directory, pollutant, id = 1:332) {
    totalsum <- 0
    totalcount <- 0
    
    for (i in id) {
        
        ## generate the filename padding
        if (i < 10) {
            padding="00"
        }
        else if (i >= 10 && i < 100) {
            padding="0"
        }
        else if (i >= 100 && i < 1000) {
            padding=""
        }
        else {
            print ("ERROR: unexpected filename (>999)")
            return
        }
        
        ## create filename padding
        fname <- paste(directory, "/", padding, i, ".csv", sep="")
        
        ## read in file
        mon_data <- read.csv(fname, header=TRUE, sep = ",")

        ## select appropraite column
        if (pollutant = "sulfate") {
            one_col <- mon_data$sulfate
        } 
        else if (pollutant = "nitrate") {
            one_col <- mon_data$nitrate
        } 
        else {
            print ("ERROR: unexpected pollutant")
            return
        }
            
        is_valid <- !is.na(one_col)
        valid_col <- one_col[is_valid]
        totalcount <- totalcount + 1
        totalsum <- totalsum + sum(valid_col)
    }
}
  

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
}