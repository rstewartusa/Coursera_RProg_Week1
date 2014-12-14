pollutantmean <- function(directory, pollutant, id = 1:332) {

    all_data <- vector('numeric')    
    
    for (i in id) {
    
        fname <- paste("./", directory, sprintf("/%03d.csv",i), sep="")
        mon_data <- read.csv(fname, header=TRUE, sep = ",")
        one_col <- mon_data[[pollutant]]
        all_data <- append(all_data, one_col)
    }
    
    return(mean(all_data,na.rm=TRUE))
}
  

complete <- function(directory, id = 1:332) {
    output <- data.frame(id = vector('integer'), nobs = vector('integer'))
    
    for (i in id) {
        
        fname <- paste("./", directory, sprintf("/%03d.csv",i), sep="")
        mon_data <- read.csv(fname, header=TRUE, sep = ",")
        
        num_complete = sum(complete.cases(mon_data))
        output <- rbind (output, data.frame(id = c(i), nobs = c(num_complete)))
    }
    return(output)
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