pollutantmean <- function(directory, pollutant, id = 1:332) {

    all_data <- vector('numeric')    
    
    for (i in id) {
    
        fname <- paste(directory, sprintf("/%03d.csv",i), sep="")
        mon_data <- read.csv(fname, header=TRUE, sep = ",")
        one_col <- mon_data[[pollutant]]
        all_data <- append(all_data, one_col)
    }
    
    return(mean(all_data,na.rm=TRUE))
}