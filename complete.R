complete <- function(directory, id = 1:332) {
    output <- data.frame(id = vector('integer'), nobs = vector('integer'))
    
    for (i in id) {
        
        fname <- paste(directory, sprintf("/%03d.csv",i), sep="")
        mon_data <- read.csv(fname, header=TRUE, sep = ",")
        
        num_complete = sum(complete.cases(mon_data))
        output <- rbind (output, data.frame(id = c(i), nobs = c(num_complete)))
    }
    return(output)
}
