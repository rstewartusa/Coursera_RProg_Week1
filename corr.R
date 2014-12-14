corr <- function(directory, threshold = 0) {
    output <- vector('numeric')
    
    num_files <- length(list.files(directory, full.names=TRUE))

    source("complete.R")
    com_cases <- complete(directory, c(1:num_files))
    rows_above_threshold <- c(1:num_files)[com_cases$nobs > threshold]
    
    for (i in rows_above_threshold) {
        fname <- paste(directory, sprintf("/%03d.csv",i), sep="")
        mon_data <- read.csv(fname, header=TRUE, sep = ",")
        mon_cor <-cor(mon_data$sulfate, mon_data$nitrate, use = "pairwise.complete.obs", method = "pearson")
        output <- append(output, mon_cor)
    }
    
    return(output)
}