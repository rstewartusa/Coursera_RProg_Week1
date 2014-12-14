pollutantmean_LONG <- function(directory, pollutant, id = 1:332) {
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
        
        ## create full filename 
        fname <- paste("./", directory, "/", padding, i, ".csv", sep="")
        
        ## read in file
        mon_data <- read.csv(fname, header=TRUE, sep = ",")

        ## select appropriate column
        if (pollutant == "sulfate") {
            one_col <- mon_data$sulfate
        } 
        else if (pollutant == "nitrate") {
            one_col <- mon_data$nitrate
        } 
        else {
            print ("ERROR: unexpected pollutant")
            return
        }
            
        is_valid <- !is.na(one_col)
        valid_col <- one_col[is_valid]
        
        totalcount <- totalcount + length(valid_col)
        totalsum <- totalsum + sum(valid_col)
    }
    
    calcmean <- totalsum / totalcount
    return(calcmean)
}

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
        
        ##fname <- paste("./",directory, sprintf("/%03d.csv",i), sep="")
        fname <- paste(directory, sprintf("/%03d.csv",i), sep="")
        mon_data <- read.csv(fname, header=TRUE, sep = ",")
        
        num_complete = sum(complete.cases(mon_data))
        output <- rbind (output, data.frame(id = c(i), nobs = c(num_complete)))
    }
    
    return(output)
}

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