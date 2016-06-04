library("stringr")

pollutantmean <- function(directory, pollutant, id = 1:332){ 
	vals <- vector('numeric')
	for(i in id){
		filename <- paste(c(directory, str_pad(i, 3, pad = "0"), ".csv"), collapse = "")
		print(filename)
		indata <- read.csv(filename)
		if(pollutant == "sulfate")
		{
			vals <- c(vals, indata[[2]][!is.na(indata[[2]])])
		}else{
			vals <- c(vals, indata[[3]][!is.na(indata[[3]])])
		}
	}	
	mean(vals)
}

complete <- function(directory, id = 1:332){
	df <- data.frame(id=numeric(), nobs=numeric())
	for(i in id){
		filename <- paste(c(directory, str_pad(i, 3, pad = "0"), ".csv"), collapse = "")
		indata <- read.csv(filename)
		df <- rbind(df, c(i, nrow(indata[!is.na(indata[[2]]) & !is.na(indata[[3]]),])))		
	}
	df
}

corr <- function(directory, threshold =0){
	completeVars <- complete(directory)	
	aboveThreshIds <- subset(completeVars, completeVars[2] > threshold)[[1]]
	print(aboveThreshIds)
	result <- vector('numeric')
	for(i in aboveThreshIds){
		filename <- paste(c(directory, str_pad(i, 3, pad = "0"), ".csv"), collapse = "")
		indata <- read.csv(filename)
		result <- c(result, cor(indata[!is.na(indata[[2]]) & !is.na(indata[[3]]),][[2]], indata[!is.na(indata[[2]]) & !is.na(indata[[3]]),][[3]]))
	}
	result
}