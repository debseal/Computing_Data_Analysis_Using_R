corr <- function(directory, threshold = 0) {	
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
		output <- list()
		filesDir <- normalizePath(directory)
		for (file in list.files(filesDir)){
			csvFile <- paste(filesDir,"\\\\",file, sep="")
			if (file.exists(csvFile)){
				myDataFrame <- read.csv(csvFile)
			}
			else {
				stop("File not found!!!!")
			}
			completeCases <- complete.cases(myDataFrame)
			completeCount <- table(completeCases)["TRUE"]
			if (is.na(completeCount)) {
			completeCount <- -1
			}
			#print (completeCount)
			if (completeCount > threshold ){
				output <- c(output,cor(myDataFrame$sulfate, myDataFrame$nitrate,use = "complete"))
			}
		}
		return(as.numeric(output))
}