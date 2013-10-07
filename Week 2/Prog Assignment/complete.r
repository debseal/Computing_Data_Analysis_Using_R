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
		output <- list()
		filesDir <- normalizePath(directory)
		for (i in id){
		csvFile <- paste(filesDir,"\\\\",formatC(i, width=3, flag="0"),".csv", sep="")
		if (file.exists(csvFile)){
		myDataFrame <- read.csv(csvFile)
		}
		else {
		stop("File not found!!!!")
		}
		completeCases <- complete.cases(myDataFrame)
		output <- c(output,i,table(completeCases)["TRUE"])
		}
		outputMatrix <- matrix(output,nrow=length(id), ncol=2, byrow=TRUE)
		colnames(outputMatrix) <- c("id", "nobs")
		myDataFrame <- as.data.frame(outputMatrix)
		return(myDataFrame)
}