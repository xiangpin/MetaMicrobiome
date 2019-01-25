##' @title creat a vector with high/low versus the threshold. 
##' 
##' @description
##' Creat a vector with hith/low versus the threshold, suitable for 
##' analysis with \code{[metafor]{rma}}.
##'
##' @details
##' TODO
##'
##' @param threshold numeric, the threshold.
##' @param var_of_interest character, the interesting variable names.
##' @param dataset dataframe. a dataframe contain the inteeresting variable.
##' @return a vector with high/low versus the threshold.
##'
##' @author Shuangbin Xu
##' @export
##' @examples
##'
##' library("MetaMicrobiome")
##' testfile <- system.file("data", package="MetaMicrobiome", "Baxter_16_alpha_data.csv.gz")
##' data <- read.csv(testfile, header=TRUE, check.names=FALSE)
##' thresVetor <- getthresholds(dataset=data, 
##'			c("Shannon", "Observe", "J"),
##'			type="median")
##' highlowVector <- high_low_vector(dataset=data, 
##'		threshold=thresVetor, 
##'		var_of_interest="Shannon")
##' head(highlowVector)
##' 
high_low_vector <- function(var_of_interest, dataset, threshold){
    	if (length(var_of_interest) >1 ){
		print ("error: the var_of_interest should be one element.")
		break 
	}else{
    		threshold <- threshold[var_of_interest]
	}
	high_low <- factor(ifelse(dataset[,var_of_interest] < threshold,
				     invisible("low"), invisible("high")), levels = c("low", "high"))
	#names(high_low) <- var_of_interest 
	return(high_low)
}



##' @title creat list of vectors with high/low versus the threshold.
##' 
##' @description
##' Creat list of vetors with high/low versus the threshold, suitable for 
##' analysis with \code{[metafor]{rma}}
##'
##' @details
##' TODO
##' 
##' @param threshold vector, the threshold values vector.
##' @param var_of_interest vector, the vector of interesting variables.
##' @param dataset dataframe, the dataframe contained the interesting variables.
##' @return list of vector with high/low versus the threshold.
##' @author Shuangbin Xu
##' @export 
##' @examples
##'
##' library("MetaMicrobiome")
##' testfile <- system.file("data", package="MetaMicrobiome", "Baxter_16_alpha_data.csv.gz")
##' data <- read.csv(testfile, header=TRUE, check.names=FALSE)
##' thresVetor <- getthresholds(dataset=data, 
##'				c("Shannon", "Observe", "J"), 
##'				type="median")
##' multiVariableHL <- MultiHighLow(var_of_interest=c("Shannon", 
##'							     "Observe",
##'							     "J"),
##'					dataset=data, 
##'					threshold=thresVetor)
##' head(multiVariableHL)
##'
##'

MultiHighLow <- function(var_of_interest, dataset, threshold){
    	threshold <- threshold[var_of_interest]
       confTab <- mapply(high_low_vector, var_of_interest,
			    MoreArgs=list(dataset=dataset, threshold=threshold),
			    SIMPLIFY=FALSE) 
	names(confTab) <- var_of_interest 
	return(confTab)
}
