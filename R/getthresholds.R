##' @title Creat the thresholds vector. 
##'
##' @description
##' Creat the thresholds for the \code{\link[MetaMicrobiome]{high_low_vector}}
##' @details
##' TODO
##'
##' @param dataset dataframe, a dataframe contain interesting variable.
##' @param var_of_interest character, a vector interesting variables
##' @param type character, the method of choose thresholds, (median or mean), default is median
##'
##' @return a vector thresholds of interesting variable.
##' @author Shuangbin Xu
##' @export
##' @examples
##'
##' library("MetaMicrobiome")
##' data <- read.csv(system.file("data", 
##'          package="MetaMicrobiome", 
##'	      "Baxter_16_alpha_data.csv.gz")) 
##' thresVetor <- getthresholds(dataset=data, 
##'               var_of_interest=c("Shannon", 
##'                                 "Observe"), 
##'               type="median")
##' 
getthresholds <- function(dataset=NULL, 
			     var_of_interest=NULL, 
			     type="median"){
	thresvalue <- apply(dplyr::select(dataset, 
			      dplyr::one_of(var_of_interest)), 
			      2, 
			      function(x){if(type=="median") {median(x)}else{mean(x)}})
	names(thresvalue) <- var_of_interest 
	return(thresvalue)
}
