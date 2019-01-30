##' @title Calcutate effect sizes via linear (Mixed-Effects) models
##' 
##' @description
##' The function can be used to calculate various effect sizes. See \code{[metafor]{rma}} 
##' and \code{[metafor]{escalc}} details.
##' @details
##' TODO 
##'
##' @param var_of_interest vector, the interesting variables.
##' @param dataset dataframe, the results of multiRunRR. 
##' @param measure character, a character string indicating which effect size or outcome
##' measure should be calculated. See \code{[metafor]{rma}} and \code{[metafor]{escalc}} details.
##' @param methodtype character, the specifying whether a fixed- or a random/mixed-effects 
##' model should be fitted, See \code{[metafor]{rma}} details.
##' @return a results dataframe of the pooled data with the random-effect
##' model ot fixed-effect model. See the \code{metafor{rma}} details.
##' @export
##' @importFrom metafor rma
##' @author Shuangbin Xu
##' @examples
##'
##' library("MetaMicrobiome") 
##' study <- c("Baxter_16", 
##'		"Deng_18", 
##'		"Flemer_17",
##'	      	"Flemer_18",
##'	      	"Hale_17", 
##'		"Mori_18",
##'	      	"Zeller_15")
##' data <- lapply(study, 
##'         function(x){read.csv(system.file("data", 
##'                     package="MetaMicrobiome", 
##'                     paste(x, "_alpha_data.csv.gz", sep="")))})
##' names(data) <- study
##' threholds <- mapply(getthresholds, data, 
##'                     MoreArgs=list(var_of_interest=c("Observe", "Shannon", "J"),
##'                     type="median"), SIMPLIFY=FALSE)
##' multiHighLowVector <- mapply(MultiHighLow, 
##'                          data,
##'                          threholds,
##'                          MoreArgs=list(var_of_interest=c("Observe", 
##'						"Shannon", 
##'						"J")),SIMPLIFY=FALSE)
##'
##' multiRRresult <- mapply(multiRunRR, 
##'                     multiHighLowVector, 
##'                     data, 
##'                     MoreArgs=list(prefix="Group", grouptype="CRC"), 
##'                     SIMPLIFY=FALSE)
##'
##' multistudyRRresult <- mapply(multiVarRRTab, 
##'                             multiRRresult, 
##'                             MoreArgs=list(var_of_interest=c("Observe", 
##'								"Shannon", "J")), 
##'                             SIMPLIFY=FALSE)
##' multistudyRRresult2 <- dplyr::bind_rows(lapply(study,                                                                     
##'                                               function(x) 
##'				dplyr::mutate(multistudyRRresult[[x]], study=x)))
##' 
##' multistudyRRresult2
##' pooledREML <- dplyr::bind_rows(mapply(RunPoolEffect,
##'						c("Observe", "Shannon", "J"),
##'                     MoreArgs=list(dataset=multistudyRRresult2,
##'                                   methodtype="REML"),
##'                     SIMPLIFY=FALSE)) 
##' head(pooledREML)

RunPoolEffect <- function(var_of_interest, dataset, measure="OR", methodtype="REML"){
	dataset <- dplyr::filter(dataset, measure==var_of_interest)
	rr_pooled_test <- metafor::rma(ai=eval(parse(text="tpos")), bi=eval(parse(text="cpos")), 
					ci=eval(parse(text="tneg")), di=eval(parse(text="cneg")), data=dataset,
                                   measure = measure, method = methodtype)
	est <- exp(rr_pooled_test$b[[1, 1]])
	lower <- exp(rr_pooled_test$ci.lb)
	upper <- exp(rr_pooled_test$ci.ub)
       results <- data.frame(measure=var_of_interest, 
				 est=est, lower=lower,
               		 upper=upper,
		 		 pvalue = rr_pooled_test$pval,
               		 I2 = rr_pooled_test$I2, 
				 H2=rr_pooled_test$H2, 
				 QEp=rr_pooled_test$QEp, 
				 study=methodtype, 
				 stringsAsFactors=FALSE)
       return(results)
}
