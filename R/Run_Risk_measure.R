##' @title Summary measures base on epiR
##' 
##' @description
##' Computes summary measures of risk and a chi-squared test for difference 
##' in the observed proportions from count data presented in a 2 by 2 table
##' with high low vector. With multiple strata the function returns crude 
##' and Mantel-Haenszel adjusted measures of association and chi-squared 
##' tests of homogeneity(based on \code{[epiR]{epi.2by2}}.
##' @details
##' TODO
##'
##' @param var_high_low list or vector, the list of vectors with high/low versus the threshold
##' @param metadavector dataframe, metada dataframe contained the group information. 
##' @param prefix character, the header names of the metada data frame, default is 'Group'.
##' @param grouptype character, the positive group names, default is "Case".
##' @param method characer, a character string indicating the study design on which the
##' tabular data has been based. Options are \code{cohort.count}, \code{cohort.time}, \code{case.control}, 
##' or \code{cross.sectional}, default is \code{cohort.count}. See \code{\link[epiR]{epi.2by2}} for details.
##' @param conf.level numeric, magnitude of the returned confidence intervals. Must be a
##' single number between 0 and 1. See \code{\link[epiR]{epi.2by2}} for details.
##' @param score character, Wald and score confidence intervals for the effect 
##' value for each strata, default is \code{OR.strata.score}, See \code{\link[epiR]{epi.2by2}} for details .
##' @param ... Additional arguments passed to \code{\link{epi.2by2}}.
##' @return the summary measures of risk and a chi-squared test 
##' @export
##' @importFrom epiR epi.2by2
##' @author Shuangbin Xu
##' @examples
##' 
##'
##' 
run_rr <- function(var_high_low, metadavector, prefix="Group", grouptype="Case", 
		     method="cohort.count", conf.level=0.95, score="OR.strata.score",...){
	metadavector <- factor(ifelse(metadavector[,prefix]==grouptype, 
				invisible("Y"), invisible("N")), 
				  levels=c("Y", "N"))
	contingency <- table(var_high_low, metadavector)
	tryCatch({RRtest <- epiR::epi.2by2(contingency, method="cohort.count", ...);},
	    error =function(e){RRtest <- NA})
	tryCatch({test_values <- cbind(RRtest$massoc[[score]],
				pvalue = RRtest$massoc$chisq.strata$p.value);},
		  error=function(e){test_values <- NA})
	tryCatch({combined_data <- list(continTab = contingency, test_values = test_values);}, 
	    error = function(e){combined_data <- NA})
	return(combined_data)
}



##' @title Summary measures base on epiR for multi variables
##' 
##' @description
##' Computes summary measures of risk and a chi-squared test for difference
##' in the observed proportions from count data presented in a 2 by 2 table
##' with high low vector. With multiple strata the function returns crude
##' and Mantel-Haenszel adjusted measures of association and chi-squared
##' tests of homogeneity for multi variables (based on \code{[epiR]{epi.2by2}}.
##' 
##' @details
##' TODO
##' 
##' @param multiHighLow list, mulit variable high-lower-vector.
##' @param metadavector dataframe,  metada dataframe contained the group information.
##' @param prefix character, the header names of the metada data frame, default is 'Group'.
##' @param grouptype character, the positive group names, default is "Case".
##' @param method characer, a character string indicating the study design on which the
##' tabular data has been based. Options are \code{cohort.count}, \code{cohort.time}, \code{case.control},
##' or \code{cross.sectional}, default is \code{cohort.count}. See \code{\link[epiR]{epi.2by2}} for details.
##' @param conf.level numeric, magnitude of the returned confidence intervals. Must be a
##' single number between 0 and 1. See \code{\link[epiR]{epi.2by2}} for details.
##' @param score character, Wald and score confidence intervals for the effect
##' value for each strata, default is \code{OR.strata.score}, See \code{\link[epiR]{epi.2by2}} for details .
##' @param ... Additional arguments passed to \code{\link[epiR]{epi.2by2}}.
##' @return the summary measures of risk and a chi-squared test
##' @author ShuangbinXu
##' @export 
##' @examples
##' 
##' 
##'

multiRunRR <- function(multiHighLow=NULL, metadavector=NULL, prefix="Disease", grouptype="Case",
			  method="cohort.count", conf.level=0.95, score="OR.strata.score", ...){
	obtain_rr <- lapply(multiHighLow,
			      function(x) run_rr(var_high_low=x, 
						    metadavector=metadavector,
						    prefix=prefix, grouptype=grouptype, 
						    method=method, conf.level=conf.level,
						    score=score,...))
	return(obtain_rr)

}

##' @title collate results for summary measure 
##' 
##' @description
##' Collate results for summary measure
##' @details
##' TODO
##'
##' @param multiRunRRTab list, the results of 
##' @param var_of_int vector, interesting variables
##' @return a results of dataframe for summary measure.
##' @author ShuangbinXu
##' @export
##' @examples
##' 
##'
##'
tidy_data <- function(multiRunRRTab=NULL, var_of_int=NULL){
	testdata <- data.frame(multiRunRRTab[[var_of_int]][["test_values"]])
	countdata <- data.frame(multiRunRRTab[[var_of_int]][["continTab"]])
	countdata <- tidyr::unite(countdata, "group", colnames(countdata)[1:2])
	rownames(countdata) <- countdata$group
	countdata$group <- NULL
	countdata <- data.frame(t(countdata))
	colnames(countdata) <- c("tpos", "tneg", "cpos", "cneg")
	resultdata <- data.frame(cbind(measure=var_of_int, testdata, countdata))
	return(resultdata)
}



##' @title collate results for summary measure of multi-variables 
##' 
##' @description
##' Collate results for summary measure of multi-variables.
##' @details
##' TODO
##'
##' @param multiRunRRTab list, the results of the 
##' @param var_of_interest vector, the interesting variables.
##' @return 
##'
##' @author Shuangbin Xu
##' @export
##' @examples
##' 
##'
##'

multiVarRRTab <- function(multiRunRRTab=NULL, var_of_interest=NULL){
	dat <- dplyr::bind_rows(mapply(tidy_data, var_of_interest, 
		MoreArgs=list(multiRunRRTab=multiRunRRTab),
		SIMPLIFY=FALSE))
	return(dat)
}
