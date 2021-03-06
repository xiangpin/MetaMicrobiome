% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RunPooled.R
\name{RunPoolEffect}
\alias{RunPoolEffect}
\title{Calcutate effect sizes via linear (Mixed-Effects) models}
\usage{
RunPoolEffect(var_of_interest, dataset, measure = "OR",
  methodtype = "REML")
}
\arguments{
\item{var_of_interest}{vector, the interesting variables.}

\item{dataset}{dataframe, the results of multiRunRR.}

\item{measure}{character, a character string indicating which effect size or outcome
measure should be calculated. See \code{[metafor]{rma}} and \code{[metafor]{escalc}} details.}

\item{methodtype}{character, the specifying whether a fixed- or a random/mixed-effects 
model should be fitted, See \code{[metafor]{rma}} details.}
}
\value{
a results dataframe of the pooled data with the random-effect
model ot fixed-effect model. See the \code{metafor{rma}} details.
}
\description{
The function can be used to calculate various effect sizes. See \code{[metafor]{rma}} 
and \code{[metafor]{escalc}} details.
}
\details{
TODO
}
\examples{

library("MetaMicrobiome") 
study <- c("Baxter_16", 
	"Deng_18", 
	"Flemer_17",
      	"Flemer_18",
      	"Hale_17", 
	"Mori_18",
      	"Zeller_15")
data <- lapply(study, 
        function(x){read.csv(system.file("data", 
                                          package="MetaMicrobiome", 
                                          paste(x, "_alpha_data.csv.gz", sep="")))})
names(data) <- study
threholds <- mapply(getthresholds, data, 
                    MoreArgs=list(var_of_interest=c("Observe", "Shannon", "J"),
                    type="median"), SIMPLIFY=FALSE)
multiHighLowVector <- mapply(MultiHighLow, 
                         data,
                         threholds,
                         MoreArgs=list(var_of_interest=c("Observe", 
					"Shannon", 
					"J")),SIMPLIFY=FALSE)

multiRRresult <- mapply(multiRunRR, 
                    multiHighLowVector, 
                    data, 
                    MoreArgs=list(prefix="Group", grouptype="CRC"), 
                    SIMPLIFY=FALSE)

multistudyRRresult <- mapply(multiVarRRTab, 
                            multiRRresult, 
                            MoreArgs=list(var_of_interest=c("Observe", 
							"Shannon", "J")), 
                            SIMPLIFY=FALSE)
multistudyRRresult2 <- dplyr::bind_rows(lapply(study,                                                                     
                                              function(x) 
			dplyr::mutate(multistudyRRresult[[x]], study=x)))

multistudyRRresult2
pooledREML <- dplyr::bind_rows(mapply(RunPoolEffect,
					c("Observe", "Shannon", "J"),
                                      MoreArgs=list(dataset=multistudyRRresult2,
                                      methodtype="REML"),
                                      SIMPLIFY=FALSE)) 
head(pooledREML)
}
\author{
Shuangbin Xu
}
