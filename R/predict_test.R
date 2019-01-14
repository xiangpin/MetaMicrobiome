#' @title Model Predictions
#'
#' @description
#' predict the test datasets with the results of the models.
#' @details
#' TODO
#'
#' @param model object, a model object for predict.
#' @param teststudy character, the names of the dataset, if the dataset is a list.
#' @param dataset list or dataframe, the test dataset.
#' @param classvariable character, the header name of the train or test dataframe for the classification.
#' @param classtype character, the name of the positive group for classification. 
#' @param Trainstudy chraracter, the name of the origin data of the model.
#'
#' @return a list contianed the predict prob and roc results with sensitivity and specificity.
#' @author Shuangbin Xu
#' @export
#' @importFrom pROC roc 
#' @examples
#'
#'
predict_test <- function(model=NULL, 
			    teststudy=NULL, 
			    dataset=NULL, 
			    classvariable=NULL, 
			    classtype=NULL, 
			    Trainstudy=NULL){
       if (!is.null(teststudy)){
	    if (teststudy %in% names(dataset)){
	       data <- dataset[[teststudy]]
	    }else{
	    	data <- dataset
	    }
	}else{
		data <- dataset
	}
       target <- as.vector(data[[classvariable]]) 
       data[[classvariable]] <- NULL 
       diseasetype <- classtype
       testPred <- predict(model, data, type="prob")
       testPredTarget <- predict(model, data)
       roccurve <- pROC::roc(target, testPred[,diseasetype])
       aucscore <- roccurve$auc
       sensitivity <- roccurve$sensitivities
       specificity <- roccurve$specificities 
       rocplot <- data.frame(Specificity=specificity, Sensitivity=sensitivity, stringsAsFactors=F)
	testPred <- data.frame(testPred, stringsAsFactors=F)
       testPred <- data.frame(cbind(sample=rownames(testPred),testPred), stringsAsFactors=F)
       rocplot$AUC <- aucscore     
	if (!is.null(Trainstudy)){
	       rocplot$TrainStudy <- Trainstudy 
		testPred$TrainStudy <- Trainstudy 
	}
	if (!is.null(teststudy)){
	    	rocplot$TestStudy <- teststudy 
	       rocplot$group <- paste(rocplot$TestStudy, round(rocplot$AUC, 4), sep=" AUC:")
		testPred$TestStudy <- teststudy 
	}
       result <- list(rocplot=rocplot, PredProb=testPred)
       return (result)
             
}



#' @title model predictions
#' 
#' @description 
#' predict the multi-test datasets with the results of the multi-models.
#' @details
#' TODO
#'
#' @param study character, the names of the specified test dataset.
#' @param dataset list, a list contained multi test datasets.
#' @param models list, a list contained multi model object for predictions.
#' @param classvariable character, the header name of the train of test dataframe 
#' for the classification.
#' @param classtype character, the name of the positive group for classification.
#' @author Shuangbin Xu
#' @export
#' @examples
#'
#'

get_mapply_predict_test <- function(study=NULL, 
					 dataset=NULL, 
					 models=NULL, 
	  			        classvariable=NULL, 
				        classtype=NULL){
    	trainmodel <- models[[study]]
	subdata <- dataset[setdiff(names(dataset), study)]
	leavestudy <- setdiff(names(dataset), study)
	predict_result <- mapply(predict_test,
				    leavestudy,
				    MoreArgs=list(model=trainmodel,
						    dataset=subdata,
						    classvariable=classvariable,
						    classtype=classtype,
						    Trainstudy=study),
				    SIMPLIFY=FALSE)
	return(predict_result)
}
