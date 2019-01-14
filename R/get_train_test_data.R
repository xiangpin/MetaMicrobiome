#' @title get train and test data 
#' 
#' @description
#' Build the train and test datasets with a multi-datasets.
#' @details
#' TODO
#' 
#' @param test_study character, the names of the list datasets.
#' @param datasets list, a list contianed the multi-dataframes with features and target infromation.
#'
#' @return a list contianed the test datasets and train datasets.
#' @author Shuangbin Xu
#' @export
#' @examples
#'
#'
#' 
get_train_test_data <- function(test_study=NULL, datasets=NULL){
       testdata <- datasets[test_study]
       trainstudy <- setdiff(names(datasets), test_study)
       traindata <- dplyr::bind_rows(datasets[trainstudy])
       resultdata <- list(testdata=testdata, traindata=traindata)
       return(resultdata)
}
