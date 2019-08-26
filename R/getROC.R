#' Calculates ROC Curve
#'
#' Calculates the ROC curve
#' @param score a numeric vector with the scores from the prodictive model
#' @param response a boolean (TRUE or FALSE) vector with the real values from the dataset
#' @return dataframe with the values of the ROC curve.
#' @examples 
#' score = runif(1000)
#' response = (score + rnorm(1000,0,0.1)) > 0.5
#' ROC = getROC(score, response)
#' plot(ROC$false_positive,ROC$true_positive,type="l")
#' 
#' @export


getROC = function(score, response){
  dataset = data.frame(score, response)
  dataset = dataset[order(score,decreasing = T),]
  dataset = transform(
    dataset,
    true_positive = cumsum(response) / sum(response),
    false_positive = cumsum(!response) / sum(!response),
    identity = seq(from = 0, to = 1, length = nrow(dataset))
  )
  return(dataset)
}