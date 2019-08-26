#' Plots lifts from getLift function
#' 
#'  Plots lifts ffrom getLift function
#'  @param lifts named list with lifts from getLift
#'  @param ... lifts from getLift
#'  @cuts points to show exact value of lifts
#'  @value column to be ploted, cum_lift, lift, cum_response, response
#'  
#' @examples 
#' score1 = runif(1000)
#' response1 = (score1 + rnorm(1000,0,0.1)) > 0.5
#' lift1 = getLift(score1,response1)
#' score2 = runif(1000)
#' response2 = (score2 + rnorm(1000,0,0.3)) > 0.5
#' lift2 = getLift(score2,response2)
#' lifts = list(model1 = lift1, model2 = lift2)
#' plotLift(lifts)
#' #or
#' plotLift(model1 = lift1, model2 = lift2) 
#' 
#' @author Daniel Fischer
#' @export


plotLift = function(lifts = list(),...,cuts = c(5,15), value = "cum_lift"){
  safeLibrary(purrr)
  safeLibrary(ggplot2)
  safeLibrary(gridExtra)
  
  if(class(lifts) != "list"){
    lifts = list(model = lifts)
  }
  extra_lifts = list(...)
  for(model in names(extra_lifts)){
    lifts[[model]] = extra_lifts[[model]]
  }
  
  dataset = map2_dfr(names(lifts), lifts, function(model,values){
    values$model = model
    values[,"value"] = values[,value]
    return(values)
  })
  
  hline = switch (value,
                  cum_lift = 1,
                  lift = 1,
                  response_rate = sum(dataset[nrow(dataset),"cum_response"]),
                  cum_response = sum(dataset[nrow(dataset),"cum_response"])
  )
  
  gr = ggplot(dataset,aes(percentil, value,color = model)) + 
    geom_line(size = 1.5) + 
    geom_abline(slope=0, intercept=hline,size=1.5) +
    ylab(value)
   
   if(length(cuts) > 0 ){
     gr = gr + geom_vline(xintercept = cuts)
     tabla = filter(dataset,percentil %in% cuts) 
     gr = gr + geom_label(aes(label = round(value,2),fill=model),tabla,color = "black")
   }
   
   return(gr)
}

