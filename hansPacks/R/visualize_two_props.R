#' A visualization of two proportions using 5million samples
#'
#' This function allows you to quickly visualize the sampled beta distributions from two proportions
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords bayes, ab test, proportion
#' @export
#' @examples
#' visualize_two_props()

visualize_two_props <- function(prior_alpha,prior_beta,p1,p2,n1,n2,trials) {
alpha1 <- n1*p1
alpha2 <- n2*p2

beta1 <- n1-alpha1
beta2 <- n2-alpha2

beta1 <- data.frame(value=rbeta(trials,prior_alpha+alpha1,prior_beta+beta1))
beta1$sample <- 'proportion 1'
beta2 <- data.frame(value=rbeta(trials,prior_alpha+alpha2,prior_beta+beta2))
beta2$sample <- 'proportion 2'

data <- rbind(beta1,beta2)

view <- ggplot(data,aes(x=value,colour=sample)) + geom_density() + ylab('density') + xlab('proportion value') + theme(legend.position='bottom', legend.title=element_blank()) + scale_x_continuous(labels=scales::percent)

return(view)
}
