bayes_prop_ab <- function(prior_alpha,prior_beta,p1,p2,n1,n2,trials) {
alpha1 <- n1*p1
alpha2 <- n2*p2

beta1 <- n1-alpha1
beta2 <- n2-alpha2

beta1 <- rbeta(trials,prior_alpha+alpha1,prior_beta+beta1)
beta2 <- rbeta(trials,prior_alpha+alpha2,prior_beta+beta2)

return(sum(beta2 > beta1)/trials)
}
