fit_model = function(fun, data, paramfun = NULL) {
    
    sublist = sort(unique(data$subject))
    S = length(sublist)
    if (is.null(paramfun)) {
        paramfun = function() RL_paramfun() 
    }

    # get parameter structure
    param = paramfun()

    # fit model
    R = mfit_optimize(fun, param, data)

    # collect latent variables
    for (s in 1:S) {
        fun(R$x[s,], data[df$subject == sublist[s],])
    }
    empirical_prior = mfit_priorfit(R$x, param)     # estimate prior

    return (list(
        result = R,
        empirical_prior = empirical_prior
    ))
}