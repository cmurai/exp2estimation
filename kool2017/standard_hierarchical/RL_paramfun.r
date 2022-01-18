RL_paramfun = function(mode = 'uniform') {
    param = list()

    gammaLL = function(par,x) -sum(dgamma(x, par[1], par[2], log = TRUE))
    betaLL = function(par,x) -sum(dbeta(x, par[1], par[2], log = TRUE))
    normLL = function(par,x) -sum(dnorm(x, par[1], par[2], log = TRUE))

    param[[1]] = list(
        name = 'inverse temperature',
        hp = c(3,2),
        logpdf = function(x) sum(dgamma(x, shape = param[[1]]$hp[1], rate = param[[1]]$hp[2], log = TRUE)),
        lb = 0,   # lower bound
        ub = 20,  # upper bound
        fit = function(x) {
            R = optim(
                runif(length(param[[1]]$hp)), fn = gammaLL, x = x, 
                method = 'L-BFGS-B', hessian = TRUE
            )
            return (R$par)
        }
    )

    param[[2]] = list(
        name = 'learning rate',
        hp = c(1.2, 1.2),
        logpdf = function(x) sum(dbeta(x, param[[2]]$hp[1], param[[2]]$hp[2], log = TRUE)),
        lb = 0,
        ub = 1,
        fit = function(x) {
            R = optim(
                runif(length(param[[2]]$hp)), fn = betaLL, x = x, 
                method = 'L-BFGS-B', lower = 0, hessian = TRUE
            )
            return (R$par)
        }
    )

    param[[3]] = list(
        name = 'eligibility trace decay',
        logpdf = function(x) 0,
        lb = 0,
        ub = 1
    )

    param[[4]] = list(
        name = 'mixing weight low',
        logpdf = function(x) 0,
        lb = 0,
        ub = 1
    )

    param[[5]] = list(
        name = 'mixing weight high',
        logpdf = function(x) 0,
        lb = 0,
        ub = 1
    )

    param[[6]] = list(
        name = 'choice stickiness',  # pi
        hp = c(0,3),
        logpdf = function(x) sum(dnorm(x, param[[6]]$hp[1], param[[6]]$hp[2], log = TRUE)),
        lb = -10,
        ub = 10,
        fit = function(x) {
            R = optim(
                runif(length(param[[6]]$hp)), fn = normLL, x = x, 
                method = 'L-BFGS-B', hessian = TRUE
            )
            return (R$par)
        }
    )

    param[[7]] = list(
        name = 'response stickiness',  # rho
        hp = c(0,3),
        logpdf = function(x) sum(dnorm(x, param[[6]]$hp[1], param[[6]]$hp[2], log = TRUE)),
        lb = -10,
        ub = 10,
        fit = function(x) {
            R = optim(
                runif(length(param[[6]]$hp)), fn = normLL, x = x, 
                method = 'L-BFGS-B', hessian = TRUE
            )
            return (R$par)
        }
    )

    if (mode == 'uniform') {
        for(i in 1:length(param)) {
            param[[i]]$logpdf = function(x) 0
        }
    } else if (mode == 'empirical') {
        
    }

    return (param)
}