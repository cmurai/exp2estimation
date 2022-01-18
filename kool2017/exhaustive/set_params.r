set_params = function() {
    param = list()

    g = c(4.82, 0.88)  # parameters of gamma prior
    param[[1]] = list(
        name = 'inverse temperature',
        logpdf = function(x) sum(dgamma(x, shape = g[1], rate = g[2], log = TRUE)),
        lb = 0,  # lower bound
        ub = 20  # upper bound
    )

    param[[2]] = list(
        name = 'learning rate',
        logpdf = function(x) 0,
        lb = 0,
        ub = 1
    )

    param[[3]] = list(
        name = 'eligibility trace decay',
        logpdf = function(x) 0,
        lb = 0,
        ub = 1
    )

    param[[4]] = list(
        name = 'mixing weight',
        logpdf = function(x) 0,
        lb = 0,
        ub = 1
    )

    mu = 0.15; sd = 1.42  # parameters of choice stickiness
    param[[5]] = list(
        name = 'choice stickiness',  # pi
        logpdf = function(x) sum(dnorm(x, mu, sd, log = TRUE)),
        lb = -20,
        ub = 20
    )

    param[[6]] = list(
        name = 'response stickiness',  # rho
        logpdf = function(x) sum(dnorm(x, mu, sd, log = TRUE)),
        lb = -20,
        ub = 20
    )

    return (param)
}