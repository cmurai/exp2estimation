mfit_optimize_hierarchical = function(likfun, param, df, nstarts = 2, parallel = FALSE) {

    if ('subject' %in% colnames(df)) {
        sublist = sort(unique(df$subject))
        N = nrow(df[df[,'subject'] == sublist[1],])
    } else {
        sublist = c(1)
        N = nrow(df)
    }

    # initialization
    tol = 1e-3
    maxiter = 20
    iter = 0
    K = length(param)
    S = length(sublist)

    L = numeric(S)
    goodHessian = numeric(S)
    lme = numeric(maxiter)

    # extract lower and upper bounds
    lb = c(); ub = c()
    for (k in 1:K) {
        lb = append(lb, ifelse(is.null(param[[k]]$lb), -Inf, param[[k]]$lb))
        ub = append(ub, ifelse(is.null(param[[k]]$ub), Inf, param[[k]]$ub))
    }

    # group-level initial parameters
    if (all(is.infinite(lb)) && all(is.infinite(ub))) {
        m = rnorm(K)
        v = rep(100,K)
    } else {
        m = ub + 0.5*(ub-lb)
        v = ub - lb
    }

    # identify link function is default
    if (is.null(param[[1]]$link)) {
        for (k in 1:K) {
            param[[k]]$link = function(x) x
        }
    }

    # run expectation-maximization 
    while (iter < maxiter) {

        iter = iter + 1
        message(paste('iteration', iter))

        # construct prior
        for (k in 1:K) {
            param[[k]]$logpdf = function(x) -0.5 * ((param[[k]]$link(x) - m[k] / sqrt(v[k]))^2 - log((sqrt(2*pi) * sqrt(v[k]))))
        }

        # E-step: find individual parameter estimates
        if (parallel) {
            results = mfit_optimize_parallel(likfun, param, df, nstarts)
        } else {
            results = mfit_optimize(likfun, param, df, nstarts)
        }

        # transform parameters to (-Inf, Inf)
        x_orig = results$x
        for (k in 1:K) {
            results$x[,k] = param[[k]]$link(results$x[,k])
        }

        # M-step: update group-level parameters
        v = numeric(K)
        for (s in 1:S) {
            v = v + results$x[s,]^2 + diag(ginv(results$H[[s]], tol = 0))
            tryCatch(
                {
                    h = logdet(results$H[[s]],'chol')
                    L[s] = results$logpost[s] + 0.5*(results$K * log(2*pi) - h)  # calculate lml
                    goodHessian[s] = 1
                }, error = function(e) {
                    message('Hessian is not positive definite')
                    tryCatch(
                        {
                            h = logdet(results$H[s])
                            L[s] = results$logpost[s] + 0.5*(results$K * log(2*pi) - h)  # calculate lml
                            goodHessian[s] = 0
                        }, error = function(e) {
                            message('could not calculate')
                            L[s] = NaN
                            goodHessian[s] = -1
                        }
                    )
                }
            )
        }
        
        m = colMeans(results$x, na.rm = TRUE)
        v = pmax(1e-5, v/S - m^2)               # make sure variances don't get too small
        L[is.na(L)] = mean(L, na.rm = TRUE)     # interpolate to avoid imaginary numbers
        lme[iter] = sum(L) - K*log(N)

        result = list(
            group = list(m = m, v = v), 
            lme = lme,
            goodHessian = goodHessian,
            x = x_orig
        )

        if (iter > 1 && abs(lme[iter] - lme[iter-1]) < tol) {
            return (results)
        }
    }
    return (result)
}