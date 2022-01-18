mfit_optimize = function(likfun, param, df, nstarts = 5) {
    # results = list()

    if ('subject' %in% colnames(df)) {
        sublist = sort(unique(df$subject))
    } else {
        sublist = c(1)
    }
    S = length(sublist) 

    K = length(param)

    # save info to results structure
    x = matrix(0,S,K)
    logpost = numeric(S)
    loglik = numeric(S)
    bic = numeric(S)
    aic = numeric(S)
    # lml = numeric(S)
    # se = matrix(0,S,K)
    H = list()

    # extract lower and upper bounds
    lb = c(); ub = c()
    for (i in 1:length(param)) {
        lb = append(lb, ifelse(is.null(param[[i]]$lb), -Inf, param[[i]]$lb))
        ub = append(ub, ifelse(is.null(param[[i]]$ub), Inf, param[[i]]$ub))
    }

    if (!is.null(param[[1]]$x0)) nstarts = length(param[[1]]$x0)

    for (s in 1:S) {
        idxsub = sublist[s]
        subdf = df[df[,'subject'] == idxsub,]
        cat(paste("Subject",idxsub,"began at", Sys.time(), "\n"))

        # construct posterior function 
        f = function(x,df) -mfit_post(x,param,df,likfun)

        for (i in 1:nstarts) {
            # set initial value for parameters
            x0 = c()
            if (!is.null(param[[1]]$x0)) {
                for (j in 1:length(param)) {
                    x0[j] = param[[j]]$x0[i]
                }
            } else {
                for (j in 1:length(param)) {
                    x0[j] = runif(1,lb[j],ub[j])
                }
            }
            # conduct minimization
            res = solnp(x0, fun = f, df = subdf, 
                        control = list(trace=0),
                        UB = ub, LB = lb)
            logp = -res$values[length(res$values)]

            if (i == 1 || logpost[s] < logp) {
                logpost[s] = logp            # 事後分布を考慮した事後対数尤度
                x[s,] = res$pars             # パラメータの推定値
                H[[s]] = res$hessian         # ヘッセ行列
                # se[s,] = sqrt(pmax(diag(solve(H, tol = 10^(-100))),0))  # 標準誤差
            }
        }
        loglik[s] = likfun(x[s,],subdf)  # 対数尤度
        bic[s] = K*log(length(subdf)) - 2*loglik[s]
        aic[s] = K*2 - 2*loglik[s]
        # lml[s] = logpost[s] + K/2 * log(2*pi) - 0.5*log(det(H))
        
        # print(paste(idxsub, logpost[s], loglik[s], bic[s], aic[s], lml[s]))
        # print(x[s,])
    } 
    return (list(
        K = K, param = param, likfun = likfun,
        x = x, 
        subject = sublist,
        logpost = logpost, 
        loglik = loglik,
        bic = bic,
        aic = aic,
        # lml = lml,
        # se = se
        H = H
    ))
}