# Parameter fit
paramfit = function(modelfunc, df, nParam, prior=NULL) {
    if ('subject' %in% df) {
        sublist = dplyr::distinct(df, subject)$subject
    } else {
        sublist = c(1)
    }
    nSubject = length(sublist) 
    
    lml = numeric(nSubject)
    neglp = numeric(nSubject)
    diag.inv = matrix(0, nSubject, nParam)
    se = matrix(0, nSubject, nParam)
    paramlist = matrix(0, nSubject, nParam)
    
    n = 1
    
    for (idxsub in sublist) {
        if (nSubject == 1) {
            subdf = df
        } else {
            subdf = dplyr::filter(df, subject == idxsub)
            cat(paste0("Subject ",idxsub," began at ", Sys.time(), "\n"))
        }
        
        fvalmin = Inf
        T = nrow(subdf)
        
        for(idx in 1:20){
            # set initial value 
            initparam = runif(nParam)
            
            res = solnp(initparam, fun = func_minimize, 
                        modelfunc = modelfunc, 
                        control = list(trace=0),
                        UB = ublist, LB = lblist, 
                        df = subdf, prior = prior)
            
            nll = res$values[length(res$values)]
            
            if(nll < fvalmin){
                paramest = res$pars
                lp = -nll
                res_best = res
                fvalmin = nll
                H = res$hessian
            }
        }
        
        neglp[n] = nll
        se[n,] = sqrt(pmax(diag(solve(H, tol = 10^(-100))),0))
        
        paramlist[n,] = paramest
        
        # ラプラス近似による対数周辺尤度の計算
        lml[n] = lp + nParam/2 * log(2*pi) - 0.5 *log(det(H))
        
        n = n + 1
    }
    return(list(negll = neglp, lml = lml, paramest = paramlist, 
                diag.inv = diag.inv, se = se))
}

ublist <- c(10, 1.0, 10, 10, 10, 1.0, 1.0)
lblist <- c(0.0,0.0,0.0,0.0,-10, 0.0, 0.0)

# Objective function to be minimized 
func_minimize = function(param, modelfunc, df, prior) {
    ret = modelfunc(param,df,prior)
    
    # Return negative log-likelihood
    return (ret$negll)
}