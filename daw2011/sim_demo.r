sim_demo = function(T = 201, N = 100, estimator = "MAP") {

    if (estimator == 'MAP') {
        prior = list(
            gamma_shape = 1.2, gamma_scale = 5,  # for beta1, beta2 
            beta_a = 1.1, beta_b = 1.1,          # for alpha1, alpha2, lambda, w
            normal_mean = 0, normal_sd = 1       # for p
        )
    } else {prior = NULL}

    x = c(5.19,3.69,0.54,0.42,0.11,0.57,0.39)  # from Daw et al. (2011)
    nParam = length(x)

    cat(paste('... Fitting', estimator, '... \n'))
    results = data.frame(
        matrix(nrow = 0, ncol = nParam+1, 
               dimnames = list(NULL, c('beta1','beta2','alpha1','alpha2','p','lambda','w','negll')))
    )
    se = data.frame(
        matrix(nrow = 0, ncol = nParam,
               dimnames = list(NULL, c('beta1','beta2','alpha1','alpha2','p','lambda','w')))
    )
    t_test = rep(0,7)
    ci_test = rep(0,7)

    for(i in 1:N) {
        R = r_values(T)
        df = rlsim(x,T,R)
        res = paramfit(func_notrandom, df, nParam, prior)

        tmp = matrix(c(res$paramest, res$negll), ncol=(nParam+1), nrow=1)
        colnames(tmp) = c('beta1','beta2','alpha1','alpha2','p','lambda','w','negll')
        results = rbind(results, tmp)

        tmp = matrix(res$se, ncol = nParam, nrow = 1)
        colnames(tmp) = c('beta1','beta2','alpha1','alpha2','p','lambda','w')        
        se = rbind(se, tmp)
        
        t_test = t_test + as.numeric((res$paramest / res$se) >= 1.96)
        ci = matrix(c(res$paramest - res$se, res$paramest + res$se), nrow = nParam, ncol = 2)
        ci_test = ci_test + (ci[,1] <= x) * (x <= ci[,2])
        
        if ((i*10) %% N == 0) {
            tmp1 = paste0(rep("=",10*i/N), collapse = "")
            tmp2 = paste0(rep("-",10*(N - i)/N), collapse = "")
            tmp3 = paste0(i/N*100,'% complete', collapse = "")
            cat(paste(tmp1,tmp2,' ',tmp3, sep=""),'\n')
            if (i == N) {
                cat("Completed!\n")
                rm(tmp1, tmp2, res, tmp)
            }
        }
    }

    boxplot(results[c('beta1','beta2','alpha1','alpha2','p','lambda','w')])
    print(summary(results))
    print('Standard deviation')
    print(apply(results,2,sd))
    print('t-Test (% Significant)')
    print(t_test / N * 100)
    print('Credible Interval Test (% Significant)')
    print(ci_test / N * 100)

    print('Standard errors')
    print(summary(se))
    print(colSums(se == 0))

    return(list(res = results, se = se))
}