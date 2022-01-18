#-------------------------------------------------------------------#
# 個人レベル分析・固定効果分析の最尤推定・MAP推定を行うための関数の定義 #
#-------------------------------------------------------------------#

# Objective function to be minimized (for single subject parameter fit)
func_minimize_SS = function(param, modelfunc, data, prior){
    ret = modelfunc(param, data, prior)

    # Return negative log-likelihood
    return(ret$negll)
}

#------------------------------#
# Parameter fit single-subject #
#------------------------------#

# 全てのパラメータを個人ごとに推定するためにデータの整理・変数の定義を行う(スライド14)
paramfit = function(modelfunc, df, nParam, prior=NULL){
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
    
    n = 1  # 被験者番号(idxsub) != nSubjectなので、nSubjectに併せて結果を格納していく

    for(idxsub in sublist){
        if (nSubject == 1) {
            subdf = df
        } else {
            subdf = dplyr::filter(df, subject == idxsub)
            cat(paste0("Subject ",idxsub," began at ", Sys.time(), "\n"))
        }
    
        fvalmin = Inf
        T = nrow(subdf)
        
        for(idx in 1:25){
            # set initial value 
            initparam = runif(nParam)
            
            res = solnp(initparam, fun = func_minimize_SS, 
                         modelfunc = modelfunc, 
                         control = list(trace=0), 
                         UB = ublist, LB = lblist, 
                         data = subdf, prior = prior)
            
            nll = res$values[length(res$values)]
            
            if(nll < fvalmin){
                paramest = res$pars
                lp = -nll
                res_MAP_best = res
                fvalmin = nll
                H = res$hessian
            }
        }
        
        neglp[n] = nll
        se[n,] = sqrt(pmax(diag(solve(H)),0) / T)

        paramlist[n,] = paramest

        # ラプラス近似による対数周辺尤度の計算
        lml[n] = lp + nParam/2 * log(2*pi) - 0.5 *log(det(H))
        
        n = n + 1
    }
  return(list(negll = neglp, lml = lml, paramest = paramlist, se = se))
}

# パラメータの上限と下限
ublist = c( Inf, Inf,1.0,1.0, Inf, Inf, Inf)
lblist = c(-Inf,-Inf,0.0,0.0,-Inf,-Inf,-Inf)

