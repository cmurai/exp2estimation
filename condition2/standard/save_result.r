save_result = function(res, fname) {
    df = data.frame()
    for (i in 1:length(res)) {
        tmp = data.frame(
            subject = res[[i]]$subject,
            day = rep(i, length(res[[i]]$subject)),
            beta = res[[i]]$x[,1], 
            alpha = res[[i]]$x[,2],
            lambda = res[[i]]$x[,3],
            wA = res[[i]]$x[,4],
            wB = res[[i]]$x[,5],
            pi = res[[i]]$x[,6],
            rho = res[[i]]$x[,7],
            logpost = res[[i]]$logpost,
            ll = res[[i]]$loglik,
            bic = res[[i]]$bic,
            aic = res[[i]]$aic,
            lml = res[[i]]$lml,
            se_beta = res[[i]]$se[,1],
            se_alpha = res[[i]]$se[,2],
            se_lambda = res[[i]]$se[,3],
            se_wA = res[[i]]$se[,4],
            se_wB = res[[i]]$se[,5],
            se_pi = res[[i]]$se[,6],
            se_rho = res[[i]]$se[,7]
        )
        df = rbind(df, tmp)
    }
    df = df[order(df$day,df$subject),]
    write.csv(df, fname, row.names = FALSE)
}