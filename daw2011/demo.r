# ----- generate simulation data ----- #

files = list.files('./daw2011', pattern = "*.r", full.names = TRUE)
sapply(files[2:5], source)

library(Rsolnp)

# simulation parameters
N = 201
x = c(5.19,3.69,0.54,0.42,0.11,0.57,0.39)
nParam = length(x)
S = 1

# simulate data from RL agent on Daw et al. (2011) task
R = r_values(N)
df = rlsim(x,N,R)


# ---- fit models ---- #
cat('... Fitting model ...')
cat('-- MLE Estimator --')
mle_results = data.frame(
    matrix(nrow = 0, ncol = 8, 
           dimnames = list(NULL, c('beta1','beta2','alpha1','alpha2','p','lambda','w','negll')))
)
mle_test = rep(0,7)
for (i in 1:100) {
    res = paramfit(func_notrandom, df, nParam)
    tmp = matrix(c(res$paramest, res$negll), ncol=8, nrow=1)
    colnames(tmp) = c('beta1','beta2','alpha1','alpha2','p','lambda','w','negll')
    mle_results = rbind(mle_results, tmp)
    mle_test = mle_test + as.numeric((res$paramest / res$se) >= 1.96)
    if (i %% 10 == 0) {
        tmp1 = paste(rep("■",(i/10)), collapse="")
        tmp2 = paste(rep("□",(10 - i/10)), collapse="")
        cat(paste(tmp1,tmp2, sep=""),'\n')
        if(i==100) {
            cat("Completed!\n")
            rm(tmp1, tmp2, res, tmp)
        }
    }
}

boxplot(mle_results[c('beta1','beta2','alpha1','alpha2','p','lambda','w')])
summary(mle_results)
apply(mle_results,2,sd)

print(mle_test)


cat('-- MAP estimator --')
prior <- list(
    gamma_shape = 1.2, gamma_scale = 5,  # for beta1, beta2
    beta_a = 1.1, beta_b = 1.1,          # for alpha1, alpha2, lamb, w
    normal_mean = 0, normal_sd = 1       # for p
)

map_results = data.frame(
    matrix(nrow = 0, ncol = 8, 
           dimnames = list(NULL, c('beta1','beta2','alpha1','alpha2','p','lambda','w','negll')))
)
map_test = rep(0,7)
ci_test = rep(0,7)
for (i in 1:100) {
    R = r_values(N)
    df = rlsim(x,N,R)
    res = paramfit(func_notrandom, df, nParam, prior)
    tmp = matrix(c(res$paramest, res$negll), ncol=8, nrow=1)
    colnames(tmp) = c('beta1','beta2','alpha1','alpha2','p','lambda','w','negll')
    map_results = rbind(map_results, tmp)
    map_test = map_test + as.numeric((res$paramest / res$se) >= 1.96)
    ci = matrix(c(res$paramest - res$se, res$paramest + res$se), nrow = 7, ncol = 2)
    ci_test = ci_test + (ci[,1] <= x) * (x <= ci[,2])
    if (i %% 10 == 0) {
        tmp1 = paste(rep("■",(i/10)), collapse="")
        tmp2 = paste(rep("□",(10 - i/10)), collapse="")
        cat(paste(tmp1,tmp2, sep=""),'\n')
        if(i==100) {
            cat("Completed!\n")
            rm(tmp1, tmp2, res, tmp)
        }
    }
}

boxplot(map_results[c('beta1','beta2','alpha1','alpha2','p','lambda','w')])
summary(map_results)
apply(map_results,2,sd)

print(map_test)
print(ci_test)
