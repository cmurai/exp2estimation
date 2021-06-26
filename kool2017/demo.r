# ----- generate simulation data ----- #

setwd("C:/Users/chie9/python/exploration_exploitation/kool2017")
files = list.files()
sapply(files[2:5], source)

library(Rsolnp)

# simulation parameters 
N = 200   # number of trials per subject

# parameter values for each agent 
nParam = 7  # number of parameters
S = 1       # number of subjects
x = matrix(
    c(c(0.64,0.82,0.25,0.17,-0.12,0.62,0.95)),
    nrow = S, ncol = nParam
)

# simulate data from RL agent on Kool et al. (2017) task
df = data.frame()
testdf = data.frame()
for (s in 1:S) {
    R = r_values(N)
    df = rlsim(x[s,], R, N)
    testdf = rlsim(x[s,], R, N)
}

# ----- fit models ----- #

# create parameter structure 
# param = 

# g = c(4.82, 0.88)     # gamma prior for beta
# param[1]$name = 'inverse temperature'
# param[1]$logpdf = function(x) sum(log(rgamma(x,g[1], g[2])))
# param[1]$lb = 0   # lower bound 
# param[1]$ub = 50  # upper bound

# mu = 0.15; sd = 1.42  # normal prior for pi, rho
# param[2]$name = 'stickiness'
# param[2]$logpdf = function(x) sum(log(rnorm(x,mu,sd)))
# param[2]$lb = -Inf
# param[2]$ub = Inf

# param[3]$name = 's'


# run optimization 
nstarts = 2  # number of random parameter initializations 
cat('... Fitting model ...')
# results = mfit_optimize(rllik, param, df, nstarts)
source(files[3])
results1 = data.frame(
    matrix(ncol=7,nrow=0, 
           dimnames=list(NULL, c('beta','alpha','lambda','pi','rho','w_l','w_h'))))
test1 = rep(0,7)
for (i in 1:100){
    res = paramfit(mfit_optimize, df, nParam)
    tmp = res$paramest
    colnames(tmp) = c('beta','alpha','lambda','pi','rho','w_l','w_h')
    results1 = rbind(results1, tmp)
    test1 = test1 + as.numeric((res$paramest / res$se) > 1.96)
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
print(test1)
boxplot(results1)
summary(results1)
apply(results1,2,sd)

results2 = data.frame(
    matrix(ncol=7,nrow=0, 
           dimnames=list(NULL, c('beta','alpha','lambda','pi','rho','w_l','w_h'))))
test2 = rep(0,7)
for (i in 1:100){
    res = paramfit(mfit_optimize, df, nParam)
    tmp = res$paramest
    colnames(tmp) = c('beta','alpha','lambda','pi','rho','w_l','w_h')
    results2 = rbind(results2, tmp)
    test2 = test2 + as.numeric((res$paramest / res$se) > 1.96)
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
print(test2)
boxplot(results2)
summary(results2)
apply(results2,2,sd)

write.csv(df, file = 'data/210626_choices01.csv', row.names = FALSE)
write.csv(testdf, file = 'data/210626_choices02.csv', row.names = FALSE)

write.csv(results1, file = 'data/result/210626_mle01.csv', row.names = FALSE)
write.csv(results2, file = 'data/result/210626_mle02.csv', row.names = FALSE)

# conduct logistic regression for choices

# compute predictive probability for the two models on test data

