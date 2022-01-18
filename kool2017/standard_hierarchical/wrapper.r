library(dplyr)
library(Rsolnp)
library(MASS)
library(Matrix)

wrapper = function(data) {
    # load functions
    files = list.files('./mfit', full.names = TRUE)
    sapply(files, source)
    files = grep(list.files('./kool2017/standard_hierarchical', full.names = TRUE), 
                 pattern = "wrapper", invert = TRUE, value = TRUE)  # 自分自身を呼び出さないため
    sapply(files, source)

    nstarts = 5

    params = set_params()
    f = function(x,data) MB_MF_rllik(x,data)
    #results = mfit_optimize(f,params,data,nstarts)
    results = mfit_optimize_hierarchical(f,params,data,nstarts)
    return (results)
}

# set data
d_files = list.files('./kool2017/data', pattern = "_condition1_day", full.names = TRUE)
data = read.csv(d_files[1])
tmp = wrapper(data[data[,'subject'] %in% sort(unique(data$subject))[1:3],])

# source('./kool2017/standard/kool_data.r')
# data = kool_data()

res = wrapper(data)
res$x

res01 = list()
for (i in 1:length(d_files)) {
    data = read.csv(d_files[i])
    res01[[i]] = wrapper(data)
    print(res01[[i]])
}
save_result(res01, './kool2017/result/pre_standard.csv')
