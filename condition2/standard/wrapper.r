library(dplyr)
library(Rsolnp)

wrapper = function(data) {
    # load functions
    files = list.files('./mfit', full.names = TRUE)
    sapply(files, source)
    files = grep(list.files('./condition2/standard', full.names = TRUE), 
                 pattern = "wrapper", invert = TRUE, value = TRUE)  # 自分自身を呼び出さないため
    sapply(files, source)

    nstarts = 100
    N = 200

    params = set_params()
    f = function(x,data) MB_MF_rllik(x,data)
    results = mfit_optimize(f,params,data,nstarts)
    return (results)
}

# set data
d_files = list.files('./condition2/data', pattern = "_condition2_day", full.names = TRUE)

res02 = list()
for (i in 1:length(d_files)) {
    data = read.csv(d_files[i])
    res02[[i]] = wrapper(data)
    print(res02[[i]])
}
save_result(res02, './condition2/result/pre_standard.csv')

for(i in 1:length(res02)) {
    print('Day ')
}