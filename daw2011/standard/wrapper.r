library(dplyr)
library(Rsolnp)

wrapper = function(data) {
    # load functions
    files = list.files('./mfit', full.names = TRUE)
    sapply(files, source)
    files = grep(list.files('./daw2011/standard', full.names = TRUE), 
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
d_files = list.files('./daw2011/data', pattern = "condition3_day", full.names = TRUE)
res = list()
i = 1
for (d in d_files) {
    data = read.csv(d)
    res[[i]] = wrapper(data)
    print(res[[i]])
    i = i + 1
}

source('./daw2011/standard/kool_data.r')
data = kool_data()

res = wrapper(data)
res$x
