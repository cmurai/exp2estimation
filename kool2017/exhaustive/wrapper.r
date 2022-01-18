library(dplyr)
library(Rsolnp)

wrapper = function(data) {
    # load functions
    files = list.files('./mfit', full.names = TRUE)
    sapply(files, source)
    files = grep(list.files('./kool2017/exhaustive', full.names = TRUE), 
                 pattern = "wrapper", invert = TRUE, value = TRUE)  # 自分自身を呼び出さないため
    sapply(files, source)

    nstarts = 100

    params = set_params()
    f = function(x,data) MB_MF_rllik(x,data)
    dataA = dplyr::filter(data, stake == 1)
    resA = mfit_optimize(f,params,dataA,nstarts)
    print(resA$x)
    dataB = dplyr::filter(data, stake != 1)
    dataB$s1 = dataB$s1 - 2
    dataB$s2[!is.na(dataB$s2)] = dataB$s2[!is.na(dataB$s2)] - 2
    resB = mfit_optimize(f,params,dataB,nstarts)
    print(resB$x)
    return (list(A = resA, B = resB))
}

# set data
d_files = list.files('./kool2017/data', pattern = "test_condition", full.names = TRUE)

for (d in d_files) {
    print(paste('=====',d,'====='))
    df = read.csv(d)
    results = wrapper(df)
}
