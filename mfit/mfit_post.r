# function logp = mfit_post(x,param,data,likfun)
    
#     % Evaluate log probability of parameters under the (unnormalized) posterior.
#     %
#     % USAGE: logp = mfit_post(x,param,data,likfun)
#     %
#     % INPUTS:
#     %   x - parameter values
#     %   param - parameter structure
#     %   data - data structure
#     %   likfun - function handle for likelihood function
#     %
#     % OUTPUTS:
#     %   logp - log unnormalized posterior probability
#     %
#     % Sam Gershman, July 2015

#     logp = likfun(x,data);
    
#     if isfield(param,'logpdf')
#         for k = 1:length(param)
#             logp = logp + param(k).logpdf(x(:,k));
#         end
#     end

mfit_post = function(x,param,data,likfun) {
    logp = likfun(x,data);

    if (!is.null(param[[1]]$logpdf)) {
        for (k in 1:length(param)) {
            logp = logp + param[[k]]$logpdf(x[k]);
        }
    }
    return (logp)
}