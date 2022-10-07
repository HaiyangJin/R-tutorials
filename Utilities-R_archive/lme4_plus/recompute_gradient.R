## recompute gradient and Hessian with Richardson extrapolation
# codes are mainly from https://rdrr.io/cran/lme4/man/convergence.html
# Author: Haiyang Jin (https://haiyangjin.github.io/portfolio/)
# please source("get_pars.R") at first

recompute_gradient <- function(lmm) {
    
    devfun <- update(lmm, devFunOnly=TRUE)
    
    pars <- get_pars(lmm)
    
    if (require("numDeriv")) {
        cat("hess:\n"); print(hess <- hessian(devfun, unlist(pars)))
        cat("grad:\n"); print(grad <- grad(devfun, unlist(pars)))
        cat("scaled gradient:\n")
        print(scgrad <- solve(chol(hess), grad))
    }
}

