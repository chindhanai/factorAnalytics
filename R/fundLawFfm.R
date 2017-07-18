install.packages('matlib')
library(matlib)

fundLawFfm = function(ffmObj, ...) {

  # Standardize the residual returns
  returns = t(ffmObj$residuals)
  N = nrow(returns)
  N_TP = ncol(returns)
  sigmaReturn = matrix(0, nrow = N, ncol = N_TP)
  avg <- apply(returns, MARGIN = 1, FUN = mean)
  var_past <- apply(returns, MARGIN = 1, FUN = var)

  for (i in 1:N) {
    ts <- (returns[i, ] - avg[i]) ^ 2
    var_past_2 <- var_past[i]
    sigmaReturn[i, ] <- sapply(ts[2], function(x) var_past_2 <<- 2e-06 + alpha * x + beta * var_past_2)
  }

  sigmaReturn = sqrt(sigmaReturn)
  returns <- (returns - avg) / sigmaReturn

  # Standardized exposures
  Z = ffmObj$Beta
  fmFormula = ffmObj$fm.formula

  # Fit the ffm for the residual returns (TBD)
  # Result: residFfmObj
  data = ffmObj$data
  data[["RETURN"]] = as.vector(returns)
  residFfmObj = fitFfmMod(data = data, addIntercept = ffmObj$addIntercept,
                          asset.var = ffmObj$asset.var, ret.var = ffmObj$ret.var,
                          date.var = ffmObj$date.var, exposure.vars = ffmObj$exposure.vars,
                          z.score = ffmObj$z.score, tsScoreType = ffmObj$tsScoreType,
                          rob.stats = ffmObj$rob.stats, stdReturn = ffmObj$stdReturn)

  # Compute IR
  tsIC = residFfmObj$factor.returns
  IC = apply(coredata(tsIC), MARGIN = 2, mean)
  K = length(IC)
  covIC = residFfmObj$factor.cov
  varResid = 1 - sum(diag(covIC) + IC ^ 2)
  IR = as.numeric(sqrt(t(IC) %*% solve(diag(varResid, nrow = K, ncol = K) / N + covIC) %*% IC))
  if (det(covIC) < 1e-6){
    asympIR = sqrt(t(IC) %*% Ginv(covIC) %*% IC)
  } else {
    asympIR = sqrt(t(IC) %*% solve(covIC) %*% IC)
  }

  alpha = Omega = vector("list", N_TP - 1)
  stockNames = rownames(Z[[1]])
  expoNames = colnames(Z[[1]])
  for (t in 2:N_TP) {
    Lambda = diag(sigmaReturn[, t])
    # Conditional mean forecast of returns
    alpha[[t - 1]] = Lambda %*% Z[[t - 1]] %*% IC
    colnames(alpha[[t - 1]]) = ""
    rownames(alpha[[t - 1]]) = stockNames
    # Conditional forecast error covariance of returns
    Omega[[t - 1]] = Lambda %*% (Z[[t - 1]] %*% covIC %*% t(Z[[t - 1]]) + diag(varResid, nrow = N, ncol = N)) %*% Lambda
    colnames(Omega[[t - 1]]) = rownames(Omega[[t - 1]]) = stockNames
  }
  names(alpha) = names(Omega) = index(tsIC)[-1]

  return(list(IC = IC, varResid = varResid, IR = IR, asympIR = asympIR,
              residFfmObj = residFfmObj,
              alpha = alpha, Omega = Omega))
}
