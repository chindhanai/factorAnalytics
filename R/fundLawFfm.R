library(matlib)
fundLawFfm = function(data, asset.var, ret.var, date.var, exposure.vars,
                      benchmarkData, fit.method = c("LS", "Rob"), 
                      tsScoreType = "EWMA", alpha = 0.1, beta = 0.81,...) {

  fit.method = fit.method[1]
  # Preprocessing to obtain the residual returns
  data[[date.var]] <- as.Date(data[[date.var]])
  time.periods <- unique(data[[date.var]])     # Extract unique time periods from data
  N_TP <- length(time.periods)
  if (N_TP < 2) {
    stop("Invalid args: at least 2 unique time periods are required to fit the
         factor model")
  }
  
  # Order data.frame by date.var
  data <- data[order(data[, date.var]), ]
  
  # Extract asset names from data
  asset.names <- unique(data[[asset.var]])
  N <- length(asset.names)
  
  # Define Return matrix
  totReturns = matrix(data[[ret.var]], nrow = N)
  row.names(totReturns) = asset.names
  colnames(totReturns) = as.character(time.periods)
  
  # The benchmark return
  benchReturns = benchmarkData
  benchReturns = as.vector(benchReturns[as.yearmon(time.periods), ])
  reg.list = vector("list", N)
  residReturns = matrix(0, nrow = N, ncol = N_TP)
  
  # Fitting the single factor model for each asset
  for (i in 1:N) {
    if (grepl("LS", fit.method)) {
      reg.list[[i]] <- lm(totReturns[i, ] ~ benchReturns)
      residReturns[i, ] <- reg.list[[i]]$residuals
    } else if (grepl("Rob", fit.method)) {
      reg.list[[i]] <- lmRob(totReturns[i, ] ~ benchReturns, mxr=200, 
                        mxf = 200, mxs = 200)
      residReturns[i, ] <- reg.list[[i]]$residuals
    }
  }
  row.names(residReturns) = asset.names
  colnames(residReturns) = as.character(time.periods)

  # Standardize the residual returns
  sigmaReturn = matrix(0, nrow = N, ncol = N_TP)
  avg <- apply(residReturns, MARGIN = 1, FUN = mean)
  var_past <- apply(residReturns, MARGIN = 1, FUN = var)

  for (i in 1:N) {
    ts <- (residReturns[i, ] - avg[i]) ^ 2
    var_past_2 <- var_past[i]
    sigmaReturn[i, ] <- sapply(ts[2], function(x) var_past_2 <<- 2e-06 + alpha * x + beta * var_past_2)
  }
  
  sigmaReturn = sqrt(sigmaReturn)
  residReturns <- residReturns / sigmaReturn

  # Fit Ffm to the residual returns
  data[[ret.var]] = as.vector(residReturns)
  fitResid = fitFfmMod(data = data, addIntercept = T,
                       asset.var = asset.var, ret.var = ret.var, date.var = date.var,
                       exposure.vars = exposure.vars,
                       z.score = "tsScore", tsScoreType = tsScoreType)
  residFactorReturns = fitResid$factor.returns
  Z = fitResid$Beta

  # Compute IR
  IC = apply(coredata(residFactorReturns), MARGIN = 2, mean)
  K = length(IC)
  covIC = fitResid$factor.cov
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
  names(alpha) = names(Omega) = index(residFactorReturns)[-1]

  return(list(IC = IC, varResid = varResid, IR = IR, asympIR = asympIR,
              residFactorReturns = residFactorReturns,
              alpha = alpha, Omega = Omega))
}
