
fitFfmMod <- function(data, asset.var, ret.var, date.var, exposure.vars,
                      weight.var = NULL, fit.method = c("LS","WLS","Rob","W-Rob"),
                      rob.stats = FALSE, full.resid.cov = FALSE, addIntercept = FALSE,
                      z.score = c("tsScore", "csScore", "None"), lagExposures = FALSE,
                      resid.EWMA = FALSE, lambda_reg = 0.9, lambda_tsScore = 0.95,
                      a = 2.5, tsScoreType = c("EWMA", "robEWMA", "GARCH"), alpha = 0.1,
                      beta = 0.81, ...) {

  # record the call as an element to be returned
  this.call <- match.call()

  # set defaults and check input validity
  if (missing(data) || !is.data.frame(data)) {
    stop("Invalid args: data must be a data.frame")
  }
  fit.method = fit.method[1]
  if (!(fit.method %in% c("LS","WLS","Rob","W-Rob"))) {
    stop("Invalid args: fit.method must be 'LS', 'WLS', 'Rob' or 'W-Rob'")
  }
  if (missing(asset.var) || !is.character(asset.var)) {
    stop("Invalid args: asset.var must be a character string")
  }
  if (missing(date.var) || !is.character(date.var)) {
    stop("Invalid args: date.var must be a character string")
  }
  if (missing(ret.var) || !is.character(ret.var)) {
    stop("Invalid args: ret.var must be a character string")
  }
  if (missing(exposure.vars) || !is.character(exposure.vars)) {
    stop("Invalid args: exposure.vars must be a character vector")
  }
  if (ret.var %in% exposure.vars) {
    stop("Invalid args: ret.var can not also be an exposure")
  }
  if (!is.null(weight.var) && !is.character(weight.var)) {
    stop("Invalid args: weight.var must be a character string")
  }
  if (!is.logical(rob.stats) || length(rob.stats) != 1) {
    stop("Invalid args: control parameter 'rob.stats' must be logical")
  }
  if (!is.logical(full.resid.cov) || length(full.resid.cov) != 1) {
    stop("Invalid args: control parameter 'full.resid.cov' must be logical")
  }
  z.score = z.score[1]
  if (!(z.score %in% c("tsScore", "csScore", "None"))) {
    stop("Invalid args: control parameter 'z.score' must be either tsScore, csScore, or NULL")
  }
  tsScoreType = tsScoreType[1]
  if (!(tsScoreType %in% c("EWMA", "robEWMA", "GARCH"))) {
    stop("Invalid args: control parameter 'tsScoreType' must be either EWMA, robEWMA, GARCH, or NULL")
  }
  if ((lambda_tsScore < 0) || (lambda_tsScore > 1)) {
    return("Invalid arg: lambda_tsScore must be between 0 and 1")
  }

  # initialize to avoid R CMD check's NOTE: no visible binding for global vars
  DATE = NULL
  W = NULL
  model.MSCI = FALSE                 # MSCI = Market Sector Country Intercept
  model.styleOnly = FALSE            # Exposures are all numeric
  restriction.mat = NULL
  g.cov = NULL

  # Ensure dates are in required format
  data[[date.var]] <- as.Date(data[[date.var]])
  time.periods <- unique(data[[date.var]])     # Extract unique time periods from data
  N_TP <- length(time.periods)
  if (N_TP < 2) {
    stop("Invalid args: at least 2 unique time periods are required to fit the
         factor model")
  }

  # order data.frame by date.var
  data <- data[order(data[, date.var]), ]

  # extract asset names from data
  asset.names <- unique(data[[asset.var]])
  N <- length(asset.names)

  #Define Return matrix
  returns = matrix(data[[ret.var]], nrow = N)
  row.names(returns) = asset.names
  colnames(returns) = as.character(time.periods)

  # Check number & type of exposures; convert character exposures to dummy variables
  which.numeric <- sapply(data[, exposure.vars, drop = FALSE], is.numeric)
  exposures.num <- exposure.vars[which.numeric]
  exposures.char <- exposure.vars[!which.numeric]

  if (length(exposures.char) > 1)
  {
    model.MSCI = TRUE            #Model has both Sector and Country along with Intercept
  }
  if (model.MSCI == TRUE && !addIntercept) {
    stop("Invalid args: Sector + Country model without Market(Intercept) is currently unavailable")
  }
  if (length(exposures.char) == 0)
  {
    model.styleOnly = TRUE
  }

  if(lagExposures)
  {
    # Get the style exposures except for the last time period
    dataExpoLagged <- data[1:(N * (N_TP-1)), exposures.num]
    # Remove data corresponding to the first time period
    dataLagged <- data[-(1:N), ]
    # Replace style expo with lagged exposures
    dataLagged[, exposures.num] <- dataExpoLagged
    data <- dataLagged
    # Update the time period length
    time.periods <- unique(data[[date.var]])
    N_TP <- length(time.periods)
  }

  # Set up the weight exposure. If weight.var is not NULL, then we will normalize
  # the weights; otherwise, all weights are set to 1
  if (!is.null(weight.var)) {
    # weight exposures within each period using weight.var
    w <- unlist(by(data = data, INDICES = data[[date.var]],
                   function(x) x[[weight.var]] / sum(x[[weight.var]])))
  } else {
    w <- rep(1, nrow(data))
  }

  # Weight the numeric exposures
  data[, exposures.num] = w * data[, exposures.num]

  # Convert numeric exposures to z-scores
  for (i in exposures.num) {
    stdNumExpo <- by(data = data, INDICES = data[[date.var]], FUN = zScore,
                     i = i, rob.stats = rob.stats, z.score = z.score, a = a,
                     tsScoreType = tsScoreType, lambda = lambda_tsScore, alpha = alpha,
                     beta = beta)
    data[[i]] <- unlist(stdNumExpo)
  }

  # return(data[, exposures.num])

  # Create factor model formula to be passed to lm or lmRob
  fm.formula <- paste(ret.var, "~", paste(exposure.vars, collapse = " + "))

  # FIRST LOOP
  # Covers model.MSCI = F, model.styleOnly = T,F, addIntercept = F
  if (!model.MSCI) {
    if (!model.styleOnly) {
      # This implies length(exposure.char) = 1
      # Note: Remove Intercept as it introduces the rank deficiency in the
      # exposure matrix. Implemetation with Intercept is handled
      # later, using a Restriction matrix to remove the rank deficiency.
      fm.formula <- paste(fm.formula, "- 1")
      data[, exposures.char] <- as.factor(data[, exposures.char])
      contrasts.list <- lapply(seq(length(exposures.char)), function(i)
        function(n) contr.treatment(n, contrasts=FALSE))
      names(contrasts.list) <- exposures.char

      if (!addIntercept) {

      } else {

        contrasts.list <- NULL
        # Formula to extract beta of Sec or Country
        formula.expochar = as.formula(paste(ret.var, "~", exposures.char, " -1"))
        factor.names <- c("Market", paste(levels(data[, exposures.char]), sep = " "),
                          exposures.num)
        # Make a matrix of dummy varibles from exposures.char
        beta.expochar <- model.matrix(object = formula.expochar, data = data)
        rownames(beta.expochar) <- rep(asset.names, length(time.periods))

        # Beta for the whole model (generally without the intercept)
        beta <- model.matrix(object = as.formula(fm.formula), data = data)
        rownames(beta) <- rep(asset.names, N_TP)

        # Define beta.star as the beta of the whole model with Intercept/Market
        # represented by a column of ones
        beta.star <- cbind("Market" = rep(1, nrow(beta.expochar)), beta.expochar)

        if (length(exposures.num) > 0) {

          beta.style <- matrix(beta[,exposures.num], ncol = length(exposures.num))
          colnames(beta.style) = exposures.num

          # Define Beta for Style factors
          B.style = beta.style[((N_TP - 1) * N + 1) : (N * N_TP), ]

        }

        # Number of factors
        K <- dim(beta.star)[2]
        # Define Restriction matrix
        R_matrix = rbind(diag(K - 1), c(0, rep(-1, K - 2)))
        #Define B.Mod = X * R
        B.mod = (beta.star[1:N, ]) %*% R_matrix
        #Formula for Markt+Sector/Country Model
        fmSI.formula = as.formula(paste(ret.var, "~", "B.mod + ",
                                        paste(exposures.num, collapse = " + "), " -1" ))
      }
    } else {
      if (!addIntercept) {
        # model.MSCI = F, model.styleOnly = T, addIntercept = F
        fm.formula <- paste(fm.formula, "- 1")
        contrasts.list <- NULL
      }
    }
    # convert the pasted expression into a formula object
    fm.formula <- as.formula(fm.formula)
  }

  if (model.MSCI == FALSE) {
    #Perform regression using fm.formula without any restriction matrix, if WLS/WRob is the
    #fit.method when addIntercept =TRUE. Else use fmSI.formula.
    if (!(grepl("W",fit.method)) && addIntercept == TRUE && !model.styleOnly){
      fm.formula = fmSI.formula
      contrasts.list = NULL
    }

    # estimate factor returns using LS or Robust regression
    # returns a list of the fitted lm or lmRob objects for each time period
    if (grepl("LS",fit.method))
    {
      reg.list <- by(data = data, INDICES = data[[date.var]], FUN = lm,
                     formula = fm.formula, contrasts = contrasts.list,
                     na.action = na.fail)
    }
    else if (grepl("Rob",fit.method))
    {
      reg.list <- by(data=data, INDICES=data[[date.var]], FUN=lmRob,
                     formula=fm.formula, contrasts=contrasts.list,
                     mxr=200, mxf=200, mxs=200, na.action=na.fail)
    }

    # compute residual variance for all assets for weighted regression
    if (grepl("W",fit.method)) {
      if (rob.stats) {
        resid.var <- apply(sapply(reg.list, residuals), 1, scaleTau2)^2
      } else {
        resid.var <- apply(sapply(reg.list, residuals), 1, var)
      }
      if (resid.EWMA) {
        res = sapply(reg.list, residuals)
        w <- matrix(0,N,N_TP)
        for (i in 1:N) {
          var_tminus1 = as.numeric(resid.var[i])
          for (j in 2:N_TP) {
            w[i,j] = var_tminus1 + ((1-lambda_reg)*(res[i,j]^2-var_tminus1))
            var_tminus1 = w[i,j]
          }
        }
      w[,1] = resid.var
      data <- cbind(data, W = 1 / as.numeric(w))
      } else {
        data <- cbind(data, W = 1 / resid.var)
      }
    }

    # estimate factor returns using WLS or weighted-Robust regression
    # returns a list of the fitted lm or lmRob objects for each time period
    if (fit.method == "WLS") {
      if (addIntercept  && !model.styleOnly) {
        fm.formula = fmSI.formula
        contrasts.list = NULL
      }
      reg.list <- by(data=data, INDICES=data[[date.var]],
                     FUN=function(x) {
                       lm(data=x, formula=fm.formula, contrasts=contrasts.list,
                          na.action=na.fail, weights=W)
                     })
    } else if (fit.method =="W-Rob") {
      reg.list <- by(data = data, INDICES = data[[date.var]],
                     FUN = function(x) {
                       lmRob(data = x, formula = fm.formula, contrasts = contrasts.list,
                             na.action = na.fail, weights = W,
                             mxr = 200, mxf = 200, mxs = 200)
                     })
    }
  }

  ## Compute or Extract objects to be returned
  if ((addIntercept == FALSE || model.styleOnly ==TRUE) && model.MSCI == FALSE) {
    # number of factors including Market and dummy variables
    if (!model.styleOnly) {
      factor.names <- c(exposures.num,
                        paste(levels(data[,exposures.char]),sep=""))
    } else {
      if (addIntercept) {
        factor.names <- c("Alpha", exposures.num)
      } else {
          factor.names <- exposures.num
      }
    }
    K <- length(factor.names)
    # exposure matrix B or beta for the last time period - N x K
    beta <- model.matrix(fm.formula, data=subset(data, DATE == time.periods[N_TP]))
    rownames(beta) <- asset.names
    #Shorten the Sector/Country names
    colnames(beta) = gsub("COUNTRY|SECTOR|GICS.", "", colnames(beta))
    #colnames(beta) = gsub(paste(exposures.char), "", colnames(beta))
    #Remove SECTOR/COUNTRY to shorten the coef names.
    if (length(exposures.char) > 0 )
    {
      reg.list = lapply(seq(1:N_TP), function(x){ names(reg.list[[x]]$coefficients) = gsub("COUNTRY|SECTOR|GICS.", "",names(reg.list[[x]]$coefficients) ) ;reg.list[[x]]})
      names(reg.list) = as.character(unique(data[[date.var]]))
    } else if(model.styleOnly && addIntercept) {
      reg.list = lapply(seq(1:N_TP), function(x){ names(reg.list[[x]]$coefficients)[1] = "Alpha";reg.list[[x]]})
      names(reg.list) = as.character(unique(data[[date.var]]))
    }

    # time series of factor returns = estimated coefficients in each period
    factor.returns <- sapply(reg.list, function(x) {
      temp <- coef(x)
      temp[match(factor.names, names(temp))]})
    # simplify factor.names for dummy variables
    if (length (exposures.char)) {
      factor.names <- c(exposures.num, levels(data[, exposures.char]))
    }
    rownames(factor.returns) <- factor.names
    factor.returns <- checkData(t(factor.returns)) # T x K

    # time series of residuals
    residuals <- sapply(reg.list, residuals) #  N x T
    row.names(residuals) <- asset.names
    residuals <- checkData(t(residuals)) # T x N

    # r-squared values for each time period
    r2 <- sapply(reg.list, function(x) summary(x)$r.squared)

    # factor and residual covariances
    if (rob.stats) {
      if (kappa(na.exclude(coredata(factor.returns))) < 1e+10) {
        factor.cov <- covRob(coredata(factor.returns), estim="pairwiseGK",
                             distance=FALSE, na.action=na.omit)$cov
      } else {
        cat("Covariance matrix of factor returns is singular.\n")
        factor.cov <- covRob(coredata(factor.returns), distance=FALSE,
                             na.action=na.omit)$cov
      }
      resid.var <- apply(coredata(residuals), 2, scaleTau2, na.rm=T)^2
      if (full.resid.cov) {
        resid.cov <- covOGK(coredata(residuals), sigmamu=scaleTau2, n.iter=1)$cov
      } else {
        resid.cov <- diag(resid.var)
      }
    } else {
      factor.cov <- covClassic(coredata(factor.returns), distance=FALSE,
                               na.action=na.omit)$cov
      resid.var <- apply(coredata(residuals), 2, var, na.rm=T)
      if (full.resid.cov) {
        resid.cov <- covClassic(coredata(residuals), distance=FALSE,
                                na.action=na.omit)$cov
      } else {
        resid.cov <- diag(resid.var)
      }
    }
    # return covariance estimated by the factor model
    # (here beta corresponds to the exposure of last time period,N_TP)
    return.cov <-  beta %*% factor.cov %*% t(beta) + resid.cov

    if (addIntercept) {
      colnames(beta)[1] = "Alpha"
    }
    beta = beta[, colnames(factor.returns)]

  } else if (addIntercept == TRUE && model.MSCI == FALSE && model.styleOnly ==FALSE) {
    #Rename regression coefs
    reg.list= lapply(seq(1:N_TP), function(x){ names(reg.list[[x]]$coefficients) =  paste("g", seq(1:length(reg.list[[x]]$coefficients)), sep = "");reg.list[[x]]})
    names(reg.list) = as.character(unique(data[[date.var]]))
    #Extract g coef
    g = sapply(reg.list, function(x) coef(x))
    # Factor returns = restriction matrix * g coefficients
    factor.returns  = R_matrix %*% g[1:(K-1), ]
    if(length(exposures.num) > 0){
      factor.returns = rbind(factor.returns, g[K:nrow(g), ])
    }
    rownames(factor.returns) = factor.names
    colnames(factor.returns) = as.character(unique(data[[date.var]]))
    #Extract residuals
    residuals = sapply(reg.list, residuals)
    colnames(residuals) = as.character(unique(data[[date.var]]))
    row.names(residuals) = asset.names
    # Create a T x N xts object of residuals
    residuals <- checkData(t(residuals))
    r2<- sapply(reg.list, function(x) summary(x)$r.squared)
    names(r2) = as.character(unique(data[[date.var]]))
    factor.returns <- checkData(t(factor.returns)) # T x K
    # Rearrange g,factor return to Mkt- Style Factor - Sec/Country order
    if(length(exposures.num) > 0) {g = g[c(1,K:nrow(g),2:(K-1)),]}
    factor.names<- c("Market", exposures.num,
                     paste(levels(data[,exposures.char]),sep=" "))
    factor.returns = factor.returns[, factor.names]
    # Factor Covarinace
    factor.cov <-covClassic(coredata(factor.returns), distance=FALSE,
                            na.action=na.omit)$cov
    g.cov <- cov(t(g))
    # Residual Variance
    resid.var <- apply(coredata(residuals), 2, var, na.rm=T)
    names(resid.var) <- asset.names
    resid.cov <- diag(resid.var)
    # Returns covariance
    if (length(exposures.num) > 0) {
      beta.combine = cbind(beta.star, beta.style)
      beta.stms = cbind(B.mod[,1], B.style, B.mod[,-1])
    } else {
      beta.combine = beta.star
      beta.stms = B.mod
    }

    colnames(beta.combine) = gsub("COUNTRY|SECTOR|GICS.", "", colnames(beta.combine))
    beta.combine = beta.combine[, factor.names]
    return.cov <-  beta.stms %*% g.cov %*% t(beta.stms) + resid.cov
    #Exposure matrix for the last time period
    beta = beta.combine[((N_TP-1) * N + 1):(N_TP * N), 1:ncol(beta.combine)]
    #Restriction matrix
    restriction.mat = R_matrix
  } else if (model.MSCI) {

      # determine factor model formula to be passed to lm
      fm.formula <- paste(ret.var, "~", paste(exposure.vars, collapse = " + "))
      if (!model.styleOnly) {
        fm.formula <- paste(fm.formula, "- 1")
        for (i in exposures.char) {
          data[, i] <- as.factor(data[,i])
          if (grepl("SECTOR",i)) {
            formula.ind = as.formula(paste(ret.var, "~", i, "-1"))
          } else  {
              formula.cty = as.formula(paste(ret.var, "~", i, "-1"))
          }
        }
      }

      # convert the pasted expression into a formula object
      fm.formula <- as.formula(fm.formula)
      #Extract model beta, expo.char beta and expo.num betas
      beta <- model.matrix(fm.formula, data=data)
      beta.ind <- model.matrix(formula.ind, data=data)
      beta.cty <- model.matrix(formula.cty, data=data)
      beta.mic <- cbind("Market" = rep(1, nrow(beta.ind)), beta.ind, beta.cty)

      if (length(exposures.num) > 0) {
        beta.style<- beta[, exposures.num]
      }

      fac.names.indcty = lapply(seq(exposures.char), function(x)
        paste(levels(data[, exposures.char[x]]), sep=""))

      if(grepl("SECTOR", exposures.char[1])){
        factor.names <- c("Market",unlist(fac.names.indcty),
                          exposures.num)
      } else {
        factor.names <- c("Market", unlist((fac.names.indcty)[2]), unlist((fac.names.indcty)[1]),
                        exposures.num)
      }

      rownames(beta.mic) <- rep(asset.names, N_TP)
      asset.names <- unique(data[[asset.var]])
      K <- length(factor.names)
      K1 <- dim(beta.ind)[2]
      K2 <- dim(beta.cty)[2]
      #Define Restriction matrix
      rMic <- rbind(cbind(diag(K1), matrix(0, nrow = K1, ncol = K2 - 1)),
                    c(c(0, rep(-1, K1 - 1)), rep(0, K2 - 1)),
                    cbind(matrix(0, ncol = K1, nrow = K2 - 1), diag(K2 - 1)),
                    c(rep(0, K1), rep(-1, K2 - 1)))

      reg.list <- list()

      B.mod = (beta.mic[1:N, ]) %*% rMic  #Y = X * R
      if (length(exposures.num) > 0) {
        B.style = beta.style[((N_TP-1)*N+1) : (N_TP*N), ]
      }
      fmMSCI.formula = as.formula(paste(ret.var, "~", "B.mod+", paste(exposures.num, collapse="+"),"-1" ))
      reg.list <- by(data=data, INDICES=data[[date.var]],
                     FUN=function(x) {lm(data=x, formula=fmMSCI.formula,
                                      na.action=na.fail)})

      reg.list= lapply(seq(1:N_TP), function(x){ names(reg.list[[x]]$coefficients) =  paste("g", seq(1:length(reg.list[[x]]$coefficients)), sep = "");reg.list[[x]]})
      names(reg.list) = as.character(unique(data[[date.var]]))
      g = sapply(reg.list, function(x) coef(x))
      factor.returns  = rMic %*% g[1:(K1+K2-1), ]

      if(length(exposures.num) > 0){
        factor.returns = rbind(factor.returns, g[(K1+K2):nrow(g), ])
      }

      rownames(factor.returns) <- factor.names
      colnames(factor.returns) = as.character(unique(data[[date.var]]))

      if(length(exposures.num) > 0){
        residuals = returns - B.mod %*% g[1:(K1+K2-1), ] - B.style %*% g[(K1+K2):nrow(g), ] #NxT
      } else {
          residuals = returns - B.mod %*% g[1:(K1+K2-1), ]
      }

      colnames(residuals) = as.character(unique(data[[date.var]]))
      # Create a T x N xts object of residuals
      residuals <- checkData(t(residuals))
      # all.equal(x,residuals)
      r2<- sapply(reg.list, function(x) summary(x)$r.squared)
      # r2 <- as.numeric(sapply(X = summary(reg.list), FUN = "[","r.squared"))
      names(r2) = as.character(unique(data[[date.var]]))
      factor.returns <- checkData(t(factor.returns)) # T x K
      # Re-order the columns in the order mkt-style-sector-country
      if(length(exposures.num) > 0) {
        factor.returns <- factor.returns[,c(1,(K1+2+K2):K, 2:(K1+1), (K1+2):(K1+K2+1))]
      }
      factor.names <- colnames(factor.returns)
      #Factor Covarinace
      factor.cov <- covClassic(coredata(factor.returns), distance = FALSE,
                               na.action = na.omit)$cov
      #Residual Variance
      resid.var <- apply(coredata(residuals), 2, var, na.rm=T)
      names(resid.var) <- asset.names
      resid.cov <- diag(resid.var)
      #Returns covariance
      if (length(exposures.num) > 0) {
        beta.combine = cbind(beta.mic, beta.style)
      } else {
          beta.combine = beta.mic
      }

      colnames(beta.combine) = gsub("COUNTRY|SECTOR|GICS.", "", colnames(beta.combine))
      beta.combine = beta.combine[, factor.names]
      #Exposure matrix
      beta = beta.combine[((N_TP - 1) * N + 1):(N_TP * N), 1:K]
      return.cov <-  beta %*% factor.cov %*% t(beta) + resid.cov

      #colnames(beta) = gsub("COUNTRY|SECTOR", "", colnames(beta))
      #factor.cov = factor.cov[factor.names, factor.names]
      #Restriction matrix
      restriction.mat = rMic
  }

  Beta <- vector("list", N_TP)
  for (t in 1:N_TP) {
    Beta[[t]] <- beta.combine[((t - 1) * N + 1):(t * N), ]
  }
  # create list of return values.
  result <- list(factor.fit = reg.list, beta = beta, factor.returns = factor.returns,
                 residuals = residuals,
                 r2 = r2, factor.cov = factor.cov, g.cov = g.cov, resid.cov = resid.cov,
                 return.cov = return.cov, restriction.mat = restriction.mat,
                 resid.var = resid.var, call = this.call, Beta = Beta,
                 data = data, date.var = date.var, ret.var = ret.var,
                 asset.var = asset.var, exposure.vars = exposure.vars,
                 weight.var = weight.var, fit.method = fit.method,
                 asset.names = asset.names, factor.names = factor.names,
                 time.periods = time.periods, fm.formula = fm.formula)

  class(result) <- "ffm"
  return(result)
}

# Function to calculate z-scores with additional argument 'rob.stat' to use robust
# location and scale parameters

zScore = function(data, i, z.score, rob.stats = TRUE,
                  lambda = NULL, tsScoreType = NULL, a = 2.5, alpha = 0.1,
                  beta = 0.81) {

  if (z.score == "tsScore"){

    # sigma_0 is calculated the same way as in cusumActMgr
    var_past <- var(data[[i]])
    TS <- (data[[i]] - mean(data[[i]])) ^ 2

    if (tsScoreType == "EWMA") {

      # Use scoping assignment to update the EWMA variance
      var_present <- sapply(TS, function(x) var_past <<- lambda * var_past + (1 - lambda) * x)
      scores <- (data[[i]] - mean(data[[i]])) / sqrt(var_present)

    } else if (tsScoreType == "robEWMA") {

        # Use scoping assignment to update the EWMA variance
        var_present <- sapply(TS, function(x) var_past <<- ifelse(abs(x) > a * var_past,
                                                                    var_past,
                                                                    lambda * var_past + (1 - lambda) * x))
        scores <- (data[[i]] - mean(data[[i]])) / sqrt(var_present)

    } else {
        var_present <- sapply(TS, function(x) var_past <<- 2e-06 + alpha * x + beta * var_past)
        scores <- (data[[i]] - mean(data[[i]])) / sqrt(var_present)
    }
  } else if (z.score == "csScore") {

    if (rob.stats) {
      scores <- (data[[i]] - median(data[[i]])) / mad(data[[i]], center = median(data[[i]]))
    } else {
      scores <- (data[[i]] - mean(data[[i]])) / sd(data[[i]])
    }

  } else {}
  return(scores)
}
