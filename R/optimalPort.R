`optimalPort` <-
function(model, Rf=NULL, shortSell=NULL, eps=10^(-4)){
	#===> updating model and error checking <===#
	if(!is.null(Rf)){
		model$Rf <- Rf
	}
	if(!is.null(shortSell)){
		model$shorts <- ifelse(shortSell[1] %in% c('y', 'yes', 'Y', 'Yes', 'YES', TRUE), TRUE, FALSE)
	}
	if(!model$shorts & model$model == 'MGM'){
		warning('Short sales are always permitted under the multigroup model.')
		model$shorts <- TRUE
	}
	if(!model$shorts & model$model == 'none'){
		warning('Short sales are always permitted when no model is specified.')
		model$shorts <- TRUE
	}
	
	#===> ensuring Rf is reasonable for the data <===#
	if(model$Rf > -10^6){
		temp <- optimalPort(model, Rf=-10^7, eps=eps)
		if(model$Rf >= temp$R-eps){
			stop("Rf may not be valid for this stock model.\n")
		}
	}
	
	#===> setup the model <===#
	op <- list()
	class(op) <- "optimalPortfolio"
	op$model  <- model
	op$X      <- NA
	op$R      <- NA
	op$risk   <- NA
	
	#===> find optimal portfolio based on model <===#
	if(model$model == 'none'){
		optimalPortUt <- function(model){
			R <- model$R - model$Rf
			Z <- solve(model$COV) %*% R
			X <- as.numeric(Z / sum(Z))
			names(X) <- rownames(Z)
			ps       <- portReturn(list(R=model$R, COV=model$COV), X)
			return(list(X=X, R=ps$R, VAR=ps$VAR))
		}
		minRiskPortUt <- function(model){
			if(length(model$R) > 2){
				MRPM <- minRiskPortMultiUt(model)
				return(MRPM)
			}
			temp <- as.numeric(t(c(1,-1)) %*% model$COV %*% c(1,-1))
			X    <- model$COV[2:1,] %*% c(1,-1) * c(-1, 1) / temp
			port <- portReturn(model, X)
			R    <- sum(X*model$R)
			V    <- as.numeric(t(X) %*% model$COV %*% X)
			return(list(X=X, R=port$R, VAR=V))
		}
		minRiskPortMultiUt <- function(model, curveInfo=FALSE){
			maxRf <- optimalPortUt(model, -1000)$R
			Rf    <- maxRf - 0.001*(1:2)
			G1    <- optimalPortUt(model, Rf[1])
			G2    <- optimalPortUt(model, Rf[2])
			R.    <- c(G1$R, G2$R)
			V.    <- matrix(NA, 2, 2)
			V.[1,1] <- G1$VAR
			V.[2,2] <- G2$VAR
			V.[2,1] <- V.[1,2] <- as.numeric(t(G1$X) %*% model$COV %*% G2$X)
			MRP     <- minRiskPortUt(list(R=R., COV=V.))
			X       <- G1$X * MRP$X[1] + G2$X * MRP$X[2]
			if(!curveInfo){
				return(list(R=MRP$R, VAR=MRP$VAR, X=X))
			} else {
				return(list(R=MRP$R, VAR=MRP$VAR, X=X, G1=G1))
			}
		}
		OP      <- optimalPortUt(model)
		op$X    <- OP$X
		op$R    <- OP$R
		op$risk <- sqrt(OP$VAR)
	} else if(model$model == 'SIM'){
		ratio <- (model$R - model$Rf)/model$beta
		o     <- order(-ratio)
		alpha <- model$alpha[o]
		beta  <- model$beta[o]
		R     <- model$R[o]
		MSE   <- model$MSE[o]
		ratio <- ratio[o]
		c1    <- (R-model$Rf)*beta/MSE
		c2    <- cumsum(c1)
		c3    <- beta^2 / MSE
		c4    <- cumsum(c3)
		Ci    <- model$VM*c2 / (1 + model$VM*c4)
		cStar <- ifelse(model$shorts, rev(Ci)[1], max(Ci))
		
		z <- (beta/MSE) * (ratio - cStar)
		t <- ifelse(model$shorts, length(Ci), which.max(Ci)[1])
		X <- z[1:t]/sum(z[1:t])
		
		temp <- list(R=R[1:t], COV=model$COV[o[1:t],o[1:t]])
		ps   <- portReturn(temp, X)
		VAR  <- sum(beta[1:t]*X)^2*model$VM + sum(MSE[1:t] * X^2)
		X    <- X[match(model$ticker, names(X))]
		names(X)    <- model$ticker
		X[is.na(X)] <- 0
		op$X    <- X
		op$R    <- ps$R
		op$risk <- sqrt(ps$VAR)
	} else if(model$model == 'CCM'){
		ratio    <- (model$R - model$Rf)/model$sigma
		o        <- order(-ratio)
		ratio    <- ratio[o]
		R        <- model$R[o]
		rhoRatio <- model$rho / (1+(1:length(model$R) - 1)*model$rho)
		ratioSum <- cumsum(ratio)
		Ci       <- rhoRatio * ratioSum
		cStar    <- ifelse(model$shorts, rev(Ci)[1], max(Ci))
		
		z    <- (ratio - cStar) / ((1-model$rho) * model$sigma[o])
		t    <- ifelse(model$shorts, length(Ci), which.max(Ci)[1])
		X    <- z[1:t]/sum(z[1:t])
		temp <- list(R=R[1:t], COV=model$COV[o[1:t],o[1:t]])
		ps   <- portReturn(temp, X)
		X    <- X[match(model$ticker, names(X))]
		names(X)    <- model$ticker
		X[is.na(X)] <- 0
		op$X    <- X
		op$R    <- ps$R
		op$risk <- sqrt(ps$VAR)
	} else if(model$model == 'MGM'){
		ind  <- model$industry
		indU <- unique(model$industry)
		N    <- rep(NA, length(indU))
		for(i in 1:length(indU)){
			N[i] <- sum(ind == indU[i])
		}
		I3    <- diag(rep(1,length(indU)))
		A     <- I3 + model$rho*N/(1-diag(model$rho))
		C     <- rep(NA, length(indU))
		ratio <- (model$R - model$Rf) / model$sigma
		for(i in 1:length(indU)){
			theI <- (ind == indU[i])
			C[i] <- sum(ratio[theI]/(1-model$rho[i,i]))
		}
		PHI        <- as.numeric(solve(A) %*% C)
		names(PHI) <- indU
		z          <- rep(NA, length(ind))
		for(i in 1:length(ind)){
			k     <- which(indU == ind[i])
			cStar <- sum(model$rho[k,] * PHI)
			den   <- model$sigma[i] * (1-model$rho[k,k])
			z[i]  <- (ratio[i] - cStar) / den
		}
		X        <- z / sum(z)
		names(X) <- names(model$R)
		ps       <- portReturn(model, X)
		op$X     <- X
		op$R     <- ps$R
		op$risk  <- sqrt(ps$VAR)
	}
	return(op)
}

