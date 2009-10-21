`plot.portReturn` <-
function(x, xlab='Risk', ylab='Return', main='Risk and Return of Stocks', addNames=FALSE, pos=3, ylim="default", col='heatGradient', ...){
	if(all(x$X == x$X[1])){
		if(col[1] == 'heatGradient'){
			col <- '#550055'
		} else {
			if(length(col) < length(x$X)){
				col <- rep(col, length(x$X))
			}			
		}
	} else if(col[1] == 'heatGradient'){
		tMin <- min(x$X)
		tMax <- max(x$X)
		inte <- (x$X-tMin)/(tMax-tMin)
		inte <- round(99*inte, 0)
		inteSupp <- 99-inte
		inte <- as.character(inte)
		inteSupp <- as.character(inteSupp)
		inte[nchar(inte)<2] <- paste('0',inte[nchar(inte)<2],sep='')
		temp <- paste('0',inteSupp[nchar(inteSupp)<2],sep='')
		inteSupp[nchar(inteSupp)<2] <- temp
		col <- paste('#', inte, '00', inteSupp, sep='')
	} else if(length(col) < length(x$X)){
		col <- rep(col, length(x$X))
	}
	plot(x$model, xlab=xlab, ylab=ylab, main=main, addNames=addNames, pos=pos, ylim=ylim, col=col, ...)
}

