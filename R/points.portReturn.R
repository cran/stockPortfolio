`points.portReturn` <-
function(x, addNames=FALSE, pos=3, col='heatGradient', ...){
	if(col == 'heatGradient'){
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
	}
	points(x$model, addNames=addNames, pos=pos, col=col, ...)
}

