#Update of Cplot
# plot a circular histogram
# add a mean vector
# add a circular confidence interval
# accomodate axial data
# I'll write a function to draw the "little angle" arc for CI too
Cplot2 <- function(x, alpha, ax, rho.col, out.by, kappa.ci, ...){
	if(missing(ax)){ax <- F}#fit mean axis (not direction)
	if(missing(rho.col)){rho.col <- 'deeppink4'}#fit mean axis (not direction)
	if(missing(out.by)){out.by = 0.05} # draw outside (or inside) by
	if(missing(alpha)){alpha = 0.05} #proportion NOT to plot across
		#spacing of stacked points, now automatically stacks towards centre unless otherwise specified
	if(missing(kappa.ci)){kappa.ci = F} #proportion NOT to plot across
		#spacing of stacked points, now automatically stacks towards centre unless otherwise specified
	sp <- 0.04
	
		if(!(	sum('mycirc'%in% ls())	)){
		mycirc <- function(angles, clock){
			if(missing(clock)){clock <- T}
			if(clock){
			return(		as.circular(angles,units='degrees',type='angles',
			modulo='2pi',zero=pi/2,rotation='clock',	template='none')	)
				}else{
				as.circular(angles,units='degrees',type='angles',
				 modulo='2pi',zero=pi/2,rotation='counter',template='none')
				}#if(clock)
			}#mycirc <- function(angles, clock)
	}#if(!(	sum('mycirc'%in% ls())	))
	
	if(ax == F){
			#circular plot settings
	increments <- 5 #degrees
	zr <- pi/2 #start at top of screen (pi*	90	/180)
	bn <- 10*10*360/5 #bins 	
	degrad <- 180/pi #conversion from radians to degrees
	tcl <- rgb(1,1,1,0)#transparent colour
	pcl <- rgb(.3,.1,.1,.5)#point colour
	#plot characters
	lw <- 0.5 #line width
	pnt <- 2.5 #point size
	arw <- 10 #arrowhead angle
	arl <- 0.1 #arrowhead length
	#	set up input variables
	hd <- as.circular(x,units='degrees',type='angles',
			modulo='2pi',zero=pi/2,rotation='clock',	template='none')
	sm <- summary(hd)
	sv <- degrad*sd.circular(hd, na.rm=T)
	lbl <- 90*(1:4-1)
	plot(hd, col=tcl, main="", zero=zr, axes=F, shrink=1,tol=0.075)
	axis.circular(1, at = mycirc(lbl), labels = paste0(lbl, 'º'))
	par(new=T)
	plot.circular(hd, col=tcl,main="",zero=zr,axes=F,shrink=1.05,tol=0.075)
	points(hd,stack=T,bin=bn,sep=-sp,zero=zr,...)
	par(new=T)
	plot(hd, col=tcl, main="", zero=zr, axes=F, shrink=1,tol=0.075)
	if(kappa.ci){
	lines.circular(mycirc(rep(mean(hd, na.rm =T),2)), y = -(1-A1(mle.vonmises.bootstrap.ci(hd, alpha = 0.05, bias = T, reps = 10^4)$kappa.ci)), col = rgb(1,0,0,0.2), lwd = 7, lend = 'butt')
	}#if(kappa.ci)
	arrows.circular( mean(hd, na.rm =T),zero=zr, col = rho.col,lwd=3,
		 length=arl,angle=arw,shrink = rho.circular(hd,na.rm =T))
	#find the lower and upper confidence interval bounds
	ci <- mle.vonmises.bootstrap.ci(hd, alpha = alpha, bias = T, 
          reps = 10^4)$mu.ci
	# if(abs(diff(ci)) > 180){#check if the default angle is "little"
		# #otherwise, plot in the reverse direction
		# sq = seq( from = as.numeric(min(ci)),
					# to = -as.numeric(360 - max(ci)), length.out = 10^3)}else{ #if it is "little" plot from min to max
		# sq = seq( from = as.numeric(min(ci)),
					# to = as.numeric(max(ci)), length.out = 10^3)
	# }#if(abs(diff(qt)) > pi)
	
	if(sign(max(ci) - mean(hd, na.rm =T)) == -1 | sign(mean(hd, na.rm =T) - min(ci)) == -1 ){#check if mean is right or left of max CI
		#otherwise, plot in the reverse direction
		sq = seq( from = as.numeric(min(ci)),
					to = -as.numeric(360 - max(ci)), length.out = 10^3)}else{ #if it is "little" plot from min to max
		sq = seq( from = as.numeric(min(ci)),
					to = as.numeric(max(ci)), length.out = 10^3)
	}#if(abs(diff(qt)) > pi)
	
	#draw a line in coordinates of rcos(x) rsin(y)
	#any other line arguments (i.e. col or lwd) input as "..."
	lines.circular(mycirc(sq), rep(out.by,10^3), col = rho.col, lwd = 3)
	}#if(ax == F)
}###################	END OF FUNCTION	###########################	

