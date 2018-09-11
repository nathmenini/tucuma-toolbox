# --------------------------------------------------------------- LIBS ----

options(shiny.sanitize.errors = FALSE)

# this tool is still not compatible with Windows OS
if(Sys.info()[["sysname"]] == "Windows") {
	stop("This tool is not yet compatible with Windows.\nPlease use a UNIX-like system.")
}

# load and install packages if needed
if(!require("pacman")) {
	install.packages("pacman")
	require("pacman")
}

# CRAN
packs <- c(
	"beepr",
	"bfast",
	"bfastSpatial",
	"car",
	"caret",
	"cluster",
	"colorspace",
	"data.table",
	"devtools",
	"doMC",
	"dplyr",
	"FastKNN",
	"fields",
	"foreach",
	"ggmap",
	"igraph",
	"knitr",
	"leaflet",
	"lubridate",
	"magrittr",
	"markdown",
	"parallel",
	"pryr",
	"purrr",
	"RColorBrewer",
	"raster",
	"Rcpp",
	"rgeos",
	"rgp",
	"robust",
	"robustbase",
	"rPython",
	"SDMTools",
	"shiny",
	"shinyBS",
	"shinyFiles",
	"shinyjs",
	"stringr",
	"strucchange",
	"tools",
	"TSdist",
	"xtable",
	"zoo"
)

# GitHub
packs_gh <- c(
	"verbe039/bfast"
)

p_load(char = packs)
p_load_gh(char = packs_gh)

# ---------------------------------------------------------- FUNCTIONS ----

# preprocessing
ppBfastmonitor <- function(x, date, ...) {
	tmpTs <- bfastts(data = x,
					 dates = date,
					 type = "irregular")
	tmp <- bfastmonitor(tmpTs, ...)
	return(tmp)
}

ppBfast01 <- function(x, date, ...) {
	tmpTs <- bfastts(data = x,
					 dates = date,
					 type = "irregular")
	tmp <- bfast01(tmpTs, ...)
	return(tmp)
}

ppBfast <- function(x, date, ...) {
	# bfast needs regularly spaced data. Here, we force the data regularity
	# with a monthly aggregation. Temporal gaps are linearly filled.
	timeYM <- strftime(date, "%Y-%m")
	x <- data.frame(time = timeYM, x)
	x <- aggregate(x = x$x,
				   by = list(x$time),
				   FUN = median)
	names(x)[1] <- "time"

	# create an empty continuous year-month df
	mYear <- as.numeric(substr(head(x$time, 1), 1, 4))
	MYear <- as.numeric(substr(tail(x$time, 1), 1, 4))
	mMonth <- as.numeric(substr(head(x$time, 1), 6, 7))
	MMonth <- as.numeric(substr(tail(x$time, 1), 6, 7))
	fullYear <- rep(mYear:MYear, each = 12)
	fullMonth <- rep(1:12, times = length(mYear:MYear))

	# correcting the first year
	if(mMonth > 1) {
		fullYear <- fullYear[-(1:(mMonth - 1))]
		fullMonth <- fullMonth[-(1:(mMonth - 1))]
	}

	# correcting the last year
	if (MMonth < 12) {
		fullYear <- fullYear[-((length(fullYear) - 12 + MMonth + 1):length(fullYear))]
		fullMonth <- fullMonth[-((length(fullMonth) - 12 + MMonth + 1):length(fullMonth))]
	}
	dfull <- data.frame(time = paste0(fullYear, "-", sprintf("%02d", fullMonth)))

	# merging x df with the full year-month df
	d <- merge(dfull, x, all = T)
	d$time <- NULL

	dts <- na.approx(
		ts(data = c(d)$x,
		   start = c(mYear, mMonth),
		   end = c(MYear, MMonth),
		   frequency = 12),
		rule = 2
	)

	tmp <- bfast(dts, max.iter = 1, ...)
	return(tmp)
}

# plotting
plotRaw <- function(serie, matchCol, xAxisCustom, ylimCustom, ylab, seriePar, coords) {
	# placeholder blank plot
	par(mar = c(4, 4, 0, 0) + 0.1)
	plot(y = serie[, matchCol],
		 x = serie$date,
		 axes = F,
		 xlab = "Time",
		 ylab = toupper(colnames(serie)[matchCol]),
		 ylim = ylimCustom,
		 col = "white")

	# redraw axis
	axis(side = 1,
		 at = date(paste0(xAxisCustom, "-01-01")),
		 labels = xAxisCustom)
	if(ylimCustom[1] == -1) {
		yAxisStep <- 0.2
	} else {
		yAxisStep <- 0.1
	}
	axis(side = 2,
		 at = seq(ylimCustom[1], ylimCustom[2], yAxisStep))

	# draw a grid on both axis directions
	for(i in xAxisCustom) {
		abline(v = date(paste0(i, "-01-01")),
			   col = rgb(.8, .8, .8),
			   lty = 2)
	}
	for(i in seq(ylimCustom[1], ylimCustom[2], yAxisStep)) {
		if(i == 0) {
			# highlight a guide line at "0"
			abline(h = i,
				   col = "black",
				   lty = 3,
				   lwd = 1.3)
		} else {
			abline(h = i,
				   col = rgb(.8, .8, .8),
				   lty = 2)
		}
	}
	box()

	# draw time series as points and lines
	lines(
		y = serie[, matchCol],
		x = serie$date,
		col = rgb(.1, .1, .1)
	)

	points(
		y = serie[, matchCol],
		x = serie$date,
		col = seriePar[, 1],
		pch = as.numeric(seriePar[, 2]),
		cex = 0.6
	)

	if(mean(serie[, matchCol], na.rm = TRUE) > (ylimCustom[1] + 0.5 * (diff(ylimCustom)))) {
		posText <- 0.1
	} else {
		posText <- 0.9
	}

	if(coords$show) {
		text(
			x = serie$date %>% year() %>% unique() %>% range() %>% mean() %>% sum(0.5) %>% date_decimal() %>% format("%Y-%m-%d") %>% as.Date(),
			y = ylimCustom[1] + posText * (diff(ylimCustom)),
			labels = paste0("LatLong = (", sprintf("%.5f", round(coords$lat, 5)), ", ", sprintf("%.5f", round(coords$lon, 5)), ")"),
			cex = 1.3
		)
	}
}
plotRawLegend <- function(satOrder, seriePar) {
	# placeholder blank plot
	par(mar = c(4, 0, 0, 0) + 0.1)
	plot(0,
		 col = "white",
		 axes = F,
		 xlab = "",
		 ylab = "")

	# first, draw lines
	legend(x = "center",
		   lty = 1,
		   x.intersp = 2,
		   y.intersp = 1.5,
		   legend = names(satOrder),
		   text.col = "white",
		   bty = "n",
		   col = rgb(.1, .1, .1))

	serieParOrder <- unique(seriePar)
	serieParOrder <- rbind(serieParOrder[order(serieParOrder[, 2]), ])

	# then, draw points
	legend(x = "center",
		   x.intersp = 2,
		   y.intersp = 1.5,
		   legend = names(satOrder),
		   bty = "n",
		   col = serieParOrder[, 1],
		   pch = as.numeric(serieParOrder[, 2])
	)
}

plotBfm <- function(serie, matchCol, bfmOut, xAxisCustom, ylimCustom, ylab) {
	# define a vector with rounded dates to avoid strange rounding errors
	# to interfere with the conditions below
	dates <- round(decimal_date(serie$date), digits = 3)

	# conditions
	condHist <- dates >= round(bfmOut$history[1], digits = 3) & dates <= round(bfmOut$history[2], digits = 3)
	condMoni <- dates >= round(bfmOut$monitor[1], digits = 3)
	condStart <- dates <= round(bfmOut$history[2], digits = 3)
	condPred <- dates >= round(bfmOut$history[1], digits = 3)

	# background blank plot
	par(mar = c(4, 4, 0, 0) + 0.1)
	plot(y = serie[, matchCol],
		 x = serie$date,
		 col = "white",
		 axes = F,
		 ylim = ylimCustom,
		 ylab = ylab,
		 xlab = "Time")

	# redraw axis
	axis(side = 1,
		 at = date(paste0(xAxisCustom, "-01-01")),
		 labels = xAxisCustom)
	if(ylimCustom[1] == -1) {
		yAxisStep <- 0.2
	} else {
		yAxisStep <- 0.1
	}
	axis(side = 2,
		 at = seq(ylimCustom[1], ylimCustom[2], yAxisStep))
	for(i in xAxisCustom) {
		abline(v = date(paste0(i, "-01-01")),
			   col = rgb(.8, .8, .8),
			   lty = 2)
	}
	for(i in seq(ylimCustom[1], ylimCustom[2], yAxisStep)) {
		if(i == 0) {
			# highlight a guide line at "0"
			abline(h = i,
				   col = "black",
				   lty = 3,
				   lwd = 1.3)
		} else {
			abline(h = i,
				   col = rgb(.8, .8, .8),
				   lty = 2)
		}
	}
	box()

	# draw a guide line at "0"
	abline(h = 0,
		   lty = 3,
		   lwd = 1.3,
		   col = "black")

	# draw historical data
	lines(y = serie[, matchCol][which(condStart)],
		  x = serie$date[which(condStart)])

	# draw data of monitoring period
	lines(y = serie[, matchCol][which(condMoni)],
		  x = serie$date[which(condMoni)],
		  col = "red",
		  type = "o",
		  pch = 20)

	# draw stable history points
	points(y = serie[, matchCol][which(condHist)],
		   x = serie$date[which(condHist)],
		   pch = 20,
		   col = "darkgreen")

	# draw fit based on stable history
	lines(y = bfmOut$tspp$prediction,
		  x = serie$date[which(condPred)],
		  col = "blue",
		  lwd = 1.5)

	# draw line of start of the monitoring period
	abline(v = as.Date(date_decimal(bfmOut$monitor[1]), format = "YYYY-MM-DD"),
		   lty = 2,
		   lwd = 1.2)

	# draw line of time of detected break (if it exists)
	if(is.na(!bfmOut$mefp$breakpoint)) {
		abline(v = serie$date[c(which(condHist), which(condMoni))[bfmOut$mefp$breakpoint]-1],
			   lty = 2,
			   lwd = 2,
			   col = "red")
	}
}
plotBfmLegend <- function() {
	# placeholder blank plot
	par(mar = c(4, 0, 0, 0) + 0.1)
	plot(0,
		 col = "white",
		 axes = F,
		 xlab = "",
		 ylab = "")

	legendText <- c(
		"Historical data",
		"Stable history",
		"New data",
		"Fit using stable history",
		"Start of monitoring",
		"Detected break"
	)

	# first, draw the lines
	legend(x = "center",
		   x.intersp = 2,
		   y.intersp = 1.5,
		   legend = legendText,
		   text.col = "white",
		   bty = "n",
		   lty = c(rep(1, 4), rep(2, 2)),
		   lwd = c(rep(1, 2), 1.2, 1.5, .9, 2),
		   col = c(rep("black", 2), "red", "blue", "black", "red"))

	# then, draw the points
	legend(x = "center",
		   x.intersp = 2,
		   y.intersp = 1.5,
		   legend = legendText,
		   bty = "n",
		   col = c(rgb(0,0,0,0), "darkgreen", "red", rep(rgb(0,0,0,0), 3)),
		   pch = 20)
}

plotBf01 <- function(serie, matchCol, bf01Out, xAxisCustom, ylimCustom, ylab) {
	# background blank plot
	par(mar = c(4, 4, 0, 0) + 0.1)
	plot(y = serie[, matchCol],
		 x = serie$date,
		 col = "white",
		 axes = F,
		 ylim = ylimCustom,
		 ylab = ylab,
		 xlab = "Time")

	# redraw axis
	axis(side = 1,
		 at = date(paste0(xAxisCustom, "-01-01")),
		 labels = xAxisCustom)
	if(ylimCustom[1] == -1) {
		yAxisStep <- 0.2
	} else {
		yAxisStep <- 0.1
	}
	axis(side = 2,
		 at = seq(ylimCustom[1], ylimCustom[2], yAxisStep))
	for(i in xAxisCustom) {
		abline(v = date(paste0(i, "-01-01")),
			   col = rgb(.8, .8, .8),
			   lty = 2)
	}
	for(i in seq(ylimCustom[1], ylimCustom[2], yAxisStep)) {
		if(i == 0) {
			# highlight a guide line at "0"
			abline(h = i,
				   col = "black",
				   lty = 3,
				   lwd = 1.3)
		} else {
			abline(h = i,
				   col = rgb(.8, .8, .8),
				   lty = 2)
		}
	}
	box()

	# draw a guide line at "0"
	abline(h = 0,
		   lty = 3,
		   lwd = 1.3,
		   col = "black")

	lines(
		y = as.zoo(bf01Out)$response,
		x = serie$date,
		col = "black",
		lwd = 1
	)
	lines(
		y = as.zoo(bf01Out)$fitted,
		x = serie$date,
		col = "blue",
		lwd = 2
	)

	if(bf01Out$breaks == 1) {
		# bp
		abline(
			v = serie$date[bf01Out$confint[2]],
			lty = 2,
			lwd = 2,
			col = "red"
		)
	}
}
plotBf01Legend <- function() {
	# placeholder blank plot
	par(mar = c(4, 0, 0, 0) + 0.1)
	plot(0,
		 col = "white",
		 axes = F,
		 xlab = "",
		 ylab = "")

	legendText <- c(
		"Data",
		"Fitted model",
		"Detected break"
	)

	# draw the lines
	legend(x = "center",
		   x.intersp = 2,
		   y.intersp = 1.5,
		   legend = legendText,
		   bty = "n",
		   lty = c(1, 1, 2),
		   lwd = c(1, 1, 2),
		   col = c("black", "blue", "red"))
}

plotBfast <- function(serie, matchCol, bfastOut, xAxisCustom, ylimCustom, ylab) {
	# background blank plot
	par(mar = c(4, 4, 0, 0) + 0.1)
	plot(y = serie[, matchCol],
		 x = serie$date,
		 col = "white",
		 axes = F,
		 ylim = ylimCustom,
		 ylab = ylab,
		 xlab = "Time")

	# redraw axis
	axis(side = 1,
		 at = date(paste0(xAxisCustom, "-01-01")),
		 labels = xAxisCustom)
	if(ylimCustom[1] == -1) {
		yAxisStep <- 0.2
	} else {
		yAxisStep <- 0.1
	}
	axis(side = 2,
		 at = seq(ylimCustom[1], ylimCustom[2], yAxisStep))
	for(i in xAxisCustom) {
		abline(v = date(paste0(i, "-01-01")),
			   col = rgb(.8, .8, .8),
			   lty = 2)
	}
	for(i in seq(ylimCustom[1], ylimCustom[2], yAxisStep)) {
		if(i == 0) {
			# highlight a guide line at "0"
			abline(h = i,
				   col = "black",
				   lty = 3,
				   lwd = 1.3)
		} else {
			abline(h = i,
				   col = rgb(.8, .8, .8),
				   lty = 2)
		}
	}
	box()

	# draw a guide line at "0"
	abline(h = 0,
		   lty = 3,
		   lwd = 1.3,
		   col = "black")

	lines(y = bfastOut$Yt, x = as.Date(time(bfastOut$Yt)), col = "gray", lwd = 2)
	lines(y = bfastOut$output[[1]]$Tt + bfastOut$output[[1]]$St, x = as.Date(time(bfastOut$Yt)), col = "blue", lwd = 2)

	if(!bfastOut$nobp$Vt & !is.na(bfastOut$output[[1]]$Vt.bp)[1]) {
		for(i in 1:length(bfastOut$output[[1]]$Vt.bp)) {
			abline(
				v = as.Date(time(bfastOut$Yt))[bfastOut$output[[1]]$Vt.bp[i]],
				lty = 2,
				lwd = 1.5,
				col = "red"
			)
		}
	}
	if(!bfastOut$nobp$Wt & !is.na(bfastOut$output[[1]]$Wt.bp)[1]) {
		for(i in 1:length(bfastOut$output[[1]]$Wt.bp)) {
			abline(
				v = as.Date(time(bfastOut$Yt))[bfastOut$output[[1]]$Wt.bp[i]],
				lty = 2,
				lwd = 1.5,
				col = "darkgreen"
			)
		}
	}
}
plotBfastLegend <- function() {
	# placeholder blank plot
	par(mar = c(4, 0, 0, 0) + 0.1)
	plot(0,
		 col = "white",
		 axes = F,
		 xlab = "",
		 ylab = "")

	legendText <- c(
		"Data",
		"Fitted model",
		"Detected trend break",
		"Detected season break"
	)

	# draw the lines
	legend(x = "center",
		   x.intersp = 2,
		   y.intersp = 1.5,
		   legend = legendText,
		   bty = "n",
		   lty = c(1, 1, 2, 2),
		   lwd = c(1, 2, 2, 2),
		   col = c("black", "blue", "red", "darkgreen"))
}

# --------------------------------------------------------------- DEFS ----

# define custom AwesomeIcons markers
iconSet <- awesomeIconList(
	# unselected marker
	marker = makeAwesomeIcon(
		icon = "circle",
		markerColor = "lightgray",
		iconColor = "white",
		library = "fa"
	),
	# selected marker
	markerSel = makeAwesomeIcon(
		icon = "circle",
		markerColor = "blue",
		iconColor = "white",
		library = "fa"
	)
)

# TODO OLD TO NEW
# define all available satellites the user can choose from:
# LHS is the alias name; RHS is the name that GEE understands
# satChoices <- c(
# 	"Landsat 5 SR" = "LT5_SR",
# 	"Landsat 7 SR" = "LE7_SR",
# 	"Landsat 8 SR" = "LC8_SR"
# )
satChoices <- c(
  "Landsat 5 SR" = "LT05/C01/T1_SR",
  "Landsat 7 SR" = "LE07/C01/T1_SR",
  "Landsat 8 SR" = "LC08/C01/T1_SR"
)

# coordinates (long, lat, zoom) of the default fixed center
viewCenter <- c(-5.98, 24.69, 3)

# TODO OLD TO NEW
# defining colors & pch for points from differente satellite sources
# satPar <- function(x) {
# 	customPalette <- brewer.pal(length(satChoices) + 1, "Dark2")
# 	switch(
# 		x,
# 		"LT5_SR" = c(customPalette[1], 15),
# 		"LE7_SR" = c(customPalette[2], 16),
# 		"LC8_SR" = c(customPalette[3], 17),
# 		"Mixed" = c(customPalette[4], 18)
# 	)
# }
satPar <- function(x) {
  customPalette <- brewer.pal(length(satChoices) + 1, "Dark2")
  switch(
    x,
    "LT05/C01/T1_SR" = c(customPalette[1], 15),
    "LE07/C01/T1_SR" = c(customPalette[2], 16),
    "LC08/C01/T1_SR" = c(customPalette[3], 17),
    "Mixed" = c(customPalette[4], 18)
  )
}

# create a .md file from the following .Rmd, in order to be embedded into
# the Shiny app
knit(input = "./md/tutorial.Rmd", output = "md/tutorial.md", quiet = T)

# longlat projection default CRS
proj_ll <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

proj_utm <- function(shape) {
	p <- "+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +units=m"
	paste0(p, " +zone=", zoneCalc(extent(shape)[1]))
}

zoneCalc <- function(long) {
	(floor((long + 180)/6) %% 60) + 1
}


# global tsr

# ------
# INICIO
# ------

# leitura do nome de todos os arquivos ".grd" na wd
arq <- dir(path = "data", pattern = ".grd")
arq <- strtrim(arq, nchar(arq)-4)

# cores customizadas para plotar os graficos da consultas
colDist <- colorRampPalette(c("red", "white", "green"))
colSim <- colorRampPalette(c("green", "white", "red"))

# -----------------
# FUNCOES AUXILARES
# -----------------

# distancia DTW (escrita em C)
cppFunction('double distDTWC(NumericVector x, NumericVector y) {
				int nrow = x.size(), ncol = y.size();
				double cost;
				double min;
				NumericMatrix d(nrow+1,ncol+1);

				for(int i = 0; i < nrow+1; ++i)
				d(i,0) = INFINITY;
				for(int j = 0; j < ncol+1; ++j)
				d(0,j) = INFINITY;
				d(0,0) = 0;

				for(int i = 1; i < nrow+1; ++i) {
				for(int j = 1; j < ncol+1; ++j) {
				cost = sqrt(pow(x[i-1],2.0) + pow(y[j-1],2.0));

				min = d(i-1,j);
				if(d(i,j-1)<min) min = d(i,j-1);
				if(d(i-1,j-1)<min) min = d(i-1,j-1);

				d(i,j) = cost + min;
				}
				}

				return d(nrow,ncol);
				}')

# distancia generalizada de Minkowski (escrita em C)
cppFunction('double distMinkC(NumericVector x, NumericVector y, double pot) {
				int n = y.size();
				double out = 0;

				for(int i = 0; i < n; ++i) {
				out = out + pow(fabs(y[i] - x[i]), pot);
				}

				out = pow(out, 1/pot);

				return out;
				}')

# distancia de Chebyshev [Minkowski quando pot->Inf] (escrita em C)
cppFunction('double distChebC(NumericVector x, NumericVector y) {
				int n = y.size();
				double out = 0;

				for(int i = 0; i < n; ++i) {
				if(fabs(y[i] - x[i]) > out)
				out = fabs(y[i] - x[i]);
				}

				return out;
				}')

# similaridade do cosseno (escrita em C)
cppFunction('double simCosC(NumericVector x, NumericVector y) {
				int n = y.size();
				double dot = 0.0;
				double denom_a = 0.0;
				double denom_b = 0.0;

				for(int i = 0; i < n; ++i) {
				dot += x[i]*y[i];
				denom_a += x[i]*x[i];
				denom_b += y[i]*y[i];
				}

				return dot / (sqrt(denom_a) * sqrt(denom_b));
				}')



# GP aba
pgm <- function(serie) {
	n <- length(serie)
	m <- n/2 - 1
	I <- abs(fft(serie)/sqrt(n))^2  # periodogram
	P <- (4/n)*I                    # scaled-periodogram
	f <- 0:m/n
	list(f=f, P=P[1:(m+1)])
}

tsDistances <- data.frame(
	name = c(
		"cor", "fourier", "sts", "spec.llr", "int.per", "acf", "euclidean",
		"manhattan", "infnorm", "cid", "cort", "mindist.sax", "pacf",
		"ccor", "per", "ar.mah.statistic", "ar.pic", "erp", "edr", "dtw"
	),
	acronym = c(
		"PEC", "FOU", "STS", "LLR", "INP", "ACF", "EUC",
		"MAN", "INF", "CID", "CRT", "SAX", "PCF", "COR",
		"PER", "ARM", "ARP", "ERP", "EDR", "DTW"
	),
	type = c(
		"eq", "eq", "eq", "eq", "eq", "un", "eq", "eq", "eq", "eq", "eq",
		"un", "un", "un", "eq", "un", "un", "un", "un", "un"
	)
)

distType <- c("eq") # "eq", "un"
numCores <- 4

# distance matrices calculation
# distance matrices calculation
distMat <- function(input, p) {
	registerDoMC(cores = numCores)

	argListParallel <- list()
	argListParallel$j <- 1:numCores
	argListParallel$.packages <- packs
	argListParallel$.export <- c("tsDistances")
	out <- do.call(foreach, args = argListParallel) %dopar% {
		# divisao para dataset com "eq", "un"
		if(j == 1) iDist <- 1:12
		if(j == 2) iDist <- 13:16
		if(j == 3) iDist <- 17:18
		if(j == 4) iDist <- 19:20

		result <- list()
		for(i in iDist) {
			# elapsed <- proc.time()

			argList <- list()
			argList$distance <- as.character(tsDistances$name[i])
			if(argList$distance %in% c("edr","lcss")) argList$epsilon <- 0.1
			if(argList$distance == "erp") argList$g <- 0
			if(argList$distance == "tquest") argList$tau <- 50
			if(argList$distance == "pred") argList$h <- 6
			if(argList$distance == "mindist.sax") argList$w <- 40
			if(argList$distance == "lb.keogh") argList$window.size <- 9
			if(argList$distance == "dtw") argList$distance.only <- T
			if(p == 0) argList$X <- lapply(input, function(x) x$Yt)
			if(p == 1) argList$X <- lapply(input, function(x) x$Tt)
			result[[which(iDist == i)]] <- do.call(TSDatabaseDistances, argList)
		}

		return(result)
	}

	# concatenate outputs from each core
	out <- c(out[[1]], out[[2]], out[[3]], out[[4]])

	if(p == 2) {
		# trend breakpoints-based distance
		tmp <- lapply(input, function(x) x$bp.T)
		tmp2 <- unlist(lapply(tmp, function(x) {
			if(sum(is.na(x[[1]]))) 0
			else length(x)
		}))
		out[[nrow(tsDistances) + 1]] <- dist(tmp2, method = "manhattan")

		# trend piecewise-parameters-based distance
		minYear <- floor(min(unlist(lapply(input, function(x) { min(time(x$Yt)) }))))
		tmp <- lapply(input, function(x) {
			# y = m*x + b
			m <- NULL
			b <- NULL
			w <- 1 # temporal weight
			l <- length(x$Tt)
			# use the first 2 points of the TS to calculate the first m and b
			m <- (x$Tt[2] - x$Tt[1])/(time(x$Tt)[2] - time(x$Tt)[1])
			b <- x$Tt[2] - m*(time(x$Tt)[2] - minYear)
			if(!is.na(x$bp.T)[1]) {
				w <- x$bp.T[1]/l
				for(i in 1:length(x$bp.T)) {
					# use 2 points before each BP to calculate remaining m and b
					m <- c(m, (x$Tt[x$bp.T[i]+2] - x$Tt[x$bp.T[i]+1])/(time(x$Tt)[x$bp.T[i]+2] - time(x$Tt)[x$bp.T[i]+1]))
					b <- c(b, x$Tt[x$bp.T[i]+2] - m[i+1]*(time(x$Tt)[x$bp.T[i]+2] - minYear))
					if(i > 1) {
						w <- c(w, (x$bp.T[i] - x$bp.T[i-1])/l)
					}
				}
				w <- c(w, (l - x$bp.T[i])/l)
			}
			data.frame(m = m, b = b, w = w)
		})
		tmp2 <- lapply(tmp, function(x) {
			c(weighted.mean(x$m, x$w),
			  weighted.mean(x$b, x$w))
		})
		tmp2 <- matrix(unlist(tmp2), ncol = 2, byrow = T)
		out[[nrow(tsDistances) + 2]] <- dist(tmp2, method = "euclidean")

		# seasonal parameters-based distance
		tmp <- lapply(input, function(x) {
			# amplitude
			amp <- max(x$St)-min(x$St)
			# frequency
			a <- x$St
			p <- pgm(a)
			mp <- sort(p$P, TRUE)[1:5]
			# weighted mean of frequencies using peridogram values
			freq = sum((length(a)/as.numeric(names(mp)) * mp)/sum(mp))
			data.frame(amp = amp, freq = freq)
		})
		tmp2 <- matrix(unlist(tmp), ncol = 2, byrow = T)
		out[[nrow(tsDistances) + 3]] <- dist(tmp2, method = "euclidean")
	}

	return(out)
}

gpFunctions <- c("+", "-", "*", "sSqrt", "sDiv", "sLn", "abs")

# GP aux function
gpAux <- function(dTest,
						dVal,
						dTrain,
						classifTest,
						classifVal,
						classifTrain,
						seed) {

	# GP parameters
	sLn <- function(a) ifelse(a <= 0, 0, log(a))
	sDiv <- function(a,b) ifelse(b == 0, 1, a/b)
	sSqrt <- safeSqroot
	customFunctionSet <- functionSet(list = gpFunctions)
	customConstantSet <- constantFactorySet(function() rnorm(1))
	customInputVariableSet <- inputVariableSet(list = names(dTrain))

	# fitness function is a minimization problem
	customFitnessFunction <- function(f) {
		if(class(suppressWarnings(do.call(f, dTrain))) != "dist") {
			return(Inf)
		} else{
			# kNN classifier
			groups <- knn_training_function(
				dataset = matrix(nrow = length(classifTrain)),
				distance = suppressWarnings(do.call(f, dTrain) %>% as.matrix()),
				label = cbind(as.character(classifTrain)),
				k = 10 # kNN
			)
			# normalized_accuracy
			groups <- factor(groups, levels(classifTrain))
			confMat <- table(classifTrain, groups)
			accTrain <- mean(c(diag(confMat)[1] / sum(confMat[1,]),
									 diag(confMat)[2] / sum(confMat[2,])))
			return(1 - accTrain)
		}
	}

	# progress monitor to keep individuals
	indiv <- list()
	indiv$fit <- indiv$pop <- indiv$score <- NULL
	customProgressMonitor <- function(population, objectiveVectors, fitnessFunction, stepNumber, evaluationNumber, bestFitness, timeElapsed, ...) {
		# keeping v individuals for validation set
		v <- 10
		ord <- order(objectiveVectors$fitnessValues)[1:v]
		indiv$fit <<- c(indiv$fit, 1 - objectiveVectors$fitnessValues[ord])
		indiv$pop <<- c(indiv$pop, population[ord])
	}

	set.seed(seed)
	popSize <- 100
	gpResult <- geneticProgramming(functionSet = customFunctionSet,
											 inputVariables = customInputVariableSet,
											 constantSet = customConstantSet,
											 fitnessFunction = customFitnessFunction,
											 crossoverFunction = pryr::partial(crossover, crossoverprob = 0.9),
											 mutationFunction = pryr::partial(mutateSubtree, mutatesubtreeprob = 0.1, funcset = customFunctionSet, inset = customInputVariableSet, conset = customConstantSet),
											 populationSize = popSize,
											 stopCondition = makeStepsStopCondition(50),
											 searchHeuristic = makeAgeFitnessComplexityParetoGpSearchHeuristic(lambda = popSize*0.25, crossoverProbability = 0.9, newIndividualsMaxDepth = 10, enableComplexityCriterion = T),
											 progressMonitor = customProgressMonitor)

	gpResult$fitnessValues <- 1 - gpResult$fitnessValues
	bestSolTrain <- gpResult$population[gpResult$fitnessValues == max(gpResult$fitnessValues)]
	bestSolTrain <- bestSolTrain[[which.max(sapply(bestSolTrain, funcDepth))]]

	# validation set
	for(i in 1:length(indiv$pop)) {
		groups <- knn_training_function(
			dataset = matrix(nrow = length(classifVal)),
			distance = suppressWarnings(do.call(indiv$pop[[i]], dVal) %>% as.matrix()),
			label = cbind(as.character(classifVal)),
			k = 10 # kNN
		)
		groups <- factor(groups, levels(classifVal))
		confMat <- table(classifVal, groups)
		accVal <- mean(c(diag(confMat)[1] / sum(confMat[1, ]),
							  diag(confMat)[2] / sum(confMat[2, ])))
		indiv$score <- c(indiv$score, mean(c(indiv$fit[i], accVal)) - sd(c(indiv$fit[i], accVal)))
	}

	bestSolVal <- indiv$pop[indiv$score == max(indiv$score)]
	bestSolVal <- bestSolVal[[which.max(sapply(bestSolVal, funcDepth))]]

	# test set
	accTest <- NULL
	## without validation set
	groups <- knn_training_function(
		dataset = matrix(nrow = length(classifTest)),
		distance = suppressWarnings(do.call(bestSolTrain, dTest) %>% as.matrix()),
		label = cbind(as.character(classifTest)),
		k = 10 # kNN
	)
	groups <- factor(groups, levels(classifTest))
	confMat <- table(classifTest, groups)
	wrongTrain <- which(classifTest != groups)
	accTest[1] <- mean(c(diag(confMat)[1] / sum(confMat[1,]),
								diag(confMat)[2] / sum(confMat[2,])))

	## with validation set
	groups <- knn_training_function(
		dataset = matrix(nrow = length(classifTest)),
		distance = suppressWarnings(do.call(bestSolVal, dTest) %>% as.matrix()),
		label = cbind(as.character(classifTest)),
		k = 10 # kNN
	)
	groups <- factor(groups, levels(classifTest))
	confMat <- table(classifTest, groups)
	wrongVal <- which(classifTest != groups)
	accTest[2] <- mean(c(diag(confMat)[1] / sum(confMat[1,]),
								diag(confMat)[2] / sum(confMat[2,])))
	accTest <- matrix(accTest, ncol = 2)
	colnames(accTest) <- c("without_val", "with_val")

	# prepare list for output
	res <- list()
	res$bestSolTrain <- bestSolTrain
	res$bestSolVal <- bestSolVal
	res$wrongTrain <- wrongTrain
	res$wrongVal <- wrongVal
	res$accTest <- accTest

	return(res)
}

# GP run function
gpRun <- function(dDec,
						dRaw,
						classif,
						seed,
						distType = "eq",
						nFolds = 5) {
	# subset tsDistances if only unequal distances are used
	if(distType == "un") tsDistances <- tsDistances[tsDistances$type == distType, ]

	# create k aproximatelly equally sized folds
	k <- nFolds
	set.seed(seed)
	folds <- as.character(classif)
	folds[which(folds == levels(classif)[1])] <- sample(cut(1:length(which(folds == levels(classif)[1])), breaks = k, labels = F))
	folds[which(folds == levels(classif)[2])] <- sample(cut(1:length(which(folds == levels(classif)[2])), breaks = k, labels = F))
	folds <- as.numeric(folds)

	# define number of cores for parallel programming
	argList <- list()
	registerDoMC(cores = nFolds)

	# run GP with k-fold CV
	argList$iTest <- 1:k
	argList$.packages <- packs
	argList$.export <- c("gpAux", "gpFunctions", "tsDistances")
	gpOut <- do.call(foreach, argList) %dopar% {
		iVal <- iTest + 1
		if(iVal > nFolds) iVal <- 1

		# split fold data into test/validation/train
		indTest <- which(folds == iTest, arr.ind = T)
		indVal <- which(folds == iVal, arr.ind = T)
		classifTest <- classif[indTest]
		classifVal <- classif[indVal]
		classifTrain <- classif[-c(indTest, indVal)]

		# split distance matrix data into test/validation/train and run GP
		dRawTest <- dRawVal <- dRawTrain <- dDecTest <- dDecVal  <- dDecTrain <- list()

		## GPraw
		dRawTest <- lapply(dRaw, function(x) {
			as.dist(as.matrix(x)[indTest, indTest])
		})
		dRawVal <- lapply(dRaw, function(x) {
			as.dist(as.matrix(x)[indVal, indVal])
		})
		dRawTrain <- lapply(dRaw, function(x) {
			as.dist(as.matrix(x)[-c(indTest, indVal), -c(indTest, indVal)])
		})
		names(dRawTest) <- names(dRawVal) <- names(dRawTrain) <- as.character(tsDistances$acronym)
		gpRawRes <- gpAux(
			dTest = dRawTest,
			dVal = dRawVal,
			dTrain = dRawTrain,
			classifTest = classifTest,
			classifVal = classifVal,
			classifTrain = classifTrain,
			seed = seed
		)

		## GPdec
		dDecTest <- lapply(dDec, function(x) {
			as.dist(as.matrix(x)[indTest, indTest])
		})
		dDecVal <- lapply(dDec, function(x) {
			as.dist(as.matrix(x)[indVal, indVal])
		})
		dDecTrain <- lapply(dDec, function(x) {
			as.dist(as.matrix(x)[-c(indTest, indVal), -c(indTest, indVal)])
		})
		names(dDecTest) <- names(dDecVal) <- names(dDecTrain) <- c(paste0(as.character(tsDistances$acronym), "_T"), paste0(as.character(tsDistances$acronym), "_S"), paste0(rep("DEC"), 1:3))
		gpDecRes <- gpAux(
			dTest = dDecTest,
			dVal = dDecVal,
			dTrain = dDecTrain,
			classifTest = classifTest,
			classifVal = classifVal,
			classifTrain = classifTrain,
			seed = seed
		)

		# change the environment of bestSol due to the parallelization
		# If not fixed, the Rdata file size would be very large due to some
		# environment related problems
		oldEnv <- c(environment(gpRawRes$bestSolTrain),
						environment(gpDecRes$bestSolTrain))
		environment(gpRawRes$bestSolTrain) <- .GlobalEnv
		environment(gpRawRes$bestSolVal) <- .GlobalEnv
		environment(gpDecRes$bestSolTrain) <- .GlobalEnv
		environment(gpDecRes$bestSolVal) <- .GlobalEnv

		# baseline results (individual distance/similarity functions)
		accBaseline <- NULL
		wrongBaseline <- list()
		for(l in 1:length(dRawTest)) {
			groups <- knn_training_function(
				dataset = matrix(nrow = length(classifTest)),
				distance = suppressWarnings(dRawTest[[l]] %>% as.matrix()),
				label = cbind(as.character(classifTest)),
				k = 10 # kNN
			)
			groups <- factor(groups, levels(classifTest))
			confMat <- table(classifTest, groups)
			wrongBaseline[[l]] <- which(classifTest != groups)
			accBaseline[l] <- mean(c(diag(confMat)[1] / sum(confMat[1,]),
											 diag(confMat)[2] / sum(confMat[2,])))
		}
		accBaseline <- matrix(accBaseline, ncol = length(dRawTest))
		colnames(accBaseline) <- as.character(tsDistances$acronym)

		# prepare list for output
		out <- list()
		out$oldEnv <- oldEnv
		out$rawBestIndTrain <- gpRawRes$bestSolTrain
		out$rawBestIndVal <- gpRawRes$bestSolVal
		out$decBestIndTrain <- gpDecRes$bestSolTrain
		out$decBestIndVal <- gpDecRes$bestSolVal
		out$rawWrongTrain <- gpRawRes$wrongTrain
		out$rawWrongVal <- gpRawRes$wrongVal
		out$decWrongTrain <- gpDecRes$wrongTrain
		out$decWrongVal <- gpDecRes$wrongVal
		out$baseWrong <- wrongBaseline
		out$rawAcc <- gpRawRes$accTest
		out$decAcc <- gpDecRes$accTest
		out$baseAcc <- accBaseline

		return(out)
	}

	for(i in 1:k) {
		for(j in 1:2) {
			rm(list = ls(envir = gpOut[[i]]$oldEnv[[j]]),
				envir = gpOut[[i]]$oldEnv[[j]])
		}
		gpOut[[i]]$oldEnv <- NULL
	}

	if(Sys.info()[["sysname"]] == "Windows") {
		close(pb)
		stopCluster(cl)
	}

	return(gpOut)
}

# GP raw vs decomposition comparison
gpPlotFold <- function(res, titleText = "") {
	nFolds <- length(res)

	defaultPar <- par()
	par(mar = c(5, 4, 4, 10) + .1)

	plot(0, 0,
		  xlim = c(1, nFolds),
		  ylim = c(0, 1),
		  col = "white",
		  xlab = "fold",
		  ylab = "balanced accuracy",
		  yaxp = c(0,1,10),
		  main = titleText)
	abline(h = seq(0,1,.1), v = 1:5, col = "gray", lty = 3)
	box()

	par(xpd = T)

	# baseline
	basAcc <- sapply(res, function(x) { x$baseline })
	for(i in 1:nrow(basAcc)) {
		lines(
			y = basAcc[i,],
			x = 1:nFolds,
			lwd = 1,
			col = rainbow(nrow(basAcc))[i]
		)
	}
	# GPdec
	decAcc <- sapply(res, function(x) { x$decAcc })
	lines(decAcc,
			col = "black",
			lwd = 2,
			lty = 1)
	# GPraw
	rawAcc <- sapply(res, function(x) { x$rawAcc })
	lines(rawAcc,
			col = "black",
			lwd = 2,
			lty = 5)

	legend(
		"right",
		inset = c(-0.41, 0),
		legend = c(
			expression("GP"["dec."]),
			expression("GP"["raw"]),
			as.character(tsDistances$acronym)
		),
		col = c(rep("black", 2), rainbow(9)),
		lty = c(1, 5, rep(1, 9)),
		lwd = c(2, 2, rep(1, 9)),
		bty = "n"
	)

	suppressWarnings(par(defaultPar))
}

gpPlotSynTree <- function(res, type = c("raw", "dec"), val = NULL) {
	type <- match.arg(type)

	if(type == "dec") {
		if(val) {
			z <- body(res$decBestIndVal)
		} else {
			z <- body(res$decBestIndTrain)
		}
	}
	if(type == "raw") {
		if(val) {
			z <- body(res$rawBestIndVal)
		} else {
			z <- body(res$rawBestIndTrain)
		}
	}

	g1 <- exprToGraph(z)

	x1 <- suppressWarnings(which(!is.na(as.numeric(g1$vertices))))
	x2 <- suppressWarnings(round(as.numeric(g1$vertices), 1))
	g1$vertices[x1] <- as.character(x2[!is.na(x2)])
	g2 <- graph(g1$edges, n = length(g1$vertices))
	V(g2)$label <- g1$vertices %>% str_split("_") %>% map_chr(1)
	V(g2)$col <-
		ifelse(
			g1$vertices %in% gpFunctions,
			"white",
			ifelse(!is.na(x2), "lightgoldenrod1", "SkyBlue")
		)
	plot(g2,
		  layout = layout.reingold.tilford(g2, root = 1),
		  edge.arrow.size = 0.8,
		  asp = -1,
		  margin = 0,
		  vertex.label.cex = 0.6,
		  vertex.size = 16,
		  vertex.color = V(g2)$col,
		  vertex.label.color = "black")
}
