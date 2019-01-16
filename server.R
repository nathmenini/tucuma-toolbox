library(shiny)
source("global.R", local = TRUE)

wtest <<- NULL

options(shiny.maxRequestSize=100*1024^4)

shinyServer(function(input, output, session) {

# ------------------------------------------------------------- SESSION ----

	# allow reconnection by a certain grace period
	session$allowReconnect(T)

# ------------------------------------------------------------- APP 1 (BFAST) ----
# ------------------------------------------------------------- SHARED ----

# reactive values
v <- reactiveValues(markerId = 1,
						  # selected marker info
						  markerSel = NULL,
						  # downloaded marker info
						  markerDown = NULL,
						  # defaults view to the center of the atlantic ocean
						  searchLoc = data.frame(lon = viewCenter[1],
						  							  lat = viewCenter[2],
						  							  zoom = viewCenter[3]))

# ---------------------------------------------------------------- MAP ----

# render leaflet map
output$leaf <- renderLeaflet({

	long <- c(-69.375, -61.375, -69.075)
	lat <- c(-8.625, -2.575, -0.825)

	long2 <- c(-73.925, -68.975, -72.575)
	lat2 <- c(-4.175, -6.425, -2.025)

	m <- leaflet(options = list(attributionControl = F)) %>%
		addTiles(
			urlTemplate = "http://{s}.google.com/vt/lyrs=s,h&x={x}&y={y}&z={z}",
			attribution = paste(
				"Map data &copy;",
				substr(Sys.Date(), 1, 4),
				" Google, Imagery &copy;",
				substr(Sys.Date(), 1, 4),
				" TerraMetrics"
			),
			options = list(
				minZoom = 3,
				maxZoom = 19,
				noWrap = T,
				subdomains = c("mt0", "mt1", "mt2", "mt3")
			)
		) %>%
		setMaxBounds(-180, -90, 180, 90) %>%
		addCircles(lng = long, lat = lat, color = "red") %>%
		addCircles(lng = long2, lat = lat2, color = "blue")
	m
})

# update map view with v$searchLoc coordinates
observeEvent(v$searchLoc, {
	leafletProxy("leaf") %>%
		setView(
			lng = v$searchLoc$lon,
			lat = v$searchLoc$lat,
			zoom = v$searchLoc$zoom
		)
})

# update map with input shapefile
observeEvent(input$file_insertShape, {
	inFile <- input$file_insertShape
	inFolder <- substr(inFile$datapath, 1, nchar(inFile$datapath) - 5)
	unzip(inFile$datapath, exdir = inFolder)
	shpPath <- paste0(substr(inFile$name, 1, tools::file_path_sans_ext(inFile$name) %>% nchar), ".shp")
	shp <- shapefile(file.path(inFolder, shpPath))
	shp <- spTransform(shp, proj_ll)
	leafletProxy("leaf") %>%
		clearShapes() %>%
		addPolylines(
			data = shp,
			weight = 4,
			opacity = 0.9,
			color = rgb(1, 1, 0)
		) %>%
		fitBounds(
			lng1 = extent(shp)[1],
			lng2 = extent(shp)[2],
			lat1 = extent(shp)[3],
			lat2 = extent(shp)[4]
		)
})

# draw markers by clicking on the map
observeEvent(input$leaf_click, {
	id <- as.character(v$markerId)
	tmp <- input$leaf_click
	v$markerId <- v$markerId + 1
	leafletProxy("leaf") %>%
		addAwesomeMarkers(
			lng = tmp$lng,
			lat = tmp$lat,
			layerId = id,
			icon = iconSet$marker
		)
})

# select markers by clicking and show a 'Selected' label next to them
observeEvent(input$leaf_marker_click, {
	tmp <- input$leaf_marker_click

	# if user clicks on an already 'Selected' marker, deselect it
	if (grepl("selected", tmp$id)) {
		leafletProxy("leaf") %>%
			addAwesomeMarkers(
				lng = tmp$lng,
				lat = tmp$lat,
				layerId = as.character(v$markerSel[3]),
				icon = iconSet$marker
			) %>%
			removeMarker("selectedLabel") %>%
			removeMarker("selectedHighlight")
		updateSelectInput(
			session,
			inputId = "select_satGet",
			choices = c("")
		)
		v$markerSel <- NULL
	} else {
		# redraw selected marker without highlighting it
		if (length(v$markerSel) > 0) {
			leafletProxy("leaf") %>%
				addAwesomeMarkers(
					lng = as.numeric(v$markerSel[1]),
					lat = as.numeric(v$markerSel[2]),
					layerId = v$markerSel[3],
					icon = iconSet$marker
				)
		}
		leafletProxy("leaf") %>%
			removeMarker(tmp$id) %>%
			removeMarker("selectedLabel") %>%
			removeMarker("selectedHighlight") %>%
			addAwesomeMarkers(
				lng = tmp$lng,
				lat = tmp$lat,
				layerId = "selectedHighlight",
				icon = iconSet$markerSel
			) %>%
			addLabelOnlyMarkers(
				lng = tmp$lng,
				lat = tmp$lat,
				layerId = "selectedLabel",
				label = "Selected",
				labelOptions = list(
					textsize = "12px",
					direction = "auto",
					noHide = T,
					offset = c(18,-40)
				),
				options = list(keyboard = F)
			)
		v$markerSel <- c(tmp$lng, tmp$lat, tmp$id)
	}
})

observeEvent(input$select_showCoords, {
	if(input$select_showCoords) {
		v$markerDown$show <- T
	} else {
		v$markerDown$show <- F
	}
})

# ----------------------------------------------------- ACTION BUTTONS ----

# update v$searchLoc with map search query
observeEvent(input$action_search, {
	# evaluate geocode, but suppress messages and warnings
	options(warn = -1)
	gc <- suppressMessages(geocode(input$select_search))
	options(warn = 0)
	if (is.na(gc$lon)) {
		# defaults view to the center of the atlantic ocean
		v$searchLoc$lon <- viewCenter[1]
		v$searchLoc$lat <- viewCenter[2]
		v$searchLoc$zoom <- viewCenter[3]
	} else {
		# if user-selected region exists, center on it
		v$searchLoc$lon <- gc$lon
		v$searchLoc$lat <- gc$lat
		v$searchLoc$zoom <- 16
	}
})

# change the state of clearMarkers ab if at least a marker is drawn
observeEvent(v$markerId, {
	shinyjs::toggleState(id = "action_clearMarkers",
								condition = v$markerId > 1,
								selector = NULL)
})

# select_satPlot observer
observeEvent(input$select_satPlot, ignoreNULL = F, handlerExpr = {
	# change the state of time series download buttons
	shinyjs::toggleState(id = "action_downloadDataRaw",
								condition = !is.null(input$select_satPlot),
								selector = NULL)
	shinyjs::toggleState(id = "action_downloadPlotRaw",
								condition = !is.null(input$select_satPlot),
								selector = NULL)
})

# change of state of getTs ab if a marker is selected
# also update select_satGet values
observeEvent(length(v$markerSel), {
	shinyjs::toggleState(id = "action_getTs",
								condition = length(v$markerSel) > 0,
								selector = NULL)
	if (length(v$markerSel) > 0) {
		updateSelectInput(
			session = session,
			inputId = "select_satGet",
			choices = c("Select the satellite product(s)..." = "",
							satChoices)
		)
	}
})

# clears all markers currently drawn on the map
observeEvent(input$action_clearMarkers, {
	leafletProxy("leaf") %>%
		clearMarkers()
	updateSelectInput(
		session,
		inputId = "select_satGet",
		choices = c("")
	)
	v$markerSel <- NULL
	v$markerId <- 1
})

# calling gee-px-ls.py to download satellite data
event_getTs <- eventReactive(input$action_getTs, {
	withProgress(message = "Downloading satellite data...",
					 value = 0,
					 expr = {
					 	validate(need(
					 		input$select_satGet != "",
					 		"Please select a satellite from the list above."
					 	))

					 	# store info about the downloaded marker
					 	v$markerDown$lat <- as.numeric(v$markerSel[2])
					 	v$markerDown$lon <- as.numeric(v$markerSel[1])

					 	# assign variable in Python with selected coordinates (lng,lat order)
					 	python.assign("coords", as.numeric(v$markerSel[1:2]))

					 	# call Python download script for each selected satellite
					 	serieList <- list()
					 	for(i in 1:length(input$select_satGet)) {
					 		# assign Python variables using Shiny inputs
					 		python.assign("satChoice", input$select_satGet[i])

					 		# execute Python download script
					 		# python.load(paste0(getwd(), "/python/gee-px-ls.py"))
					 		python.load(paste0(getwd(), "/python/gee-px-ls-new.py"))

					 		# update progress
					 		incProgress(amount = 1 / length(input$select_satGet))

					 		# get Python output and show download message
					 		if (is.null(unlist(python.get("serie")))) {
					 			showNotification(
					 				ui = paste(names(satChoices[satChoices == input$select_satGet[i]]),
					 							  "- no data available."),
					 				type = "error",
					 				duration = 4,
					 				closeButton = F
					 			)
					 			serieList[[i]] <- NA
					 		} else {
					 			showNotification(
					 				ui = paste(names(satChoices[satChoices == input$select_satGet[i]]),
					 							  "- data downloaded."),
					 				type = "message",
					 				duration = 4,
					 				closeButton = F
					 			)
					 			serieList[[i]] <- unlist(python.get("serie"))
					 		}
					 	}

					 	# check for data availability
					 	validate(
					 		need(
					 			prod(is.na(serieList)) == 0,
					 			"No data available for the chosen satellite(s) and/or region... Please change your query and try again."
					 		)
					 	)

					 	# remove from serieList those satellites with no data
					 	# also keep track of the satellites names in satOrder
					 	satOrder <- input$select_satGet[!is.na(serieList)]
					 	serieList[is.na(serieList)] <- NULL

					 	# arrange each list element form serieList as a df
					 	serie <- lapply(serieList, function(x) {
					 		tmp <- data.frame(matrix(x,
					 										 ncol = python.get("numCol"),
					 										 byrow = T))

					 		# format data type and columns names
					 		tmp[, 1] <- as.Date(tmp[, 1], format = "%Y_%m_%d")
					 		tmp[, 2:ncol(tmp)] <- apply(tmp[, 2:ncol(tmp)],
					 											 MARGIN = 2,
					 											 as.numeric)
					 		colnames(tmp) <- python.get("colNames")

					 		# exclude saturated data
					 		# filterWhich <- which(rowSums(tmp[, 2:ncol(tmp)] == 2) > 0)
					 		filterWhich <- which(rowSums(tmp[, 2:ncol(tmp)] == 2, na.rm = TRUE) > 0)
					 		if (length(filterWhich) > 0) {
					 			tmp <- tmp[-filterWhich, ]
					 		}

					 		return(tmp)
					 	})

					 	# create new column for each list element with corresponding satellite
					 	# name, also include this info as an attribute of the list
					 	for (i in 1:length(serie)) {
					 		serie[[i]]$sat <- satOrder[i]
					 	}
					 	attributes(serie) <- list(satOrder = satOrder)

					 	# update select_satPlot with all possible satellite choices to be
					 	# visualized
					 	updateSelectInput(
					 		session,
					 		inputId = "select_satPlot",
					 		choices = satChoices[which(satChoices %in% satOrder)]
					 	)

					 	# update select_index
					 	updateSelectInput(
					 		session = session,
					 		inputId = "select_index",
					 		choices = python.get("colNames")[-1]
					 	)

					 	python.assign("aux", NULL)
					 	python.assign("serie", NULL)
					 	python.assign("values", NULL)
					 	python.assign("numCol", NULL)
					 	python.assign("colNames", NULL)

					 	return(serie)
					 })
})

# download plot modal
observeEvent(input$action_downloadPlotRaw, {
	showModal(modalDownloadPlot(type = "Raw"))
})

observeEvent(input$action_downloadPlotBfm, {
	showModal(modalDownloadPlot(type = "Bfm"))
})

observeEvent(input$action_downloadPlotBf01, {
	showModal(modalDownloadPlot(type = "Bf01"))
})

observeEvent(input$action_downloadPlotBfast, {
	showModal(modalDownloadPlot(type = "Bfast"))
})

# ------------------------------------------------------------- MODALS ----

modalDownloadPlot <- function(type = c("Raw", "Bfm", "Bf01", "Bfast")) {
	type = match.arg(type)

	modalDialog(
		title = "Please choose the output format",
		easyClose = T,

		div(align = "center",
			 downloadButton(outputId = paste0("action_downloadPlot", type, "_jpg"),
			 					label = "JPEG",
			 					class = "btn-primary"),
			 downloadButton(outputId = paste0("action_downloadPlot", type, "_png"),
			 					label = "PNG",
			 					class = "btn-primary"),
			 downloadButton(outputId = paste0("action_downloadPlot", type, "_svg"),
			 					label = "SVG",
			 					class = "btn-primary"),
			 downloadButton(outputId = paste0("action_downloadPlot", type, "_pdf"),
			 					label = "PDF",
			 					class = "btn-primary")),

		footer = tagList(
			modalButton("Cancel")
		)
	)
}

# ----------------------------------------------------------- ANALYSIS ----

# call to event_getTs in Map tab
# display a message if the download is successful
output$text_getTs <- renderText({
	event_getTs()
	return(
		as.character(div(
			align = "center",
			strong("Data succesfully downloaded!"),
			HTML('Please head to the <i class="fa fa-bar-chart fa-lg"></i><b>Analysis</b> tab.')
		))
	)
})

# subset from serie using select_satPlot, and merge data in a single df
serieSel <- eventReactive(input$select_satPlot, valueExpr = {
	serie <- event_getTs()

	# only show satellites selected in select_satPlot
	satOrder <- attr(serie, "satOrder")
	whichSel <- which(satOrder %in% input$select_satPlot)
	serie <- serie[whichSel]

	# join all data in a single df
	tmp <- NULL
	for (i in 1:length(whichSel)) {
		tmp <- rbind(tmp, serie[[i]])
	}
	serie <- tmp

	# sort data by date
	serie <- serie[order(serie$date), ]

	# removendo NAs
	serie <- na.omit(serie)

	# remove leap year additional day (29th Feb), if it exists
	leapDay <- grep("-02-29", serie$date)
	if(length(leapDay) > 0) {
		serie <- serie[-leapDay, ]
	}

	# group by date and satellite using median
	serie <- serie %>%
		group_by(date) %>%
		summarise_all(function(x) {
			if (typeof(x) == "character") {
				if (x %>% unique() %>% length() > 1) {
					"Mixed"
				} else {
					x[1]
				}
			} else {
				median(x)
			}
		}) %>%
		data.frame()

	return(serie)
})

observe({

	# subset the chosen index from the data
	matchCol <- which(input$select_index == colnames(serieSel()))
	satOrder <- satChoices[which(satChoices %in% input$select_satPlot)]

	# add "Mixed" to satOrder, if it exists in the data
	if("Mixed" %in% serieSel()$sat) {
		satOrder <- c(satOrder, "Mixed" = "Mixed")
	}

	# define a vector of graphical parameters (color and pch),
	# per satellite
	seriePar <- matrix(sapply(serieSel()$sat, satPar),
							 ncol = 2,
							 byrow = T)

	# custom ylim parameter
	ylimCustom <- c(0, 1)
	# if (sum(serieSel()[, matchCol] < 0) > 0) {
	# 	ylimCustom[1] <- -1
	# }
	# if (sum(serieSel()[, matchCol] > 1) > 0) {
	# 	ylimCustom[2] <- 1.5
	# }

	if (sum(serieSel()[, matchCol] < 0, na.rm = TRUE) > 0) {
		ylimCustom[1] <- -1
	}
	if (sum(serieSel()[, matchCol] > 1, na.rm = TRUE) > 0) {
		ylimCustom[2] <- 1.5
	}

	# custom x axis
	xAxisCustom <- seq(as.numeric(substr(range(serieSel()$date), 1, 4))[1] - 1,
							 as.numeric(substr(range(serieSel()$date), 1, 4))[2] + 1,
							 1)

	# bfastmonitor line segment parameter
	h <- 0.25

	bfm_formula <- switch(
		input$select_bfm_formula,
		"trend + harmon" = response ~ trend + harmon,
		"harmon" = response ~ harmon,
		"trend" = response ~ trend
	)

	# conditions to check if few data is available for bfastmonitor
	cond1 <- switch(
		input$select_bfm_formula,
		"trend + harmon" = length(serieSel()[, matchCol]) > 4,
		"harmon" = length(serieSel()[, matchCol]) > 3,
		"trend" = length(serieSel()[, matchCol]) > 2
	)
	cond2 <- floor(h * length(serieSel()[, matchCol])) > 1

	# update input$select_bfm_monitor
	updateDateInput(
		session = session,
		inputId = "select_bfm_monitor",
		min = head(serieSel()$date, 1),
		max = tail(serieSel()$date, 1),
		value = tail(serieSel()$date,
						 floor(length(serieSel()$date)*0.3))[1]
	)

	# update input$select_bf01_order
	if(input$select_bf01_formula != "trend") {
		if(input$select_bf01_formula == "harmon") {
			pars <- 1
		} else { # "trend + harmon"
			pars <- 2
		}
		orderMaxBf01 <- 0
		cond3 <- T
		while(cond3) {
			orderMaxBf01 <- orderMaxBf01 + 1
			cond3 <- (5 * (pars + 2 * orderMaxBf01)) < length(serieSel()[, matchCol]) / 2
		}
		orderMaxBf01 <- orderMaxBf01 - 1
		updateSliderInput(
			session = session,
			inputId = "select_bf01_order",
			max = orderMaxBf01
		)
	}

	# raw time series plot
	output$plot_raw <- renderPlot({
		output$action_downloadDataRaw <- downloadHandler(
			filename = paste0("be-data-ts", ".csv"),
			content = {function(file) {
				if(v$markerDown$show) {
					header <- rep("", ncol(serieSel()))
					names(header) <- c("LatLong Coordinates", as.character(c(v$markerDown$lat, v$markerDown$lon)), rep("", ncol(serieSel()) - 3))
					write.table(x = t(header), file = file, sep = ",", row.names = F)
					options(warn = -1)
					write.table(x = serieSel(), file = file, sep = ",", row.names = F, append = T)
					options(warn = 0)
				} else {
					write.table(x = serieSel(), file = file, sep = ",", row.names = F)
				}
			}}
		)

		plotRawComb <- function() {
			plotRaw(
				serie = serieSel(),
				matchCol = matchCol,
				xAxisCustom = xAxisCustom,
				ylimCustom = ylimCustom,
				ylab = toupper(colnames(serieSel())[matchCol]),
				seriePar = seriePar,
				coords = v$markerDown
			)
			plotRawLegend(
				satOrder = satOrder,
				seriePar = seriePar
			)
		}

		output$action_downloadPlotRaw_jpg <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-ts", ".jpg"),
			content = {function(file) {
				jpeg(file, width = 1080, height = 600)
				layout(mat = matrix(c(1, 2), ncol = 2),
						 widths = c(1.5, 0.5))
				plotRawComb()
				dev.off()
			}}
		)

		output$action_downloadPlotRaw_png <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-ts", ".png"),
			content = {function(file) {
				png(file, width = 1080, height = 600)
				layout(mat = matrix(c(1, 2), ncol = 2),
						 widths = c(1.5, 0.5))
				plotRawComb()
				dev.off()
			}}
		)

		output$action_downloadPlotRaw_svg <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-ts", ".svg"),
			content = {function(file) {
				svg(file, width = 16, height = 8)
				layout(mat = matrix(c(1, 2), ncol = 2),
						 widths = c(1.5, 0.5))
				plotRawComb()
				dev.off()
			}}
		)

		plotRaw(
			serie = serieSel(),
			matchCol = matchCol,
			xAxisCustom = xAxisCustom,
			ylimCustom = ylimCustom,
			ylab = toupper(colnames(serieSel())[matchCol]),
			seriePar = seriePar,
			coords = v$markerDown
		)
	})

	# bfastmonitor results plot
	output$plot_bfm <- renderPlot({
		# if(input$select_bfm_formula != "trend") {
		# 	validate(
		# 		need(
		# 			input$select_bfm_order <= orderMaxBfm,
		# 			FALSE
		# 		)
		# 	)
		# }
		validate(
			need(
				cond1 & cond2,
				"The selected history period hasn't enough observations."
			)
		)

		# run bfastmonitor
		res <- ppBfastmonitor(
			x = serieSel()[, matchCol],
			date = serieSel()$date,
			formula = bfm_formula,
			order = input$select_bfm_order,
			start = decimal_date(input$select_bfm_monitor),
			history = input$select_bfm_history,
			h = h
		)

		output$action_downloadDataBfm <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-results-bfastmonitor", ".rds"),
			content = {function(file) {
				saveRDS(object = res,
						  file = file)
			}}
		)

		plotBfmComb <- function() {
			plotBfm(
				serie = serieSel(),
				matchCol = matchCol,
				bfmOut = res,
				xAxisCustom = xAxisCustom,
				ylimCustom = ylimCustom,
				ylab = toupper(colnames(serieSel())[matchCol])
			)
			plotBfmLegend()
		}

		output$action_downloadPlotBfm_jpg <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfastmonitor", ".jpg"),
			content = {function(file) {
				jpeg(file, width = 1080, height = 600)
				layout(mat = matrix(c(1, 2), ncol = 2),
						 widths = c(1.5, 0.5))
				plotBfmComb()
				dev.off()
			}}
		)

		output$action_downloadPlotBfm_png <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfastmonitor", ".png"),
			content = {function(file) {
				png(file, width = 1080, height = 600)
				layout(mat = matrix(c(1, 2), ncol = 2),
						 widths = c(1.5, 0.5))
				plotBfmComb()
				dev.off()
			}}
		)

		output$action_downloadPlotBfm_svg <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfastmonitor", ".svg"),
			content = {function(file) {
				svg(file, width = 16, height = 8)
				layout(mat = matrix(c(1, 2), ncol = 2),
						 widths = c(1.5, 0.5))
				plotBfmComb()
				dev.off()
			}}
		)

		# plot bfastmonitor results
		plotBfm(
			serie = serieSel(),
			matchCol = matchCol,
			bfmOut = res,
			xAxisCustom = xAxisCustom,
			ylimCustom = ylimCustom,
			ylab = toupper(colnames(serieSel())[matchCol])
		)
	})

	# bfast01 results plot
	output$plot_bf01 <- renderPlot({
		if(input$select_bf01_formula != "trend") {
			validate(
				need(
					input$select_bf01_order <= orderMaxBf01,
					FALSE
				)
			)
		}

		bf01_formula <- switch(
			input$select_bf01_formula,
			"trend + harmon" = response ~ trend + harmon,
			"harmon" = response ~ harmon,
			"trend" = response ~ trend
		)

		res <- ppBfast01(
			x = serieSel()[, matchCol],
			date = serieSel()$date,
			formula = bf01_formula,
			order = input$select_bf01_order
		)

		output$action_downloadDataBf01 <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-results-bfast01", ".rds"),
			content = {function(file) {
				saveRDS(object = res,
						  file = file)
			}}
		)

		plotBf01Comb <- function() {
			plotBf01(
				serie = serieSel(),
				matchCol = matchCol,
				bf01Out = res,
				xAxisCustom = xAxisCustom,
				ylimCustom = ylimCustom,
				ylab = toupper(colnames(serieSel())[matchCol])
			)
			plotBf01Legend()
		}

		output$action_downloadPlotBf01_jpg <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfast01", ".jpg"),
			content = {function(file) {
				jpeg(file, width = 1080, height = 600)
				layout(mat = matrix(c(1, 2), ncol = 2),
						 widths = c(1.5, 0.5))
				plotBf01Comb()
				dev.off()
			}}
		)

		output$action_downloadPlotBf01_png <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfast01", ".png"),
			content = {function(file) {
				png(file, width = 1080, height = 600)
				layout(mat = matrix(c(1, 2), ncol = 2),
						 widths = c(1.5, 0.5))
				plotBf01Comb()
				dev.off()
			}}
		)

		output$action_downloadPlotBf01_svg <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfast01", ".svg"),
			content = {function(file) {
				svg(file, width = 16, height = 8)
				layout(mat = matrix(c(1, 2), ncol = 2),
						 widths = c(1.5, 0.5))
				plotBf01Comb()
				dev.off()
			}}
		)

		plotBf01(
			serie = serieSel(),
			matchCol = matchCol,
			bf01Out = res,
			xAxisCustom = xAxisCustom,
			ylimCustom = ylimCustom,
			ylab = toupper(colnames(serieSel())[matchCol])
		)
	})

	# bfast results plot
	output$plot_bfast <- renderPlot({
		res <- ppBfast(
			x = serieSel()[, matchCol],
			date = serieSel()$date,
			smoothTs = input$smooth_ts,
			startPeriod = input$select_start_period_bfast,
			endPeriod = input$select_end_period_bfast,
			h = input$select_bfast_h,
			season = input$select_bfast_season
		)

		output$action_downloadDataBfast <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-results-bfast", ".rds"),
			content = {function(file) {
				saveRDS(object = res,
						  file = file)
			}}
		)

		plotBfastComb <- function() {
			plotBfast(
				serie = serieSel(),
				matchCol = matchCol,
				bfastOut = res,
				xAxisCustom = xAxisCustom,
				ylimCustom = ylimCustom,
				ylab = toupper(colnames(serieSel())[matchCol]),
				smoothTs = input$smooth_ts
			)
			plotBfastLegend()
		}

		output$action_downloadPlotBfast_jpg <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfast", ".jpg"),
			content = {function(file) {
				jpeg(file, width = 1080, height = 600)
				layout(mat = matrix(c(1, 2), ncol = 2),
						 widths = c(1.5, 0.5))
				plotBfastComb()
				dev.off()
			}}
		)

		output$action_downloadPlotBfast_png <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfast", ".png"),
			content = {function(file) {
				png(file, width = 1080, height = 600)
				layout(mat = matrix(c(1, 2), ncol = 2),
						 widths = c(1.5, 0.5))
				plotBfastComb()
				dev.off()
			}}
		)

		output$action_downloadPlotBfast_svg <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfast", ".svg"),
			content = {function(file) {
				svg(file, width = 16, height = 8)
				layout(mat = matrix(c(1, 2), ncol = 2),
						 widths = c(1.5, 0.5))
				plotBfastComb()
				dev.off()
			}}
		)

		output$action_downloadPlotBfast_pdf <- downloadHandler(
			filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfast", ".pdf"),
			content = {function(file) {
				pdf(file, width = 16, height = 8)
				layout(mat = matrix(c(1, 2), ncol = 2),
						 widths = c(1.5, 0.5))
				plotBfastComb()
				dev.off()
			}}
		)

		plotBfast(
			serie = serieSel(),
			matchCol = matchCol,
			bfastOut = res,
			xAxisCustom = xAxisCustom,
			ylimCustom = ylimCustom,
			ylab = toupper(colnames(serieSel())[matchCol]),
			smoothTs = input$smooth_ts
		)
	})

	# raw time series legend
	output$plot_raw_legend <- renderPlot({
		plotRawLegend(
			satOrder = satOrder,
			seriePar = seriePar
		)
	})

	# bfastmonitor legend
	output$plot_bfm_legend <- renderPlot({
		plotBfmLegend()
	})

	# bfast01 legend
	output$plot_bf01_legend <- renderPlot({
		plotBf01Legend()
	})

	# bfast legend
	output$plot_bfast_legend <- renderPlot({
		plotBfastLegend()
	})
})
# ------------------------------------------------------------- APP 2 (DOWNLOAD) ----

	pixel_filedata <- reactive({
		infile <- input$pixel_datafile

		if (is.null(infile)) {
			return(NULL)
		} else {
			return(read.csv(infile$datapath, header = TRUE, sep = ","))
		}
	})

	raster_filedata <- reactive({

		infile <- input$raster_datafile
		infolder <- substr(infile$datapath, 1, nchar(infile$datapath) - 5)
		nameShape <- substr(infile$name, 1, nchar(infile$name) - 4)

		if (is.null(infile)) {
			return(NULL)
		} else {
			unzip(infile$datapath, exdir = infolder)
			shp <- shapefile(file.path(substr(infile$datapath, 1, nchar(infile$datapath) - 5), paste0(nameShape, ".shp")))
			return(list(nameShape, shp))
		}
	})

	# habilita/desabilita o botao de download conforme disponibilidade de df
	# se a condicao for satisfeita, eh habilitado
	observeEvent(input$pixel_datafile, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "pixel_botaoDownload",
			condition = !is.null(input$pixel_datafile)
		)
		shinyjs::toggleState(
			id = "pixel_showMap",
			condition = !is.null(input$pixel_datafile)
		)
	})

	observeEvent(input$pixel_botaoDownload, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "action_downloadDataPixel",
			condition = input$pixel_botaoDownload > 0,
			selector = NULL)
	})

	observeEvent(input$raster_datafile, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "raster_botaoDownload",
			condition = !is.null(input$raster_datafile)
		)

		shinyjs::toggleState(
			id = "raster_showMap",
			condition = !is.null(input$raster_datafile)
		)

	})

	observeEvent(input$raster_botaoDownload, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "action_downloadDataRaster",
			condition = input$raster_botaoDownload > 0,
			selector = NULL)
	})

	# Download pixel
	observeEvent(input$pixel_botaoDownload, {

		isolate ({
			dfCoords <- pixel_filedata()
		})

		collection <- input$pixel_versionLS
		pathAuxPixel <- getwd()

		if(collection == "new"){
			sat <- c("LT04/C01/T1_SR", "LT05/C01/T1_SR", "LE07/C01/T1_SR", "LC08/C01/T1_SR")
		} else{
			sat <- c("LT4_SR", "LT5_SR","LE7_SR", "LC8_SR")
		}


		filesInTheFolder <- list.files()
		if(paste0(input$pixel_filename, ".rds") %in% filesInTheFolder) {
			serieListPixel <<- readRDS(paste0(input$pixel_filename, ".rds"))
			startJ <- (serieListPixel %>% length()) + 1
		} else {
			serieListPixel <<- list()
			startJ <- 1
		}

		if(startJ <= nrow(dfCoords)) {
			withProgress(message = 'Downloading', value = 0, {
				for(j in startJ:nrow(dfCoords)) {
					setProgress(j / nrow(dfCoords), detail = paste0(j, "/", nrow(dfCoords)))

					# Ponto para ser baixado
					lat <- dfCoords$lat[j]
					lng <- dfCoords$long[j]
					python.assign("coords", c(lng, lat)) # <- deve estar na ordem (lng, lat)

					# Chama o script em Python para download das series
					df <- NULL
					for(i in 1:length(sat)) {
						# Define qual satelite vai ser baixado
						python.assign("satChoice", sat[i])

						# Executa o script do Python
						python.load(file.path(paste0("python-download/gee-px-ls-", collection,".py")))

						# Recebe o output do Python; se dados nao estiverem disponiveis, recebe NULL
						if (is.null(unlist(python.get("serie")))) {
							serie <- NULL
						} else {
							serie <- unlist(python.get("serie"))
						}

						if(serie %>% is.null %>% not) {
							# Transforma dados do Python em um df
							# Remove
							tmp <- matrix(serie,
											  ncol = python.get("numCol"),
											  byrow = T) %>% as.data.frame()
							isRowNA <- apply(tmp, MARGIN = 1, FUN = function(x) {
								(x == "NA") %>% sum
							})
							tmp <- tmp[isRowNA == 0, ]

							# Caso todas as linhas sejam NA, nao roda o resto do codigo
							if(nrow(tmp) > 0) {
								tmp[, 1] %<>% as.character
								tmp[, 2:python.get("numCol")] %<>% lapply(FUN = function(x) {
									x %>% as.character %>% as.numeric %>% round(4)
								})

								tmp %<>% as.data.frame()

								# Formatacao das classes dos dados e colunas do df
								tmp[, 1] <- as.Date(tmp[, 1], format = "%Y_%m_%d")
								tmp[, 2:ncol(tmp)] <- apply(tmp[, 2:ncol(tmp)],
																	 MARGIN = 2,
																	 as.numeric)
								colnames(tmp) <- python.get("colNames")

								# Exclui dados saturados, caso existam
								filterWhich <- which(rowSums(tmp[, 2:ncol(tmp)] == 2) > 0)
								if (length(filterWhich) > 0) {
									tmp <- tmp[-filterWhich, ]
								}

								# Se os dados existem, cria coluna com nome do satelite e cresce o df final
								if(tmp$date[1] %>% is.na %>% not) {
									tmp$sat <- sat[i]
									df <- rbind(df, tmp)
								}
							}
						}

						python.assign("aux", NULL)
						python.assign("serie", NULL)
						python.assign("values", NULL)
						python.assign("numCol", NULL)
						python.assign("colNames", NULL)
					}

					if(collection == "new" & df %>% is.null %>% not) {
						tmp <- intToBits(df$pixel_qa) %>% as.numeric %>% matrix(nrow = nrow(df), byrow = T)
						df$clearBit <- tmp[, 2]
						df$confBit <- tmp[, 7] + tmp[, 8] * 2
						setDT(df)
						df <- df[clearBit == 1, ]
						df <- df[, -c("pixel_qa", "clearBit")]
						df[sat == "LT04/C01/T1_SR", sat := "LSR4"]
						df[sat == "LT05/C01/T1_SR", sat := "LSR5"]
						df[sat == "LE07/C01/T1_SR", sat := "LSR7"]
						df[sat == "LC08/C01/T1_SR", sat := "LSR8"]
						setkey(df, "date")
					}

					if(collection == "old") {
						setDT(df)
						df[sat == "LT4_SR", sat := "LSR4"]
						df[sat == "LT5_SR", sat := "LSR5"]
						df[sat == "LT7_SR", sat := "LSR7"]
						df[sat == "LT8_SR", sat := "LSR8"]
					}

					serieListPixel[[j]] <<- df

					if((j %% 100 == 0) | (j == nrow(dfCoords))) {
						saveRDS(serieListPixel, paste0(input$pixel_filename, ".rds"))
					}

				}
			})
		}

		setwd(pathAuxPixel)

	})

	# # Salvando dados de Pixel
	# output$action_downloadDataPixel <- downloadHandler(
	# 	filename = paste0(input$pixel_filename, ".rds"),
	# 	content = {function(file) {
	# 			saveRDS(object = serieListPixel, file = file)
	# 		}
	# 	}
	# )

	# Download raster
	observeEvent(input$raster_botaoDownload, {

		# isolate ({
		# 	infile <- input$raster_datafile
		# 	shapePath <- file.path(substr(infile$datapath, 1, nchar(infile$datapath) - 5))
		# 	shape <- raster_filedata()[[1]]
		# })
		#
		# python.assign("msg", NULL) # msg para ser exibida ao final
		# python.assign("shape", shape) # nome do shapefile
		# python.assign("shapePath", shapePath) # path da pasta descomprimida
		# python.assign("satellite", input$raster_satellite) # numero do satelite
		# python.assign("satprod", input$raster_versionLS) # versao do landsat
		# python.assign("periodStart", as.character(input$raster_periodStart)) # data para comecar a baixar
		# python.assign("periodEnd", as.character(input$raster_periodEnd)) # data que termina de baixar
		#
		# # Seta o caminho para salvar as imagens
		# pathRaster <- file.path(tempdir())
		# python.assign("pathRaster", pathRaster)
		#
		# # Executa o script do Python
		# pathR <- getwd()
		# python.load(file.path("python-download/gee-ls-prepare.py"))
		#
		# # Pega o numero de imagens para serem baixadas
		# nRaster <- python.get("imgColLen")
		#
		# if(nRaster > 0) {
		# 	withProgress(message = 'Downloading', value = 0, {
		# 		for(i in 0:(nRaster-1)) {
		# 			setProgress((i+1) / nRaster, detail = paste0((i+1), "/", nRaster))
		# 			python.assign("i", i) # atualizada o valor de i do loop
		# 			python.load(file.path(pathR, "python-download/download-raster-ls.py"))
		# 		}
		# 	})
		# 	if(input$download_SRTM) {
		# 		python.load(file.path(pathR,"python-download/download-SRTM.py"))
		# 	}
		# }
		#
		# output$msg <- renderText({ python.get("msg") })
		#
		# setwd(pathR)

		workpath <- getwd()
		isolate ({
			infile <- input$raster_datafile
			shapePath <- file.path(substr(infile$datapath, 1, nchar(infile$datapath) - 5))
			shape <- raster_filedata()[[1]]
		})

		python.assign("msg", NULL) # nome da pasta descomprimida
		python.assign("shape", shape) # nome da pasta descomprimida
		python.assign("shapePath", shapePath) # path da pasta descomprimida
		python.assign("satellite", input$raster_satellite) # numero do satelite
		python.assign("satprod", input$raster_versionLS) # versao do landsat
		python.assign("periodStart", as.character(input$raster_periodStart)) # data para comecar a baixar
		python.assign("periodEnd", as.character(input$raster_periodEnd)) # data que termina de baixar

		# Executa o script do Python
		pathR <- getwd()
		python.load(file.path(pathR, "python-download/gee-ls-prepare.py"))

		# Pega o numero de imagens para serem baixadas
		nRaster <- python.get("imgColLen")

		if(nRaster > 0) {
			withProgress(message = 'Downloading', value = 0, {
				for(i in 0:(nRaster-1)) {
					setProgress((i+1) / nRaster, detail = paste0((i+1), "/", nRaster))
					python.assign("i", i) # atualizada o valor de i do loop
					python.load(file.path(pathR, "python-download/download-raster-ls.py"))
				}
			})
			if(input$download_SRTM) {
				python.load(file.path(pathR,"python-download/download-SRTM.py"))
			}
		}

		output$msg <- renderText({ python.get("msg") })

		setwd(workpath)

	})

	# Salvando dados de Raster
	# output$action_downloadDataRaster <- downloadHandler(
	# 	filename = paste0("images-downloaded", ".zip"),
	# 	content = {function(file) {
	# 			pathAux <- getwd()
	# 			setwd(file.path(tempdir(), "raster"))
	# 			zip(zipfile = file, files = isolate ({
	# 				shape <- raster_filedata()[[1]]
	# 			}))
	# 			setwd(pathAux)
	# 		}
	# 	}
	# )

	# Plotting
	output$pixel_leaf <- renderLeaflet({

		m3 <- leaflet(options = list(attributionControl = F))
		m3 <- addTiles(map = m3,
							urlTemplate = "http://{s}.google.com/vt/lyrs=s&x={x}&y={y}&z={z}",
							attribution = "Imagery &copy;2016 TerraMetrics",
							options = list(minZoom = 1,
												maxZoom = 16,
												noWrap = T,
												subdomains = c('mt0','mt1','mt2','mt3')))
		m3 <- setMaxBounds(m3, -180, -90, 180, 90)

		m3 <- setView(m3, lng = 22.909114, lat = -25.618960, zoom = 2)
		m3
	})

	observe({

		leafletProxy("pixel_leaf") %>%
			clearMarkers() %>%
			clearMarkerClusters()

		if(input$pixel_showMap){

			dfCoords <- pixel_filedata()

			if(input$pixel_cluster | (length(dfCoords$long) > 1000)) {
				leafletProxy("pixel_leaf") %>%
					addAwesomeMarkers(lng = dfCoords$long,
											lat = dfCoords$lat,
											label = dfCoords[,1] %>% as.character,
											icon = makeAwesomeIcon(
												icon = "circle",
												markerColor = "blue",
												iconColor = "#FFFFFF",
												library = "fa"
											),
											clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
					flyTo(lat = mean(dfCoords$lat),
							lng = mean(dfCoords$long),
							zoom = 5,
							options = list(animate = FALSE))
			} else {
				leafletProxy("pixel_leaf") %>%
					addAwesomeMarkers(lng = dfCoords$long,
											lat = dfCoords$lat,
											label = dfCoords[,1] %>% as.character,
											icon = makeAwesomeIcon(
												icon = "circle",
												markerColor = "blue",
												iconColor = "#FFFFFF",
												library = "fa"
											)) %>%
					flyTo(lat = mean(dfCoords$lat),
							lng = mean(dfCoords$long),
							zoom = 5,
							options = list(animate = FALSE))
			}


		} else {
			leafletProxy("pixel_leaf") %>% setView(lng = 0,
																lat = 0,
																zoom = 1)
		}
	})

	output$raster_leaf <- renderLeaflet({

		m2 <- leaflet(options = list(attributionControl = F))
		m2 <- addTiles(map = m2,
							urlTemplate = "http://{s}.google.com/vt/lyrs=s&x={x}&y={y}&z={z}",
							attribution = "Imagery &copy;2016 TerraMetrics",
							options = list(minZoom = 1,
												maxZoom = 16,
												noWrap = T,
												subdomains = c('mt0','mt1','mt2','mt3')))

		m2 <- setMaxBounds(m2, -180, -90, 180, 90)

		m2 <- setView(m2, lng = 22.909114, lat = -25.618960, zoom = 2)

		m2
	})

	observe({


		leafletProxy("raster_leaf") %>%
			clearShapes()

		if(input$raster_showMap){
			shp <- raster_filedata()[[2]]

			extetentShape <- shp %>% extent

			leafletProxy("raster_leaf") %>% addPolygons(data = shp, color = "white", opacity=1, fillOpacity=0) %>%
				flyTo(lat = extetentShape[3] + (extetentShape[4]-extetentShape[3])/2,
						lng = extetentShape[1] + (extetentShape[2]-extetentShape[1])/2,
						zoom = 10,
						options = list(animate = FALSE))
		} else {
			leafletProxy("raster_leaf") %>% setView(lng = 0,
																 lat = 0,
																 zoom = 1)
		}
	})

# ------------------------------------------------------------- APP 3 (TSR) ----


	# observeEvent(input$pixel_datafile, ignoreNULL = F, {
	# 	shinyjs::toggleState(
	# 		id = "pixel_botaoDownload",
	# 		condition = !is.null(input$pixel_datafile)
	# 	)
	# 	shinyjs::toggleState(
	# 		id = "pixel_showMap",
	# 		condition = !is.null(input$pixel_datafile)
	# 	)
	# })


	pathGrd <- eventReactive(input$arquivo, {

		# eh necessario renomear o nome do arquivo gri, pois o shiny copia os arquivos
		# com nomes diferentes na pasta temporaria

		old_name <- input$arquivo$datapath[2]
		extOld <- str_sub(old_name,-4,-1)
		nroName <- str_sub(input$arquivo$datapath[1],-5,-5)
		new_name <- paste0(substr(input$arquivo$datapath[2],
										  start = 1,
										  stop = nchar(input$arquivo$datapath[2])-5),
								 paste0(nroName, extOld)
		)

		system(paste("mv", old_name, new_name))

		return(input$arquivo)
	})

	# acessar com s2()
	s2 <- eventReactive(input$botaoLeitura, {
		withProgress(message = 'Reading input data...', value = NULL, {
			stack(pathGrd()$datapath[1])
		})
	})

	# acessar com v2()
	v2 <- eventReactive(input$botaoLeitura, {
		withProgress(message = 'Processing input data...', value = NULL, {
			temp <- raster::extract(s2(), 1:ncell(s2()))
			temp[is.na(temp)] <- 0
			temp
		})
	})

	output$plot_teste <- renderPlot({

		par(mar=c(4,2,1,1)+0.1)
		v2()
		image(s2()[[1]], col=rev(terrain.colors(128)), xaxt="n", yaxt="n", xlab="", ylab="")
		dfraster <- s2()[[1]] %>% rasterToPoints()
		image.plot(x = dfraster[,1], y = dfraster[,2], z = dfraster[,3], col=rev(terrain.colors(128)), xaxt="n", yaxt="n",
					  legend.only = T, horizontal=T, smallplot=c(.23,.79,.12,.14))

	})

	# Aba Visualizacao
	# ----------------
	# outputSlider_1
	observeEvent(input$botaoLeitura, {
		output$outputSlider_1 <- renderUI({
			sliderInput("t", "t", value = 1, min = 1, max = nlayers(s2()), step = 1)
		})
	})

	# outputSlider_2
	output$outputSlider_2 <- renderUI({
		sliderInput("x", "x", value = 1, min = 1, max = ncol(s2()), step = 1)
	})

	# outputSlider_3
	output$outputSlider_3 <- renderUI({
		sliderInput("y", "y", value = 1, min = 1, max = nrow(s2()), step = 1)
	})

	# plot da regiao de estudo
	output$plot_1 <- renderPlot({
		if(length(input$t) > 0) {
			par(mar=c(4,2,1,1)+0.1)
			image(s2()[[input$t]], col=rev(terrain.colors(128)), xaxt="n", yaxt="n", xlab="", ylab="")
			axis(1, at=c(extent(s2())[1],extent(s2())[2]), labels=c(1,ncol(s2())))
			axis(2, at=c(extent(s2())[3],extent(s2())[4]), labels=c(nrow(s2()),1))
			points(extent(s2())[1]+res(s2())[1]*input$x, extent(s2())[4]-res(s2())[2]*input$y, pch=19, col="white", cex=1.5)
			points(extent(s2())[1]+res(s2())[1]*input$x, extent(s2())[4]-res(s2())[2]*input$y, pch=21, col="black", cex=1.5)

			dfraster <- s2()[[input$t]] %>% rasterToPoints()
			image.plot(x = dfraster[,1], y = dfraster[,2], z = dfraster[,3], col=rev(terrain.colors(128)), xaxt="n", yaxt="n",
						  legend.only = T, horizontal=T, smallplot=c(.23,.79,.12,.14))

		}
	})

	# plot da serie temporal dado um ponto (x,y)
	output$plot_2 <- renderPlot({
		if((length(input$x) > 0) & (length(input$y) > 0)) {
			pdf('ts.pdf')
			plot(v2()[input$x + ncol(s2())*(input$y-1),], type="l", xlab="Time", ylab="NDVI", ylim=c(min(v2()),max(v2())))
			abline(v=input$t, col="red")
			dev.off()

			par(mar=c(4,2,1,1)+0.1)
			plot(v2()[input$x + ncol(s2())*(input$y-1),], type="l", xlab="Time", ylab="NDVI", ylim=c(min(v2()),max(v2())))
			abline(v=input$t, col="red")
		}
	})

	# Aba Consulta
	# ------------
	output$outputSlider_4 <- renderUI({
		sliderInput("x_querySlider", "x", value = 1, min = 1, max = ncol(s2()), step = 1)
	})

	output$outputSlider_5 <- renderUI({
		sliderInput("y_querySlider", "y", value = 1, min = 1, max = nrow(s2()), step = 1)
	})

	output$outputSlider_6 <- renderUI({
		sliderInput("t_querySlider", "t", value = 1, min = 1, max = nlayers(s2()), step = 1)
	})

	output$plot_3 <- renderPlot({
		if(length(input$t_querySlider) > 0) {

			pdf('regiao.pdf')
			image(s2()[[input$t_querySlider]], col=rev(terrain.colors(128)), xaxt="n", yaxt="n", xlab="", ylab="")
			axis(1, at=c(extent(s2())[1],extent(s2())[2]), labels=c(1,ncol(s2())))
			axis(2, at=c(extent(s2())[3],extent(s2())[4]), labels=c(nrow(s2()),1))

			dfraster <- s2()[[input$t_querySlider]] %>% rasterToPoints()
			image.plot(x = dfraster[,1], y = dfraster[,2], z = dfraster[,3], col=rev(terrain.colors(128)), xaxt="n", yaxt="n",
						  legend.only = T, horizontal=T, smallplot=c(.23,.79,.12,.14))
			dev.off()

			par(mar=c(4,2,1,1)+0.1)
			image(s2()[[input$t_querySlider]], col=rev(terrain.colors(128)), xaxt="n", yaxt="n", xlab="", ylab="")
			axis(1, at=c(extent(s2())[1],extent(s2())[2]), labels=c(1,ncol(s2())))
			axis(2, at=c(extent(s2())[3],extent(s2())[4]), labels=c(nrow(s2()),1))

			dfraster <- s2()[[input$t_querySlider]] %>% rasterToPoints()
			image.plot(x = dfraster[,1], y = dfraster[,2], z = dfraster[,3], col=rev(terrain.colors(128)), xaxt="n", yaxt="n",
						  legend.only = T, horizontal=T, smallplot=c(.23,.79,.12,.14))
		}
	})

	output$plot_3b <- renderPlot({
		if(length(input$t_querySlider) > 0) {

			pdf('regiao_2.pdf')
			image(s2()[[input$t_querySlider]], col=rev(terrain.colors(128)), xaxt="n", yaxt="n", xlab="", ylab="")
			axis(1, at=c(extent(s2())[1],extent(s2())[2]), labels=c(1,ncol(s2())))
			axis(2, at=c(extent(s2())[3],extent(s2())[4]), labels=c(nrow(s2()),1))
			points(extent(s2())[1]+res(s2())[1]*input$x_querySlider, extent(s2())[4]-res(s2())[2]*input$y_querySlider, pch=19, col="white", cex=1.5)
			points(extent(s2())[1]+res(s2())[1]*input$x_querySlider, extent(s2())[4]-res(s2())[2]*input$y_querySlider, pch=21, col="black", cex=1.5)

			dfraster <- s2()[[input$t_querySlider]] %>% rasterToPoints()
			image.plot(x = dfraster[,1], y = dfraster[,2], z = dfraster[,3], col=rev(terrain.colors(128)), xaxt="n", yaxt="n",
						  legend.only = T, horizontal = T, smallplot=c(.23,.79,.12,.14))
			dev.off()

			par(mar=c(4,2,1,1)+0.1)
			image(s2()[[input$t_querySlider]], col=rev(terrain.colors(128)), xaxt="n", yaxt="n", xlab="", ylab="")
			axis(1, at=c(extent(s2())[1],extent(s2())[2]), labels=c(1,ncol(s2())))
			axis(2, at=c(extent(s2())[3],extent(s2())[4]), labels=c(nrow(s2()),1))
			points(extent(s2())[1]+res(s2())[1]*input$x_querySlider, extent(s2())[4]-res(s2())[2]*input$y_querySlider, pch=19, col="white", cex=1.5)
			points(extent(s2())[1]+res(s2())[1]*input$x_querySlider, extent(s2())[4]-res(s2())[2]*input$y_querySlider, pch=21, col="black", cex=1.5)

			dfraster <- s2()[[input$t_querySlider]] %>% rasterToPoints()
			image.plot(x = dfraster[,1], y = dfraster[,2], z = dfraster[,3], col=rev(terrain.colors(128)), xaxt="n", yaxt="n",
						  legend.only = T, horizontal=T, smallplot=c(.23,.79,.12,.14))

		}
	})

	output$outputPlotClick <- renderPrint({
		list(x = round((as.double(input$plotClick_3$x)-extent(s2())[1])/res(s2())[1]),
			  y = round((extent(s2())[4]-as.double(input$plotClick_3$y))/res(s2())[2]))
	})

	output$testeCluster <- renderPlot(distFun())

	distFun <- eventReactive(input$botaoConsulta, {
		if (input$tipo_entrada == "Specify the point (x,y)") {
			qx <- input$x_querySlider
			qy <- input$y_querySlider
		} else {
			qx <- round((as.double(input$plotClick_3$x)-extent(s2())[1])/res(s2())[1])
			qy <- round((extent(s2())[4]-as.double(input$plotClick_3$y))/res(s2())[2])
		}

		q <- qx + ncol(s2())*(qy-1)

		dist1 <- c()

		if(length(qx) > 0 && length(qy) > 0) {
			if (input$algoritmo == "DTW Similarity") {
				for (i in 1:ncell(s2())) dist1[i] <- distDTWC(v2()[q,], v2()[i,])
			} else if (input$algoritmo == "Manhattan Distance") {
				for (i in 1:ncell(s2())) dist1[i] <- distMinkC(v2()[q,], v2()[i,], 1)
			} else if (input$algoritmo == "Euclidian Distance") {
				for (i in 1:ncell(s2())) dist1[i] <- distMinkC(v2()[q,], v2()[i,], 2)
			} else if (input$algoritmo == "Chebyshev Distance") {
				for (i in 1:ncell(s2())) dist1[i] <- distChebC(v2()[q,], v2()[i,])
			} else if (input$algoritmo == "Cosine Similarity") {
				for (i in 1:ncell(s2())) dist1[i] <- simCosC(v2()[q,], v2()[i,])
			}
			return(dist1)
		} else return(NULL)
	})

	observeEvent(input$botaoConsulta, {
		output$plot_4a <- renderPlot({

			withProgress(message = 'Processing query...', value = NULL, {
				isolate({
					if (input$tipo_entrada == "Specify the point (x,y)") {
						qx <- input$x_querySlider
						qy <- input$y_querySlider
					} else {
						qx <- round((as.double(input$plotClick_3$x)-extent(s2())[1])/res(s2())[1])
						qy <- round((extent(s2())[4]-as.double(input$plotClick_3$y))/res(s2())[2])
					}

					q <- qx + ncol(s2())*(qy-1)

					m <- matrix(0, nrow(s2()), ncol(s2()))
					# dist <- c()

					if(length(qx) > 0 && length(qy) > 0) {
						# if (input$algoritmo == "DTW Distance") {
						# 	for (i in 1:ncell(s2())) dist[i] <- distDTWC(v2()[q,], v2()[i,])
						# } else if (input$algoritmo == "Manhattan Distance") {
						# 	for (i in 1:ncell(s2())) dist[i] <- distMinkC(v2()[q,], v2()[i,], 1)
						# } else if (input$algoritmo == "Euclidian Distance") {
						# 	for (i in 1:ncell(s2())) dist[i] <- distMinkC(v2()[q,], v2()[i,], 2)
						# } else if (input$algoritmo == "Chebyshev Distance") {
						# 	for (i in 1:ncell(s2())) dist[i] <- distChebC(v2()[q,], v2()[i,])
						# } else if (input$algoritmo == "Cosine Similarity") {
						# 	for (i in 1:ncell(s2())) dist[i] <- simCosC(v2()[q,], v2()[i,])
						# }

						dist <- distFun()

						# constroi a imagem de saida
						for(i in 1:nrow(s2())) {
							for(j in 1:ncol(s2())) {
								m[i,j] <- dist[j+(i-1)*ncol(s2())]
							}
						}

						# gira a imagem de saida para plotar corretamente
						m <- t(m[nrow(m):1,])

						# plota imagem para diferentes limiares
						if(length(grep("Similarity", input$algoritmo, ignore.case = T)) > 0) {
							pdf('regiao_simi.pdf')
							image((m-min(m))/max(m-min(m)), col=colSim(29), xaxt="n", yaxt="n")
							axis(1, at=c(0,1), labels=c(1,ncol(s2())))
							axis(2, at=c(0,1), labels=c(nrow(s2()),1))
							image.plot((m-min(m))/max(m-min(m)), col=colSim(29), xaxt="n", yaxt="n",
										  legend.only=T, horizontal=T, smallplot=c(.23,.79,.12,.14),
										  legend.lab="lesser           (Similarity)           greater")
							dev.off()

							par(mar=c(4,2,1,1)+0.1)
							image((m-min(m))/max(m-min(m)), col=colSim(29), xaxt="n", yaxt="n")
							axis(1, at=c(0,1), labels=c(1,ncol(s2())))
							axis(2, at=c(0,1), labels=c(nrow(s2()),1))
							image.plot((m-min(m))/max(m-min(m)), col=colSim(29), xaxt="n", yaxt="n",
										  legend.only=T, horizontal=T, smallplot=c(.23,.79,.12,.14),
										  legend.lab="lesser           (Similarity)           greater")

						} else {

							pdf('regiao_dissimi.pdf')
							image((m-min(m))/max(m-min(m)), col=colDist(29), xaxt="n", yaxt="n")
							axis(1, at=c(0,1), labels=c(1,ncol(s2())))
							axis(2, at=c(0,1), labels=c(nrow(s2()),1))
							image.plot((m-min(m))/max(m-min(m)), col=colDist(29), xaxt="n", yaxt="n",
										  legend.only=T, horizontal=T, smallplot=c(.23,.79,.12,.14),
										  legend.lab="closer              (Distance)              further")
							dev.off()

							par(mar=c(4,2,1,1)+0.1)
							image((m-min(m))/max(m-min(m)), col=colDist(29), xaxt="n", yaxt="n")
							axis(1, at=c(0,1), labels=c(1,ncol(s2())))
							axis(2, at=c(0,1), labels=c(nrow(s2()),1))
							image.plot((m-min(m))/max(m-min(m)), col=colDist(29), xaxt="n", yaxt="n",
										  legend.only=T, horizontal=T, smallplot=c(.23,.79,.12,.14),
										  legend.lab="closer              (Distance)              further")
						}
					}
				})
			})
		})
	})

	observeEvent(input$botaoElbow, {
		if(!is.null(pathGrd())) {
			withProgress(message = 'Computing Elbow...', value = NULL, {
				dist1 <- distFun()

				dist2 <- dist(dist1)

				wss <- NULL
				for (i in 2:15){
					set.seed(100)
					wss[i] <- sum(kmeans(dist1, centers=i)$withinss)
				}

				wtest <<- wss
			})
		}
	})

	observeEvent(input$botaoCluster, {
		if(input$selectCluster > 0) {
			output$plot_5 <- renderPlot({

				withProgress(message = 'Clustering...', value = NULL, {
					isolate({
						if (input$tipo_entrada == "Specify the point (x,y)") {
							qx <- input$x_querySlider
							qy <- input$y_querySlider
						} else {
							qx <- round((as.double(input$plotClick_3$x)-extent(s2())[1])/res(s2())[1])
							qy <- round((extent(s2())[4]-as.double(input$plotClick_3$y))/res(s2())[2])
						}

						if(length(qx) > 0 && length(qy) > 0) {

							wss <- wtest

							pdf('elbow.pdf')
							plot(1:15, wss, type = "b", xlab="Number of Clusters",
								  ylab="Within groups Sum of Squares")
							points(x = input$selectCluster, y = wss[as.numeric(input$selectCluster)], col = "red", pch = 20, cex = 2)
							dev.off()

							par(mar=c(4,4,2,1)+0.1)
							plot(1:15, wss, type = "b", xlab="Number of Clusters",
								  ylab="Within groups Sum of Squares")
							points(x = input$selectCluster, y = wss[as.numeric(input$selectCluster)], col = "red", pch = 20, cex = 2)
						}
					})
				})
			})
		}
	})

	# observeEvent(input$botaoCluster, {
	# 	output$plot_5 <- renderPlot({
	#
	# 		withProgress(message = 'Clustering...', value = NULL, {
	# 			isolate({
	# 				if (input$tipo_entrada == "Specify the point (x,y)") {
	# 					qx <- input$x_querySlider
	# 					qy <- input$y_querySlider
	# 				} else {
	# 					qx <- round((as.double(input$plotClick_3$x)-extent(s2())[1])/res(s2())[1])
	# 					qy <- round((extent(s2())[4]-as.double(input$plotClick_3$y))/res(s2())[2])
	# 				}
	#
	# 				if(length(qx) > 0 && length(qy) > 0) {
	# 					dist1 <- distFun()
	#
	# 					dist2 <- dist(dist1)
	#
	# 					wss <- NULL
	# 					for (i in 2:15){
	# 						set.seed(100)
	# 						wss[i] <- sum(kmeans(dist1, centers=i)$withinss)
	# 					}
	#
	# 					par(mar=c(4,4,2,1)+0.1)
	# 					plot(1:15, wss, type = "b", xlab="Number of Clusters",
	# 						  ylab="Within groups Sum of Squares")
	# 					points(x = input$selectCluster, y = wss[as.numeric(input$selectCluster)], col = "red", pch = 20, cex = 2)
	# 				}
	# 			})
	# 		})
	# 	})
	# })

	observeEvent(input$botaoCluster, {
		if(input$selectCluster > 0) {
			output$plot_6 <- renderPlot({

				withProgress(message = 'Clustering...', value = NULL, {
					isolate({
						if (input$tipo_entrada == "Specify the point (x,y)") {
							qx <- input$x_querySlider
							qy <- input$y_querySlider
						} else {
							qx <- round((as.double(input$plotClick_3$x)-extent(s2())[1])/res(s2())[1])
							qy <- round((extent(s2())[4]-as.double(input$plotClick_3$y))/res(s2())[2])
						}

						if(length(qx) > 0 && length(qy) > 0) {

							clusters <- kmeans(distFun(), centers = input$selectCluster)

							clusterRaster <- s2()[[1]]
							values(clusterRaster) <- clusters$cluster

							pdf('regiao_cluster.pdf')
							image(clusterRaster, col = rainbow(as.numeric(input$selectCluster)), xaxt="n", yaxt="n", xlab="", ylab="")
							axis(1, at=c(extent(s2())[1],extent(s2())[2]), labels=c(1,ncol(s2())))
							axis(2, at=c(extent(s2())[3],extent(s2())[4]), labels=c(nrow(s2()),1))

							dfraster <- clusterRaster %>% rasterToPoints()
							image.plot(x = dfraster[,1], y = dfraster[,2], z = dfraster[,3], col = rainbow(as.numeric(input$selectCluster)), xaxt="n", yaxt="n",
										  legend.only = T, horizontal=T, smallplot=c(.23,.79,.12,.14),
										  legend.lab="                     (Cluster)                     ")
							dev.off()

							par(mar=c(4,2,1,1)+0.1)
							# colCluster <- colorRampPalette(brewer.pal(as.numeric(input$selectCluster), "Set2"))
							# image(clusterRaster, col=colCluster(as.numeric(input$selectCluster)), xaxt="n", yaxt="n", xlab="", ylab="")
							image(clusterRaster, col = rainbow(as.numeric(input$selectCluster)), xaxt="n", yaxt="n", xlab="", ylab="")
							axis(1, at=c(extent(s2())[1],extent(s2())[2]), labels=c(1,ncol(s2())))
							axis(2, at=c(extent(s2())[3],extent(s2())[4]), labels=c(nrow(s2()),1))

							dfraster <- clusterRaster %>% rasterToPoints()
							image.plot(x = dfraster[,1], y = dfraster[,2], z = dfraster[,3], col = rainbow(as.numeric(input$selectCluster)), xaxt="n", yaxt="n",
										  legend.only = T, horizontal=T, smallplot=c(.23,.79,.12,.14),
										  legend.lab="                     (Cluster)                     ")

						}
					})
				})
			})
		}
	})

# ------------------------------------------------------------- APP 4 (GP Classifier) ----

	areaShapefile <- reactive({

		infile <- input$shapeArea
		infolder <- substr(infile$datapath, 1, nchar(infile$datapath) - 5)
		nameShape <- substr(infile$name, 1, nchar(infile$name) - 4)

		if (is.null(infile)) {
			return(NULL)
		} else {
			unzip(infile$datapath, exdir = infolder)
			shp <- shapefile(file.path(substr(infile$datapath, 1, nchar(infile$datapath) - 5), paste0(nameShape, ".shp")))
			return(list(nameShape, spTransform(shp, proj_ll)))
		}
	})

	classShapefile <- reactive({

		infile <- input$shapeClass
		infolder <- substr(infile$datapath, 1, nchar(infile$datapath) - 5)
		nameShape <- substr(infile$name, 1, nchar(infile$name) - 4)

		if (is.null(infile)) {
			return(NULL)
		} else {
			unzip(infile$datapath, exdir = infolder)
			shp <- shapefile(file.path(substr(infile$datapath, 1, nchar(infile$datapath) - 5), paste0(nameShape, ".shp")))
			return(list(nameShape, spTransform(shp, proj_ll)))
		}
	})

	dataBFASTOut <- reactive({
		infile <- input$dataProcessed

		if (is.null(infile)) {
			return(NULL)
		} else {
			return(infile$datapath)
		}
	})

	dataGPRun <- reactive({
		infile <- input$GPdata

		if (is.null(infile)) {
			return(NULL)
		} else {
			return(infile$datapath)
		}
	})

	dataResults <- reactive({
		infile <- input$showResultsData

		if (is.null(infile)) {
			return(NULL)
		} else {
			return(infile$datapath)
		}
	})

	toListen <- reactive({
		list(input$shapeArea, input$shapeClass)
	})

	observeEvent(toListen(), ignoreNULL = F, {
		shinyjs::toggleState(
			id = "preProcessButton",
			condition = !(is.null(input$shapeArea) || is.null(input$shapeClass))
		)
	})

	observeEvent(input$preProcessButton, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "runBFAST",
			condition = input$preProcessButton > 0
		)
	})

	toListen2 <- reactive({
		list(input$distanceMatrix, input$distanceMatrix2)
	})

	observeEvent(toListen2(), ignoreNULL = F, {
		shinyjs::toggleState(
			id = "GPrun2",
			condition = input$distanceMatrix > 0 || input$distanceMatrix2 > 0
		)
	})

	observeEvent(input$GPdata, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "GPrun",
			condition = !is.null(input$GPdata)
		)
	})

	# habilita/desabilita o botao de download conforme disponibilidade de df
	# se a condicao for satisfeita, eh habilitado
	observeEvent(input$shapeArea, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "showROIMap",
			condition = !is.null(input$shapeArea)
		)
	})

	observeEvent(input$shapeClass, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "showCOIMap",
			condition = !is.null(input$shapeClass)
		)
	})

	observeEvent(input$showResultsData, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "getResults",
			condition = !is.null(input$showResultsData)
		)
	})

	observeEvent(input$dataProcessed, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "distanceMatrix",
			condition = !is.null(input$dataProcessed)
		)
	})

	observeEvent(input$preProcessButton, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "distanceMatrix2",
			condition = input$preProcessButton > 0
		)
	})

	observeEvent(input$preProcessButton, {

		withProgress(message = 'Processing shapes...', value = NULL, {
			areaEuc <- sapply(classShapefile()[[2]]@polygons[[1]]@Polygons, function(x) {
				x@area
			})

			# escolhendo os 150 poligonos com maior area para evitar false positives
			top <- order(areaEuc, decreasing = T)[1:150]
			shpEucTop <- classShapefile()[[2]]
			shpEucTop@polygons[[1]]@Polygons <- shpEucTop@polygons[[1]]@Polygons[top]

			shpOther <- gDifference(areaShapefile()[[2]], classShapefile()[[2]])
		})

		path <- getwd()
		if(file.exists(file.path(path, "GPFolder"))){
			if(file.exists(file.path(path, "GPFolder", "MOD13Q1")) &&
				file.exists(file.path(path, "GPFolder", "MYD13Q1"))) {

				f_band <- "ndvi"
				f_dir <- dir(file.path(path, "GPFolder"))
				f_sat <- which(startsWith(f_dir, "M"))

				if(!("extracted-modis" %in% list.files(file.path(path, "GPFolder")))) {
					for(k in 1:length(f_sat)) {
						# filter only zip files from wd

						setwd(file.path(path, "GPFolder", f_dir[f_sat[k]]))
						files <- dir(pattern = ".zip", full.names = T)

						# remove .tmp files if they exist
						tmp <- dir(pattern = ".tmp", full.names = T)
						tmp <- file.remove(tmp)

						# output folder
						dir.create("../extracted-modis/", showWarnings = F)
						outPath <- paste0("../extracted-modis")

						# progress bar to follow up loop progress
						withProgress(message = (paste("Extracting", f_dir[f_sat[k]],"files...")), value = 0, {
							# loop that extracts files from zips
							for (i in 1:length(files)) {

								# update progress bar
								setProgress(i / length(files), detail = paste0(i, "/", length(files)))

								# list the content of zip
								auxList <- unzip(files[i],
													  list = T,
													  overwrite = F)$Name

								# list .tif files indexes inside the zips
								toUnzip <- grep(".tif", auxList)

								# extracts .tif files from zips
								if(!prod(auxList[toUnzip] %in% dir(outPath))) {
									unzip(files[i],
											files = auxList[toUnzip],
											exdir = outPath)
								}

								# rename unzipped files to match zip name
								zipName <- strsplit(strsplit(files[i], "/")[[1]][2], "\\.")[[1]][1]
								pat <- strsplit(auxList[toUnzip[1]], "\\.")[[1]][1]
								files_pat <- dir(path = outPath, pattern = pat, full.names = T)
								files_new <- gsub(pat, zipName, files_pat)
								file.rename(files_pat, files_new)

							}
						})
						# close(pb)
					}
				}

				withProgress(message = 'Extracting time series...', value = NULL, {
					# list all .tif files inside the extracted folder
					files <- dir(path = file.path(path, "GPFolder", "extracted-modis"),
									 pattern = paste0(f_band, ".tif"))

					# reorder Terra/Aqua files by date
					filesDates <- substr(files, 13, 22)
					files <- files[order(filesDates)]

					# create rasters for all files
					rasters <- lapply(file.path(path, "GPFolder", "extracted-modis", files),
											FUN = raster)

					# defining the stack of all the rasters
					s <- raster::stack(rasters)

					# if s has no time variable, create one
					if(is.null(getZ(s))) {
						s <- setZ(s, as.Date(substr(names(s), 13, 22), format = "%Y_%m_%d"))
					}

					# mask raster from the region with Euc/Other shapes
					ras <- s[[1]]
					suppressWarnings({
						rasEuc <- mask(ras, shpEucTop, updatevalue = -1, updateNA = -1)
						rasOther <- mask(ras, shpOther, updatevalue = -1, updateNA = -1)
					})
					pointsEuc <- which(values(rasEuc) != -1, arr.ind = T)
					pointsOther <- which(values(rasOther) != -1, arr.ind = T)

					# sampling Euc/Other pixels
					set.seed(1)
					smpEuc <<- sample(pointsEuc, 250)
					smpOther <<- sample(pointsOther, 250)
				})

				withProgress(message = 'Filling gaps...', value = NULL, {
					# create and extract time series from all the points (or a sample)
					d <- zooExtract(x = s, c(smpEuc, smpOther))
					d <- as.matrix(d)

					# reduce date to months
					dtime <- strftime(getZ(s), "%Y-%m")

					# aggregate data by month
					l <- length(unique(dtime))
					d2 <- matrix(nrow = l, ncol = ncol(d))
					for(i in 1:l) {
						w <- which(dtime == unique(dtime)[i])
						if(length(w) == 1) d2[i,] <- d[w,]
						else d2[i,] <- colMedians(d[w,], na.rm = T)
					}
					d2[which(is.nan(d2))] <- NA
					rm(d)

					# create the full year-month empty data structure
					mYear <- as.numeric(substr(min(getZ(s)), 1, 4))
					MYear <- as.numeric(substr(max(getZ(s)), 1, 4))
					mMonth <- as.numeric(substr(min(getZ(s)), 6, 7))
					MMonth <- as.numeric(substr(max(getZ(s)), 6, 7))
					fullYear <- rep(mYear:MYear, each = 12)
					fullMonth <- rep(1:12, times = length(mYear:MYear))
					if(mMonth>1) {
						fullYear <- fullYear[-(1:(mMonth-1))]
						fullMonth <- fullMonth[-(1:(mMonth-1))]
					}
					fullYear <- fullYear[-((length(fullYear)-12+MMonth+1):length(fullYear))]
					fullMonth <- fullMonth[-((length(fullMonth)-12+MMonth+1):length(fullMonth))]
					dfull <- data.frame(time = paste0(fullYear,"-",sprintf("%02d", fullMonth)))
					timediff <- setdiff(as.character(dfull$time), unique(dtime))

					# merging the aggregated data with full year-month
					d3 <- rbind(d2, matrix(nrow = length(timediff), ncol = ncol(d2)))
					d3 <- data.frame(time = c(unique(dtime), timediff), d3)
					d3 <- d3[order(d3$time),]
					d3$time <- NULL
					colnames(d3) <- NULL
					rm(d2)

					dts1 <- ts(
						d3,
						start = c(mYear, mMonth),
						end = c(MYear, MMonth),
						frequency = 12
					)
					rm(d3)

					# filling NA gaps with linear interpolation
					dts <- na.approx(dts1, rule = 2)
					dts[which(is.na(dts))] <- 0
					rm(dts1)

					setwd(path)
				})

				withProgress(message = (paste("Running BFAST...")), value = 0, {
					# define number of cores for parallel programming, based on OS
					# detectCores()-1 cores will be used
					# argList <- list()
					# registerDoMC(cores = detectCores()-1)

					# minimum segment size (months) used to calculate h parameter in bfast
					hMon <- 48
					h <- hMon/nrow(dts)

					# run bfast for all other pixels
					# bfastOutput <<- foreach(i = 1:ncol(dts), .packages = c("bfast")) %dopar% {
					out <- list()
					for(i in 1:ncol(dts)){
						# update progress bar
						setProgress(i / ncol(dts), detail = paste0(i, "/", ncol(dts)))

						# only runs bfast for points inside shapefile (outside = 0)
						if(sum(dts[,i]) != 0) {
							resBfast <- bfast(Yt = dts[,i],
													max.iter = 1,
													h = h)

							# select which outputs from bfast to keep
							# out <- list()

							out[[i]] <- list()

							## 1) original time series (Yt)
							out[[i]]$Yt <- resBfast$Yt
							## 2) trend component (Tt)
							out[[i]]$Tt <- resBfast$output[[1]]$Tt
							## 3) seasonal component (St)
							out[[i]]$St <- resBfast$output[[1]]$St
							## 4) trend breakpoints (bp.T)
							if(!resBfast$nobp$Vt) {
								out[[i]]$bp.T <- resBfast$output[[1]]$bp.Vt$breakpoints
							} else {
								out[[i]]$bp.T <- NA
							}
							## 5) seasonal breakpoints (bp.S)
							if(!resBfast$nobp$Wt) {
								out[[i]]$bp.S <- resBfast$output[[1]]$bp.Wt$breakpoints
							} else {
								out[[i]]$bp.S <- NA
							}
							## 6) trend bp magnitudes matrix (mag.T)
							# Columns described below:
							# [,1] -> value before bp
							# [,2] -> value after bp
							# [,3] -> magnitude of the bp
							out[[i]]$mag.T <- resBfast$Mags
							# return(out)
						} else {
							out[[i]] <- list(Yt = rep(NA, nrow(dts)),
											Tt = rep(NA, nrow(dts)),
											St = rep(NA, nrow(dts)),
											bp.T = NULL,
											bp.S = NULL,
											mag.T = NULL)
						}

						# return(out)
					}

					bfastOutput <<- out

					# create rdata dir
					path <- getwd()
					dir.create(file.path(path, "GPFolder"), showWarnings = F)

					# save rdata with environment variables
					save(file = file.path(path, "GPFolder", "featuresBFAST.RData"),
						  list = c("bfastOutput", "smpEuc", "smpOther"),
						  envir = .GlobalEnv)
				})

			} else {
				print("Please, you need to provide both MOD and MYD folders.")
			}

		}

	})

	observe({

		if(input$distanceMatrix2 > 0) {
			load("GPFolder/featuresBFAST.RData")
		} else if(input$distanceMatrix > 0) {
			load(dataBFASTOut())
		}

		if(exists("bfastOutput")) {
			withProgress(message = 'Computing distance matrix...', value = NULL, {
				dRaw <<- distMat(bfastOutput, p = 0)
				d <<- distMat(bfastOutput, p = 1)
				classif <<- c(rep("euc", 250), rep("other", 250)) # eucalipto

				save(file = file.path("GPFolder/eucalipto-guerric-modis-ndvi-1.RData"),
					  list = c("d", "dRaw", "classif"), # eucalipto
					  envir = .GlobalEnv)
			})
		}

	})

	observe({

		if(input$GPrun2 > 0) {
			load("GPFolder/eucalipto-guerric-modis-ndvi-1.RData")
		} else if(input$GPrun > 0) {
			load(dataGPRun())
		}

		if(exists("dRaw")) {
			withProgress(message = 'Running GP...', value = NULL, {


				keep <- 250
				procNum <- 0
				if(!is.factor(classif)) classif <- factor(classif)
				classif <- classif[1:(250 + keep)]

				d <- lapply(d, function(x) {
					as.dist(as.matrix(x)[1:(250 + keep), 1:(250 + keep)])
				})
				dRaw <- lapply(dRaw, function(x) {
					as.dist(as.matrix(x)[1:(250 + keep), 1:(250 + keep)])
				})

				sd <- procNum + 1

				elapsed <- proc.time()

				gpTmp <- gpRun(
					dDec = d,
					dRaw = dRaw,
					classif = classif,
					seed = sd,
					distType = "eq",
					nFolds = 5
				)

				elapsed <- proc.time() - elapsed

				gpRes <- NULL
				attributes(gpTmp) <- list(seed = sd)
				gpRes[[sd]] <- gpTmp

				# dir.create(paste0("results/", f_region, "/", f_sat, "/"), showWarnings = F)
				save(file = file.path("GPFolder", "resultsGP.Rdata"),
					  list = c("gpRes", "classif"),
					  envir = .GlobalEnv)

			})
		}

	})

	output$leafGPClassifier <- renderLeaflet({

		m2 <- leaflet(options = list(attributionControl = F))
		m2 <- addTiles(map = m2,
							urlTemplate = "http://{s}.google.com/vt/lyrs=s&x={x}&y={y}&z={z}",
							attribution = "Imagery &copy;2016 TerraMetrics",
							options = list(minZoom = 1,
												maxZoom = 16,
												noWrap = T,
												subdomains = c('mt0','mt1','mt2','mt3')))

		m2 <- setMaxBounds(m2, -180, -90, 180, 90)

		m2 <- setView(m2, lng = 22.909114, lat = -25.618960, zoom = 2)

		m2
	})

	observe({

		leafletProxy("leafGPClassifier") %>%
			clearShapes()

		if(input$showROIMap){
			shp <- areaShapefile()[[2]]

			extetentShape <- shp %>% extent

			leafletProxy("leafGPClassifier") %>% addPolygons(data = shp, color = "white", opacity=1, fillOpacity=0) %>%
				flyTo(lat = extetentShape[3] + (extetentShape[4]-extetentShape[3])/2,
						lng = extetentShape[1] + (extetentShape[2]-extetentShape[1])/2,
						zoom = 10,
						options = list(animate = FALSE))
		}

		if(input$showCOIMap){
			shp <- classShapefile()[[2]]

			extetentShape <- shp %>% extent

			leafletProxy("leafGPClassifier") %>% addPolygons(data = shp, color = "yellow", opacity = 1, fillOpacity = 0, weight = 2) %>%
				flyTo(lat = extetentShape[3] + (extetentShape[4]-extetentShape[3])/2,
						lng = extetentShape[1] + (extetentShape[2]-extetentShape[1])/2,
						zoom = 10,
						options = list(animate = FALSE))
		}

		if(input$showROIMap == FALSE && input$showCOIMap == FALSE) {
			leafletProxy("leafGPClassifier") %>% setView(lng = 0,
																		lat = 0,
																		zoom = 1)
		}
	})

	observeEvent(input$getResults, {

		load(dataResults())

		out <- list()
		j <- 1

		out[[j]] <- matrix(nrow = length(tsDistances$acronym) + 4, ncol = 1)

		iCol <- 1

		baseAcc <- sapply(gpRes, function(x) {
			sapply(x, function(y) {
				y$baseAcc
			}) %>% rowMeans()
		})
		decAcc <- sapply(gpRes, function(x) {
			sapply(x, function(y) {
				y$decAcc
			}) %>% rowMeans()
		})
		rawAcc <- sapply(gpRes, function(x) {
			sapply(x, function(y) {
				y$rawAcc
			}) %>% rowMeans()
		})
		resMean <- c(rowMeans(decAcc), rowMeans(rawAcc), rowMeans(baseAcc))
		resSd <- c(apply(decAcc, MARGIN = 1, sd), apply(rawAcc, MARGIN = 1, sd), apply(baseAcc, MARGIN = 1, sd))
		# resMean[1:4] <- c(0.959, 0.958, 0.957, 0.958)
		# resSd[1:4] <- c(0.003, 0.004, 0.002, 0.004)
		out[[j]] <- paste(sprintf("%.3f", round(resMean, 3)), "+-", sprintf("%.3f", round(resSd, 3)))
		out[[j]] <- out[[j]] %>% cbind()

		rownames(out[[j]]) <- c(
			"GPdec.train",
			"GPdec.val",
			"GPraw.train",
			"GPraw.val",
			tsDistances$acronym %>% as.character
		)

		colnames(out[[j]]) <- "250"
		dfResults <- data.frame(Approach = rownames(out[[j]]), `250` = out[[1]][,1] %>% as.character())

		output$table <- renderTable(dfResults)

		output$plotTree <- renderPlot({
			gpPlotSynTree(res = gpRes[[input$seedGP %>% as.numeric()]][[input$folderGP %>% as.numeric()]], type = "dec", val = T)
		})



	})


})
