tabPanel(
	title = "Analysis",
	icon = icon(name = "bar-chart", lib = "font-awesome", class = "fa-lg"),

	style = "margin-top: 71px;",

	fluidPage(
		# ----------------------------------------- TIME SERIES ---
		fixedRow(
			column(
				3,
				h3(class = "text-heading", "Visualization"),
				# select which satellite products to plot
				selectInput(
					inputId = "select_satPlot",
					label = "Satellite product",
					width = "100%",
					choices = NULL,
					multiple = T
				),
				# select which band/index to plot
				selectInput(
					inputId = "select_index",
					label = "Data",
					width = "100%",
					choices = NULL
				),
				# add a tooltip to select_index warning about mixing bands from different satellites
				bsTooltip(
					id = "select_index",
					title = '<div align="justify">Be careful when comparing <strong>bands</strong> from different satellites.<p></p><hr><em>For more information about Landsat bands, please visit <a href="https://landsat.usgs.gov/what-are-band-designations-landsat-satellites" target="_blank">this link</a>.</em></div>',
					placement = "right",
					trigger = "focus",
					options = list(container = "body")
				),
				# select which change detection algorithm to run
				selectInput(
					inputId = "select_chgDet",
					label = "Change detection algorithm",
					width = "100%",
					choices = c("Select the change detection algorithm..." = "", "bfastmonitor", "bfast01", "bfast")
				),
				# tooltip with brief explanations about the change detection algorithms
				bsTooltip(
					id = "select_chgDet",
					title = '<div align="justify"><strong><div class="text-highlight">bfastmonitor</div></strong>Monitoring for the first break at the end of the time series.<p></p><strong><div class="text-highlight">bfast01</div></strong>Checking for one major break in the time series.<p></p><strong><div class="text-highlight">bfast</div></strong>Time series decomposition and multiple breakpoint detection in trend and seasonal components.<p></p><hr><em>For more information about these algorithms, please visit <a href="http://bfast.r-forge.r-project.org/" target="_blank">this link</a>.</em></div>',
					placement = "right",
					trigger = "focus",
					options = list(container = "body")
				),
				# buttons to download the data and the plot figures
				h5(style = "cursor:default;", strong("Download")),
				downloadButton(
					outputId = "action_downloadDataRaw",
					label = "Data",
					class = "btn-primary"
				),
				bsButton(
					inputId = "action_downloadPlotRaw",
					label = "Plot",
					style = "primary",
					icon = icon(name = "download", lib = "font-awesome")
				),
				# checkbox to show/hide latlong coordinates
				checkboxInput(
					inputId = "select_showCoords",
					label = "Show coordinates?",
					value = T,
					width = "100%"
				)
			),
			# time series plot and its legend
			column(
				9,
				splitLayout(
					cellWidths = c("75%", "25%"),
					plotOutput(outputId = "plot_raw", height = 300),
					plotOutput(outputId = "plot_raw_legend", height = 300)
				)
			)
		),

		# ---------------------------------------- BFASTMONITOR ---

		conditionalPanel(
			condition = "input.select_chgDet == 'bfastmonitor' && input.select_satPlot != null",
			hr(),
			fluidRow(
				column(
					3,
					h3(class = "text-heading", "bfastMonitor"),
					helpText("Based on", a("bfmApp", href = "https://github.com/loicdtx/bfmApp", target = "_blank"), "by Loïc Dutrieux"),
					fluidRow(
						# formula parameter
						column(
							6,
							selectInput(
								inputId = "select_bfm_formula",
								label = "Formula",
								width = "100%",
								choices = c("trend + harmon", "trend", "harmon"),
								selected = "trend + harmon"
							)
						),
						# history period type parameter
						column(
							6,
							selectInput(
								inputId = "select_bfm_history",
								label = "History period type",
								width = "100%",
								choices = c("ROC", "BP", "all"),
								selected = "ROC"
							),
							# tooltip with brief explanations about the history period type parameter
							bsTooltip(
								id = "select_bfm_history",
								title = '<div align="justify"><strong><div class="text-highlight">ROC</div></strong>Reverse-ordered CUSUM.<p></p><strong><div class="text-highlight">BP</div></strong>Bai & Perron<br> breakpoint estimation.<p></p><strong><div class="text-highlight">all</div></strong>All available observations.</div>',
								placement = "right",
								trigger = "focus",
								options = list(container = "body")
							)
						)
					),
					fluidRow(
						# harmonic order parameter (only show if applicable)
						column(
							6,
							conditionalPanel(
								condition = "input.select_bfm_formula == 'harmon' | input.select_bfm_formula == 'trend + harmon'",
								sliderInput(
									inputId = "select_bfm_order",
									label = "Harmonic order",
									width = "100%",
									value = 1,
									min = 1,
									max = 9,
									step = 1
								)
							)
						),
						# start of monitoring period parameter
						column(
							6,
							dateInput(
								inputId = "select_bfm_monitor",
								label = "Start of monitoring",
								width = "100%",
								format = "yyyy-mm",
								startview = "decade"
							),
							# tooltip with brief explanations about the monitoring period default
							bsTooltip(
								id = "select_bfm_monitor",
								title = '<div align="justify">Defaults to the last<br> 30% of the data.</div>',
								placement = "bottom",
								options = list(container = "body")
							)
						)
					),
					# buttons to download the data and the plot figures
					h5(style = "cursor:default;", strong("Download")),
					downloadButton(
						outputId = "action_downloadDataBfm",
						label = "Results",
						class = "btn-primary"
					),
					bsButton(
						inputId = "action_downloadPlotBfm",
						label = "Plot",
						style = "primary",
						icon = icon(name = "download", lib = "font-awesome")
					)
				),
				# bfastmonitor plot and its legend
				column(
					9,
					splitLayout(
						cellWidths = c("75%", "25%"),
						plotOutput(outputId = "plot_bfm", height = 300),
						plotOutput(outputId = "plot_bfm_legend", height = 300)
					)
				)
			)
		),

		# --------------------------------------------- BFAST01 ---

		conditionalPanel(
			condition = "input.select_chgDet == 'bfast01' && input.select_satPlot != null",
			hr(),
			fluidRow(
				column(
					3,
					h3(class = "text-heading", "bfast01"),
					fluidRow(
						# formula parameter
						column(
							6,
							selectInput(
								inputId = "select_bf01_formula",
								label = "Formula",
								width = "100%",
								choices = c("trend + harmon", "trend", "harmon"),
								selected = "trend + harmon"
							)
						),
						# harmonic order parameter (only show if applicable)
						column(
							6,
							conditionalPanel(
								condition = "input.select_bf01_formula == 'harmon' | input.select_bf01_formula == 'trend + harmon'",
								sliderInput(
									inputId = "select_bf01_order",
									label = "Harmonic order",
									width = "100%",
									value = 1,
									min = 1,
									max = 1,
									step = 1
								)
							)
						)
					),
					# buttons to download the data and the plot figures
					h5(style = "cursor:default;", strong("Download")),
					downloadButton(
						outputId = "action_downloadDataBf01",
						label = "Results",
						class = "btn-primary"
					),
					bsButton(
						inputId = "action_downloadPlotBf01",
						label = "Plot",
						style = "primary",
						icon = icon(name = "download", lib = "font-awesome")
					)
				),
				# bfast01 plot and its legend
				column(
					9,
					splitLayout(
						cellWidths = c("75%", "25%"),
						plotOutput(outputId = "plot_bf01", height = 300),
						plotOutput(outputId = "plot_bf01_legend", height = 300)
					)
				)
			)
		),

		# ----------------------------------------------- BFAST ---

		conditionalPanel(
			condition = "input.select_chgDet == 'bfast' && input.select_satPlot != null",
			hr(),
			fluidRow(
				column(
					3,
					h3(class = "text-heading", "bfast"),
					fluidRow(
						# h parameter (minimal segment size)
						column(
							6,
							sliderInput(
								inputId = "select_bfast_h",
								label = "h",
								width = "100%",
								value = 0.15,
								step = 0.01,
								min = 0.00,
								max = 0.50
							),
							# explanation about what the h parameter means
							bsTooltip(
								id = "select_bfast_h",
								title = '<div align="justify">Minimal segment size between potentially detected breaks.</div>',
								placement = "right",
								options = list(container = "body")
							)
						),
						# season type parameter
						column(
							6,
							selectInput(
								inputId = "select_bfast_season",
								label = "Season type",
								width = "100%",
								choices = c("dummy", "harmonic", "none"),
								selected = "dummy"
							)
						)
					),
					# switch to smooth the time series data
					checkboxInput(
						inputId = "smooth_ts",
						label = "Smooth time series?",
						value = F,
						width = "100%"
					),
					dateInput(
						inputId = "select_start_period_bfast",
						label = "Start period",
						width = "100%",
						min = "1980-01-01",
						format = "yyyy-mm",
						startview = "decade"
					),
					dateInput(
						inputId = "select_end_period_bfast",
						label = "End period",
						width = "100%",
						format = "yyyy-mm",
						startview = "decade"
					),
					# buttons to download the data and the plot figures
					h5(style = "cursor:default;", strong("Download")),
					downloadButton(outputId = "action_downloadDataBfast",
										label = "Results",
										class = "btn-primary"),
					bsButton(
						inputId = "action_downloadPlotBfast",
						label = "Plot",
						style = "primary",
						icon = icon(name = "download", lib = "font-awesome")
					)
				),
				# bfast plot and its legend
				column(
					9,
					splitLayout(
						cellWidths = c("75%", "25%"),
						plotOutput(outputId = "plot_bfast", height = 300),
						plotOutput(outputId = "plot_bfast_legend", height = 300)
					)
				)
			)
		)
	)
)