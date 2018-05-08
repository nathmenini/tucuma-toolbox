tabPanel(
	title = "Raster",
	icon = icon(name = "image", lib = "font-awesome", class = "fa-lg"),

	style = "margin-top: 71px;",

	sidebarPanel(
		# id = "map-toolbar",

		# Set up shinyjs
		useShinyjs(),

		fileInput(inputId = "raster_datafile",
					 label = "Choose shapefile",
					 accept = c(".zip")),
		helpText(
			"The shape must be compressed into a zip with, at least, the .shp, .shx, .dbf, and .prj files. The zip file must have the same name as its contents."
		),
		br(),
		checkboxInput("raster_showMap", "Show shapefile on the map?", FALSE),
		checkboxInput("download_SRTM", "Download SRTM data?", FALSE),
		selectInput(inputId = "raster_versionLS",
						label = "Landsat SR Version",
						choices = list("Collection 1" = "SR_new",
											"Pre-Collection" = "SR_old",
											"TOA" = "TOA")),
		selectInput(inputId = "raster_satellite",
						label = "Landsat Number",
						choices = list(4, 5, 7, 8)),
		textInput(inputId = "raster_periodStart",
					 label = "Period start",
					 value = "2000-01-01"),
		textInput(inputId = "raster_periodEnd",
					 label = "Period end",
					 value = "2000-12-31"),
		bsButton(
			inputId = "raster_botaoDownload",
			label = "Download",
			style = "primary",
			icon = icon("download", lib = "font-awesome"),
			width = "50%"
		),
		textOutput("msg")
	),

	mainPanel(
		leafletOutput(
			outputId = "raster_leaf"
		)
	)
)