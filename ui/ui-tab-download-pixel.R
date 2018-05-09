tabPanel(
	title = "Pixel",
	icon = icon(name = "thumb-tack", lib = "font-awesome", class = "fa-lg"),

	style = "margin-top: 71px;",

	sidebarPanel(
		# id = "map-toolbar",

		# Set up shinyjs
		useShinyjs(),

		fileInput(inputId = "pixel_datafile",
					 label = "Choose CSV File",
					 accept = c(".csv")),
		helpText("The input data must be a .csv file, with comma sep. There must be three columns: plot (id), lat (latitude) and long (longitude)."),

		checkboxInput("pixel_showMap", "Show points on the map?", FALSE),

		br(),

		textInput(inputId = "pixel_filename",
					 label = "Downloaded data file name",
					 value = "downloaded-data"),

		HTML("<h5><b>Choose the folder</b></h5>"),

		shinyDirButton(id = "dir_download_pixel",
							label = "Browse...",
							title = "Choose the folder"),
		verbatimTextOutput("dir_download_pixel_text"),

		helpText("Enter the folder that your data will be downloaded."),
		br(),
		selectInput(inputId = "pixel_versionLS",
						label = "Landsat SR Version",
						choices = list("Collection 1" = "new",
											"Pre-Collection" = "old")),
		bsButton(
			inputId = "pixel_botaoDownload",
			label = "Download",
			style = "primary",
			icon = icon("download", lib = "font-awesome"),
			width = "50%"
		)
		# verbatimTextOutput("teste", placeholder = FALSE)
	),

	mainPanel(
		leafletOutput(
			outputId = "pixel_leaf"
		)
	)
)
