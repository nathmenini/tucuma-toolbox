tabPanel(
	title = "Processing",
	icon = icon(name = "spinner", lib = "font-awesome", class = "fa-lg"),

	style = "margin-top: 71px;",

	sidebarPanel(

		# Set up shinyjs
		useShinyjs(),

		fileInput(inputId = "shapeArea",
					 label = "Choose shapefile of ROI",
					 accept = c(".zip")),

		checkboxInput("showROIMap", "Show ROI on the map?", FALSE),

		fileInput(inputId = "shapeClass",
					 label = "Choose shapefile of COI",
					 accept = c(".zip")),

		checkboxInput("showCOIMap", "Show COI on the map?", FALSE),

		helpText(
			"The shapes must be compressed into a zip with, at least, the .shp, .shx, .dbf, and .prj files. The zip file must have the same name as its contents."
		),

		hr(),

		bsButton(
			inputId = "preProcessButton",
			label = "Pre-processing Data",
			style = "primary",
			icon = icon("download", lib = "font-awesome"),
			width = "50%"
		),
		helpText("Press the button 'Pre-processing Data' to pre-process the images."),

		hr(),

		radioButtons("entrada_bfastOut", "Do you already have the OutputData?",
						 c("Yes",
						   "No")
		),

		conditionalPanel(
			condition = "input.entrada_bfastOut == 'Yes'",
			fileInput(inputId = "dataProcessed",
						 label = "Choose the processed data",
						 accept = c(".Rdata")),
			bsButton(
				inputId = "distanceMatrix",
				label = "Distance Matrix",
				style = "primary",
				icon = icon("download", lib = "font-awesome"),
				width = "50%"
			),
			helpText("Press the button 'Distance Matrix' to calculate the distance matrix.")
		),
		conditionalPanel(
			condition = "input.entrada_bfastOut == 'No'",
			bsButton(
				inputId = "distanceMatrix2",
				label = "Distance Matrix",
				style = "primary",
				icon = icon("download", lib = "font-awesome"),
				width = "50%"
			),
			helpText("Press the button 'Distance Matrix' to calculate the distance matrix.")
		),

		hr(),

		radioButtons("entrada_GPRun", "Do you already have the Distance Matrix?",
						 c("Yes",
						   "No")
		),

		conditionalPanel(
			condition = "input.entrada_GPRun == 'Yes'",
			fileInput(inputId = "GPdata",
						 label = "Choose the distance matrix data",
						 accept = c(".Rdata")),
			bsButton(
				inputId = "GPrun",
				label = "Run GP",
				style = "primary",
				icon = icon("download", lib = "font-awesome"),
				width = "50%"
			),
			helpText("Press the button 'Run GP' to run the GP algorithm.")
		),
		conditionalPanel(
			condition = "input.entrada_GPRun == 'No'",
			bsButton(
				inputId = "GPrun2",
				label = "Run GP",
				style = "primary",
				icon = icon("download", lib = "font-awesome"),
				width = "50%"
			),
			helpText("Press the button 'Run GP' to run the GP algorithm.")
		)
	),

	mainPanel(
		div(
			tags$style(type = "text/css", "#leafGPClassifier {height: calc(100vh - 80px) !important;}"),

			leafletOutput(
				outputId = "leafGPClassifier"
			)
		)
	)
)
