tabPanel(
	title = "Results",
	icon = icon(name = "bar-chart", lib = "font-awesome", class = "fa-lg"),

	style = "margin-top: 71px;",

	sidebarPanel(

		# Set up shinyjs
		useShinyjs(),

		fileInput(inputId = "showResultsData",
					 label = "Choose the Results Data",
					 accept = c(".Rdata")),
		bsButton(
			inputId = "getResults",
			label = "Get results",
			style = "primary",
			icon = icon("download", lib = "font-awesome"),
			width = "50%"
		),
		helpText("Press the button 'Get results' to view the results."),

		selectInput(inputId = "seedGP",
						label = "Seed Number",
						choices = list(1, 2, 3, 4, 5)),

		selectInput(inputId = "folderGP",
						label = "Fold Number",
						choices = list(1, 2, 3, 4, 5))
	),

	mainPanel(
		column(
			width = 6,
			h3("Table Results"),
			tableOutput('table')
		),
		column(
			width = 6,
			h3("Syntax Tree"),
			plotOutput("plotTree", width = 400, height = 400)
		)
	)
)
