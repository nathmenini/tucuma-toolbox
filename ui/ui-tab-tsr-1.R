tabPanel(
	title = "Input",
	icon = icon(name = "database", lib = "font-awesome", class = "fa-lg"),
	style = "margin-top: 71px;",

	sidebarPanel(
		useShinyjs(),
		width = 3,
		fileInput(inputId = "arquivo",
					 label = "Choose input data:",
					 accept = c(".grd", ".gri"),
					 multiple = TRUE),
		bsButton(
			inputId = "botaoLeitura",
			label = "Read Data",
			style = "primary",
			icon = icon("download", lib = "font-awesome"),
			width = "50%"
		),
		# actionButton("botaoLeitura", "Read Data", icon = icon("download", lib="font-awesome")),
		helpText("Press the button 'Read Data' to read the files with extensions '.grd' and '.gri'.")
		# verbatimTextOutput("text_APP3", placeholder = FALSE)
	),

	mainPanel(
		h3("File preview:"),
		plotOutput("plot_teste", width = 400, height = 400)
	)
)