tabPanel(
	title = "Input",
	icon = icon(name = "database", lib = "font-awesome", class = "fa-lg"),
	style = "margin-top: 71px;",

	br(),
	column(
		width = 6,
		selectInput("arquivo", "Choose input data:", choices = arq),
		actionButton("botaoLeitura", "Read Data", icon = icon("download", lib="font-awesome")),
		helpText("Press the button 'Read Data' to read the file with extension '.grd' selected above in the drop-down menu.")
	),
	column(
		width = 6,
		h3("File preview:"),
		plotOutput("plot_teste", width = 400, height = 400)
	)
)