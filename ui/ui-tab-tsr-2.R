tabPanel(
	title = "Exploratory",
	icon = icon(name = "search", lib = "font-awesome", class = "fa-lg"),
	style = "margin-top: 71px;",

	sidebarPanel(
		useShinyjs(),
		HTML("<h5><b>Choose the time (t):</b></h5>"),
		uiOutput("outputSlider_1"),

		hr(),

		HTML("<h5><b>Choose a pair of coordinates (x,y):</b></h5>"),
		uiOutput("outputSlider_2"),
		uiOutput("outputSlider_3")
	),

	mainPanel(
		column(
			width = 6,
			h3("Region of interest:"),
			plotOutput("plot_1", width = 400, height = 400)
		),
		column(
			width = 6,
			h3("Corresponding (x,y) time series:"),
			plotOutput("plot_2", width = 400, height = 400)
		)
	)
)