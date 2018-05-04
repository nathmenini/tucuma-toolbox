tabPanel(
	title = "Exploratory",
	icon = icon(name = "search", lib = "font-awesome", class = "fa-lg"),
	style = "margin-top: 71px;",

	br(),
	fluidRow(
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
	),

	hr(),

	fluidRow(
		column(
			width = 6,
			wellPanel(
				h4("Choose the time (t):"),
				uiOutput("outputSlider_1")
			)
		),
		column(
			width = 6,
			wellPanel(
				h4("Choose a pair of coordinates (x,y):"),
				uiOutput("outputSlider_2"),
				uiOutput("outputSlider_3")
			)
		)
	)
)