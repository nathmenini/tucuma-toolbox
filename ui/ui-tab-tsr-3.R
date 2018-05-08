tabPanel(
	title = "Retrieval",
	icon = icon(name = "sitemap", lib = "font-awesome", class = "fa-lg"),
	style = "margin-top: 71px;",

	sidebarPanel(
		useShinyjs(),
		width = 3,
		selectInput("algoritmo", "Choose the distance/similarity measure:",
						c("Manhattan Distance",
						  "Euclidian Distance",
						  "Chebyshev Distance",
						  "DTW Distance",
						  "Cosine Similarity"
						)
		),

		hr(),

		HTML("<h5><b>Choose the time (t):</b></h5>"),
		uiOutput("outputSlider_6"),
		radioButtons("tipo_entrada", "Type of input:",
						 c("Click on the image",
						   "Specify the point (x,y)")
		),

		hr(),

		conditionalPanel(
			condition = "input.tipo_entrada == 'Specify the point (x,y)'",
			HTML("<h5><b>Choose a pair of coordinates (x,y):</b></h5>"),
			uiOutput("outputSlider_4"),
			uiOutput("outputSlider_5")
		),

		actionButton("botaoConsulta", "Retrieve", width = 100, icon = icon("search", lib="font-awesome")),
		conditionalPanel(
			condition = "input.tipo_entrada == 'Click on the image'",
			br(),
			hr(),
			br(),
			verbatimTextOutput("outputPlotClick")
		)
	),

	mainPanel(
		column(
			width = 6,
			conditionalPanel(
				condition = "input.tipo_entrada == 'Click on the image'",
				h3("Input"),
				plotOutput("plot_3", width = 400, height = 400, click = "plotClick_3")
			),
			conditionalPanel(
				condition = "input.tipo_entrada == 'Specify the point (x,y)'",
				h3("Input"),
				plotOutput("plot_3b", width = 400, height = 400)
			)
		),
		column(
			width = 6,
			h3("Output"),
			plotOutput("plot_4a", width = 400, height = 400)
		)
	)
)