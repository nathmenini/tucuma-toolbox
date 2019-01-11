tabPanel(
	title = "Retrieval",
	icon = icon(name = "sitemap", lib = "font-awesome", class = "fa-lg"),
	style = "margin-top: 71px;",

	sidebarPanel(
		useShinyjs(),
		width = 3,
		HTML("<h4><b>Retrieval:</b></h5>"),
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
		),

		br(),
		hr(),
		br(),

		HTML("<h4><b>Clustering:</b></h5>"),
		actionButton("botaoElbow", "Compute Elbow plot", width = 200, icon = icon("search", lib="font-awesome")),
		selectInput(inputId = "selectCluster",
						label = "Number of clusters",
						choices = list(0,2,3,4,5,6,7,8,9,10,11,12,13,14,15), width = 200),
		actionButton("botaoCluster", "Elbow and Clusters Plots", width = 200, icon = icon("search", lib="font-awesome"))
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
		),
		column(
			width = 6,
			h3("Elbow plot"),
			plotOutput("plot_5", width = 400, height = 400)
		),
		column(
			width = 6,
			h3("Cluster map"),
			plotOutput("plot_6", width = 400, height = 400)
		)
	)
)
