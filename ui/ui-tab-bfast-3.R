tabPanel(
	title = "Tutorial",
	icon = icon(name = "question-circle", lib = "font-awesome", class = "fa-lg"),

	fluidPage(
		div(style = "max-width: 70%; margin-left: auto; margin-right: auto; margin-top: 71px;",
			 align = "justify",
			 includeMarkdown("./md/tutorial.md")
		)
	)
)