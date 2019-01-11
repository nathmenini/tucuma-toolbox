shinyUI(navbarPage(
	title = div(em(strong("Tucumã Toolbox"))),
	windowTitle = "Tucumã Toolbox",

	id = "navbar",
	position = "fixed-top",
	collapsible = T,
	header = {
		# load styles.css file with custom styles
		tags$head(includeCSS("www/styles.css"))
	},

	navbarMenu("BFAST Explorer",
				  source(file.path("ui/ui-tab-bfast-1.R"), local = TRUE)$value,
				  source(file.path("ui/ui-tab-bfast-2.R"), local = TRUE)$value,
				  source(file.path("ui/ui-tab-bfast-3.R"), local = TRUE)$value
	),

	navbarMenu("DAM",
				  source(file.path("ui/ui-tab-download-pixel.R"), local = TRUE)$value,
				  source(file.path("ui/ui-tab-download-raster.R"), local = TRUE)$value
	),

	navbarMenu("TSR",
				  source(file.path("ui/ui-tab-tsr-1.R"), local = TRUE)$value,
				  source(file.path("ui/ui-tab-tsr-2.R"), local = TRUE)$value,
				  source(file.path("ui/ui-tab-tsr-3.R"), local = TRUE)$value

	),

	navbarMenu("GPC",
				  source(file.path("ui/ui-tab-GP-1.R"), local = TRUE)$value,
				  source(file.path("ui/ui-tab-GP-2.R"), local = TRUE)$value
	),

	source(file.path("ui/ui-tab-bfast-4.R"), local = TRUE)$value
))
