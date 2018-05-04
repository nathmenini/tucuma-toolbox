tabPanel(
	title = "Map",
	icon = icon(name = "map-marker", lib = "font-awesome", class = "fa-lg"),

	# set up shinyjs
	useShinyjs(),
	div(
		id = "map-leaflet",
		# render a leaflet map on the background
		leafletOutput(
			outputId = "leaf",
			height = "100%"
		),

		# create a toolbar on top of the map
		fixedPanel(
			id = "map-toolbar",
			class = "panel panel-default",
			top = 60,
			bottom = "auto",
			right = 250,
			left = 250,
			# map search query
			fluidRow(
				column(
					4,
					HTML('
						  <div class="form-group shiny-input-container" style="width: 100%;">
						  <div class="input-group">
						  <label class="input-group-btn">
						  <button id="action_search" style="width: 100%;" type="button" class="btn action-button btn-primary">
						  <i class="fa fa-search"></i>
						  </button>
						  </label>
						  <input id="select_search" type="text" class="form-control" value="" placeholder="Search for a location..."/>
						  </div>
						  </div>
						  '),
					# enable "Enter" key press on textInput to activate the search button
					tags$script('
									$(document).on("keyup", function (e) {
									if (e.keyCode == 13) {
									$("#action_search").click();
									}
									});
									')
					),
				# 'insert shape' button
				column(
					4,
					div(
						HTML('
							  <div class="form-group shiny-input-container" style="width: 100%;">
							  <div class="input-group">
							  <label id="help_insertShape" class="input-group-btn">
							  <span class="btn btn-primary btn-file">
							  <i class="fa fa-upload"></i>
							  <input id="file_insertShape" name="file_insertShape" type="file" style="display: none;" accept="application/octet-stream,application/zip,.zip"/>
							  </span>
							  </label>
							  <input type="text" class="form-control" placeholder="Upload a shapefile..." readonly="readonly"/>
							  </div>
							  <div id="file_insertShape_progress" class="progress progress-striped active shiny-file-input-progress">
							  <div class="progress-bar"></div>
							  </div>
							  </div>
							  '),
						bsTooltip(
							id = "help_insertShape",
							title = '<div align="justify"><p>The shape must be compressed into a zip with, at least, the <strong>.shp</strong>, <strong>.shx</strong>, <strong>.dbf</strong>, and <strong>.prj</strong> files.</p> The zip file <strong>must</strong> have the same name as its contents.</div>',
							placement = "bottom",
							trigger = "hover",
							options = list(container = "body")
						)
						)
				),
				# 'clear all markers' button
				column(
					4,
					bsButton(
						inputId = "action_clearMarkers",
						label = "Clear Markers",
						style = "danger",
						icon = icon(name = "trash", lib = "font-awesome"),
						width = "100%"
					)
				)
					),
			fluidRow(
				# select which satellite products to download
				column(
					6,
					selectInput(
						inputId = "select_satGet",
						label = NULL,
						width = "100%",
						choices = c(""),
						multiple = T
					)
				),
				# 'get data' button
				column(
					6,
					bsButton(
						inputId = "action_getTs",
						label = "Get Data",
						style = "success",
						icon = icon(name = "download", lib = "font-awesome"),
						width = "100%"
					),
					# tooltip with data preprocessing description
					bsTooltip(
						id = "action_getTs",
						title = '<div align="justify"><p>The Landsat "Surface Refle- ctance" (SR) product is, by construction, already atmosphe- rically corrected.</p> In addition, the downloaded data is also filtered for cloud effects by using the CFMask algorithm.</div>',
						placement = "bottom",
						trigger = "hover",
						options = list(container = "body")
					)
				)
			),
			# output text with download results
			htmlOutput(outputId = "text_getTs")
				)
	)
)
