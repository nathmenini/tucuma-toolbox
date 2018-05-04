tabPanel(
	title = "About",
	icon = icon(name = "info-circle", lib = "font-awesome", class = "fa-lg"),

	fluidPage(
		div(
			style = "margin-left: auto; margin-right: auto; margin-top: 71px; width: 600px;",
			div(
				align = "center",
				HTML('<h1><strong><em>BFAST Explorer</em></strong> <br> <small class = text-highlight>v0.0.3</small></h1>')
			),
			hr(),
			HTML('<h3 class="text-heading"> <i class="fa fa-users"></i> Authors</h3>'),
			p(),
			div("MSc. Alexandre Esteves Almeida", a(icon("user", lib = "font-awesome"), href = "http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4672382A6", target = "_blank"), a(icon("envelope", lib = "font-awesome"), href = "mailto:almeida.xan@gmail.com")),
			div("Nathalia Menini, Master's Student", a(icon("user", lib = "font-awesome"), href = "http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K8200531Y0", target = "_blank"), a(icon("envelope", lib = "font-awesome"), href = "mailto:nathmenini@gmail.com")),
			div("Prof. Dr. Jan Verbesselt", a(icon("user", lib = "font-awesome"), href = "https://www.wur.nl/en/Persons/Jan-Verbesselt.htm", target = "_blank")),
			div("Prof. Dr. Ricardo da Silva Torres", a(icon("user", lib = "font-awesome"), href = "http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4769295A9", target = "_blank")),
			hr(),
			HTML('<h3 class="text-heading"> <i class="fa fa-university"></i> Development</h3>'),
			p(),
			div(
				align = "center",
				div(class = "image-highlight", a(img(src = "logo-tribes.png", height = "80px"), href = "http://www.e-tribes.com.br", target = "_blank")),
				p(),
				div(class = "image-highlight", a(img(src = "logo-unicamp.svg", height = "80px"), href = "http://www.unicamp.br/unicamp/english/", target = "_blank")),
				div(class = "image-highlight", a(img(src = "logo-wur.png", height = "120px"), href = "http://www.wur.nl/en.htm", target = "_blank"))
			),
			p(),
			HTML('This work is a product of the Tribes project, developed within the University of Campinas (UNICAMP) <a href="http://www.ic.unicamp.br/en" target="_blank">Institute of Computing</a> and the Wageningen University &amp; Research (WUR) <a href="http://www.wur.nl/en/Expertise-Services/Chair-groups/Environmental-Sciences/Laboratory-of-Geo-information-Science-and-Remote-Sensing.htm" target="_blank">Laboratory of Geo-information Science and Remote Sensing</a>.'),
			hr(),
			HTML('<h3 class="text-heading"> <i class="fa fa-handshake-o"></i> Supporters</h3>'),
			p(),
			div(align = "center",
				 div(class = "image-highlight", a(img(src = "logo-fapesp.svg", height = "40px"), href = "http://www.fapesp.br/en/", target = "_blank")),
				 div(style = "display:inline;", HTML("&nbsp&nbsp")),
				 div(class = "image-highlight", a(img(src = "logo-ms-research.png", height = "40px"), href = "http://www.fapesp.br/en/5392", target = "_blank")),
				 div(style = "display:inline;", HTML("&nbsp&nbsp")),
				 div(class = "image-highlight", a(img(src = "logo-cnpq.svg", height = "60px"), href = "http://cnpq.br/", target = "_blank"))
			)
		),
		HTML('
					<span class="fa-stack hidden-n" style="position: absolute; left: 1px;">
						<i class="fa fa-heart fa-stack-2x text-danger"></i>
						<strong class="fa-stack-1x fa-inverse" style="margin-top: -.05em;">N</strong>
					</span>
			 	')
	)
)