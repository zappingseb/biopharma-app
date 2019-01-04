library(rsconnect)

file.remove("app.R")
file.copy(from="finished_app.R",to="app.R")
rsconnect::setAccountInfo(name="sebastianwolf",
		token=as.character(Sys.getenv("shinyappstoken")),
		secret=Sys.getenv("shinysecret")
		
		)
deployApp(server="shinyapps.io")