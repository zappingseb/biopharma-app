library(rsconnect)
file.remove("app.R")
file.copy(from="finished_app.R",to="finisehd_app")
setAccountInfo(name="sebastianwolf",
		token=Sys.getenv("shinyappstoken"),
		secret=Sys.getenv("shinysecret"))
deployApp()