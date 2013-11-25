library(seasonal)
seas(AirPassengers)



echo 'X13_PATH = YOUR_PATH_TO_X13' >> .Renviron

# Don't forget to exchange `YOUR_PATH_TO_X13` with the path to your installation of X-13ARIMA-SEATS.



# http://stackoverflow.com/questions/18669886/how-to-unfold-user-and-environment-variable-in-r-language

# To set an environment variable to make it always available to R you need to
# set that variable in a file called .Renviron which by default is located in
# your {$HOME} directory. So for instance to make the environment variable
# R_WORKSPACE available I add the line


R_WORKSPACE = ~/Documents/R/StackOverflow



?Startup

Sys.setenv(X13_PATH = "~/balbla/X13" )


Sys.getenv("X13_PATH")

Sys.setenv(X13_PATH = "~/X13" )




* checking R code for possible problems ... NOTE
inspect: no visible global function definition for ‘picker’
inspect: no visible global function definition for ‘checkbox’
inspect: no visible global function definition for ‘slider’
inspect: no visible global function definition for ‘manipulate’
inspect: no visible binding for global variable ‘outlier.critical’
inspect: no visible binding for global variable ‘method’
inspect: no visible binding for global variable ‘modelsearch’
inspect: no visible binding for global variable ‘calendar’
inspect: no visible binding for global variable ‘view’
inspect: no visible binding for global variable ‘is.static.call’






