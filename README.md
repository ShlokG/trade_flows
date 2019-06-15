# trade_flows
Code for the app displaying migration flows within the United States from 1990 to 2010.

Details are in the Help section of the app.

To run the app, in RStudio, run the following line:

    runGitHub( "trade_flows", "ShlokG")

Note that you must have loaded the "shiny" library and installed certain packages before running the above line. You can run the below code to do this:

    install.packages('shiny')
    library(shiny)
    install.packages('ggmap')
    install.packages('readxl')
    install.packages('ggplot2')
    install.packages('maps')
    install.packages('dplyr')
    install.packages('stringr')
    install.packages('sp')
    install.packages('maptools')
