library(shiny)
library(ggmap)

counties = map_data("county")
data(state)

chooser = setNames(list("net", "Inflow", "Outflow"), list("Net", "Inflow", "Outflow"))
cat("foo1\n")
# Define UI for miles per gallon application
shinyUI(navbarPage("Migration Flows",
                   tabPanel("Counties",
                            sidebarPanel(
                              selectInput("state", "State:",
                                          setNames(list(unique(counties$region)), "States"),
                                          selected = unique(counties$region)[1]),
                              selectInput("county", "County:", setNames(list(unique(counties$subregion)), "Counties"),
                                          selected = unique(counties$subregion)[1]),
                              # Simple integer interval
                              sliderInput("year", "Year:", 
                                          min=1990, max=2010, value=2010,sep=""),
                              
                              radioButtons("flow", "Choose one:", choiceNames = list("Net Inflow", "Inflow", "Outflow"), 
                                           choiceValues = list("Net Inflow", "Inflow", "Outflow"), selected = "Outflow"),
                              
                              checkboxInput("exclude", "Exclude County of Interest", TRUE)
                            ),
                            mainPanel(h3(textOutput("caption")),
                                      plotOutput("map"))
                            ),
                   tabPanel("States",
                            sidebarPanel(
                              selectInput("state2", "State:",
                                          setNames(list(unique(counties$region)), "States"),
                                          selected = unique(counties$region)[1]),
                              # Simple integer interval
                              sliderInput("year2", "Year:", 
                                          min=1990, max=2010, value=2010,sep=""),
                              
                              radioButtons("flow2", "Choose one:", choiceNames = list("Net Inflow", "Inflow", "Outflow"), 
                                           choiceValues = list("Net Inflow", "Inflow", "Outflow"), selected = "Outflow"),
                              
                              checkboxInput("exclude2", "Exclude State of Interest", TRUE)
                            ),
                            mainPanel(h3(textOutput("caption2")),
                                      plotOutput("map2"))
                   ),
                   tabPanel("Regions",
                            sidebarPanel(
                              selectInput("region", "Region:",
                                          setNames(list(unique(state.division)), "Region"),
                                          selected = unique(state.division)[1]),
                              # Simple integer interval
                              sliderInput("year3", "Year:", 
                                          min=1990, max=2010, value=2010,sep=""),
                              
                              radioButtons("flow3", "Choose one:", choiceNames = list("Net Inflow", "Inflow", "Outflow"), 
                                           choiceValues = list("Net Inflow", "Inflow", "Outflow"), selected = "Outflow"),
                              
                              checkboxInput("exclude3", "Exclude Region of Interest", TRUE)
                            ),
                            mainPanel(h3(textOutput("caption3")),
                                      plotOutput("map3"))
                   ),
                   tabPanel("Help",
                            mainPanel(h4(htmlOutput("caption4")))
                   )
))