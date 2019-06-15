library(shiny)
library(datasets)
library(readxl)
library(ggmap)
library(ggplot2)
library(dplyr)
library(stringr)
library(maps)
library(sp)
library(maptools)

# Source for latlong2county: https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

# Test the function using points in Wisconsin and Oregon.
#testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))
#locs = latlong2county(data.frame(e$x, e$y))
#locs2 = data.frame(state = substr(locs,1,regexpr(",", locs)-1),
#                   county = substr(locs,regexpr(",",locs)+1,nchar(locs)))
#paste(locs2$county, locs2$state, sep = ", ")
#paste0("\n", values$df_data[which(region == locs2$state & subregion == locs2$county)[1],
#              which(colnames(values$df_data) == paste0("X", as.character(input$year)))],collapse="")


memory.limit(30000)

df = read.csv("demographic-research.40-40/county_migration_data.csv")
crosswalk = read.csv("demographic-research.40-40/ssa_fips_state_county2017.csv")
crosswalk$county = tolower(crosswalk$county)

state_wlk = read.csv("demographic-research.40-40/states.csv")

df2 = merge(df, crosswalk, by.x = "origin", by.y = "fipscounty", all.x = TRUE)
df2$ssacounty = NULL
df2$cbsa = NULL
df2$cbsaname = NULL
df2$ssastate = NULL
colnames(df2)[which(colnames(df2)=="county")] = "origin_cty"
colnames(df2)[which(colnames(df2)=="state")] = "origin_state"
df3 = merge(df2, crosswalk, by.x = "destination", by.y = "fipscounty", all.x = TRUE)

# Removing variables for memory
rm(df2)
rm(df)
rm(crosswalk)


df3$ssacounty = NULL
df3$cbsa = NULL
df3$cbsaname = NULL
df3$ssastate = NULL
colnames(df3)[which(colnames(df3)=="county")] = "dest_cty"
colnames(df3)[which(colnames(df3)=="state")] = "dest_state"

df3$orig_state = tolower(state_wlk$State[match(df3$origin_state, state_wlk$Abbreviation)])
df3$destin_state = tolower(state_wlk$State[match(df3$dest_state, state_wlk$Abbreviation)])

counties <- map_data("county")
states <- map_data("state")

df32 = df3
colnames(df32)[which(colnames(df32) == "origin_cty")] = "subregion"
colnames(df32)[which(colnames(df32) == "orig_state")] = "region"

df_dest2 = inner_join(df32, counties, by = c("subregion" = "subregion", "region" = "region"))

rm(df32)

df_origs = df3 
colnames(df_origs)[which(colnames(df_origs) == "dest_cty")] = "subregion"
colnames(df_origs)[which(colnames(df_origs) == "destin_state")] = "region"


df_origs2 = inner_join(df_origs, counties, by = c("subregion" = "subregion", "region" = "region"))

rm(df_origs)

df_dest2$cty_int = df_dest2$dest_cty
df_origs2$cty_int = df_origs2$origin_cty
df_dest2$state_int = df_dest2$destin_state
df_origs2$state_int = df_origs2$orig_state


# Net Inflows
outs = df3[match(paste(df3$destination, df3$origin), paste(df3$origin, df3$destination)),]
outs[is.na(outs)] = 0
df_outs = df3
df_outs[,3:23] = 10^(log10(df_outs[,3:23]) - log10(outs[,3:23]))

colnames(df_outs)[which(colnames(df_outs) == "origin_cty")] = "subregion"
colnames(df_outs)[which(colnames(df_outs) == "orig_state")] = "region"

df_outs2 = inner_join(df_outs, counties, by = c("subregion" = "subregion", "region" = "region"))

rm(df_outs)
rm(outs)

df_outs2$cty_int = df_outs2$dest_cty
df_outs2$state_int = df_outs2$destin_state

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

df_dest2 = df_dest2[!duplicated(df_dest2[c("region","state_int","subregion","cty_int","long","lat","group","order")]),]
df_origs2 = df_origs2[!duplicated(df_origs2[c("region","state_int","subregion","cty_int","long","lat","group","order")]),]
df_outs2 = df_outs2[!duplicated(df_outs2[c("region","state_int","subregion","cty_int","long","lat","group","order")]),]

##########
# States #
##########
df_origs_state = df3 %>%
  group_by(orig_state, destin_state) %>%
  summarize(X1990 = sum(X1990), X1991 = sum(X1991),
            X1992 = sum(X1992), X1993 = sum(X1993),
            X1994 = sum(X1994), X1995 = sum(X1995),
            X1996 = sum(X1996), X1997 = sum(X1997),
            X1998 = sum(X1998), X1999 = sum(X1999),
            X2000 = sum(X2000), X2001 = sum(X2001),
            X2002 = sum(X2002), X2003 = sum(X2003),
            X2004 = sum(X2004), X2005 = sum(X2005),
            X2006 = sum(X2006), X2007 = sum(X2007),
            X2008 = sum(X2008), X2009 = sum(X2009),
            X2010 = sum(X2010))

df_origs_state = as.data.frame(df_origs_state)
df_os = df_origs_state
colnames(df_os)[which(colnames(df_os) == "orig_state")] = "region"

df_dest_os = inner_join(df_os, states, by = "region")
df_dest_os$state_int = df_dest_os$destin_state

rm(df_os)

df_os_2 = df_origs_state
colnames(df_os_2)[which(colnames(df_os_2) == "destin_state")] = "region"

df_origs_os = inner_join(df_os_2, states, by = "region")
df_origs_os$state_int = df_origs_os$orig_state

rm(df_os_2)

# Net Inflows
outs = df_origs_state[match(paste(df_origs_state$orig_state, df_origs_state$destin_state), 
                            paste(df_origs_state$destin_state, df_origs_state$orig_state)),]

df_outs_st = df_origs_state
df_outs_st[,3:23] = 10^(-log10(df_outs_st[,3:23]) + log10(outs[,3:23]))

colnames(df_outs_st)[which(colnames(df_outs_st) == "destin_state")] = "region"

df_outs_st2 = inner_join(df_outs_st, states, by = "region")

df_outs_st2$state_int = df_outs_st2$orig_state


rm(df_origs_state)
rm(df_outs_st)

df_dest_os = df_dest_os[!duplicated(df_dest_os[c("region","state_int","long","lat","group","order")]),]
df_origs_os = df_origs_os[!duplicated(df_origs_os[c("region","state_int","long","lat","group","order")]),]
df_outs_st2 = df_outs_st2[!duplicated(df_outs_st2[c("region","state_int","long","lat","group","order")]),]

##########
# Region #
##########
df3$orig_region = state.division[match(df3$origin_state, state.abb)]
df3$destin_region = state.division[match(df3$dest_state, state.abb)]

df_origs_reg = df3 %>%
  group_by(orig_state, destin_state) %>%
  summarize(X1990 = sum(X1990), X1991 = sum(X1991),
            X1992 = sum(X1992), X1993 = sum(X1993),
            X1994 = sum(X1994), X1995 = sum(X1995),
            X1996 = sum(X1996), X1997 = sum(X1997),
            X1998 = sum(X1998), X1999 = sum(X1999),
            X2000 = sum(X2000), X2001 = sum(X2001),
            X2002 = sum(X2002), X2003 = sum(X2003),
            X2004 = sum(X2004), X2005 = sum(X2005),
            X2006 = sum(X2006), X2007 = sum(X2007),
            X2008 = sum(X2008), X2009 = sum(X2009),
            X2010 = sum(X2010), 
            orig_region = orig_region[1], destin_region = destin_region[1])

df_origs_match = df3 %>%
  group_by(orig_region, destin_region) %>%
  summarize(X1990 = sum(X1990, na.rm=T), X1991 = sum(X1991, na.rm=T),
            X1992 = sum(X1992, na.rm=T), X1993 = sum(X1993, na.rm=T),
            X1994 = sum(X1994, na.rm=T), X1995 = sum(X1995, na.rm=T),
            X1996 = sum(X1996, na.rm=T), X1997 = sum(X1997, na.rm=T),
            X1998 = sum(X1998, na.rm=T), X1999 = sum(X1999, na.rm=T),
            X2000 = sum(X2000, na.rm=T), X2001 = sum(X2001, na.rm=T),
            X2002 = sum(X2002, na.rm=T), X2003 = sum(X2003, na.rm=T),
            X2004 = sum(X2004, na.rm=T), X2005 = sum(X2005, na.rm=T),
            X2006 = sum(X2006, na.rm=T), X2007 = sum(X2007, na.rm=T),
            X2008 = sum(X2008, na.rm=T), X2009 = sum(X2009, na.rm=T),
            X2010 = sum(X2010, na.rm=T))

# Removing more
rm(df3)


df_origs_reg = as.data.frame(df_origs_reg)
df_origs_match = as.data.frame(df_origs_match)

df_origs_reg[,3:23] = df_origs_match[match(paste(df_origs_reg$orig_region,
                                                 df_origs_reg$destin_region), 
                                           paste(df_origs_match$orig_region, 
                                                 df_origs_match$destin_region)),3:23]


rm(df_origs_match)


df_or = df_origs_reg
colnames(df_or)[which(colnames(df_or) == "orig_state")] = "region"

df_dest_or = full_join(df_or, states, by = "region")
rm(df_or)

df_dest_or$state_int = df_dest_or$destin_region
df_dest_or$state_not = df_dest_or$orig_region


df_or_2 = df_origs_reg
colnames(df_or_2)[which(colnames(df_or_2) == "destin_state")] = "region"

df_origs_or = inner_join(df_or_2, states, by = "region")

rm(df_or_2)

df_origs_or$state_int = df_origs_or$orig_region
df_origs_or$state_not = df_origs_or$destin_region

# Net Inflows
outr = df_origs_reg[match(paste(df_origs_reg$orig_state, df_origs_reg$destin_state), 
                            paste(df_origs_reg$destin_state, df_origs_reg$orig_state)),]

df_outr_st = df_origs_reg
df_outr_st[,3:23] = 10^(-log10(df_outr_st[,3:23]) + log10(outr[,3:23]))

rm(outr)
rm(df_origs_reg)

colnames(df_outr_st)[which(colnames(df_outr_st) == "destin_state")] = "region"

df_outr_st2 = inner_join(df_outr_st, states, by = "region")

rm(df_outr_st)

df_outr_st2$state_int = df_outr_st2$orig_region
df_outr_st2$state_not = df_outr_st2$destin_region
df_outr_st2$subregion = NULL



df_dest_or = df_dest_or[!duplicated(df_dest_or[c("region","state_int","state_not","long","lat","group","order")]),]
df_origs_or = df_origs_or[!duplicated(df_origs_or[c("region","state_int","state_not","long","lat","group","order")]),]
df_outr_st2 = df_outr_st2[!duplicated(df_outr_st2[c("region","state_int","state_not","long","lat","group","order")]),]


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  
  values <- reactiveValues(df_data = subset(df_origs2, cty_int == "autauga" & state_int == "alabama"),
                           flowing = "Outflow", exclusion = TRUE)
  
  # State
  values2 <- reactiveValues(df_data = subset(df_origs_os, state_int == "alabama"),
                           flowing = "Outflow", exclusion = TRUE)
  
  # Region
  values3 <- reactiveValues(df_data = subset(df_origs_or, state_int == "East South Central"),
                            flowing = "Outflow", exclusion = TRUE)
  
  observeEvent(input$county, {
    cat(input$county)
    cat(input$state)
    if(values$flowing == "Inflow"){values$df_data <- subset(df_dest2, cty_int == input$county & state_int == input$state)}
    else if(values$flowing == "Outflow"){values$df_data <- subset(df_origs2, cty_int == input$county & state_int == input$state)}
    else{values$df_data <- subset(df_outs2, cty_int == input$county & state_int == input$state)}
    
    if(values$exclusion){values$df_data <- subset(values$df_data, subregion != input$county | region != input$state)}
  })
  
  # State
  observeEvent(input$state2, {
    cat("Bye")
    if(values2$flowing == "Inflow"){values2$df_data <- subset(df_dest_os, state_int == input$state2)}
    else if(values2$flowing == "Outflow"){values2$df_data <- subset(df_origs_os, state_int == input$state2)}
    else{values2$df_data <- subset(df_outs_st2, state_int == input$state2)}
    
    if(values2$exclusion){values2$df_data <- subset(values2$df_data, region != input$state2)}
    
  })
  
  # Region
  observeEvent(input$region, {
    cat("Hello")
    if(values3$flowing == "Inflow"){values3$df_data <- subset(df_dest_or, state_int == input$region)}
    else if(values3$flowing == "Outflow"){values3$df_data <- subset(df_origs_or, state_int == input$region)}
    else{values3$df_data <- subset(df_outr_st2, state_int == input$region)}
    
    if(values3$exclusion){values3$df_data <- subset(values3$df_data, state_not != input$region)}
    
  })
  
  
  observeEvent(input$flow, {
    values$flowing <- input$flow
    
    if(values$flowing == "Inflow"){values$df_data <- subset(df_dest2, cty_int == input$county & state_int == input$state)}
    else if(values$flowing == "Outflow"){values$df_data <- subset(df_origs2, cty_int == input$county & state_int == input$state)}
    else{values$df_data <- subset(df_outs2, cty_int == input$county & state_int == input$state)}
    
    if(values$exclusion){values$df_data <- subset(values$df_data, subregion != input$county | region != input$state)}
  })
  
  # State
  observeEvent(input$flow2, {
    cat("Or")
    values2$flowing <- input$flow2
    
    if(values2$flowing == "Inflow"){values2$df_data <- subset(df_dest_os, state_int == input$state2)}
    else if(values2$flowing == "Outflow"){values2$df_data <- subset(df_origs_os, state_int == input$state2)}
    else{values2$df_data <- subset(df_outs_st2, state_int == input$state2)}
    
    if(values2$exclusion){values2$df_data <- subset(values2$df_data, region != input$state2)}
  })
  
  # Region
  observeEvent(input$flow3, {
    cat("See?")
    values3$flowing <- input$flow3
    
    if(values3$flowing == "Inflow"){values3$df_data <- subset(df_dest_or, state_int == input$region)}
    else if(values3$flowing == "Outflow"){values3$df_data <- subset(df_origs_or, state_int == input$region)}
    else{values3$df_data <- subset(df_outr_st2, state_int == input$region)}
    
    if(values3$exclusion){values3$df_data <- subset(values3$df_data, state_not != input$region)}
  })
  
  observeEvent(input$exclude, {
    values$exclusion <- input$exclude
    
    if(values$exclusion){values$df_data <- subset(values$df_data, subregion != input$county | region != input$state)}
    else{
      if(values$flowing == "Inflow"){values$df_data <- subset(df_dest2, cty_int == input$county & state_int == input$state)}
      else if(values$flowing == "Outflow"){values$df_data <- subset(df_origs2, cty_int == input$county & state_int == input$state)}
      else{values$df_data <- subset(df_outs2, cty_int == input$county & state_int == input$state)}
    }
  })
  
  # State
  observeEvent(input$exclude2, {
    values2$exclusion <- input$exclude2
    
    if(values2$exclusion){values2$df_data <- subset(values2$df_data, region != input$state2)}
    else{
      if(values2$flowing == "Inflow"){values2$df_data <- subset(df_dest_os, state_int == input$state2)}
      else if(values2$flowing == "Outflow"){values2$df_data <- subset(df_origs_os, state_int == input$state2)}
      else{values2$df_data <- subset(df_outs_st2, state_int == input$state2)}
    }
  })
  
  # Region
  observeEvent(input$exclude3, {
    values3$exclusion <- input$exclude3
    
    if(values3$exclusion){values3$df_data <- subset(values3$df_data, state_not != input$region)}
    else{
      if(values3$flowing == "Inflow"){values3$df_data <- subset(df_dest_or, state_int == input$region)}
      else if(values3$flowing == "Outflow"){values3$df_data <- subset(df_origs_or, state_int == input$region)}
      else{values3$df_data <- subset(df_outr_st2, state_int == input$region)}
    }
  })

  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
    paste0("Migrant ", input$flow, ": ", str_to_title(input$county), " (", input$year, ")")
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  # States
  formulaText2 <- reactive({
    paste0("Migrant ", input$flow2, ": ", str_to_title(input$state2), " (", input$year2, ")")
  })
  
  # Return the formula text for printing as a caption
  output$caption2 <- renderText({
    formulaText2()
  })
  
  # Region
  formulaText3 <- reactive({
    paste0("Migrant ", input$flow3, ": ", str_to_title(input$region), " (", input$year3, ")")
  })
  
  # Return the formula text for printing as a caption
  output$caption3 <- renderText({
    formulaText3()
  })
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  
  #if(input$flow == "Net Inflow"){
    output$map <- renderPlot({
      ggplot(data = counties, mapping = aes(x = long, y = lat, group = group)) + 
        coord_fixed(1.3) + 
        geom_polygon(color = "black", fill = "gray") +
        geom_polygon(data = values$df_data, 
                     aes(fill = values$df_data[,which(colnames(values$df_data) == paste0("X", as.character(input$year)))])) +
        geom_polygon(color = "black", fill = NA) +
        theme_bw() + 
        ditch_the_axes + 
        scale_fill_gradientn(colours = rev(rainbow(7)), trans="log10") + 
        labs(fill="Number of Migrants")
    })
  #}
    
    # State
    output$map2 <- renderPlot({
      ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) + 
        coord_fixed(1.3) + 
        geom_polygon(color = "black", fill = "gray") +
        geom_polygon(data = values2$df_data, 
                     aes(fill = values2$df_data[,which(colnames(values2$df_data) == paste0("X", as.character(input$year2)))])) +
        geom_polygon(color = "black", fill = NA) +
        theme_bw() + 
        ditch_the_axes + 
        scale_fill_gradientn(colours = rev(rainbow(7)), trans="log10") + 
        labs(fill="Number of Migrants")
    })
    
    # State
    output$map3 <- renderPlot({
      ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) + 
        coord_fixed(1.3) + 
        geom_polygon(color = "black", fill = "gray") +
        geom_polygon(data = values3$df_data, 
                     aes(fill = values3$df_data[,which(colnames(values3$df_data) == paste0("X", as.character(input$year3)))])) +
        geom_polygon(color = "black", fill = NA) +
        theme_bw() + 
        ditch_the_axes + 
        scale_fill_gradientn(colours = rev(rainbow(7)), trans="log10") + 
        labs(fill="Number of Migrants")
    })
    
    # Hover
    ## Counties
    output$info <- renderText({
      xy_str <- function(e){
        locs = latlong2county(data.frame(e$x, e$y))
        locs2 = data.frame(state = substr(locs,1,regexpr(",", locs)-1),
                           county = substr(locs,regexpr(",",locs)+1,nchar(locs)))
        strs = paste(locs2$county, locs2$state, sep = ", ")
        vals = values$df_data[which(values$df_data$region == locs2$state & values$df_data$subregion == locs2$county)[1],
                              which(colnames(values$df_data) == paste0("X", as.character(input$year)))]
        
        if(!is.na(vals)){
          strs = (paste0(strs, "\n", as.character(vals),collapse=""))
        }
        return(strs)
      }
      if(!is.null(input$map_hov)){
        print(xy_str(input$map_hov))
      }
    })
    
    # Hover
    ## State
    output$info2 <- renderText({
      xy_str2 <- function(e){
        locs = latlong2county(data.frame(e$x, e$y))
        locs2 = data.frame(state2 = substr(locs,1,regexpr(",", locs)-1))
        strs = locs2$state2
        vals = values2$df_data[which(values2$df_data$region == locs2$state2)[1],
                              which(colnames(values2$df_data) == paste0("X", as.character(input$year2)))]
        
        if(!is.na(vals)){
          strs = (paste0(strs, "\n", as.character(vals),collapse=""))
        }
        return(strs)
      }
      if(!is.null(input$map2_hov)){
        print(xy_str2(input$map2_hov))
      }
    })
    
    # Hover
    ## Region
    output$info3 <- renderText({
      xy_str3 <- function(e){
        locs = latlong2county(data.frame(e$x, e$y))
        locs2 = data.frame(state = substr(locs,1,regexpr(",", locs)-1),
                           county = substr(locs,regexpr(",",locs)+1,nchar(locs)))
        hov_reg = as.character(values3$df_data[match(locs2$state,values3$df_data$region),32])
        strs = hov_reg
        vals = values3$df_data[which(values3$df_data$region == locs2$state)[1],
                              which(colnames(values3$df_data) == paste0("X", as.character(input$year3)))]
        
        if(!is.na(vals)){
          strs = (paste0(strs, "\n", as.character(vals),collapse=""))
        }
        return(strs)
      }
      if(!is.null(input$map3_hov)){
        print(xy_str3(input$map3_hov))
      }
    })
  
    # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Integer"),
      Value = as.character(c(input$year)), 
      stringsAsFactors=FALSE)
  }) 
  
  # States
  # Reactive expression to compose a data frame containing all of the values
  sliderValues2 <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Integer"),
      Value = as.character(c(input$year2)), 
      stringsAsFactors=FALSE)
  }) 
  
  # Region
  # Reactive expression to compose a data frame containing all of the values
  sliderValues3 <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Integer"),
      Value = as.character(c(input$year3)), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })
  
  # States
  # Show the values using an HTML table
  output$values2 <- renderTable({
    sliderValues2()
  })
  
  # Region
  # Show the values using an HTML table
  output$values3 <- renderTable({
    sliderValues3()
  })
  
  observe({
    x <- input$state
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "county",
                      choices = setNames(list(unique(subset(counties, region == input$state)$subregion)),
                                         "Counties"),
                      selected = unique(subset(counties, region == input$state)$subregion)[1]
    )
  })
  
  # Help
  # Return the formula text for printing as a caption
  output$caption4 <- renderUI({
    str1 <- "Checking \"Exclude County of Interest\" or the corresponding mark for 
    State and Region will gray out the selected area for which 
    inflows, outflows, or net inflows are desired.<br/>"
    str2 <- "Note that the values for Net Inflow are log base-10 of (inflows/outflows))
    while for Outflow and Inflow, it's the total number of migrations<br/>"
    str3 <- "Lastly, dark grey means that there were no flows in the specified year but there were
    flows in other years. Light grey means there were no flows between 1990 and 2010 between
    the specified areas in the given direction.<br/>"
    str4 <- "Cite: I used  Matthew Hauer and James Byar's 2019 paper, \"IRS county-to-county migration data, 1990-2010\"
    for the migration data, which they originally obtained from the IRS dataset."
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
  })
  
})


