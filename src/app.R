#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)
library(plotly)
library(shinycssloaders)


options(shiny.sanitize.errors = TRUE)

################# This code was adapted from the package ggradar by Ricardo Bion
# I had to adapt the code to make it compatiable with the plotly tooltip :)

ggradar <- function(plot.data,
                    base.size = 15,
                    font.radar = "sans",
                    values.radar = c("0%", "50%", "100%"),
                    axis.labels = colnames(plot.data)[-1],
                    grid.min = 0, # 10,
                    grid.mid = 0.5, # 50,
                    grid.max = 1, # 100,
                    centre.y = grid.min - ((1 / 9) * (grid.max - grid.min)),
                    plot.extent.x.sf = 1,
                    plot.extent.y.sf = 1.2,
                    x.centre.range = 0.02 * (grid.max - centre.y),
                    label.centre.y = FALSE,
                    grid.line.width = 0.5,
                    gridline.min.linetype = "longdash",
                    gridline.mid.linetype = "longdash",
                    gridline.max.linetype = "longdash",
                    gridline.min.colour = "grey",
                    gridline.mid.colour = "#007A87",
                    gridline.max.colour = "grey",
                    grid.label.size = 6,
                    gridline.label.offset = -0.1 * (grid.max - centre.y),
                    label.gridline.min = TRUE,
                    label.gridline.mid = TRUE,
                    label.gridline.max = TRUE,
                    axis.label.offset = 1.15,
                    axis.label.size = 5,
                    axis.line.colour = "grey",
                    group.line.width = 1.5,
                    group.point.size = 6,
                    group.colours = NULL,
                    background.circle.colour = "#D7D6D1",
                    background.circle.transparency = 0.2,
                    plot.legend = if (nrow(plot.data) > 1) TRUE else FALSE,
                    legend.title = "",
                    plot.title = "",
                    legend.text.size = 14,
                    legend.position = "left",
                    plotly.tooltip = FALSE,
                    absolute.values = NULL) {
  
  plot.data <- as.data.frame(plot.data)
  
  if(!is.factor(plot.data[, 1])) {
    plot.data[, 1] <- as.factor(as.character(plot.data[, 1]))
  }
  
  names(plot.data)[1] <- "group"
  
  
  if(!is.null(absolute.values)){
    
    # adding the absolute values of the data set
    absolute.values <- as.data.frame(absolute.values)
    
    if(!is.factor(absolute.values[, 1])) {
      absolute.values[, 1] <- as.factor(as.character(absolute.values[, 1]))
    }
    
    names(absolute.values)[1] <- "group"
    
  }
  
  
  
  var.names <- colnames(plot.data)[-1] # Short version of variable names
  # axis.labels [if supplied] is designed to hold 'long version' of variable names
  # with line-breaks indicated using \n
  
  # calculate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x <- (grid.max + abs(centre.y)) * plot.extent.x.sf
  plot.extent.y <- (grid.max + abs(centre.y)) * plot.extent.y.sf
  
  # Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data) - 1) {
    stop("'axis.labels' contains the wrong number of axis labels", call. = FALSE)
  }
  if (min(plot.data[, -1]) < centre.y) {
    stop("plot.data' contains value(s) < centre.y", call. = FALSE)
  }
  if (max(plot.data[, -1]) > grid.max) {
    stop("'plot.data' contains value(s) > grid.max", call. = FALSE)
  }
  
  ### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data
  # [creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[, 2:ncol(plot.data)] <- plot.data[, 2:ncol(plot.data)] + abs(centre.y)
  # print(plot.data.offset)
  # (b) convert into radial coords
  group <- NULL
  group$path <- CalculateGroupPath(plot.data.offset)
  
  # adding the columns for the plotly tooltip
  
  if(plotly.tooltip == TRUE){
    
    # creates the same format data as the y and x path in the group$path data frame
    # for the non-normalized values
    longer.absolute <- absolute.values %>% pivot_longer(cols = var.names,
                                                        names_to = "feature",
                                                        values_to = "value")
    # normalized (scaled values)
    longer.rescaled <- plot.data %>% pivot_longer(cols = var.names,
                                                  values_to = "rescaled_value") %>%
      select(rescaled_value) %>%
      mutate(percentage = round(rescaled_value*100, digits = 1)) %>%
      select(percentage)
    
    
    #format all the data into one dataframe
    absolute.rescaled <- cbind(longer.absolute, longer.rescaled)
    
    # creates dataframes according to the level of the factor group placed into the list
    list.final <- list()
    
    for(i in 1:length(levels(absolute.rescaled$group))){
      
      list.final[[i]] <- subset(absolute.rescaled, absolute.rescaled$group == levels(absolute.rescaled$group)[i])
      
      
    }
    # adds a row to every factor level do match the data from the groups$path
    # the first row of each level repeats it self in the last row of each level
    # therefore the first row is copied an inserted at the end of levels' dataframe
    for(i in 1:length(levels(absolute.rescaled$group))){
      
      if(i == 1){
        
        list.final[[i]] <- rbind(list.final[[i]], list.final[[i]][1,])
        
        
        #ready.to.cbind is a data frame containing absolute, normalized, and feature in the exact same format as group$path
        ready.to.cbind <- list.final[[i]]
        
      }else{
        
        list.final[[i]] <- rbind(list.final[[i]], list.final[[i]][1,])
        
        ready.to.cbind <- rbind(ready.to.cbind, list.final[[i]])
        
        
      }
      
      
    }
    
    #getting only the distinct columns of the data frame
    ready.to.cbind <- ready.to.cbind[,2:4]
    
    rownames(ready.to.cbind) <- NULL
    
    # cbiding with the group$path dataframe
    
    group$path <- cbind(group$path, ready.to.cbind)
    
    
    
  }
  
  
  #print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CalculateAxisPath(var.names, grid.min + abs(centre.y), grid.max + abs(centre.y))
  # print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  # Labels
  axis$label <- data.frame(
    text = axis.labels,
    x = NA,
    y = NA
  )
  # print(axis$label)
  # axis label coordinates
  n.vars <- length(var.names)
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * sin(angles[i])
  })
  axis$label$y <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * cos(angles[i])
  })
  # print(axis$label)
  # (e) Create Circular grid-lines + labels
  # caclulate the cooridinates required to plot circular grid-lines for three user-specified
  # y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0, 0), grid.min + abs(centre.y), npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0, 0), grid.mid + abs(centre.y), npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0, 0), grid.max + abs(centre.y), npoints = 360)
  # print(head(gridline$max$path))
  # gridline labels
  gridline$min$label <- data.frame(
    x = gridline.label.offset, y = grid.min + abs(centre.y),
    text = as.character(grid.min)
  )
  gridline$max$label <- data.frame(
    x = gridline.label.offset, y = grid.max + abs(centre.y),
    text = as.character(grid.max)
  )
  gridline$mid$label <- data.frame(
    x = gridline.label.offset, y = grid.mid + abs(centre.y),
    text = as.character(grid.mid)
  )
  # print(gridline$min$label)
  # print(gridline$max$label)
  # print(gridline$mid$label)
  ### Start building up the radar plot
  
  # Declare 'theme_clear', with or without a plot legend as required by user
  # [default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw(base_size = base.size) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.key = element_rect(linetype = "blank")
    )
  
  if (plot.legend == FALSE) legend.position = "none"
  
  # Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)],
  # then centred labels for axis labels almost immediately above/below x= 0
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly
  # identify plot extent when plotting first (base) layer]
  
  # base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(
      data = subset(axis$label, axis$label$x < (-x.centre.range)),
      aes(x = x, y = y, label = text), size = axis.label.size, hjust = 1, family = font.radar
    ) +
    scale_x_continuous(limits = c(-1.5 * plot.extent.x, 1.5 * plot.extent.x)) +
    scale_y_continuous(limits = c(-plot.extent.y, plot.extent.y))
  
  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  base <- base + geom_path(
    data = gridline$min$path, aes(x = x, y = y),
    lty = gridline.min.linetype, colour = gridline.min.colour, size = grid.line.width
  )
  base <- base + geom_path(
    data = gridline$mid$path, aes(x = x, y = y),
    lty = gridline.mid.linetype, colour = gridline.mid.colour, size = grid.line.width
  )
  base <- base + geom_path(
    data = gridline$max$path, aes(x = x, y = y),
    lty = gridline.max.linetype, colour = gridline.max.colour, size = grid.line.width
  )
  
  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(
    data = subset(axis$label, abs(axis$label$x) <= x.centre.range),
    aes(x = x, y = y, label = text), size = axis.label.size, hjust = 0.5, family = font.radar
  )
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(
    data = subset(axis$label, axis$label$x > x.centre.range),
    aes(x = x, y = y, label = text), size = axis.label.size, hjust = 0, family = font.radar
  )
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(
    data = gridline$max$path, aes(x, y),
    fill = background.circle.colour,
    alpha = background.circle.transparency
  )
  
  # + radial axes
  base <- base + geom_path(
    data = axis$path, aes(x = x, y = y, group = axis.no),
    colour = axis.line.colour
  )
  
  # ... + group (cluster) 'paths'
  base <- base + geom_path(
    data = group$path, aes(x = x, y = y, group = group, colour = group),
    size = group.line.width
  )
  
  # ... + group points (cluster data)
  
  if(plotly.tooltip == TRUE){ # geom_point with the text for plotly conversion with ggplotly
    base <- base + geom_point(data = group$path, aes(x = x, y = y, group = group, colour = group,
                                                     text = paste0("<b>",
                                                                   group,
                                                                   "</b>",
                                                                   "<br>",
                                                                   "<br><b>",
                                                                   feature,
                                                                   "</b><br>Value: ", value,
                                                                   "<br>Rescaled Value: ", percentage,"%"), size = group.point.size))
    
  }else{ #normal geom_point
    
    base <- base + geom_point(data = group$path, aes(x = x, y = y, group = group, colour = group, size = group.point.size))
    
  }
  
  # ... + amend Legend title
  if (plot.legend == TRUE) base <- base + labs(colour = legend.title, size = legend.text.size)
  
  # ... + grid-line labels (max; mid; min)
  if (label.gridline.min == TRUE) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[1]), data = gridline$min$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar)
  }
  if (label.gridline.mid == TRUE) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[2]), data = gridline$mid$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar)
  }
  if (label.gridline.max == TRUE) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[3]), data = gridline$max$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar)
  }
  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y == TRUE) {
    centre.y.label <- data.frame(x = 0, y = 0, text = as.character(centre.y))
    base <- base + geom_text(aes(x = x, y = y, label = text), data = centre.y.label, size = grid.label.size, hjust = 0.5, family = font.radar)
  }
  
  if (!is.null(group.colours)) {
    colour_values <- rep(group.colours, 100)
  } else {
    colour_values <- rep(c(
      "#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051",
      "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"
    ), 100)
  }
  
  base <- base + theme(legend.key.width = unit(3, "line")) + theme(text = element_text(
    size = 20,
    family = font.radar
  )) +
    theme(legend.text = element_text(size = legend.text.size), legend.position = legend.position) +
    theme(legend.key.height = unit(2, "line")) +
    scale_colour_manual(values = colour_values) +
    theme(text = element_text(family = font.radar)) +
    theme(legend.title = element_blank())
  
  if(legend.title != "") {
    base <- base + theme(legend.title = element_text())
  }
  
  if (plot.title != "") {
    base <- base + ggtitle(plot.title)
  }
  
  return(base)
}


CalculateGroupPath <- function(df) {
  path <- df[, 1]
  
  ## find increment
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / (ncol(df) - 1))
  ## create graph data frame
  graphData <- data.frame(seg = "", x = 0, y = 0)
  graphData <- graphData[-1, ]
  
  for (i in levels(path)) {
    pathData <- subset(df, df[, 1] == i)
    for (j in c(2:ncol(df))) {
      # pathData[,j]= pathData[,j]
      
      
      graphData <- rbind(graphData, data.frame(
        group = i,
        x = pathData[, j] * sin(angles[j - 1]),
        y = pathData[, j] * cos(angles[j - 1])
      ))
    }
    ## complete the path by repeating first pair of coords in the path
    graphData <- rbind(graphData, data.frame(
      group = i,
      x = pathData[, 2] * sin(angles[1]),
      y = pathData[, 2] * cos(angles[1])
    ))
  }
  # Make sure that name of first column matches that of input data (in case !="group")
  colnames(graphData)[1] <- colnames(df)[1]
  graphData$group <- factor(graphData$group, levels=levels(df[, 1]) ) # keep group order
  graphData # data frame returned by function
}


CalculateAxisPath <- function(var.names, min, max) {
  # var.names <- c("v1","v2","v3","v4","v5")
  n.vars <- length(var.names) # number of vars (axes) required
  # Cacluate required number of angles (in radians)
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.vars)
  # calculate vectors of min and max x+y coords
  min.x <- min * sin(angles)
  min.y <- min * cos(angles)
  max.x <- max * sin(angles)
  max.y <- max * cos(angles)
  # Combine into a set of uniquely numbered paths (one per variable)
  axisData <- NULL
  for (i in 1:n.vars) {
    a <- c(i, min.x[i], min.y[i])
    b <- c(i, max.x[i], max.y[i])
    axisData <- rbind(axisData, a, b)
  }
  # Add column names + set row names = row no. to allow conversion into a data frame
  colnames(axisData) <- c("axis.no", "x", "y")
  rownames(axisData) <- seq(1:nrow(axisData))
  # Return calculated axis paths
  as.data.frame(axisData)
}



funcCircleCoords <- function(center = c(0, 0), r = 1, npoints = 100) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}





################# End Adapted code 

## data


naia_volley <- read.csv("naia.csv")
naia_radar <- read.csv("naia_team_comparisom.csv")

naia_radar <- naia_radar[,2:9]


## aplication

# Define UI for application that draws a histogram
ui <- fluidPage(
    
  br(),
  
    # Application title
    titlePanel(h2(img(src = "https://raw.githubusercontent.com/EnzoNMigliano/Week2-CIS-images/main/images/volley.jpeg", height = 70, width = 200),
                  "Naia Womens' Volleyball Top 20 National Teams (2006-2019)", align = "center")),
 
    
  br(),
  
  br(),
  
    tabsetPanel(type = "tabs",
                
                tabPanel("Home",
                         br(),
                         h3(HTML("<b>NAIA Volleyball Shiny App</b>"), align = "center"),
                         br(),
                         HTML("<div><p> <b>Welcome!</b> This Shiny App provides a quick overview
                         of  Womens' Volleyball at a college level league.
                         The data used in this Shiny App was gathered from 
                              the National Association of Intercollegiate Athletics
                              (NAIA) <a href = http://www.dakstats.com/WebSync/Pages/MultiTeam/MultiTeamStandings.aspx?association=10&sg=WVB&sea=NAIWVB_2020&division=NAIWVB1>official website</a>.
                              The data set contains some statistics about the top 20 teams of each season
                              from 2006 to 2019. There are two analyses available:
                              <br><br><ul> - Historical Analysis
                              <br><br> - Comparative Analysis</ul>
                              <br>The analyses can be found under their respective tabs located at the top of the webpage.
                              Under the historical analysis you will be able to have an overview of the data,
                              exploring which teams were the top 20 teams during different seasons. While in the comparison
                              analysis, you will be able to compare teams accros seasons regarding their statistics. I hope
                              you enjoy this app! <br><br><center><i>(Feel free to contact me at my LinkedIn or Github through the links located at
                              the bottom of the webpage)</i></center>
                              
                              </p></div>")),
                
                tabPanel("Historical",
                         
                         br(),
                        
                         
                         h3(HTML("<b>Historical Data</b>"), align = "center"),
                         
                         br(),
                         
                         br(),
                         
                         # Sidebar with a slider input for number of bins 
                         sidebarLayout(
                           sidebarPanel(
                             sliderInput("Year",
                                         h4(HTML("<b>Year</b>")),
                                         min = 2006,
                                         max = 2019,
                                         value = 2019,
                                         sep = ""),
                             selectInput(inputId = "WL",
                                         label = h4(HTML("<b>Wins or Losses</b>")),
                                         choices = list(Wins = "W",Losses = "L")),
                             radioButtons(inputId = "display",
                                          label = h4(HTML("<b>Display</b>")),
                                          choices = c("Both", "Graph", "Table")),
                             
                           ),
                           
                           # Show a plot of the generated distribution
                         mainPanel(
                                  conditionalPanel(
                                    condition = "input.display == 'Both'",
                                    withSpinner(plotlyOutput("distPlot")),
                                    br(),
                                    div(withSpinner(tableOutput("table")), align = "center")
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.display == 'Graph'",
                                    withSpinner(plotlyOutput("distPlot2"))
                                    
                                    
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.display == 'Table'",
                                    div(withSpinner(tableOutput("table2")), align = "center")
                                    
                                    
                                  )
                         ) 
                           
                         )
                ),
                tabPanel("Comparative",
                         br(),
                         
                         
                         h3(HTML("<b>Comparative Analysis</b>"), align = "center"),
                         
                         br(),
                         
                         
                         HTML("<div><p><b><h5>Understanding a Radar Plot</h5></b></br>
                                  A singular plot would not display how two different teams compare with one another
                                  accross different features. Therefore, many utilize the radar plot, specially in sports,
                                  to display along the different radius of a circle how two  teams compare with one another 
                                  in the same dimentions. In the NAIA data set we have the following dimentions:
                                  <br><ul>
                                  - W (Number of Wins) <br>
                                  - L (NUmber of Losses) <br>
                                  - PF (Points of)<br>
                                  - PA (POints Against)<br>
                                  - PCT (Percentage of wins on the total number of games)<br>
                                  - Year <br>
                                  - Position (Nation wide leaderboard, poistion = 1 means 1st) <br>
                                  </ul>
                                  <br>
                                  The radar plot can be really hard to interpret, if you still having question regarding this plot
                                  please feel free to access some external resources about this type of plot <a href = https://www.edrawmax.com/radar-chart/#:~:text=A%20radar%20chart%20is%20an,on%20a%20two%2Ddimensional%20plane.&text=Sometimes%2C%20the%20axes%20are%20also,to%20plot%20the%20spider%20chart.> here </a>.
                              Also, if is your preference you may visualize the plot in a bar graph by selecting 'Type of Plot = Bar'"),
                         br(),
                         br(),
                         br(),
                         sidebarLayout(
                           sidebarPanel(div(radioButtons(inputId = "TypeofPlot",
                                                         label = h3(HTML("<b>Type of Plot</b>")),
                                                         choices = c("Radar Plot", "Bar Graph"),
                                                         inline = TRUE)),
                         div(  
                         radioButtons(inputId = "ComparisonNumber",
                                      label = h3(HTML("<b>Number of Teams</b>")),
                                      choices = c(1,2,3),
                                      inline = TRUE),
                         align = "center"),
                         
                         hr(),

                         
                         HTML("<center><h4><b>Select Your Team</b><h4></center>"),
                         
                         conditionalPanel(condition = "input.ComparisonNumber == 1",
                                          sliderInput("Year2",
                                                      h4(HTML("<b>Year</b>")),
                                                      min = 2006,
                                                      max = 2019,
                                                      value = 2019,
                                                      sep = ""),
                                          uiOutput("input")),
                         
                         conditionalPanel(condition = "input.ComparisonNumber == 2",
                                          sliderInput("Year3",
                                                      h4(HTML("<b>Year</b>")),
                                                      min = 2006,
                                                      max = 2019,
                                                      value = 2019,
                                                      sep = ""),
                                          uiOutput("input2"),
                                          sliderInput("Year4",
                                                      h4(HTML("<b>Year</b>")),
                                                      min = 2006,
                                                      max = 2019,
                                                      value = 2019,
                                                      sep = ""),
                                          uiOutput("input3")),
                           
                         conditionalPanel(condition = "input.ComparisonNumber == 3",
                                          sliderInput("Year5",
                                                      h4(HTML("<b>Year</b>")),
                                                      min = 2006,
                                                      max = 2019,
                                                      value = 2019,
                                                      sep = ""),
                                          uiOutput("input4"),
                                          sliderInput("Year6",
                                                      h4(HTML("<b>Year</b>")),
                                                      min = 2006,
                                                      max = 2019,
                                                      value = 2019,
                                                      sep = ""),
                                          uiOutput("input5"),
                                          sliderInput("Year7",
                                                      h4(HTML("<b>Year</b>")),
                                                      min = 2006,
                                                      max = 2019,
                                                      value = 2019,
                                                      sep = ""),
                                          uiOutput("input6"))
                             
                           ),
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                                     
                             div(uiOutput("titleCompare"), align = "center"),
                             br(),
                                                          br(),
                           withSpinner(plotlyOutput("compare")),
                           div(withSpinner(tableOutput("compareTable")), align = "center")
                           ) 
                           
                         ))),
  br(),
  
  hr(),
  
  print("Copyright Enzo Novi Migliano ©"), HTML("&nbsp;&nbsp;&nbsp;&nbsp;<a href = https://github.com/EnzoNMigliano>  Github </a>&nbsp;&nbsp;&nbsp;&nbsp;<a href = https://www.linkedin.com/in/enzo-novi-migliano/> LinkedIn </a>")
  
    
        
        
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    naia_volley <- read.csv("naia.csv")
    
    
   
    
   
    output$distPlot <- renderPlotly({
    
        
        if(input$WL == "L"){
          
          
        
          plot_lose <- ggplot(data = naia_volley %>% filter(Year == input$Year),
                 mapping = aes(y = L,
                               x = factor(Position),
                               group = factor(Position),
                               text = paste0("<b>",University,"</b>",
                                             "<br>","<br>",
                                             "Position: ", Position,
                                             "<br>Wins: ", W,
                                             "<br>Losses: ", L,
                                             "<br>Points of: ", PF,
                                             "<br>Points Against: ",  PA))) +
            geom_col(mapping = aes(fill = factor(Position)),
                     position = "dodge", stat = "identity",
                     orientation = "x") +
            facet_wrap(~Year) +
            scale_y_continuous(
              labels = scales::number_format(accuracy = 1)) +
            ylim(0, 15)+
            theme_minimal() +
            labs(title = "Number of Losses by National Position",
                 subtitle = "Filtered by Year",
                 caption = "Source: NAIA",
                 x = "Position",
                 y = "Losses",
                 fill = "Position") 
          ggplotly(plot_lose, tooltip = "text")
            
        }else{
            
         
          
          plot_win <- ggplot(data = naia_volley %>% filter(Year == input$Year),
                 mapping = aes(y = W,
                               x = factor(Position),
                               group = factor(Position),
                               text = paste0("<b>",University,"</b>",
                                             "<br>","<br>",
                                             "Position: ", Position,
                                             "<br>Wins: ", W,
                                             "<br>Losses: ", L,
                                             "<br>Points of: ", PF,
                                             "<br>Points Against: ",  PA))) +
            geom_col(mapping = aes(fill = factor(Position)),
                     position = "dodge", stat = "identity",
                     orientation = "x") +
            facet_wrap(~Year) +
            theme_minimal() +
            labs(title = "Number of Wins by National Position",
                 subtitle = "Filtered by Year",
                 caption = "Source: NAIA",
                 x =  "Position",
                 y = "Wins",
                 fill = "Position")
            
          ggplotly(plot_win, tooltip = "text")
          
        }
            
      }) #render plot 
    
    
    
    output$distPlot2 <- renderPlotly({
      
      
      if(input$WL == "L"){
        
        
        
        plot_lose <-  ggplot(data = naia_volley %>% filter(Year == input$Year),
               mapping = aes(y = L,
                             x = factor(Position),
                             group = factor(Position),
                             text = paste0("<b>",University,"</b>",
                                           "<br>","<br>",
                                           "Position: ", Position,
                                           "<br>Wins: ", W,
                                           "<br>Losses: ", L,
                                           "<br>Points of: ", PF,
                                           "<br>Points Against: ",  PA))) +
          geom_col(mapping = aes(fill = factor(Position)),
                   position = "dodge", stat = "identity",
                   orientation = "x") +
          facet_wrap(~Year)  +
          scale_y_continuous(
            labels = scales::number_format(accuracy = 1)) +
          ylim(0, 15)+
          theme_minimal() +
          labs(title = "Number of Losses by National Position",
               subtitle = "Filtered by Year",
               caption = "Source: NAIA",
               x = "Position",
               y = "Losses",
               fill = "Position") 
        ggplotly(plot_lose, tooltip = "text")
        
      }else{
        
        
        
        plot_win <-  ggplot(data = naia_volley %>% filter(Year == input$Year),
               mapping = aes(y = W,
                             x = factor(Position),
                             group = factor(Position),
                             text = paste0("<b>",University,"</b>",
                                           "<br>","<br>",
                                           "Position: ", Position,
                                           "<br>Wins: ", W,
                                           "<br>Losses: ", L,
                                           "<br>Points of: ", PF,
                                           "<br>Points Against: ",  PA))) +
          geom_col(mapping = aes(fill = factor(Position)),
                   position = "dodge", stat = "identity",
                   orientation = "x") +
          facet_wrap(~Year) +
          theme_minimal() +
          labs(title = "Number of Wins by National Position",
               subtitle = "Filtered by Year",
               caption = "Source: NAIA",
               x =  "Position",
               y = "Wins",
               fill = "Position")
        
        ggplotly(plot_win, tooltip = "text")
      }
      
    }) #render plot 
    
    
    output$table2 <- renderTable({naia_volley %>% filter(Year == input$Year)})
    
      output$table <- renderTable({naia_volley %>% filter(Year == input$Year)})
      
      
      output$input <- renderUI({
        selectInput(inputId = "input1",
                    label = h4(HTML("<b>University Name</b>")),
                    choices = naia_volley[naia_volley$Year == input$Year2,]$University)
        
        })
      
      output$input2 <- renderUI({
        selectInput(inputId = "input2",
                    label = h4(HTML("<b>University Name</b>")),
                    choices = naia_volley[naia_volley$Year == input$Year3,]$University)
        
      })
      
      
      output$input3 <- renderUI({
        selectInput(inputId = "input3",
                    label = h4(HTML("<b>University Name</b>")),
                    choices = naia_volley[naia_volley$Year == input$Year4,]$University)
        
      })
      
      
      
      output$input4 <- renderUI({
        selectInput(inputId = "input4",
                    label = h4(HTML("<b>University Name</b>")),
                    choices = naia_volley[naia_volley$Year == input$Year5,]$University)
        
      })
      
      
      output$input5 <- renderUI({
        selectInput(inputId = "input5",
                    label = h4(HTML("<b>University Name</b>")),
                    choices = naia_volley[naia_volley$Year == input$Year6,]$University)
        
      })
      
      
      
      output$input6 <- renderUI({
        selectInput(inputId = "input6",
                    label = h4(HTML("<b>University Name</b>")),
                    choices = naia_volley[naia_volley$Year == input$Year7,]$University)
        
      })
      
      
      output$compareTable <- renderTable({
        
        if(input$ComparisonNumber == 1){
          
          naia_volley %>% filter(University == input$input1,
                                 Year == input$Year2)
          
        }else if(input$ComparisonNumber == 2){
          
          
          temp1 <- naia_volley %>% filter(University == input$input2,
                                 Year == input$Year3)
          
          temp2 <- naia_volley %>% filter(University == input$input3,
                                 Year == input$Year4)
          
          final <- rbind(temp1, temp2)
            
            return(final)
          
        }else{
          
          temp1 <- naia_volley %>% filter(University == input$input4,
                                          Year == input$Year5)
          
          temp2 <- naia_volley %>% filter(University == input$input5,
                                          Year == input$Year6)
          
          
          temp3 <- naia_volley %>% filter(University == input$input6,
                                          Year == input$Year7)
          
          final <- rbind(temp1, temp2, temp3) 
          
          return(final)
          
        }
        
        
        
        
      })
      
      
      output$titleCompare <- renderUI({
        
        if(input$ComparisonNumber == 1){
          
          text <- paste0("<h3>","<b>",input$input1, " ", input$Year2, " Analysis", "</b>", "</h3>")
          
          HTML(text)
          
        }else if(input$ComparisonNumber == 2){
          
          text <- paste0("<h3>","<b>",input$input2, " ", input$Year3, " VS ", input$input3, " ", input$Year4,
                         "</b>", "</h3>")
          
          HTML(text)
          
          
        }else{
          
          
          text <- paste0("<h3>","<b>",input$input4, " ", input$Year5, " VS ", input$input5, " ", input$Year6,
                         " VS ",input$input6, " ", input$Year7, "</b>", "</h3>")
          
          HTML(text)
          
        }
        
      })
      
      
      output$compare <- renderPlotly({
        
        if(input$TypeofPlot != "Bar Graph"){
        
        
        if(input$ComparisonNumber == 1){
          
          
          
          absolute <- naia_volley %>% filter(Year == input$Year2, 
                                             University == input$input1) %>% select(-Year)
          
          plot.data <- naia_radar %>% mutate_each(funs(rescale), -University, -Year) %>%
            filter(Year == input$Year2, 
                   University == input$input1) %>% select(-Year)
          
          ggplotly(ggradar(plot.data = plot.data,
                           plotly.tooltip = TRUE,
                           absolute.values = absolute),
                   tooltip = "text")
            
        
          
        }else if(input$ComparisonNumber == 2){
          
          
          absolute1 <- naia_volley %>% filter(Year == input$Year3, 
                                              University == input$input2) %>% select(-Year)
          
          absolute2 <- naia_volley %>% filter(Year == input$Year4, 
                                              University == input$input3) %>% select(-Year)
            
          absolute <- rbind(absolute1, absolute2)
          
          absolute[1, "University"] <- paste("1.", absolute[1, "University"])
          
          absolute[2, "University"] <- paste("2.", absolute[2, "University"])
          
          plot.data1 <- naia_radar %>% mutate_each(funs(rescale), -University, -Year) %>%
            filter(Year == input$Year3, 
                   University == input$input2) %>% select(-Year)
            
          plot.data2 <- naia_radar %>% mutate_each(funs(rescale), -University, -Year) %>%
            filter(Year == input$Year4, 
                   University == input$input3) %>% select(-Year)
          
          plot.data <- rbind(plot.data1, plot.data2)
          
          ggplotly(ggradar(plot.data = plot.data,
                           plotly.tooltip = TRUE,
                           absolute.values = absolute),
                   tooltip = "text")
          
          
        }else{
          
          
          absolute1 <- naia_volley %>% filter(Year == input$Year5, 
                                              University == input$input4) %>% select(-Year)
          
          absolute2 <- naia_volley %>% filter(Year == input$Year6, 
                                              University == input$input5) %>% select(-Year)
            
            absolute3 <-  naia_volley %>% filter(Year == input$Year7, 
                                                 University == input$input6) %>% select(-Year)
            
            absolute <- rbind(absolute1, absolute2, absolute3)
            
            absolute[1, "University"] <- paste("1.", absolute[1, "University"])
            
            absolute[2, "University"] <- paste("2.", absolute[2, "University"])
            
            absolute[3, "University"] <- paste("3.", absolute[3, "University"])
            
            
            
          
          plot.data1 <- naia_radar %>% mutate_each(funs(rescale), -University, -Year) %>%
            filter(Year == input$Year5, 
                   University == input$input4) %>% select(-Year)
            
            plot.data2 <- naia_radar %>% mutate_each(funs(rescale), -University, -Year) %>%
              filter(Year == input$Year6, 
                     University == input$input5) %>% select(-Year)
            
            plot.data3 <- naia_radar %>% mutate_each(funs(rescale), -University, -Year) %>%
              filter(Year == input$Year7, 
                     University == input$input6) %>% select(-Year)
            
            plot.data <- rbind(plot.data1, plot.data2, plot.data3)
          
          ggplotly(ggradar(plot.data = plot.data,
                           plotly.tooltip = TRUE,
                           absolute.values = absolute),
                   tooltip = "text")
          
        }
          
        }else{
          
          if(input$ComparisonNumber == 1){
            
            
            
            absolute <- naia_volley %>% filter(Year == input$Year2, 
                                               University == input$input1) %>% select(-Year, -PCT)
            names <- colnames(absolute)[-1]
            
           absolute <- absolute %>% pivot_longer(cols = names,
                                                names_to = "feature",
                                                values_to = "values")
            
            ggplotly(ggplot(data = absolute,
                            mapping = aes(y = feature,
                                          x = values,
                                          fill = University,
                                          text = paste0("<b>",University,"</b>",
                                                        "<br>","<br>",
                                                        "<b>",feature,":","</b>",values)))+
                              geom_col(position = "dodge")+
                       theme_minimal(),
                     tooltip = "text")
            
            
            
          }else if(input$ComparisonNumber == 2){
            
            
            absolute1 <- naia_volley %>% filter(Year == input$Year3, 
                                                University == input$input2) %>% select(-Year)
            
            absolute2 <- naia_volley %>% filter(Year == input$Year4, 
                                                University == input$input3) %>% select(-Year)
            
            absolute <- rbind(absolute1, absolute2)
            
            absolute[1, "University"] <- paste("1.", absolute[1, "University"])
            
            absolute[2, "University"] <- paste("2.", absolute[2, "University"])
            
            names <- colnames(absolute)[-1]
           
            absolute <- absolute %>% pivot_longer(cols = names,
                                                  names_to = "feature",
                                                  values_to = "values")
            
            ggplotly(ggplot(data = absolute,
                            mapping = aes(y = feature,
                                          x = values,
                                          fill = University,
                                          text = paste0("<b>",University,"</b>",
                                                        "<br>","<br>",
                                                        "<b>",feature,":","</b>",values)))+
                              geom_col(position = "dodge")+
                       theme_minimal(),
                     tooltip = "text")
            
            
          }else{
            
            
            absolute1 <- naia_volley %>% filter(Year == input$Year5, 
                                                University == input$input4) %>% select(-Year)
            
            absolute2 <- naia_volley %>% filter(Year == input$Year6, 
                                                University == input$input5) %>% select(-Year)
            
            absolute3 <-  naia_volley %>% filter(Year == input$Year7, 
                                                 University == input$input6) %>% select(-Year)
            
            absolute <- rbind(absolute1, absolute2, absolute3)
            
            absolute[1, "University"] <- paste("1.", absolute[1, "University"])
            
            absolute[2, "University"] <- paste("2.", absolute[2, "University"])
            
            absolute[3, "University"] <- paste("3.", absolute[3, "University"])
            
            
            names <- colnames(absolute)[-1]
            
            absolute <- absolute %>% pivot_longer(cols = names,
                                                  names_to = "feature",
                                                  values_to = "values")
            
            
            
            ggplotly(ggplot(data = absolute,
                            mapping = aes(y = feature,
                                          x = values,
                                          fill = University,
                                          text = paste0("<b>",University,"</b>",
                                                        "<br>","<br>",
                                                        "<b>",feature,":","</b>",values)))+
                              geom_col(position = "dodge")+
                       theme_minimal(),
                     tooltip = "text")
            
          }
          
          
          
        }
        
      })
    
} # server

# Run the application 
shinyApp(ui = ui, server = server)
