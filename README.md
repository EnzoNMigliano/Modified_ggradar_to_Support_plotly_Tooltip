### Checkout the plotly adaptation working on ggradar live at my Shiny App

https://enzo-migliano.shinyapps.io/volleynaia/


### Most of the code posted in the folder "scr" comes from @ricardo-bion. Please, follow the link below for the ggradar source code:

https://github.com/ricardo-bion/ggradar


### How to use the customization

A. In the ggrardar source code

1. In the source code search (i.e., ctrl + f) for "plotly.tooltip"

2.  Find the "plotly.tooltip" close to the code were we create the geom_point (i.e.,base <- base + geom_point)

3. Customize the argument "text =" from geom_point() as you woul for a regular ggplot that you wish to transform in a plotly


B. When calling the ggradar function

4. When creating your ggradar include the argument "plotly.tooltip = TRUE"

5. Hope for the best! Lol
