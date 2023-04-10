library(shiny)
library(dplyr)
library(stringr)
library(rpgmapr)
library(rpgcolorsr)
library(magrittr)
library(sf)
library(purrr)
library(leaflet)
library(shinycssloaders)

# If you are not publishing this app to web (i.e., you are only using it
# locally), you'll need to set the working directory to the application
# directory so that the "Data" folder can be found. If necessary, do that here:
# setwd("path_to_your_local_application_directory")

# Globals ======================================================================

# These are the only things that should really need to be set on a
# project-by-project basis. The basemap uses the Renaissance Planning standards
# for interactive mapping in R; you can change it if you want, but this should
# be sufficient 99% of the time.

# Title
TITLE = "Jobs in Benton County, OR (mimi example application)" #only thing that should need to be changed

# Basemap
BASEMAP = leaflet() %>%
  addMapPane("Basemap", zIndex = 410) %>%
  addMapPane("Details", zIndex = 420) %>%
  addMapPane("Data", zIndex = 430) %>%
  addMapPane("Roads", zIndex = 440) %>%
  addMapPane("Labels", zIndex = 450) %>%
  addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/renplan/cl4zupz4n000u14mv38mp0q2l/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoicmVucGxhbiIsImEiOiJjaWgzcHdjOTIweTJvdzdtNWxlYnZ5MXZjIn0.zMsUwMAoEu6DZvd7IYVtjg",
           options = pathOptions(pane = "Basemap"),
           group = "Basemap") %>%
  addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/renplan/cl4zuotax001o15nyt6lnvssc/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoicmVucGxhbiIsImEiOiJjaWgzcHdjOTIweTJvdzdtNWxlYnZ5MXZjIn0.zMsUwMAoEu6DZvd7IYVtjg",
           options = pathOptions(pane = "Details"),
           group = "Details") %>%
  addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/renplan/cl4zu0pxx002015nwqeasn33i/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoicmVucGxhbiIsImEiOiJjaWgzcHdjOTIweTJvdzdtNWxlYnZ5MXZjIn0.zMsUwMAoEu6DZvd7IYVtjg",
           options = pathOptions(pane = "Roads"),
           group = "Roads") %>%
  addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/renplan/cl4ztwaxq000r14mv74t1tktp/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoicmVucGxhbiIsImEiOiJjaWgzcHdjOTIweTJvdzdtNWxlYnZ5MXZjIn0.zMsUwMAoEu6DZvd7IYVtjg",
           options = pathOptions(pane = "Labels"),
           group = "Labels")

# Data =========================================================================

# The mimi-style app assumes 2 qualities of your data:
# in all of your datasets
# 1. They are sf objects saved as .rds [in EPSG:4326, for leaflet mapping]
# 2. The first column is a unique ID
# Note that the first requirement is a little flexible, if you change how the 
# data is read in/constructed below, but it will make your life a little harder
# if they are not .rds sf objects. The second requirement is NOT FLEXIBLE AT
# ALL, and the application WILL NOT WORK AS INTENDED if this requirement is
# not met!

# By default, the mimi-style app will load all files in the app's "Data" 
# directory WITH NO MODIFICATIONS. In this first section, you can provide
# modifications to the loading and formatting of your data, if necessary (e.g., 
# filters, transformations, merges, etc.). However, note that, if you do change 
# anything:
# 1. anything you want to be mapped MUST wind up in the list "dfs"
# 2. names for each object in "dfs" MUST be in the vector "file_names"
# 3. "file_names" must be set as the list names for "dfs"
# A pro-tip is to save your files with names you'd like to see in a drop down
# menu -- you can prioritize normal-English-intelligible names over standard
# practices in file path naming, if you like (e.g. spaces instead of
# underscores, uses of title case, etc.). If you do this, you won't need to
# update the "file_names" section here!
datasets = list.files("Data", pattern=".rds$")
dfs = lapply(datasets, function(file){
  readRDS(file.path("Data", file))
})
file_names = lapply(datasets, function(x){
  str_replace(x, pattern=".rds$", replacement="")
}) %>%
  unlist()
names(dfs) = file_names

# This section builds a data dictionary of columns by table, separating out the 
# IDs. This dictionary will be the basis for formulating selectors in the UI, 
# as well as for informing any joins between tables that may be requested. DO
# NOT CHANGE ANYTHING IN THIS SECTION, or the app will not work as intended!
ddict = purrr::map2(c(file_names), dfs, function(file,data){
  data.frame(file=file,
             col=names(data)[names(data)!="geometry"],
             type=c("ID",rep("value",ncol(data)-2))) %>%
    #mutate(map_var = paste0(col, " (", file, ")")) %>%
    mutate(dtype = lapply(col, function(x){
      ifelse(is.factor(data[[x]]) | is.character(data[[x]]), "char", "num")
    }) %>% unlist())
}) %>%
  Reduce(rbind, .)

# UI ===========================================================================

# The UI will work perfectly if the data inputs are built as described above.
# It should not need to be altered on a project-by-project basis.

ui = fluidPage(
  # CSS for sending leaflet to back (so selector goes on top of it)
  tags$head(
    tags$style(
      HTML(".leaflet-top, .leaflet-bottom {z-index: unset !important;}")
    )
  ),
  
  # Application title
  titlePanel(TITLE),
  
  # Fluid row: selectors on the left, map on the right
  fluidRow(
    # Sidebar
    column(
      5,
      fluidRow(
        # Panel with variable controls
        column(
          6,
          column(
            12,
            # Introduction to data controls
            h4(HTML("<b>Data controls</b>")),
            h5(HTML(paste0("Select a variable, or expression thereof, to ",
                           "be mapped."))),
            # Map variable
            selectInput(
              inputId = "MAP_TAB",
              label = "Primary variable source:",
              choices = ddict$file %>% unique(),
              selectize = TRUE,
              multiple = FALSE
            ),
            # Map variable -- set in UI to be everything in the map table
            uiOutput(outputId = "UI_MAP_VAR"),
            # Adjust table -- set in UI to be "None" or any table
            uiOutput(outputId = "UI_ADJUST_TAB"),
            # Adjustment variable -- set in UI to be everything in the adjust 
            # table
            uiOutput(outputId = "UI_ADJUST_VAR"),
            # Adjustment operation -- set in UI to only exist if adjust table
            # is given
            uiOutput(outputId = "UI_ADJUST_OP"),
            # Normalization table -- set in UI to be "None" or any table 
            uiOutput(outputId = "UI_NORM_TAB"),
            # Normalization variable -- set in UI to be everything in the 
            # normalization table 
            uiOutput(outputId = "UI_NORM_VAR"),
            # Style
            style = "height:670px;background-color: #3fbfba;"
          ),
          style = paste0("background-color:#beb5af;",
                         "padding-top:10px;",
                         "padding-bottom:10px;",
                         "padding-right:5px;",
                         "padding-left:10px;")
        ),
        # Panel with visualization controls
        column(
          6,
          fluidRow(
            column(
              12,
              column(
                12,
                # Introduction to visualization controls
                h4(HTML("<b>Visualization controls</b>")),
                h5(HTML(paste0("Fine tune the way your data will be ",
                               "visualized."))),
                # Binned or continuous -- set in UI so we can set options based on
                # the map var. Options include natural breaks, quantiles, manual
                # bins or continuous (noting that for factor variables, you have 
                # no options!)
                uiOutput(outputId = "UI_VIZ_TYPE"),
                # If natural breaks or quantiles, yield an nbins input
                uiOutput(outputId = "UI_NBINS"),
                # If manual bins, yield a bin cuts input
                uiOutput(outputId = "UI_CUT_POINTS"),
                # If continuous, yield a min/max inputs
                uiOutput(outputId = "UI_SCALE_MIN"),
                uiOutput(outputId = "UI_SCALE_MAX"),
                # Style
                style = "height:445px;background-color:#96a821;"
              ),
              style = paste0("background-color:#beb5af;",
                             "padding-top:10px;",
                             "padding-bottom:5px;",
                             "padding-right:5px;",
                             "padding-left:5px;")
            )
          ),
          fluidRow(
            # Panel with go button
            column(
              12,
              column(
                12,
                # Go!
                h5(HTML("<b>Click the button below to produce the map!</b>")),
                actionButton(
                  inputId = "GO",
                  label = "Update map"
                ),
                # Style
                style = "height:215px;background-color:#f29c20;padding-top:50px",
                align = "center"
              ),
              style = paste0("background-color:#beb5af;",
                             "padding-top:5px;",
                             "padding-bottom:10px;",
                             "padding-right:5px;", #why are these 0s? think 10, idk but looks best
                             "padding-left:5px;")
            )
          )
        )
      )
    ),
    column(
      7,
      fluidRow(
        # Zoom to feature -- set in UI as all unique feature IDs
        uiOutput(outputId = "UI_ZOOM"),
        # Style
        style = paste0("height:80px;",
                       "background-color:#beb5af;",
                       "padding-top:10px;",
                       "padding-bottom:5px;",
                       "padding-right:10px;",
                       "padding-left:5px;"),
        align = "left"
      ),
      fluidRow(
        # Map
        leafletOutput("MAP", height=595) %>% 
          withSpinner(color="#0dc5c1"),
        # Style
        style = paste0("background-color:#beb5af;",
                       "padding-top:5px;",
                       "padding-bottom:10px;",
                       "padding-right:10px;",
                       "padding-left:5px;"),
      )
    )
  )
)

# Server =======================================================================

# The server will work perfectly if the data inputs and UI are built as 
# described above. On a project-by-project basis, you will not need to change
# the processing of the data, but you may wish to change some of the specifics
# of the maps -- e.g., the color ramps, feature stroke weights, opacity, etc.
# The defaults, however, should cover most use cases.

server = function(input, output){
  
  # Set the possible map variables
  output$UI_MAP_VAR = renderUI({
    selectInput(
      inputId = "MAP_VAR",
      label = "Primary variable:",
      choices = ddict$col[ddict$file == input$MAP_TAB & ddict$type == "value"],
      selectize = TRUE,
      multiple = FALSE
    )
  })
  
  # Set a variable determining if the map variable is a character/factor. We're
  # gonna need to know this for a lot of our option setting
  map_var_is_char = reactive({
    ddict$dtype[ddict$file == input$MAP_TAB & ddict$col == input$MAP_VAR] == "char"
  })
  
  # Set the possible adjustment tables
  output$UI_ADJUST_TAB = renderUI({
    req(input$MAP_VAR)
    if(map_var_is_char()){
      NULL
    } else{
      selectInput(
        inputId = "ADJUST_TAB",
        label = "Adjustment variable source:",
        choices = c("None", ddict$file %>% unique()),
        selectize = TRUE,
        multiple = FALSE
      )
    }
  })
  
  # Set the possible adjustment variables
  output$UI_ADJUST_VAR = renderUI({
    req(input$MAP_VAR, input$ADJUST_TAB)
    if(input$ADJUST_TAB == "None" | map_var_is_char()){
      NULL
    } else{
      selectInput(
        inputId = "ADJUST_VAR",
        label = "Adjustment variable:",
        choices = ddict$col[ddict$file == input$ADJUST_TAB & ddict$type == "value"],
        selectize = TRUE,
        multiple = FALSE
      )
    }
  })
  
  # Create buttons for adjustment op if adjustment is requested
  output$UI_ADJUST_OP = renderUI({
    req(input$MAP_VAR, input$ADJUST_TAB)
    if(input$ADJUST_TAB == "None" | map_var_is_char()){
      NULL
    } else{
      radioButtons(
        inputId = "ADJUST_OP",
        label = "Adjustment operation:",
        choices = c("-","+","x"),
        inline = TRUE
      )
    }
  })
  
  # Set the possible normalization tables
  output$UI_NORM_TAB = renderUI({
    req(input$MAP_VAR)
    if(map_var_is_char()){
      NULL
    } else{
      selectInput(
        inputId = "NORM_TAB",
        label = "Normalization variable source:",
        choices = c("None", ddict$file %>% unique()),
        selectize = TRUE,
        multiple = FALSE
      )
    }
  })
  
  # Set the possible normalization variables
  output$UI_NORM_VAR = renderUI({
    req(input$MAP_VAR, input$NORM_TAB)
    if(input$NORM_TAB == "None" | map_var_is_char()){
      NULL
    } else{
      selectInput(
        inputId = "NORM_VAR",
        label = "Normalization variable:",
        choices = ddict$col[ddict$file == input$NORM_TAB & ddict$type == "value"],
        selectize = TRUE,
        multiple = FALSE
      )
    }
  })
  
  # Set the possible visualization types
  output$UI_VIZ_TYPE = renderUI({
    req(input$MAP_VAR)
    if(map_var_is_char()){
      viz_choices = "Factor"
    } else{
      viz_choices = c("Natural breaks","Quantile","Manual bins","Continuous")
    }
    radioButtons(
      inputId = "VIZ_TYPE",
      label = "Visualization method:",
      choices = viz_choices,
      selected = viz_choices[1]
    )
  })
  
  # If natural breaks or quantiles, yield an nbins input
  output$UI_NBINS = renderUI({
    req(input$VIZ_TYPE)
    if(input$VIZ_TYPE %in% c("Natural breaks","Quantile")){
      textInput(inputId = "NBINS",
                label = "Number of bins:",
                value = "10")
    } else{
      NULL
    }
  })
  
  # If manual bins, yield a bin cuts input
  output$UI_CUT_POINTS = renderUI({
    req(input$VIZ_TYPE)
    if(input$VIZ_TYPE == "Manual bins"){
      textInput(inputId = "CUT_POINTS",
                label = "Input manual bins (separated by commas):",
                value = "",
                placeholder = "Enter 1 or more breaks")
    } else{
      NULL
    }
  })
  
  # If continuous, yield a min/max inputs
  output$UI_SCALE_MIN = renderUI({
    req(input$VIZ_TYPE)
    if(input$VIZ_TYPE == "Continuous"){
      textInput(inputId = "SCALE_MIN",
                label = "Continuous scale minimum:",
                value = "",
                placeholder = "Defaults to map min")
    } else{
      NULL
    }
  })
  output$UI_SCALE_MAX = renderUI({
    req(input$VIZ_TYPE)
    if(input$VIZ_TYPE == "Continuous"){
      textInput(inputId = "SCALE_MAX",
                label = "Continuous scale maximum:",
                placeholder = "Defaults to map max")
    } else{
      NULL
    }
  })
  
  # Build the map!
  leaf = eventReactive(input$GO, {
    # Start by identifying the data of interest (map, adjust, and norm)
    # Map
    data_map = ddict %>%
      dplyr::filter(file == input$MAP_TAB & col == input$MAP_VAR)
    col_map = data_map$col
    tab_map = data_map$file
    id_map = ddict$col[ddict$file == tab_map & ddict$type == "ID"]
    SEL = c(col_map, tab_map, id_map, "__map__") %>% t()
    # Adjust
    if(input$ADJUST_TAB != "None" & !map_var_is_char()){
      data_adj = ddict %>%
        dplyr::filter(file == input$ADJUST_TAB & col == input$ADJUST_VAR)
      col_adj = data_adj$col
      tab_adj = data_adj$file
      id_adj = ddict$col[ddict$file == tab_adj & ddict$type == "ID"]
      SEL = rbind(SEL, c(col_adj, tab_adj, id_adj, "__adj__"))
    }
    # Norm
    if(input$NORM_TAB != "None" & !map_var_is_char()){
      data_norm = ddict %>%
        dplyr::filter(file == input$NORM_TAB & col == input$NORM_VAR)
      col_norm = data_norm$col
      tab_norm = data_norm$file
      id_norm = ddict$col[ddict$file == tab_norm & ddict$type == "ID"]
      SEL = rbind(SEL, c(col_norm, tab_norm, id_norm, "__norm__"))
    }
    # Select data out of the appropriate tables
    SEL = data.frame(SEL) %>%
      setNames(c("col","tab","id","vtype"))
    unique_tabs = unique(SEL$tab)
    LEAF_DF = lapply(unique_tabs, function(x){
      tab_sel = SEL %>% 
        dplyr::filter(tab == x)
      col_sel = c(tab_sel$id[1], tab_sel$col)
      names(col_sel) = c("ID",tab_sel$vtype)
      tab = dfs[[x]] %>%
        dplyr::select(all_of(col_sel))
      if(x == unique_tabs[1]){
        tab
      } else{
        tab %>% st_drop_geometry()
      }
    }) %>%
      purrr::reduce(inner_join, by="ID")
    # Calculate the final variable appropriately, and identify the way in which
    # the variable was calculated
    function_inputs = names(LEAF_DF)
    if("__adj__" %in% function_inputs){
      if(input$ADJUST_OP == "+"){
        intermediate = LEAF_DF$`__map__` + LEAF_DF$`__adj__`
        VAR_NAME = "(__map__ + __adj__)"
      } else if(input$ADJUST_OP == "-"){
        intermediate = LEAF_DF$`__map__` - LEAF_DF$`__adj__`
        VAR_NAME = "(__map__ - __adj__)"
      } else{
        intermediate = LEAF_DF$`__map__` * LEAF_DF$`__adj__`
        VAR_NAME = "(__map__ * __adj__)"
      }
    } else{
      intermediate = LEAF_DF$`__map__`
      VAR_NAME = "__map__"
    }
    if("__norm__" %in% function_inputs){
      final = intermediate / LEAF_DF$`__norm__`
      VAR_NAME = paste(VAR_NAME, "/ __norm__")
    } else{
      final = intermediate
      VAR_NAME = str_replace_all(VAR_NAME, "\\(|\\)","")
    }
    LEAF_DF[[VAR_NAME]] = final
    # Change the column names to honor the original data
    w = which(names(LEAF_DF) == VAR_NAME)
    for(i in 1:nrow(SEL)){
      names(LEAF_DF) = str_replace_all(names(LEAF_DF), SEL$vtype[i], SEL$col[i])
    }
    names(LEAF_DF)[1] = SEL$id[SEL$vtype == "__map__"]
    # Identify the "final variable" (i.e., the variable that will be mapped)
    FINAL_VAR = names(LEAF_DF)[w]
    # Produce labels
    labs = LEAF_DF %>%
      st_drop_geometry() %>%
      purrr::map2(names(.), ., function(col,vals){
        if(is.numeric(vals)){
          paste0("<b>", col, "</b>: ", prettyNum(round(vals,3), big.mark=","))
        } else{
          paste0("<b>", col, "</b>: ", vals)
        }
      }) %>%
      purrr::pmap(., paste, sep="<br>") %>%
      unlist()
    LEAF_DF$`__label__` = labs
    # Select variables of interest
    map_set = c(names(LEAF_DF)[1],
                "value"=FINAL_VAR, 
                "label"="__label__")
    LEAF_DF = LEAF_DF %>%
      dplyr::select(all_of(map_set))
    # Make the palette based on the visualization requested
    if(map_var_is_char()){
      # Variable is a character -- must be Factor palette
      PAL = colorFactor(palette = rpg_color_palettes$rpg_rainbow_no_grey,
                        domain = LEAF_DF$value)
    } else if(input$VIZ_TYPE == "Continuous"){
      # Continuous palette -- first, set the scale min and max according to
      # inputs
      if(input$SCALE_MIN == ""){
        min_val = min(LEAF_DF$value, na.rm=TRUE)
      } else{
        min_val = str_replace_all(input$SCALE_MIN, " ","") %>%
          as.numeric()
      }
      if(input$SCALE_MAX == ""){
        max_val = max(LEAF_DF$value, na.rm=TRUE)
      } else{
        max_val = str_replace_all(input$SCALE_MAX, " ","") %>%
          as.numeric()
      }
      # Center it at 0
      lb = floor(min_val)
      ub = ceiling(max_val)
      if(lb < 0){
        if(ub == 0){
          palette_colors = colorRampPalette(rpg_color_palettes$rpg_cold_warm[1:3])(length(lb:ub))
        } else if(ub < 0){
          palette_colors = colorRampPalette(rpg_color_palettes$rpg_cold_warm[1:3])(length(lb:ub)+1)
          palette_colors = palette_colors[1:(length(palette_colors)-1)]
        }
        else{
          lower = colorRampPalette(rpg_color_palettes$rpg_cold_warm[1:3])(abs(lb)+1)
          upper = colorRampPalette(rpg_color_palettes$rpg_cold_warm[3:5])(ub+1)
          lower = lower[1:(length(lower)-1)]
          palette_colors = c(lower, upper)
        }
      } else if(lb == 0){
        palette_colors = colorRampPalette(rpg_color_palettes$rpg_cold_warm[3:5])(length(lb:ub))
      } else{
        palette_colors = colorRampPalette(rpg_color_palettes$rpg_cold_warm[3:5])(length(lb:ub)+1)
        palette_colors = palette_colors[2:length(palette_colors)]
      }
      # Palette is continuous from min to max
      PAL = colorNumeric(palette = palette_colors,
                         domain = lb:ub)
    } else{
      # We have some sort of binned palette -- breaks will differ based on type
      if(input$VIZ_TYPE == "Natural breaks"){
        # Natural breaks
        nbins = as.numeric(input$NBINS)
        breaks = rgeoda::natural_breaks(nbins, LEAF_DF %>% dplyr::filter(!is.na(value)) %>% dplyr::select(value)) %>%
          c(min(LEAF_DF$value, na.rm=TRUE)-(2e-16), ., max(LEAF_DF$value, na.rm=TRUE)+(2e-16))
      } else if(input$VIZ_TYPE == "Quantile"){
        # Quantiles
        nbins = as.numeric(input$NBINS)
        probs = seq(1/nbins, (nbins-1)/nbins, by=1/nbins)
        breaks = quantile(LEAF_DF$value, probs) %>%
          c(min(LEAF_DF$value, na.rm=TRUE)-(2e-16), ., max(LEAF_DF$value, na.rm=TRUE)+(2e-16))
      } else{
        # Manual
        breaks = input$CUT_POINTS %>%
          str_replace_all(" ","") %>%
          str_split(",") %>%
          unlist() %>%
          as.numeric() %>%
          c(min(LEAF_DF$value, na.rm=TRUE)-(2e-16), ., max(LEAF_DF$value, na.rm=TRUE)+(2e-16)) #TODO
      }
      # Center it at 0
      bin_type = lapply(1:(length(breaks)-1), function(i){
        if(breaks[i] < 0){
          if(breaks[i+1] <= 0){
            "neg"
          } else{
            "zero"
          }
        } else{
          "pos"
        }
      }) %>% unlist()
      palette_colors = c(
        colorRampPalette(rpg_color_palettes$rpg_cold_warm[1:3])(sum(bin_type=="neg")),
        colorRampPalette(rpg_color_palettes$rpg_cold_warm[3])(sum(bin_type=="zero")),
        colorRampPalette(rpg_color_palettes$rpg_cold_warm[4:5])(sum(bin_type=="pos")+1)
      )
      # The palette is then Bin with the proper break points
      PAL = colorBin(palette = palette_colors,
                     domain = LEAF_DF$value,
                     bins = unique(breaks))
    }
    # For mapping, we need to make sure we're calling the proper function for
    # the geometry. So, we'll create the call here, with appropriate options
    # Geometry specific options
    gt = st_geometry_type(LEAF_DF, by_geometry=FALSE)
    if(gt %in% c("POINT","MULTIPOINT")){
      LEAF = BASEMAP %>%
        addCircleMarkers(
          data = LEAF_DF,
          radius = 3,
          stroke = FALSE,
          fill = TRUE,
          fillColor = ~PAL(value),
          fillOpacity = 1,
          label = lapply(LEAF_DF$label, HTML),
          highlightOptions = highlightOptions(
            fillColor = "white",
            fillOpacity = 1,
            weight = 6,
            bringToFront = TRUE
          ),
          group = FINAL_VAR,
          options = pathOptions(pane = "DATA")
        )
    } else if(gt %in% c("LINESTRING","MULTILINESTRING")){
      LEAF = BASEMAP %>%
        addPolylines(
          data = LEAF_DF,
          weight = 2,
          stroke = TRUE,
          color = ~PAL(value),
          opacity = 1,
          label = lapply(LEAF_DF$label, HTML),
          highlightOptions = highlightOptions(
            color = "white",
            opacity = 1,
            weight = 4,
            bringToFront = TRUE
          ),
          group = FINAL_VAR,
          options = pathOptions(pane = "Data")
        )
    } else{
      LEAF = BASEMAP %>%
        addPolygons(
          data = LEAF_DF,
          weight = 0.2,
          color = "black",
          opacity = 1,
          fill = TRUE,
          fillColor = ~PAL(value),
          fillOpacity = 0.75,
          label = lapply(LEAF_DF$label, HTML),
          highlightOptions = highlightOptions(
            color = "white",
            opacity = 1,
            weight = 4,
            bringToFront = TRUE
          ),
          group = FINAL_VAR,
          options = pathOptions(pane = "Data")
        )
    }
    # Finally, let's set our legend and layer options
    LEAF = LEAF %>%
      addLegend(
        data = LEAF_DF,
        position = "topright",
        pal = PAL,
        values = ~value,
        title = FINAL_VAR,
        opacity = 1
      ) %>%
      addLayersControl(
        position = "topleft",
        overlayGroups = c("Basemap","Details","Roads","Labels",FINAL_VAR),
        options = layersControlOptions(collapsed = TRUE)
      )
    # We've now got a map!
    return(list(df = LEAF_DF, map = LEAF))
  })
  
  # Produce the map
  output$MAP = renderLeaflet({
    leaf()$map
  })
  
  # Set the "zoom to" selector
  output$UI_ZOOM = renderUI({
    selectInput(
      inputId = "ZOOM",
      label = paste0("Zoom to ", names(leaf()$df)[1], ":"),
      choices = c("None", leaf()$df[[1]]),
      selectize = TRUE,
      multiple = FALSE
    )
  })
  
  # Zoom when requested
  observe({
    req(input$ZOOM)
    if(input$ZOOM == "None"){
      bbox = leaf()$map$x$limits %>%
        unlist() %>%
        .[c("lng1","lat1","lng2","lat2")] %>%
        unname()
    } else{
      bbox = leaf()$df %>%
        dplyr::slice(which(leaf()$df[[1]] == input$ZOOM)) %>%
        st_bbox() %>%
        unname()
    }
    leafletProxy("MAP") %>% 
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  })
}

# Run the application ==========================================================

shinyApp(ui = ui, server = server)

