# Author: Miguel Eersel ----
# Contact info:
# e-mail: m.eersel@outlook.com
# mobile: +597-864 2328 (Suriname)


# Shiny AIS Ship Dashboard -----
# version: 0.1

# Brief description
# This is a SIMPLE Shiny Dashboard using:
# 1) the {shiny.sematic} from the Fomantic-UI framework
# see: https://fomantic-ui.com/introduction/getting-started.html
#
# 2) using Shiny Modules
# see: https://shiny.rstudio.com/articles/modules.html


# START  -----------------

# 1.0 Loading packages ----
library(shiny)
library(shiny.semantic)


library(tidyverse)

library(leaflet)

library(reactable)
library(reactablefmtr)

library(geodist)

# 1.2 Load Source files ----
source("R/ships_dropdown_module.R")




# 1.3 Loading the data -----
ships_nested_clean_tbl <- read_rds("00_data/ship_nested_clean_tbl.rds")
ships_longest_dist_tbl <- read_rds("00_data/ship_longest_dist_tbl.rds")

# 1.4 Drop down menu item list ----
 ships_dropdown_tbl <-  ships_longest_dist_tbl %>% 
                          select(ship_type, shipname) 

# 1.4.a ddropdown ship type list
   ships_type_lst <-  ships_dropdown_tbl %>% 
                       distinct(ship_type) %>% 
                       arrange(ship_type) %>% 
                       pull(ship_type) 

   


# 2.0 UI of the shiny app ----
ui <- semanticPage(
    title = "AIS Ship Dashboard",
    div(
        class = "ui container grid", style = "margin-top: 5px",
        div(
            class = "row centered", style = "color: #8a8a8a; font-size: 3rem",
            "AIS Ship Dashboard"
        ),
        div(
            class = "four wide column",

# 2.1 Call Dropdown module -----            
            ship_inputs_ui(id = "main_ship_input",data_ui = ships_type_lst), 
            br(),
          ),
        div(
            class = "twelve wide column",
            tabset(
                list(
                    list(
# 2.2 Longest path tab ----                        
                        menu = div("Longest path"),
                        content = div(
# 2.2.1 longest dist info ----                                
                            div(class = "row centered", 
                                
                             segment(textOutput("ship_info_dist")),
                             
                        
                             segment(textOutput("ship_info_hours"))
                             
                            ),
# 2.2.2 UI Leaflet longest path ----                                
                         leaflet::leafletOutput("ship_position"),

                            
                            br(),
                            "*LAT and LON information has been used forpositiong"
                        )
                    ), # end of list TABS"Longest path"

# 2.3 UI Leaflet ship all postions ----                                

                    list(
                        menu = div("All positions"),
                        content = div(
                                 leaflet::leafletOutput("ship_last_positions")
                                )
                        ), # end of list TAB "Last positions"

# 2.4 UI SHIP TYPE statistics ----                                

                    list(
                        menu = div("SHIPTYPE Statistics"),
                        content = div(
                          reactable:::reactableOutput("ship_type_plot")
                        )
                    ) # end of list TAB "Statistics"
                  )
                ) # end of TABSET PANEL
            )
        )
    
) # end of UI block 
   
   
   
# 3.0 SERVER LOGIC of the shiny app ----
   
server <- function(input, output, session) {

    
    
# 3.1 Call Dropdown module  ----        
    selected_ship_inputs <- ship_inputs_srv( 
                                       id = "main_ship_input",
                                       ships_dropdown_tbl
                                       )
    
# 3.2 Create Reactives ----  
    
# 3.2.1 Longest path reactive ----    
    ship_location_react_tbl <-  reactive({
        
        shipname_sel  <-  selected_ship_inputs()$shipname
        ship_type_sel <-  selected_ship_inputs()$ship_type
        
        df <- ships_longest_dist_tbl %>% 
               filter(shipname %in% c(shipname_sel),
                      ship_type %in% c(ship_type_sel)) %>% 
               mutate(traveltime_hours = round(traveltime_hours,2))
        
        df1 <-  df %>% 
                select(shipname:ship_type, 
                       lon = lag_lon, 
                       lat = lag_lat,
                       datetime = lag_datetime,
                       traveltime_hours) %>% 
                mutate(path_marker = "STARTPOINT")
        
        df2 <-  df %>% 
            select(shipname:ship_type, 
                   lon, 
                   lat, 
                   datetime, 
                   traveltime_hours) %>% 
            mutate(path_marker = "ENDPOINT") 
        
        
        df_result <-  bind_rows(df1,df2)
        
        return(df_result)
          
        
    })
    
# 3.2.2 All positions reactive ----
    
    ship_last_positions <-  reactive({
        req(selected_ship_inputs())
        
        shipname_sel  <-  selected_ship_inputs()$shipname
        ship_type_sel <-  selected_ship_inputs()$ship_type
 
# Select the dataframe for the unique shiptype + shipname combination       
        df <- ships_nested_clean_tbl %>% 
            filter(shipname %in% c(shipname_sel),
                   ship_type %in% c(ship_type_sel)) %>% 
            unnest(cols = data) %>% 
            ungroup()
        

        
        df_result <- df %>%
                       arrange(datetime) 
        
        nr_rows <-  df_result$nr_obs[1]
        

        
        df_result <-  df_result %>% 
                     mutate(
                       pos_label = paste0(
                         "Latitude: ", lat, "<br>",
                         "Longitude: ", lon, "<br>",
                         "DateTime: ", format(datetime, "%d %b, %Y %H:%M:%S")
                          ),
                       pos_label = map(pos_label, HTML) # create popup label
                     )
                    
               
       return(df_result)  
    })

# 3.3 SERVER OUTPUTS ----        
    

# 3.3.1  Longest path Info ----    
        output$ship_info_dist <- renderText({
            paste(
                "Longest consecutive travel distance: ",
                ship_location_react_tbl()$consecutive_dist_m[1], " meters.")
            })

        output$ship_info_hours <- renderText({
            paste(
                "Travel time: ", ship_location_react_tbl()$traveltime_hours[1], " hours.")
        })        
        
        
# 3.3.2 Output: Leaflet longest path----
    
output$ship_position <- leaflet::renderLeaflet({
            if (nrow(ship_location_react_tbl() ) == 0) {
                toast("Missing ship data")
                return()
            }
    
    
            leaflet(ship_location_react_tbl(),  
                    options = leafletOptions(attributionControl = FALSE)) %>%
                
                addProviderTiles("CartoDB.PositronNoLabels") %>%
                
                addMarkers(
                    lng = ~lon, 
                    lat = ~lat,
                    popup = ~paste0(
                        "Coordinates: ",path_marker, br(),
                        "Latitude: ", lat, "<br>",
                        "Longitude: ", lon, "<br>",
                        "DateTime: ", format(datetime, "%d %b, %Y %H:%M:%S")
                    )
                ) %>%
                
                addPolylines(
                    lng = ~lon, lat = ~lat, weight = 2, color = "#7f7f7f"
                )
        })

        

# 3.3.3 Output: Leaflet all positions  ----
        
        output$ship_last_positions <- leaflet::renderLeaflet({
            if (nrow(ship_last_positions() ) == 0) {
                toast("Missing ship data")
                return()
            }
# 3.3.3.a Add categorical color pallet ----            
            factpal <- leaflet::colorFactor(topo.colors(20), 
                                            ship_last_positions()$date)
            
            leaflet(ship_last_positions(),  
                    options = leafletOptions(attributionControl = FALSE)) %>%
                
                addProviderTiles("CartoDB.PositronNoLabels") %>%
                
                addCircles(
                    lng = ~lon, 
                    lat = ~lat,
                    radius = 40,
                    label = ~pos_label,
                    color = ~factpal(date)
                  ) 
        })                

# 3.3.4  Output: Ship type statistics ----   
                output$ship_type_plot <- reactable::renderReactable({
                    data <- ships_nested_clean_tbl %>% 
                        select(-data) %>% 
                        group_by(ship_type) %>% 
                        summarise(
                            tot_types = n(),
                            tot_obs   = sum(nr_obs)
                        ) %>% 
                        ungroup() %>% 
                        mutate(avg_obs = (tot_obs/tot_types) %>%  round(0))
                    
                    reactable::reactable(data = data,
                                         columns = list(
                                             avg_obs = colDef(align = "left",
                                                              cell = reactablefmtr::data_bars(data,
                                                                                              colors = "red",
                                                                                              number_fmt = scales::comma_format(accuracy = 1))),
                                             tot_types = colDef(align = "left",
                                                                cell = reactablefmtr::data_bars(data,
                                                                                                colors = "darkgreen",
                                                                                                number_fmt = scales::comma_format(accuracy = 1))),
                                             
                                             tot_obs = colDef(align = "left",
                                                              cell = reactablefmtr::data_bars(data, 
                                                                                              colors = "dodgerblue",
                                                                                              number_fmt = scales::comma_format(accuracy = 1)))
                                         ),
                                         sortable = TRUE
                    ) 
                    
                })
                    
        
        
} # end of server function ----

# 4.0 Running the shiny app ----
shinyApp(ui, server)