require(sf)
require(readr)
require(dplyr)
require(tidyr)
require(purrr)
require(leaflet)

source("helpers.R")
source("ui.R")

water <- read_rds("data/water.rds")

schools <- water %>%
    unnest(schools) %>%
    unite(school_system, School, system_name, sep = ", ", remove=FALSE)

school_choices <- schools %>% pull(school_system) %>% unique

water_summary <- summarize_water_data(water)
st_geometry(water_summary) <- NULL ## extract tibble

water <- water %>%
    left_join(water_summary)

ui <- makeUI(school_choices)

server <- function(input, output) {

    filteredData <- reactive({
        these_schools <- input$school_search

        if (length(these_schools) == 0)
            return(water)
        else {
            these_systems <- schools %>%
                filter(school_system %in% these_schools) %>%
                pull(system_id)
            return(water %>% filter(system_id %in% these_systems))
        }
    })
    
    output$ca_map <- renderLeaflet({

        bbox <- as.vector(st_bbox(water))

        leaflet(water) %>%
            addTiles() %>%
            fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

    })

    observe({

        this_data = filteredData()
        bbox <- as.vector(st_bbox(this_data))

        leafletProxy("ca_map", data = this_data) %>%
            clearShapes() %>%
            addPolygons(
                popup = ~paste0(
                           "<h3>", natural_name, "</h3>",
                           "<p>",
                           "Number of Schools: ", `Number of Schools`,
                           "<br>",
                           "Number of (Dangerous) Samples since 2017: ", `Number of (Dangerous) Samples since 2017`,
                           "<br>",
                           "Proportion of Chemicals over MCL: ", round(`Proportion of Chemicals over MCL`,
                                                                       digits = 2),
                           "<br>",
                           "Highest Percentage over MCL: ", `Highest Percentage over MCL`,
                           "<br>",
                           "Offending Chemical over MCL: ", `Offending Chemical over MCL`,
                           "</p>"
                       )
            ) %>%
            fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

    })
}

shinyApp(ui, server)
