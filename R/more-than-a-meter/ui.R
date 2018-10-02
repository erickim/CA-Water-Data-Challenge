nav <- tags$nav
button <- tags$button
ul <- tags$ul
li <- tags$li

source("html_modules.R")

makeUI <- function(choices) {

    fluidPage(

        includeCSS("css/main.css"),

        tags$script(src = "js/scroll.js"),

        nav(
            class = "navbar navbar-default navbar-fixed-top",
            div(
                
                class = "container-fluid",
                
                div(
                    class = "navbar-header",

                    ## mobile nav
                    button(

                        ## element attrs
                        class = "navbar-toggle collapsed",
                        type = "button",
                        `data-toggle` = "collapse",
                        `data-target`="#main-nav",
                        `aria-expanded`="false",

                        ## children
                        span(class="sr-only", "Toggle navigation"),
                        span(class="icon-bar"),
                        span(class="icon-bar"),
                        span(class="icon-bar")
                        
                    ), ## end button.navbar-toggle.collapsed

                    a(class = "navbar-brand", href="#", "More than a Meter")

                ), ## end div.navbar-header

                div(
                    id = "main-nav",
                    class = "collapse navbar-collapse ", 
                    
                    ul(
                        class = "nav navbar-nav",
                        li(
                            a("About", href="#about"),
                            class="active"
                        ),
                        li(
                            a("Explore The Data", href="#data")
                        ),
                        li(
                            a("Get Involved", href="#get-involved")
                        )
                    )
                ) ## end div#main-nav

            ) ## end div.container-fluid
            
        ), ## end nav.navbar.navbar-default.navbar-fixed-top

        div(
            id = "top-banner",
            class = "container-fluid",
            ##top = "51px",
            ##left = 0,
            ##width = "100%",
            
            img(
                src = "images/boy-water-fountain-outside.png", ## source: https://www.choa.org/~/media/images/Childrens/heroes/medical-services/sports-medicine/boy-water-fountain-outside.png
                alt = "A boy drinking water from a water fountain.",
                class = "img-responsive",
                width = "100%",
                height = "auto"
            ),
            div("All children deserve clean drinking water.", id = "banner-overlay"),

            div(
                id = "see-the-data",
                a(
                    href = "#data",
                    h4("See the California Data"),
                    div(
                        class="scroll-down"
                    )
                )
            )
        ),

        three_column_section_block(

            id = "about",

            header_text = "The Water Alarm for Children",

            col1 = div(
                h2(strong("Delivering up-to-date water quality metrics to school staff.")),
                p("Currently, California schools have the option to request that their water provider conduct lead sampling at consumption sites. This system leaves room for gaps in monitoring and hazard communication. Our alarm system provides schools with up-to-date monitoring results and guidance regarding infrastructure-level contaminants, regardless of their enrollment in testing programs. We report many contaminants in addition to lead to demonstrate the extensibility and scalability of our reporting system.")
            ),
            col2 = div(
                h2(strong("Monitoring is just the first step in ensuring safe drinking water.")),
                p("Once contaminants are detected at quantities that may pose a risk to human health, especially to that of the most vulnerable members of society -- children -- changemakers need pointed and actionable opportunities for intervention. Rather than rely on a fire hydrant of information in the form of complex data structures, community leaders including administrators, parents, and local politicians can receive alerts directly in their email inboxes, a media they access every day.")
            ),
            col3 = div(
                h2(strong("Direct information leads to direct participation in local institutions.")),
                p("We hope that with this alert tool, parents will know whether to send their children to school with water bottled from other sources, administrators will know if they ought to invest in filtration devices, and politicians will know when to get involved.")
            )
        ), ## section_block#about 

        full_width_section_block(

            id = "data",

            header_text = "The Data",

            absolutePanel(
                id = "controls",
                class = "panel panel-default",
                height = "auto",
                width = 322,
                top = 1175,
                left = 50,
                selectizeInput(
                    inputId = "school_search",
                    label = "Find my school",
                    choices = c("Enter school name" = "", choices),
                    multiple = TRUE ## allow multiple schools to be selected at once
                ),
                ul(
                    li("Start typing name of school above to get matches."),
                    li("Once selected, the map will zoom into the region of your school."),
                    li("You may select multiple schools at one time."),
                    li("To remove a school, click on it and press backspace."),
                    li("Remove all schools to see all water systems again."),
                    li("Click on blue regions on map to view stats for your school's area.")
                )
                    
            ),

            leafletOutput("ca_map", width="100%", height="600px")

        ),

        section_block(
            id = "get-involved",
            "Get Involved",
            h2(class = "text-center", "Coming soon...")
        )

    ) ## fluidPage
    
}
