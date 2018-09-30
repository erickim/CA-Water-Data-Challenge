require(shiny)

nav <- tags$nav
button <- tags$button
ul <- tags$ul
li <- tags$li

ui <- fluidPage(

    includeCSS("css/main.css"),

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
                        a("About", href="#"),
                        class="active"
                    )
                )
            ) ## end div#main-nav

        ) ## end div.container-fluid
    
    ),

    div(
        id = "top-banner",
        class = "container-fluid",
        ##top = "51px",
        ##left = 0,
        ##width = "100%",
        
        img(
            ## source: https://www.choa.org/~/media/images/Childrens/heroes/medical-services/sports-medicine/boy-water-fountain-outside.png
            src = "images/boy-water-fountain-outside.png",
            alt = "A boy drinking water from a water fountain.",
            class = "img-responsive",
            width = "100%",
            height = "auto"
        ),
        div("All children deserve clean drinking water", id = "banner-overlay")
    ),

    div(
        class = "container",

        h2(class = "text-center section-header", "The Problem"),
        div(
            class = "col-xs-4 text-center",
            img(class = "img-circle", src = "https://via.placeholder.com/140x140"),
            p("Part 1")
        ),
        div(
            class = "col-xs-4 text-center",
            img(class = "img-circle", src = "https://via.placeholder.com/140x140"),
            p("Part 2")
        ),
        div(
            class = "col-xs-4 text-center",
            img(class = "img-circle", src = "https://via.placeholder.com/140x140"),
            p("Part 3")
        ),
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum ut nibh mollis, condimentum ligula sed, venenatis dolor. Phasellus non purus in ex tristique tempor non a odio. Vivamus lobortis tincidunt nibh. Curabitur elit est, tempus in ante at, egestas commodo augue. Maecenas finibus, magna ut suscipit convallis, nunc elit posuere ligula, a venenatis nisl ante nec augue. Aenean consectetur euismod semper. Proin euismod euismod massa. Nulla facilisi. Morbi semper odio lectus, ut lacinia tellus finibus blandit. Aenean eget felis tristique, aliquam lacus eu, semper neque. Vivamus non turpis non lectus iaculis congue. Nunc rutrum ut purus nec faucibus. Cras sed nisl dolor.")
        
    ),

    div(
        class = "container",

        h2(class = "text-center section-header", "About the Water Alarm for Children"),
        div(
            class = "section-copy",
            p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum ut nibh mollis, condimentum ligula sed, venenatis dolor. Phasellus non purus in ex tristique tempor non a odio. Vivamus lobortis tincidunt nibh. Curabitur elit est, tempus in ante at, egestas commodo augue. Maecenas finibus, magna ut suscipit convallis, nunc elit posuere ligula, a venenatis nisl ante nec augue. Aenean consectetur euismod semper. Proin euismod euismod massa. Nulla facilisi. Morbi semper odio lectus, ut lacinia tellus finibus blandit. Aenean eget felis tristique, aliquam lacus eu, semper neque. Vivamus non turpis non lectus iaculis congue. Nunc rutrum ut purus nec faucibus. Cras sed nisl dolor."),
            p("Pellentesque vel velit pulvinar, feugiat turpis non, dictum ante. Vestibulum id interdum ex, vel convallis nunc. Nunc euismod a mi sit amet venenatis. Ut eget sem vitae quam condimentum ultrices. Aliquam in interdum sapien, id ultrices massa. Sed accumsan, neque eu viverra fermentum, nibh nibh scelerisque ex, ut sodales erat felis cursus ex. Donec venenatis pharetra ex ut feugiat. Nam tristique sagittis justo eu pellentesque. Integer placerat felis ut lorem elementum, sed commodo tortor fermentum. Donec volutpat lorem vitae erat lacinia tristique. Nullam sem mauris, vestibulum condimentum lectus blandit, efficitur malesuada libero. Donec ullamcorper consequat velit, sed congue urna vehicula eu.")
        )
    )

    

)

server <- function(input, output) {
    
}

shinyApp(ui, server)
