section_block <- function(id, header_text, ...) {
    return(
        div(
            id = id,
            class = "container",
            h1(class = "text-center section-header", header_text),
            ...
        )
    )
}

full_width_section_block <- function(id, header_text, ...) {
    return(
        div(
            id = id,
            class = "container-fluid",
            h1(class = "text-center section-header", header_text),
            ...
        )
    )
}

three_column_section_block <- function(id, header_text, col1, col2, col3, ...) {
        
    return(
        div(
            id = id,
            class = "container",
            h1(class = "text-center section-header", header_text),
            div(
                class = "col-xs-4",
                col1
            ),
            div(
                class = "col-xs-4",
                col2
            ),
            div(
                class = "col-xs-4",
                col3
            ),
            ...
        )
    )

}

            
        
