section_block <- function(id, header_text, ...) {
    return(
        div(
            id = id,
            class = "container",
            h2(class = "text-center section-header", header_text),
            ...
        )
    )
}
