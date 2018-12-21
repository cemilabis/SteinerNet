check_input <- function (type, terminals, glist) {
        
        g <- glist[[1]]
        g <- as.undirected(g)
        
        # Checking terminals
        
        if (is.null(terminals) || is.na(terminals) || length(terminals) == 0)
                stop("Error: Terminals not found")
        
        # Checking graph
        
        if (is.null(g))
                stop("Error: The graph object is Null.")
        
        if (length(V(g)) == 0 )
                stop("Error: The graph doesn't contain vertices.")
        
        if (is.null(V(g)$name)) {
                # creating name attribute
                V(g)$name <- as.character(1:length(V(g)))
                attr_flag <- FALSE
        } else {
                # creating new name and realname attributes
                V(g)$realname <- V(g)$name
                V(g)$name     <- as.character(1:length(V(g)))
                attr_flag <- TRUE
        }
        
        # Mathcing names of vertices and terminals, if possible
        
        if (class(terminals) == "character") {
                # terminals contain realname of vertices
                if (sum(terminals %in% V(g)$realname) != length(terminals)) {
                        stop("Error: vertices names do not contain terminal names")
                } else {
                        # Convert realnames of terminals to names (character id's)
                        terminals <- V(g)$name[match(terminals, V(g)$realname)]
                }
        } else if (class(terminals) == "numeric" | class(terminals) == "integer") {
                # terminals contains id's of vertices
                terminals <- V(g)$name[terminals]
        } else
                print("Error: invalid type of terminals")
        
        V(g)$color            <- "yellow"
        V(g)[terminals]$color <- "red"
        
        # Checking type
        
        if ( !(type == "SPM" | type == "EXA" | type == "SP" | type == "RSP" | type == "KB" | type == "ASP") )
                stop("Error: the input type is not correct. Choose one from SPM, EXA, SP, RSP or KB.")
        
        varlist      <- c()
        varlist[[1]] <- g
        varlist[[2]] <- terminals
        varlist[[3]] <- attr_flag
        
        return(varlist)
}
