# All shortest paths between terminals (ASP)
asp_steiner <- function (optimize, terminals, glist, color) {
        g <- glist[[1]]
        
        paths <- lapply(terminals, function (x) get.all.shortest.paths(g, x, terminals)$res)
        #nodes <- unique(unlist(paths))
        nodes <- unique(names(unlist(paths)))
        
        if (optimize) {
                steinert <- minimum.spanning.tree(induced_subgraph(graph = g, vids = nodes))
                a   <- V(steinert)$color
                b   <- degree(steinert, v = V(steinert), mode = c("all"))
                a1  <- match(a, "yellow")
                b1  <- match(b, "1")
                opt <- sapply(1:length(a1), function (r) a1[r] * b1[r])
                new_g <- delete.vertices(steinert, grep(1, opt))
                steinert <- new_g
        } else
                steinert <- induced_subgraph(graph = g, vids = nodes)
        
        glst <- c()
        
        if (color) {
                V(g)[setdiff(x = nodes, y = terminals)]$color <- "green"
                glst[[length(glst) + 1]] <- g
        }
        
        glst[[length(glst) + 1]] <- steinert
        
        return(glst)
}
