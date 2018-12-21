# Shortest Path Based Approximation (SP)
steinertree2 <- function (optimize, terminals, glist, color) {
        g <- glist[[1]]
        
        # Pick a terminal randomly and Form a subtree (sub-graph G')
        prob     <- sample(1:length(terminals), 1)
        subtree  <- terminals[[prob]]
        nsubtree <- setdiff(terminals, subtree)
        
        # Proceed until all terminals not in G'
        while ( !all(is.element(terminals, intersect(subtree, terminals))) ) {
                # Compute shortest paths and their lengths between each node in subtree (G') and the remaining nodes
                paths <- lapply(subtree, function (x) get.all.shortest.paths(g, x, nsubtree))
                
                r <- 1:length(paths)
                t <- sapply(r, function (r) sapply(paths[[r]]$res, length))
                
                # Compute a minimum for each set of lengths from each node to other nodes
                if (class(t) == "list" || class(t) == "integer") {
                        r  <- 1:length(t)
                        t2 <- sapply(r, function (r) min(t[[r]]))
                }
                if (class(t) == "matrix") {
                        r  <- 1:dim(t)[2]
                        t2 <- sapply(r, function (r) min(t[, r]))
                }
                
                # Find a path with minimum among minimum length
                t3 <- which(t2 == min(t2))
                
                # Note, graph has to have name attribute, because in found variable we assign names
                # of vertices. It is much more convenient to work with names, not with ids.
                if (length(paths) > 1) {
                        if (class(t) == "list" || class(t) == "integer")
                                t4 <- which(t[[t3[1]]] == min(t[[t3[1]]]))
                        
                        if (class(t) == "matrix")
                                t4 <- which( t[ , t3[1]] == min(t[ , t3[1]]) )
                        
                        #found <- unlist(paths[[t3[1]]][t4][1]$res)
                        found <- names(unlist(paths[[t3[1]]][t4][1]$res))
                } else {
                        #found <- unlist(paths[[1]][t3][1]$res)
                        found <- names(unlist(paths[[1]][t3][1]$res))
                }
                
                # Add all vertices from all shortest paths to subtree
                #subtree  <- union(subtree, V(g)[unique(found)])
                subtree  <- union(subtree, V(g)[unique(found)]$name)
                #nsubtree <- setdiff(nsubtree, V(g)[unique(found)])
                nsubtree <- setdiff(nsubtree, V(g)[unique(found)]$name)
        }
        
        # Perform "optimization": find minimum spanning tree and remove nodes of degree 1
        if (optimize) {
                steinert <- minimum.spanning.tree(induced_subgraph(g, subtree))
                a   <- V(steinert)$color
                b   <- degree(steinert, v = V(steinert), mode = c("all"))
                a1  <- match(a, "yellow")
                b1  <- match(b, "1")
                opt <- sapply(1:length(a1), function (r) a1[r] * b1[r])
                new_g <- delete.vertices(steinert, grep(1, opt))
                steinert <- new_g
        } else
                steinert <- induced_subgraph(g, subtree)
        
        glst <- c()
        if (color) {
                V(g)[subtree]$color   <- "green"
                V(g)[terminals]$color <- "red"
                
                glst[[length(glst) + 1]] <- g
        }
        
        glst[[length(glst) + 1]] <- steinert
        
        return(glst)
}
