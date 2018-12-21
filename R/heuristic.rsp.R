# Randomized all shortest paths approximation (RSP)
appr_steiner <- function (repeattimes, optimize, terminals, glist, color) {
        set <- c()
        g <- glist[[1]]
        
        # Start with the sub-graph G* consisting of all nodes and edges appearing on shortest paths between terminals
        paths <- lapply(terminals, function (x) get.all.shortest.paths(g, x, terminals)$res)
        
        r  <- 1:length(paths)
        t1 <- lapply(r, function (r) length(paths[[r]]))
        
        distances <- lapply(r, function (r) lapply(1:t1[[r]], function(x, y) length(paths[[y]][[x]]), y = r))
        neighbour_distance <- max(unlist(distances))
        
        # Note, graph has to have name attribute, because we assign names of vertices to
        # path variable. It is much more convenient to work with names, not with ids.
        #paths <- unique(unlist(paths))
        paths <- unique(names(unlist(paths)))
        #set   <- V(g)[paths]
        set   <- V(g)[paths]$name
        size  <- length(E(minimum.spanning.tree(induced_subgraph(g, union(terminals, set)))))
        
        i <- 1
        while (i <= repeattimes) {
                #seed_list <- unlist(neighborhood(graph = g, order = neighbour_distance, nodes = terminals, mode = "all"))
                seed_list <- names(unlist(neighborhood(graph = g, order = neighbour_distance, nodes = terminals, mode = "all")))
                seed_list <- seed_list[!(seed_list %in% terminals)]
                seed <- sample(seed_list, 1)
                
                paths2 <- get.all.shortest.paths(g, seed, terminals)
                paths2 <- paths2$res
                
                #seedpaths <- unique(unlist(paths2))
                seedpaths <- unique(names(unlist(paths2)))
                
                set2  <- union(set, V(g)[seedpaths]$name)
                size2 <- length(E(minimum.spanning.tree(induced_subgraph(g, union(terminals, set2)))))
                
                if (size2 < size) {
                        size <- size2
                        set  <- set2
                }
                
                seed  <- sample(set, 1, prob = NULL)
                set2  <- V(g)[setdiff(set, seed)]$name
                size2 <- length(E(minimum.spanning.tree(induced_subgraph(g, union(terminals, set2)))))
                
                if (size2  < size && is.connected(minimum.spanning.tree(induced_subgraph(g, union(terminals, set2))))) {
                        size <- size2
                        set  <- set2
                }
                
                i <- i + 1
        }
        
        # Perform "optimization": find minimum spanning tree and remove nodes of degree 1
        if (optimize) {
                steinert <- minimum.spanning.tree(induced_subgraph(g, union(terminals, set)))
                a     <- V(steinert)$color
                b     <- degree(steinert, v = V(steinert), mode = c("all"))
                a1    <- match(a, "yellow")
                b1    <- match(b, "1")
                opt   <- sapply(1:length(a1), function(r) a1[r] * b1[r])
                new_g <- delete.vertices(steinert, grep(1, opt))
                steinert <- new_g
        } else
                steinert <- induced_subgraph(g, union(terminals, set))
        
        glst <- c()
        if (color) {
                V(g)[setdiff(set, terminals)]$color <- "green"
                glst[[length(glst) + 1]]  <- g
        }
        
        glst[[length(glst) + 1]] <- steinert
        
        return(glst)
}
