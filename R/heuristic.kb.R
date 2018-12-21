# Minimum spanning tree based approximation (Kruskal's minimum spanning tree algorithm)
steinertree3 <- function (optimize, terminals, glist, color) {
        makesubtrees <- function (x) {
                if ( !is.na(any(match(t3, x))) )
                        #return(union(subtrees[[x]],
                        #	     found[[grep(1, match(t3, x))]][[1]]))
                        return(union(subtrees[[x]],
                                     names(found[[grep(1, match(t3, x))]][[1]])))
                else return(subtrees[[x]])
        }
        
        subtreenum <- c()
        x <- c()
        g <- glist[[1]]
        
        # Make a Streiner Tree from every terminal
        r <- 1:length(terminals)
        subtrees  <- lapply(r, function (r) terminals[[r]])
        terminals <- subtrees
        nsubtrees <- lapply(r, function (r) setdiff(terminals, subtrees[r]))
        
        # Proceed until all terminals won't be added to a subtree
        while (length(subtrees) > 1) {
                # Find shortest paths between different Steiner Trees and compute their lengths
                r     <- 1:length(subtrees)
                #paths <- lapply(r, function (r) lapply(subtrees[[r]],
                #				       function (x, y) get.all.shortest.paths(g, x, y)$res,
                #				       y = nsubtrees[[r]]))
                paths <- lapply(r, function (r) lapply(subtrees[[r]],
                                                       function (x, y) get.all.shortest.paths(g, x, y)$res,
                                                       y = unlist(nsubtrees[[r]])))
                
                r <- 1:length(paths)
                t <- sapply(r, function (r) sapply(paths[[r]][[1]], length))
                
                # Compute a minimum for each set of lengths from each Steiner tree to other trees
                if (class(t) == "list" | class(t) == "integer") {
                        r  <- 1:length(t)
                        t2 <- sapply(r, function (x) min(t[[x]]))
                }
                if (class(t) == "matrix") {
                        r  <- 1:dim(t)[2]
                        t2 <- sapply(r, function (r) min(t[, r]))
                }
                
                # Find a minimum among minimum length and paths corresponding to it
                t3    <- which(t2 == min(t2))
                t3len <- 1:length(t3)
                
                if (length(paths) > 1) {
                        if (class(t) == "list" || class(t) == "integer" )
                                t4 <- lapply(t3len, function (x) which(t[[t3[x]]] == min(t[[t3[x]]])))
                        if (class(t) == "matrix")
                                t4 <- lapply(t3len, function (x) which((t[ , t3[x]]) == min(t[ , t3[x]])))
                        
                        found <- lapply( t3len, function (x) paths[t3[x]][[1]][[1]][t4[[x]][1]] )
                } else {
                        intersect(subtrees[[x]], V(g)[unlist(terminals)])
                        print("Error")
                }
                
                # Merge subgraphs and paths
                subtrees <- lapply(1:length(subtrees), function (x) makesubtrees(x))
                
                # Delete repeated subtrees (presume that length is more than 1)
                i <- 1
                j <- 2
                while (i <= (length(subtrees) - 1)) {
                        j <- i + 1
                        while (j <= length(subtrees)) {
                                if (length(intersect(subtrees[[i]], subtrees[[j]])) > 0) {
                                        subtrees[[i]] <- union(subtrees[[i]], subtrees[[j]])
                                        subtrees <- subtrees[-j]
                                        j <- j - 1
                                }
                                j <- j + 1
                        }
                        i <- i + 1
                }
                nsubtrees <- lapply(1:length(subtrees), function (x) setdiff(terminals, subtrees[[x]]))
        }
        
        # Perform "optimization": find minimum spanning tree and remove nodes of degree 1
        if (optimize) {
                steinert <- minimum.spanning.tree(induced_subgraph(g, subtrees[[1]]))
                a   <- V(steinert)$color
                b   <- degree(steinert, v = V(steinert), mode = c("all"))
                a1  <- match(a, "yellow")
                b1  <- match(b, "1")
                opt <- sapply(1:length(a1), function (r) a1[r] * b1[r] )
                new_g <- delete.vertices(steinert, grep(1, opt))
                steinert <- new_g
        } else
                steinert <- induced_subgraph(g, subtrees[[1]])
        
        glst <- c()
        if (color) {
                V(g)[subtrees[[1]]]$color     <- "green"
                V(g)[unlist(terminals)]$color <- "red"
                #V(g)[terminals]$color <- "red"
                
                glst[[length(glst) + 1]]  <- g
        }
        
        glst[[length(glst) + 1]] <- steinert
        
        return(glst)
}
