# Exact algorithm
steinerexact <- function (terminals, glist, color) {
        rwhile <- function (lim) {
                if (get("runloop", envir = en)) {
                        r <- length(V(g)) - lim
                        
                        allcom <- combn(t[1:length(V(g))], r)
                        allmst <- lapply(1:dim(allcom)[2],
                                         function (x)  minimum.spanning.tree(induced_subgraph(g, allcom[ , x])))
                        assign("allmst", allmst, envir = en)
                        
                        edgmst <- lapply(1:dim(allcom)[2],
                                         function (x) get.edgelist(allmst[[x]], names = TRUE))
                        assign("edgmst", edgmst, envir = en)
                        
                        # Check connectivity
                        connectedlist <- lapply(1:dim(allcom)[2], function (x) is.connected(allmst[[x]]))
                        # Check terminals availability
                        withterminals <- lapply(1:dim(allcom)[2], function (x) all(is.element(terminals, V(allmst[[x]])$name)))
                        
                        # Both previous conditions
                        smst <- lapply(1:dim(allcom)[2], function (x) connectedlist[[x]] && withterminals[[x]])
                        
                        assign("runloop",   !is.element(TRUE, unlist(smst)),  envir = en)
                        assign("sol_place", get("sol_place", envir = en) + 1, envir = en)
                }
                return(smst)
        }
        
        g <- glist[[1]]
        t <- V(g)$name
        
        lim <- length(V(g)) - length(terminals)
        
        en <- new.env(hash = TRUE, parent = emptyenv(), size = NA)
        assign("runloop",  TRUE, envir = en)
        assign("sol_place",   0, envir = en)
        smst <- c()
        
        res <- lim:1
        sol <- sapply(res, function (x) rwhile(x))
        
        sol_place <- get("sol_place", envir = en)
        allmst    <- get("allmst",    envir = en)
        edgmst    <- get("edgmst",    envir = en)
        
        # Size of trees
        iter <- length(sol[[sol_place]])
        size <- lapply(1:iter, function (x) length(edgmst[[x]]) / 2)
        midresult <- lapply(1:iter, function (x) size[[x]] * as.integer(sol[[sol_place]][[x]]))
        
        min_len <- min(unlist(midresult)[unlist(midresult) > 0])
        poslist <- which(unlist(midresult) == min_len)
        stgraphlist <- allmst[poslist]
        
        stgraphlist2 <- c()
        
        if (color) {
                green_guys <- lapply(stgraphlist, function (x) V(x)$name)
                green_guys <- unique(unlist(green_guys))
                
                V(g)[green_guys]$color <- "green"
                #V(g)[as.numeric(terminals)]$color <- "red"
                V(g)[terminals]$color  <- "red"
                
                stgraphlist2[[length(stgraphlist2) + 1]] <- g
                stgraphlist2[[length(stgraphlist2) + 1]] <- stgraphlist
                stgraphlist <- stgraphlist2
        }
        
        return(stgraphlist)
}
