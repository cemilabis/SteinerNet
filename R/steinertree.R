####--------------------------------------- Documentation ---------------------------------------####
#' Find Steiner Tree
#' 
#' @description A set of functions for finding Steiner Tree. Includes both exact and heuristic approaches.
#' 
#' @usage steinertree(type, repeattimes = 70, optimize = TRUE, terminals,
#'             graph, color = TRUE, merge = FALSE)
#' 
#' @param type a character scalar, which indicates type of algorithms to perform. Can be
#'             "EXA", "SP", "KB", "RSP", "SPM" or "ASP".
#' @param repeattimes a numeric scalar to specify "RSP" algorithm; number of times the optimization procedure is repeated.
#' @param optimize a logical scalar to specify all algorithms except "EXA"; if TRUE, an optimization of the resultant
#'                 steiner tree is performed, otherwise nothing is done.
#' @param terminals a numeric vector (ids of terminals are passed) or character vector (vertices must have 'name' attribute).
#' @param graph an igraph graph; should be undirected, otherwise it is converted to undirected.
#' @param color a logical scalar; whether to return an original graph with terminals colored in red and
#'              steiner nodes colored in green. Note, if several trees will be found, steiner nodes from all trees
#'              are colored in green.
#' @param merge a logical scalar to specify "EXA" and "SPM" algorithms; if several trees will be found, whether to return
#'              a list with trees or merge them
#' 
#' @return (color = FALSE) Returns a list first element of which is a steiner tree (or a graph of merged trees).
#'         If several steiner trees are found, return a list, each element of which is a steiner tree.
#'          
#'         (color = TRUE) Returns a list, first element of which is a colored original graph and second element is
#'         a steiner tree (or a graph of merged trees) or list of steiner trees.
#'         
#' @details If input graph doesn't have 'name' attribute, one is created. In this case it will contain character ids of vertices.
#'          Also before execution all vertices will be colored in yellow and terminals will be colored in red.
#' 
#' @seealso \code{\link{generate_st_samples}}
#' 
#' @examples
#' steinertree(type = "RSP", optimize = FALSE,
#'             terminals = c(1, 3),
#'             graph = graph("Cubical"),
#'             color = TRUE, merge = FALSE)
#' 
#' @references 1. Path heuristic and Original path heuristic ,Section 4.1.3 of the book "The Steiner tree Problem",
#'                Petter,L,Hammer
#'                
#'             2. "An approximate solution for the Steiner problem in graphs", H Takahashi, A Matsuyama
#'             
#'             3. F K. Hwang, D S. Richards and P Winter, "The steiner tree Problem", Kruskal-Based Heuristic
#'                Section 4.1.4, ISBN: 978-0-444-89098-6
#'                
#'             4. Afshin Sadeghi and Holger Froehlich, "Steiner tree methods for optimal sub-network
#'                identification: an empirical study", BMC Bioinformatics 2013 14:144
#'                
#'             5. F K. Hwang, D S. Richards and P Winter, "The steiner tree Problem", Kruskal-Based Heuristic Section
#'                4.1.4, The Optimal solution for steiner trees on networks, ISBN: 978-0-444-89098-6.
#'             
#' @export
####------------------------------------- End Documentation -------------------------------------####
steinertree <- function (type, repeattimes = 70, optimize = TRUE, terminals, graph, color = TRUE, merge = FALSE) {
    
        glist      <- c()
        glist[[1]] <- graph
    
        varlist <- check_input(type = type, terminals = terminals, glist = glist)
    
        glist[[1]] <- varlist[[1]]
        terminals  <- varlist[[2]]
        attr_flag  <- varlist[[3]]
    
        if (type == "SP")
                result <- steinertree2(optimize = optimize, terminals = terminals, glist = glist, color = color)
    
        if (type == "KB")
                result <- steinertree3(optimize = optimize, terminals = terminals, glist = glist, color = color)
    
        if (type == "RSP")
                result <- appr_steiner(repeattimes = repeattimes, optimize = optimize, terminals = terminals,
                                       glist = glist, color = color)
    
        if (type == "EXA")
                result <- steinerexact(terminals = terminals, glist = glist, color = color)
    
        if (type == "SPM")
                result <- steinertree8(optimize = optimize, terminals = terminals, glist = glist, color = color)
    
        if (type == "ASP")
                result <- asp_steiner(optimize = optimize, terminals = terminals, glist = glist, color = color)
        
        result <- restore_name_attribute(attr_flag, type, result, color)
        
        if (merge & (type == "EXA" | type == "SPM")) {
                if (color) {
                        result[[2]] <- merge_steiner(treelist = result[[2]])
                } else {
                        result <- merge_steiner(treelist = result)
                }
        }
        
        return(result)
}
