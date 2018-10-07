library(ape)
library(magrittr)
library(igraph)
library(MASS)

# -------- Derive the data ------------
data(biopsy)
# Names taken from: https://cran.r-project.org/web/packages/MASS/MASS.pdf
names(biopsy)<-c(
  "ID",
  "clump thickness.",
  "uniformity of cell size",
  "uniformity of cell shape",
  "marginal adhesion",
  "single epithelial cell size",
  "bare nuclei (16 values are missing)",
  "bland chromatin",
  "normal nucleoli",
  "mitoses",
  "disease status"
)

#' Function to paint a phylogenetic tree by hierarchical clustering
#' 
#' This function plots a phylogenetic tree of the input matrix.
#' The tree groups are colored by calculated clusters. The label
#' input can be used to color the nodes by true classes the inputs
#' have.
#' 
#' @param x \code{data.frame} A data frame containing only numerics to
#'          be clustered for the rows
#' @param method \code{character} The name of clustering method for \link{hclust}
#' 
#' @param nc \code{numeric} The number of clusters to be evaluated (cutting of dendrogram)
#' 
#' @param labels \code{character} The names of the nodes inside the phylogenetic tree
#' 
#' @param color_func \code{character} The name of a function that takes the arguments
#'                   n = number of items and alpha (0-1) that returns a character
#'                   vector of HEX colors
#' 
#' @return An igraph plot
#' 
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
phyltree <- function(x= data.frame(), method="ward.D2", nc = 2, labels=c(), color_func = "viridis"){
	
	# --------- perform clustering ----------------
	clustered_patients <- list()
	# Clustering via Hierarchical Clustering
	clust <- hclust(dist(x), method = method)
 	clustered_patients$clust = clust # clustering cluster_resultput of hclust
	clustered_patients$cluster_assigment = cutree(clust, k = nc) #cluster assignement
	
	# Plot data as phylogenetic tree
	list_clust <- function(ordered_cluster_assign){
		a = unique(ordered_cluster_assign)
		a = sort(a[!is.na(a)])
		g1 = list()
		for(i in a){
			g1[[i]] = which(ordered_cluster_assign==a[i])
		}
		return(g1)
	}
	
	phylogenetic_tree <- ape::as.phylo(clustered_patients$clust,use.labels=F) %>% 
			ape::makeNodeLabel()
	phylogenetic_graph <- igraph::graph.edgelist(phylogenetic_tree$edge, directed = F)
	
	tree_data <- list(
			nr_patients = length(phylogenetic_tree$tip.label),
			nr_splits = length(phylogenetic_tree$node.label)
	)
	
	dd = data.frame(
			names_vert = as.character(1:(tree_data$nr_patients + tree_data$nr_splits)),
			col_vert = as.character(c(
							# Colors for clusters
							do.call(color_func,args=list(nc+1))[clustered_patients$cluster_assigment],
							# black for phylogenetic splits
							rep("black",tree_data$nr_splits))),
			size_vert = c(
					# Vertices size in graph is 15 for patients
					rep(15,tree_data$nr_patients),
					# and 1 for splits
					rep(1,tree_data$nr_splits)),
			lab_vert = c(
					# Label by benign / malignant
					labels,
					# NA label for splits
					rep(NA,tree_data$nr_splits)
			),
			cluster_assignment = c(
					# Cluster assignment value
					clustered_patients$cluster_assigment,
					# NA for phylogenetic splits
					rep(NA,tree_data$nr_splits)),
			color_known_status = c(
					# Colors for status of disease known from
					# biopsy data set
			  do.call(color_func,args=list(nc+1,0.5))[as.numeric(as.factor(labels))],
					# black for phylogenetic splits
					rep("black",tree_data$nr_splits)
			)
	
	)
	
	set.seed(12345)
	igraph::plot.igraph(phylogenetic_graph,
			laycluster_result=layout_nicely,
			mark.groups = list_clust(dd$cluster_assignment),
			mark.col = do.call(color_func,args=list(nc+1,0.4)),
			mark.border = NA,
			vertex.color= as.character(dd$color_known_status),
			vertex.size =  dd[,3],
			vertex.label =  as.character(dd[,4]),
			vertex.label.color = "black",
			vertex.label.dist = 0.5)
}

author_notes <- function(){
	
	readLines("www/author_notes.html")
	
}
