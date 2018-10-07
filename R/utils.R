




phyltree <- function(x= data.frame(), method="ward.D2", nc = 2, labels=c()){
	
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
							viridis(nc+1)[clustered_patients$cluster_assigment],
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
					viridis(nc+1,0.5)[as.numeric(as.factor(labels))],
					# black for phylogenetic splits
					rep("black",tree_data$nr_splits)
			)
	
	)
	
	set.seed(12345)
	igraph::plot.igraph(phylogenetic_graph,
			laycluster_result=layout_nicely,
			mark.groups = list_clust(dd$cluster_assignment),
			mark.col = viridis(nc+1, 0.4),
			mark.border = NA,
			vertex.color= as.character(dd$color_known_status),
			vertex.size =  dd[,3],
			vertex.label =  as.character(dd[,4]),
			vertex.label.color = "black",
			vertex.label.dist = 0.5)
}

