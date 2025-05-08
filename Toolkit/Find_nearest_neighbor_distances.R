library(sf)
library(mapview)

# Load tree data
Tree_API <- "https://services1.arcgis.com/qAo1OsXi67t7XgmS/arcgis/rest/services/Tree_Inventory/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
trees <- st_read(Tree_API)

# Filter for only active trees
trees <- trees[trees$STATUS == "ACTIVE",]

# Filter to smaller areas for the example
trees_sel <- trees[trees$SUB_WATERSHED == "UPPER SCHNEIDER CREEK", ][1:100,]

mapview(trees_sel)

# Find distances between trees
tree_distance <- st_distance(trees_sel, trees_sel)

# Remove 0.00 m (i.e. the current tree)
diag(tree_distance) <- NA

# Find distance to nearest trees
nearest_distances <- apply(tree_distance, 1, min, na.rm = TRUE)

# Add to table
trees_sel$nn_dist_m <- round(nearest_distances,2)

mapview(trees_sel, zcol = "nn_dist_m")
