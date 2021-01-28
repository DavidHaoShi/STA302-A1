
process_image <- function(image_file_name, k_list)
{ 
  # process_image(image_file_name, k_list) 
  #
  # Input:
  # - image_file_name: a PNG or JPEG image.
  # - k_list:          the number of centers in the clustering       
  #
  # Output:
  # - cluster_info:    A list or tibble of information derived from the k_means 
  #                    that will be sufficient to be the input to any other 
  #                    function you write. This should be the only function that  
  #                    computes a clustering. This should include at least:
  #                     ∗ the original output of the kclust calls,
  #                     ∗ the tidied clusters, their associated RGB values and 
  #                       their nearest DMC thread color information.
  #
  # Example:
  #   library(imager)
  #   library(tidyverse) 
  #   library(tidymodels)
  #   library(sp)
  #   library(scales)
  #   library(cowplot)
  #   library(dmc) 
  #   image_file_name = "/Users/david/Desktop/STA314/STA314 A1/test.jpg"
  #   k_list = c(2,3,4,5,6,7)
  #   cluster_info = process_image(image_file_name, k_list)
  
  # load image and show
  im <- imager::load.image(image_file_name)
  plot(im, main="Original Picture")
  
  # k-means clustering
  data <- as.data.frame(im, wide = "c") %>% 
    rename(R = c.1, G = c.2, B = c.3)
  dat = select(data, c(-x,-y))
  kclusts <- tibble(k_list) %>% 
    mutate(kclust = map(k_list, ~kmeans(x = dat , centers = .x, nstart = 4)), 
      glanced = map(kclust, glance),
      tidies = map(kclust, tidy))
  clusterings <- kclusts %>%
    unnest(cols = c(glanced))
  cluster_info <- list(data, clusterings) 
  return(cluster_info)
} 


scree_plot <- function(cluster_info)
{
  # scree_plot(cluster_info) produces and plots a scree plot.
  #
  # Input:
  # - cluster_info:     The output of process_image.
  #
  # Output:
  # - A screeplot that describes the total sum of squares within-cluster as the 
  #   number of clusters increases.
  #
  # Example:
  #   library(imager)
  #   library(tidyverse) 
  #   library(tidymodels)
  #   library(sp)
  #   library(scales)
  #   library(cowplot)
  #   library(dmc) 
  #   image_file_name = "/Users/david/Desktop/STA314/STA314 A1/test.jpg"
  #   k_list = c(2,3,4,5,6,7)
  #   scree_plot(process_image(image_file_name, k_list)[[2]])
  
  withinss_plot <- ggplot(cluster_info, aes(k_list,tot.withinss)) +
    geom_point() + geom_line() + labs(title= "Withinss by value of K",
                                      x = "Number of cluster centers",
                                      y = "Total sum of squares within-cluster")
  nclust = length(cluster_info$k_list)
  ratio = rep(NA, nclust-1)
  for (kk in 2:nclust) 
  {
    ratio[kk-1] = cluster_info$tot.withinss[kk]/cluster_info$tot.withinss[kk-1] 
  }
  plot_data <- data.frame(k = cluster_info$k_list[2:nclust],ratio) 
  ratio_plot <- ggplot(plot_data, aes(x=k, y = ratio)) + geom_point() + 
    geom_line() + labs(title= "Proportion by num of K",
                       x = "Number of cluster centers",
                       y = "Proportion of variance explained")
  withinss_plot
  #plot_grid(withinss_plot, ratio_plot, labels = c("Withinss by num of K", 
                                                  #"Proportion by num of K")) 
}


color_strips <- function(a_cluster_info)
{
  # color_strips(cluster_info) produces color strips with the DMC color closest 
  # to the cluster center color.
  #
  # Input:
  # - cluster_info:      The output of process_image.
  #
  # Output:
  # - color_strips_info: A tibble of information derived from the k_means that 
  #                      will be used from the input to other functions.
  #
  # Example:
  #   library(imager)
  #   library(tidyverse) 
  #   library(tidymodels)
  #   library(sp)
  #   library(scales)
  #   library(cowplot)
  #   library(dmc) 
  #   image_file_name = "/Users/david/Desktop/STA314/STA314 A1/test.jpg"
  #   k_list = c(2,3,4,5,6,7)
  #   cluster_info = process_image(image_file_name, k_list)
  #   k=4
  #   a_cluster_info = color_strips(cluster_info[[2]][k-1,])
  
  # We use the augment() function to augment the initial data with the clusters 
  # in order to do the replacement of cluster center for the four clusters.
  color_strips_info <- augment(a_cluster_info$kclust[[1]], cluster_info[[1]]) %>% 
    rename(cluster = .cluster)
  centres <- a_cluster_info$tidies[[1]] %>%
  # we can add a column to the tidied centers to add the color in a way 
  # that we can use for plots. The rgb function will do this and display the 
  # color as a hex string.
    mutate(col = rgb(R,G,B)) 
  center_color <- map(centres$col, ~dmc(.x, visualize = FALSE, method = "euclidean")) %>% 
    tibble() %>%
    unnest(cols = c(.))
  show_col(center_color$hex)
  color_strips_info <- center_color[array(color_strips_info$cluster),] %>% 
    select(dmc,name,hex) %>%
    cbind(color_strips_info, .)
  return(color_strips_info)
}


make_pattern <- function(cluster_info, k, x_size, black_white = FALSE, 
                         background_colour= NULL)
{
  # make_pattern(cluster_info, k, x_size, black_white = FALSE, background_color= NULL) 
  # plots the pattern.
  #
  # Input:
  # - cluster_info:     The output of process_image.
  # - k:                The chosen cluster size.        
  # - x_size:           The (approximate) total number of possible stitches in 
  #                     the horizontal direction.         
  # - black_white:      (logical) Print the pattern in black and white (TRUE) or 
  #                     color (FALSE,default)
  # - background_color: The color of the background, which should not be 
  #                     stitched in the pattern. (Default is to not have a color)  
  #
  # Output:
  # - A cross-stitch pattern that can be followed, complete with a legend that  
  #   has thread color, and a guide grid.
  #
  # Example:
  #   library(imager)
  #   library(tidyverse) 
  #   library(tidymodels)
  #   library(sp)
  #   library(scales)
  #   library(cowplot)
  #   library(dmc) 
  #   image_file_name = "/Users/david/Desktop/STA314/STA314 A1/test.jpg"
  #   k_list = c(2,3,4,5,6,7)
  #   cluster_info = process_image(image_file_name, k_list)
  #   k=4
  #   a_cluster_info = color_strips(cluster_info[[2]][k-1,])
  #   make_pattern(a_cluster_info, k = 4, x_size = 100, black_white = FALSE, background_colour = NULL)
  
  source("functions.R")
  data_changed = change_resolution(a_cluster_info, x_size=x_size)
  # since our plot produced by ggplot is initially upside down, we use 
  # scale_y_reverse() function to correct.
  ggplot(data_changed, aes(x = x, y = y, color=I(hex), shape=hex)) + geom_point() + 
    scale_y_reverse() + labs(title="Cross-stitch Pattern",
                             x = "Number of stitches",y = "Number of stitches")
  
}



change_resolution <- function(image_df, x_size)
{
  ## change_resolution(image_df, x_size) subsamples an image to produce
  ## a lower resolution image. Any non-coordinate columns in the data
  ## frame are summarized with their most common value in the larger
  ## grid cell.
  ##
  ## Input:
  ## - image_df: A data frame in wide format. The x-coordinate column MUST
  ##             be named 'x' and the y-coordinate column MUST be named 'y'.
  ##             Further columns have no naming restrictions.
  ## - x_size:   The number of cells in the x-direction. The number of cells
  ##             in the vertical direction will be computed to maintain the 
  ##             perspective. There is no guarantee that the exact number
  ##             of cells in the x-direction is x_size
  ##
  ## Output:
  ## - A data frame with the same column names as image_df, but with fewer 
  ##   entries that corresponds to the reduced resolution image.
  ##
  ## Example:
  ##   library(imager)
  ##   library(dplyr)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager') 
  ##   im <- load.image(fpath)
  ##   im_dat<- as.data.frame(im,wide = "c") %>% rename(R = c.1, G = c.2, B = c.3) %>%
  ##            select(x,y,R,G,B)
  ##   agg_image <- change_resolution(im_dat, 50)
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  if(!require(dplyr)) {
    stop("The dplyr packages must be installed. Run install.packages(\"dplyr\") and then try again.")
  }
  
  sp_dat <- image_df 
  gridded(sp_dat) = ~x+y
  
  persp = (gridparameters(sp_dat)$cells.dim[2]/gridparameters(sp_dat)$cells.dim[1])
  y_size = floor(x_size*persp)
  orig_x_size = gridparameters(sp_dat)$cells.dim[1]
  orig_y_size = gridparameters(sp_dat)$cells.dim[2]
  
  x_res = ceiling(orig_x_size/x_size)
  y_res = ceiling(orig_y_size/y_size)
  
  gt = GridTopology(c(0.5,0.5), c(x_res, y_res),
                    c(floor(orig_x_size/x_res), floor(orig_y_size/y_res)))
  SG = SpatialGrid(gt)
  agg = aggregate(sp_dat, SG, function(x) names(which.max(table(x)))[1] )
  agg@grid@cellsize <- c(1,1)
  df <- agg %>% as.data.frame %>% rename(x = s1, y = s2)  %>% select(colnames(image_df))
  
  return(df)
  
}

