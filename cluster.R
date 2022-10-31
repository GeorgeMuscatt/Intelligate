# cluster is a function to perform model-based clustering analysis to identify a sub-population of interest from FCS data
cluster <- function (dataframe, experiment_number, what = c("ellipses", "classification")){
  
  set.seed(123)
  
  # two options for input variable "what" - "ellipses" to output ellipses coordinates or "classification" to output classified events:
  if (is.null(what) || length(what) != 1L || !what %in% c("ellipses", "classification")) {
    stop("\nRequired argument \"what\" must be one of: \"ellipses\" or \"classification\". Intelligate aborted.")
  }
  
  sample_names <- unique(dataframe$names) # retrieve sample names (.fsc file names)
  
  # initialise input variables:
  df <- filter(dataframe, names == sample_names[experiment_number]) # save event measurements for current experiment
  lambda <- 1 # cluster tuning parameter (range [0,1])
  
  cat(sprintf("Intelligate is analysing experiment saved in \"%s\"", sample_names[experiment_number]))
  cat("\n\nFluoresence channels detected in experiment are...\n\n")
  cat(colnames(df)[!colnames(df) == "names"], sep = "\n")
  
  ## 3. QUERY FLUORESCENCE CHANNELS FOR SUB-POPULATION IDENTIFICATION ##
  
  channel_1 <- readline(prompt = "Please specify the first fluorescence channel to detect sub-population of interest:\n")
  
  if (!channel_1 %in% colnames(df)[!colnames(df) == "names"]){
    cat("\nFluoresence channel was inputted incorrectly. Please copy and paste the channel name from the console without spaces.")
    cat("\nFluoresence channels detected in flowset are...\n\n")
    cat(colnames(df)[!colnames(df) == "names"], sep = "\n")
    channel_1 <- readline(prompt = "Please specify the first fluorescence channel to detect sub-population of interest:\n")
    if (!channel_1 %in% colnames(df)[!colnames(df) == "names"]){
      stop("\nFluoresence channel was inputted incorrectly again. Intelligate aborted.")
    }
  }
  
  channel_2 <- readline(prompt = "Please specify the second fluorescence channel to detect sub-population of interest:\n")
  
  if (!channel_2 %in% colnames(df)[!colnames(df) == "names"]){
    cat("\nFluoresence channel was inputted incorrectly. Please copy and paste the channel name from the console without spaces.")
    cat("\nFluoresence channels detected in flowset are...\n\n")
    cat(colnames(df)[!colnames(df) == "names"], sep = "\n")
    channel_2 <- readline(prompt = "Please specify the second fluorescence channel to detect sub-population of interest:\n")
    if (!channel_2 %in% colnames(df)[!colnames(df) == "names"]){
      stop("\nFluoresence channel was inputted incorrectly again. Intelligate aborted.")
    }
  }
  
  cat(sprintf("\nIntelligate will identify clusters from channels %s and %s:", channel_1, channel_2))
  
  # plot the two user-inputted fluorescence channels:
  plot <- 
    ggplot(df, aes_string(x = channel_1, y = channel_2)) +
    geom_point(pch = 21, colour = "black", fill = NA, alpha = 0.2) +
    theme_bw() +
    labs(x = sprintf("log %s", channel_1), 
         y = sprintf("log %s", channel_2))
  print(plot)
  
  answer <- userQuery("\n\nIs the sub-population of interest present across the two channels in this plot?", default = "n")
  
  if (answer == "n"){
    stop("\nIntelligate aborted. Please re-run and select different channels for analysis.")
  }
  
  cat("\nPrimary clustering started...\n")
  
  ## 4. PRIMARY CLUSTERING ## 
  
  # produce a density estimate for each data point, using a Gaussian finite mixture model from Mclust:
  density_estimates <- densityMclust(df[, c(channel_1, channel_2)], plot = FALSE)
  
  # dimension reduction:
  dimension_reduction <- MclustDR(as.Mclust(density_estimates), lambda = lambda)
  
  # join cluster_ids to the parent dataframe:
  dots_cluster <- cbind(tibble("Cluster" = dimension_reduction$classification), dimension_reduction$x)
  
  ## 5. VISUALISE IDENTIFIED CLUSTERS ##
  
  num_clusters <- length(levels(dots_cluster$Cluster)) # record the number of clusters identified
  plots <- list() # create an empty list to store plots
  
  cat("\n\nVisualising identified clusters...please wait...\n")
  # highlight each cluster in a plot, and save in list for visualising:
  plots <- lapply(1:num_clusters, 
                  function(.x) 
                    ggplot(dots_cluster, aes_string(x = channel_1, y = channel_2)) +
                    geom_point(pch = 21, colour = "black", fill = NA, alpha = 0.2) +
                    geom_point(data = filter(dots_cluster, Cluster == .x), colour = "goldenrod", pch = 21, fill = NA) +
                    facet_wrap(~ sprintf("Cluster %s", .x)) +
                    theme_bw() +
                    labs(x = sprintf("log %s", channel_1), 
                         y = sprintf("log %s", channel_2)))
  do.call(grid.arrange, plots) # arrange plots from list
  
  cluster_number <- readline(prompt = "Enter the number of the cluster containing the sub-population of interest:\n")
  
  if (!cluster_number %in% 1:num_clusters){
    cat("\nCluster number is not present. Please re-enter.\n")
    cluster_number <- readline(prompt = "Enter the number of the cluster containing the sub-population of interest:\n")
    if  (!cluster_number %in% 1:num_clusters){
      stop("\nCluster number is not present. Intelligate aborted.")
    }
  }
  
  answer <- userQuery("\n\nWould you like to perform secondary clustering analysis?", default = "n")
  
  if (answer == "y"){
    cat("\nSecondary clustering started...\n")
    
    ## 6. (OPTIONAL) SECONDARY CLUSTERING ## 
    
    # produce a second density estimate for each data point:
    density_estimates2 <- densityMclust(filter(dots_cluster, Cluster == cluster_number)[, c(channel_1, channel_2)], plot = FALSE)
    
    # dimension reduction:
    dimension_reduction2 <- MclustDR(as.Mclust(density_estimates2), lambda = lambda)
    
    # join cluster_ids to the parent dataframe:
    dots_cluster2 <- cbind(tibble("Sub-cluster" = dimension_reduction2$classification), dimension_reduction2$x)
    
    ## 7. (OPTIONAL) VISUALISE IDENTIFIED SUB-CLUSTERS ##
    
    num_clusters <- length(levels(dots_cluster2$`Sub-cluster`)) # record the number of sub-clusters identified
    plots <- list() # create an empty list to store plots
    
    cat("\n\nVisualising identified sub-clusters...please wait...\n")
    # highlight each sub-cluster in a plot, and save in list for visualising:
    plots <- lapply(1:num_clusters, 
                    function(.x) 
                      ggplot(dots_cluster, aes_string(x = channel_1, y = channel_2)) +
                      geom_point(pch = 21, colour = "black", fill = NA, alpha = 0.2) +
                      geom_point(data = filter(dots_cluster2, `Sub-cluster` == .x), colour = "goldenrod", pch = 21, fill = NA) +
                      facet_wrap(~ sprintf("Sub-cluster %s", .x)) +
                      theme_bw() +
                      labs(x = sprintf("log %s", channel_1), 
                           y = sprintf("log %s", channel_2)))
    do.call(grid.arrange, plots) # arrange plots from list
    
    subcluster_number <- readline(prompt = "Enter the number of the sub-cluster containing the sub-population of interest:\n")
    
    if (!subcluster_number %in% 1:num_clusters){
      cat("\nSub-cluster number is not present. Please re-enter.\n")
      subcluster_number <- readline(prompt = "Enter the number of the sub-cluster containing the sub-population of interest:\n")
      if  (!subcluster_number %in% 1:num_clusters){
        stop("\nSub-cluster number is not present. Intelligate aborted.")
      }
    }
    
    ## 8. PLOT SUB-POPULATION IDENTIFICATION ##
    
    # function to access ellipse information for visualisation:
    get.ellipses <- function(channels, mclust_output, probability){
      centers <- mclust_output$parameters$mean[channels, ]
      vars <- mclust_output$parameters$variance$sigma[channels, channels, ]
      plyr::ldply(1:ncol(centers), function(cluster){
        data.frame(ellipse::ellipse(vars[,,cluster], centre = centers[, cluster], 
                                    level = probability), `Cluster` = cluster)
      })
    }
    
    # extract ellipse information for visualisation:
    ellipses <- 
      get.ellipses(channels = c(channel_1, channel_2), mclust_output = density_estimates2, probability = 0.5) %>% 
      filter(`Cluster` == subcluster_number)
    
    cat("\nVisualising sub-cluster identification...")
    # highlight identified sub-cluster and overlay sub-cluster ellipse:
    plot <- 
      ggplot(dots_cluster, aes_string(x = channel_1, y = channel_2)) +
      geom_point(pch = 21, colour = "black", fill = NA, alpha = 0.2) +
      geom_point(data = filter(dots_cluster2, `Sub-cluster` == subcluster_number), colour = "goldenrod", pch = 21, fill = NA) +
      # geom_point(data = filter(dots_cluster, `Cluster` == cluster_number), colour = "goldenrod", pch = 21, fill = NA) +
      geom_path(data = ellipses) +
      theme_bw() +
      labs(x = sprintf("log %s", channel_1), 
           y = sprintf("log %s", channel_2))
    print(plot)
    
    cat(sprintf("\n\nIntelligate has finished analysing experiment saved in \"%s\"\n\n", sample_names[experiment_number]))
    
  }
  
  if (answer == "n"){
    cat("\nSecondary clustering not performed.\n")
  
  ## 8. PLOT SUB-POPULATION IDENTIFICATION ##
  
  # function to access ellipse information for visualisation:
  get.ellipses <- function(channels, mclust_output, probability){
    centers <- mclust_output$parameters$mean[channels, ]
    vars <- mclust_output$parameters$variance$sigma[channels, channels, ]
    plyr::ldply(1:ncol(centers), function(cluster){
      data.frame(ellipse::ellipse(vars[,,cluster], centre = centers[, cluster], 
                                  level = probability), `Cluster` = cluster)
    })
  }
  
  # extract ellipse information for visualisation:
  ellipses <- 
    get.ellipses(channels = c(channel_1, channel_2), mclust_output = density_estimates, probability = 0.5) %>% 
    filter(`Cluster` == cluster_number)
  
  cat("\nVisualising cluster identification...")
  # highlight identified sub-cluster and overlay sub-cluster ellipse:
  plot <- 
    ggplot(dots_cluster, aes_string(x = channel_1, y = channel_2)) +
    geom_point(pch = 21, colour = "black", fill = NA, alpha = 0.2) +
    geom_point(data = filter(dots_cluster, `Cluster` == cluster_number), colour = "goldenrod", pch = 21, fill = NA) +
    geom_path(data = ellipses) +
    theme_bw() +
    labs(x = sprintf("log %s", channel_1), 
         y = sprintf("log %s", channel_2))
  print(plot)
  
  cat(sprintf("\n\nIntelligate has finished analysing experiment saved in \"%s\"\n\n", sample_names[experiment_number]))
  }
  
  if (experiment_number < max(length(unique(dataframe$names)))){
    invisible(readline(prompt = "Press [enter] to continue to the next experiment.\n"))
  }
  
  ## 9. OUTPUT ELLIPSES OR CLASSIFICATION ## 
  
  if (what == "ellipses"){
    return (ellipses)
  } else if (what == "classification" & answer == "y"){
    return (dots_cluster2)
  } else if (what == "classification" & answer == "n"){
    return (dots_cluster)
  }
}
