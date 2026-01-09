library(readr)
library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)


get_medici_network <- function(medici_network_csv = "data/medici_network.csv") {
  
  # ---- load data -------------------------------------------------------------
  edges <- read_csv(
    medici_network_csv,
    comment = "#",
    col_types = cols(
      from = col_character(),
      to   = col_character(),
      type = col_character()
    )
  )
  
  # ---- classify --------------------------------------------------------------
  edges <- edges %>%
    mutate(
      # case_when lets us assign different values for the column on the lhs
      domain = case_when(
        type %in% c("personal_loan", "mallevadori", "patronage") ~ "OBLIGATION",
        type == "marriage"                                       ~ "MARRIAGE",
        type %in% c("trade", "friendship")                       ~ "PEER",
      )
    ) %>%
    mutate(
      # add column to indicate whether the relationship is directional
      directed = domain %in% c("OBLIGATION", "MARRIAGE"),
    )
  
  g <- graph_from_data_frame(edges, directed = TRUE)
  print(g)
  print(edges)
  return (list(edges = edges, graph = g))
}

plot_medici_network <- function(medici_network_csv = "data/medici_network.csv",
                                line_width = 1.25, arrow_size = 3, cap_size = 2,
                                edge_colors = c(
                                  OBLIGATION = "#d73027",
                                  MARRIAGE   = "#1a9850",
                                  PEER       = "#7b3294"
                                )) {
  # Load the Medici edge dataframe and graph
  medici_network_pkg <- get_medici_network(medici_network_csv)
  print(names(medici_network_pkg))
  # Extract edges
  edges <- medici_network_pkg$edges
  # Extract graph
  g <- medici_network_pkg$graph
  
  ## PLOTTING
  # 1. Run Fruchterman-Reingold to get initial positions.
  lay <- create_layout(g, layout = "fr")
  
  # 2. Identify different node sets.
  peer_nodes <- edges %>%
    filter(domain == "PEERS") %>%
    pull(to) %>%
    unique()
  obligation_nodes <- edges %>%
    filter(domain == "OBLIGATION") %>%
    pull(to) %>%
    unique()
  marriage_nodes <- edges %>%
    filter(domain == "MARRIAGE") %>%
    pull(to) %>%
    unique()
  
  # 3. Impose some structure for different node types.
  lay <- lay %>%
    mutate(
      
      # obligation nodes below
      y = ifelse(name %in% obligation_nodes, y - 0.5, y),
      
      # marriage nodes get slightly blown out randomly left or right to make room
      x = ifelse(name %in% marriage_nodes, x + 1*rnorm(1), x),
      
      # pin Medici above all
      x = ifelse(name == "Medici", mean(x), x),
      y = ifelse(name == "Medici", max(y) + 2, y),
      
      # peers roughly level with Medici
      y = ifelse(name %in% peer_nodes, max(y) - 0.2, y*rnorm(1,1,0.1))
    )
  
  # Build unit-list that  understands, adding arrows for directed edges
  arrow_lengths <- unit(ifelse(edges$directed, arrow_size, 0), "mm")
  
  p <- 
    # Start by plotting the layout computed above
    ggraph(lay) +
    # Draw edges—"fan" refers to the fact that edges arc for readability
    geom_edge_fan(
      aes(color = domain), arrow = arrow(length = arrow_lengths),
      start_cap = circle(cap_size, "mm"),
      end_cap = circle(cap_size, "mm"),
      width = line_width,
      strength = 3,
      alpha = 0.8) +
    # Customize edge color legend
    scale_edge_color_manual(values = edge_colors, name = "Domain") +
    # Draw nodes
    geom_node_point(size = 3) +
    # Print node labels
    geom_node_text(
      aes(label = name),
      size = 7,
      repel = TRUE,
      vjust = -1,
      hjust = 0.5,
      
    ) +
    # Remove axes or any other plot marks beyond what's spec'd above
    theme_void()
      
  return (p)
}

print(plot_medici_network(medici_network_csv = "posts/identity-coalitions-ergonomics-of-power/data/medici_network.csv"))