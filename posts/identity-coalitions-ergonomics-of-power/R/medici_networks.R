library(readr)
library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)

# Needed for system-installed fonts (Fira Sans specif.)
library(showtext)
library(sysfonts)



get_medici_network <- function(medici_network_csv = "data/medici_network.csv") {
  
  # ---- load data -------------------------------------------------------------
  edge_tbl <- read_csv(
    medici_network_csv,
    comment = "#",
    col_types = cols(
      from = col_character(),
      to   = col_character(),
      type = col_character()
    )
  )
  
  # ---- classify --------------------------------------------------------------
  edge_tbl <- edge_tbl %>%
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
  
  g <- graph_from_data_frame(edge_tbl, directed = TRUE)

  return (list(edge_table = edge_tbl, graph = g))
}


# ---- font setup: make sure Fira Sans is installed to reproduce ---------------
if (!"Fira Sans" %in% sysfonts::font_families()) {
  sysfonts::font_add_google("Fira Sans", "Fira Sans")
}
showtext::showtext_auto()


plot_medici_network <- function(medici_network_csv = "data/medici_network.csv",
                                output_save_name = "medici_network.pdf",
                                cap_size = 2, x_scale = 1.0, y_scale = 1.0,
                                name_font_size = 2.35, line_width=0.6, 
                                output_width=2000, output_height = 1200,
                                arrow_size = 1, fan_strength = .8, 
                                output_dim_units = "px", 
                                legend_text_size = 9, legend_position = c(0.88, 0.295),
                                layout_csv = "data/layout.csv",
                                edge_colors = c(
                                  OBLIGATION = "#d73027",
                                  MARRIAGE   = "#1a9850",
                                  PEER       = "#7b3294"
                                )) {
  
  # Load the Medici edge dataframe and graph
  medici_network_pkg <- get_medici_network(medici_network_csv)
  
  # Extract edges
  edges <- medici_network_pkg$edge_table
  
  # Extract graph
  g <- medici_network_pkg$graph
  
  ## PLOTTING
  # 1. Load positions and create layout 
  # First read the file to a tbl 
  coords <- read_csv(
    layout_csv,
    col_types = cols(
      name = col_character(),
      x    = col_double(),
      y    = col_double()
    )
  )
  
  # Assign vertices x and y positions
  V(g)$x <- x_scale * coords$x[match(V(g)$name, coords$name)]
  V(g)$y <- y_scale * coords$y[match(V(g)$name, coords$name)]
  # Stop if we missed any, eg due to missing or misspelled clan coordinates
  stopifnot(!any(is.na(V(g)$x)), !any(is.na(V(g)$y)))

  lay <- create_layout(
    g,
    layout = "manual",
    x = V(g)$x,
    y = V(g)$y
  )

  ## Create layout
  # 2. Identify different node sets.
  peer_nodes <- edges %>%
    filter(domain == "PEER") %>%
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
  
  # 3. Build unit-list that  understands, adding arrows for directed edges
  arrow_lengths <- unit(ifelse(edges$directed, arrow_size, 0), "mm")
  
  # 4. Do the ploting
  p <- 
    ggraph(lay) +
      geom_edge_fan(
        aes(color = domain),
        arrow = arrow(length = arrow_lengths, type="open", angle=18),
        start_cap = rectangle(4.7*name_font_size, 1.75*name_font_size, "mm"),
        end_cap   = rectangle(4.6*name_font_size, 1.75*name_font_size, "mm"),
        width = line_width,
        alpha = 1.0,
        lineend = "butt",
        linejoin = "mitre",
        linemitre=5,
        strength = fan_strength
      ) +
      # geom_node_label(
      geom_node_text(
        aes(label = name),
        size = name_font_size,
        family = "Fira Sans"
      ) +
      scale_edge_color_manual(values = edge_colors, name = "Domain") +
      coord_equal() +
      theme_void() +
      theme(
        legend.position = legend_position, 
        legend.text = element_text(
          size = legend_text_size,
          family = "Fira Sans"
        ),
        legend.title = element_text(
          family = "Fira Sans"
        )
      )
    
  ggsave(
    output_save_name, p, width = output_width, 
    height = output_height, units = output_dim_units
  )
  
  return (p)
}


plot_medici_network(medici_network_csv = "posts/identity-coalitions-ergonomics-of-power/data/medici_network.csv", layout_csv = "posts/identity-coalitions-ergonomics-of-power/data/layout.csv", name_font_size = 2.65, edge_colors = edge_colors, line_width = 0.45, output_width = 7.15, output_height = 4, output_dim_units = "in", x_scale = 1.2, y_scale = 1.2, arrow_size = 1.5)