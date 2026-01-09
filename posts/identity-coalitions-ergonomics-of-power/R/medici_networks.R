library(readr)
library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)


# ---- load data -------------------------------------------------------------
edges <- read_csv(
  "data/medici_network.csv",
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
    domain = case_when(
      type %in% c("personal_loan", "mallevadori", "patronage") ~ "OBLIGATION",
      type == "marriage"                                       ~ "MARRIAGE",
      type %in% c("trade", "friendship")                       ~ "PEER",
    )
  ) %>%
  mutate(
    directed = domain %in% c("OBLIGATION", "MARRIAGE"),
  )

edge_colors <- c(
  OBLIGATION      = "#d73027",
  MARRIAGE  = "#1a9850",
  PEER = "#7b3294"
)

g <- graph_from_data_frame(edges, directed = TRUE)

DEFAULT_ARROW_SIZE = 3
arrow_lengths <- unit(ifelse(edges$directed, 4, 0), "mm")


# 1. run FR once
lay <- create_layout(g, layout = "fr")

# 2. identify node sets
obligation_nodes <- edges %>%
  filter(domain == "OBLIGATION") %>%
  pull(to) %>%
  unique()

peer_nodes <- edges %>%
  filter(domain == "PEERS") %>%
  pull(to) %>%
  unique()

# 3. impose vertical structure
lay <- lay %>%
  mutate(
    # pin Medici
    x = ifelse(name == "Medici", 0, x),
    y = ifelse(name == "Medici", 2.5, y),

    # obligation nodes below
    y = ifelse(name %in% obligation_nodes, y - 2.5, y),

    # peers roughly level with Medici
    y = ifelse(name %in% peer_nodes, y * 0.2 + 1.2, y)
  )


# MT version
cap_size = 1.25
p <- 
  # ggraph(g, layout = "fr") +
  ggraph(lay) +
  # geom_edge_link(
  geom_edge_fan(
    aes(color = domain), arrow = arrow(length = arrow_lengths),
    start_cap = circle(cap_size, "mm"),
    end_cap = circle(cap_size, "mm"),
    width = 1.25,
    strength = 3,
    alpha = 0.8) +
  geom_node_point(size = 3) +
  geom_node_text(
    aes(label = name),
    size = 7,
    vjust = -1,
    hjust = 0.5
  ) +
  scale_edge_color_manual(values = edge_colors, name="Domain") +
  theme_void()
    
print(p)
