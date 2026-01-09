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

# MT version
cap_size = 1.25
p <- 
  ggraph(g, layout = "fr") +
  # geom_edge_link(
  geom_edge_fan(
    aes(color = domain), arrow = arrow(length = arrow_lengths),
    start_cap = circle(cap_size, "mm"),
    end_cap = circle(cap_size, "mm"),
    width = 1.25,
    strength = 2,
    alpha = 0.8) +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, size = 7) +
  scale_edge_color_manual(values = edge_colors, name="Domain") +
  theme_void()
    
print(p)
# # ---- asymmetric plot -------------------------------------------------------

# p_asymm <-
#   ggraph(g_asymm, layout = "fr") +
#   geom_edge_link(
#     aes(color = domain),
#     arrow = arrow(length = unit(2, "mm")),
#     end_cap = circle(2, "mm"),
#     alpha = 0.85
#   ) +
#   geom_node_point(size = 3) +
#   geom_node_text(aes(label = name), repel = TRUE, size = 3) +
#   scale_edge_color_manual(values = edge_colors) +
#   ggtitle("Asymmetric Power Relationships") +
#   theme_void()

# # ---- symmetric plot --------------------------------------------------------

# p_symm <-
#   ggraph(g_symm, layout = "fr") +
#   geom_edge_link(
#     aes(color = domain),
#     alpha = 0.6
#   ) +
#   geom_node_point(size = 3) +
#   geom_node_text(aes(label = name), repel = TRUE, size = 3) +
#   scale_edge_color_manual(values = edge_colors) +
#   ggtitle("Symmetric Relationships") +
#   theme_void()

