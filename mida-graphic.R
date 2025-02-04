library(DeclareDesign)
library(rdss)
library(tidyverse)
library(scales)
library(ggdag)
library(ggraph)
library(ggforce)
library(ggtext)
library(latex2exp)

dag <-
  dagify(amstar ~ mstar + I,
         dstar ~ mstar + D,
         adstar ~ dstar + A,
         m ~ M,
         am ~ m + I,
         d ~ D + m,
         ad ~ A + d)

dag_base <-
  tidy_dagitty(dag) %>%
  select(name, direction, to, circular) %>%
  as_tibble()

nodes_df <-
  tibble(
    name = c(
      "mstar", "amstar", "dstar", "adstar",
      "M", "I", "D", "A",
      "m", "am", "d", "ad"),
    label =
      c("m<sup>*</sup>", "a<sub>m<sup>*</sup></sub>", "d<sup>*</sup>", "a<sub>d<sup>*</sup></sub>",
        "M", "I", "D", "A",
        "m", "a<sub>m</sub>", "d", "a<sub>d</sub>"),
    long_label = c(
      "the real<br>world",
      "estimand: the<br>answer you seek",
      "the dataset<br>you'll get",
      "estimate: the<br>answer you'll get",
      "model: the worlds<br>you consider",
      "inquiry: the<br>question you ask",
      "your data<br>strategy",
      "your answer<br>strategy",
      "an imagined<br>world",
      "a conjectured<br>estimand",
      "a simulated<br>dataset",
      "a simulated<br>estimate"),
    lbl_direction = c("N", "N", "N", "N",
                      "S", "S", "S", "S",
                      "S", "S", "S", "S"),
    x = rep(c(0.5, 1.75, 3.25, 4.5), 3),
    y = rep(c(4, 2.5, 1), each = 4)
  )

endnodes_df <-
  nodes_df %>%
  transmute(to = name, xend = x, yend = y)

gg_df <-
  dag_base %>%
  left_join(nodes_df, by = "name") %>%
  left_join(endnodes_df, by = "to")

gg_df <-
  gg_df %>%
  mutate(arced2 = (name == "mstar" & to == "dstar"),
         arced1 = (name == "m" & to == "d"),
         short = (name %in% c("M", "I", "D", "A") &
                    to %in% c("m", "am", "d", "ad"))) %>%
  arrange(name)

rect_df <-
  tibble(
    xmin = c(0, 2.75),
    xmax = c(2.25, 5.0),
    ymin = c(1.75, 1.75),
    ymax = c(3, 3)
  )

dd_dark_blue <- "#3564ED"
dd_light_blue <- "#72B4F3"
dd_orange <- "#F38672"
dd_purple <- "#7E43B6"
dd_gray <- gray(0.2)
dd_pink <- "#C6227F"
dd_light_gray <- gray(0.8)
dd_dark_blue_alpha <- "#3564EDA0"
dd_light_blue_alpha <- "#72B4F3A0"

g1 <-
  ggplot(data = gg_df) +
  geom_rect(data = rect_df, aes(x = NULL, y = NULL,
                                xend = NULL, yend = NULL,
                                xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = dd_light_blue_alpha,
            alpha = 0.5) +
  aes(x = x,
      y = y,
      xend = xend,
      yend = yend) +
  geom_point(
    data = (. %>% distinct(name, .keep_all = TRUE)),
    color = gray(.1),
    fill = NA,
    size = 14,
    stroke = 0.5,
    pch = 1
  ) +
  geom_dag_edges_arc(
    data = filter(gg_df, arced1),
    curvature = -0.45,
    edge_width = 0.35
  ) +
  geom_dag_edges_arc(
    data = filter(gg_df, arced2),
    curvature = 0.45,
    edge_width = 0.35
  ) +
  geom_dag_edges(data_directed = filter(gg_df, short, !arced1, !arced2),
                 aes(y = y - 0.25),
                 edge_width = 0.35) +
  geom_dag_edges(data_directed = filter(gg_df, !short, !arced1, !arced2),
                 edge_width = 0.35) +
  geom_richtext(
    data = (. %>% distinct(name, .keep_all = TRUE)),
    color = "black",
    fill = "transparent",
    aes(label = label),
    size = 4,
    label.size  = NA) +
  geom_richtext(
    data = (. %>% distinct(name, .keep_all = TRUE)),
    aes(y = y + if_else(lbl_direction == "N", 0.25, -0.25),
        vjust = if_else(lbl_direction == "N", "bottom", "top"),
        label = long_label),
    color = gray(0.2),
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    size = 3) +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = 0:5) +
  annotate("text", x = 1.125, y = 2.8, label = "Theoretical side") +
  annotate("text", x = 3.875, y = 2.8, label = "Empirical side") +
  annotate("text", x = -0.125, y = 1, angle = 90, label = "Simulations") +
  annotate("text", x = -0.125, y = 2.35, angle = 90, label = "Design") +
  annotate("text", x = -0.125, y = 4, angle = 90, label = "Reality") +
  coord_fixed(clip = "off") +
  theme_dag()

ggsave(g1, filename = "static/mida.svg", width = 6.5, height = 5.5)
