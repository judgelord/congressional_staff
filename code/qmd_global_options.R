source(here::here("code", "formatting.R"))

# directory to store model objects
if (!dir.exists(here::here("models"))) {dir.create(here::here("models"))}

# directory to store figures
if (!dir.exists(here::here("figs"))) {dir.create(here::here("figs"))}

knitr::opts_chunk$set(
  echo = T, # code is folded
  fig.width = 7.5,
  fig.height = 3.5,
  split = T,
  fig.align = 'center',
  fig.path='figs/',
  fig.retina = 3,
  out.width = "100%",
  warning = F,
  message = F)


start_time <- Sys.time()

# plot defaults
library(ggplot2); theme_set(
  theme_minimal() +
    theme(
 )
)

# plot defaults
library(ggplot2)

okabe_ito <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#000000"  # black
)

options(
  ggplot2.continuous.fill   = NULL,
  ggplot2.continuous.colour = NULL,
  ggplot2.continuous.color  = NULL
)

theme_set(
  theme_minimal() +
    theme(
      palette.colour.continuous = scales::pal_viridis(option = "cividis"),
      palette.fill.continuous   = scales::pal_viridis(option = "cividis"),
      palette.colour.discrete   = okabe_ito,
      palette.fill.discrete     = okabe_ito
    )
)

