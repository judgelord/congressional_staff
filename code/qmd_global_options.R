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
  fig.retina = 6,
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

library(ggplot2)
library(scales)

options(
  ggplot2.continuous.fill = NULL,
  ggplot2.continuous.colour = NULL,
  ggplot2.continuous.color = NULL
)

theme_set(
  theme_minimal() +
    theme(
      palette.colour.continuous = scales::pal_viridis(option = "cividis"),
      palette.fill.continuous   = scales::pal_viridis(option = "cividis"),
      palette.colour.discrete   = scales::pal_viridis(option = "cividis", direction = -1),
      palette.fill.discrete     = scales::pal_viridis(option = "cividis", direction = -1),
      # # FOR AJPS
      # panel.grid = element_blank(),
      # legend.position = "bottom",
      # # make text black, not grey
      # axis.text = element_text(color="black"),
      # axis.ticks = element_line(color = "black"),
      # # add space between labels and text
      # axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
      # axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
      # plot.title = element_text(vjust = 1,
      #                           lineheight = 0,
      #                           margin = margin(0, 0, 0, 0)), # Margins (t, r, b, l)
      # # END FOR AJPS
      panel.border  = element_blank()
    )
)
