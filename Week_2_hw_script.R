
library(dplyr)
library(ggplot2)
library(tidyverse)



#Plot 1: Count of cars per gear
#plot(mtcars$gear)

gear_plot <- mtcars$gear |>
  table() |>
  as.data.frame() |>
  ggplot(aes(x = Var1, y = Freq)) + 
  geom_col(fill = 'gray') + 
  coord_flip() +
  geom_text(aes(label=Freq), hjust = 2, color = 'blue') +
  labs(#title = 'THREE-GEAR CARS ARE DOMINANT',
       subtitle = 'Count of Cars per GEAR',
       x = 'Gear',
       caption = "Source: mtcars data")+
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_line(colour = "black", linewidth = 1, linetype = "solid"))

saveRDS(gear_plot, file = "gear_plot.rds")


#plot 2: Count of cars per carburator

#make categorical/factor

carb_plot <- mtcars$carb |> 
  table() |> 
  as.data.frame() |> 
  ggplot(aes(x = Var1, y = Freq))+
  geom_col(fill = 'yellow')+
  geom_text(aes(label = Freq), vjust = -0.5)+
  labs(#title = 'FEW CARS have SIX or more CARBURATORS',
       subtitle = 'Count of Cars per CARB',
       x = 'CARB',
       caption = "Source: mtcars data") +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour = "black", linewidth = 1, linetype = "solid")
  )


saveRDS(carb_plot, file = "carb_plot.rds")


#Plot 3:Distribution of Cylinder count by carburators and gears

#x-axis count
#y cylinders
#top gears?
#annotated with percentages

#Need to adjust dataframe for plotting
plot_data <- mtcars |> 
  count(cyl, carb, gear) |> 
  mutate(
    pct = n / nrow(mtcars) * 100,
    pct_label = paste0(round(pct, 1), "%")
  ) |> 
  group_by(carb, gear) |> 
  mutate(is_max = n == max(n)) |> 
  ungroup()




cyl_dist_plot <- ggplot(plot_data, aes(x = factor(cyl), y = n)) +
  geom_col(aes(fill = is_max),
           linewidth = 0.3) +
  geom_text(aes(label = pct_label),
            vjust = 0.75,
            size = 2.5) +
  scale_fill_manual(values = c("TRUE" = "yellow", "FALSE" = "yellow"), guide = "none") +
  facet_grid(gear ~ carb,
             labeller = labeller(
               gear = label_both,
               carb = label_both
             )) +
  scale_y_continuous(breaks = 1:5,
                     limits = c(0, 5.5)) +
  labs(
    title = "Distribution of Cylinder Counts by Carburators and Gears",
    x = "cylinders",
    y = "count",
    caption = "Source: mtcars data"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "gray"),
    panel.grid.minor = element_blank()
  )


saveRDS(cyl_dist_plot, file = "cyl_dist_plot.rds")
