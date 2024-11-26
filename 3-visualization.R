# Load necessary packages for plotting
library(ggplot2)
library(ggpubr)
library(wesanderson)
library(grid)
library(patchwork)

# Load cleaned data
ethiopia_df_2 <- read_csv(here("tmp", "ethiopia_df_2.csv"))
ethiopia_annual_2018 <- read_csv(here("tmp", "ethiopia_annual_2018.csv"))
ethiopia_annual_2021 <- read_csv(here("tmp", "ethiopia_annual_2021.csv"))

# Load Ethiopia shape file for plotting
ethiopia_shape_path <- here::here("data", "spatial_data", "gadm41_ETH_0.shp")
ethiopia_shape <- sf::st_read(ethiopia_shape_path)

# Set font and color scheme
my_font_3 <- "Gill Sans"
pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")
Zissou2 <- wesanderson::wes_palette("Zissou1", 10, type = "continuous")

# Plot rainfall maps for 2018
df <- ethiopia_df_2 %>% filter(year == 2018, variable == "chirps")

ggplot() +
  geom_raster(data = df, aes(x = x, y = y, fill = tp)) +
  geom_sf(data = ethiopia_shape,
          fill = NA,
          color = "#B4D6CF") +
  facet_wrap( ~ month) +
  scale_fill_gradientn(na.value = "white",
                       colours = c("#ffffff", pal)) +
  labs(
    title = "CHIRPS Monthly Total Rainfall in Ethiopia",
    subtitle = "2018",
    fill = "Rainfall (mm)",
    x = "",
    y = ""
  ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    theme = theme(
      legend.key.width = unit(0.5, "lines"),
      legend.key.height = unit(5, "lines")
    )
  )) +
  ggpubr::theme_pubclean(base_family = my_font_3, base_size = 14) +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.border = element_rect(colour = "#B4D6CF", fill = NA),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    text = element_text(size = 12, family = my_font_3),
    title = element_text(
      size = 14,
      family = my_font_3,
      face = "bold",
      colour = "#09447D"
    ),
    strip.background = element_rect(fill = alpha("#B4D6CF", 0.5), colour = "#B4D6CF"),
    strip.text = element_text(
      size = 12,
      family = my_font_3,
      colour = "#09447D"
    ),
    legend.text = element_text(size = 10, family = my_font_3)
  )
png(
  "ethiopia_rainfall_2018_test.png",
  width = 14,
  height = 18,
  units = "cm",
  res = 1000
)
print(ggplot2::last_plot())
dev.off()

# Create function for plotting annual rainfall maps for each dataset and year
plot_rainfall <- function(variable,
                          year,
                          distribution = FALSE,
                          save_path = here("figures")) {
  df <- ethiopia_df_2 %>% filter(year == !!year, variable == !!variable)
  
  if (distribution) {
    p <- ggplot(df, aes(x = tp)) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 20,
        fill = "#EACAAE",
        alpha = 0.8
      ) +
      geom_density(lwd = .5,
                   linetype = 1,
                   colour = 2) +
      facet_wrap( ~ month, scales = "free") +
      labs(
        title = paste0(df$dataset, "-Monthly Rainfall Distribution in Ethiopia"),
        subtitle = as.character(year),
        x = "Rainfall (mm)",
        y = "Density"
      ) +
      ggpubr::theme_pubclean(base_family = my_font_3, base_size = 14) +
      theme(
        title = element_text(
          size = 14,
          family = my_font_3,
          face = "bold",
          colour = "#09447D"
        ),
        axis.title = element_text(size = 12, family = my_font_3),
        strip.background = element_rect(fill = alpha("#B4D6CF", 0.5)),
        legend.text = element_text(size = 10, family = my_font_3)
      )
  } else {
    p <- ggplot() +
      geom_raster(data = df, aes(x = x, y = y, fill = tp)) +
      geom_sf(
        data = ethiopia_shape,
        fill = NA,
        linewidth = 0.5,
        color = "#B4D6CF"
      ) +
      facet_wrap( ~ month) +
      scale_fill_gradientn(
        na.value = "white",
        breaks = floor(seq(0, max(df$tp), length.out = 10)),
        guide = guide_legend(keywidth = unit(0.4, "cm")),
        colours = c("#ffffff", Zissou2)
      ) +
      labs(
        title = paste(df$dataset, "-Monthly Total Rainfall in Ethiopia"),
        subtitle = as.character(year),
        fill = "Rainfall (mm)",
        x = "",
        y = ""
      ) +
      ggpubr::theme_pubclean(base_family = my_font_3, base_size = 14) +
      theme(
        legend.position = "right",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 12, family = my_font_3),
        title = element_text(
          size = 14,
          family = my_font_3,
          face = "bold",
          colour = "#09447D"
        ),
        strip.background = element_rect(
          fill = alpha("#B4D6CF", 0.5),
          colour = "#B4D6CF"
        ),
        strip.text = element_text(
          size = 12,
          family = my_font_3,
          colour = "black"
        ),
        panel.border = element_rect(colour = "#B4D6CF", fill = NA),
        legend.text = element_text(size = 10, family = my_font_3)
      )
  }
  
  # Ensure the save_path directory exists
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }
  
  # Save the plot
  plot_filename <- if (distribution) {
    paste0(save_path, "/", variable, "_", year, "_distribution.png")
  } else {
    paste0(save_path, "/", variable, "_", year, ".png")
  }
  
  ggsave(
    filename = plot_filename,
    plot = p,
    width = 8,
    height = 6,
    units = "in",
    dpi = 600
  )
}

# Plot rainfall for all datasets and years
uniq_datasets <- unique(ethiopia_df_2$variable)
uniq_years <- unique(ethiopia_df_2$year)

for (dataset in uniq_datasets) {
  for (year in uniq_years) {
    plot_rainfall(dataset, year)
  }
}

# Plot combined annual rainfall maps for 2018 and 2021
combined_1 <- p_2018 + p_2021

ggsave(
  filename = here("figures", "annual_rainfall_combined.png"),
  plot = combined_1,
  width = 14,
  height = 8,
  dpi = 400
)

# Save PDF version of combined plot
ggplot2::ggsave(
  here("figures", "annual_rainfall_combined.pdf"),
  combined_1,
  width = 14,
  height = 8
)


main_data_with_rainfall <- read_csv(here::here("data", "cleaned_data", "main_data_with_rainfall.csv"))


# box plot of maize yield by year
main_data_with_rainfall %>%
  ggplot(aes(x = year, y = maize_yield)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Year", y = "Maize yield (kilograms per hectare)")

plt_maize_yield <- ggstatsplot::ggbetweenstats(
  data = main_data_with_rainfall,
  x = year,
  y = maize_yield,
  type = "parametric",
  messages = FALSE,
  ylab = "Maize yield (Kilograms per hectare)",
  title = "Maize yield by year"
) +
  theme(plot.subtitle = element_text(size = 8),
        plt.caption = element_text(size = 8))
plt_nitrogen_per_hectare <- ggstatsplot::ggbetweenstats(
  data = main_data_with_rainfall,
  x = year,
  y = nitrogen_per_hectare,
  type = "parametric",
  messages = FALSE,
  ylab = "Nitrogen (Kilograms per hectare )",
  title = "Nitrogen per hectare by year"
) +
  theme(plot.subtitle = element_text(size = 8),
        plt.caption = element_text(size = 8))

# combine the plots into a single plot using patchwork
interest_vars <- plt_maize_yield + plt_nitrogen_per_hectare
interest_vars
ggsave(
  here::here("reports", "interest_vars.png"),
  interest_vars,
  width = 10,
  height = 5,
  units = "in",
  dpi = 300
)






ethiopia_shape_path <- here::here("data", "spatial_data")
ethiopia_shape <- sf::st_read(paste0(ethiopia_shape_path, "/gadm41_ETH_1.json"))



library(sf)
gps_points <- main_data_with_rainfall %>%
  dplyr::select(unique_plot_id, ea_id, year, lat_dd_mod, lon_dd_mod) %>%
  mutate(year = as.factor(year)) %>%
  dplyr::filter(!is.na(lat_dd_mod) & !is.na(lon_dd_mod)) %>%
  # group by unique plot id and create a variable indicating wave , only year 2018, only year 2021, both years
  group_by(unique_plot_id) %>%
  mutate(
    wave = case_when(
      2018 %in% year & 2021 %in% year ~ "2018 & 2021",
      2018 %in% year ~ "2018",
      2021 %in% year ~ "2021",
      TRUE ~ "none"
    )
  ) %>%
  mutate(wave = factor(wave, levels = c("2018", "2021", "2018 & 2021"))) %>%
  st_as_sf(coords = c("lon_dd_mod", "lat_dd_mod"),
           crs = 4326)

# keep only that have data for both 2018 and 2021
gps_points_filtered <- gps_points %>%
  group_by(unique_plot_id) %>%
  filter(n_distinct(year) == 2 & all(c(2018, 2021) %in% year)) %>%
  ungroup()

# plot the points and ethiopia shape for 2018
plt_plot_location_1 <- ggplot2::ggplot() +
  geom_sf(
    data = ethiopia_shape,
    fill = alpha("#0E67BC", 0.1),
    color = "white",
    linewidth = 1
  ) +
  geom_sf(data = gps_points, aes(color = wave), size = 1.3) +
  scale_color_manual(values = c("#14A581", "#09447D", "#D35230")) +
  theme_bw(base_family = my_font, base_size = 15) +
  labs(legend.title = "Panel Wave",
       color = "Panel Wave",
       title = "Plot approximate location by Wave") +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = alpha("#B4D6CF", 0.1), color = "white"),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill = alpha("#B4D6CF", 0.1), color = "black"),
    strip.text = element_text(size = 12, color = "black")
  )
plt_plot_location_1

ggsave(
  here::here("reports", "plot_location_map_1.png"),
  plt_plot_location_1,
  width = 8,
  height = 7,
  units = "in",
  dpi = 300
)

# plot the points and ethiopia shape for 2018
plt_plot_location <- ggplot2::ggplot() +
  geom_sf(
    data = ethiopia_shape,
    fill = alpha("#0E67BC", 0.1),
    color = "white",
    linewidth = 1
  ) +
  geom_sf(data = gps_points, aes(color = wave)) +
  scale_color_manual(values = c("#14A581", "#09447D", "#D35230")) +
  facet_wrap( ~ wave, ncol = 2) +
  theme_bw(base_family = my_font, base_size = 14) +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = alpha("white", 0.1), color = "white"),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill = alpha("white", 0.1), color = "black"),
    strip.text = element_text(size = 12, color = "black"),
    panel.grid = element_blank()
  )
plt_plot_location

ggsave(
  here::here("reports", "plot_location_map_new.png"),
  plt_plot_location,
  width = 7,
  height = 7,
  units = "in",
  dpi = 600
)
