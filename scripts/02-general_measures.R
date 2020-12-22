library(tidyverse)
library(bernr)
library(here)
library(patchwork)
library(paletteer)

theme_set(theme_light(base_family = "Roboto"))

# Read data ---------------------------------------------------------------

general_1 <- pro_read("general_wall_tot.rds")
air_1 <- pro_read("air_wall_tot.rds")
mat_1 <- pro_read("mat_wall_tot.rds")
water_1 <- pro_read("water_wall_tot.rds")


# Environment -------------------------------------------------------------


pm_plot <- general_1 %>% 
  select(week, pm_10_mg_m3_avg_probe_1, pm_10_mg_m3_avg_probe_2, pm_10_mg_m3_avg_kontrollwert) %>% 
  pivot_longer(-week) %>%
  mutate(week = week - 6) %>% 
  mutate(value = parse_number(value)) %>% 
  mutate(name = case_when(str_detect(name, "probe_1") ~ "sample 1",
                          str_detect(name, "probe_2") ~ "sample 2", 
                          str_detect(name, "kontroll") ~ "control")) %>% 
  mutate(name = factor(name, levels = c("sample 1", "sample 2", "control"))) %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(x = week, y = value, shape = name, color = name)) + 
  geom_point(size = 2) + 
  geom_line() + 
  labs(x = "", y = "Particulate Matter 10 (mcg/m3)", 
       color = NULL, shape = NULL) + 
  scale_y_continuous(breaks = seq(0, 2000, 250)) + 
  scale_x_continuous(breaks = seq(-2, 16, 2), 
                     limits = c(-2, 17)) + 
  scale_color_paletteer_d("ggsci::default_nejm")

temp_plot <- general_1 %>% 
  select(week, temperatur_draussen_c) %>% 
  mutate(temp = parse_number(temperatur_draussen_c), 
         week = week - 6) %>% 
  filter(!is.na(temp)) %>% 
  ggplot(aes(x = week, y = temp)) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(-2, 16, 2), limits = c(-2, 16)) + 
  labs(x = "", y = "Outside temperature (°C)")

co2_plot <- general_1 %>% 
  select(week, co2_ppm) %>% 
  mutate(co2= parse_number(co2_ppm, na = "-"), 
         week = week - 6) %>% 
  filter(!is.na(co2)) %>% 
  ggplot(aes(x = week, y = co2)) + 
  geom_line(color = "grey40") + 
  scale_x_continuous(breaks = seq(-2, 16, 2), limits = c(-2, 16)) + 
  labs(x = "Weeks", y = "CO2 (ppm)") + 
  scale_y_continuous(breaks = seq(300, 700, 100))


environment <- pm_plot / temp_plot / co2_plot + 
  plot_layout(height = c(3, 1, 1)) & 
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank()) &
  geom_vline(xintercept = 0, color = "grey50", linetype = 2) & 
  geom_vline(xintercept = 14, color = "grey50", linetype = 3) &
  plot_annotation(title = "Measurements of the environment",
                  caption = "**Dashed line**: installation of first wall<br>
                  **Dotted line**: installation of an adapted second wall") & 
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot", 
        plot.title = element_text(face = "bold"),
        plot.caption = ggtext::element_markdown(lineheight = 1.5, hjust = 0.5))


environment

ggsave("graphs/02-environment_plot.png", dpi = 300, width = 6, height = 10)


# Air ---------------------------------------------------------------------



gesamt_plot <- air_1 %>% 
  select(week, starts_with("gesamtkeimzahl")) %>% 
  pivot_longer(-week) %>%
  mutate(week = week - 6) %>% 
  mutate(value = parse_number(str_extract(value, "[0-9]+"))) %>% 
  mutate(name = case_when(str_detect(name, "probe_1") ~ "sample 1",
                          str_detect(name, "probe_2") ~ "sample 2", 
                          str_detect(name, "kontroll") ~ "control")) %>% 
  mutate(name = factor(name, levels = c("sample 1", "sample 2", "control"))) %>%
  filter(!is.na(value)) %>% 
  ggplot(aes(x = week, y = value)) + 
  geom_line(aes(group = name, color = name)) + 
  labs(x = "", y = "Gesamtkeimzahl (CFU/m3)", 
       color = NULL) + 
  scale_x_continuous(breaks = seq(-2, 16, 2), limits = c(-2, 17)) + 
  scale_color_paletteer_d("ggsci::default_nejm") +
  coord_cartesian(ylim = c(0, 550), clip = "off") 

yeast_plot <- air_1 %>% 
  select(week, starts_with("hefen")) %>% 
  pivot_longer(-week) %>%
  mutate(week = week - 6) %>% 
  mutate(value = parse_number(str_extract(value, "[0-9]+"))) %>% 
  mutate(name = case_when(str_detect(name, "probe_1") ~ "sample 1",
                          str_detect(name, "probe_2") ~ "sample 2", 
                          str_detect(name, "kontroll") ~ "control")) %>% 
  mutate(name = factor(name, levels = c("sample 1", "sample 2", "control"))) %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(x = week, y = value)) + 
  geom_line(aes(group = name, color = name)) + 
  labs(x = "", y = "Fungi (CFU)", 
       color = NULL) + 
  scale_x_continuous(breaks = seq(-2, 16, 2), limits = c(-2, 17)) + 
  scale_color_paletteer_d("ggsci::default_nejm")

aspergillus_plot <- air_1 %>% 
  select(week, starts_with("aspergillus")) %>% 
  pivot_longer(-week) %>%
  mutate(week = week - 6) %>% 
  mutate(value2 = if_else(value == "nn", NA_character_, "Aspergillus spp.")) %>% 
  mutate(name = case_when(str_detect(name, "probe_1") ~ "sample 1",
                          str_detect(name, "probe_2") ~ "sample 2", 
                          str_detect(name, "kontroll") ~ "control")) %>% 
  mutate(name = factor(name, levels = c("sample 1", "sample 2", "control"))) %>% 
  filter(!is.na(value2)) %>% 
  ggplot(aes(x = week, y = value2)) + 
  geom_point(aes(color = name)) + 
  labs(x = "Weeks", y = "Aspergillus spp.", 
       color = NULL) + 
  scale_x_continuous(breaks = seq(-2, 16, 2), limits = c(-2, 17)) + 
  scale_color_paletteer_d("ggsci::default_nejm") +
  theme(plot.margin = margin(t = 4, l = 4, r = 4, b = 4, unit = "mm"),
        axis.text.y = element_blank())

air <- gesamt_plot / yeast_plot / aspergillus_plot + 
  plot_layout(height = c(3, 3, 1)) & 
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank()) &
  geom_vline(xintercept = 0, color = "grey50", linetype = 2) & 
  geom_vline(xintercept = 14, color = "grey50", linetype = 3) &
  plot_annotation(title = "Measurements of the surrounding air",
                  caption = "**Dashed line**: installation of first wall<br>
                  **Dotted line**: installation of an adapted second wall") & 
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot", 
        plot.title = element_text(face = "bold"),
        plot.caption = ggtext::element_markdown(lineheight = 1.5, hjust = 0.5))
air
ggsave("graphs/02-air_plot.png", dpi = 300, width = 6, height = 10)

# Mat measurements --------------------------------------------------------

asp_niger <- mat_1 %>% 
  select(week, contains("aspergillus")) %>% 
  pivot_longer(-week) %>% 
  mutate(value = na_if(value, "nn")) %>% 
  drop_na() %>% 
  mutate(name = case_when(str_detect(name, "probe_1") ~ "sample 1", 
                          str_detect(name, "probe_2") ~ "sample 2", 
                          str_detect(name, "probe_3") ~ "sample 3"),
         week = week - 6,
         y = if_else(name == "sample 2", 200, 230)) %>% 
  add_row(week = 9, name = "sample 1", value = NA, y = 200)


fungi_mat <- mat_1 %>% 
  select(week, contains("hefen")) %>% 
  drop_na %>% 
  pivot_longer(-week) %>% 
  mutate(value = parse_number(value)) %>% 
  mutate(name = case_when(str_detect(name, "probe_1") ~ "location 1", 
                          str_detect(name, "probe_2") ~ "location 2", 
                          str_detect(name, "probe_3") ~ "location 3"),
         week = week - 6) %>% 
  ggplot(aes(x = week, y = value)) + 
  geom_line(aes(color = name)) +  
  labs(x = NULL, y = "Fungi (CFU/25cm2)", color = NULL) + 
  scale_x_continuous(breaks = seq(-2, 16, 2), limits = c(-2, 17)) + 
  scale_color_paletteer_d("ggsci::default_nejm")

asp_mat <- asp_niger %>% 
  count(value, week, name) %>% 
  mutate(n = if_else(is.na(value), NA_integer_, n)) %>% 
  ggplot(aes(x = week, y = n)) + 
  geom_point(aes(color = name), 
             position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = seq(-2, 16, 2), limits = c(-2, 17)) + 
  scale_color_paletteer_d("ggsci::default_nejm") + 
  scale_y_continuous(breaks = 1) + 
  labs(x = "Weeks", y = "Aspergillus niger", color = NULL) + 
  theme(axis.text.y = element_blank())


mat_plot <- fungi_mat / asp_mat +
  plot_layout(heights = c(3, 1),) &
  geom_vline(xintercept = 0, color = "grey50", linetype = 2) & 
  geom_vline(xintercept = 14, color = "grey50", linetype = 3) &
  plot_annotation(title = "Direct measurements at the wall",
                  caption = "**Dashed line**: installation of first wall<br>
                  **Dotted line**: installation of an adapted second wall") & 
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot", 
        plot.title = element_text(face = "bold"),
        plot.caption = ggtext::element_markdown(lineheight = 1.5, hjust = 0.5))

mat_plot
ggsave("graphs/02-mat_plot.png", dpi = 300, width = 6, height = 10)



# Water measurements ------------------------------------------------------

water <- water_1 %>% 
  select(week, contains("gesamtkeimzahl")) %>% 
  pivot_longer(-week) %>% 
  mutate(name = case_when(str_detect(name, "frisch") ~ "fresh water", 
                          str_detect(name, "tank") ~ "water tank",
                          str_detect(name, "abwasser") ~ "draining water"),
         value = as.numeric(str_extract(value, "[0-9]*")),
         week = week - 6) %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(x = week, y = value, color = name)) + 
  geom_line() + 
  scale_y_log10() + 
  labs(title = "Measurements in water", 
       x = "Weeks", y = "Gesamtkeimzahl (KBE/mL)", 
       color = NULL) + 
  scale_x_continuous(breaks = seq(-2, 16, 2), limits = c(-2, 17)) + 
  scale_color_paletteer_d("ggsci::default_nejm") + 
  facet_wrap(~name, ncol = 1, strip.position = "right") + 
  theme(strip.background = element_blank(), 
        strip.text.y.right = element_text(face = "bold", color = "black", hjust = 0, angle = 0),
        plot.title.position = "plot", 
        plot.title = element_text(face = "bold"),
        legend.position = "none") + 
  geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
  geom_vline(xintercept = 14, color = "grey50", linetype = 3)


water
ggsave("graphs/02-water_plot.png", dpi = 300, width = 6, height = 6)     




pm2 <- pm_plot + 
  geom_point(aes(shape = name, color = name), size = 2) + 
  labs(title = "Environment", shape = NULL, y = "Particulate matter 10 (10mcg/m3)") + 
  theme(plot.title = element_text(face = "bold", margin = margin(b = 1.5, unit = "line"))) + 
  scale_y_continuous(breaks = seq(0, 2000, 500))

gesamt2 <- gesamt_plot + 
  geom_point(aes(shape = name, color = name), size = 2) + 
  labs(y = "Total aerobic bacteria\n(CFU)", 
       title = "Surrounding air", shape = NULL) + 
  theme(plot.title = element_text(face = "bold", margin = margin(b = 1.5, unit = "line")))

fungi2 <- fungi_mat +
  geom_point(aes(shape = name, color = name), size = 2) + 
  labs(title = "Direct measurement on wall", shape = NULL) + 
  theme(plot.title = element_text(face = "bold", margin = margin(b = 1.5, unit = "line")))

yeast2 <- yeast_plot + 
  geom_point(aes(shape = name, color = name), size = 2) + 
  labs(shape = NULL) 

pm2 / gesamt2 / yeast2 / fungi2 + 
  plot_layout(heights = c(2, 1, 1, 2))   & 
  labs(x = "Weeks") & 
  geom_vline(xintercept = 0, color = "grey50", linetype = 2) & 
  geom_vline(xintercept = 14, color = "grey50", linetype = 3) &
  plot_annotation(caption = "**Dashed line**: installation of first wall / **Dotted line**: installation of an adapted second wall") & 
  theme(panel.grid.minor.x = element_blank(), 
  panel.grid.minor.y = element_blank(),
  plot.title = element_text(face = "bold"),
  plot.caption = ggtext::element_markdown(lineheight = 1.5, hjust = 0.5),
  axis.title = element_text(size = 9, lineheight = 1.5))

ggsave(here::here("graphs", "02-graphs_combined.pdf"), height = 10, width = 8, device = cairo_pdf)
ggsave(here::here("graphs", "02-graphs_combined.png"), height = 10, width = 8, dpi = 300)

