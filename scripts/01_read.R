library(readxl)
library(tidyverse)
library(here)

# Specify path

path <- here::here("data", "wand_2.xlsx")

## Overall

# General measurement

general_df <- readxl::read_excel(path = path, range = c("A4:T19"))
head <- readxl::read_excel(path = path, range = c("A4:T4"))
col_names <- names(head)


general_wall_tot <- general_df %>% 
  pivot_longer(-Kalenderwoche) %>% 
  rename(variable = Kalenderwoche, 
         Kalenderwoche = name) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(Kalenderwoche = str_extract(Kalenderwoche, "[0-9]*\\.[0-9]{4}")) %>% 
  separate(Kalenderwoche, c("week", "year"), convert = TRUE) %>% 
  mutate(year = 2018) %>%
  janitor::clean_names() %>% 
  select(-(3:6))


# Air measurement

air_df <- readxl::read_excel(path = path, range = c("A20:T29"))

names(air_df) <- col_names

air_wall_tot <- air_df %>% 
  pivot_longer(-Kalenderwoche) %>% 
  rename(variable = Kalenderwoche, 
         Kalenderwoche = name) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(Kalenderwoche = str_extract(Kalenderwoche, "[0-9]*\\.[0-9]{4}")) %>% 
  separate(Kalenderwoche, c("week", "year"), convert = TRUE) %>% 
  mutate(year = 2018) %>%
  janitor::clean_names()



# Water measurement

water_df <- readxl::read_excel(path = path, range = c("A30:T42"))

names(water_df) <- col_names

water_wall_tot <- water_df %>% 
  pivot_longer(-Kalenderwoche) %>% 
  rename(variable = Kalenderwoche, 
         Kalenderwoche = name) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(Kalenderwoche = str_extract(Kalenderwoche, "[0-9]*\\.[0-9]{4}")) %>% 
  separate(Kalenderwoche, c("week", "year"), convert = TRUE) %>% 
  mutate(year = 2018) %>%
  janitor::clean_names()



# mat

mat_df <- readxl::read_excel(path = path, range = c("A43:T52"))

names(mat_df) <- col_names

mat_wall_tot <- mat_df %>% 
  pivot_longer(-Kalenderwoche) %>% 
  rename(variable = Kalenderwoche, 
         Kalenderwoche = name) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(Kalenderwoche = str_extract(Kalenderwoche, "[0-9]*\\.[0-9]{4}")) %>% 
  separate(Kalenderwoche, c("week", "year"), convert = TRUE) %>% 
  mutate(year = 2018) %>% 
  janitor::clean_names()




## Wall 1:

# General measurement

general_df <- readxl::read_excel(path = path, range = c("A4:T19"))
head <- readxl::read_excel(path = path, range = c("A4:T4"))
col_names <- names(head)


general_wall_1 <- general_df %>% 
  pivot_longer(-Kalenderwoche) %>% 
  rename(variable = Kalenderwoche, 
         Kalenderwoche = name) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(Kalenderwoche = str_extract(Kalenderwoche, "[0-9]*\\.[0-9]{4}")) %>% 
  separate(Kalenderwoche, c("week", "year"), convert = TRUE) %>% 
  mutate(year = 2018) %>% 
  filter(week < 18) %>% 
  janitor::clean_names() %>% 
  select(-(3:6))


# Air measurement

air_df <- readxl::read_excel(path = path, range = c("A20:T29"))

names(air_df) <- col_names

air_wall_1 <- air_df %>% 
  pivot_longer(-Kalenderwoche) %>% 
  rename(variable = Kalenderwoche, 
         Kalenderwoche = name) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(Kalenderwoche = str_extract(Kalenderwoche, "[0-9]*\\.[0-9]{4}")) %>% 
  separate(Kalenderwoche, c("week", "year"), convert = TRUE) %>% 
  mutate(year = 2018) %>% 
  filter(week <18) %>% 
  janitor::clean_names()



# Water measurement

water_df <- readxl::read_excel(path = path, range = c("A30:T42"))

names(water_df) <- col_names

water_wall_1 <- water_df %>% 
  pivot_longer(-Kalenderwoche) %>% 
  rename(variable = Kalenderwoche, 
         Kalenderwoche = name) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(Kalenderwoche = str_extract(Kalenderwoche, "[0-9]*\\.[0-9]{4}")) %>% 
  separate(Kalenderwoche, c("week", "year"), convert = TRUE) %>% 
  mutate(year = 2018) %>% 
  filter(week <18) %>% 
  janitor::clean_names()



# mat

mat_df <- readxl::read_excel(path = path, range = c("A43:T52"))

names(mat_df) <- col_names

mat_wall_1 <- mat_df %>% 
  pivot_longer(-Kalenderwoche) %>% 
  rename(variable = Kalenderwoche, 
         Kalenderwoche = name) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(Kalenderwoche = str_extract(Kalenderwoche, "[0-9]*\\.[0-9]{4}")) %>% 
  separate(Kalenderwoche, c("week", "year"), convert = TRUE) %>% 
  mutate(year = 2018) %>% 
  filter(week <18) %>% 
  janitor::clean_names()

## Wall 2:

# General measurement

general_df <- readxl::read_excel(path = path, range = c("A4:T19"))
head <- readxl::read_excel(path = path, range = c("A4:T4"))
col_names <- names(head)


general_wall_2 <- general_df %>% 
  pivot_longer(-Kalenderwoche) %>% 
  rename(variable = Kalenderwoche, 
         Kalenderwoche = name) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(Kalenderwoche = str_extract(Kalenderwoche, "[0-9]*\\.[0-9]{4}")) %>% 
  separate(Kalenderwoche, c("week", "year"), convert = TRUE) %>% 
  mutate(year = 2018) %>% 
  filter(week >= 18) %>% 
  janitor::clean_names() %>% 
  select(-(3:6))


# Air measurement

air_df <- readxl::read_excel(path = path, range = c("A20:T29"))

names(air_df) <- col_names

air_wall_2 <- air_df %>% 
  pivot_longer(-Kalenderwoche) %>% 
  rename(variable = Kalenderwoche, 
         Kalenderwoche = name) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(Kalenderwoche = str_extract(Kalenderwoche, "[0-9]*\\.[0-9]{4}")) %>% 
  separate(Kalenderwoche, c("week", "year"), convert = TRUE) %>% 
  mutate(year = 2018) %>% 
  filter(week >= 18) %>% 
  janitor::clean_names()



# Water measurement

water_df <- readxl::read_excel(path = path, range = c("A30:T42"))

names(water_df) <- col_names

water_wall_2 <- water_df %>% 
  pivot_longer(-Kalenderwoche) %>% 
  rename(variable = Kalenderwoche, 
         Kalenderwoche = name) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(Kalenderwoche = str_extract(Kalenderwoche, "[0-9]*\\.[0-9]{4}")) %>% 
  separate(Kalenderwoche, c("week", "year"), convert = TRUE) %>% 
  mutate(year = 2018) %>% 
  filter(week >= 18) %>% 
  janitor::clean_names()



# mat

mat_df <- readxl::read_excel(path = path, range = c("A43:T52"))

names(mat_df) <- col_names

mat_wall_2 <- mat_df %>% 
  pivot_longer(-Kalenderwoche) %>% 
  rename(variable = Kalenderwoche, 
         Kalenderwoche = name) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(Kalenderwoche = str_extract(Kalenderwoche, "[0-9]*\\.[0-9]{4}")) %>% 
  separate(Kalenderwoche, c("week", "year"), convert = TRUE) %>% 
  mutate(year = 2018) %>% 
  filter(week >= 18) %>% 
  janitor::clean_names()


write_rds(general_wall_1, here("processed", "general_wall_1.rds"))
write_rds(general_wall_2, here("processed", "general_wall_2.rds"))
write_rds(general_wall_tot, here("processed", "general_wall_tot.rds"))
write_rds(air_wall_1, here("processed", "air_wall_1.rds"))
write_rds(air_wall_2, here("processed", "air_wall_2.rds"))
write_rds(air_wall_tot, here("processed", "air_wall_tot.rds"))
write_rds(water_wall_1, here("processed", "water_wall_1.rds"))
write_rds(water_wall_2, here("processed", "water_wall_2.rds"))
write_rds(water_wall_tot, here("processed", "water_wall_tot.rds"))
write_rds(mat_wall_1, here("processed", "mat_wall_1.rds"))
write_rds(mat_wall_2, here("processed", "mat_wall_2.rds"))
write_rds(mat_wall_tot, here("processed", "mat_wall_tot.rds"))
