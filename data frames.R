source("fc_functions.R")

# data
uncleaned_data <- readxl::read_excel("Selected food.xlsx")

# Data Cleaning 
data <- clean_data(uncleaned_data)

# Beans ========================================================================
beans_df <- data %>% 
  filter(str_detect(item_labels, "Beans")) %>% 
  mutate(commodity = case_when(
    item_labels == "Beans brown,sold loose" ~ "Brown",
    item_labels == "Beans:white black eye. sold loose" ~ "White")) %>% 
  select(-item_labels)

# colors
beans_colr <- c("Brown" = "#a83c09", "White" = "snow3")


# Rice =========================================================================
rice_df <- data %>% 
  filter(str_detect(item_labels, "Rice")) %>% 
  filter(item_labels != "Rice Medium Grained") %>% 
  mutate(commodity = case_when(
    item_labels == "Broken Rice (Ofada)"   ~ "Ofada",
    item_labels == "Rice agric sold loose" ~ "Agric",
    item_labels == "Rice local sold loose" ~ "Local",
    # item_labels == "Rice Medium Grained"   ~ "Medium",
    item_labels == "Rice,imported high quality sold loose" ~ "Imported",
    TRUE ~ item_labels)) %>% 
  select(-item_labels)


# Catfish ======================================================================
catfish_df <- data %>% 
  filter(str_detect(item_labels, "Catfish")) %>% 
  mutate(commodity = case_when(
    item_labels == "Catfish (obokun) fresh" ~ "Fresh",
    item_labels == "Catfish :dried" ~ "Dried",
    item_labels == "Catfish Smoked" ~ "Smoked",
    TRUE ~ item_labels)) %>% 
  select(-item_labels)


# Garri ========================================================================
garri_df <- data %>% 
  filter(str_detect(item_labels, "Gari")) %>%
  mutate(commodity = case_when(
    item_labels == "Gari white,sold loose"  ~ "White",
    item_labels == "Gari yellow,sold loose" ~ "Yellow",
    TRUE ~ item_labels)) %>% 
    select(- item_labels)

# colors
garri_colr <- c("White" = "ivory3", "Yellow" = "#fedf08")


# chicken ======================================================================
chicken_df <- data %>% 
  filter(str_detect(item_labels, "Chicken|chicken")) %>% 
  mutate(commodity = case_when(
    item_labels == "Chicken Feet" ~ "Feet",
    item_labels == "Chicken Wings" ~ "Wings",
    item_labels == "Frozen chicken" ~ "Frozen",
    TRUE ~ item_labels)) %>% 
  select(-item_labels)

# colors 
chicken_colr <- c("Feet" = "#faee66", "Frozen" = "#fef69e", "Wings" = "#fff39a")


# Tuber ========================================================================
tuber_df <- data %>% 
  filter(str_detect(item_labels, "potato|Yam")) %>% 
  mutate(commodity = item_labels) %>% 
  select(-item_labels)

# colors 
tuber_colr <- c("Irish potato" = "goldenrod2", "Sweet potato" = "deeppink1",
                "Yam tuber" = "tan4")


# Cooking oil ==================================================================
cookingOil_df <- data %>% 
  filter(str_detect(item_labels, "oil")) %>% 
  mutate(commodity = case_when(
    item_labels == "Groundnut oil: 1 bottle, specify bottle" ~ "Groundnut",
    item_labels == "Palm oil: 1 bottle,specify bottle" ~ "Palm",
    item_labels == "Vegetable oil:1 bottle,specify bottle" ~ "Vegetable",
    TRUE ~ item_labels)) %>% 
  select(-item_labels)


# Plantain =====================================================================
plantain_df <- data %>% 
  filter(str_detect(item_labels, "Plantain")) %>% 
  mutate(commodity = case_when(
    item_labels == "Plantain(ripe)" ~ "Ripe",
    item_labels == "Plantain(unripe)" ~ "Unripe",
    TRUE ~ item_labels)) %>% 
  select(-item_labels)


# Fish =========================================================================
fish_df <- data %>% 
  filter(str_detect(item_labels, "Tilapia|Titus|Mackerel")) %>% 
  mutate(commodity = case_when(
    item_labels == "Tilapia fish (epiya) fresh" ~ "Tilapia",
    item_labels == "Titus:frozen" ~ "Titus",
    item_labels == "Mackerel : frozen" ~ "Mackerel",
    TRUE ~ item_labels)) %>% 
  select(-item_labels)

# colors 
fish_colr <- c("Mackerel" = "grey79", "Tilapia" = "#acbf69", "Titus" = "grey75")


# Beef =========================================================================
beef_df <- data %>% 
  filter(str_detect(item_labels, "Beef")) %>% 
  mutate(commodity = case_when(
    item_labels == "Beef Bone in"  ~ "With Bone",
    item_labels == "Beef,boneless" ~ "Boneless",
    TRUE ~ item_labels)) %>% 
  select(-item_labels)


# Bread ========================================================================
bread_df <- data %>% 
  filter(str_detect(item_labels, "Bread")) %>%
  mutate(commodity = case_when(
    item_labels == "Bread sliced 500g" ~ "Sliced",
    item_labels == "Bread unsliced 500g" ~ "Unsliced",
    TRUE ~ item_labels)) %>% 
  select(-item_labels)

# color 
bread_colr <- c("Sliced" = "#f7d560", "Unsliced" = "tan1")


# Others =======================================================================
commodities_df <- data %>% 
  filter(str_detect(item_labels, "Onion|Tomato|Wheat")) %>% 
  mutate(commodity = case_when(
    item_labels == "Onion bulb" ~ "Onion",
    item_labels == "Wheat flour: prepacked (golden penny 2kg)" ~ "Wheat Flour",
    TRUE ~ item_labels)) %>% 
  select(-item_labels)


