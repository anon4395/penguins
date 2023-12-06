#Function to rename columns, and remove columns not needed
clean_column_names <- function(penguins_data) {
  penguins_data %>%
    select(-starts_with("Delta")) %>%
    select(-Comments) %>%
    clean_names()
}

#Function to remove empty columns or rows
remove_empty_cols_rows <- function(penguins_data) {
  penguins_data %>% 
    remove_empty(c("rows","cols"))
}

#Function to shorten species name to first word only e.g. 'Adelie' 'Chinstrap' 'Gentoo'
shorten_species <- function(penguins_data) {
  penguins_data %>%
    mutate(species = word(species, 1, sep = " "))
}

#Function to shorten sex to first letter only ('M' or 'F')
shorten_sex <- function(penguins_data) {
  penguins_data %>%
    mutate(sex = substring(sex, 1, 1))
}

#Function to subset based on columns 
subset_cols <- function(penguins_data, column_names) {
  penguins_data %>%
    select(all_of(column_names))
}

#A function to remove rows with NA values
remove_NA <- function(penguins_data) {
  penguins_data %>%
    na.omit
}

#A function to filter by: 
   #species
filter_by_species <- function(penguins_data, selected_species) {
     penguins_data %>%
         filter(species == selected_species)
}

  #sex
filter_by_sex <- function(penguins_data, selected_sex) {
  penguins_data %>%
    filter(sex == selected_sex)
}

  #island
filter_by_island <- function(penguins_data, selected_island) {
  penguins_data %>%
    filter(island == selected_island)
}




