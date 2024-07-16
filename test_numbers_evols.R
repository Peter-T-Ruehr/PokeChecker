# test with umbers, evolution chains, and tralations sheet

library(shiny)
library(dplyr)
library(googlesheets4)
gs4_deauth()
library(DT)
library(stringr) 
library(data.table)
library(tidyr)

# load google sheets
pokedex_sheet_id <- "https://docs.google.com/spreadsheets/d/1WZWY-zCCki9jD26-v7QQ1CCX85NmiYclPLukeIPHJV4/"

# pokedex
pokedex_df_raw <- read_sheet(pokedex_sheet_id,sheet = "pokedex") 

# evolution chains
evolutions_df <- read_sheet(pokedex_sheet_id,sheet = "evolution_chains") 
# get rid of #
evolutions_df <- evolutions_df %>%
  mutate(across(everything(), ~ gsub("#", "", .)))


# translations
translations_df_wide <- read_sheet(pokedex_sheet_id,sheet = "translations")
# get rid of #
translations_df_wide <- translations_df_wide %>% 
  mutate(dex = gsub("#", "", dex))

colnames(translations_df_wide) <- gsub("name_", "", colnames(translations_df_wide))

translations_df <- as_tibble(melt(setDT(translations_df_wide), 
                                  id.vars = c("dex"), 
                                  variable.name = "language", 
                                  value.name = "name"))


# here: think about how to deal with special forms and fill pokedex_df with entries
# fill alolan, hisuian, galaran , paldean shadow and purified
special_forms <- c("Alola", "Hisui", "Galar", "Paldea")
i=1
form=special_forms[1]
for(i in 1:nrow(pokedex_df_raw)){
  for(form in special_forms){
    if(sum(grepl(form, pokedex_df_raw$name_german))>=1){
      rows_to_edit <- which(grepl(form, pokedex_df_raw$name_german))
      row = 2
      for(row in rows_to_edit){
        pokedex_df_raw$shadow_in_dex[row] <- "x"
        pokedex_df_raw$purified_in_dex[row] <- "x"
      }
    }
  }
}



# clean evolutions_df
evolutions_df_filtered <- evolutions_df %>% 
  # select(dex_A, dex_B, dex_C, dex_mega1, dex_mega2)
  # mutate(across('dex_A', str_replace, '#', ''))
  mutate(across('dex_A':'mega2', \(x) str_replace(x, '#', '')))

curr_language <- "german"

mon_of_interest <- "Glumanda"
dex_of_interest <- translations_df %>% 
  # filter(language == curr_language) %>% 
  filter(grepl(mon_of_interest, name)) %>% 
  pull(dex)

stages <- evolutions_df %>% 
  select(dex_A, dex_B, dex_C, dex_mega1, dex_mega2) %>% 
  filter(dex_A == dex_of_interest |
           dex_B == dex_of_interest |
           dex_C == dex_of_interest) %>% 
  unlist()

# remove NAs from stages
stages <- tibble(dex = unique(stages[!is.na(stages)]))

# get info on catching status
stages_caught <- stages %>% 
  left_join(pokedex_df_raw,# %>% 
            # distinct(),
            # select(-c(name_english, name_german)),
            by = "dex")

# get images from https://serebii.net
i=1
j=5
for(i in 1:nrow(stages_caught)){
  curr_dex <- stages_caught$dex[i]
  for(j in 1:ncol(stages_caught)){
    stages_caught[i,j] <- gsub("^x$", paste0("<img src=", 'https://serebii.net/scarletviolet/pokemon/new/', substring(curr_dex, first = 2), ".png \"",  "height=\"52\"></img>"), stages_caught[i,j])
  }
}

https://serebii.net/scarletviolet/pokemon/new/0001.png
https://serebii.net/scarletviolet/pokemon/new/001.png
# translate stages into table of current name
