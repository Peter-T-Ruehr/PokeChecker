library(rvest)
library(tidyr)
# library(stringr) # str_split()
library(tidyverse)


headers <- c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36'
)




urls <- tibble(anchor="NA", url="NA", xpath="NA") %>% 
  slice(0)


urls <- urls  %>% 
  add_row(anchor = "Kanto", 
          url = "https://serebii.net/pokemongo/gen1pokemon.shtml",
          xpath = '//*[@id="content"]/main/table[3]') %>%
  add_row(anchor = "Johto", 
          url = "https://serebii.net/pokemongo/gen2pokemon.shtml",
          xpath = '//*[@id="content"]/main/table[3]') %>% 
  add_row(anchor = "Hoenn", 
          url = "https://serebii.net/pokemongo/gen3pokemon.shtml",
          xpath = '//*[@id="content"]/main/table[3]') %>% 
  add_row(anchor = "Sinnoh", 
          url = "https://serebii.net/pokemongo/gen4pokemon.shtml",
          xpath = '//*[@id="content"]/main/table[3]') %>% 
  add_row(anchor = "Unova", 
          url = "https://serebii.net/pokemongo/gen5pokemon.shtml",
          xpath = '//*[@id="content"]/main/table[3]') %>% 
  add_row(anchor = "Kalos", 
          url = "https://serebii.net/pokemongo/gen6pokemon.shtml",
          xpath = '//*[@id="content"]/main/table[3]') %>% 
  add_row(anchor = "Alola", 
          url = "https://serebii.net/pokemongo/gen7pokemon.shtml",
          xpath = '//*[@id="content"]/main/table[3]') %>% 
  add_row(anchor = "Unknown", 
          url = "https://serebii.net/pokemongo/unknownpokemon.shtml",
          xpath = '//*[@id="content"]/main/table[3]') %>% 
  add_row(anchor = "Galar", 
          url = "https://serebii.net/pokemongo/gen8pokemon.shtml",
          xpath = '//*[@id="content"]/main/table[3]') %>% 
  add_row(anchor = "Hisui", 
          url = "https://serebii.net/pokemongo/hisuipokemon.shtml",
          xpath = '//*[@id="content"]/main/table[3]') %>% 
  add_row(anchor = "Paldea", 
          url = "https://serebii.net/pokemongo/gen9pokemon.shtml",
          xpath = '//*[@id="content"]/main/table[3]') %>% 
  add_row(anchor = "Mega", 
          url = "https://serebii.net/pokemongo/megaevolution.shtml",
          xpath = '//*[@id="content"]/main/table[2]')
urls


# create pokedex with english names and numbers
Pokedex <- tibble()

u=1
url <- urls$url[u]
xpath <- urls$xpath[u]
for(u in 1:nrow(urls)){
  url <- urls$url[u]
  xpath <- urls$xpath[u]
  
  response <- httr::GET(url, httr::add_headers(headers))
  
  if (exists("response")) {
    
    anchor <- urls$anchor[u]
    print(paste0(anchor, " Request succeeded logic"))
    
    page <- read_html(httr::content(response, "text"))
    
    table <- page %>%
      html_nodes(xpath = xpath) %>%
      html_table() %>% 
      .[[1]] %>% 
      as_tibble()
    
    # print(table)
    table <- table %>% 
      drop_na()
    
    if(anchor != "Mega"){
      col_names <- c("number", "2","3","name","5","6","7","HP","9","Attack","11",",Defense","13","MaxCP","15","MaxBuddyCP","17","SpecialAttacks")
      
      colnames(table) <- col_names
    } else{
      col_names <- c("number", "2","3","name","5","6","7","HP","9","Attack","11",",Defense","13","MaxCP","15","MaxBuddyCP")
      
      
      colnames(table) <- col_names
      
      table <- table %>% 
        mutate("17" = NA,
               SpecialAttacks = NA)
    }
    
    
    Pokedex <- rbind(Pokedex, table)
    
    # readline("Press Enter to continue.")
    
    
  } else {
    
    print("Failed request handling")
    
  }
  rm(response)
}

Pokedex_reduced <- Pokedex %>% 
  select(number, name) %>% 
  arrange(number) %>% 
  mutate(number = gsub("#", "", number)) %>% 
  rename(name_english = name)
  


# get german names and numbers
url <- 'https://www.bisafans.de/pokedex/listen/numerisch.php'
xpath <- '//*[@id="content"]/section/div/table'

response <- httr::GET(url, httr::add_headers(headers))

if (exists("response")) {
  
  print(paste0("German names", " Request succeeded logic"))
  
  page <- read_html(httr::content(response, "text"))
  
  table_German_raw <- page %>%
    html_nodes(xpath = xpath) %>%
    html_table()
  
  table_German <- table_German_raw %>% 
    .[[1]] %>% 
    as_tibble() %>% 
    rename(number = Nr.) %>% 
    select(-Typen) %>% 
    rename(name_german = Pokémon) %>% 
    mutate(number = sprintf("%04d", number))
}

# fuse English and German
multilingial <- Pokedex_reduced %>% 
  left_join(table_German, by = "number") %>% 
  rename(name_german_raw = name_german) %>% 
  distinct(name_english, .keep_all = TRUE)

# add mega etc to name_german
multilingial$name_german <- NA
i=8
curr_base_name <- "nothing_nothing"

for(i in 1:100){ # nrow(multilingial)
  if(i==1) {
    curr_base_name <- "nothing_nothing"
  }
  
  curr_name_english <- multilingial$name_english[i]
  curr_name_german_raw <- multilingial$name_german_raw[i]
  
  print(paste(i, curr_name_english, "and", curr_name_german_raw))
 
  
  
  if(grepl(curr_base_name, curr_name_english)){
    
    curr_name_german <- gsub(curr_base_name, curr_name_german_raw, curr_name_english)
    multilingial$name_german[i] <- curr_name_german
    
    message(paste(curr_name_german_raw, "becomes", curr_name_german))
    
  } else{
    # print("no")
    curr_base_name <- curr_name_english
    multilingial$name_german[i] <- curr_name_german_raw
  }
  
}

# write csv keeping leading zeros
write.table(multilingial %>% 
              select(-name_german_raw), 
            file = "multilingial.csv", sep = ",", row.names = FALSE, quote = FALSE)
