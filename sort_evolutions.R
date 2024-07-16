library(tibble)
# library(readr)

library(stringr)
library(googlesheets4)
gs4_deauth()
library(dplyr)

# load google sheets
sheet_id <- "https://docs.google.com/spreadsheets/d/1WZWY-zCCki9jD26-v7QQ1CCX85NmiYclPLukeIPHJV4/"

evolutions_raw  <- read_sheet(sheet_id, sheet = "evolutions_raw") %>% 
  filter_all(any_vars(!is.na(.)))
evolutions_raw

english_names  <- read_sheet(sheet_id, sheet = "english_names")
english_names

mega_evolutions  <- read_sheet(sheet_id, sheet = "mega_evolutions")
mega_evolutions

primal_reversions  <- read_sheet(sheet_id, sheet = "primal_reversions")
primal_reversions

# remove all family rows
i=1
for(i in nrow(evolutions_raw):1){
  curr_first_cell <- evolutions_raw$A[i]
  if(grepl("family", curr_first_cell)){
    evolutions_raw <- evolutions_raw[-i,]
  }
}

# remove columns photos, evolution items
evolutions_no_levels <- evolutions_raw %>% 
  select(-c(A,C,D,`F`,G))

# rename columes
colnames(evolutions_no_levels) <- c("A", "B", "C")

# add dex numbers
evolutions_dex <- evolutions_no_levels
evolutions_dex$dex_A <- NA
evolutions_dex$dex_B <- NA
evolutions_dex$dex_C <- NA

# A
r=7
for(r in 1:nrow(evolutions_dex)){
  curr_name_raw <- as.character(evolutions_dex$A[r])
  if(!is.na(curr_name_raw)){
    curr_name <- sub("\\s\\(.*","",curr_name_raw)
    if(grepl("Unown", curr_name)){
      print(paste0("Solved Unown conflict."))
      curr_dex <- english_names %>% 
        filter(name_english == "Unown") %>% 
        pull(dex)
    } else{
      curr_dex <- english_names %>% 
        filter(name_english == curr_name) %>% 
        pull(dex)
    }
    
    if(length(curr_dex == 1)){
      evolutions_dex$dex_A[r] <- curr_dex
    } else{
      message(curr_name_raw)
    }
  }
}

r=7
for(r in 1:nrow(evolutions_dex)){
  curr_name_raw <- as.character(evolutions_dex$B[r])
  if(!is.na(curr_name_raw)){
    curr_name <- sub("\\s\\(.*","",curr_name_raw)
    if(grepl("Unown", curr_name)){
      print(paste0("Solved Unown conflict."))
      curr_dex <- english_names %>% 
        filter(name_english == "Unown") %>% 
        pull(dex)
    } else{
      curr_dex <- english_names %>% 
        filter(name_english == curr_name) %>% 
        pull(dex)
    }
    
    if(length(curr_dex == 1)){
      evolutions_dex$dex_B[r] <- curr_dex
    } else{
      message(curr_name_raw)
    }
  }
}

r=7
for(r in 1:nrow(evolutions_dex)){
  curr_name_raw <- as.character(evolutions_dex$C[r])
  if(!is.na(curr_name_raw)){
    curr_name <- sub("\\s\\(.*","",curr_name_raw)
    if(grepl("Unown", curr_name)){
      print(paste0("Solved Unown conflict."))
      curr_dex <- english_names %>% 
        filter(name_english == "Unown") %>% 
        pull(dex)
    } else{
      curr_dex <- english_names %>% 
        filter(name_english == curr_name) %>% 
        pull(dex)
    }
    
    if(length(curr_dex == 1)){
      evolutions_dex$dex_C[r] <- curr_dex
    } else{
      message(curr_name_raw)
    }
  }
}

# sort columns
evolutions_dex <- evolutions_dex %>% 
  select(dex_A, A, dex_B, B, dex_C, C)

# add Mega evolution columns
evolutions_dex_mega <- evolutions_dex
evolutions_dex_mega$dex_mega1 <- NA
evolutions_dex_mega$mega1 <- NA
evolutions_dex_mega$dex_mega2 <- NA
evolutions_dex_mega$mega2 <- NA

i=1
last_row_number = 0
for(i in 1:nrow(mega_evolutions)){
  curr_mega_dex <- mega_evolutions$dex[i]
  curr_mega_name <- mega_evolutions$name_english_mega[i]
  
  # get row in evolutions dex that corresponds to current mega
  curr_row_number <- which(evolutions_dex_mega$dex_C == curr_mega_dex)
  if(length(curr_row_number) == 0){
    curr_row_number <- which(evolutions_dex_mega$dex_B == curr_mega_dex)
  }
  if(length(curr_row_number) == 0){
    curr_row_number <- which(evolutions_dex_mega$dex_A == curr_mega_dex)
  }
  
  # only take first number
  curr_row_number <- curr_row_number[1]
  
  if(curr_row_number == last_row_number){
    evolutions_dex_mega$mega2[curr_row_number] <- curr_mega_name
    evolutions_dex_mega$dex_mega2[curr_row_number] <- curr_mega_dex
  } else{
    evolutions_dex_mega$mega1[curr_row_number] <- curr_mega_name
    evolutions_dex_mega$dex_mega1[curr_row_number] <- curr_mega_dex
  }
  last_row_number <- curr_row_number
}

# remove emtpy rows
evolutions_dex_mega <- evolutions_dex_mega %>% 
  filter_all(any_vars(!is.na(.)))
write.table(evolutions_dex_mega, "clipboard", sep="\t", row.names=FALSE, na = "")
