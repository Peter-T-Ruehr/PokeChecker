library(shiny)
library(dplyr)
library(googlesheets4)
gs4_deauth()
library(DT)

# load google sheets
sheet_id <- "https://docs.google.com/spreadsheets/d/1WZWY-zCCki9jD26-v7QQ1CCX85NmiYclPLukeIPHJV4/"
pokedex_df <- read_sheet(sheet_id, sheet = "pokedex")
evolutions_df <- read_sheet(sheet_id, sheet = "evolutions")
# pokedex_df <- tibble(mtcars) %>% 
#   mutate(name = rownames(mtcars))

# Define UI for application that draws a histogram
ui <- fluidPage(
  selectInput(inputId = "mon_of_interest_german", 
              label = "Pokemon", 
              choices = pokedex_df$name_german,
              multiple = FALSE),
  
  h3("Present in Pokédex"),
  DT::dataTableOutput('data_present'),
  # p(),
  # h3("Needed in Pokédex"),
  # DT::dataTableOutput("data_needed")
)

# Define server logic
server <- function(input, output, session) {
  output$data_present <- DT::renderDataTable({
    req(input$mon_of_interest_german)
    
    mon_of_interest_german <- gsub(" .+$", "", input$mon_of_interest_german)
    
    # # testing
    # mon_of_interest_german <- "Vivillon"
    
    if(grepl('\\(', mon_of_interest_german)){
      mon_of_interest_german <- gsub(" .+$", "", mon_of_interest_german)
    }
    
    mon_of_interest_english <- pokedex_df %>% 
      mutate(name_german = gsub(" .+$", "", name_german)) %>% 
      filter(name_german == mon_of_interest_german) %>% 
      slice(1) %>% 
      pull(name_english) #%>% 
      # gsub(" .+$", "", .)
    
    if(grepl('\\(', mon_of_interest_english)){
      mon_of_interest_english <- gsub(" .+$", "", mon_of_interest_english)
    } 
    
    stages <- evolutions_df %>% 
      filter(name1 == mon_of_interest_english |
               name2 == mon_of_interest_english |
               name3 == mon_of_interest_english) %>% 
      unlist()
    
    stages <- stages[!is.na(stages)]
    
    
    # get entries of current stages
    curr_pokedex_present <- pokedex_df %>% 
      mutate(name_english = gsub(" \\(.+", "", name_english)) %>% 
      filter(name_english %in% stages)
    
    # fill alolan, hisuian, galaran , paldean shadow and purified
    special_forms <- c("Alola", "Hisui", "Galar", "Paldea")
    for(i in 1:nrow(curr_pokedex_present)){
      for(form in special_forms){
        if(sum(grepl(form, curr_pokedex_present$name_german))>=1){
          rows_to_edit <- which(grepl(form, curr_pokedex_present$name_german))
          row = 2
          for(row in rows_to_edit){
            curr_pokedex_present$shadow_in_dex[row] <- "(x)"
            curr_pokedex_present$purified_in_dex[row] <- "(x)"
          }
        }
      }
    }
    
    # add images to present entries
    curr_pokedex_present <- curr_pokedex_present %>% 
      replace(is.na(.), "-") %>% 
      mutate(image = paste0("<img src=", "\"https://img.pokemondb.net/artwork/avif/", gsub("\\. ", "-", tolower(name_english)), ".avif \"",  "height=\"52\"></img>"))
    
    
    # change to Alola images
    if(sum(grepl("Alola", curr_pokedex_present$name_german)) >= 1){
      for(i in 1:nrow(curr_pokedex_present)){
        if(grepl("Alola", curr_pokedex_present$name_german[i])){
          curr_pokedex_present$image[i] <- paste0("<img src=", "\"https://img.pokemondb.net/artwork/avif/", gsub("\\. ", "-", tolower(curr_pokedex_present$name_english[i])), "-alolan.avif \"",  "height=\"52\"></img>")
          
        }
      }
    }
    
    # change to Hisui images
    if(sum(grepl("Hisui", curr_pokedex_present$name_german)) >= 1){
      for(i in 1:nrow(curr_pokedex_present)){
        if(grepl("Hisui", curr_pokedex_present$name_german[i])){
          curr_pokedex_present$image[i] <- paste0("<img src=", "\"https://img.pokemondb.net/artwork/avif/", gsub("\\. ", "-", tolower(curr_pokedex_present$name_english[i])), "-hisuian.avif \"",  "height=\"52\"></img>")
          
        }
      }
    }
    
    # change to Galar images
    if(sum(grepl("Galar", curr_pokedex_present$name_german)) >= 1){
      for(i in 1:nrow(curr_pokedex_present)){
        if(grepl("Galar", curr_pokedex_present$name_german[i])){
          curr_pokedex_present$image[i] <- paste0("<img src=", "\"https://img.pokemondb.net/artwork/avif/", gsub("\\. ", "-", tolower(curr_pokedex_present$name_english[i])), "-galarian.avif \"",  "height=\"52\"></img>")
          
        }
      }
    }
    
    # change to Paldean images
    if(sum(grepl("Paldea", curr_pokedex_present$name_german)) >= 1){
      for(i in 1:nrow(curr_pokedex_present)){
        if(grepl("Paldea", curr_pokedex_present$name_german[i])){
          curr_pokedex_present$image[i] <- paste0("<img src=", "\"https://img.pokemondb.net/artwork/avif/", gsub("\\. ", "-", tolower(curr_pokedex_present$name_english[i])), "-paldean.avif \"",  "height=\"52\"></img>")
          
        }
      }
    }
    
    for(i in 1:nrow(curr_pokedex_present)){
      curr_pokedex_present$image[i] <- gsub("mime jr..avif", "mime-jr.avif", curr_pokedex_present$image[i])
    }
    
    # i=1
    for(i in 1:nrow(curr_pokedex_present)){
      curr_pokedex_present[i,1:7] <- as.list(gsub("^x$", curr_pokedex_present[i,8], curr_pokedex_present[i,1:7]))
      curr_pokedex_present[i,1:7] <- as.list(gsub("^b$", curr_pokedex_present[i,8], curr_pokedex_present[i,1:7]))
    }
    
    curr_pokedex_present <- curr_pokedex_present %>% 
      select(-image)
    
    colnames(curr_pokedex_present) <- gsub("_in_dex", "", colnames(curr_pokedex_present))
    colnames(curr_pokedex_present) <- gsub("stars3", "`3 stars`", colnames(curr_pokedex_present))
    curr_colnames <- colnames(curr_pokedex_present)
    
    curr_rownames <- rownames(t(curr_pokedex_present))
    curr_pokedex_present <- tibble(as.data.frame(t(curr_pokedex_present)))
    colnames(curr_pokedex_present) <- paste("stage", 1:ncol(curr_pokedex_present))
    
    if(ncol(curr_pokedex_present) == 3){
      curr_pokedex_present <- curr_pokedex_present %>% 
        mutate(var = curr_rownames) %>% 
        select(var, `stage 1`:`stage 3`)
    } else if(ncol(curr_pokedex_present) == 2){
      curr_pokedex_present <- curr_pokedex_present %>% 
        mutate(var = curr_rownames) %>% 
        select(var, `stage 1`:`stage 2`)
    } else if(ncol(curr_pokedex_present) == 1){
      curr_pokedex_present <- curr_pokedex_present %>% 
        mutate(var = curr_rownames) %>% 
        select(var, `stage 1`)
    }
    
    curr_pokedex_present <- curr_pokedex_present %>% 
      mutate(var = curr_colnames) %>% 
      select(var, 1:(ncol(curr_pokedex_present))) %>% 
      mutate(var = gsub("name_german", "German", var),
             var = gsub("name_english", "English", var))
    
    # separate stages if multple forms exist
    if(grepl('\\(', curr_pokedex_present[1,2])){
      final_df <- tibble()
      for(i in 1:length(stages)){
        columns_with_curr_stage <- which(grepl(stages[i], curr_pokedex_present[2,]))
        curr_df <- curr_pokedex_present[c(1,columns_with_curr_stage)]
        colnames(curr_df) <- c("var", 1:(ncol(curr_df)-1))
        final_df <- rbind(final_df,
                          curr_df)
        
      }
      curr_pokedex_present <- final_df
    }
    
    
    DT::datatable(curr_pokedex_present,
                  escape = FALSE,
                  
                  # extensions = 'Buttons',
                  
                  options = list(pageLength = 1000,
                                 searching = FALSE,
                                 lengthChange = FALSE,
                                 dom = 'tB',
                                 autoWidth = TRUE,
                                 buttons = c('copy', 'csv', 'excel')))
  })
  
  output$data_needed <- DT::renderDataTable({ # renderTable
    req(input$mon_of_interest_german)
    
    mon_of_interest_german <- gsub(" .+$", "", input$mon_of_interest_german)
    
    mon_of_interest_english <- pokedex_df %>% 
      mutate(name_german = gsub(" .+$", "", name_german)) %>% 
      filter(name_german == mon_of_interest_german) %>% 
      slice(1) %>% 
      pull(name_english) %>% 
      gsub(" .+$", "", .)
    
    stages <- evolutions_df %>% 
      filter(name1 == mon_of_interest_english |
               name2 == mon_of_interest_english |
               name3 == mon_of_interest_english) %>% 
      unlist()
    
    stages <- stages[!is.na(stages)]
    
    entries_present <- pokedex_df %>% 
      filter(name_english %in% stages)
    
    
    
    highest_stage <- length(stages)
    
    needs_stage1 <- c("m1", "f1", "m1_3stars", "f1_3stars", "m1_shadow", "f1_shadow", "m1_purified", "f1_purified")
    needs_stage2 <- c("m2", "f2", "m2_3stars", "f2_3stars", "m2_shadow", "f2_shadow", "m2_purified", "f2_purified")
    needs_stage3 <- c("m3", "f3", "m3_3stars", "f3_3stars", "m3_shadow", "f3_shadow", "m2_purified", "f2_purified")
    
    if(highest_stage == 3){
      needs <- tibble(needs_stage1, needs_stage2, needs_stage3)
    } else if(highest_stage == 2){
      needs <- tibble(needs_stage1, needs_stage2)
    }  else if(highest_stage == 1){
      needs <- tibble(needs_stage1)
    } 
    
    # what is needed in 3rd stage?
    if(length(stages) == 3){
      stage_3_male_needed <- ifelse(entries_present$male_in_dex[3]=="x", FALSE, TRUE)
      stage_3_female_needed <- ifelse(entries_present$female_in_dex[3]=="x", FALSE, TRUE)
      stage_3_stars3_needed <- ifelse(entries_present$stars3_in_dex[3]=="x", FALSE, TRUE)
      stage_3_shadow <- ifelse(entries_present$shadow_in_dex[3]=="x", FALSE, TRUE)
      stage_3_purified_needed <- ifelse(entries_present$purified_in_dex[3]=="x", FALSE, TRUE)
      
      if(!is.na(stage_3_male_needed)){
        needs$needs_stage3[1] <- NA
      }
      if(!is.na(stage_3_female_needed)){
        needs$needs_stage3[2] <- NA
      }
      if(!is.na(stage_3_stars3_needed)){
        needs$needs_stage3[3] <- NA
        needs$needs_stage3[4] <- NA
      }
      if(!is.na(stage_3_shadow)){
        needs$needs_stage3[5] <- NA
        needs$needs_stage3[6] <- NA
      }
      if(!is.na(stage_3_purified_needed)){
        needs$needs_stage3[7] <- NA
        needs$needs_stage3[8] <- NA
      }
    }
    
    # what is needed in 2nd stage?
    if(length(stages) >= 2){
      stage_2_male_needed <- ifelse(entries_present$male_in_dex[2]=="x", FALSE, TRUE)
      stage_2_female_needed <- ifelse(entries_present$female_in_dex[2]=="x", FALSE, TRUE)
      stage_2_stars3_needed <- ifelse(entries_present$stars3_in_dex[2]=="x", FALSE, TRUE)
      stage_2_shadow <- ifelse(entries_present$shadow_in_dex[2]=="x", FALSE, TRUE)
      stage_2_purified_needed <- ifelse(entries_present$purified_in_dex[2]=="x", FALSE, TRUE)
      
      if(!is.na(stage_2_male_needed)){
        needs$needs_stage2[1] <- NA
      }
      if(!is.na(stage_2_female_needed)){
        needs$needs_stage2[2] <- NA
      }
      if(!is.na(stage_2_stars3_needed)){
        needs$needs_stage2[3] <- NA
        needs$needs_stage2[4] <- NA
      }
      if(!is.na(stage_2_shadow)){
        needs$needs_stage2[5] <- NA
        needs$needs_stage2[6] <- NA
      }
      if(!is.na(stage_2_purified_needed)){
        needs$needs_stage2[7] <- NA
        needs$needs_stage2[8] <- NA
      }
    }
    
    # what is needed in 1st stage?
    stage_1_male_needed <- ifelse(entries_present$male_in_dex[1]=="x", FALSE, TRUE)
    stage_1_female_needed <- ifelse(entries_present$female_in_dex[1]=="x", FALSE, TRUE)
    stage_1_stars3_needed <- ifelse(entries_present$stars3_in_dex[1]=="x", FALSE, TRUE)
    stage_1_shadow <- ifelse(entries_present$shadow_in_dex[1]=="x", FALSE, TRUE)
    stage_1_purified_needed <- ifelse(entries_present$purified_in_dex[1]=="x", FALSE, TRUE)
    
    if(!is.na(stage_1_male_needed)){
      needs$needs_stage1[1] <- NA
    }
    if(!is.na(stage_1_female_needed)){
      needs$needs_stage1[2] <- NA
    }
    if(!is.na(stage_1_stars3_needed)){
      needs$needs_stage1[3] <- NA
      needs$needs_stage1[4] <- NA
    }
    if(!is.na(stage_1_shadow)){
      needs$needs_stage1[5] <- NA
      needs$needs_stage1[6] <- NA
    }
    if(!is.na(stage_1_purified_needed)){
      needs$needs_stage1[7] <- NA
      needs$needs_stage1[8] <- NA
    }
    
    DT::datatable(needs,
                  escape = FALSE,
                  
                  # extensions = 'Buttons',
                  
                  options = list(pageLength = 1000,
                                 searching = FALSE,
                                 lengthChange = FALSE,
                                 dom = 'tB',
                                 autoWidth = TRUE,
                                 buttons = c('copy', 'csv', 'excel')))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
