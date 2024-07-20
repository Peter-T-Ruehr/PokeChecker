library(shiny)
library(googlesheets4)
gs4_deauth()
library(DT)
library(stringr) 
library(data.table)
library(tidyr)
library(dplyr)

# translations
# translations_df_wide <- read_sheet(pokedex_df_raw,sheet = "translations")
# translations_df_wide <- read.csv(file = "PokeChecker/data/translations_df_wide.csv")
translations_df_wide <- read.csv(file = "data/translations_df_wide.csv")
# get rid of #
translations_df_wide <- translations_df_wide %>% 
  mutate(dex = gsub("#", "", dex))

# evolution chains
# evolutions_df <- read_sheet(pokedex_df_raw,sheet = "evolution_chains") 
# evolutions_df <- read.csv(file = "PokeChecker/data/evolution_chains.csv")
evolutions_df <- read.csv(file = "data/evolution_chains.csv")
# get rid of #
evolutions_df <- evolutions_df %>%
  mutate(across(everything(), ~ gsub("#", "", .)))

# load google sheets
load_sheets <- function(){
  pokedex_df_raw <<- "https://docs.google.com/spreadsheets/d/1WZWY-zCCki9jD26-v7QQ1CCX85NmiYclPLukeIPHJV4/"
  
  # pokedex
  pokedex_df_raw <<- read_sheet(pokedex_df_raw,sheet = "pokedex")
  # pokedex_df_raw <- read.csv(file = "data/pokedex.csv", colClasses = "character")
  # pokedex_df_raw <- read.csv(file = "PokeChecker/data/pokedex.csv", colClasses = "character")
  
  
  # get column names to mirror and their numbers
  cols_to_mirror <- c("male_in_dex","female_in_dex","stars3_in_dex","shadow_in_dex","purified_in_dex")
  cols_to_mirror_nos <- which(colnames(dex_caught) %in% cols_to_mirror)
  
  # replace all b with x
  pokedex_df_raw <<- pokedex_df_raw %>%
    mutate(across(all_of(cols_to_mirror), ~ str_replace_all(., "b", "x")))
  
  # replace all x with 1
  pokedex_df_raw <<- pokedex_df_raw %>%
    mutate(across(all_of(cols_to_mirror), ~ str_replace_all(., "x", "1")))
  
  # get 3 stars, erlöst and crypto checks into table of special forms
  # i=37
  # j=3
  # for(i in 1:nrow(pokedex_df_raw)){
  #   curr_dex <- pokedex_df_raw$dex[i]
  #   curr_name_english <- pokedex_df_raw$name_english[i]
  #   # # get catch status of current pokemon
  #   # dex_caught <- pokedex_df_raw %>%
  #   #   filter(dex == curr_dex)
  # 
  #   
  # 
  #   if(length((unique (grep(paste(special_forms,collapse="|"),
  #                           curr_name_english, value=TRUE)))) > 0){
  #     if(any(grepl("^1$", pokedex_df_raw[i,])) |
  #        any(grepl("^b$", pokedex_df_raw[i,]))){
  #       curr_name_german <- pokedex_df_raw$name_german[i]
  #       pokedex_df_raw[i,] <- curr_pokedex_df_raw %>% slice(1)
  #       pokedex_df_raw$name_german[i] <- curr_name_german
  #       pokedex_df_raw$name_english[i] <- curr_name_english
  #     }
  #   } else {
  #     # filter pokedex_df_raw to only contain data of the curr dex number
  #     curr_pokedex_df_raw <- pokedex_df_raw %>%
  #       filter(dex == curr_dex)
  #   }
  # }
  
  i=1097
  j=3
  for(i in 1:nrow(pokedex_df_raw)){
    curr_dex <- pokedex_df_raw$dex[i]
    curr_name_english <- pokedex_df_raw$name_english[i]
    
    # get catch status of current pokemon
    dex_caught <- pokedex_df_raw %>%
      filter(dex == curr_dex)
    
    if(nrow(dex_caught) > 1){
      
      # get check values from kanto forms
      check_m <- as.numeric(gsub("x", 1, dex_caught$male_in_dex[1]))
      check_f <- as.numeric(gsub("x", 1, dex_caught$female_in_dex[1]))
      check_3 <- as.numeric(gsub("x", 1, dex_caught$stars3_in_dex[1]))
      check_s <- as.numeric(gsub("x", 1, dex_caught$shadow_in_dex[1]))
      check_p <- as.numeric(gsub("x", 1, dex_caught$purified_in_dex[1]))
      
      # go through all special form rows
      k=2
      for(k in 2:nrow(dex_caught)){
        
        if(grepl("Mega", dex_caught$name_english[k]) == FALSE){
          # check if the current form as any checked mark
          if(any(grepl("^1$", dex_caught[k,]))){
            curr_name_english_spec <- dex_caught$name_english[k]
            dex_caught$male_in_dex[k] <- max(check_m, as.numeric(dex_caught$male_in_dex[k]), na.rm = TRUE)
            dex_caught$female_in_dex[k] <- max(check_m, as.numeric(dex_caught$female_in_dex[k]), na.rm = TRUE)
            dex_caught$stars3_in_dex[k] <- max(check_m, as.numeric(dex_caught$stars3_in_dex[k]), na.rm = TRUE)
            dex_caught$shadow_in_dex[k] <- max(check_m, as.numeric(dex_caught$shadow_in_dex[k]), na.rm = TRUE)
            dex_caught$purified_in_dex[k] <- max(check_m, as.numeric(dex_caught$purified_in_dex[k]), na.rm = TRUE)
            
            # get dex_caught data into pokedex_df_raw
            pokedex_df_raw[pokedex_df_raw$name_english ==  curr_name_english_spec,] <<- dex_caught[k,]
          }
        }
        
        
      }
    }
  }
}


colnames(translations_df_wide) <- gsub("name_", "", colnames(translations_df_wide))

translations_df <- as_tibble(melt(setDT(translations_df_wide), 
                                  id.vars = c("dex"), 
                                  variable.name = "language", 
                                  value.name = "name"))


special_forms <- c("Galarian", "Alolan", "Hisuian", "Paldean")



load_sheets()

# Define UI for application 
ui <- fluidPage(
  textInput(inputId = "pokedex_df_raw", 
            label = "Spreadsheet URL", 
            value = "https://docs.google.com/spreadsheets/d/1WZWY-zCCki9jD26-v7QQ1CCX85NmiYclPLukeIPHJV4/", 
            width = "80%", 
            placeholder = NULL),
  # p(),
  # actionButton("reload_dex", "Reload Pokedex Data"),
  # p(),
  radioButtons(
    inputId = "language",
    label = "Language",
    choices = c("name_english", "name_german"), # "English", "German", "name_english", "name_german"
    selected = "name_german",
    inline = FALSE,
    width = NULL,
    choiceNames = NULL,
    choiceValues = NULL
  ),
  
  actionButton("reload", "reload"),
  
  selectInput(inputId = "mon_of_interest", 
              label = "Pokemon", 
              choices = pokedex_df_raw$name_german,
              multiple = FALSE),
  
  h3("Present in Pokédex"),
  textOutput("mon_of_interest_selected_orig"),
  textOutput("mon_of_interest_selected_final"),
  textOutput("language_selected"),
  DT::dataTableOutput('data_present'),
  p(),
  HTML("For instructions on how to use this app with your own Pokédex, please visit <a href='https://github.com/Peter-T-Ruehr/PokeChecker'  target='_blank'>PokeChecker's GitHub page.</a>"),
  p(),
  HTML("Enjoy and cheers, <a href='https://x.com/Peter_Th_R'  target='_blank'>Pete</a>"),
  p(),
  p(),
  HTML("Pokémon and All Respective Names are Trademark & © of Nintendo 1996-2024"),
  HTML("Image Content is Mirrored from <a href='www.serebii.net/'  target='_blank'>www.serebii.net/</a>; © Copyright of Serebii.net 1999-2024.") # , licensed under the  <a href='https://creativecommons.org/licenses/by-nc-sa/2.5/'  target='_blank'>Creative Commons Attribution-NonCommercial-ShareAlike</a> license.
  
  
)

# Define server logic
server <- function(input, output, session) {
  # observe(language_column <- input$language)
  
  observeEvent(input$reload, {
    # session$sendCustomMessage(type = 'testmessage',
    #                           message = 'Thank you for clicking')
    load_sheets()
  })
  
  # Make drop-down choice
  observeEvent(input$language, {
    updateSelectInput(session,
                      "mon_of_interest",
                      choices = pokedex_df_raw %>% 
                        pull(input$language) # input$language
    )
  })
  
  # # read pokedex sheet
  # read_pokedex_data <- reactive({
  #   read_sheet(input$pokedex_df_raw, sheet = "pokedex")
  # })
  # 
  # observeEvent(input$reload_dex, {
  #   pokedex_df_raw <- read_pokedex_data()
  # })
  
  
  # 
  #   pokedex_df_raw <- pokedex_df()
  
  # render table with present data
  output$data_present <- DT::renderDataTable({
    req(input$mon_of_interest,
        input$language)
    
    output$mon_of_interest_selected_orig <- renderText({ 
      paste("Original selection:", input$mon_of_interest)
    })
    
    mon_of_interest <- gsub(" \\(.+\\)", "", input$mon_of_interest)
    # mon_of_interest <- gsub(" \\(Mega\\)", "", input$mon_of_interest) # str_split(
    # mon_of_interest <- gsub(" \\(Mega X\\)", "", mon_of_interest)
    # mon_of_interest <- gsub(" \\(Mega Y\\)", "", mon_of_interest)
    # mon_of_interest <- gsub(" \\(Small\\)", "", mon_of_interest)
    # mon_of_interest <- gsub(" \\(Primal\\)", "", mon_of_interest)
    # mon_of_interest <- gsub(" \\(Average\\)", "", mon_of_interest)
    # mon_of_interest <- gsub(" \\(Large\\)", "", mon_of_interest)
    # mon_of_interest <- gsub(" \\(Super Size\\)", "", mon_of_interest)
    # mon_of_interest <- gsub(" \\(Confined\\)", "", mon_of_interest)
    # mon_of_interest <- gsub(" \\(Unbound\\)", "", mon_of_interest)
    
    output$mon_of_interest_selected_final <- renderText({ 
      paste("Converted selection:", mon_of_interest)
    })
    
    # # testing
    # mon_of_interest <- "Glurak"
    # mon_of_interest <- "Sandan"
    # mon_of_interest <- "Obstagoon"
    
    # if(grepl("(Male)", mon_of_interest)){
    #   suffix <- "(Male)"
    # } else if(grepl("(Female)", mon_of_interest)){
    #   suffix <- "(Female)"
    # } else {
    #   suffix <- ""
    # }
    
    # curr_language <- "german"
    
    # mon_of_interest <- "Glumanda"
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
                by = "dex") %>% 
      drop_na(name_german)
    
    # # here: think about how to deal with special forms and fill pokedex_df with entries
    # # fill alolan, hisuian, galaran , paldean shadow and purified
    # # get unique dexes
    # dexes <- unique(stages_caught$dex)
    # special_forms <- c("Alola", "Hisui", "Galar", "Paldea")
    # 
    # i=1
    # j=4
    # for(i in 1:length(dexes)){
    #   curr_dex <- dexes[i]
    #   curr_stages_caught <- stages_caught %>% 
    #     filter(dex == curr_dex)
    #   curr_name_english <- curr_stages_caught$name_english[i]
    #   if(grepl(special_forms, curr_name_english))
    #   for(j in 1:ncol(stages_caught)){
    #     curr_cell <- stages_caught[i,j]
    #   }
    # }
    # 
    # form=special_forms[1]
    # for(i in 1:nrow(stages_caught)){
    #   for(form in special_forms){
    #     if(sum(grepl(form, stages_caught$name_german))>=1){
    #       rows_to_edit <- which(grepl(form, stages_caught$name_german))
    #       row = 2
    #       for(row in rows_to_edit){
    #         stages_caught$shadow_in_dex[row] <- "x"
    #         stages_caught$purified_in_dex[row] <- "x"
    #       }
    #     }
    #   }
    # }
    
    
    
    # get images from https://serebii.net
    i=1
    j=5
    for(i in 1:nrow(stages_caught)){
      curr_dex <- stages_caught$dex[i]
      curr_name_english <- stages_caught$name_english[i]
      for(j in 1:ncol(stages_caught)){
        # stages_caught[i,j] <- gsub("^b$", "x", stages_caught[i,j])
        if (grepl("Galarian", curr_name_english) == TRUE){
          stages_caught[i,j] <- gsub("^1$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/', 
                                                   substring(curr_dex, first = 2), "-g.png",  " height=\"60\"></img>"), 
                                     stages_caught[i,j])
        } else if (grepl("Alolan", curr_name_english) == TRUE){
          stages_caught[i,j] <- gsub("^1$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/', 
                                                   substring(curr_dex, first = 2), "-a.png",  " height=\"60\"></img>"), 
                                     stages_caught[i,j])
        } else if (grepl("Mega X", curr_name_english) == TRUE){
          stages_caught[i,j] <- gsub("^1$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/', 
                                                   substring(curr_dex, first = 2), "-mx.png",  " height=\"60\"></img>"), 
                                     stages_caught[i,j])
        } else if (grepl("Mega Y", curr_name_english) == TRUE){
          stages_caught[i,j] <- gsub("^1$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/', 
                                                   substring(curr_dex, first = 2), "-my.png",  " height=\"60\"></img>"), 
                                     stages_caught[i,j])
        } else if (grepl("Mega", curr_name_english) == TRUE){
          stages_caught[i,j] <- gsub("^1$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/', 
                                                   substring(curr_dex, first = 2), "-m.png",  " height=\"60\"></img>"), 
                                     stages_caught[i,j])
        }  else if (grepl("Primal", curr_name_english) == TRUE){
          stages_caught[i,j] <- gsub("^1$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/', 
                                                   substring(curr_dex, first = 2), "-m.png",  " height=\"60\"></img>"), 
                                     stages_caught[i,j])
        } else if (grepl("Pom-Pom", curr_name_english) == TRUE){
          stages_caught[i,j] <- gsub("^1$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/', 
                                                   substring(curr_dex, first = 2), "-p.png",  " height=\"60\"></img>"), 
                                     stages_caught[i,j])
        } else if (grepl("P'au", curr_name_english) == TRUE){
          stages_caught[i,j] <- gsub("^1$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/', 
                                                   substring(curr_dex, first = 2), "-pau.png",  " height=\"60\"></img>"), 
                                     stages_caught[i,j])
        } else if (grepl("Sensu", curr_name_english) == TRUE){
          stages_caught[i,j] <- gsub("^1$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/', 
                                                   substring(curr_dex, first = 2), "-s.png",  " height=\"60\"></img>"), 
                                     stages_caught[i,j])
        } else {
          stages_caught[i,j] <- gsub("^1$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/',
                                                   substring(curr_dex, first = 2), ".png",  " height=\"60\"></img>"),
                                     stages_caught[i,j])
        }
      }
    }
    
    
    # stages_caught[i,j] <- gsub("^1$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/', 
    #                                          substring(curr_dex, first = 2), ".png \"",  "height=\"20\"></img>"), 
    #                            stages_caught[i,j])
    
    # https://www.serebii.net/pokemon/art/144.png
    DT::datatable(stages_caught,
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
