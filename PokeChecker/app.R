library(shiny)
library(googlesheets4)
gs4_deauth()
library(DT)
library(stringr) 
library(data.table)
library(tidyr)
library(dplyr)

# translations
# translations_df_wide <- read_sheet(pokedex_sheet_id,sheet = "translations")
# translations_df_wide <- read.csv(file = "PokeChecker/data/translations_df_wide.csv")
translations_df_wide <- read.csv(file = "data/translations_df_wide.csv")
# get rid of #
translations_df_wide <- translations_df_wide %>% 
  mutate(dex = gsub("#", "", dex))

# evolution chains
# evolutions_df <- read_sheet(pokedex_sheet_id,sheet = "evolution_chains") 
# evolutions_df <- read.csv(file = "PokeChecker/data/evolution_chains.csv")
evolutions_df <- read.csv(file = "data/evolution_chains.csv")
# get rid of #
evolutions_df <- evolutions_df %>%
  mutate(across(everything(), ~ gsub("#", "", .)))

# load google sheets
pokedex_sheet_id <- "https://docs.google.com/spreadsheets/d/1WZWY-zCCki9jD26-v7QQ1CCX85NmiYclPLukeIPHJV4/"

# pokedex
pokedex_df_raw <- read_sheet(pokedex_sheet_id,sheet = "pokedex") 




colnames(translations_df_wide) <- gsub("name_", "", colnames(translations_df_wide))

translations_df <- as_tibble(melt(setDT(translations_df_wide), 
                                  id.vars = c("dex"), 
                                  variable.name = "language", 
                                  value.name = "name"))


# Define UI for application 
ui <- fluidPage(
  textInput(inputId = "pokedex_sheet_id", 
            label = "Spreadsheet URL", 
            value = "https://docs.google.com/spreadsheets/d/1WZWY-zCCki9jD26-v7QQ1CCX85NmiYclPLukeIPHJV4/", 
            width = "80%", 
            placeholder = NULL),
  
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
  HTML("Unless otherwise stated, image content is taken from <a href='www.serebii.net/'  target='_blank'>www.serebii.net/</a>, licensed under the  <a href='https://creativecommons.org/licenses/by-nc-sa/2.5/'  target='_blank'>Creative Commons Attribution-NonCommercial-ShareAlike</a> license.")
  
  
)

# Define server logic
server <- function(input, output, session) {
  # observe(language_column <- input$language)
  
  # Make drop-down choice
  observeEvent(input$language, {
    updateSelectInput(session,
                      "mon_of_interest",
                      choices = pokedex_df_raw %>% 
                        pull(input$language) # input$language
    )
  })
  
  # # read pokedex sheet
  #   pokedex_df <- reactive({
  #   read_sheet(input$pokedex_sheet_id, sheet = "pokedex")
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
    
    mon_of_interest <- gsub(" \\(Mega)", "", input$mon_of_interest) # str_split(
    mon_of_interest <- gsub(" \\(Mega X\\)", "", mon_of_interest)
    mon_of_interest <- gsub(" \\(Mega Y\\)", "", mon_of_interest)
    
    output$mon_of_interest_selected_final <- renderText({ 
      paste("Converted selection:", mon_of_interest)
    })
    
    # testing
    # mon_of_interest <- "Glurak"
    mon_of_interest <- "Sandan"

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
        stages_caught[i,j] <- gsub("^b$", "x", stages_caught[i,j])
        if (grepl("Galarian", curr_name_english) == TRUE){
          stages_caught[i,j] <- gsub("^x$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/', 
                                                   substring(curr_dex, first = 2), "-g.png \"",  "height=\"20\"></img>"), 
                                     stages_caught[i,j])
        } else if (grepl("Alolan", curr_name_english) == TRUE){
          stages_caught[i,j] <- gsub("^x$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/', 
                                                   substring(curr_dex, first = 2), "-a.png \"",  "height=\"20\"></img>"), 
                                     stages_caught[i,j])
        } else {
          stages_caught[i,j] <- gsub("^x$", paste0("<img src=", 'https://www.serebii.net/pokemon/art/', 
                                                   substring(curr_dex, first = 2), ".png \"",  "height=\"20\"></img>"), 
                                     stages_caught[i,j])
        }
      }
    }
    
    
    
    
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
