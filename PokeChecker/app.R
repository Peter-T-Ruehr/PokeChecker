library(shiny)
library(dplyr)
library(googlesheets4)
gs4_deauth()
library(DT)

# load google sheets
pokedex_sheet_id <- "https://docs.google.com/spreadsheets/d/1WZWY-zCCki9jD26-v7QQ1CCX85NmiYclPLukeIPHJV4/"
pokedex_df <- read_sheet(pokedex_sheet_id, sheet = "pokedex") %>% 
  rename(German = name_german,
         English = name_english)

evolutions_sheet_id <- "https://docs.google.com/spreadsheets/d/1WZWY-zCCki9jD26-v7QQ1CCX85NmiYclPLukeIPHJV4/"
evolutions_df <- read_sheet(evolutions_sheet_id, sheet = "evolutions")



# Define UI for application that draws a histogram
ui <- fluidPage(
  textInput(inputId = "pokedex_sheet_id", 
            label = "Spreadsheet URL", 
            value = "https://docs.google.com/spreadsheets/d/1WZWY-zCCki9jD26-v7QQ1CCX85NmiYclPLukeIPHJV4/", 
            width = "80%", 
            placeholder = NULL),
  
  radioButtons(
    inputId = "language",
    label = "Language",
    choices = c("English", "German"), # "English", "German"
    selected = "German",
    inline = FALSE,
    width = NULL,
    choiceNames = NULL,
    choiceValues = NULL
  ),
  
  
  selectInput(inputId = "mon_of_interest", 
              label = "Pokemon", 
              choices = pokedex_df$German,
              multiple = FALSE),
  
  h3("Present in Pokédex"),
  # textOutput("mon_of_interest_selected_orig"),
  # textOutput("mon_of_interest_selected_final"),
  DT::dataTableOutput('data_present'),
  p(),
  HTML("For instructions on how to use this app with your own Pokédex, please visit <a href='https://github.com/Peter-T-Ruehr/PokeChecker'  target='_blank'>PokeCher's GitHub page.</a>"),
  p(),
  HTML("Enjoy and cheers, <a href='https://x.com/Peter_Th_R'  target='_blank'>Pete</a>")
)

# Define server logic
server <- function(input, output, session) {
  # observe(language_column <- input$language)
  
  # Make drop-down choice of sel_2 dependent upon user input of sel_1
  observeEvent(input$language, {
    updateSelectInput(session,
                      "mon_of_interest",
                      choices = pokedex_df() %>% 
                        pull(input$language)
    )
  })
  
  pokedex_df <- reactive({
    read_sheet(input$pokedex_sheet_id, sheet = "pokedex") %>% 
      rename(German = name_german,
             English = name_english)
  })
  
  output$data_present <- DT::renderDataTable({
    req(input$mon_of_interest,
        input$language)
    
    output$mon_of_interest_selected_orig <- renderText({ 
      paste("You have selected", input$mon_of_interest)
    })
    
    mon_of_interest <- input$mon_of_interest
    
    # # testing
    # mon_of_interest <- "Vivillon"
    
    if(grepl('\\(', mon_of_interest)){
      mon_of_interest <- gsub(" \\(.+$", "", mon_of_interest)
    }
    
    # translate from german to english
    if(input$language == "German"){
      mon_of_interest <- pokedex_df() %>% 
        mutate(German = gsub(" \\(.+$", "", German)) %>% 
        filter(German == mon_of_interest) %>% 
        slice(1) %>% 
        pull(English) 
      
      if(grepl('\\(', mon_of_interest)){
        mon_of_interest <- gsub(" \\(.+$", "", mon_of_interest)
      }
    }
    
    output$mon_of_interest_selected_final <- renderText({ 
      paste("You have selected", mon_of_interest)
    })
    
    stages <- evolutions_df %>% 
      filter(name1 == mon_of_interest |
               name2 == mon_of_interest |
               name3 == mon_of_interest) %>% 
      unlist()
    
    stages <- stages[!is.na(stages)]
    
    
    # get entries of current stages
    curr_pokedex_present <- pokedex_df() %>% 
      mutate(English = gsub(" \\(.+", "", English)) %>% 
      filter(English %in% stages)
    
    # fill alolan, hisuian, galaran , paldean shadow and purified
    special_forms <- c("Alola", "Hisui", "Galar", "Paldea")
    for(i in 1:nrow(curr_pokedex_present)){
      for(form in special_forms){
        if(sum(grepl(form, curr_pokedex_present$German))>=1){
          rows_to_edit <- which(grepl(form, curr_pokedex_present$German))
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
      mutate(image = paste0("<img src=", "\"https://img.pokemondb.net/artwork/avif/", gsub("\\. ", "-", tolower(English)), ".avif \"",  "height=\"52\"></img>"))
    
    
    # change to Alola images
    if(sum(grepl("Alola", curr_pokedex_present$German)) >= 1){
      for(i in 1:nrow(curr_pokedex_present)){
        if(grepl("Alola", curr_pokedex_present$German[i])){
          curr_pokedex_present$image[i] <- paste0("<img src=", "\"https://img.pokemondb.net/artwork/avif/", gsub("\\. ", "-", tolower(curr_pokedex_present$English[i])), "-alolan.avif \"",  "height=\"52\"></img>")
          
        }
      }
    }
    
    # change to Hisui images
    if(sum(grepl("Hisui", curr_pokedex_present$German)) >= 1){
      for(i in 1:nrow(curr_pokedex_present)){
        if(grepl("Hisui", curr_pokedex_present$German[i])){
          curr_pokedex_present$image[i] <- paste0("<img src=", "\"https://img.pokemondb.net/artwork/avif/", gsub("\\. ", "-", tolower(curr_pokedex_present$English[i])), "-hisuian.avif \"",  "height=\"52\"></img>")
          
        }
      }
    }
    
    # change to Galar images
    if(sum(grepl("Galar", curr_pokedex_present$German)) >= 1){
      for(i in 1:nrow(curr_pokedex_present)){
        if(grepl("Galar", curr_pokedex_present$German[i])){
          curr_pokedex_present$image[i] <- paste0("<img src=", "\"https://img.pokemondb.net/artwork/avif/", gsub("\\. ", "-", tolower(curr_pokedex_present$English[i])), "-galarian.avif \"",  "height=\"52\"></img>")
          
        }
      }
    }
    
    # change to Paldean images
    if(sum(grepl("Paldea", curr_pokedex_present$German)) >= 1){
      for(i in 1:nrow(curr_pokedex_present)){
        if(grepl("Paldea", curr_pokedex_present$German[i])){
          curr_pokedex_present$image[i] <- paste0("<img src=", "\"https://img.pokemondb.net/artwork/avif/", gsub("\\. ", "-", tolower(curr_pokedex_present$English[i])), "-paldean.avif \"",  "height=\"52\"></img>")
          
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
}

# Run the application 
shinyApp(ui = ui, server = server)
