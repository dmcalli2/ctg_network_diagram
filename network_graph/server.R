#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)
library(tidyverse)

load("Scratch_data/trials_conditions_lookup_atc_code_lookup.Rdata")
load(file = "Scratch_data/repository_trials.Rdata")
csdr <- csdr$nct_id
trial_rep <- list(csdr = csdr, yoda = yoda, biolincc = biolincc)
other <- setdiff(trials$nct_id, unlist(trial_rep))
trial_rep$other <- other
trial_rep <- map(trial_rep, unique)

wider_drug <- trials$wider_grouping %>% 
  unique() %>% 
  sort()

shinyServer(function(input, output) {
  # create context sensitive menus
  
  # Create context sensitive drug grouping lists bzased on type of trial intersted in
  condition_lists <- reactive({
    make_condition_lists <- conditions_lkp %>% 
    inner_join(trials %>% 
                 filter(trials$comparator %in% input$trial_type,
                        trials$nct_id %in% unlist(trial_rep[input$data_share])),
               by = "nct_id") %>% 
    distinct(wider_grouping, mesh_broad_label)
  
  make_condition_lists <- by(make_condition_lists, 
                             make_condition_lists$wider_grouping,
                             function(x) {
                               x$mesh_broad_label %>%  unique() %>%  sort()
                               })
  make_condition_lists
  })

  # Wider drug groupings
  output$wider_drug_controls <- renderUI({
    selectInput(inputId = "wider_drug_groupings",
                label = "2: Choose relevant group of drugs",
                choices = names(condition_lists()) %>% 
                  unique() %>% 
                  sort(),
                multiple = TRUE,
                selected = if(input$select_all_wider) {
                  names(condition_lists())} else {
                    names(condition_lists())[1]})
  })
  # Conditions for selected drug groupings
  conditions_choose <- reactive(condition_lists()[input$wider_drug_groupings] %>% 
                                  unlist() %>% 
                                  unique() %>%
                                  sort() %>% 
                                  setNames(nm = .))

  # Choose conditions for selected drug groupings
  output$condition_controls <- renderUI({
    checkboxGroupInput("conditions", "3: Choose Conditions",
                       conditions_choose(),
                       selected = if(input$select_all_cond) {
                  names(conditions_choose())}  else {
                    names(conditions_choose())[1]})
  })
  
  # Select dataset based on conditions and drug groupings and trial type
  trials_selected <- reactive(trials %>% 
                                filter(wider_grouping %in% input$wider_drug_groupings,
                                       nct_id %in% 
                                         conditions_lkp$nct_id[conditions_lkp$mesh_broad_label 
                                                               %in% input$conditions],
                                       comparator %in% input$trial_type,
                                       nct_id %in% unlist(trial_rep[input$data_share])
                                       ) %>% 
                                arrange(wider_grouping, atc_3, atc_4, atc_5))
  # create edges
  my_edges <- reactive({
  edge_nct_id <- trials_selected() %>% 
    select(from = b, to = nct_id, atc_3, atc_4, wider_grouping) %>% 
    distinct()
  edge_drug <- trials_selected() %>% 
    select(from = atc_5, to = b, atc_3, atc_4, wider_grouping) %>% 
    distinct()
  edge_5 <- trials_selected() %>% 
    mutate(from = atc_4, to = atc_5) %>% 
    distinct(from, to, atc_3, atc_4, wider_grouping)
  edge_4 <-  trials_selected() %>% 
    mutate(from = atc_3, to = atc_4) %>% 
    distinct(from, to, atc_3, atc_4, wider_grouping)
  
   # Omit 3-digit ATC code if using compact layout and examining only a single wider grouping
      if(input$choice_layout == "compact" & length(input$wider_drug_groupings) ==1){
        edge_4 <- NULL}
  
  my_edges <- bind_rows(trial = edge_nct_id,
                        drug = edge_drug,
                        e4_5 = edge_5,
                        e3_4 = edge_4,
                        .id = "type_of_edge")
  my_edges
  })
  
  # Create nodes
  # Create a value variable for nodes such that the largest node is half 1
  my_nodes <- eventReactive(input$draw_network, {
    if(nrow(trials_selected()) ==0){
      my_nodes <- data.frame(id = 1,label = "Select trial types and conditions")
    } else {
      nct_nodes <- trials_selected() %>% 
      mutate(title = conditions,
             label = nct_id,
             value = 12.5* enrollment/sum(enrollment)) %>% 
      select(id = nct_id, title, wider_grouping, group = atc_5, label, value, comparator) %>% 
      mutate(shape = "diamond", atc_level = "NA") %>% 
      distinct()
      
      nct_nodes$shape[nct_nodes$comparator == "undefined"]  <- "dot"
      nct_nodes$shape[nct_nodes$comparator == "diff_class"] <- "square"
      nct_nodes$shape[nct_nodes$comparator == "plac_stnd"]   <- "star"
      nct_nodes$shape[nct_nodes$comparator == "poss_plac_stnd"] <- "diamond"
      nct_nodes$shape[nct_nodes$comparator %in% c("same_4")] <- "triangle"
      nct_nodes$shape[nct_nodes$comparator %in% c("same_5")] <- "triangleDown"
  
      nct_nodes$comparator <- NULL
      
      drug_nodes <- trials_selected() %>% 
        mutate(title = NA) %>% 
          select(id = b, title, wider_grouping, group = atc_5) %>% 
        mutate(shape = "rectangle", atc_level = "atc_7", label = id,
               value = 1) %>% 
        distinct()
    
      class_nodes <- trials_selected() %>% 
        select(atc_3, atc_4, atc_5, wider_grouping) %>% 
        mutate(group = atc_5) %>% 
        gather(key = "atc_level", value = "id", - wider_grouping, -group) %>% 
        mutate(shape = "circle", label = id, value = 1) %>% 
        distinct() %>% 
        inner_join(atc_lbl, c("id" = "code"))
      
      # Omit 3-digit ATC code if using compact layout and having only a single class 
      if(input$choice_layout == "compact" & length(input$wider_drug_groupings) ==1){
        class_nodes <- class_nodes %>% 
          filter(!nchar(id) == 3)}
    
       my_nodes <- bind_rows(nct_nodes, drug_nodes, class_nodes) 
    }
    my_nodes
  })
  output$network <-   renderVisNetwork({
    withProgress(message = 'LARGE NETWORK MAY BE SLOW', value = 0,{
      if(input$choice_layout == "tree"){
        visNetwork(nodes = my_nodes() %>%
                      distinct(id, .keep_all = TRUE),
                      edges = my_edges() %>%
                      distinct(from, to, .keep_all = TRUE))  %>% 
                      visOptions(collapse = TRUE, nodesIdSelection = TRUE) %>% 
                visHierarchicalLayout(direction = "LR", sortMethod = "directed")
      } else if(input$choice_layout == "compact") {
         visNetwork(nodes = my_nodes() %>%
                      distinct(id, .keep_all = TRUE),
                      edges = my_edges() %>%
                      distinct(from, to, .keep_all = TRUE))  %>% 
                      visOptions(collapse = TRUE, nodesIdSelection = TRUE) %>% 
                visLayout(improvedLayout = TRUE)
      }
      })
  })
  
  
  ## Collapse all nodes at the class level
   observe({
     if(input$goCol_5 > 0){
       visNetworkProxy("network") %>% 
         visCollapse(nodes = my_nodes()$id[my_nodes()$atc_level == "atc_5"] %>%  unique())
     }
   })
  
   ## Collapse all nodes at the drug level
   observe({
     if(input$goCol_7 > 0){
       visNetworkProxy("network") %>% 
         visCollapse(nodes = my_nodes()$id[my_nodes()$atc_level == "atc_7"] %>%  unique())
     }
   })
  
   ## Uncollapse all nodes
   observe({
     if(input$goUC > 0){
       visNetworkProxy("network") %>% visUncollapse()
     }
   })
   
   ## Drop Nodes
   observe({
      if(input$other > 0){
       visNetworkProxy("network") %>% 
         visRemoveNodes(other)
      }
     })
   observe({
      if(input$csdr > 0){
       visNetworkProxy("network") %>% 
         visRemoveNodes(csdr)
     }
   })
   observe({
      if(input$yoda > 0){
       visNetworkProxy("network") %>% 
         visRemoveNodes(yoda)
     }
   })
      observe({
      if(input$nih > 0){
       visNetworkProxy("network") %>% 
         visRemoveNodes(biolincc)
     }
   })
   
 ## Get metadata for selected trial (and or class)
   ## Select data for table
data_table_data_frame <- reactive({
       trials_selected() %>% 
         filter(nct_id %in% input$network_selected |
                atc_3 %in% input$network_selected |
                  atc_4 %in% input$network_selected |
                  atc_5 %in% input$network_selected |
                  b %in% input$network_selected) %>% 
         select(`NCT ID` = nct_id_link,
                `Title` = official_title,
                `Enrollment` = enrollment,
                `Conditions` = conditions,
                `Date registered` = first_received_date) %>% 
         distinct() 
})

  output$smry_tbl <- DT::renderDataTable(data_table_data_frame(),
                                         options = list(dom = 't'),
                                         escape = FALSE)
})
