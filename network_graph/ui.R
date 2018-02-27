#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(visNetwork)
fluidPage(tabsetPanel(
  tabPanel("Choose trials",
           flowLayout(
  checkboxGroupInput("trial_type", "1a: Select type of trial",
           choices = list("Placebo/usual care (star)" = "plac_stnd",
                          "Probable placebo/usual care(diamond)" = "poss_plac_stnd",
                          "Different class (square)" = "diff_class",
                          "Same ATC class 4th digit (triangle)" = "same_4",
                          "Same ATC class 5th digit (triangle, downward)" = "same_5",
                          "Unknown (dot)" = "undefined"),
           selected = c("Placebo/usual care (star)" = "plac_stnd",
                          "Different class (diamond)" = "diff_class",
                          "Unknown (dot)" = "undefined")),
         checkboxGroupInput("data_share", "1b: Select repository",
           choices = list("Other" = "other",
                          "CSDR" = "csdr",
                          "YODA" = "yoda",
                          "NIH" = "biolincc"),
           selected = c("Other" = "other",
                          "CSDR" = "csdr",
                          "YODA" = "yoda",
                          "NIH" = "biolincc")),
         uiOutput("wider_drug_controls"),
         uiOutput("condition_controls"),
         actionButton("select_all_wider", "Select ALL classes"),
         actionButton("select_all_cond", "Select ALL conditions"),
         radioButtons(inputId = "choice_layout", label = "Network layout",
                               choices = list("Tree" = "tree", "Compact" = "compact"),
                               selected = "tree"))),
  tabPanel("Network graph",
    h3("Select nodes and click on trial metadata tab to view trial characteristics"),
           actionButton("draw_network", "Draw/Re-draw Network Diagram", icon("paper-plane"), 
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
           column(h3("Network Controls"),
                  actionButton("goCol_5", "Collapse all drugs"),
                  actionButton("goCol_7", "Collapse all trials"),
                  actionButton("goUC", "Uncollapse all nodes"),
                  h3("Remove Nodes"),
                  actionButton(inputId = "other", label = "Other"),
                  actionButton(inputId = "csdr", label = "CSDR"),
                  actionButton(inputId = "yoda", label = "YODA"),
                  actionButton(inputId = "nih", label = "NIH"),
                  width = 3),
    column(visNetworkOutput("network", width = "1200px", height = "1200px"),
         width = 9)),
  tabPanel("Trial metadata",
  DT::dataTableOutput("smry_tbl"))
  )
)





