# remotes::install_github("surveydown-dev/surveydown", force = TRUE)
library(surveydown)
library(dplyr)
library(glue)

# Database setup - see the documentation for details:
# https://surveydown.org/store-data
db <- sd_database(
  host   = "aws-0-eu-west-2.pooler.supabase.com",
  dbname = "postgres",
  port   = "6543",
  user   = "postgres.azibxqzfpqacgssmtxtd",
  table  = "block1"
)


# Server setup
server <- function(input, output, session) {

  # Using URL parameters to obtain prolific PIDs
  sd_store_value(sd_get_url_pars("PROLIFIC_PID"), id = "PROLIFIC_PID")
  sd_store_value(sd_get_url_pars("SESSION_ID"), id = "SESSION_PID")
  sd_store_value(sd_get_url_pars("STUDY_ID"), id = "STUDY_ID")


  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if(
    input$attention_screenout == "blue" ~ "end_screenout",
    input$consent == "no" ~ "end_consent",
    input$consent_understand == "no" ~ "end_consent"
  )


  # Database designation and other settings
  sd_server(
    db = db,
    auto_scroll = TRUE,
    rate_survey = FALSE,
    all_questions_required = TRUE
  )

}

# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
