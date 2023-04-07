# Updates:
#2023/04/07
# Now use the Google service account method to authenticate into Google Sheet
# See https://github.com/tidyverse/googlesheets4/issues/184


#2022/05/31
# Trying to set up automatic auth for google sheet


#2022/04/08
# Created a list that maps color to grant categories
# This list is used in the reactive map so that the color-category relationship stays constant regardless of user selection

# 2022/3/25
# based on app_20220324.R


library(googledrive)
library(googlesheets4)
library(dplyr)
library(tidyr)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(stringr)

# auto authorization to google account: 
#Step 1: do this once
# options(gargle_oauth_cache = ".secrets")
# drive_auth()
# list.files(".secrets/")

# Step 2: Keep this in script
#gs4_auth(cache = ".secrets")
#gs4_deauth()
#options(gargle_oauth_cache = ".secrets")
#gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)
# 
# drive_deauth()
# drive_auth(cache = ".secrets", email = TRUE)

# 04/07/2023: Now doing this only
gs4_auth(path = "service_account.json")

# prepare data ----------------------------------------------------------------------

splitComma <- function(data, i){
  as.numeric(str_split(data, ",")[[i]][1])
}

#Load CLIR Grants
# Load from google sheet. One tab at a time.
CHC_df <- read_sheet("https://docs.google.com/spreadsheets/d/1gjRPMNqsFLRzNJmjnuJxphclT3E9cvJnVS_eDBbfo2k/edit#gid=36591203", sheet="CHC")

DHC_df <- read_sheet("https://docs.google.com/spreadsheets/d/1gjRPMNqsFLRzNJmjnuJxphclT3E9cvJnVS_eDBbfo2k/edit#gid=36591203", sheet="DHC")

RAR_df <- read_sheet("https://docs.google.com/spreadsheets/d/1gjRPMNqsFLRzNJmjnuJxphclT3E9cvJnVS_eDBbfo2k/edit#gid=36591203", sheet="RAR")


CHC_df <- CHC_df %>%
  separate('Latitude / Longitude', c("lat", "long"), sep=',') %>%
  rename(year = Cohort, 
         Lead.Organization_Type = "Lead Organization_Type", 
         Lead.Organization_Category = "Lead Organization_Category",
         Lead.Organization_State = "Lead Organization_State") %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(long = as.numeric(long)) %>% 
  mutate(year_url = paste0('<a href="https://www.clir.org/hiddencollections/funded-projects/#', year, '">Go to CLIR\'s grants webpage by year</a>'))

#cleaning NA values from columns and then rows
DHC_df <- DHC_df %>%
  #select_if(~sum(!is.na(.))>0) %>% na.omit %>%
  separate('Latitude / Longitude', c("lat", "long"), sep=',') %>%
  rename(Lead.Organization_State = "State of Lead Organization",
         Lead.Organization_Type = Organization_Type,
         Lead.Organization_Category = Organization_Category,
         year = Cohort) %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(long = as.numeric(long)) %>% 
  mutate(year_url = paste0('<a href="https://www.clir.org/hiddencollections/funded-projects/#', year, '">Go to CLIR\'s grants webpage by year</a>'))

RAR_df <- RAR_df %>%
  #select_if(~sum(!is.na(.))>0) %>% na.omit %>% drop_na() %>%
  separate('Latitude / Longitude', c("lat", "long"), sep=',') %>%
  mutate(call_section = case_when(Cohort == '1 (2017)' ~ 'pilot-call',
                                  Cohort == '2 (2017)' ~ 'second-call',
                                  Cohort == '3 (2018)' ~ 'third-call',
                                  Cohort == '4 (2018)' ~ 'fourth-call',
                                  Cohort == '5 (2019)' ~ 'fifth-call',
                                  Cohort == '6 (2019)' ~ 'sixth-call',
                                  Cohort == '7 (2020)' ~ 'seventh-call',
                                  Cohort == '8 (2021)' ~ 'eighth-call')) %>% 
  mutate(year_url = paste0('<a href="https://www.clir.org/recordings-at-risk/funded-projects/#', 
                           call_section, 
                           '">Go to CLIR\'s grants webpage by call </a>')) %>% 
  mutate(Cohort = as.integer(str_extract(Cohort, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"))) %>%
  rename(Lead.Organization_State = "State of Lead Organization",
         Lead.Organization_Type = Organization_Type,
         Lead.Organization_Category = Organization_Category,
         year = Cohort) %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(long = as.numeric(long))
  

#merge CLIR Grants
master_df <- CHC_df %>% bind_rows(DHC_df) %>% bind_rows(RAR_df)

master_df <- master_df %>%
  mutate(area_full = case_when(`Program` == "Cataloging Hidden Special Collections and Archives" ~ "Cataloging Hidden Special Collections and Archives",
                               `Program` == "Digitizing Hidden Special Collections and Archives" ~ "Digitizing Hidden Special Collections and Archives",
                               `Program` == "Recordings at Risk" ~ "Recordings at Risk")) %>% 
  replace_na(list('Lead.Organization_Type' = "Not Available",
                  'Lead.Organization_Category' = "Not Available"))

# Fix some of the text in the organization category column
master_df$'Lead.Organization_Category' <- recode(master_df$'Lead.Organization_Category', 
                                                 MediaOrganization = "Media Organization", 
                                                 ReligiousOrganization = "Religious Organization", 
                                                 HistoricalSociety = "Historical Society", 
                                                 CommunityOrganization = "Community Organization", 
                                                 ResearchInstitute = "Research Institute")


#define jitter amount
JITTER = 1

#jitter the data to prevent overlap
master_df$lat_jt <- jitter(master_df$lat, factor = JITTER)
master_df$long_jt <- jitter(master_df$long, factor = JITTER)

# Create a list of popup texts used for for circles on map
pops <- lapply(seq(nrow(master_df)), function(i) {
  paste0('<p>', 
         '<b>', 'Grant Program: ', '</b>', master_df[i, 'Program'], '</p><p>',
         '<b>', 'Project Title: ', '</b>', master_df[i, 'Project Title'], '</p><p>',
         '<b>', 'Lead Organization: ', '</b>', master_df[i, 'Lead Organization'], '</p><p>',
         '<b>', 'Lead Organization Type: ', '</b>', master_df[i, 'Lead.Organization_Type'], '</p><p>',
         '<b>', 'Lead Organization Category: ', '</b>', master_df[i, 'Lead.Organization_Category'], '</p><p>',
         '<b>', 'Initial Award Year: ', '</b>', master_df[i, 'year'], '</p><p>',
        master_df[i, 'year_url'], '</p>'
  )
})


programs <- list("Cataloging Hidden Special Collections and Archives" = "Cataloging Hidden Special Collections and Archives", "Digitizing Hidden Special Collections and Archives" = "Digitizing Hidden Special Collections and Archives", "Recordings at Risk" = "Recordings at Risk")

organization_types <- list("Independent" = "Independent",
                           "Academic" = "Academic",
                           "Government" = "Government",
                           "Public" = "Public",
                           "Joint" = "Joint",
                           "Indigenous" = "Indigenous",
                           "Tribal" = "Tribal",
                           "Not Available" = "Not Available")

organization_categories <- list("Archive" = "Archive",
                                "Association/Society" = "Association/Society",
                                "Community Organization" = "CommunityOrganization",
                                "Consortium" = "Consortium",
                                "Department" = "Department",
                                "Department/Program" = "Department/Program",
                                "Foundation" = "Foundation",
                                "Historical Society" = "Historical Society",
                                "Library" = "Library",
                                "Library/Archive" = "Library/Archive",
                                "Library/Archive/Museum" = "Library/Archive/Museum",
                                "Media Organization" = "MediaOrganization",
                                "Museum" = "Museum",
                                "Museum/Library" = "Museum/Library",
                                "Research Institute" = "Research Institute",
                                "Religious Organization" = "ReligiousOrganization",
                                "Symphony" = "Symphony",
                                "Theater" = "Theater",
                                "Other" = "Other",
                                "Not Available" = "Not Available")

# A list that maps color to grant categories
# Used in the reactive map so that the color-category relationship stays constant regardless of user selection
color_area_full = c("#b30000", "#7eb0d5", "#ffee65")
names(color_area_full) = levels(factor(master_df$area_full))
                                

#ui ------------------------------------------------------------------                              
ui <- fluidPage(
  titlePanel(div(class="header", a(href="https://www.clir.org/fellowships/", img(src='clir.png', align = "right", width="32", height="32")),
                 h1("  CLIR Grants Over the Years",
                    style='padding-left: 20px'
                 ), style='padding-right: 5px'),
             windowTitle = "CLIR Grants"
  ),
  
  tags$style(type = "text/css", "#livemap {height:calc(100vh - 80px)!important;}"),
  tags$style(HTML("#input_pane {background-color: rgba(192,192,192,0.3);}")),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "radiobutton_color.css")
  ),
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}",
             ".leaflet .legend i{
      border-radius: 50%;
      border:2px solid gray;
      width: 15px;
      height: 15px;
      margin-top: 1px;
      }
    "),
  leafletOutput("livemap"),
  absolutePanel(top = 300, left = 80,
                width=500, height = 360,
                shinyjs::useShinyjs(),
                id = "input_pane",
                draggable=TRUE,
                chooseSliderSkin("Shiny", color = "#9a1c46"),
                setSliderColor(c("#9a1c46"), c(1)),
                div(style = "margin: auto; width: 90%; height: 90%",
                    pickerInput("field", "Grant Program",
                                choices = programs,
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE),
                                selected = programs),
                    pickerInput("type", "Organization Type",
                                choices = sort(unique(master_df$'Lead.Organization_Type')),
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE),
                                selected = sort(unique(master_df$'Lead.Organization_Type'))),
                    pickerInput("category", "Organization Category",
                                choices = sort(unique(master_df$'Lead.Organization_Category')),
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE),
                                selected = sort(unique(master_df$'Lead.Organization_Category'))), 
                    sliderInput("yslider", "Initial Award Year",
                              min(master_df$year),
                              max(master_df$year),
                              value = range(master_df$year),
                              step = 1,
                              sep = "",
                              width='100%'),
                    actionButton(
                      inputId = "help",
                      label = "Help"
                    ),
                    actionButton(
                      inputId = "reset",
                      label = "Reset"
                    )
                )
              )
)


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  observeEvent(input$help, {
    sendSweetAlert(
      session = session,
      title = "How to go back to select all choices?",
      text = "Click 'Deselect All' in one of the dropdowns, then click 'Select All.' Or, click the Reset button",
      type = "info"
    )
  })
  
  observeEvent(input$reset, {
    shinyjs::reset("input_pane")
  })
  
  
  # A reactive dataframe listening to the year slider
  reactive_data_chrono <- reactive({
    master_df %>%
      filter(year >= input$yslider[1] & year <= input$yslider[2]) %>% 
      filter(Lead.Organization_Type %in% input$type) %>%
      filter(Lead.Organization_Category %in% input$category) %>%
      filter(Program %in% input$field)
  })
  
  # A reactive list of popup texts listening to reactive_data_chrono()
  reactive_pops <- reactive({
    filtered_df = reactive_data_chrono() 
    pops <- lapply(seq(nrow(filtered_df)), function(i) {
      paste0('<p>', 
             '<b>', 'Grant Program: ', '</b>', filtered_df[i, 'Program'], '</p><p>',
             '<b>', 'Project Title: ', '</b>', filtered_df[i, 'Project Title'], '</p><p>',
             '<b>', 'Lead Organization: ', '</b>', filtered_df[i, 'Lead Organization'], '</p><p>',
             '<b>', 'Lead Organization Type: ', '</b>', filtered_df[i, 'Lead.Organization_Type'], '</p><p>',
             '<b>', 'Lead Organization Category: ', '</b>', filtered_df[i, 'Lead.Organization_Category'], '</p><p>',
             '<b>', 'Initial Award Year: ', '</b>', filtered_df[i, 'year'], '</p><p>',
             filtered_df[i, 'year_url'], '</p>'
             )
      
    })
  })
  
  # Base map
  output$livemap <- renderLeaflet({
    circle_pal <- colorFactor(
      palette = c("#b30000", "#7eb0d5", "#ffee65"),
      domain = master_df$area_full,
      na.color = NA)
    
    leaflet(master_df) %>%
      
      addProviderTiles("CartoDB.Positron") %>%
      
      addCircles(
        lng = master_df$long_jt, #removed jitter
        lat = master_df$lat_jt,
        popup = lapply(pops, htmltools::HTML),
        radius = 35000,
        stroke=TRUE, 
        weight=0.75, 
        opacity = 1,
        fillOpacity = 0.55,
        fillColor=circle_pal(master_df$area_full),
        color="#838383"
      ) %>% 
      addLegend("bottomright", pal = circle_pal,
                values = master_df$area_full,
                title = "Grant Program") %>% 
      setView(lat=41.11829615928769, lng=-98.00494833061417, zoom = 4)
  })

  
  # Reactive map listening to both reactive_data_chrono() and reactive_labs()
  observe({
    circle_pal <- colorFactor(
      palette = color_area_full[levels(factor(reactive_data_chrono()$area_full))],
      domain = reactive_data_chrono()$area_full,
      na.color = NA)
    leafletProxy("livemap", data = reactive_data_chrono()) %>%
      clearControls() %>% 
      clearShapes() %>%
      addCircles(
        lng = reactive_data_chrono()$long_jt,
        lat = reactive_data_chrono()$lat_jt,
        popup = lapply(reactive_pops(), htmltools::HTML),
        # change radius according to Zoom level
        # "livemap" is the map ID
        radius = case_when(input$livemap_zoom <= 4 ~ 35000,
                           input$livemap_zoom == 5 ~ 30000,
                           input$livemap_zoom == 6 ~ 15000,
                           input$livemap_zoom == 7 ~ 8000,
                           input$livemap_zoom == 8 ~ 4000,
                           input$livemap_zoom == 9 ~ 2000,
                           input$livemap_zoom == 10 ~ 1000,
                           input$livemap_zoom == 11 ~ 500,
                           input$livemap_zoom >= 12 ~ 200),
        stroke=TRUE,
        weight=0.75, 
        opacity = 1,
        fillOpacity = 0.55,
        fillColor=circle_pal(reactive_data_chrono()$area_full),
        color="#838383"
      ) %>% 
      addLegend("bottomright", pal = circle_pal,
                values = reactive_data_chrono()$area_full,
                title = "Grant Program")
  })
  
# Update selected choices in dropdowns based on user selection 
observeEvent(c(input$field,
               input$type,
               input$category),{
  filtered_df <-
    if (!is.null(input$field) &
        !is.null(input$type) &
        !is.null(input$category)) {
      master_df %>%
        filter(year >= input$yslider[1] & year <= input$yslider[2]) %>% 
        filter(Lead.Organization_Type %in% input$type) %>%
        filter(Lead.Organization_Category %in% input$category) %>%
        filter(Program %in% input$field)
    }
    else {
      master_df
    }
  # for field
  if (!is.null(input$field)) {
    updatePickerInput(
      session,
      "field",
      choices = levels(factor(master_df$Program)),
      selected = unique(filtered_df$Program))
  } else{
  }
  # for type
  if (!is.null(input$type)) {
    updatePickerInput(
      session,
      "type",
      choices = sort(unique(master_df$Lead.Organization_Type)),
      selected = sort(unique(filtered_df$Lead.Organization_Type))
    )
  }
  # for category
  if (!is.null(input$category)) {
    updatePickerInput(
      session,
      "category",
      choices =  sort(unique(master_df$Lead.Organization_Category)),
      selected  = sort(unique(filtered_df$Lead.Organization_Category))
    )
  }
},
  ignoreInit = T,
  ignoreNULL = F
)
 
}

shinyApp(ui, server)