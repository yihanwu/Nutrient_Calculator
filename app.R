library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)

# load files from Canada Nutrient File

nutr_files <- list.files(pattern = "*.rda")
lapply(nutr_files,load,.GlobalEnv)
# format quantities for ingredients

# take everything before parenthesis
ca_measure_name$units <- regmatches(ca_measure_name$MeasureDescription, regexpr("^([^(]+)", ca_measure_name$MeasureDescription, perl = T))
# only take what is in parenthesis
ca_measure_name$description <- gsub("^([^(]+)", "", ca_measure_name$MeasureDescription, perl = T)
# extract numeric values
r <- regexpr("^[[:digit:]]+[\\/\\.]*[[:digit:]]*", ca_measure_name$units)
out <- rep(NA,nrow(ca_measure_name))
out[r!=-1] <- regmatches(ca_measure_name$units, r)
ca_measure_name$numeric <- out
# convert fractions to decimal
fractions <- grep("\\/", ca_measure_name$numeric)
ca_measure_name$numeric[fractions] <- sapply(ca_measure_name$numeric[fractions], function(x) eval(parse(text=x)))
# fill in blank numeric values
ca_measure_name$numeric[is.na(ca_measure_name$numeric)] <- 1
# everything numberic
ca_measure_name$numeric <- round(as.numeric(ca_measure_name$numeric), 5)
# now remove numbers from units
ca_measure_name$units <- gsub("^[[:digit:]]+[\\/\\.]*[[:digit:]]*", "", ca_measure_name$units)


# format ingredient choices
ca_food_choices <- ca_food_name$FoodID
names(ca_food_choices) <- ca_food_name$FoodDescription


# format daily values

daily_value <- read.table("daily_values.txt", sep = "\t", header=T, stringsAsFactors = F)

ui <- dashboardPage(
  dashboardHeader(title = "Nutrition Calculator"),
  dashboardSidebar(
    selectizeInput(
      'food_id', '1. Ingredient', choices = ca_food_choices,
      options = list(
        placeholder = 'Type to search for ingredient',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    conditionalPanel('input.food_id != ""', 
                     selectizeInput('measure_unit', '2. Measure Unit', choices = c("Select an ingredient" = "")),
                     numericInput('quantity', '3. Quantity', value = 1, min = 0, step = 1)),
    actionButton("add", "Add ingredient"),
    actionButton("remove", "Remove ingredient"),
    numericInput("serving", "Number of servings contained", min = 0.01, step = 1, value = 1),
    tags$p("Note: All nutrient information is based on the Canadian Nutrient File. Nutrient amounts do not account for variation in nutrient retention and yield losses of ingredients during preparation. % daily values (DV) are taken from the Table of Daily Values from the Government of Canada. This data should not be used for nutritional labeling.")
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("calories"),
      valueBoxOutput("over_nutrient"),
      valueBoxOutput("rich_nutrient")
    ),
    fluidRow(
      box(title = "Ingredients",
          solidHeader = T,
          width = 4,
          collapsible = T,
          div(DT::DTOutput("ing_df"), style = "font-size: 70%;")),
      box(title = "Macronutrients", solidHeader = T,
          width = 8, collapsible = T,
          plotlyOutput("macro_plot"))
    ), # row
    fluidRow(
      box(title = "Nutrition Table",
          solidHeader = T,
          width = 4, 
          collapsible = T,
          collapsed = F,
          tags$p(textOutput("serving", inline = T)),
          div(DT::DTOutput("nutrient_table"), style = "font-size: 70%;")),
      box(title = "Minerals", solidHeader = T,
          width = 8, collapsible = T,
          plotlyOutput("mineral_plot"))
    ),# row
    fluidRow(
      box(title = "Vitamins", solidHeader=T,
          width = 12, collapsible = T,
          plotlyOutput("vitamin_plot"))
    ) # row
  ) # body

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # make reactive to store ingredients
  ing_df <- shiny::reactiveValues()
  ing_df$df <- data.frame("quantity" = numeric(), 
                          "units" = character(), 
                          "ingredient_name" = character(), 
                          "FoodID" = numeric(), 
                          stringsAsFactors = F)
  ing_df$measure <- data.frame("numeric" = numeric(),
                               "units" = character(),
                               "description" = character(),
                               "ConversionFactorValue" = numeric(),
                               "MeasureID" = numeric(),
                               "FoodID" = numeric(),
                               stringsAsFactors = F)
  # step 1 get singular ingredient
  measure_df <- eventReactive(input$food_id,{
    measure_df <- ca_food_name[ca_food_name$FoodID==input$food_id, "FoodID"] %>% 
      left_join(ca_conversion_factor) %>% 
      left_join(ca_measure_name) %>% 
      select(numeric, units, description, ConversionFactorValue, MeasureID, FoodID) 
    # measure_df <- rbind(measure_df, c(numeric = 100, units = "g", description = "", ConversionFactorValue = 1, MeasureID = ""))
    measure_df
  })
  # step 2 update the measure unit for singular ingredient
  observe({
    units <- unique(paste(measure_df()$units, measure_df()$description))
    updateSelectInput(session, "measure_unit", "2. Measure Unit", choices = units)
  })
  # step 3 update the ingredient dataframe
  observeEvent(input$remove, {
    isolate(ing_df$df<-ing_df$df[-(nrow(ing_df$df)),])
    isolate(ing_df$measure <- ing_df$measure[-nrow(ing_df$measure),])
  })
  observeEvent(input$add, {
    isolate(ing_df$df[nrow(ing_df$df) + 1,] <- c(input$quantity,
                                                 input$measure_unit, 
                                                 names(ca_food_choices[ca_food_choices == input$food_id]), 
                                                 as.numeric(input$food_id)))
    # get actual working ingredient dataframe for dplyr
    input_measure <- measure_df()
    input_measure <- input_measure[paste(measure_df()$units, measure_df()$description) == input$measure_unit, ]
    if(nrow(input_measure) > 1){
      input_measure <- input_measure[which(abs(input_measure$numeric-input$quantity)==min(abs(input_measure$numeric-input$quantity))),]
    }
    isolate(ing_df$measure[nrow(ing_df$measure) + 1, ] <- input_measure)
    # update choices
    updateNumericInput(session, 'quantity', '3. Quantity', 1)
    updateSelectizeInput(session, 'measure_unit', '2. Measure Unit')
    updateSelectInput(session, 'food_id', '1. Ingredient', choices = ca_food_choices)
  })
  # main nutrition data frame
  nutrition_df <- reactive({
    measure_food_df <- ing_df$measure
    ing_quant <- ing_df$df
    measure_food_df$quantity <- ing_quant$quantity
    measure_food_df <- measure_food_df %>%
      left_join(ca_nutrient_amount) %>%
      left_join(ca_nutrient_name) %>%
      # filter(NutrientID %in% select_nutrients) %>%
      mutate(NutrientName = tolower(NutrientName)) %>%
      mutate(NutrientValue = as.numeric(NutrientValue) * as.numeric(ConversionFactorValue) * as.numeric(quantity) / as.numeric(numeric) / input$serving) %>%
    select(NutrientName, NutrientValue, NutrientID, NutrientUnit, ConversionFactorValue, quantity, FoodID) %>% 
      group_by(NutrientName) %>% 
      summarize(Value = round(sum(NutrientValue, na.rm = T),2),
                Unit = first(NutrientUnit),
                NutrientID = first(NutrientID))
    
    measure_food_df
  })
  # display nutrients necessary for label
  nutrient_table <- reactive({
    select_nutrients <- c(208, 204, 606, 605, 601, 307, 205, 291, 269, 203, 814, 401, 301, 303)
    measure_food_df <- nutrition_df() %>% filter(NutrientID %in% select_nutrients)
    measure_food_df <- measure_food_df[order(match(measure_food_df$NutrientID, select_nutrients)),] %>%
      select(NutrientName, Value, Unit)
    measure_food_df
  })
  # df with dv%
  dv_df <- reactive({
    dv_df <- daily_value %>% left_join(nutrition_df())
    # hack for total sat fats and trans fats
    dv_df$Value[2] <- sum(nutrition_df()$Value[nutrition_df()$NutrientID %in% c(605, 606)], na.rm = T)
    dv_df$Unit[2] <- "g"
    dv_df$pct_dv <- round(dv_df$Value / dv_df$DV, 3) * 100
    dv_df
  })
  
  output$macro_plot <- renderPlotly({
    df_macro <- dv_df() %>% filter(Group == "macronutrients")
    plot_macro <- ggplot(df_macro) + 
      geom_col(aes(x = Nutrient, y = pct_dv, fill = pct_dv)) +
      labs(x = "Nutrient", y = "% Daily Value") + 
      theme_gray() + 
      ylim(0, NA) +
      geom_hline(yintercept = 100) +
      scale_fill_gradient(low = "green",
                          high = "red", 
                          limits = c(0, 100),
                          na.value = "darkred",
                          name = "% Daily Value") +
      theme(panel.background = element_rect(fill = "mintcream"), 
            legend.position = "none") +
      scale_x_discrete(labels = c("Cholesterol", "Fat", "Fibre", "Sodium",  "Sugars", "Saturated and \n Trans Fats"))
    ggplotly(plot_macro)
  })
  output$mineral_plot <- renderPlotly({
    df_min <- dv_df() %>% filter(Group == "mineral")
    plot_min <- ggplot(df_min) + 
      geom_col(aes(x = Nutrient, y = pct_dv, fill = pct_dv)) +
      labs(x = "Nutrient", y = "% Daily Value")  + 
      theme_gray() + 
      ylim(0, NA) +
      geom_hline(yintercept = 100) +
      scale_fill_gradient(low = "red",
                          high = "green",
                          limits = c(0, 100),
                          na.value = "khaki1", 
                          name = "% Daily Value") +
      theme(
            legend.position = "none",
            panel.background = element_rect(fill = "lightyellow"))
    ggplotly(plot_min)
  })
  output$vitamin_plot <- renderPlotly({
    df_vit <- dv_df() %>% filter(Group == "vitamin")
    req(input$quantity)
    plot_vit <- ggplot(df_vit) + 
      geom_col(aes(x = Nutrient, y = pct_dv, fill = pct_dv)) +
      geom_hline(yintercept = 100) +
      labs(x = "Nutrient", y = "% Daily Value")  + 
      theme_gray() + 
      ylim(0, NA) +
      scale_fill_gradient(low = "red",
                          high = "green",
                          limits = c(0, 100),
                          na.value = "khaki1") +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "aliceblue"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    ggplotly(plot_vit)
  })
  # dt indicator
  output$ing_df <- DT::renderDataTable(ing_df$df[,1:3], 
                                      # colnames = c("Quantity", "Units", "Ingredient"), 
                                       rownames=F, options = list(pageLength = 5))
  output$nutrient_table <- DT::renderDataTable(nutrient_table())
  # value boxes
  output$calories <- renderValueBox({
    valueBox(paste0(nutrition_df()$Value[nutrition_df()$NutrientID == 208], "kcal"), 
             "Calories", icon = icon("fire"), color = "yellow")
  })
  output$over_nutrient <- renderValueBox({
    nutrition_df <- dv_df() %>% 
      # filter(NutrientID %in% c(601, 204, 307, 269, 0)) %>% 
      tidyr::drop_na(pct_dv) %>% filter(pct_dv > 100)
    if(nrow(nutrition_df) > 0){
      valueBox("Over Daily Value", HTML(paste0(nutrition_df$Nutrient, sep="<br>")), icon = icon("exclamation-triangle"), color = "red")
    } else {
      valueBox("All nutrients", "below recommended DV", icon = icon("exclamation-triangle"), color = "green")
    }
  })
  output$rich_nutrient <- renderValueBox({
    nutrition_df <- dv_df() %>% tidyr::drop_na(pct_dv) %>% filter(pct_dv >= 50) %>% filter(pct_dv < 100)
    if(nrow(nutrition_df) > 0){
      valueBox("High levels* of ", HTML(paste0(c(nutrition_df$Nutrient,"*above 50% recommended DV"),  sep="<br>")), icon = icon("exclamation-triangle"), color = "green")
    } else {
      valueBox(HTML("All nutrients"), "below 50% recommended DV", icon = icon("exclamation-triangle"), color = "orange")
    }
      
  })
  output$serving <- renderText(paste("for 1 serving (", input$serving, "servings in recipe)"))
}

# Run the application 
shinyApp(ui = ui, server = server)
  
