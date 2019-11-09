#### Scale shiny app v2019 ####


library(shiny)
library(shinydashboard)
#library(shinyjs)

## NOTES #######
# could use conditional panel for group 1 or 2 on grades 7 etc...

# checkboxInput("smooth", "Smooth"),
# conditionalPanel(
#   condition = "input.smooth == true",
#   selectInput("smoothMethod", "Method",
#               list("lm", "glm", "gam", "loess", "rlm"))
# )

## TO DO

## Make separate buttons for scales, arpeggios, etc. and then a random exam set... 
## Hands together or separately - why is this different? because there is a higher probability 
# of choosing hands together. Could have done this in the initial set up, but anyway...

## define functions

## Could have done a function to set up scales... if I'd started doing all grades at same time... 

saveData <- function(data) {
  data <- as.data.frame(t(data), stringsAsFactors = F)
  if (exists("responses")) {
    responses <<- rbind(responses, data, make.row.names = F, stringsAsFactors = F)
  } else {
    responses <<- data
  }
}


sumData <- function() {
  if (exists("responses")) {
    final <- aggregate(responses$rate1, list(responses$rate1), length)
    final$pc <- round(final$x/sum(final$x)*100, 1)
    final$x <- NULL
    colnames(final) <- c("Rating", "pc")
    # reorder
    final <- merge(final, data.frame(Rating = c("good", "medium", "bad"), order = 1:3), all.x = T)
    final <- final[order(final$order),]
    final$order <- NULL # delete column
    final
  }
}




practiceList <- function() {
  if (exists("responses")) {
    pList <- data.frame(Practice = as.character(responses$scale[responses$rate1 == "bad"]))
    pList
  }
}

# Define scales
## Print sharps, flats, and natural
# print("\U266F") # sharp
# print("\U266D") # flat
# print("\U266E") # natural

scale.letter <- c("C", "D♭", "D", "E♭", "E", "F", "F#", "G", "A♭",  "A", "B♭", "B")

g6.scales <- scale.letter
g7.2.scales <- scale.letter[c(8, 10, 12, 6, 4, 2)]
g7.1.scales <- scale.letter[c(1, 3, 5, 7, 11, 9)]
g8.scales <- scale.letter[c(1, 3, 12, 7, 6, 4, 9, 2)]

#print(g7.1.scales)
          
#print(scale.letter)
mm <- c("major", "harmonic minor", "melodic minor")
stacc.leg <- c("staccato", "legato")
root <- c("root position", "first inversion", "second inversion")

### grade 6 scales ####
scales.6 <- paste("Scale:", apply(expand.grid(scale.letter, mm), 1, paste, collapse = " "))
#print(scales)
arps.6 <- paste("Arpeggio:", apply(expand.grid(scale.letter, c("major", "minor")), 1, paste, collapse= " "))

all.tech.6 <- list(Scales = scales.6,
                 Arpeggios = arps.6, 
                 Contr = c("Contrary motion E\u266D","Contrary motion A", "Chromatic contrary motion A#(LH) C#(RH)"),
                 Staccato = c("Staccato thirds C major", "Staccato A", "Staccato E\u266D"),
                 Dim = c("Diminished sevenths - B", "Diminished sevenths - C#"),
                 Chrom = paste("Chromatic scale:", scale.letter))
all.tech.6 <- lapply(all.tech.6, enc2utf8)

#### grade 7 scales ####
# grade 7 group 1
scales.7.1 <- paste("Scale:", apply(expand.grid(g7.1.scales, mm, stacc.leg), 1, paste, collapse = " "))
#print(scales.7.1)
arps.7.1 <- paste("Arpeggio:", apply(expand.grid(g7.1.scales, c("major", "minor"), root[1:2]), 1, paste, collapse= " "))
scales.3rd.7.1 <- paste("Scale a third apart:", 
                        apply(expand.grid(g7.1.scales, mm[1:2], stacc.leg), 1, paste, collapse= " "))
contr.7.1 <- paste("Contrary motion:", 
                   apply(expand.grid(g7.1.scales, mm[1:2], stacc.leg), 1, paste, collapse= " "))
chrom7 <- paste("Chromatic scale:", apply(expand.grid(scale.letter, stacc.leg), 1, paste, collapse= " "))
chrom.contr7 <- paste("Chromatic contrary-motion:", 
                     apply(expand.grid(scale.letter[c(1,7)], stacc.leg), 1, paste, collapse= " "))
dom7th.7.1 <- paste("Dominant sevenths: in the key of ", apply(expand.grid(g7.1.scales), 1, paste, collapse = " "))

all.tech.7.1 <- list(Scales = scales.7.1,
                   Arpeggios = arps.7.1, 
                   Sc3apart = scales.3rd.7.1,
                   Contr = contr.7.1,
                   Leg3rds = "Legato scale in thirds: C major",
                   Stac6th = "Staccato scale in sixths: C major",
                   Chrom = chrom7,
                   Chrom_contr = chrom.contr7,
                   Dom7th = dom7th.7.1,
                   Dim = c("Diminished sevenths - A", "Diminished sevenths - C#"))
                   
all.tech.7.1 <- lapply(all.tech.7.1, enc2utf8)

# Grade 7 Group 2 ##
scales.7.2 <- paste("Scale:", apply(expand.grid(g7.2.scales, mm, stacc.leg), 1, paste, collapse = " "))
arps.7.2 <- paste("Arpeggio:", apply(expand.grid(g7.2.scales, c("major", "minor"), root[1:2]), 1, paste, collapse= " "))
scales.3rd.7.2 <- paste("Scale a third apart:", 
                        apply(expand.grid(g7.2.scales, mm[1:2], stacc.leg), 1, paste, collapse= " "))
contr.7.2 <- paste("Contrary motion:", 
                   apply(expand.grid(g7.2.scales, mm[1:2], stacc.leg), 1, paste, collapse= " "))
dom7th.7.2 <- paste("Dominant sevenths: in the key of ", apply(expand.grid(g7.2.scales), 1, paste, collapse = " "))

all.tech.7.2 <- list(Scales = scales.7.2,
                     Arpeggios = arps.7.2, 
                     Sc3apart = scales.3rd.7.2,
                     Contr = contr.7.2,
                     Leg3rds = "Legato scale in thirds: C major",
                     Stac6th = "Staccato scale in sixths: C major",
                     Chrom = chrom7,
                     Chrom_contr = chrom.contr7,
                     Dom7th = dom7th.7.2,
                     Dim = c("Diminished sevenths - A", "Diminished sevenths - C#"))

all.tech.7.2 <- lapply(all.tech.7.2, enc2utf8)

### Grade 8 scales####
scales.8 <- paste("Scale:", apply(expand.grid(g8.scales, mm, stacc.leg), 1, paste, collapse = " "))
#print(scales.7.1)
arps.8 <- paste("Arpeggio:", apply(expand.grid(g8.scales, c("major", "minor"), root), 1, paste, collapse= " "))
scales.3rd.8 <- paste("Scale a third apart:", 
                        apply(expand.grid(g8.scales, mm[1:2], stacc.leg), 1, paste, collapse= " "))
scales.6th.8 <- paste("Scale a sixth apart:", 
                      apply(expand.grid(g8.scales, mm[1:2], stacc.leg), 1, paste, collapse= " "))
chrom.3rd.8 <- paste("Chromatic scale a minor third apart:", apply(expand.grid(scale.letter, stacc.leg), 1, paste, collapse= " "))

dom7th.8 <- paste("Dominant sevenths: in the key of ", apply(expand.grid(g8.scales), 1, paste, collapse = " "))
dim7.8 <- paste("Diminshed sevenths:", scale.letter)

all.tech.8 <- list(Scales = scales.8,
                   Arpeggios = arps.8,
                   Sc3apart = scales.3rd.8,
                   Sc6apart = scales.6th.8,
                   Leg3rds = c("Legato scale in thirds: C major", "Legato scale in thirds: B♭ major"),
                   Chrom3apart = chrom.3rd.8,
                   ChromINm3rd = c("Chromatic scale in minor thirds: A#","Chromatic scale in minor thirds: C#"),
                   wholeTone = "Whole tone scale: E",
                   Dom7th = dom7th.8,
                   Dim = dim7.8)

all.tech.8 <- lapply(all.tech.7.1, enc2utf8)


### UI stuff #####

# store scale info
# define fields for results table
i.fields <- c("rate1")

#o.fields <- c("type", "random.scale")
# o.fields <- c("random.scale")


## changing radiobutton size...  CSS
# https://stackoverflow.com/questions/41654766/how-to-change-the-size-and-spacing-of-check-boxes-and-radio-buttons-in-shiny-app



ui <- dashboardPage(
  dashboardHeader(
    title = "Track my Scales!",
    titleWidth = 450
  ),
  
  dashboardSidebar(
    h4("Choose grade"),
    radioButtons("grade", label = "grade", list("6" = "six", "7 group 1" = "seven1", "7 group 2" = "seven2", "8" = "eight"), selected = "eight")
    ),
  
  dashboardBody(
    #useShinyjs(),
    fluidRow(
      box(
        title = "Play", status = "success", solidHeader =T, width = 12, #footer = textOutput("rasLayer"),
        h3("Play me!"),
        h4(span(textOutput("random_scale"), style = "color:darkred;font-weight:bold")),
        h4(span(textOutput("handsTS"), style = "color:darkred;font-weight:bold")),
        h4("Choose scales:"),
        actionButton("action1", label = "Exam set"),
        actionButton("scales", label = "Scales"),
        actionButton("arpeg", label = "Arpeggios"),
        actionButton("tech", label = "Technical"),
        radioButtons("rate1", label = "How did you do?", 
                     list("Not so good" = "bad","Mas o menos" = "medium", "Pretty good" = "good")), # 
        actionButton("submit", label = "Submit"),
        actionButton("done", label = "Show results"),
        p()
        )),
    fluidRow(
      box(
        title = "Results", solidHeader = T, status = "success", width = 12,#footer = textOutput("modLayer"),
        tableOutput("final"),
        tableOutput("practiceList"),
        p()
      )
    )
  )
)




# Define server logic 
server <- function(input, output) {
  
  
  #tech
  ## random scale choice
  ## use isolate
  
  ## TO DO ?  put these into reactive values list??? why ??? wha'ts the difference between that and a function?
  
  ## get set of scales according to grade
    all.tech <- reactive({
    
    switch(input$grade,
           
           
           "six" = all.tech.6,
           "seven1" = all.tech.7.1,
           "seven2" = all.tech.7.2,
           "eight" = all.tech.8
           )
    })
  
  prob <- reactive({
    
    switch(input$grade,
           
           "six" = c(0.5, 0.3, 0.05, 0.05, 0.05, 0.05),
           "seven1" = c(0.2, 0.2, 0.1, 0.1, 0.05,0.05,0.1,0.1,0.05,0.05),
           "seven2" = c(0.2, 0.2, 0.1, 0.1, 0.05,0.05,0.1,0.1,0.05,0.05),
           "eight" = c(0.2, 0.2, 0.1125, 0.1125, 0.025,0.1, 0.025,0.025,0.1,0.1)
           )
    
    
  })
  

  ## Make scales
  scale <- reactiveValues(sc = NULL, hands = NULL, rating = NULL)
  
  # scale <- eventReactive(input$action1,{
  #   sample(all.tech()[[sample(seq_along(all.tech()),1,prob = prob())]], 1)})
  # 
  
  # get rating
  observeEvent(input$rate1, {
    
    scale$rating <- input$rate1
    
  })
  
  # get any random tech exercise
  observeEvent(input$action1,{
    scale$sc <- sample(all.tech()[[sample(seq_along(all.tech()),1,prob = prob())]], 1)})
  
  # get only scales
  observeEvent(input$scales,{
    scale$sc <- sample(all.tech()$Scales,1)})
  
    # get only arpeggios
  observeEvent(input$arpeg,{
    scale$sc <- sample(all.tech()$Arpeggios,1)})
  
  # get just other technical
  observeEvent(input$tech,{
    scale$sc <- sample(unlist(all.tech()[!names(all.tech()) %in% c("Scales", "Arpeggios")], use.names = F), 1)})
  
  # Generate scale name to print ----
  output$random_scale <- renderText({scale$sc})
  
  ## reset scales when grade is pressed
  observeEvent(input$grade, { 
    scale$sc <- NULL}
  )
  
  observeEvent(input$grade, {
    
  scale$hands <- switch(input$grade,
                        "six" = c("TS", "TS", "T", "T", "S", "S"),
                        "seven1" = c("TS", "TS", "T", "T", "S", "S", "TS", "T", "TS", "TS"),
                        "seven2" = c("TS", "TS", "T", "T", "S", "S", "TS", "T", "TS", "TS"),
                        "eight" = c("TS", "TS", "TS", "T", "S", "T", "S", "TS", "TS", "TS")
                        )
  })
  
  #   #if(exists("scale")) rm(scale)
  #   if(exists(output$random_scale)) output$random_scale <- NULL
  #   })#, event.env = .GlobalEnv)
  # 
  
  
  # hands together or separately.
  observeEvent(input$action1 | input$scales | input$arpeg | input$tech, { # can also use list(input$actn, input$...)
    
    if(input$action1 == 0 && input$scales == 0 && input$arpeg == 0 && input$tech == 0) {return()}
    
    grp <- which(sapply(all.tech(), function(x) any(is.element(scale$sc, x))))
    
    scale$handsText <- switch(scale$hands[grp],
                             "T" = "Hands together",
                             "S" = "Hands separately",
                             "TS" = sample(c("Hands together", "Hands separately"), size = 1, prob = c(0.7,0.3))
                            )
    })
  
  
  ## Hands togther / separately text to print
  output$handsTS <- renderText({scale$handsText})
  
  ## save data from scales and rate button
  formData <- reactive({
    #data1 <- sapply(i.fields, function(x) input[[x]])
    # print(data1)
    #data <- c(data1, scale = scale$sc)
    data <- c(rate1 = scale$rating, scale = scale$sc)
    # str(data)
    data
  })
     
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  ## summary table
  output$final <- renderTable({
    input$done
    sumData()
  })
  
  # summary barplot
  # output$sumBarplot <- renderPlot({
  #   input$done
  #   barplot(sumData()$pc, names.arg = sumData()$Rating)
  #   
  # })
  # 
  
  # practice table
  output$practiceList <- renderTable({
    input$done
    practiceList()
  })
  
  #   
}



shinyApp(ui = ui, server = server)