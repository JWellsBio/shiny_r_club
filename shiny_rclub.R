## shiny app for r club Jan 29
# an app that plots incidences of baby names for the 2000s
# interface will include choosing girls name and boys name
# default colors will be purple and springgreen

# load libraries ----
if (!require ('ggplot2')) install.packages('ggplot2')
library(ggplot2) # for general plotting
if (!require ('shiny')) install.packages('shiny')
library(shiny) # for creating app
if (!require ('babynames')) install.packages('babynames')
library(babynames) # for data on baby names: it is raw counts by year from 1880-2017 https://cran.r-project.org/web/packages/babynames/babynames.pdf
if (!require ('shinythemes')) install.packages('shinythemes')
library(shinythemes) # for some custom looks if you want them https://cran.r-project.org/web/packages/shinythemes/shinythemes.pdf

# Define the user interface ---- #this is everything the user interacts with
ui <- fluidPage(theme = shinytheme('slate'),
  
  # App title ----
  titlePanel('Baby Names in the 2000\'s'),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout( # set the layout
    
    # Sidebar panel for inputs ----
    sidebarPanel( # this is the sidebar where all the inputs can be selected
      
      # Input: Selector for variable to indicate girl's name ----
      selectInput('girl_name', 'Girl\'s Name:', # format: 'variable_name_for_server_use', 'What the user sees'
                  c('Annie' = 'Annie',          # 'Option the user sees', 'what_the_server_uses'
                    'B' = 'Bertha',
                    'Chloe' = 'Chloe', 
                    'Dorothy' = 'Dorothy', 
                    'Effie' = 'Effie', 
                    'Frankie' = 'Frankie', 
                    'Gertrude' = 'Gertrude', 
                    'Hilda' = 'Hilda', 
                    'Irene' = 'Irene', 
                    'Jenny' = 'Jenny', 
                    'Kitty' = 'Kitty', 
                    'Lucy' = 'Lucy', 
                    'Mary' = 'Mary', 
                    'Nancy' = 'Nancy', 
                    'Olivia' = 'Olivia', 
                    'Pearl' = 'Pearl', 
                    'Queen' = 'Queen', 
                    'Rachel' = 'Rachel', 
                    'Sue' = 'Sue', 
                    'Teresa' = 'Teresa', 
                    'Una' = 'Una', 
                    'Virginia' = 'Virginia', 
                    'Winona' = 'Winona', 
                    'Zoe' = 'Zoe')),
      
      # Input: Selector for variable to indicate boy's name ----
      selectInput('boy_name', 'Boy\'s Name:', 
                  c('Archie' = 'Archie', 
                    'Bill' = 'Bill', 
                    'Christopher' = 'Christopher', 
                    'Daniel' = 'Daniel', 
                    'Elias' = 'Elias', 
                    'Felix' = 'Felix', 
                    'Garrett' = 'Garrett', 
                    'Hubert' = 'Hubert', 
                    'Irving' = 'Irving', 
                    'Joel' = 'Joel', 
                    'King' = 'King', 
                    'Logan' = 'Logan', 
                    'Mason' = 'Mason', 
                    'Norris' = 'Norris', 
                    'Orlando' = 'Orlando', 
                    'Philip' = 'Philip', 
                    'Ralph' = 'Ralph', 
                    'Sherman' = 'Sherman', 
                    'Ted' = 'Ted', 
                    'Ulysses' = 'Ulysses', 
                    'Virgil' = 'Virgil', 
                    'Webster' = 'Webster', 
                    'Zack' = 'Zack')),
      
    ),
    
    # Main panel for displaying all outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput('caption')), # this comes from the server section
      
      # Output: Plot of the selected baby names ----
      plotOutput('baby_plot') # this comes from the server section
      
    )
  )
)


# Define server logic to plot selected baby names in the 2000's ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is changed by the
  # output$caption function below
  formulaText <- reactive({
    paste('Number of Babies named', input$girl_name, 'and', input$boy_name, sep = ' ')
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText() # this is redundant if we are plotting with a title
  })
  
  # Generate a plot of the selected baby names ----
  # all the plotting and data manipulation is done here based on what we selected
  output$baby_plot <- renderPlot({
    #subset baby names to chosen names
    baby_girl <- babynames[babynames$name == input$girl_name & babynames$sex == 'F',] # be sure to only capture girls named girl
    baby_boy <- babynames[babynames$name == input$boy_name & babynames$sex == 'M', ] # be sure to only capture boys named boy
    
    #subset to 2000's
    baby_girl_2000 <- baby_girl[baby_girl$year >= 2000 & baby_girl$year <= 2009, ]
    baby_boy_2000 <- baby_boy[baby_boy$year >= 2000 & baby_boy$year <= 2009, ]
    
    #combine for plotting
    baby_combined <- rbind(baby_girl_2000, baby_boy_2000)
    
    #plot
    ggplot(baby_combined, aes(x = year, y = n, fill = name))+ #barplot number per year, filled by the names we chose
      geom_bar(stat = 'identity', position = 'dodge')+ #side-by-side plotting by year
      ggtitle(paste('Number of Babies named', input$girl_name, 'and', input$boy_name, 'during 2000\'s', sep = ' ')) + # main title, again this is redundant based on caption
      scale_fill_manual(values = c('springgreen', 'purple')) + # colors for bars, THESE DO NOT SWITCH BY BOY/GIRL!!
      scale_x_continuous(breaks = seq(2000, 2009, 1)) + # label every year
      labs(y = 'Number of Babies', x = 'Year', fill = 'Name') + theme_bw() + # axis labels and getting rid of default gray background
      theme(plot.title = element_text(face = 'bold', size = 15)) + # bolding everything and increasing sizes
      theme(axis.title = element_text(face = 'bold', size = 15)) + 
      theme(axis.text = element_text(face = 'bold', colour = 'black', size = 15)) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # tilt year labels
      theme(legend.text = element_text(face = 'bold', size = 15)) +
      theme(legend.title = element_text(face = 'bold', size = 15))
  })
  
}

shinyApp(ui, server)


## TWO (SUGGESTED) THINGS TO ALTER ----
# 1. Can we introduce an option to select decade? (And graph accordingly)
# 2. Can we set the colors to always pertain to the boy/girl name?

