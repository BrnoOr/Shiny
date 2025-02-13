pacman::p_load(
  tidyverse,
  rio,
  shiny,
  shinydashboard,
  shinythemes,
  plotly,
  reactable
)

# Simulacion de datos ####

data <- data.frame(
  t = as.character(rep(1:100,3)),
  categoria1 = c(rep('Uno',150),rep('Dos',150)),
  categoria2 = c(rep('Cat1',100),rep('Cat2',100),rep('Cat3',100)),
  precio1 = rgamma(300,shape = 2,scale = 20),
  precio2 = rgamma(300,shape = 2.5,scale = 15),
  precio3 = rgamma(300,shape = 1.5,scale = 25)
)

ui <- fluidPage(
  titlePanel('Proyecciones'),
  sidebarLayout(
    sidebarPanel(
      selectInput("cat1", "Seleccionar Categoria1", choices = c("Uno", "Dos")),
      selectInput("cat2", "Seleccionar Categoria2", choices = c("Cat1", "Cat2", "Cat3"))
    ),
    mainPanel(
      plotOutput('grafico')
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Proyecciones", titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Proyecciones", tabName = "proyecciones", icon = icon("chart-line")),
      selectInput("cat1", "Seleccionar Categoria1", choices = c("Uno", "Dos")),
      selectInput("cat2", "Seleccionar Categoria2", choices = c("Cat1", "Cat2", "Cat3"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { 
          background-color: #f4f4f9; 
          font-family: 'Arial', sans-serif; 
        }
        .box { 
          border-radius: 10px; 
          background-color: #ffffff; 
        }
        .box-header { 
          background-color: #4CAF50; 
          color: white; 
          border-radius: 10px 10px 0 0; 
        }
        .box-title {
          font-size: 20px;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "proyecciones",
              box(
                title = "Gráfico de Proyecciones",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                plotlyOutput("grafico")
              )
      )
    )
  )
)


# SERVER ----
server <- function(input, output, session) {
  
  df <- reactive({
    data %>% 
      group_by(t, categoria2) %>% 
      summarise(
        p1 = mean(precio1, na.rm = TRUE),
        p2 = mean(precio2, na.rm = TRUE),
        p3 = mean(precio3, na.rm = TRUE)
      ) %>% 
      filter(categoria2 == input$cat2)  # Cambio input correcto
  })
  
  output$grafico <- renderPlotly({
    req(df())  # Asegurar que df no esté vacío
    
    if (input$cat1 == 'Uno') {
      ggplotly(
        df() %>% 
        filter(categoria2 == input$cat2) %>% 
        ggplot(aes(x = t))+
        geom_line(aes(y = p1, color = "Uno", group = 1), size = 1) + 
        geom_line(aes(y = p3, color = "Dos", group = 1), size = 1) + 
        scale_color_manual(values = c("Uno" = "darkgreen", "Dos" = "darkred")) +
        labs(color = "Fuente", x = "Fecha", y = "Valor") + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5)))
      
    } else {
      ggplotly(
      df() %>% 
        filter(categoria2 == input$cat2) %>% 
        ggplot(aes(x = t))+
        geom_line(aes(y = p2, color = "Uno", group = 1), size = 1) + 
        geom_line(aes(y = p3, color = "Dos", group = 1), size = 1) + 
        scale_color_manual(values = c("Uno" = "darkgreen", "Dos" = "darkred")) +
        labs(color = "Fuente", x = "Fecha", y = "Valor") + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5)))
    }
  })
  
  output$tabla_resumen <- renderReactable({
    reactable(req(df()), bordered = TRUE, highlight = TRUE)
  })
}

shinyApp(ui = ui, server = server)
