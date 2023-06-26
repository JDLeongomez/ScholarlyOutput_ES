# ScholarlyOutput
# Una aplicación de Shiny para visualizar un perfil de Google Scholar
# https://github.com/JDLeongomez/ScholarlyOutput
# https://zenodo.org/badge/latestdoi/536271372
# Juan David Leongómez - https://jdleongomez.info/

library(shiny)
library(thematic)
library(shinythemes)
library(colourpicker)
library(stringr)
library(scholar)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(purrr) 

# Define UI for application that draws a histogram
ui <- fluidPage(theme = c("united"),
                
                # Título de la aplicación
                titlePanel(title =
                             tags$link(rel = "icon", type = "image/gif", href = "img/icon.png"),
                           "ScholarlyOutput"),
                tags$h1(HTML("<a style=color:#EA4335;  href='https://github.com/JDLeongomez/ScholarlyOutput'><b><i>ScholarlyOutput</b></i></a>")),
                tags$h4(HTML("Visualiza tu producción académica desde <img src='https://upload.wikimedia.org/wikipedia/commons/2/28/Google_Scholar_logo.png' width='150'>")),
                tags$h6(HTML("App creada en <a style=color:#EA4335;  href='https://shiny.rstudio.com/'>Shiny</a> por 
      <a style=color:#EA4335;  href='https://jdleongomez.info/es/'>Juan David Leongómez</a>
      · 2023 <br>
      Código disponible en
      <a style=color:#EA4335;  href='https://github.com/JDLeongomez/ScholarlyOutput'>GitHub</a> · 
      <a href='https://shiny.jdl-svr.lat/ScholarlyOutput/'>English version</a>")),
      tags$h6(HTML("<p dir='auto'><a target='_blank' rel='noopener noreferrer nofollow' href='https://camo.githubusercontent.com/aafa45b848b5c22e83ab7d8f3c6e5762e995ea8ee98f0395d08ce82cf2ad9a76/68747470733a2f2f696d672e736869656c64732e696f2f6769746875622f6c6173742d636f6d6d69742f4a444c656f6e676f6d657a2f53636f6c61726c794f7574707574'><img src='https://camo.githubusercontent.com/aafa45b848b5c22e83ab7d8f3c6e5762e995ea8ee98f0395d08ce82cf2ad9a76/68747470733a2f2f696d672e736869656c64732e696f2f6769746875622f6c6173742d636f6d6d69742f4a444c656f6e676f6d657a2f53636f6c61726c794f7574707574' alt='' data-canonical-src='https://img.shields.io/github/last-commit/JDLeongomez/ScholarlyOutput' style='max-width: 100%;'></a>
<a href='https://github.com/JDLeongomez/ScholarlyOutput/blob/main/LICENSE'><img src='https://camo.githubusercontent.com/d043e5ccecf2cd7dd15535c4f06bcb5bffd346e0876e141394e76fe14b321d57/68747470733a2f2f696d672e736869656c64732e696f2f62616467652f4c6963656e73652d47504c2d2d332e302d79656c6c6f772e737667' alt='License: MIT' data-canonical-src='https://img.shields.io/badge/License-GPL--3.0-yellow.svg' style='max-width: 100%;'></a>
<a href='https://zenodo.org/badge/latestdoi/536271372' rel='nofollow'><img src='https://camo.githubusercontent.com/9a07422a279c417108743f5dc9e8a4d7066d00fceb2bfe1513170d052af62ddd/68747470733a2f2f7a656e6f646f2e6f72672f62616467652f3533363237313337322e737667' alt='DOI' data-canonical-src='https://zenodo.org/badge/536271372.svg' style='max-width: 100%;'></a>"
)),

    # Sidebar with a slider input for accent colour 
    fluidRow(
      column(3,
             hr(),
             p(HTML("Esta aplicación Shiny obtiene información sobre publicaciones y citas de 
                    <a style=color:#EA4335;  href='https://scholar.google.com/'>Google Scholar</a>
                    utilizando el paquete  
                    <a style=color:#EA4335;  href='https://cran.r-project.org/web/packages/scholar/vignettes/scholar.html'>scholar</a> 
                    de R, y representa gráficamente las citas por publicación (incluyendo tanto los índices <i>h</i>- 
                    y <i>g</i>; panel <b>A</b>), así como el número de publicaciones y citas por año 
                    (incluido el número total de citas; panel <b>B</b>).")),
             hr(),
             tags$h4("Perfil a representar"),
             textInput("profl",
                       "Copia y pega la URL completa de tu perfil de Google Scholar:", 
                       value = "https://scholar.google.com/citations?user=8Q0jKHsAAAAJ", 
                       width = 600, 
                       placeholder = "https://scholar.google.com/citations?user=8Q0jKHsAAAAJ"), 
             h4("Descarga la gráfica"),
             downloadButton("SavePlotPNG", label = "Descargar PNG"),
             downloadButton("SavePlotPDF", label = "Descargar PDF"),
             downloadButton("SavePlotSVG", label = "Descargar SVG"),
             hr(),
             tags$h4("Opciones gráficas"),
             colourInput("accentCol", 
                         "Color de acento (haga clic para seleccionar):", 
                         "#EA4335",
                         returnName = TRUE),
             tags$h6(HTML("<b>Nota:</b> alternativamente, puedes pegar el  
                          nombre (p.ej. <i><b>blue</b></i>) o
                          <a style=color:#EA4335;  href='https://g.co/kgs/Dsj3Za'>código HEX</a> 
                          (p.ej. <b>#008080</b>) de un color")),
             hr(),
             tags$h4("Filtrar publicaciones"),
             tags$h6(HTML("Te recomiendo realizar algunas tareas de 
               <a style=color:#EA4335;  href='https://scholar.google.com/intl/es/scholar/citations.html#setup'>mantenimiento</a> 
               de tu perfil antes de crear esta gráfica. Esto puede incluir, por ejemplo, 
               fusionar duplicados y asegurarse de que toda la información relevante, incluido el año, 
               esté completa y sea precisa. <br><br>
               Las publicaciones sin fecha se excluyen automáticamente de la gráfica 
               (pero no del recuento total de citas). Sin embargo, dado que la calidad de la 
               gráfica está limitada por la calidad de los datos, he añadido una opción para 
               excluir las publicaciones que aparecen como publicadas antes de un determinado año.")),
             numericInput("minyear",
                          "Excluir publicaciones con fecha anterior a:", 
                          value = 1900,
                          min = 1,
                          max = lubridate::year(Sys.Date()),
                          width = 300),
             br(),
             br(),
             br(),
             #downloadLink("downloadPlot", "Download Plot")
      ),

      # Show a plot of the generated distribution
      column(6,
             offset = 1,
             br(),
             br(),
             plotOutput("scholarPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$scholarPlot <- renderPlot(
      width = 1200,
      height = 600,
      res = 120,
      {
        #Define Scholar profile
        pfl <- input$profl |> 
          str_split(pattern = 'user\\=') |>
          map_chr(c(2)) |>
          str_sub(start = 1, end = 12)
        
        #Get data from Scholar (filtering specific non-academic publications)
        ##Publications
        pubs <- get_publications(pfl) |>
          filter(!(journal == "" | journal == "target")) |> 
          filter(!(year == "" | year < input$minyear))
        
        ##Citations
        ct <- get_citation_history(pfl)
        ##Full profile
        profile <- get_profile(pfl)
        
        #Create data frame
        ##Define years (from year of first publication to current year)
        years <- data.frame(year = c(min(pubs$year, na.rm = TRUE):as.numeric(format(Sys.Date(),'%Y'))))
        ##Get number of publications per year
        pd <- pubs |>
          group_by(year) |>
          summarise(pt = length(year)) |>
          drop_na(year)
        ##Merge years and number of publications per year
        pt <- years |>
          full_join(pd) |>
          arrange(year)
        ##Add number of citations per year
        dat <- pt |>
          full_join(ct) |>
          arrange(year) |>
          mutate(year = as.integer(year)) |> 
          mutate(across(everything(), ~replace_na(.x, 0)))
        
        #Calculate metrics
        ##Get year to count last three years
        yearRecent <- as.integer(format(Sys.Date(), '%Y')) - 2
        ##Total number of citations
        citSum <- profile$total_cites
        ##Recent citations (last three years)
        citRecentSum <- ct |>
          summarize(sumB = sum(cites[year >= yearRecent]))
        ##Number of publications with more than 50 citations
        count50cit <- nrow(ct[ct$cites > 50, ])
        ##Proportion of citation in the last three years
        citRecentProp <- citRecentSum/citSum
        
        #g-index and h-index
        ##g-index
        pubs$square <- as.numeric(row.names(pubs))^2
        pubs$sums <- cumsum(pubs$cites)
        g_index <- max(which(pubs$square < pubs$sums))
        ##h-index
        h_index <- profile$h_index
        ##Rank publications according to number of citations
        pubs$rank <- seq.int(nrow(pubs))
        ##Squared root of cumulative citations (rounded down)
        pubs$sqr <- floor(sqrt(pubs$sums))
        
        ##Define parameters for secondary axis
        ylim.prim <- c(0, max(dat$pt)*1.25)   # publications
        ylim.sec <- c(0, max(dat$cites))   # citations
        b <- diff(ylim.prim)/diff(ylim.sec)
        a <- ylim.prim[1] - b*ylim.sec[1]
        
        ## Define colors
        colors <- c("Citas por publicación" = "black", "Raíz cuadrada de las citas\nacumuladas (redondeada a la baja)" = "grey")
        
        #Plot 1: Citations per publication, h-index and g-index
        p1 <- ggplot(pubs, aes(x = rank, y = cites)) +
          geom_abline(intercept = 0, slope = 1, color = input$accentCol, linetype = "dotted", linewidth = 0.7) +
          geom_line(aes(color = "Citas por publicación")) +
          geom_line(aes(y = floor(sqrt(sums)), color = "Raíz cuadrada de las citas\nacumuladas (redondeada a la baja)")) +
          scale_color_manual(values = colors) +
          geom_segment(aes(x = h_index, y = h_index, xend = h_index, yend = h_index+(g_index*0.5)),
                       size = 0.1, color = input$accentCol,
                       arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
          geom_segment(aes(x = g_index, y = g_index, xend = g_index, yend = g_index*1.5),
                       size = 0.1, color = input$accentCol,
                       arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
          annotate("text", y = h_index+(g_index*0.55), x = h_index,
                   label= bquote('índice '*italic(h) == .(h_index)),
                   hjust = 0, angle = 90,
                   color = input$accentCol, size = 3) +
          annotate("text", y = g_index*1.55, x = g_index,
                   label = bquote('índice '*italic(g) == .(g_index)),
                   hjust = 0, angle = 90,
                   color = input$accentCol, size = 3) +
          annotate("point", x = h_index, y = h_index,
                   color = input$accentCol) +
          annotate("point", x = g_index, y = g_index,
                   color = input$accentCol) +
          labs(x = "Publicación (ranking de citas)",
               y = "Citas",
               subtitle = expression(paste("Citas por publicación, índices ", italic(~h), " y ", italic(~g)))) +
          theme_pubclean() +
          theme(axis.line.x = element_line(color = "grey"),
                axis.ticks.x = element_line(color = "grey"),
                axis.line.y.left = element_line(color = "black"),
                axis.ticks.y.left = element_line(color = "black"),
                axis.text.y.left = element_text(color = "black"),
                axis.title.y.left = element_text(color = "black"),
                legend.justification = c(1,1),
                legend.position = c(1,1),
                legend.title = element_blank(),
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                plot.subtitle = element_text(size = 9),
                axis.text = element_text(size = 6),
                axis.title = element_text(size = 8))
        
        #Plot2: Publications and citations per year
        ##Plot
        p2 <- ggplot(dat, aes(year, pt)) +
          geom_col(fill = "lightgrey") +
          geom_line(aes(y = a + cites*b), color = input$accentCol) +
          scale_x_continuous(breaks = pretty_breaks()) +
          scale_y_continuous("Publicaciones", breaks = pretty_breaks(), sec.axis = sec_axis(~ (. - a)/b, name = "Citations")) +
          theme_pubclean() +
          annotate("text", y = Inf, x = -Inf,
                   label = paste0("Total de citas = ", comma(profile$total_cites)),
                   vjust = 3, hjust = -0.1,
                   color = input$accentCol, size = 3) +
          theme(axis.line.x = element_line(color = "grey"),
                axis.ticks.x = element_line(color = "grey"),
                axis.line.y.right = element_line(color = input$accentCol),
                axis.ticks.y.right = element_line(color = input$accentCol),
                axis.text.y.right = element_text(color = input$accentCol),
                axis.title.y.right = element_text(color = input$accentCol),
                axis.line.y.left = element_line(color = "black"),
                axis.ticks.y.left = element_line(color = "black"),
                axis.text.y.left = element_text(color = "black"),
                axis.title.y.left = element_text(color = "black"),
                plot.subtitle = element_text(size=9),
                axis.text = element_text(size = 6),
                axis.title = element_text(size = 8)) +
          labs(x = "Año",
               subtitle = "Publicaciones y citas por año")
        
        #Final plot
        p.fin <- ggarrange(p1, p2,
                           ncol = 2,
                           labels = "AUTO")
        
        ##Add date to final plot
        Sys.setlocale('LC_TIME','Spanish_Colombia.utf8')
        annotate_figure(p.fin,
                        bottom = text_grob(paste0("Datos tomados de Google Scholar. Figura actualizada el ",
                                                  format(Sys.Date(),'%d de %B de %Y')),
                                           hjust = 1.05, x = 1, size = 8),
                        top = text_grob(profile$name,
                                        face = "bold", hjust = -0.1, x = 0,  size = 14))
      })
    
    output$SavePlotPNG <- downloadHandler(
      filename = function(file) {
        "Scholar_profile.png"
        #ifelse(is.null(input$DataFile), return(), str_c(input$Title, ".png"))
      },
      content = function(file) {
        ggsave(file, width = 2400, height = 1200, units = "px", dpi = 300, device = "png")
      }
    )
    
    output$SavePlotPDF <- downloadHandler(
      filename = function(file) {
        "Scholar_profile.pdf"
        #ifelse(is.null(input$DataFile), return(), str_c(input$Title, ".png"))
      },
      content = function(file) {
        ggsave(file, width = 2400, height = 1200, units = "px", dpi = 300, device = "pdf")
      }
    )
    
    output$SavePlotSVG <- downloadHandler(
      filename = function(file) {
        "Scholar_profile.svg"
        #ifelse(is.null(input$DataFile), return(), str_c(input$Title, ".png"))
      },
      content = function(file) {
        ggsave(file, width = 2400, height = 1200, units = "px", dpi = 300, device = "svg")
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
