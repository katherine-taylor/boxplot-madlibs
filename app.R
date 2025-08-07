# app.R
# author: Katherine Taylor
# date: 2025-06-26



# libraries --------------------------------------------------------------

library(shiny)
library(ggplot2)
library(gridExtra)
library(grid)
library(MetBrewer)
library(stringr)

# ui ---------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Boxplot Madlibs"),
  sidebarLayout(
    sidebarPanel(
      actionButton("gen_dist1", "Distribution 1"),
      actionButton("gen_dist2", "Distribution 2"),
      br(),
      br(),
      plotOutput("boxplot", height = "300px"),
      width = 4
    ),
    mainPanel(
      fluidRow(
        column(
          12,
          div(
            style = "display: flex; align-items: center; flex-wrap: wrap; gap: 10px;",
            tags$label(
              "Distribution 1 could represent",
              style = "margin-bottom: 0;"
            ),
            textInput("q1", label = NULL, width = "150px"),

            tags$label(
              ", while distribution 2 could represent",
              style = "margin-bottom: 0;"
            ),
            textInput("q2", label = NULL, width = "150px"),

            tags$label(
              ". The median of distribution 1 is",
              style = "margin-bottom: 0;"
            ),
            textInput("q3", label = NULL, width = "100px"),

            tags$label(".", style = "margin-bottom: 0;"),

            tags$label(
              "The median of distribution 2 is",
              style = "margin-bottom: 0;"
            ),
            textInput("q4", label = NULL, width = "100px"),

            tags$label(
              "The bottom 25% of data in distribution 1 is between ",
              style = "margin-bottom: 0;"
            ),
            textInput("q5", label = NULL, width = "100px"),

            tags$label(" and ", style = "margin-bottom: 0;"),
            textInput("q6", label = NULL, width = "100px"),

            tags$label(".", style = "margin-bottom: 0;"),

            tags$label(
              "The bottom 25% of data in distribution 2 is between ",
              style = "margin-bottom: 0;"
            ),
            textInput("q7", label = NULL, width = "100px"),

            tags$label(" and ", style = "margin-bottom: 0;"),
            textInput("q8", label = NULL, width = "100px"),

            tags$label(".", style = "margin-bottom: 0;"),

            tags$label(
              "The first quartile in distribution 1 is at ",
              style = "margin-bottom: 0;"
            ),
            textInput("q9", label = NULL, width = "100px"),

            tags$label(
              ", while the first quartile in distribution 2 is at ",
              style = "margin-bottom: 0;"
            ),
            textInput("q10", label = NULL, width = "100px"),

            tags$label(".", style = "margin-bottom: 0;"),

            tags$label(
              "The middle 50% of data in distribution 1 is between ",
              style = "margin-bottom: 0;"
            ),
            textInput("q11", label = NULL, width = "100px"),

            tags$label(" and ", style = "margin-bottom: 0;"),
            textInput("q12", label = NULL, width = "100px"),

            tags$label(".", style = "margin-bottom: 0;"),

            tags$label(
              "The middle 50% of data in distribution 2 is between ",
              style = "margin-bottom: 0;"
            ),
            textInput("q13", label = NULL, width = "100px"),

            tags$label(" and ", style = "margin-bottom: 0;"),
            textInput("q14", label = NULL, width = "100px"),

            tags$label(".", style = "margin-bottom: 0;"),

            tags$label(
              "The third quartile in distribution 1 is at ",
              style = "margin-bottom: 0;"
            ),
            textInput("q15", label = NULL, width = "100px"),

            tags$label(
              ", while the third quartile in distribution 2 is at ",
              style = "margin-bottom: 0;"
            ),
            textInput("q16", label = NULL, width = "100px"),

            tags$label(".", style = "margin-bottom: 0;"),

            tags$label(
              "The upper 25% of data in distribution 1 is between ",
              style = "margin-bottom: 0;"
            ),
            textInput("q17", label = NULL, width = "100px"),

            tags$label(" and ", style = "margin-bottom: 0;"),
            textInput("q18", label = NULL, width = "100px"),

            tags$label(".", style = "margin-bottom: 0;"),

            tags$label(
              "The upper 25% of data in distribution 2 is between ",
              style = "margin-bottom: 0;"
            ),
            textInput("q19", label = NULL, width = "100px"),

            tags$label(" and ", style = "margin-bottom: 0;"),
            textInput("q20", label = NULL, width = "100px"),

            tags$label(".", style = "margin-bottom: 0;"),

            tags$label(
              "The lower extreme of distribution 1 is ",
              style = "margin-bottom: 0;"
            ),
            textInput("q21", label = NULL, width = "100px"),

            tags$label(
              " and the upper extreme of distribution 1 is ",
              style = "margin-bottom: 0;"
            ),
            textInput("q22", label = NULL, width = "100px"),

            tags$label(".", style = "margin-bottom: 0;"),

            tags$label(
              "The lower extreme of distribution 2 is ",
              style = "margin-bottom: 0;"
            ),
            textInput("q23", label = NULL, width = "100px"),

            tags$label(
              " and the upper extreme of distribution 2 is ",
              style = "margin-bottom: 0;"
            ),
            textInput("q24", label = NULL, width = "100px"),

            tags$label(".", style = "margin-bottom: 0;"),

            tags$label("Distribution 1 has ", style = "margin-bottom: 0;"),
            textInput("q25", label = NULL, width = "100px"),

            tags$label(
              "outliers, while distribution 2 has ",
              style = "margin-bottom: 0;"
            ),
            textInput("q26", label = NULL, width = "100px"),

            tags$label(" outliers.", style = "margin-bottom: 0;"),
          )
        )
      ),
      br(),
      fluidRow(
        column(12, align = "right", downloadButton("download", "Download PNG"))
      )
    )
  )
)


# server -----------------------------------------------------------------

server <- function(input, output, session) {
  # Reactive values to hold the data
  set.seed(38487)

  values <- reactiveValues(
    dist1 = rnorm(100, mean = 50, sd = 10),
    dist2 = rnorm(100, mean = 55, sd = 15),
    color1 = MetBrewer::met.brewer("Hiroshige")[1],
    color2 = MetBrewer::met.brewer("Hiroshige")[3]
  )

  get_random_met_colors <- function(n = 2) {
    # Randomly choose a palette with at least n colors
    palettes <- MetBrewer::colorblind_palettes
    suitable <- palettes[sapply(palettes, function(p) {
      length(MetBrewer::met.brewer(p)) >= n
    })]
    pal <- sample(suitable, 1)
    cols <- MetBrewer::met.brewer(pal)
    sample(cols, n)
  }

  observeEvent(input$gen_dist1, {
    values$dist1 <- rnorm(100, mean = 50, sd = 10)
    values$color1 <- get_random_met_colors(2)[1]
  })

  observeEvent(input$gen_dist2, {
    values$dist2 <- rnorm(100, mean = 55, sd = 15)
    values$color2 <- get_random_met_colors(2)[2]
  })

  # Create the boxplot
  output$boxplot <- renderPlot({
    df <- data.frame(
      value = c(values$dist1, values$dist2),
      group = factor(rep(c("Distribution 1", "Distribution 2"), each = 100))
    )
    ggplot(df, aes(x = forcats::fct_rev(group), y = value, fill = group)) +
      stat_boxplot(geom = 'errorbar') +
      geom_boxplot() +
      scale_fill_manual(
        values = c(
          "Distribution 1" = values$color1,
          "Distribution 2" = values$color2
        )
      ) +
      theme_classic() +
      theme(
        legend.position = "none",
        panel.grid.major.x = element_line(color = "gray80", size = 0.6),
        panel.grid.minor.x = element_line(color = "gray90", size = 0.3),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)
      ) +
      coord_flip() +
      labs(x = "", y = "Value") +
      scale_y_continuous(
        breaks = seq(floor(min(df$value)), ceiling(max(df$value)), by = 10),
        minor_breaks = seq(floor(min(df$value)), ceiling(max(df$value)), by = 5)
      )
  })

  # Download handler
  output$download <- downloadHandler(
    filename = function() {
      paste0("boxplot_madlibs_", sample.int(100, 1), ".png")
    },
    content = function(file) {
      # Ensure consistent group order: Distribution 1 always first
      df <- data.frame(
        value = c(values$dist1, values$dist2),
        group = factor(
          rep(c("Distribution 1", "Distribution 2"), each = 100),
          levels = c("Distribution 1", "Distribution 2")
        )
      )

      # Create the boxplot
      p <- ggplot(
        df,
        aes(x = forcats::fct_rev(group), y = value, fill = group)
      ) +
        stat_boxplot(geom = 'errorbar') +
        geom_boxplot() +
        scale_fill_manual(
          values = c(
            "Distribution 1" = values$color1,
            "Distribution 2" = values$color2
          )
        ) +
        theme_classic() +
        theme(
          legend.position = "none",
          panel.grid.major.x = element_line(color = "gray80", size = 0.6),
          panel.grid.minor.x = element_line(color = "gray90", size = 0.3),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 18)
        ) +
        coord_flip() +
        labs(x = "", y = "Value") +
        scale_y_continuous(
          breaks = seq(floor(min(df$value)), ceiling(max(df$value)), by = 10),
          minor_breaks = seq(
            floor(min(df$value)),
            ceiling(max(df$value)),
            by = 5
          )
        )

      # Convert plot to grob
      plot_grob <- ggplotGrob(p)

      # Compose the full paragraph
      paragraph <- paste(
        "Distribution 1 could represent ",
        input$q1,
        ", while distribution 2 could represent ",
        input$q2,
        ". ",
        "The median of distribution 1 is ",
        input$q3,
        ". ",
        "The median of distribution 2 is ",
        input$q4,
        ". ",
        "The bottom 25% of data in distribution 1 is between ",
        input$q5,
        " and ",
        input$q6,
        ". ",
        "The bottom 25% of data in distribution 2 is between ",
        input$q7,
        " and ",
        input$q8,
        ". ",
        "The first quartile in distribution 1 is at ",
        input$q9,
        ", while the first quartile in distribution 2 is at ",
        input$q10,
        ". ",
        "The middle 50% of data in distribution 1 is between ",
        input$q11,
        " and ",
        input$q12,
        ". ",
        "The middle 50% of data in distribution 2 is between ",
        input$q13,
        " and ",
        input$q14,
        ". ",
        "The third quartile in distribution 1 is at ",
        input$q15,
        ", while the third quartile in distribution 2 is at ",
        input$q16,
        ". ",
        "The upper 25% of data in distribution 1 is between ",
        input$q17,
        " and ",
        input$q18,
        ". ",
        "The upper 25% of data in distribution 2 is between ",
        input$q19,
        " and ",
        input$q20,
        ". ",
        "The lower extreme of distribution 1 is ",
        input$q21,
        "and the upper extreme of distribution 1 is ",
        input$q22,
        ". ",
        "The lower extreme of distribution 2 is ",
        input$q23,
        "and the upper extreme of distribution 2 is ",
        input$q24,
        ". ",
        "Distribution 1 has ",
        input$q25,
        " outliers, while distribution 2 has ",
        input$q26,
        " outliers.",
        sep = ""
      )

      # Wrap the text for better readability
      wrapped_text <- str_wrap(paragraph, width = 120)

      # Create a text grob
      text_grob <- textGrob(
        wrapped_text,
        x = 0.05,
        y = 0.5,
        just = "left",
        gp = gpar(fontsize = 18),
        vp = viewport(width = 0.95)
      )

      # Render PNG with no overlap
      png(file, width = 1200, height = 1000)
      grid.newpage()
      pushViewport(viewport(
        layout = grid.layout(3, 1, heights = unit(c(0.05, 0.5, 0.45), "npc"))
      ))

      # Title
      grid.text(
        "Boxplot Madlibs",
        gp = gpar(fontsize = 24, fontface = "bold"),
        vp = viewport(layout.pos.row = 1)
      )

      # Plot with border
      pushViewport(viewport(layout.pos.row = 2))
      grid.rect(gp = gpar(col = "black", fill = NA, lwd = 2)) # Outline around plot
      grid.draw(plot_grob)
      popViewport()

      # Text paragraph
      pushViewport(viewport(layout.pos.row = 3))
      grid.draw(text_grob)
      popViewport()

      dev.off()
    }
  )
}

shinyApp(ui, server)
