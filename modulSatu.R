modulSatu <- argonTabItem(
  tabName = "modul1",
  argonH1("Database Profitability", display = 4),
  argonRow(
    argonCard(
      width = 12,
      title = "Database Profitability",
      src = NULL,
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = FALSE,
      border_level = 0,
      icon = argonIcon("tv-2"),
      status = "primary",
      background_color = NULL,
      gradient = FALSE, 
      floating = FALSE,
      
      # argonRow(
      #   argonColumn(
      #     width = 12,
      #     
      #     
      #   )
      # ),
      
      argonRow(
        argonColumn(
          width = 12,
          argonH1("Informasi Umum", display = 4),
          
          h5("Langkah 0: menentukan tipe landuse"),
          br(),
          fluidRow(
            column(2,
                   selectInput(("tipe_landuse"),"Tipe Landuse",choices = c("SAWAH","KEBUN")),
            ),
            
          ),
          
          
          
          
          
          h5("Langkah 1: menentukan informasi umum untuk data PAM yang dibangun"),
          br(),
          fluidRow(
            column(2,
                   selectInput(("sut"),"Sistem Usaha Tani",choices = c("MONOKULTUR","AGROFORESTRI")),
            ),
            column(2,
                     selectInput("kom","Komoditas",choices = "" ),
            ),
            column(2,
                   selectInput("selected_prov",
                               "Pilih Provinsi:",
                               choices = ""),
            ),
            column(2,
                   selectInput("selected_wilayah",
                               "Pilih Kabupaten:",
                               choices = ""),
            ),
            column(2,
                   selectInput(("th"),"Tahun",choices = "" ),
            ),
            column(2,
                   selectInput(("tipeLahan"),"Tipe Lahan",choices = "" ),
                   
            )
          ),
          fluidRow(
            column(9,
                   br() 
                   
            ),
            column(3,
                   br(),
                   actionButton(("box_button"),"Menampilkan daftar data terpilih",icon("paper-plane"),style="color: white; 
                         background-color: green;") 
                   
            )
          )
        )
      ),
      # argonRow(
      #   argonColumn(
      #     width = 12,
      #     tags$div(id = 'uiShowMakro')
      # 
      #   )
      # ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowBox')
          
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowTable')
        )
      ),
      # argonRow(
      #   argonColumn(
      #     width = 12,
      #     tags$div(id = 'uiShowButton')
      #   )
      # ),
      # argonRow(
      #   argonColumn(
      #     width = 12,
      #     tags$div(id = 'uiShowResult')
      #   )
      # ),
      argonRow(
        argonColumn(
          width = 12,
          actionButton("browser_button","Browser Button")

        )
      ),

    )
  )
)
