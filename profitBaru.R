profitBaru <- argonTabItem(
  tabName = "profitBaru",
  argonH1("Analisis Profitabilitas", display = 4),
  argonRow(
    argonCard(
      width = 12,
      title = "Analisis Profitabilitas",
      src = NULL,
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = TRUE,
      border_level = 0,
      icon = argonIcon("money-coins"),
      status = "primary",
      background_color = NULL,
      gradient = FALSE, 
      floating = TRUE,
      
      argonRow(
        argonColumn(
          width = 12,
          argonH1("Informasi Umum", display = 4),
          h5("Langkah 1: menentukan informasi umum untuk data yang dibangun"),
          br(),
          fluidRow(
            column(2,
                   selectInput(("sut_newPro"),"Sistem Usaha Tani",choices = c("MONOKULTUR","AGROFORESTRI")),
            ),
            column(2,
                   textInput("kom_newPro","Komoditas", value = NULL ),
            ),
            column(2,
                   selectInput("selected_provinsi_newPro",
                               "Pilih Provinsi:",
                               choices = ""),
            ),
            column(2,
                   textInput("selected_wilayah_newPro","Wilayah", value = NULL ),
            ),
          ),
          
          fluidRow(
            column(2,
                   selectInput(("th_newPro"),"Tahun",selected = as.integer(format(Sys.Date(), "%Y")),choices = c(1995:as.integer(format(Sys.Date(), "%Y"))) ),
            ),
            column(2,
                   selectInput(("tipeLahan_newPro"),"Tipe Lahan",choices = c("MINERAL","GAMBUT") ),
                   
            ),
            column(2,
                   selectInput(("tipeKebun_newPro"),"Tipe Lahan",choices = c("CROP", "SMALLHOLDER") ),
                   
            ),
            column(4,
                   br()
            ),
            column(2,
                   br(),
                   actionButton(("asumsiMakro_button_newPro"),"Tentukan Asumsi Makro",icon("paper-plane"),style="color: white; 
                         background-color: green;")
                   #useShinyalert()
                   
            )
          )
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowMakro_newPro')
          
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowTable_newPro')
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowResult_newPro')
        )
      ),
      
    )
  )
  
  
)
