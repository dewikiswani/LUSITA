pamBaru <- argonTabItem(
  tabName = "pamBaru",
  argonH1("Membuat PAM Baru", display = 4),
  argonRow(
    argonCard(
      width = 12,
      title = "Membuat PAM Baru",
      src = NULL,
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = TRUE,
      border_level = 0,
      icon = argonIcon("atom"),
      status = "primary",
      background_color = NULL,
      gradient = FALSE, 
      floating = TRUE,
      
      argonRow(
        argonColumn(
          width = 12,
          argonH1("Informasi Umum", display = 4),
          h5("Langkah 1: menentukan informasi umum untuk data PAM yang dibangun"),
          br(),
          fluidRow(
            column(2,
                   selectInput(("sut_new"),"Sistem Usaha Tani",choices = c("MONOKULTUR","AGROFORESTRI")),
            ),
            column(2,
                   textInput("kom_new","Komoditas", value = NULL ),
            ),
            column(2,
                   selectInput("selected_prov_new",
                               "Pilih Provinsi:",
                               choices = ""),
            ),
            column(2,
                   textInput("selected_wilayah_new","Wilayah", value = NULL ),
            ),
          ),
          
          fluidRow(
            column(2,
                   selectInput(("th_new"),"Tahun",selected = as.integer(format(Sys.Date(), "%Y")),choices = c(1995:as.integer(format(Sys.Date(), "%Y"))) ),
            ),
            column(2,
                   selectInput(("tipeLahan_new"),"Tipe Lahan",choices = c("MINERAL","GAMBUT") ),
                   
            ),
            column(2,
                   selectInput(("tipeKebun_new"),"Tipe Lahan",choices = c("CROP", "SMALLHOLDER") ),
                   
            ),
            column(4,
                   br()
            ),
            column(2,
                   br(),
                   actionButton(("asumsiMakro_button_new"),"Tentukan Asumsi Makro",icon("paper-plane"),style="color: white; 
                         background-color: green;")
                   #useShinyalert()
                   
            )
          )
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowMakro_new')
          
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowTable_new')
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          tags$div(id = 'uiShowResult_new')
        )
      ),
      
    )
  )
  
  
)
