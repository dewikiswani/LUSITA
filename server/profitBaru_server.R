


observe({
  updateSelectInput(session,
                    "selected_provinsi_newPro",
                    choices =sort(unique(indonesia$provinsi)))
})



# End - Section informasi umum new---------------------------------------------



# Section asumsi makro NEW---------------------------------------------
reactData_newPro <- reactiveValues(
  tableP1 = NULL, #price input
  tableP2 = NULL, #price output
  tableIO1 = NULL, #io input
  tableIO2 = NULL, #io output
  tableCapP = NULL, #capital privat
  tableCapS = NULL #capital sosial
)

data_newPro <- reactive({
  # informasi umum
  sut <- input$sut_newPro
  kom <- toupper(input$kom_newPro)
  wilayah <- input$selected_wilayah_newPro
  th <- input$th_newPro
  tipeLahan <- input$tipeLahan_newPro
  tipeKebun <- input$tipeKebun_newPro
  waktuInput<-Sys.time()
  waktuInput<-gsub(" ","_",waktuInput,fixed = TRUE)
  waktuInput<-gsub(":","-",waktuInput,fixed = TRUE)
  reactData$timeInput <- waktuInput
  jamInput <- strsplit(waktuInput,"_")[[1]][2]
  tanggalInput <- strsplit(waktuInput,"_")[[1]][1]
  
  
  
  combineDef <- list(
    sut=sut,
    kom=kom,
    wilayah = wilayah,
    th=th,
    tipeLahan = tipeLahan,
    tipeKebun = tipeKebun,
    waktuInput = waktuInput,
    jamInput = jamInput,
    tanggalInput = tanggalInput)
  
  # save data untuk setiap perubahan
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  saveRDS(combineDef,file = fileName)
  combineDef
  
})

observeEvent(c(input$sut_newPro,input$kom_newPro,input$selected_provinsi_newPro,input$th_newPro,input$tipeLahan_newPro,input$tipeKebun_newPro), {
  
  removeUI(selector='#showMakro_newPro')
  removeUI(selector='#showTable_newPro')
  removeUI(selector='#showButton_newPro')
  removeUI(selector='#showResult_newPro')
})




observeEvent(input$asumsiMakro_button_newPro, {
  # browser()
  if(input$kom_newPro == ""){
    shinyalert("Gagal!", "User harus mendefinisikan komoditas", type = "error")
    # modalKomNull()
  }else{
    data_newPro()
    
    insertUI(selector='#uiShowMakro_newPro',
             where='afterEnd',
             ui= uiOutput('showMakro_newPro'))
  }
}) 






output$showMakro_newPro <- renderUI({
  argonRow(
    argonColumn(
      width = 12,
      argonH1("Asumsi Makro", display = 4),
      h5("Langkah 2: menentukan asumsi makro untuk data yang dibangun"),
      br(),
      fluidRow(
        column(3,
               sliderInput(("rate.p_newPro"), "Discount Rate Private", 7.1 ,min = 0, max = 15, step = 0.01)
        ),
        column(4,
               sliderInput(("nilai.tukar_newPro"), "Nilai Tukar Rupiah", 15800 ,min = 10000, max = 20000, step = 1)
        ),
        column(2,
               br(),
               actionButton(("pilihBarisOutput_newPro"),"Membangun Tabel PAM",icon("paper-plane"),style="color: white; 
                         background-color: green;")
        )
      )
    )
  )
})

observeEvent(c(input$rate.p_newPro,input$rate.s_newPro,input$nilai.tukar_newPro), {
  removeUI(selector='#showTable_newPro')
  removeUI(selector='#showButton_newPro')
  removeUI(selector='#showResult_newPro')
})

# End - Section asumsi makro NEW---------------------------------------------
################################################################################
#                                                                              #
#                    TABEL KUANTITAS OUTPUT                                     #
#                                                                              #
################################################################################

observeEvent(input$pilihBarisOutput_newPro,{
  removeUI(selector='#showTable_newPro')
  removeUI(selector='#showButton_newPro')
  removeUI(selector='#showResult_newPro')
  showModal(modalPilihBarisOutput_newPro())
  
})

modalPilihBarisOutput_newPro <- function(failed = FALSE) {
  modalDialog( 
    footer=tagList(
      actionButton(("bangunKuantitasOut_newPro"), "Lanjut",style="color: white;background-color: green;")
    ),
    argonTabSet(
      id = "tabNew",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "l",
      width = 12,
      argonTab(
        tabName = "Menentukan Jumlah Komponen (Baris) Pada Tabel Kuantitas bagian Output",
        active = T,
        h3("Berapa jumlah komponen (baris) yang akan user bangun pada Output-Tabel Kuantitas?"),
        selectInput(("pilihTambahBaris_output_newPro"),
                    " ",
                    choices = c(1:10),selected = if (is.null(reactData_newPro$tableIO2)){
                      1
                    } else {nrow(reactData_newPro$tableIO2)}
                    ,width = "800px")
      ))
    ,
    size="l",
    easyClose = FALSE)
}

observeEvent(input$bangunKuantitasOut_newPro,{
  showModal(modalTabelKuantitasOut_newPro())
})


modalTabelKuantitasOut_newPro <- function(failed = FALSE) {
  modalDialog( 
    footer=tagList(
      actionButton("backtoRowOutput","Kembali"),
      actionButton(("pilihBarisInput_newPro"), "Simpan Tabel dan Lanjutkan Membangun Input pada Tabel Kuantitas",style="color: white;background-color: green;")
    ),
    argonTabSet(
      id = "tabNew2",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "l",
      width = 12,
      argonTab(
        tabName = "Langkah 3: Mengisi Tabel Kuantitas Bagian Output",
        active = T,
        h3("Tabel Kuantitas (OUTPUT)", align = "center"),
        rHandsontableOutput('kuantitasOutput_newPro')
      ))
    ,
    size="l",
    easyClose = FALSE)
}

observeEvent(input$backtoRowOutput,{
  showModal(modalPilihBarisOutput_newPro())
})


output$kuantitasOutput_newPro <- renderRHandsontable({
  rhandsontable(valIO2_newPro(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 300,
  )
})

valIO2_newPro <- eventReactive(input$bangunKuantitasOut_newPro,{
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  if(is.null(dataDefine$ioOutput)){
    readDataTemplate <- read.table(paste0("data/template/tabel pam kosong komponen output",".csv"), header = T, sep = ",")
    yearIO <- 30 #tahun daur tanam
    
    inputData <- readDataTemplate[1:input$pilihTambahBaris_output_newPro,]
    ioInput <- inputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
    
    ioInput$jenis<- as.character(ioInput$jenis)
    ioInput$unit<- as.character(ioInput$unit)
    ioInput[,c(4:33)] <- as.numeric(as.character(ioInput[,c(4:33)]))
    
    reactData_newPro$tableIO2 <- ioInput
    rownames(reactData_newPro$tableIO2) <- c(1:nrow(reactData_newPro$tableIO2))
    reactData_newPro$tableIO2
  }else{
    reactData$tableIO2 <- dataDefine$ioOutput
    reactData$tableIO2
  }
  
})


################################################################################
#                                                                              #
#                    TABEL KUANTITAS INPUT                                     #
#                                                                              #
################################################################################

observeEvent(input$pilihBarisInput_newPro,{
  # save data untuk setiap perubahan
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  editNew<-as.data.frame(hot_to_r(input$kuantitasOutput_newPro))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  
  dataDefine$ioOutput <- editNew
  saveRDS(dataDefine,file = fileName)
  
  showModal(modalPilihBarisInput_newPro())
})

modalPilihBarisInput_newPro <- function(failed = FALSE) {
  modalDialog( 
    footer=tagList(
      actionButton(("bangunKuantitas_newPro"), "Lanjut",style="color: white;background-color: green;")
    ),
    argonTabSet(
      id = "tabNew",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "l",
      width = 12,
      argonTab(
        tabName = "Menentukan Jumlah Komponen (Baris) Pada Tabel Kuantitas bagian Input",
        active = T,
        h3("Berapa jumlah komponen (baris) yang akan user bangun pada Input-Tabel Kuantitas?"),
        selectInput(("pilihTambahBaris_input_newPro"),
                    " ",
                    choices = c(5:90),selected = if (is.null(reactData_newPro$tableIO1)){
                      28
                    } else {nrow(reactData_newPro$tableIO1)}
                    ,width = "800px")
      ))
    ,
    size="l",
    easyClose = FALSE)
}

observeEvent(input$bangunKuantitas_newPro,{
  showModal(modalTabelKuantitasIn_newPro())
})


modalTabelKuantitasIn_newPro <- function(failed = FALSE) {
  modalDialog( 
    footer=tagList(
      actionButton("backtoRowInput","Kembali"),
      actionButton(("bangunTabelHarga_newPro"), "Simpan Tabel dan Lanjutkan Membangun Tabel Harga",style="color: white;background-color: green;")
    ),
    argonTabSet(
      id = "tabNew",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "l",
      width = 12,
      argonTab(
        tabName = "Langkah 4: Mengisi Tabel Kuantitas Bagian Input",
        active = T,
        h3("Tabel Kuantitas (INPUT)", align = "center"),
        rHandsontableOutput('kuantitasInput_newPro')
      ))
    ,
    size="l",
    easyClose = FALSE)
}

observeEvent(input$backtoRowInput,{
  # browser()
  showModal(modalPilihBarisInput_newPro())
})



output$kuantitasInput_newPro <- renderRHandsontable({
  rhandsontable(valIO1_newPro(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 600,
  )
})

valIO1_newPro <- eventReactive(input$bangunKuantitas_newPro,{
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  if(is.null(dataDefine$ioInput)){
    readDataTemplate <- read.table(paste0("data/template/tabel pam kosong",".csv"), header = T, sep = ",")
    yearIO <- 30 #tahun daur tanam
    
    inputData <- readDataTemplate[1:input$pilihTambahBaris_input_newPro,]
    ioInput <- inputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
    ioInput$komponen <- as.character(ioInput$komponen)
    ioInput$jenis<- as.character(ioInput$jenis)
    ioInput$unit<- as.character(ioInput$unit)
    ioInput[,c(4:33)] <- as.numeric(as.character(ioInput[,c(4:33)]))
    
    reactData_newPro$tableIO1 <- ioInput
    reactData_newPro$tableIO1
  }else{
    reactData$tableIO1 <- dataDefine$ioInput
    reactData$tableIO1
  }
})


observeEvent(input$bangunTabelHarga_newPro,{
  # save data untuk setiap perubahan
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  editNew<-as.data.frame(hot_to_r(input$kuantitasInput_newPro))
  editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  
  
  dataDefine$ioInput <- editNew
  saveRDS(dataDefine,file = fileName)
  
  # show modal
  showModal(modalTabelHarga_newPro())
  
})




################################################################################
#                                                                              #
#                                 BUTTON HARGA                                 #
#                                                                              #
################################################################################
modalTabelHarga_newPro <- function(failed = FALSE) {
  modalDialog( 
    footer=tagList(
      actionButton(("batalButton"), "Batal", style="color: white;background-color: red;"),
      actionButton(("capitalButton_newPro"), "Simpan Tabel dan Jalankan Analisis",style="color: white;background-color: green;")
    ),
    argonTabSet(
      id = "tabNew3",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "sm",
      width = 12,
      argonTab(
        tabName = "Langkah 5: Mengisi Tabel Harga",
        active = T,
        h3("Tabel Harga Output", align = "center"),
        rHandsontableOutput('hargaOutput_newPro'),
        br(),
        h3("Tabel Harga Input", align = "center"),
        rHandsontableOutput('hargaInput_newPro')
      ))
    ,
    size="l",
    easyClose = FALSE)
}

observeEvent(input$batalButton,{
  # browser()
  removeModal()
})

output$hargaOutput_newPro <- renderRHandsontable({
  rhandsontable(valP2_newPro(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 100,
  )
})


valP2_newPro <- eventReactive(input$bangunTabelHarga_newPro,{
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  if(is.null(dataDefine$priceInput) & is.null(dataDefine$priceOutput)){
    reactData$tableP2 <- dataDefine$ioOutput[,1:2]
    
    unit.harga <- data.frame(matrix("rp",ncol = 1,nrow = nrow(reactData$tableP2)))
    unit.harga[] <- lapply(unit.harga, as.character)
    colnames(unit.harga) <- "unit.harga"
    harga <- data.frame(matrix(0,ncol = 1,nrow = nrow(reactData$tableP2)))
    colnames(harga) <- "harga"
   
    reactData$tableP2 <- cbind(reactData$tableP2,unit.harga,harga)
    reactData$tableP2
  }else{
    reactData$tableP2 <- dataDefine$priceOutput
    reactData$tableP2
  }
})

output$hargaInput_newPro <- renderRHandsontable({
  rhandsontable(valP1_newPro(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 600,
  )
})


valP1_newPro <- eventReactive(input$bangunTabelHarga_newPro,{
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  if(is.null(dataDefine$priceInput) & is.null(dataDefine$priceOutput)){
    reactData$tableP1 <- dataDefine$ioInput[,1:2]
    
    unit.harga <- data.frame(matrix("rp",ncol = 1,nrow = nrow(reactData$tableP1)))
    unit.harga[] <- lapply(unit.harga, as.character) #ubah dr faktor jd char, spy masih bisa diedit
    colnames(unit.harga) <- "unit.harga"
    harga <- data.frame(matrix(0,ncol = 1,nrow = nrow(reactData$tableP1)))
    colnames(harga) <- "harga"
    
    reactData$tableP1 <- cbind(reactData$tableP1,unit.harga,harga)
    reactData$tableP1
  }else{
    reactData$tableP1 <- dataDefine$priceInput
    reactData$tableP1
  }
})

observeEvent(input$capitalButton_newPro,{
  # save data untuk setiap perubahan
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  editNewP1<-as.data.frame(hot_to_r(input$hargaInput_newPro))
  editNewP1[is.na(editNewP1)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  
  editNewP2<-as.data.frame(hot_to_r(input$hargaOutput_newPro))
  editNewP2[is.na(editNewP2)] <- 0 
  
  dataDefine$priceInput <- editNewP1
  dataDefine$priceOutput <- editNewP2
  
  saveRDS(dataDefine,file = fileName)
  
  data.gab_newPro()
  
  showModal(modalTabelCapital_newPro())
})

modalTabelCapital_newPro <- function(failed = FALSE) {
  modalDialog( 
    footer=tagList(
      actionButton(("running_button_newPro"), "Jalankan Analisis",style="color: white;background-color: green;")
    ),
    argonTabSet(
      id = "tabNew",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "l",
      width = 12,
      argonTab(
        tabName = "Langkah 6: Jalankan analisis",
        active = T,
        h3("Selamat!!! Anda sudah selesai melakukan semua langkah analisa, hasil analisa akan ditampilkan setelah anda menekan tombol dibawah ini"),
      ))
    ,
    size="l",
    easyClose = FALSE)
}





################################################################################
#                                                                              #
#                                   SHOW TABEL                                 #
#                                                                              #
################################################################################
# Section tampilkan tabel---------------------------------------------
observeEvent(c(input$running_button_newPro), {
  # browser()
  removeModal()
  removeUI(selector='#showTable_newPro')
  removeUI(selector='#showResult_newPro')
  insertUI(selector='#uiShowTable_newPro',
           where='afterEnd',
           ui= uiOutput('showTable_newPro'))
  
  insertUI(selector='#uiShowResult_newPro',
           where='afterEnd',
           ui= uiOutput('showResult_newPro'))
}) 

output$showTable_newPro <- renderUI({
  argonRow(
    argonColumn(
      width = 12,
      argonH1("Hasil", display = 4),
      # h5("Menampilkan Hasil "),
      
      # jika tdk bisa jadi input buttton maka coba ubah nama action  buttonnya sepertinya conflict dengan script lain
      argonTabSet(
        id = "tab1_newPro",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Hasil Analisis",
          active = T,
          dataTableOutput("tableResultBAU2_newPro"),
          style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
        ),
        argonTab(
          tabName = "Tabel Harga",
          active = F,
          dataTableOutput("showTablePrice_newPro"),
          style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
        ),
        argonTab(
          tabName = "Tabel Kuantitas",
          active = F,
          dataTableOutput(("showTableKuantitas_newPro")),
          style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
        )
      ),
    )
  )
})

output$showTablePrice_newPro <- renderDataTable({
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  dataView <- rbind(dataDefine$priceInput, dataDefine$priceOutput)
  dataView[is.na(dataView)] <- 0 #NA replace with zero
  no.id <- as.numeric(1:nrow(dataView))
  rownames(dataView) <- no.id
  dataView
})

output$showTableKuantitas_newPro <- renderDataTable({
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  dataView <- rbind(dataDefine$ioInput, dataDefine$ioOutput)
  dataView[is.na(dataView)] <- 0 #NA replace with zero
  no.id <- as.numeric(1:nrow(dataView))
  rownames(dataView) <- no.id
  dataView
  
})

output$tableResultBAU2_newPro <- renderDataTable({
  dataShow <- data.frame(data.gab_newPro()$tabel2)
  dataShow
  # datatable(dataShow, option=list(dom = "ft"))
  
})





# End - Section tampilkan tabel ---------------------------------------------

################################################################################
#                                                                              #
#                                RESULT                                        #
#                                                                              #
################################################################################

output$showResult_newPro <- renderUI({
  fluidPage(
    fluidRow(
      column(12,
             id = 's1',
             tags$style('#s1 {
                           background-color: #4682B4;
                           }'),
             h1(paste0("Scatter Plot Profit Tahunan dan Kumulatif"), align = "center",style = "color: white;")
      ),
      column(6,
             plotlyOutput('showPlotProfPrivat_newPro') #grafik profit tahunan
      ),
      column(6,
             plotlyOutput('profitPlotKumP_newPro') #grafik profit kumulatif
      )
    ),
    br(),
    br(),
    fluidRow(
      
      column(2,
             # actionButton(("saveNewPAM_newPro"),"Simpan Data",icon("paper-plane"),style="color: white;background-color: green;"),
             downloadButton(("download_newPro"),"Download Hasil",icon("paper-plane"),style="color: white;background-color: green;")
             # br()
             # ,tags$div(id='teksNewPamSave')
      )
    )
    
    
  )
})





data.gab_newPro <- reactive({
  # eventReactive(c(input$running_button_newPro),{
  
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  dataDefine$rate.p <- input$rate.p_newPro
 
  dataDefine$nilai.tukar <- input$nilai.tukar_newPro
  
  #### io  ####    
  io.in <-  dataDefine$ioInput
  io.in <- cbind(grup="input",io.in)
  io.out <-  dataDefine$ioOutput
  io.out <- cbind(grup="output",io.out)
  
  io.in[is.na(io.in)] <- 0 #NA replace with zero
  io.out[is.na(io.out)] <- 0
  io.all <- rbind(io.in,io.out) #combine all data input-output
  io.all <- cbind(status="general", io.all) #add variable status
  io.all <- io.all %>% mutate_if(is.factor,as.character) #change factor var to char var
  
  
  yearIO <- ncol(io.in)-4 #banyaknya tahun pada tabel io 
  
  #### price ####
  price.in <-  dataDefine$priceInput
  price.in <- cbind(grup="input",price.in)
  price.out <-  dataDefine$priceOutput
  price.out <- cbind(grup="output",price.out)
  price.in[is.na(price.in)] <- 0
  price.out[is.na(price.out)] <- 0
  price.all <- rbind(price.in, price.out)
  
  p.price<-price.all[-6]
  p.year<-data.frame(replicate(yearIO,p.price$harga)) #replicate nilai private price sebanyak n tahun
  colnames(p.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
  p.price<-cbind(status="harga" ,p.price[c(1:4)],p.year)
  p.price <- p.price %>% mutate_if(is.factor,as.character) #change factor var to char var
  
  
  data.gab <- bind_rows(io.all,
                          p.price) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
    
    # hitung npv --------------------------------------------------------------
    dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
    dataPrivat <- filter(data.gab,status == c("harga")) #filter data private price
    p.budget <- dataGeneral[-(c(1:5,36))] * dataPrivat[-c(1:5,36)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
    p.budget <- cbind(dataGeneral[c(1:4)],dataPrivat[36],p.budget) #memunculkan kembali variabel 1 sd 5
    p.budget <- p.budget %>%
      mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
    
    
    ################ penghitungan NPV
    p.cost.input <- p.budget %>%
      filter(str_detect(grup,"input"))
    
    p.sum.cost<- p.cost.input[,-(1:5)] %>%
      colSums(na.rm = T)
    
    p.rev.output <- p.budget %>%
      filter(str_detect(grup,"output"))
    
    p.sum.rev <- p.rev.output[,-(1:5)] %>%
      colSums(na.rm = T)
    p.profit <- p.sum.rev - p.sum.cost
    profit0 <- 0
    p.profit<-c(profit0,p.profit)
    npv.p<-npv(dataDefine$rate.p/100,p.profit)
    hsl.npv<-data.frame(npv.p)
    colnames(hsl.npv) <- c("NPV (Rp/Ha)")
    rownames(hsl.npv)<-c("Value")
    hsl.npv
    
    
    hsl.npv.us<-npv.p/dataDefine$nilai.tukar
    hsl.npv.us<-data.frame(hsl.npv.us)
    colnames(hsl.npv.us) <- c("NPV (USD/Ha)")
    rownames(hsl.npv.us) <- c("Value")
    hsl.npv.us
    # ending  npv --------------------------------------------------------------
    
  # hitung EAE -------------------------------------------------------------- 
  atas <- dataDefine$rate.p*((1+dataDefine$rate.p/100)^yearIO)
  bawah <- (1+dataDefine$rate.p/100)^yearIO-1
  eae <- data.frame(npv.p*atas/bawah)
  colnames(eae)<-c("Equal Annual Equivalent (EAE)")
  rownames(eae) <- c("Value")
  eae
  
  ################ penghitungan NLC dan LC
  
  p.tot.cost<- sum(p.sum.cost)
  p.labor.input <- p.budget %>% filter(str_detect(komponen,c("tenaga kerja|tk|kerja|tenaga")))
  p.sum.labor <- p.labor.input[,-(1:5)] %>%
    sum(na.rm = T)
  
  nlc.p <- (p.tot.cost - p.sum.labor)/1000000
  nlcost <- data.frame(nlc.p)
  colnames(nlcost)<-c("Non Labor Cost for 1 cycle (MRp/ha)")
  rownames(nlcost) <- c("Value")
  nlcost
  
  lcost <- data.frame(p.sum.labor/1000000)
  colnames(lcost)<-c("Labor Cost for 1 cycle (MRp/ha)")
  rownames(lcost) <- c("Value")
  lcost
  
  # ending  nlc ------------------------------------------------------- 
  
  # hitung EC --------------------------------------------------------------
  ############# PERHITUNGAN ESTABLISHMENT COST
  p.ec <- p.sum.cost[[1]]/1000000
  ec <- data.frame(p.ec)
  ec<-data.frame(p.ec)
  colnames(ec)<-c("Establishment cost (1st year only, MRp/ha)")
  rownames(ec) <- c("Value")
  ec
  
  # ending  EC ------------------------------------------------------- 
  
  # hitung hp --------------------------------------------------------------
  ############# PERHITUNGAN HARVESTING PRODUCT
  fil.prod <- dataGeneral %>%  filter(str_detect(grup,"output")) #filter io untuk grup output (hasil panen)
  fil.prod <- fil.prod %>%  filter(str_detect(komponen,"utama"))
  sum.prod <- fil.prod[,-c(1:5,36)] %>%
    colSums(na.rm = T)
  tot.prod <- sum(sum.prod)
  
  fil.labor <- dataGeneral %>%  filter(str_detect(komponen, c("tenaga kerja|tk|kerja|tenaga")))
  fil.labor <- filter(fil.labor, str_detect(unit, c("hok|HOK|pers-day")))
  sum.labor <- fil.labor[,-c(1:5,36)] %>%
    colSums(na.rm = T)
  tot.labor <- sum(sum.labor)
  
  hp <- data.frame(tot.prod/tot.labor)/1000 # karena ton jadi di bagi 1000
  colnames(hp)<-c("Harvesting Product (ton/HOK)")
  rownames(hp) <- c("Value")
  hp
  # ending  hp ------------------------------------------------------- 
  
  # hitung lr --------------------------------------------------------------
  ############# PERHITUNGAN LABOR REQ FOR EST
  lr <- data.frame(sum.labor[[1]]) #pekerja pada tahun 1
  colnames(lr)<-c("Labor Req for Est (1st year only)")
  rownames(lr) <- c("Value")
  lr
  
  # ending  lr ------------------------------------------------------- 
  
  
  
  # hitung PI dan biaya pembangunan tahun pertama--------------------------------------------------------------
  costy1 <-p.budget %>%
    filter(str_detect(grup,"input"))%>%
    select('Y1')%>%
    colSums(na.rm = T)
  
  sumprofit <- p.profit %>%
    sum()
  
  pi <- data.frame(sumprofit/costy1)
  colnames(pi)<-c("Profitability Index")
  rownames(pi) <- c("Value")
  pi
  # ending   --------------------------------------------------------------
  
  # hitung BCR--------------------------------------------------------------
  costYear <-p.budget %>%
    filter(str_detect(grup,"input"))%>%
    select(contains('Y'))%>%
    colSums(na.rm = T)
  
  pvCost <- presentValue(costYear,c(1:length(costYear)),dataDefine$rate.p/100)
  
  revenueYear <- p.budget %>%
    filter(str_detect(grup,"output"))%>%
    select(contains('Y'))%>%
    colSums(na.rm = T)
  
  pvRevenue <- presentValue(revenueYear,c(1:length(revenueYear)),dataDefine$rate.p/100)
  
  bcr <- pvRevenue/pvCost
  
  bcr <- data.frame(bcr)
  colnames(bcr)<-c("Gross Benefit Cost Ratio")
  rownames(bcr) <- c("Value")
  bcr
  # ending   --------------------------------------------------------------
  
  # hitung RTL--------------------------------------------------------------
  revenueYearSum <- sum(revenueYear)
  nlcVal <- (p.tot.cost - p.sum.labor)
  tkQuant <- io.in %>%  filter(str_detect(komponen, c("tenaga kerja|tk|kerja|tenaga")))%>%
    select(contains('Y'))%>%
    colSums(na.rm = T)%>%
    sum()
  rtl <- (revenueYearSum-nlcVal)/tkQuant
  
  rtl <- data.frame(rtl)
  colnames(rtl)<-c("Return to Labor (Rp)")
  rownames(rtl) <- c("Value")
  rtl
  # ending   --------------------------------------------------------------
  
  # tampilan rate.p, rate.s, nilai tukar rupiah --------------------------------------------------------------
  showRateP <- data.frame(dataDefine$rate.p)
  colnames(showRateP)<-c("Discount Rate Private")
  rownames(showRateP) <- c("Value")
  showRateP
  
  showExRate <- data.frame(dataDefine$nilai.tukar)
  colnames(showExRate)<-c("Nilai Tukar Rupiah")
  rownames(showExRate) <- c("Value")
  showExRate
  
  p.profit.ha <- p.profit[-1]
  p.profit.kum <- cumsum(p.profit.ha)
  
  
  
  tabel2 <- cbind(hsl.npv,hsl.npv.us,eae,rtl,ec,bcr,lcost, nlcost,hp,pi,lr, showRateP,showExRate)
  tabel2 <- t(tabel2)
  tabel2 <- list(tabel2=tabel2)
  
  
  # RESULT 
  dataDefine$eae <- eae
  dataDefine$npv <- hsl.npv
  dataDefine$npv <- hsl.npv.us
  dataDefine$ec <- ec
  dataDefine$hp <- hp
  dataDefine$lr <- lr
  
  dataDefine$rtl <- rtl
  dataDefine$bcr <- bcr
  dataDefine$pi <- pi
  dataDefine$lcost <- lcost
  dataDefine$nlcost <- nlcost
  
  dataDefine$nlcost <- nlcost
  dataDefine$nlcost <- nlcost
  
  dataDefine$p.profit.ha <- p.profit.ha
  dataDefine$p.profit.kum <- p.profit.kum
  
  dataDefine$tabel2 <- tabel2
  
  saveRDS(dataDefine,file = fileName)
  tabel2
})


preparePlotProfitYear <- eventReactive(c(input$running_button_newPro),{
  
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  
  dat_df.new <- data.frame()
  dat_df.new <- data.frame(no = seq(1,length(dataDefine$p.profit.ha), 1),
                           profit.tahunan=dataDefine$p.profit.ha,
                           kom = dataDefine$kom)
  
  
  
  yes <- dat_df.new %>%
    group_by(kom) %>%
    plot_ly(x=~no, y=~profit.tahunan, type='scatter', color=~kom, mode="lines+markers", yaxis=~kom, colors = "green") %>%
    layout(xaxis=list(title="Tahun", domain = list(0.15, 0.95)), yaxis=list(title="Profit Tahunan (Rp/Ha)"))%>%
    layout(legend = list(title=list(text='<b> Komoditas (Urutan) </b>'),
                         orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.95, y = -0.25))
  yes
})

output$showPlotProfPrivat_newPro <- renderPlotly({
  preparePlotProfitYear()
})


output$profitPlotKumP_newPro <- renderPlotly({
  preparePlotProfitKum()
})

preparePlotProfitKum <- eventReactive(c(input$running_button_newPro),{

  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  message(paste("File ada:",file.exists(fileName)))


  dataDefine <- readRDS(fileName)
  message(paste("Length p.profit.ha:", length(dataDefine$p.profit.ha)))

  # dat_df.new <- data.frame()

  no = 1:length(dataDefine$p.profit.ha)
  profit.kumulatif=cumsum(dataDefine$p.profit.ha)
  # kom = replicate(length(dataDefine$p.profit.ha), "coklat")
  dat_df.new <- data.frame(no ,
                           profit.kumulatif)
                           # kom)

  yes <- dat_df.new %>%
    plot_ly(x=~no, y=~profit.kumulatif, type='scatter', mode="lines+markers",  colors = "cyan") %>%
    layout(xaxis=list(title="Tahun", domain = list(0.15, 0.95)), yaxis=list(title="Profit Kumulatif (Rp/Ha)"))%>%
    layout(legend = list(title=list(text='<b> Komoditas (Urutan) </b>'),
                         orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.95, y = -0.25))
  yes
})














preparePlot_newPro <- eventReactive(c(input$running_button_newPro),{
  
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  
  dataPlot <- data.frame(
    komoditas=dataDefine$kom,
    NPV.Rp=dataDefine$npv[1,1])
  
  
  dataPlot %>%
    plot_ly(x = ~komoditas, y = ~NPV.Rp, type = "bar", color = ~komoditas)%>%
    layout(yaxis=list(title="NPV (Rp/Ha)"))
})

output$plot_newPro <- renderPlotly({
  preparePlot_newPro()
})

observeEvent(input$saveNewPAM_newPro, {
  # browser()
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  # Inisialisasi workbook kosong
  wb <- wb_workbook()

  # 1. Sheet metadata: kumpulkan semua elemen atomik berdimensi 1
  meta_names <- c()
  meta_values <- c()

  for (nm in names(dataDefine)) {
    el <- dataDefine[[nm]]
    if (is.atomic(el) && length(el) == 1) {
      meta_names <- c(meta_names, nm)
      meta_values <- c(meta_values, el)
    }
  }

  metadata_df <- as.data.frame(t(meta_values), stringsAsFactors = FALSE)
  colnames(metadata_df) <- meta_names

  wb$add_worksheet("metadata")
  wb$add_data("metadata", metadata_df)

  # 2. Sheet indikator: kumpulkan elemen kecil 1x1 data.frame seperti eae, npv, dst
  indikator_list <- c("eae", "npv", "ec", "hp", "lr", "rtl", "bcr", "pi", "lcost", "nlcost")
  indikator_values <- c()
  indikator_labels <- c()

  for (nm in indikator_list) {
    if (nm %in% names(dataDefine)) {
      val <- dataDefine[[nm]]
      if (is.data.frame(val) && nrow(val) == 1 && ncol(val) == 1) {
        indikator_values <- c(indikator_values, unlist(val))
        indikator_labels <- c(indikator_labels, colnames(val))
      }
    }
  }

  indikator_df <- as.data.frame(t(indikator_values), stringsAsFactors = FALSE)
  colnames(indikator_df) <- indikator_labels

  wb$add_worksheet("indikator")
  wb$add_data("indikator", indikator_df)

  # 3. Sheet p.profit.ha (named numeric vector)
  if ("p.profit.ha" %in% names(dataDefine)) {
    pp <- dataDefine$p.profit.ha
    df_pp <- as.data.frame(t(pp))
    colnames(df_pp) <- names(pp)
    wb$add_worksheet("p.profit.ha")
    wb$add_data("p.profit.ha", df_pp)
  }

  # 4. Sheet untuk setiap data.frame besar
  for (nm in names(dataDefine)) {
    el <- dataDefine[[nm]]

    # Hindari yang sudah ditangani
    if (is.data.frame(el) &&
        !(nm %in% c("eae", "npv", "ec", "hp", "lr", "rtl", "bcr", "pi", "lcost", "nlcost"))) {

      wb$add_worksheet(nm)
      wb$add_data(nm, el)
    }
  }

  # 5. Simpan workbook
  wb$save("C:/Users/dbodro/OneDrive - CIFOR-ICRAF/Documents/GitHub/LUSITA/data/Download/dataDefine_full.xlsx")

  
})


datasetInput <- reactive({
  datapath <- paste0("data/", input$sut_newPro, "/","^KOMODITAS BARU","/")
  fileName <- paste0(datapath,"saveData_newPro","_",
                     input$sut_newPro,"_",input$kom_newPro,"_",
                     input$selected_provinsi_newPro,"_",input$th_newPro,"_",input$tipeLahan_newPro,"_",input$tipeKebun_newPro,"_",reactData$timeInput,".rds")
  dataDefine <- readRDS(fileName)
  
  # Inisialisasi workbook kosong
  wb <- wb_workbook()
  
  # 1. Sheet metadata: kumpulkan semua elemen atomik berdimensi 1
  meta_names <- c()
  meta_values <- c()
  
  for (nm in names(dataDefine)) {
    el <- dataDefine[[nm]]
    if (is.atomic(el) && length(el) == 1) {
      meta_names <- c(meta_names, nm)
      meta_values <- c(meta_values, el)
    }
  }
  
  metadata_df <- as.data.frame(t(meta_values), stringsAsFactors = FALSE)
  colnames(metadata_df) <- meta_names
  
  wb$add_worksheet("metadata")
  wb$add_data("metadata", metadata_df)
  
  # 2. Sheet indikator: kumpulkan elemen kecil 1x1 data.frame seperti eae, npv, dst
  indikator_list <- c("eae", "npv", "ec", "hp", "lr", "rtl", "bcr", "pi", "lcost", "nlcost")
  indikator_values <- c()
  indikator_labels <- c()
  
  for (nm in indikator_list) {
    if (nm %in% names(dataDefine)) {
      val <- dataDefine[[nm]]
      if (is.data.frame(val) && nrow(val) == 1 && ncol(val) == 1) {
        indikator_values <- c(indikator_values, unlist(val))
        indikator_labels <- c(indikator_labels, colnames(val))
      }
    }
  }
  
  indikator_df <- as.data.frame(t(indikator_values), stringsAsFactors = FALSE)
  colnames(indikator_df) <- indikator_labels
  
  wb$add_worksheet("indikator")
  wb$add_data("indikator", indikator_df)
  
  # 3. Sheet p.profit.ha (named numeric vector)
  if ("p.profit.ha" %in% names(dataDefine)) {
    pp <- dataDefine$p.profit.ha
    df_pp <- as.data.frame(t(pp))
    colnames(df_pp) <- names(pp)
    wb$add_worksheet("p.profit.ha")
    wb$add_data("p.profit.ha", df_pp)
  }
  
  # 4. Sheet untuk setiap data.frame besar
  for (nm in names(dataDefine)) {
    el <- dataDefine[[nm]]
    
    # Hindari yang sudah ditangani
    if (is.data.frame(el) &&
        !(nm %in% c("eae", "npv", "ec", "hp", "lr", "rtl", "bcr", "pi", "lcost", "nlcost"))) {
      
      wb$add_worksheet(nm)
      wb$add_data(nm, el)
    }
  }
  wb
  # 5. Simpan workbook
  # wb$save("C:/Users/dbodro/OneDrive - CIFOR-ICRAF/Documents/GitHub/LUSITA/data/Download/dataDefine_full.xlsx")
  
})

output$download_newPro <- downloadHandler(
  filename = function(){
    paste0("download_result.xlsx")
  }
  ,
  content = function (file){
    datasetInput()$save(file)
  }
)
