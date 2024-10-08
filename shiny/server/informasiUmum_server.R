# Section informasi umum pamTemplate---------------------------------------------
# filterInfo <- reactive({
#   readData <-komoditas 
#             choices = komoditas %>%
#               filter(sut == input$sut) 
#   readData
# })


# filterInfo <- reactiveValues(
#   sut = input$sut,
#   nama_komoditas = input$kom
# )
# 
# observe({
#   updateSelectInput(
#     session,
#     "kom",
#     choices = filterInfo$sut %>%
#       select(nama_komoditas) %>%
#       .[[1]]
#   )
# })
# 
# observe({
#   updateSelectInput(
#     session,
#     "selected_prov",
#     choices = filterInfo$nama_komoditas %>%
#       select(prov) %>%
#       .[[1]]
#   )
# })
# 
# 
# observe({
#   updateSelectInput(
#     session,
#     "selected_wilayah",
#     choices = filterInfo() %>%
#       select(wilayah) %>%
#       .[[1]]
#   )
# })
# 
# observe({
#   updateSelectInput(
#     session,
#     "th",
#     choices = filterInfo() %>%
#       select(tahun_analisis) %>%
#       .[[1]]
#   )
# })
# 
# observe({
#   updateSelectInput(
#     session,
#     "tipeLahan",
#     choices = filterInfo() %>%
#       select(tipe_lahan) %>%
#       .[[1]]
#   )
# })


observe({
  updateSelectInput(
    session,
    "sut",
    choices = komoditas %>%
      filter(tipe_landuse == input$tipe_landuse) %>%
      select(sut) %>%
      .[[1]]
  )
})


observe({
  updateSelectInput(
    session,
    "kom",
    choices = komoditas %>%
      filter(tipe_landuse == input$tipe_landuse) %>%
      filter(sut == input$sut) %>%
      select(nama_komoditas) %>%
      .[[1]]
  )
})



observe({
  updateSelectInput(
    session,
    "selected_prov",
    choices = komoditas %>%
      filter(sut == input$sut) %>%
      filter(nama_komoditas == input$kom) %>%
      select(prov) %>%
      .[[1]]
  )
})



observe({
  updateSelectInput(
    session,
    "selected_wilayah",
    choices = komoditas %>%
      filter(sut == input$sut) %>%
      filter(nama_komoditas == input$kom) %>%
      filter(prov == input$selected_prov) %>%
      select(wilayah) %>%
      .[[1]]
  )
})


observe({
  updateSelectInput(
    session,
    "th",
    choices = komoditas %>%
      filter(sut == input$sut) %>%
      filter(nama_komoditas == input$kom) %>%
      filter(prov == input$selected_prov) %>%
      filter(wilayah == input$selected_wilayah) %>%
      select(tahun_analisis) %>%
      .[[1]]
  )
})

observe({
  updateSelectInput(
    session,
    "tipeLahan",
    choices = komoditas %>%
filter(sut == input$sut) %>%
filter(nama_komoditas == input$kom) %>%
filter(prov == input$selected_prov) %>%
filter(wilayah == input$selected_wilayah) %>%
filter(tahun_analisis == input$th) %>%
select(tipe_lahan) %>% 
  .[[1]]
  )
})


# End - Section informasi umum pamTemplate---------------------------------------------

# Section informasi umum pamParsial---------------------------------------------
observe({
  updateSelectInput(
    session,
    "kom_par",
    choices = komoditas %>%
      filter(sut == input$sut_par) %>%
      select(nama_komoditas) %>%
      .[[1]]
  )
})

observe({
  updateSelectInput(
    session,
    "selected_prov_par",
    choices = komoditas %>%
      filter(sut == input$sut_par) %>%
      filter(nama_komoditas == input$kom_par) %>%
      select(prov) %>%
      .[[1]]
  )
})

observe({
  updateSelectInput(
    session,
    "selected_wilayah_par",
    choices = komoditas %>%
      filter(sut == input$sut_par) %>%
      filter(nama_komoditas == input$kom_par) %>%
      filter(prov == input$selected_prov_par) %>%
      select(wilayah) %>%
      .[[1]]
  )
})

observe({
  updateSelectInput(
    session,
    "th_par",
    choices = komoditas %>%
      filter(sut == input$sut_par) %>%
      filter(nama_komoditas == input$kom_par) %>%
      filter(prov == input$selected_prov_par) %>%
      filter(wilayah == input$selected_wilayah_par) %>%
      select(tahun_analisis) %>%
      .[[1]]
  )
})

observe({
  updateSelectInput(
    session,
    "tipeLahan_par",
    choices = komoditas %>%
      filter(sut == input$sut_par) %>%
      filter(nama_komoditas == input$kom_par) %>%
      filter(prov == input$selected_prov_par) %>%
      filter(wilayah == input$selected_wilayah_par) %>%
      filter(tahun_analisis == input$th_par) %>%
      select(tipe_lahan) %>%
      .[[1]]
  )
})

# End - Section informasi umum pamParsial---------------------------------------------
