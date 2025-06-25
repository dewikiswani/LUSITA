
#setwd("C:/dw/ICRAF/profitability/shiny")
# C:/dw/ICRAF/project R/theme 2/profitability/data/template

library(shiny)
library(argonR)
library(argonDash)
library(magrittr)

#package function npv
library(FinCal)
# library(readxl)
#spy data price yg ada nilainya bukan hanya numeric bisa terbaca yaitu pada kolom 1
library(DT)
library(dplyr)
library(stringr)
library(tidyverse)
library(rhandsontable)
library(shinyWidgets)
library(shinyalert)

# untuk tab analisis
library(shinydashboard)
library(data.table)

library(ggplot2)
library(cowplot) #ggplot2 white theme 
library(plotly)
library(lifecontingencies)



# template
source("sidebar.R")
source("navbar.R")
source("header.R")
source("footer.R")

# input file
komoditas <- read.csv("data/template/komoditas.csv", stringsAsFactors = F)
kumpulanDataJenisInputOutput <- read.csv("data/template/kumpulan jenis input output.csv")
indonesia <- read.csv("data/template/prov sampai desa.csv", stringsAsFactors = F)
# readDataAsumsiMakro <- read.table(paste0("data/template/asumsi makro",".csv"), header = T, sep = ",")



# elements
source("pamTemplate.R")
source("modulSatu.R")
source("pamBaru_modul2.R")
source("pamParsial_modul3.R")


# App
app <- shiny::shinyApp(
  ui = argonDashPage(
    title = "LUSITA",
    author = "Dewi Kiswani Bodro",
    description = "ICRAF",
    sidebar = argonSidebar,
    navbar = argonNav, 
    header = argonHeader,
    body = argonDashBody(
      argonTabItems(
        pamBaru,
        modulSatu,
        pamTemplate,
        pamParsial
        
      )
    ),
    footer = argonFooter
  )
  ,
  server = function(input, output,session) {
    ##kumpulan fungsi
    lowcase <- function(data, index.col){
      for(k in index.col){
        data[,k] <- tolower(data[,k])
      }
      return(data) 
    }
    
    # # Section informasi umum ---------------------------------------------
    source("server/informasiUmum_server.R", local = TRUE)
    # # End - Section informasi umum ---------------------------------------
    
    observeEvent(input$browser_button,{
      browser()
    })
    
    reactData <- reactiveValues(
      timeInput = NULL,
      tableP1 = NULL, #price input
      tableP2 = NULL, #price output
      tableIO1 = NULL, #io input
      tableIO2 = NULL, #io output
      tableCapP = NULL, #capital privat
      tableCapS = NULL, #capital sosial
      tableAddPupuk = NULL,
      tableAddBibit = NULL,
      tableAddPeralatan = NULL,
      tableAddTK = NULL,
      tableAddUtama =NULL,
      tableAddSampingan = NULL,
      tableAddCapPrivat = NULL,
      tableAddCapSosial = NULL,
      tableCapitalAll = NULL,
      tableScenLand = NULL,
      tableAddBahanKimia = NULL,
      tableAddTradCapital = NULL,
      tableAddTKAhli = NULL,
      tableAddTKUnskilled = NULL,
      tableAddFactorCapital = NULL
    )
    
    # Section preparation data ---------
    dataTemplate <- reactive({
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      print(paste0(datapath," SIMULASI"))
      
      readDataTemplate <- read.table(paste0(datapath,input$sut,"_",input$kom,"_",input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".csv"), header = T, sep = ",")
      readDataTemplate[is.na(readDataTemplate)] <- 0
      
      if (readDataTemplate$tipe.kebun[1] == "LARGE SCALE"){
        ##### ada tambahan kolom "bagian"
        readDataTemplate <- lowcase(readDataTemplate, c("bagian","faktor","komponen","jenis","unit.harga","unit"))
        
        yearIO <- 30 #tahun daur tanam
        
        inputData <- filter(readDataTemplate,faktor == c("input"))
        ioInput <- inputData[,c("bagian","komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
        priceInput <- inputData[,c("bagian","komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
        
        outputData <- filter(readDataTemplate,faktor == c("output"))
        ioOutput <- outputData[,c("bagian","komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
        priceOutput <- outputData[,c("bagian","komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
        
        baseScene <- filter(readDataTemplate,str_detect(bagian,"dasar"))

        
        # kondisi if else nya dari cek jumlah row di variable baseScene
        if(dim(baseScene)[1] == 0){
          cum.landScene <- NULL
        } else if(dim(baseScene)[1] > 0){
          landScene <- baseScene[,c("bagian","komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
          transpose.landScene <- data.frame(t(landScene[,c(paste0(c(rep("Y", yearIO)),1:yearIO))]))
          colnames(transpose.landScene) <- c("per.year")
          cum.landScene <- within(transpose.landScene,cummulative.land <- cumsum(per.year)) #buat kolom cumulativ land
          cum.landScene <- data.frame(t(cum.landScene))
        }
        
        # informasi umum
        sut <- input$sut
        kom <- input$kom
        wilayah <- input$selected_wilayah
        th <- input$th
        tipeLahan <- input$tipeLahan
        tipeKebun <- readDataTemplate$tipe.kebun[1]
        totalArea <- readDataTemplate$total.area[1]
        tipeData <- c("SIMULASI")
        
        # asumsi makro
        rate.p <- input$rate.p
        rate.s <- input$rate.s
        nilai.tukar <- input$nilai.tukar
        
        combineDef <- list(ioInput=ioInput,ioOutput=ioOutput,
                           priceInput=priceInput,priceOutput=priceOutput,
                           cum.landScene = cum.landScene,
                           sut=sut,
                           kom=kom,
                           wilayah = wilayah,
                           th=th,
                           tipeLahan = tipeLahan,
                           tipeKebun = tipeKebun,
                           totalArea = totalArea,
                           tipeData = tipeData,
                           rate.p = rate.p,
                           rate.s = rate.s,
                           nilai.tukar = nilai.tukar)
        
        
        # save data untuk setiap perubahan
        datapath <- paste0("data/", input$sut, "/",input$kom, "/")
        fileName <- paste0(datapath,"saveData","_",
                           input$sut,"_",input$kom,"_",
                           input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
        saveRDS(combineDef,file = fileName)
        
        
        
        print("pertama kali masuk/login. cek save data default")
        combineDef
        
        
      }else{
        readDataTemplate <- lowcase(readDataTemplate, c("faktor","komponen","jenis","unit.harga","unit"))
        
        yearIO <- 30 #tahun daur tanam
        
        inputData <- filter(readDataTemplate,faktor == c("input"))
        ioInput <- inputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
        priceInput <- inputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
        
        outputData <- filter(readDataTemplate,faktor == c("output"))
        ioOutput <- outputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
        priceOutput <- outputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga

        
        
                
        
        capital <- filter(readDataTemplate,faktor == c("modal kapital"))
        
        # kondisi if else nya dari cek jumlah row di variable capital
        if(dim(capital)[1] == 0){
          capital <- NULL
          capitalPrivat <- NULL
          capitalSosial <- NULL
        } else if(dim(capital)[1] > 0){
          capital <- capital[c("komponen","jenis","unit.harga",paste0(c(rep("Y", yearIO)),1:yearIO))]
          capitalPrivat <- filter(capital,str_detect(komponen,"privat"))
          capitalSosial <- filter(capital,str_detect(komponen,"sosial"))
        }
        
        # informasi umum
        sut <- input$sut
        kom <- input$kom
        wilayah <- input$selected_wilayah
        th <- input$th
        tipeLahan <- input$tipeLahan
        tipeKebun <- readDataTemplate$tipe.kebun[1]
        tipeData <- c("SIMULASI")
        
        # asumsi makro
        rate.p <- input$rate.p
        rate.s <- input$rate.s
        nilai.tukar <- input$nilai.tukar
        
        combineDef <- list(ioInput=ioInput,ioOutput=ioOutput,
                           priceInput=priceInput,priceOutput=priceOutput,
                           capital=capital, capitalPrivat = capitalPrivat, capitalSosial = capitalSosial,
                           sut=sut,
                           kom=kom,
                           wilayah = wilayah,
                           th=th,
                           tipeLahan = tipeLahan,
                           tipeKebun = tipeKebun,
                           tipeData = tipeData,
                           rate.p = rate.p,
                           rate.s = rate.s,
                           nilai.tukar = nilai.tukar)
        
        
        # save data untuk setiap perubahan
        datapath <- paste0("data/", input$sut, "/",input$kom, "/")
        fileName <- paste0(datapath,"saveData","_",
                           input$sut,"_",input$kom,"_",
                           input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
        saveRDS(combineDef,file = fileName)
        
        
        
        print("pertama kali masuk/login. cek save data default")
        combineDef
      }
    })
    
    resultTemplate <- reactive({
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      print(paste0(datapath," BAU"))
      
      readDataTemplate <- read.table(paste0(datapath,input$sut,"_",input$kom,"_",input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".csv"), header = T, sep = ",")
      readDataTemplate[is.na(readDataTemplate)] <- 0
      
      if (readDataTemplate$tipe.kebun[1] == "LARGE SCALE"){
        ##### ada tambahan kolom "bagian"
        readDataTemplate <- lowcase(readDataTemplate, c("bagian","faktor","komponen","jenis","unit.harga","unit"))
        
        yearIO <- 30 #tahun daur tanam
        
        inputData <- filter(readDataTemplate,faktor == c("input"))
        ioInput <- inputData[,c("bagian","komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
        priceInput <- inputData[,c("bagian","komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
        
        outputData <- filter(readDataTemplate,faktor == c("output"))
        ioOutput <- outputData[,c("bagian","komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
        priceOutput <- outputData[,c("bagian","komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
        
        baseScene <- filter(readDataTemplate,str_detect(bagian,"dasar"))
        
        
        # kondisi if else nya dari cek jumlah row di variable baseScene
        if(dim(baseScene)[1] == 0){
          cum.landScene <- NULL
        } else if(dim(baseScene)[1] > 0){
          landScene <- baseScene[,c("bagian","komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
          transpose.landScene <- data.frame(t(landScene[,c(paste0(c(rep("Y", yearIO)),1:yearIO))]))
          colnames(transpose.landScene) <- c("per.year")
          cum.landScene <- within(transpose.landScene,cummulative.land <- cumsum(per.year)) #buat kolom cumulativ land
          cum.landScene <- data.frame(t(cum.landScene))
        }
        # Asumsi makro
        # readDataAsumsiMakro <- read.table(paste0("data/template/asumsi makro",".csv"), header = T, sep = ",")
        # asumsiMakroTemplate <- filter(readDataAsumsiMakro,tahun == input$th)
        
        rate.p <- readDataTemplate$rate.p[1]
        rate.s <- readDataTemplate$rate.s[1]
        nilai.tukar <- readDataTemplate$nilai.tukar[1]
        
        # memmbuat list gabungan dataDefine
        dataDefine <- list(ioInput=ioInput,ioOutput=ioOutput,
                           priceInput=priceInput,priceOutput=priceOutput,
                           cum.landScene = cum.landScene,
                           rate.p = rate.p,
                           rate.s = rate.s,
                           nilai.tukar = nilai.tukar)
        
        #informasi umum
        dataDefine$sut <- input$sut
        dataDefine$kom <- input$kom
        dataDefine$wilayah <- input$selected_wilayah
        dataDefine$th <- input$th
        dataDefine$tipeLahan <- input$tipeLahan
        dataDefine$tipeKebun <- readDataTemplate$tipe.kebun[1]
        dataDefine$totalArea <- readDataTemplate$total.area[1]
        dataDefine$lokasi <- readDataTemplate$lokasi[1]
        dataDefine$tipeData <- c("BAU")
        
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
        
        
        yearIO <- ncol(io.in)-5 #banyaknya tahun pada tabel io 
        
        #### price ####
        price.in <-  dataDefine$priceInput
        price.in <- cbind(grup="input",price.in)
        price.out <-  dataDefine$priceOutput
        price.out <- cbind(grup="output",price.out)
        price.in[is.na(price.in)] <- 0
        price.out[is.na(price.out)] <- 0
        price.all <- rbind(price.in, price.out)
        
        p.price<-select(price.all, -harga.sosial) #remove harga sosial
        p.year<-data.frame(replicate(yearIO,p.price$harga.privat)) #replicate nilai private price sebanyak n tahun
        colnames(p.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
        p.price<-cbind(status="harga.privat" ,p.price[c("grup", "bagian","komponen", "jenis", "unit.harga")],p.year)
        p.price <- p.price %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        s.price<-select(price.all, -harga.privat) #remove harga privat
        s.year<-data.frame(replicate(yearIO,s.price$harga.sosial))
        colnames(s.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
        s.price<-cbind(status="harga.sosial",s.price[c("grup", "bagian", "komponen", "jenis", "unit.harga")],s.year)
        s.price <- s.price %>% mutate_if(is.factor,as.character) #change factor var to char
        
        price.all.year <- rbind(p.price, s.price)
        
        data.gab <- bind_rows(io.all,
                              price.all.year) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
        
        # hitung npv --------------------------------------------------------------
        dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
        dataPrivat <- filter(data.gab,status == c("harga.privat")) #filter data private price
        p.budget <- dataGeneral[-(c(1:6,ncol(dataGeneral)))] * dataPrivat[-c(1:6,ncol(dataPrivat))] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
        p.budget <- cbind(dataGeneral[c("status","grup","bagian","komponen","jenis")],dataPrivat["unit.harga"],p.budget) #memunculkan kembali variabel 1 sd 5
        p.budget <- p.budget %>%mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
        
        #perkalian antara general dengan Social Price
        dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
        s.budget <- dataGeneral[-c(1:6,ncol(dataGeneral))] * dataSosial[-c(1:6,ncol(dataSosial))]
        s.budget <- cbind(dataGeneral[c("status","grup","bagian","komponen","jenis")],dataSosial["unit.harga"],s.budget)
        s.budget <- s.budget %>%
          mutate(status = case_when(status == "general" ~ "social budget"))
        
        ################ penghitungan NPV
        p.rev.output <- p.budget %>%
          filter(str_detect(grup,"output"))
        s.rev.output <- s.budget %>%
          filter(str_detect(grup,"output"))
        
        p.sum.rev <- p.rev.output[,-(1:6)] %>%
          colSums(na.rm = T)
        s.sum.rev <- s.rev.output[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(p.sum.rev)
        # sum(s.sum.rev)
        
        p.trad.input <- p.budget %>%
          filter(str_detect(bagian,"input"))
        p.trad.input <- p.trad.input[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(p.trad.input)
        s.trad.input <- s.budget %>%
          filter(str_detect(bagian,"input"))
        s.trad.input <- s.trad.input[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(s.trad.input)
        
        p.trad.capital <- p.budget %>%
          filter(str_detect(bagian,"tradable capital"))
        p.trad.capital <- p.trad.capital[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(p.trad.capital)
        s.trad.capital <- s.budget %>%
          filter(str_detect(bagian,"tradable capital"))
        s.trad.capital <- s.trad.capital[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(s.trad.capital)
        
        p.factor.labor<- p.budget %>%
          filter(str_detect(bagian,"labor"))
        
          p.labor.admin<- p.factor.labor %>%
            filter(str_detect(komponen,"admin"))
          p.labor.admin <- p.labor.admin[,-(1:6)] %>%
            colSums(na.rm = T)
          # sum(p.labor.admin)
          
          p.labor.skill <- p.factor.labor %>%
            filter(str_detect(komponen,c("ahli")))
          p.labor.skill <- p.labor.skill[,-(1:6)] %>%
            colSums(na.rm = T)
          # sum(p.labor.skill)
          
          p.labor.unskill<- p.factor.labor %>%
            filter(str_detect(komponen,c("unskill")))
          p.labor.unskill <- p.labor.unskill[,-(1:6)] %>%
            colSums(na.rm = T)
          # sum(p.labor.unskill)
          
        s.factor.labor<- s.budget %>%
            filter(str_detect(bagian,"labor"))
          
          s.labor.admin<- s.factor.labor %>%
            filter(str_detect(komponen,"admin"))
          s.labor.admin <- s.labor.admin[,-(1:6)] %>%
            colSums(na.rm = T)
          sum(s.labor.admin)
          
          s.labor.skill <- s.factor.labor %>%
            filter(str_detect(komponen,c("ahli")))
          s.labor.skill <- s.labor.skill[,-(1:6)] %>%
            colSums(na.rm = T)
          sum(s.labor.skill)
          
          s.labor.unskill<- s.factor.labor %>%
            filter(str_detect(komponen,c("unskill")))
          s.labor.unskill <- s.labor.unskill[,-(1:6)] %>%
            colSums(na.rm = T)
          sum(s.labor.unskill)
          
          p.working.capital<- p.budget %>%
            filter(str_detect(bagian,"working capital privat"))
          p.working.capital <- p.working.capital[,-(1:6)] %>%
            colSums(na.rm = T)
          # sum(p.factor.capital)
          s.working.capital<- s.budget %>%
            filter(str_detect(bagian,"working capital sosial"))
          s.working.capital <- s.working.capital[,-(1:6)] %>%
            colSums(na.rm = T)
          
          
        p.factor.capital<- p.budget %>%
          filter(str_detect(bagian,"factor capital"))
        p.factor.capital <- p.factor.capital[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(p.factor.capital)
        s.factor.capital<- s.budget %>%
          filter(str_detect(bagian,"factor capital"))
        s.factor.capital <- s.factor.capital[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(s.factor.capital)
        
        p.total.cost <- p.trad.input + p.trad.capital + p.labor.admin + p.labor.skill + p.labor.unskill + p.working.capital + p.factor.capital
        s.total.cost <- s.trad.input + s.trad.capital + s.labor.admin + s.labor.skill + s.labor.unskill + s.working.capital + s.factor.capital
        
        
        
        
        p.profit <- p.sum.rev - p.total.cost
        s.profit <- s.sum.rev - s.total.cost
        sum(p.profit)
        sum(s.profit)
        
        value0 <- 0
        p.profit <- c(value0,p.profit)
        s.profit <- c(value0,s.profit)
        npv.p<-npv(dataDefine$rate.p/100,p.profit)
        npv.s<-npv(dataDefine$rate.s/100,s.profit)
        
        npv.p.ha <- npv.p/dataDefine$totalArea
        npv.s.ha <- npv.s/dataDefine$totalArea
        
        hsl.npv<-data.frame(PRIVATE=npv.p.ha,SOCIAL=npv.s.ha)
        
        npv.p.ha.us<-npv.p.ha/dataDefine$nilai.tukar
        npv.s.ha.us<-npv.s.ha/dataDefine$nilai.tukar
        npv.us<-data.frame(PRIVATE=npv.p.ha.us,SOCIAL=npv.s.ha.us)
        hsl.npv<-rbind(hsl.npv,npv.us)
        
        #browser()
        
        rownames(hsl.npv)<-c("NPV (Rp/Ha)", "NPV (US/Ha)")
        hsl.npv
        # ending  npv --------------------------------------------------------------
        
        
        
        
        # hitung Discounted estab cost --------------------------------------------------------------
        
        ################ penghitungan dec
        p.profit.ha <- p.profit/dataDefine$totalArea
        p.positif.cashflow <- ifelse(p.profit.ha > 0, 1, 0)
        
        p.total.cost.0 <- c(value0,p.total.cost)
        p.est.cost <- ifelse(p.positif.cashflow == 1, 0, p.total.cost.0)
      
        # p.est.cost <- length(p.positif.cashflow)
        # for(i in seq_along(p.positif.cashflow)){
        #   if(p.positif.cashflow[i] == 1){
        #     p.est.cost[i] <- 0
        #   }else{
        #     p.est.cost[i] <- p.total.cost.0[i]
        #   }
        # }
        
        s.profit.ha <- s.profit/dataDefine$totalArea
        s.positif.cashflow <- ifelse(s.profit.ha > 0, 1, 0)
        
        s.total.cost.0 <- c(value0,s.total.cost)
        s.est.cost <- ifelse(s.positif.cashflow == 1, 0, s.total.cost.0)
        
        # s.est.cost <- length(s.positif.cashflow)
        # for(i in seq_along(s.positif.cashflow)){
        #   if(s.positif.cashflow[i] == 1){
        #     s.est.cost[i] <- 0
        #   }else{
        #     s.est.cost[i] <- s.total.cost.0[i]
        #   }
        # }
        
        npv.p.dec<-npv(dataDefine$rate.p/100,p.est.cost)
        npv.s.dec<-npv(dataDefine$rate.s/100,s.est.cost)
        
        dec.p <- npv.p.dec/1000/dataDefine$totalArea
        dec.s <- npv.s.dec/1000/dataDefine$totalArea
        
        
        dec<-data.frame(PRIVATE=dec.p,SOCIAL=dec.s)
        rownames(dec)<-c("Discounted Est. Cost (MRp/Ha)")
        dec
        # ending  Discounted estab cost ------------------------------------------------------- 
        
        # hitung Year to positive cashflow --------------------------------------------------------------
        ############# PERHITUNGAN ypc
        ypc.p <- yearIO + 1 - sum(p.positif.cashflow)
        ypc.s <- yearIO + 1 - sum(s.positif.cashflow)

        ypc<-data.frame(PRIVATE=ypc.p,SOCIAL=ypc.s)
        rownames(ypc)<-c("Year to positive cashflow")
        ypc
        # ending  Year to positive cashflow ------------------------------------------------------- 
        
        # hitung Labor for establishment --------------------------------------------------------------
        # tabel labor/pekerja
        table.labor<- io.in %>%
          filter(str_detect(bagian,"labor"))
        sum.table.labor <- table.labor[,paste0(c(rep("Y", yearIO)),1:yearIO)] %>%
          colSums(na.rm = T)
        
        yearLabor <- ifelse(length(input$tahunPekerja ) == 0,25,numeric(input$tahunPekerja))
        sum.table.labor.25 <- sum.table.labor[1:yearLabor]
        total.labor <- sum(sum.table.labor.25)
        
        #tabel land/lahan
        # baseScene <- filter(readDataTemplate,str_detect(bagian,"dasar"))
        # landScene <- baseScene[,c("bagian","komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
        # transpose.landScene <- data.frame(t(landScene[,c(paste0(c(rep("Y", yearIO)),1:yearIO))]))
        # colnames(transpose.landScene) <- c("per.year")
        # cum.landScene <- within(transpose.landScene,cummulative.land <- cumsum(per.year)) #buat kolom cumulativ land
        cum.landScene <- as.matrix(cum.landScene)
      
        
        ############# PERHITUNGAN lfe
        p.total.labor.est <- ifelse(p.profit.ha[1:yearLabor+1] < 0,sum.table.labor.25 , 0)
        p.total.labor.opr <- ifelse(p.profit.ha[1:yearLabor+1] < 0,0,sum.table.labor.25)
        p.avg.labor.est <- ifelse(p.total.labor.est > 0,ifelse(p.total.labor.est == 0,0,p.total.labor.est/cum.landScene[2,1:yearLabor]) , p.total.labor.est/cum.landScene[1:yearLabor])
        p.avg.labor.opr <- ifelse(p.total.labor.opr > 0,ifelse(p.total.labor.opr == 0,0,p.total.labor.opr/cum.landScene[2,1:yearLabor]) , p.total.labor.opr/cum.landScene[1:yearLabor])
        p.avg.labor.est[is.na(p.avg.labor.est)] <- 0
        p.avg.labor.opr[is.na(p.avg.labor.opr)] <- 0
        
        s.total.labor.est <- ifelse(s.profit.ha[1:yearLabor+1] < 0,sum.table.labor.25 , 0)
        s.total.labor.opr <- ifelse(s.profit.ha[1:yearLabor+1] < 0,0,sum.table.labor.25)
        s.avg.labor.est <- ifelse(s.total.labor.est > 0,ifelse(s.total.labor.est == 0,0,s.total.labor.est/cum.landScene[2,1:yearLabor]) , s.total.labor.est/cum.landScene[1:yearLabor])
        s.avg.labor.opr <- ifelse(s.total.labor.opr > 0,ifelse(s.total.labor.opr == 0,0,s.total.labor.opr/cum.landScene[2,1:yearLabor]) , s.total.labor.opr/cum.landScene[1:yearLabor])
        s.avg.labor.est[is.na(s.avg.labor.est)] <- 0
        s.avg.labor.opr[is.na(s.avg.labor.opr)] <- 0
        
        lfe.p <- sum(p.avg.labor.est)
        lfe.s <- sum(s.avg.labor.est)
        
        lfe<-data.frame(PRIVATE=lfe.p,SOCIAL=lfe.s)
        rownames(lfe)<-c("Labor for Est. (HOK/Ha)")
        lfe
        # ending  Labor for establishment ------------------------------------------------------- 
        
        ############# PERHITUNGAN lfo
        lfo.p <- mean(p.avg.labor.opr[p.avg.labor.opr!=0])
        lfo.s <- mean(s.avg.labor.opr[s.avg.labor.opr!=0])
        
        lfo<-data.frame(PRIVATE=lfo.p,SOCIAL=lfo.s)
        rownames(lfo)<-c("Labor for Operation. (HOK/Ha/th)")
        lfo
        # ending  Labor for Operation -------------------------------------------------------
        
        
        # hitung hp --------------------------------------------------------------
        ############# PERHITUNGAN HARVESTING PRODUCT
        fil.prod <- dataGeneral %>%  filter(str_detect(grup,"output")) #filter io untuk grup output (hasil panen)
        fil.prod <- fil.prod %>%  filter(str_detect(komponen,"utama"))
        sum.prod <- fil.prod[,c(paste0(c(rep("Y", yearIO)),1:yearIO))] %>%
          colSums(na.rm = T)
        tot.prod <- sum(sum.prod)
        
        hp <- data.frame( tot.prod/total.labor)/1000 # karena ton jadi di bagi 1000
        colnames(hp)<-c("Harvesting Product (ton/HOK)")
        rownames(hp) <- c("Value")
        hp
        # ending  hp ------------------------------------------------------- 
        
        # hitung profit --------------------------------------------------------------
        p.profit.ha <- p.profit[-1]/readDataTemplate$total.area[1]
        s.profit.ha <- s.profit[-1]/readDataTemplate$total.area[1]
        # ending  profit -------------------------------------------------------
        
        
        
        ##### save data template
        # RESULT 
        dataDefine$npv <- hsl.npv
        dataDefine$dec <- dec
        dataDefine$ypc <- ypc
        dataDefine$lfe <- lfe
        dataDefine$lfo <- lfo
        dataDefine$hp <- hp
        
        # dataDefine$tabel.profit <- tabel.profit
        dataDefine$p.profit.ha <- p.profit.ha
        dataDefine$s.profit.ha <- s.profit.ha
        
        
        
        print("save result template untuk klik pertama asumsiMakro_button")
        
        fileName <- paste0(datapath,"resultTemplate","_",
                           input$sut,"_",input$kom,"_",
                           input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
        saveRDS(dataDefine,file = fileName)
        
        
        ##### ending save data template
      
        
        
        
        
      }else{
        readDataTemplate <- lowcase(readDataTemplate, c("faktor","komponen","jenis","unit.harga","unit"))
        yearIO <- 30 #tahun daur tanam
        
        inputData <- filter(readDataTemplate,faktor == c("input"))
        ioInput <- inputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
        priceInput <- inputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
        
        outputData <- filter(readDataTemplate,faktor == c("output"))
        ioOutput <- outputData[,c("komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
        priceOutput <- outputData[,c("komponen","jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
        
        
        
        # case for modal kapital
        capital <- filter(readDataTemplate,faktor == c("modal kapital"))
        
        # kondisi if else nya dari cek jumlah row di variable capital
        if(dim(capital)[1] == 0){
          capital <- NULL
          capitalPrivat <- NULL
          capitalSosial <- NULL
        } else if(dim(capital)[1] > 0){
          capital <- capital[c("komponen","jenis","unit.harga",paste0(c(rep("Y", yearIO)),1:yearIO))]
          capitalPrivat <- filter(capital,str_detect(komponen,"privat"))
          capitalSosial <- filter(capital,str_detect(komponen,"sosial"))
        }
        
        # Asumsi makro
        # readDataAsumsiMakro <- read.table(paste0("data/template/asumsi makro",".csv"), header = T, sep = ",")
        # asumsiMakroTemplate <- filter(readDataAsumsiMakro,tahun == input$th)
        
        rate.p <- readDataTemplate$rate.p[1]
        rate.s <- readDataTemplate$rate.s[1]
        nilai.tukar <- readDataTemplate$nilai.tukar[1]
        
        # memmbuat list gabungan dataDefine
        dataDefine <- list(ioInput=ioInput,ioOutput=ioOutput,
                           priceInput=priceInput,priceOutput=priceOutput,
                           capital=capital, capitalPrivat = capitalPrivat, capitalSosial = capitalSosial,
                           rate.p = rate.p,
                           rate.s = rate.s,
                           nilai.tukar=nilai.tukar)
        
        #informasi umum
        dataDefine$sut <- input$sut
        dataDefine$kom <- input$kom
        dataDefine$wilayah <- input$selected_wilayah
        dataDefine$th <- input$th
        dataDefine$tipeLahan <- input$tipeLahan
        dataDefine$tipeKebun <- readDataTemplate$tipe.kebun[1]
        dataDefine$lokasi <- readDataTemplate$lokasi[1]
        dataDefine$tipeData <- c("BAU")
        
        
        
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
        
        p.price<-select(price.all, -harga.sosial) #remove harga sosial
        p.year<-data.frame(replicate(yearIO,p.price$harga.privat)) #replicate nilai private price sebanyak n tahun
        colnames(p.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
        p.price<-cbind(status="harga.privat" ,p.price[c("grup", "komponen", "jenis", "unit.harga")],p.year)
        p.price <- p.price %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        s.price<-select(price.all, -harga.privat) #remove harga privat
        s.year<-data.frame(replicate(yearIO,s.price$harga.sosial))
        colnames(s.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
        s.price<-cbind(status="harga.sosial",s.price[c("grup", "komponen", "jenis", "unit.harga")],s.year)
        s.price <- s.price %>% mutate_if(is.factor,as.character) #change factor var to char
        
        price.all.year <- rbind(p.price, s.price)
        
        #### buat if else untuk modal kapital ####
        if (is.null(dataDefine$capital)){
          # capital = NULL
          data.gab <- bind_rows(io.all,
                                price.all.year) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
          
          # hitung npv --------------------------------------------------------------
          dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
          dataPrivat <- filter(data.gab,status == c("harga.privat")) #filter data private price
          p.budget <- dataGeneral[-(c(1:5,ncol(dataGeneral)))] * dataPrivat[-c(1:5,ncol(dataPrivat))] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
          p.budget <- cbind(dataGeneral[c(1:4)],dataPrivat["unit.harga"],p.budget) #memunculkan kembali variabel 1 sd 5
          p.budget <- p.budget %>%
            mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
          
          #perkalian antara general dengan Social Price
          dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
          s.budget <- dataGeneral[-c(1:5,ncol(dataGeneral))] * dataSosial[-c(1:5,ncol(dataSosial))]
          s.budget <- cbind(dataGeneral[c(1:4)],dataSosial["unit.harga"],s.budget)
          s.budget <- s.budget %>%
            mutate(status = case_when(status == "general" ~ "social budget"))
          
          ################ penghitungan NPV
          p.cost.input <- p.budget %>%
            filter(str_detect(grup,"input"))
          
          s.cost.input <- s.budget %>%
            filter(str_detect(grup,"input"))
          
          p.sum.cost<- p.cost.input[,-(1:5)] %>%
            colSums(na.rm = T)
          s.sum.cost<- s.cost.input[,-(1:5)] %>%
            colSums(na.rm = T)
          
          p.rev.output <- p.budget %>%
            filter(str_detect(grup,"output"))
          s.rev.output <- s.budget %>%
            filter(str_detect(grup,"output"))
          
          p.sum.rev <- p.rev.output[,-(1:5)] %>%
            colSums(na.rm = T)
          s.sum.rev <- s.rev.output[,-(1:5)] %>%
            colSums(na.rm = T)
          
          
          p.profit <- p.sum.rev - p.sum.cost
          s.profit <- s.sum.rev - s.sum.cost
          profit0 <- 0
          p.profit<-c(profit0,p.profit)
          s.profit<-c(profit0,s.profit)
          
          npv.p<-npv(dataDefine$rate.p/100,p.profit)
          npv.s<-npv(dataDefine$rate.s/100,s.profit)
          
          hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
          
          npv.p.us<-npv.p/as.numeric(dataDefine$nilai.tukar)
          npv.s.us<-npv.s/as.numeric(dataDefine$nilai.tukar)
          npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
          hsl.npv<-rbind(hsl.npv,npv.us)
          
          #browser()
          
          rownames(hsl.npv)<-c("NPV (Rp/Ha)", "NPV (US/Ha)")
          hsl.npv
          # ending  npv --------------------------------------------------------------
          
        }else if (!is.null(dataDefine$capital)){
          capital <- cbind(grup="input",dataDefine$capital)
          
          # menambahkan pada tabel io matrix bernilai 1
          ioKapital <- data.frame(matrix(data=1,nrow = nrow(capital) , ncol = ncol(dataDefine$ioInput)-3))
          colnames(ioKapital)<-paste0(c(rep("Y", yearIO)),1:yearIO)
          ioKapital<-cbind(status="modal kapital" ,capital[c(1:4)],ioKapital)
          ioKapital <- ioKapital %>% mutate_if(is.factor,as.character) #change factor var to char var
          
          
          kapitalPrivat <- filter(capital,komponen == c("modal kapital privat"))
          kapitalPrivat <- cbind(status ="harga.privat",kapitalPrivat )
          kapitalPrivat <- kapitalPrivat %>% mutate_if(is.factor,as.character) #change factor var to char var
          
          kapitalSosial <- filter(capital,komponen == c("modal kapital sosial"))
          kapitalSosial <- cbind(status ="harga.sosial",kapitalSosial )
          kapitalSosial <- kapitalSosial %>% mutate_if(is.factor,as.character) #change factor var to char var
          
          data.gab <- bind_rows(io.all, ioKapital,
                                price.all.year, 
                                kapitalPrivat, kapitalSosial) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
          # hitung npv --------------------------------------------------------------
          dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
          dataCapitalAll <- filter(data.gab,status == c("modal kapital"))
          
          
          dataGeneralPrivat <- filter(dataCapitalAll,komponen == c("modal kapital privat"))
          dataGeneralPrivat <- rbind(dataGeneral,dataGeneralPrivat)
          dataPrivat <- filter(data.gab,status == c("harga.privat"))
          p.budget <- dataGeneralPrivat[-(c(1:5,36))] * dataPrivat[-c(1:5,36)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
          p.budget <- cbind(dataGeneralPrivat[c(1:4)],dataPrivat["unit.harga"],p.budget) #memunculkan kembali variabel 1 sd 5
          p.budget <- p.budget[-1] #menghilangkan label status yang awal
          p.budget <- cbind(status = "privat budget", p.budget) #merename keseluruhan tabel status
          
          
          
          
          #perkalian antara general dengan Social Price
          dataGeneralSosial <- filter(dataCapitalAll,komponen == c("modal kapital sosial"))
          dataGeneralSosial <- rbind(dataGeneral,dataGeneralSosial)
          dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
          s.budget <- dataGeneralSosial[-c(1:5,36)] * dataSosial[-c(1:5,36)]
          s.budget <- cbind(dataGeneralSosial[c(1:4)],dataSosial["unit.harga"],s.budget)
          s.budget <- s.budget[-1] #menghilangkan label status yang awal
          s.budget <- cbind(status = "sosial budget", s.budget) #merename keseluruhan tabel status
          
          ################ penghitungan NPV
          p.cost.input <- p.budget %>%
            filter(str_detect(grup,"input"))
          
          s.cost.input <- s.budget %>%
            filter(str_detect(grup,"input"))
          
          p.sum.cost<- p.cost.input[,-(1:5)] %>%
            colSums(na.rm = T)
          s.sum.cost<- s.cost.input[,-(1:5)] %>%
            colSums(na.rm = T)
          
          p.rev.output <- p.budget %>%
            filter(str_detect(grup,"output"))
          s.rev.output <- s.budget %>%
            filter(str_detect(grup,"output"))
          
          p.sum.rev <- p.rev.output[,-(1:5)] %>%
            colSums(na.rm = T)
          s.sum.rev <- s.rev.output[,-(1:5)] %>%
            colSums(na.rm = T)
          
          
          p.profit <- p.sum.rev - p.sum.cost
          s.profit <- s.sum.rev - s.sum.cost
          profit0 <- 0
          p.profit<-c(profit0,p.profit)
          s.profit<-c(profit0,s.profit)
          
          npv.p<-npv(dataDefine$rate.p/100,p.profit)
          npv.s<-npv(dataDefine$rate.s/100,s.profit)
          
          hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
          
          npv.p.us<-npv.p/dataDefine$nilai.tukar
          npv.s.us<-npv.s/dataDefine$nilai.tukar
          npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
          hsl.npv<-rbind(hsl.npv,npv.us)
          
          #browser()
          
          rownames(hsl.npv)<-c("NPV (Rp/Ha)", "NPV (US/Ha)")
          hsl.npv
          
          # ending  npv --------------------------------------------------------------
        }
        
        # hitung nlc --------------------------------------------------------------
        
        ################ penghitungan NLC dan labor cost 
        
        p.tot.cost<- sum(p.sum.cost)
        s.tot.cost<- sum(s.sum.cost)
        
        p.labor.input <- p.budget %>% filter(str_detect(komponen,c("tenaga kerja|tk|kerja|tenaga")))
        s.labor.input <- s.budget %>% filter(str_detect(komponen,c("tenaga kerja|tk|kerja|tenaga")))
        
        p.sum.labor <- p.labor.input[,-(1:5)] %>%
          sum(na.rm = T)
        s.sum.labor <- s.labor.input[,-(1:5)] %>%
          sum(na.rm = T)
        
        
        
        nlc.p <- (p.tot.cost - p.sum.labor)/1000000
        nlc.s <- (s.tot.cost - s.sum.labor)/1000000
        nlc<-data.frame(PRIVATE=nlc.p,SOCIAL=nlc.s)
        rownames(nlc)<-c("Non Labor Cost  for 1 cycle (MRp/Ha)")
        nlc
        
        
        lcost <- data.frame(p.sum.labor/1000000)
        colnames(lcost)<-c("Labor Cost for 1 cycle (MRp/ha)")
        rownames(lcost) <- c("Value")
        lcost
        
        nlcost <- data.frame(nlc.p)
        colnames(nlcost)<-c("Non Labor Cost for 1 cycle (MRp/ha)")
        rownames(nlcost) <- c("Value")
        nlcost
        # ending  nlc ------------------------------------------------------- 
        
        # hitung EC --------------------------------------------------------------
        ############# PERHITUNGAN ESTABLISHMENT COST
        p.ec <- p.sum.cost[[1]]/1000000
        s.ec <- s.sum.cost[[1]]/1000000
        ec <- data.frame(p.ec,s.ec)
        ec<-data.frame(PRIVATE=p.ec,SOCIAL=s.ec)
        rownames(ec)<-c("Establishment cost (1st year only, MRp/ha)")
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
        
        # hitung profit --------------------------------------------------------------
        p.profit.ha <- p.profit[-1]/readDataTemplate$total.area[1]
        s.profit.ha <- s.profit[-1]/readDataTemplate$total.area[1]
        # ending  profit ------------------------------------------------------
        
        
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
        
        pvCost <- presentValue(costYear,c(1:length(costYear)),rate.p/100)
        
        revenueYear <- p.budget %>%
          filter(str_detect(grup,"output"))%>%
          select(contains('Y'))%>%
          colSums(na.rm = T)
        
        pvRevenue <- presentValue(revenueYear,c(1:length(revenueYear)),rate.p/100)
        
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
        
        
        
        ##### save data template
        
        # RESULT 
        dataDefine$npv <- hsl.npv
        dataDefine$nlc <- nlc
        dataDefine$ec <- ec
        dataDefine$hp <- hp
        dataDefine$lr <- lr
        
        dataDefine$rtl <- rtl
        dataDefine$bcr <- bcr
        dataDefine$pi <- pi
        dataDefine$lcost <- lcost
        dataDefine$nlcost <- nlcost
        
        # dataDefine$tabel.profit <- tabel.profit
        dataDefine$p.profit.ha <- p.profit.ha
        dataDefine$s.profit.ha <- s.profit.ha
        
        print("save result template untuk klik pertama asumsiMakro_button")
        
        fileName <- paste0(datapath,"resultTemplate","_",
                           input$sut,"_",input$kom,"_",
                           input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
        saveRDS(dataDefine,file = fileName)
        
        
        ##### ending save data template
      }
      
      
      
      
    })
    
    # end - Section preparation data --------
    
    # Section input informasi umum  dan asumsi makcro---------------------------------------------
    observeEvent(c(input$sut, input$kom, input$selected_prov, input$selected_wilayah, input$th, input$tipeLahan), {
      removeUI(selector='#showResult')
      removeUI(selector='#showMakro')
      removeUI(selector='#showBox')
      removeUI(selector='#showTable')
      removeUI(selector='#showButton')
      
      # dataTemplate()
      # resultTemplate()
    })
    
    observeEvent(c(input$sut, input$kom, input$selected_prov, input$selected_wilayah, input$th, input$tipeLahan,
                   input$rate.p,input$rate.s,input$nilai.tukar), {
      removeUI(selector='#showResult')
      removeUI(selector='#showBox')
      removeUI(selector='#showTable')
      removeUI(selector='#showButton')
      
      
      # dataTemplate()
      # resultTemplate()
    })
    
    
    # End - Section input informasi umum  dan asumsi makcro---------------------------------------------
    
    # Section show box ---------------------------------------------
    # tableList <-  reactiveValues(
    # )
    
    observeEvent(input$box_button, {
      # browser()
      dataTemplate()
      resultTemplate()
      
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"resultTemplate","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",
                         input$tipeLahan,".rds")
      readDataTemplate <- readRDS(fileName)
      idData <- paste0(readDataTemplate$sut, "_",readDataTemplate$kom, "_",readDataTemplate$wilayah, "_",
                       # readDataTemplate$lokasi, "_",
                       readDataTemplate$th, "_",
                       readDataTemplate$tipeLahan, "_",readDataTemplate$tipeKebun)
      dataView <- (data.frame(alamatFile = fileName,
                              ID = idData,
                              sut = readDataTemplate$sut,
                              kom = readDataTemplate$kom,
                              wilayah = readDataTemplate$wilayah,
                              # lokasi = readDataTemplate$lokasi,
                              tahun = readDataTemplate$th,
                              tipeLahan = readDataTemplate$tipeLahan,
                              tipeKebun = readDataTemplate$tipeKebun
      ))
      dataView[is.na(dataView)] <- 0 #NA replace with zero
      
      colnames(dataView) <- c("Alamat File","ID","SUT", "KOMODITAS", "KABUPATEN", 
                              # "LOKASI",
                              "TAHUN", 
                              "TIPE LAHAN","TIPE KEBUN")
      
      boxData$data <- rbind(boxData$data,dataView)
      # tableList$dataFrame <-  boxData$data
      
      insertUI(selector='#uiShowBox',
               where='afterEnd',
               ui= dashboardBody(uiOutput("showBox"))
               )
    }) 
    
    
    
    
    output$showBox <- renderUI({
      argonRow(
        argonColumn(
          width = 12,
          argonH1("Daftar Informasi Umum Yang Terpilih", display = 4),
          h5("Langkah 2: mendaftarkan informasi umum untuk data PAM yang ditampilkan"),
          br(),
          fluidPage(
            box(width=12,
                column(12,dataTableOutput("Main_table")),
                tags$script(HTML('$(document).on("click", "input", function () {
                    var checkboxes = document.getElementsByName("row_selected");
                    var checkboxesChecked = [];
                    for (var i=0; i<checkboxes.length; i++) {
                       if (checkboxes[i].checked) {
                          checkboxesChecked.push(checkboxes[i].value);
                       }
                    }
                    Shiny.onInputChange("checked_rows",checkboxesChecked);
                        })')),
                                  tags$script("$(document).on('click', '#Main_table button', function () {
                                      Shiny.onInputChange('lastClickId',this.id);
                                      Shiny.onInputChange('lastClick', Math.random())
                    });")
            )
          ),
          fluidRow(
            column(9,
                   br() 
                   
            ),
            column(3,
                   br(),
                   if (nrow(boxData$data)>0){
                     actionButton(("tampilkanTabel_button"),"Tampilkan Hasil Analisis",icon("paper-plane"),style="color: white; 
                         background-color: green;") 
                   }
                   
            )
          )
        )
      )
    })

    
    boxData <- reactiveValues()
    
    boxData$data <- data.table(
      alamatFile = NULL,
      ID=NULL,
      sut = NULL,
      kom = NULL,
      wilayah = NULL, 
      lokasi = NULL, 
      tahun = NULL,
      tipeLahan = NULL,
      tipeKebun = NULL
    )
    
    output$Main_table <- renderDataTable({
      if (nrow(boxData$data)>0){
        dataView <- boxData$data[,-c(1,2)]
        dataView[["Tombol Hapus"]]<-
          paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
                <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(boxData$data),'>Hapus dari daftar</button>
             </div>
             
             ')
      }
      else if (nrow(boxData$data)==0){
        dataView <- data.frame(matrix("Silakan pilih informasi umum terlebih dahulu",nrow=1,ncol=1))
        colnames(dataView) <- "Keterangan"
      }
      datatable(dataView,
                escape=F)
      
    })

    observeEvent(input$lastClick,
                 {
                   if (input$lastClickId%like%"delete")
                   {
                     row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
                     removeUI(selector='#showResult')
                     removeUI(selector='#showTable')
                     print("berhasil check")
                     boxData$data=boxData$data[-row_to_del]
                     print("berhasil delete")
                     
                     
                   }
                 }
    )
    
    
    # End - Section show box ---------------------------------------------
    
    # Section asumsi makro ---------------------------------------------
    observeEvent(input$asumsiMakro_button, {
      # browser()
      dataTemplate()
      resultTemplate()
      insertUI(selector='#uiShowMakro',
               where='afterEnd',
               ui= uiOutput('showMakro'))
      
      
    }) 
    
    output$showMakro <- renderUI({
      argonRow(
        argonColumn(
          width = 12,
          argonH1("Asumsi Makro", display = 4),
          h5("Langkah 2: menentukan asumsi makro untuk data PAM yang ditampilkan"),
          br(),
          fluidRow(
            # column(1,
            #        
            # ),
            column(4,
                   tableOutput("showMakroBAU")
            ),
            column(2,
                   sliderInput(("rate.p"), "Discount Rate Private", 7.4 ,min = 0, max = 15, step = 0.01)
            ),
            column(2,
                   sliderInput(("rate.s"), "Discount Rate Social", 2.4 ,min = 0, max = 8, step = 0.01)
            ),
            
            column(2,
                   sliderInput(("nilai.tukar"), "Nilai Tukar Rupiah", 14831 ,min = 10000, max = 20000, step = 1)
            )
            # ,
            # column(2,
            #        br(),
            #        actionButton(("tampilkanTabel_button"),"Tampilkan Tabel PAM",icon("paper-plane"),style="color: white; 
            #              background-color: green;")
            # )
          )
        )
      )
    })
    
    output$showMakroBAU <- renderTable({
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"resultTemplate","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      readDataTemplate <- readRDS(fileName)
      dataView <- t(data.frame(rate.p.bau = readDataTemplate$rate.p, 
                             rate.s.bau = readDataTemplate$rate.s,
                             nilai.tukar.bau = readDataTemplate$nilai.tukar,
                             tipe.kebun = readDataTemplate$tipeKebun,
                             lokasi=readDataTemplate$lokasi))
      dataView[is.na(dataView)] <- 0 #NA replace with zero
      
      nameRow <- c("Discount Rate Private", "Discount Rate Social", "Nilai Tukar Rupiah", "Tipe Kebun","Lokasi")
      dataView <- cbind(nameRow,data.frame(dataView))
      
      colnames(dataView) <- c(" ","Nilai BAU pada informasi umum yang terpilih")
      dataView
      
    })
    
    # End - Section asumsi makro ---------------------------------------------
    
    # Section tampilkan tabel---------------------------------------------
    observeEvent(input$tampilkanTabel_button, {
      # browser()
      dataTemplate()
      resultTemplate()
      
      # updateSelectInput(inputId = "filterData", choices = siterefs, selected = siteref)
      
      insertUI(selector='#uiShowTable',
               where='afterEnd',
               ui= uiOutput('showTable'))
      
      # insertUI(selector='#uiShowButton',
      #          where='afterEnd',
      #          ui= uiOutput('showButton'))
    }) 
    
    output$showTable <- renderUI({
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      argonRow(
        argonColumn(
          width = 12,
          argonH1("Output", display = 4),
          h5("Langkah 3: Menampilkan atau menyunting Tabel PAM yang terpilih"),
          
          # jika tdk bisa jadi input buttton maka coba ubah nama action  buttonnya sepertinya conflict dengan script lain
          argonTabSet(
            id = "tab-1",
            card_wrapper = TRUE,
            horizontal = TRUE,
            circle = FALSE,
            size = "sm",
            width = 12,
            iconList = lapply(X = 1:4, FUN = argonIcon, name = "atom"),
            argonTab(
              tabName = "Bar Chart NPV",
              active = T,
              uiOutput(("showNPV")),
              style = "height:1200px; overflow-y: scroll;overflow-x: scroll;"
            ),
            argonTab(
              tabName = "Hasil Perhitungan Analisis Profitabilitas",
              active = F,
              uiOutput(("showHitung")),
              style = "height:1200px; overflow-y: scroll;overflow-x: scroll;"
            ),
            argonTab(
              tabName = "Grafik profit tahunan",
              active = F,
              uiOutput(("scatProfit")),
              style = "height:1200px; overflow-y: scroll;overflow-x: scroll;"
            ),
            argonTab(
              tabName = "Data template",
              active = F,
              uiOutput(("showDataTemplate")),
              style = "height:1200px; overflow-y: scroll;overflow-x: scroll;"
            )
            
            
          )
        )
      )
    })
    
    
    
    output$showHitung <- renderUI({
      fluidPage(
        fluidRow(
          column(11,
                 br(),
                 br(),
                 h1(paste0("HASIL PERHITUNGAN BUSINESS AS USUAL (BAU)"), align = "center")
          )
        ),
        br(),
        fluidRow(
          column(12,
                id = 'TABEL1',
                tags$style('#TABEL1 {
                           background-color: #C4CCFF;
                           }'),
                h1("TABEL 1", align = "center"),
                 uiOutput("hitungBAU1")
          )
          ,
          column(12,
                 id = 'TABEL2',
                 tags$style('#TABEL2 {
                           background-color: #B3FAE0;
                           }'),
                 h1("TABEL 2", align = "center"),
                 uiOutput("hitungBAU2")
          )
        )
      )
    })
    
    output$hitungBAU1 <- renderUI({
      
      datapath <- boxData$data[,1]
      nbaris <- nrow(datapath)
      vectorAlamat <- NULL
      for (k in 1:nbaris) {
        ekstractDataTable <- melt.data.table((datapath[k,]), measure.vars = 1)
        vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
      }
      
      listAll <- lapply(vectorAlamat, readRDS)
      
      dat_list <- list()
      for (j in 1:nrow(boxData$data)) {
        dataDefine <- listAll[[j]]
        
        if (dataDefine$tipeKebun[1] == "LARGE SCALE"){
          
          tabel1 <- rbind(dataDefine$hsl.npv,dataDefine$dec,dataDefine$ypc,dataDefine$lfe,dataDefine$lfo)
          tabel1[] <- lapply(tabel1, function(i) sprintf('%.6g', i))
          
         
          dataView <- datatable(tabel1, option=list(dom = "t")) 
          dat_list[[j]] <- dataView
          
          
        }else{
          tabel1 <- rbind(dataDefine$npv,dataDefine$nlc,dataDefine$ec)
          tabel1[] <- lapply(tabel1, function(i) sprintf('%.6g', i))
          tabel1
          
          dataView <- datatable(tabel1, option=list(dom = "t")) 
          dat_list[[j]] <- dataView
        }
      }
      yes <- lapply(1:length(listAll), function(i){tfc(dat_list[[i]])})
      yes
    })
    
    output$hitungBAU2 <- renderUI({
      datapath <- boxData$data[,1]
      nbaris <- nrow(datapath)
      vectorAlamat <- NULL
      for (k in 1:nbaris) {
        ekstractDataTable <- melt.data.table((datapath[k,]), measure.vars = 1)
        vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
      }
      
      listAll <- lapply(vectorAlamat, readRDS)
      
      dat_list <- list()
      for (j in 1:nrow(boxData$data)) {
        dataDefine <- listAll[[j]]
        
        if (dataDefine$tipeKebun[1] == "LARGE SCALE"){
          
          
          # Total area
          totArea <- data.frame(dataDefine$totalArea)
          colnames(totArea)<-c("Total Area (Ha)")
          rownames(totArea) <- c("Value")
          
          
          showHp <- dataDefine$hp
          
          
          tabel2 <- data.frame(t(cbind(showHp,totArea)))
          colnames(tabel2) <- c("Nilai")
          tabel2[] <- lapply(tabel2, function(i) sprintf('%.6g', i))
          tabel2
          
          dataView <- datatable(tabel2, option=list(dom = "t")) 
          dat_list[[j]] <- dataView
          
          
        }else{
        # tampilan rate.p, rate.s, nilai tukar rupiah --------------------------------------------------------------
        showRateP <- (dataDefine$rate.p)
        showRateS <- (dataDefine$rate.s)
        showExRate <- (dataDefine$nilai.tukar)
        showHp <- dataDefine$hp
        showLr <- dataDefine$lr
        showRtl <- dataDefine$rtl
        showBCR <- dataDefine$bcr
        showPi <- dataDefine$pi
        showLcost <- dataDefine$lcost
        showNlcost <- dataDefine$nlcost
        # showNpv <- dataDefine$npv[1,1]
    
        showBauMacro <- rbind(showRateP,showRateS,showExRate)
        rownames(showBauMacro)<-c("Discount Rate Private", "Discount Rate Social", "Nilai Tukar Rupiah")
        colnames(showBauMacro) <- c("Nilai")
        showBauMacro
        
        rownames(showHp) <- c("Nilai")
        rownames(showLr) <- c("Nilai")
        showHp <- data.frame(t(showHp))
        showLr <- data.frame(t(showLr))
        
        
        
        rownames(showRtl) <- c("Nilai")
        rownames(showBCR) <- c("Nilai")
        rownames(showPi) <- c("Nilai")
        rownames(showLcost) <- c("Nilai")
        rownames(showNlcost) <- c("Nilai")
        # rownames(showNpv) <- c("Nilai")
        
        showRtl <- data.frame(t(showRtl))
        showBCR <- data.frame(t(showBCR))
        showPi <- data.frame(t(showPi))
        showLcost <- data.frame(t(showLcost))
        showNlcost <- data.frame(t(showNlcost))
        # showNpv <- data.frame(t(showNpv))
        
        
        # ending  ------------------------------------------------------- 

        tabel2 <- rbind(showHp,showLr, showRtl, showBCR, showPi, showLcost, showNlcost, showBauMacro)
        tabel2[] <- lapply(tabel2, function(i) sprintf('%.6g', i))

        dataView <- datatable(tabel2, option=list(dom = "t")) 
        dat_list[[j]] <- dataView
      }
      }
      yes <- lapply(1:length(listAll), function(i){tfc(dat_list[[i]])})
      yes
      
    })
    
    output$showNPV <- renderUI({
      fluidPage(
        fluidRow(
          column(11,
                 br(),
                 br(),
                 h1(paste0("BAR CHART NPV"), align = "center")
          )
        ),
        br(),
        fluidRow(
          column(12,
                 id = 'BARNPV',
                 tags$style('#BARNPV {
                           background-color: #C4CCFF;
                           }'),
                 plotOutput("showBarCombine"),
                 br(),
                 br()
          ),
          column(12,
                 id = 'BARNPVC',
                 tags$style('#BARNPVC {
                           background-color: #B3FAE0;
                           }'),
                 plotOutput("showBar")
                 # plotlyOutput("showBarSut")
                 # tags$div(id = 'uiplotComparing')
                 # tags$div(id = 'uiplotBar')
          )
        )
      )
    })

    output$scatProfit <- renderUI({
      fluidPage(
        fluidRow(
          column(12,
                 id = 's1',
                 tags$style('#s1 {
                           background-color: #C4CCFF;
                           }'),
                 h1(paste0("Scatter Plot Profit Tahunan"), align = "center")
          ),
          column(6,
                 plotlyOutput('showPlotProfPrivat')
          ),
          column(6,
                 plotlyOutput('showPlotProfSosial')
          )
        ),
        br(),
        br(),
        fluidRow(
          column(12,
                 id = 's2',
                 tags$style('#s2 {
                           background-color: #B3FAE0;
                           }'),
                 h1(paste0("Scatter Plot Profit Kumulatif"), align = "center")
          ),
          column(6,
                 plotlyOutput('profitPlotKumP')
          ),
          column(6,
                 plotlyOutput('profitPlotKumS')
          )
        )
        
        # fluidRow(
        #   column(2,
        #          actionButton(("saveNewPAM"),"Simpan PAM baru",icon("paper-plane"),style="color: white;background-color: green;"),
        #          br(),
        #          tags$div(id='teksNewPamSave')
        #   )
        # )
      )
    })

    
    # output$showHitung <- renderUI({
    #   fluidPage(
    #     fluidRow(
    #       column(11,
    #              br(),
    #              br(),
    #              h1(paste0("HASIL PERHITUNGAN"), align = "center"),
    #              # h1(paste0("di ",input$selected_wilayah," pada tahun ",input$th," dengan tipe lahan ", input$tipeLahan), align = "center"),
    #              # br(),
    #       )
    #     ),
    #     br(),
    #     fluidRow(
    #       column(12,
    #              id = 'bau',
    #              tags$style('#bau {
    #                         background-color: #00cca3;
    #                         }'),
    #              h3("Business As Usual (BAU)", align = "center")
    #              
    #       )
    #       # ,
    #       # column(6,
    #       #        id = 'sim',
    #       #        tags$style('#sim {
    #       #                   background-color: #b3b3ff;
    #       #                   }'),
    #       #        h3("Simulasi", align = "center")
    #       # )
    #     ),
    #     fluidRow(
    #       column(6,
    #              dataTableOutput("tableResultBAU1"),
    #              dataTableOutput("tableResultBAU2")
    #       ),
    #       
    #       # column(6,
    #       #        dataTableOutput("tableResultSimulasi1"),
    #       #        
    #       # ),
    #       
    #       # column(6,
    #       #        dataTableOutput("tableResultBAU2")
    #       #        
    #       # )
    #       # ,
    #       # column(6,
    #       #        dataTableOutput("tableResultSimulasi2")
    #       #        
    #       # ),
    #     ),
    #     
    #     br(),
    #     br(),
    #     column(12,
    #            id = 'tableNPV',
    #            tags$style('#tableNPV {
    #                         background-color: #CCFFCC;
    #                         }'),
    #            h3("Tabel NPV seluruh SUT dalam 1 Wilayah", align = "center")
    #            
    #     ),
    #     fluidRow(
    #       column(12,
    #              dataTableOutput('showTableAllProvinsi')
    #       )
    #     ),
    #     br(),
    #     br(),
    #     fluidRow(
    #       column(4,
    #              id = 'plotCom',
    #              tags$style('#plotCom {
    #                         background-color: #CCFFCC;
    #                         }'),
    #              h3("Barchart NPV BAU vs Simulasi", align = "center")
    #              
    #       ),
    #       column(8,
    #              id = 'plotAll',
    #              tags$style('#plotAll {
    #                         background-color: #CCFFCC;
    #                         }'),
    #              h3("Barchart NPV seluruh SUT dalam 1 Wilayah", align = "center")
    #              
    #       ),
    #       column(4,
    #              tags$div(id = 'uiplotComparing')
    #       ),
    #       column(8,
    #              tags$div(id = 'uiShowPlotAllKomoditas')
    #       )
    #     ),
    #     br(),
    #     br(),
    #     column(12,
    #            id = 'grafikProfit',
    #            tags$style('#grafikProfit {
    #                         background-color: #CCFFCC;
    #                         }'),
    #            h3("Grafik Profit Tahunan", align = "center")
    #            
    #     ),
    #     fluidRow(
    #       column(6,
    #              plotlyOutput('showPlotProfitPrivat')
    #       ),
    #       column(6,
    #              plotlyOutput('showPlotProfitSosial')
    #       )
    #     ),
    #     fluidRow(
    #       column(6,
    #              plotlyOutput('showPlotKumProfitPrivat')
    #       ),
    #       column(6,
    #              plotlyOutput('showPlotKumProfitSosial')
    #       )
    #     ),
    #     fluidRow(
    #       column(2,
    #              actionButton(("saveNewPAM"),"Simpan PAM baru",icon("paper-plane"),style="color: white;background-color: green;"),
    #              br(),
    #              tags$div(id='teksNewPamSave')
    #       )
    #     )
    #     
    #     
    #   )
    # })
    # 
    textTerpilih <- reactive({ input$filterData })
    
    output$selectedText <- renderText({
      paste("Kamu memilih: ", textTerpilih() ,sep="")
      })
    
    outSelect <- reactiveValues(
      alamatFile = NULL,
      namaKom = NULL
    )
    

    
    
    output$showDataTemplate <- renderUI({
      fluidPage(
        # fluidRow(
        #   column(2,
        #          selectInput("filterData","Data yang ditampilkan",choices = paste0(rownames(boxData$data), " - ",boxData$data[[3]]))
        #          # selectInput("filterData","Data yang ditampilkan",choices = outSelect())
        #   ),
        #   column(6,
        #          textOutput("selectedText")
        #   )
        # ),
        fluidRow(
          column(11,
                 br(),
                 h1("DATA TEMPLATE", align = "center"),
                 br()
                 # ,
                 # h1(paste0("HASIL ANALISIS"," ",input$kom," ",input$sut), align = "center"),
                 # h1(paste0("di ",input$selected_wilayah," pada tahun ",input$th," dengan tipe lahan ", input$tipeLahan), align = "center"),
                 # br(),
          )
        ),
        br(),
        fluidRow(
          column(12,
                 id = 'tableP',
                 tags$style('#tableP {
                            background-color: #C4CCFF;
                            }'),
                 h2("TABEL HARGA", align = "center"),
                 # dataTableOutput("showTablePrice")
                 uiOutput("showTablePrice")
          )
        ),
        br(),
        fluidRow(
          column(12,
                 id = 'tableKuan',
                 tags$style('#tableKuan {
                            background-color: #B3FAE0;
                            }'),
                 h2("TABEL KUANTITAS", align = "center"),
                 uiOutput("showTableKuantitas")
                 
          )
        ),br(),
        fluidRow(
          column(12,
                 id = 'tableKap',
                 tags$style('#tableKap {
                            background-color: #FDFFC6;
                            }'),
                 h2("TABEL KAPITAL ATAU TABEL SKENARIO LAHAN", align = "center"),
                 uiOutput("showTableKapital")

          )
        )
        
        
      )
    })
    
    
    
    # observeEvent(input$filterData, priority = 50, {
    #   if (is.null(boxData$data)) return(NULL)
       # siteref <- input$filterData
      # updateSelectInput(inputId = "site_series_feas", choices = uData$siteseries_list[[siteref]])
    # })
    
    
    output$showTablePrice <- renderUI({
      showTableP()
    })
    
    tfc = function(m) {
      force(m)
      renderDataTable(m)
    }
    
    showTableP <- eventReactive(c(input$tampilkanTabel_button, input$filterData),{

        datapath <- boxData$data[,1]
        nbaris <- nrow(datapath)
        vectorAlamat <- NULL
        for (k in 1:nbaris) {
          ekstractDataTable <- melt.data.table((datapath[k,]), measure.vars = 1)
          vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
        }
        
        listAll <- lapply(vectorAlamat, readRDS)
        
        dat_list <- list()
        for (j in 1:nrow(boxData$data)) {
          dataDefine <- listAll[[j]]
          dataView <- rbind(dataDefine$priceInput, dataDefine$priceOutput)
          dataView[is.na(dataView)] <- 0 #NA replace with zero
          dat_list[[j]] <- dataView
        }
    
        # tfc = function(m){renderDataTable({m})}
        yes <- lapply(1:length(listAll), function(i){tfc(dat_list[[i]])})
        yes

    })
    
    
    output$showTableKuantitas <- renderUI({
      showTableK()
      
    })
    
    showTableK <- eventReactive(c(input$tampilkanTabel_button,input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
      datapath <- boxData$data[,1]
      nbaris <- nrow(datapath)
      vectorAlamat <- NULL
      for (i in 1:nbaris) {
        ekstractDataTable <- melt.data.table((datapath[i,]), measure.vars = 1)
        vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
      }
      
      listAll <- lapply(vectorAlamat, readRDS)
      
      dat_list <- list()
      for (j in 1:nrow(boxData$data)) {
        dataDefine <- listAll[[j]]
        dataView <- rbind(dataDefine$ioInput, dataDefine$ioOutput)
        dataView[is.na(dataView)] <- 0 #NA replace with zero
        dat_list[[j]] <- dataView
      }
      yes <- lapply(1:length(listAll), function(i){tfc(dat_list[[i]])})
      yes
      
    })
    
    output$showTableKapital <- renderUI({
      showTableCap()
    })
    
    showTableCap <- eventReactive(c(input$tampilkanTabel_button,input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
      datapath <- boxData$data[,1]
      nbaris <- nrow(datapath)
      vectorAlamat <- NULL
      for (i in 1:nbaris) {
        ekstractDataTable <- melt.data.table((datapath[i,]), measure.vars = 1)
        vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
      }
      
      listAll <- lapply(vectorAlamat, readRDS)
      
      dat_list <- list()
      for (j in 1:length(listAll)) {
        dataDefine <- listAll[[j]]
        
        if (dataDefine$tipeKebun== "LARGE SCALE"){
          dataView <- cbind("Total Area (Ha)" = dataDefine$totalArea, dataDefine$cum.landScene)
          dataView[is.na(dataView)] <- 0 #NA replace with zero
          dat_list[[j]] <- dataView
        }
        else{
            if (!is.null(dataDefine$capital)){
            dataView <- dataDefine$capital
            dataView[is.na(dataView)] <- 0 #NA replace with zero
            dat_list[[j]] <- dataView
          }
          else if (is.null(dataDefine$capital)){
            dataView <- data.frame(matrix("tidak terdapat tabel modal kapital",nrow=1,ncol=1))
            colnames(dataView) <- "Keterangan"
            dat_list[[j]] <- dataView
          }
        }
      }
      yes <- lapply(1:length(listAll), function(i){tfc(dat_list[[i]])})
      yes
      
    })
    
    output$showTableScenLand <- renderDataTable({
      showTableS()
      
    })
    
    showTableS <- eventReactive(c(input$tampilkanTabel_button,input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      dataView <- cbind("Total Area (Ha)" = dataDefine$totalArea, dataDefine$cum.landScene)
      dataView[is.na(dataView)] <- 0 #NA replace with zero
      dataView
    })
    
    
    
    output$showButton <- renderUI({
      argonRow(
        argonColumn(
          width = 12,
          br(),
          h5("Langkah 4: Jalankan Analisis", align = "center"),
          fluidRow(
            column(5,
                   
            ),
            column(6,
                   actionButton("buatPAM_button","Jalankan Analisis",icon("paper-plane"),style="color: white;
                           background-color: green;")
            )
          )
        )
      )
      
    })
    
    # End - Section tampilkan tabel ---------------------------------------------
    
    
    # Section Popup Modal Dialog---------------------------------------------
    observeEvent(input$buatPAM_button,{
      # browser()
      dataTemplate()
      resultTemplate()
      removeUI(selector='#showResult')
      showModal(dataModalCreatePam())
      
    })
    
    dataModalCreatePam <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("closeModalCreatePam"), "Batal")
        ),
        argonTabSet(
          id = "tabTemplate",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Langkah 5: Membangun Tabel",
            active = T,
            fluidRow(
              column(8,
                     actionButton(("sunting_button"),"Sunting Tabel Kuantitas, Harga, dan Modal Kapital",style="color: white;
                         background-color: green;"),
              ),
              column(4,
                     actionButton(("template_button"),"Gunakan Data Template",style="color: white;
                         background-color: green;")
              )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$closeModalCreatePam,{
      removeUI(selector='#showResult')
      removeModal()
    })
    
    observeEvent(input$template_button,{
      # removeUI(selector='#showResult')
      showModal(
        modalDialog( 
          footer=tagList(
            actionButton(("backModalCreatePam"), "Kembali"),
            actionButton(("running_button"), "Jalankan Analisis",style="color: white;background-color: green;")
          ),
          argonTabSet(
            id = "tabPrice",
            card_wrapper = TRUE,
            horizontal = TRUE,
            circle = FALSE,
            size = "sm",
            width = 12,
            argonTab(
              tabName = "Sunting Harga Output",
              active = T,
              sidebarLayout(
                sidebarPanel(
                  fluidPage(
                    h3("Sunting secara manual"),
                    tags$h5("kolom yang dapat di sunting hanya kolom harga.privat dan harga.sosial"),
                    
                  ),
                  tags$br(),
                  width=12
                ),
                mainPanel(
                  rHandsontableOutput(('editPriceOutputTemplate')),
                  tags$br(),
                  actionButton(('savePriceOutputTemplate'), 'simpan tabel'), 
                  tags$br(), 
                  tags$br(),
                  tags$div(id='teksPriceOutputSaveTemplate'),
                  width=12)
              )
            ))
          ,
          size="l",
          easyClose = FALSE)
      )
    })
    
    observeEvent(input$backModalCreatePam,{
      showModal(dataModalCreatePam())
    })
    
    # observeEvent(c(input$tampilkanTabel_button,input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
    #   removeModal()
    #   insertUI(selector='#uiShowResult',
    #            where='afterEnd',
    #            ui= uiOutput('showResult'))
    # })
    
    # Start Price Output ------------------------------------------------------
    
    
    valP2Template <- eventReactive(input$template_button,{
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      reactData$tableP2 <- dataDefine$priceOutput
      reactData$tableP2
      
    })
    
    output$editPriceOutputTemplate <- renderRHandsontable({
      rhandsontable(valP2Template(),
                    rowHeaderWidth = 50,
                    fixedColumnsLeft = 2,
                    height = 300,
      )%>%
        hot_col(c(1:3), readOnly = TRUE)
    })
    
    observeEvent(input$savePriceOutputTemplate,{
      removeUI(selector='#textTampilOutputTemplate')
      # browser()
      
      editNew<-as.data.frame(hot_to_r(input$editPriceOutputTemplate))
      editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
      
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      # replace data price
      dataDefine$priceOutput <- editNew
      dataDefine$rate.p <- input$rate.p
      dataDefine$rate.s <- input$rate.s
      dataDefine$nilai.tukar <- input$nilai.tukar
      # dataDefine$tipeData <- c("SIMULASI")
      
      saveRDS(dataDefine,file = fileName)
      
      insertUI(selector='#teksPriceOutputSaveTemplate',
               where = 'afterEnd',
               ui = tags$div(id="textTampilOutputTemplate","tabel di atas sudah tersimpan"))
      # browser()
    })
    
    # Ending Price Output -----------------------------------------------------
    
    
    
    # output$showResult <- renderUI({
    #   fluidPage(
    #     fluidRow(
    #       column(11,
    #              br(),
    #              br(),
    #              h1(paste0("HASIL ANALISIS"," ",input$kom," ",input$sut), align = "center"),
    #              h1(paste0("di ",input$selected_wilayah," pada tahun ",input$th," dengan tipe lahan ", input$tipeLahan), align = "center"),
    #              br(),
    #       )
    #     ),
    #     br(),
    #     fluidRow(
    #       column(6,
    #              id = 'bau',
    #              tags$style('#bau {
    #                         background-color: #00cca3;
    #                         }'),
    #              h3("Business As Usual (BAU)", align = "center")
    #              
    #       ),
    #       column(6,
    #              id = 'sim',
    #              tags$style('#sim {
    #                         background-color: #b3b3ff;
    #                         }'),
    #              h3("Simulasi", align = "center")
    #       )
    #     ),
    #     fluidRow(
    #       column(6,
    #              dataTableOutput("tableResultBAU1"),
    #       ),
    #       
    #       column(6,
    #              dataTableOutput("tableResultSimulasi1"),
    #              
    #       ),
    #       
    #       column(6,
    #              dataTableOutput("tableResultBAU2")
    #              
    #       ),
    #       column(6,
    #              dataTableOutput("tableResultSimulasi2")
    #              
    #       ),
    #     ),
    #     
    #     br(),
    #     br(),
    #     column(12,
    #            id = 'tableNPV',
    #            tags$style('#tableNPV {
    #                         background-color: #CCFFCC;
    #                         }'),
    #            h3("Tabel NPV seluruh SUT dalam 1 Wilayah", align = "center")
    #            
    #     ),
    #     fluidRow(
    #       column(12,
    #              dataTableOutput('showTableAllProvinsi')
    #       )
    #     ),
    #     br(),
    #     br(),
    #     fluidRow(
    #       column(4,
    #              id = 'plotCom',
    #              tags$style('#plotCom {
    #                         background-color: #CCFFCC;
    #                         }'),
    #              h3("Barchart NPV BAU vs Simulasi", align = "center")
    #              
    #       ),
    #       column(8,
    #              id = 'plotAll',
    #              tags$style('#plotAll {
    #                         background-color: #CCFFCC;
    #                         }'),
    #              h3("Barchart NPV seluruh SUT dalam 1 Wilayah", align = "center")
    #              
    #       ),
    #       column(4,
    #              tags$div(id = 'uiplotComparing')
    #              ),
    #       column(8,
    #              tags$div(id = 'uiShowPlotAllKomoditas')
    #              )
    #     ),
    #     br(),
    #     br(),
    #     column(12,
    #            id = 'grafikProfit',
    #            tags$style('#grafikProfit {
    #                         background-color: #CCFFCC;
    #                         }'),
    #            h3("Grafik Profit Tahunan", align = "center")
    #            
    #     ),
    #     fluidRow(
    #       column(6,
    #              plotlyOutput('showPlotProfitPrivat')
    #       ),
    #       column(6,
    #              plotlyOutput('showPlotProfitSosial')
    #       )
    #     ),
    #     fluidRow(
    #       column(6,
    #              plotlyOutput('showPlotKumProfitPrivat')
    #       ),
    #       column(6,
    #              plotlyOutput('showPlotKumProfitSosial')
    #       )
    #     ),
    #     fluidRow(
    #       column(2,
    #              actionButton(("saveNewPAM"),"Simpan PAM baru",icon("paper-plane"),style="color: white;background-color: green;"),
    #              br(),
    #              tags$div(id='teksNewPamSave')
    #       )
    #     )
    #     
    #     
    #   )
    # })
    
    
    
    
    observeEvent(c(input$sut, input$kom, input$selected_prov, input$selected_wilayah, input$th, input$tipeLahan,
                  input$asumsiMakro_button, input$tampilkanTabel_button,input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
      removeUI(selector = '#showplotComparing')
      removeUI(selector = '#showPlotAllKomoditas')
      
      insertUI(selector='#uiplotComparing',
               where='afterEnd',
               ui= plotlyOutput('showplotComparing'))
      
      insertUI(selector='#uiShowPlotAllKomoditas',
               where='afterEnd',
               ui= plotlyOutput('showPlotAllKomoditas'))
    })

    output$showBar <- renderPlot({
      datapath <- boxData$data[,1]
      nbaris <- nrow(datapath)
      vectorAlamat <- NULL
      for (k in 1:nbaris) {
        ekstractDataTable <- melt.data.table((datapath[k,]), measure.vars = 1)
        vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
      }
      
      listAll <- lapply(vectorAlamat, readRDS)
      
      # dat_list <- list()
      # for (j in 1:nrow(boxData$data)) {
      #   dataDefine <- listAll[[j]]
      #   
      #   dataPlotBAU <- data.frame(wilayah=dataDefine$wilayah,
      #                             komoditas=dataDefine$kom,
      #                             NPV.Privat.RP=dataDefine$npv[1,1])
      #   
      #   dat_list[[j]] <- dataPlotBAU
      # }
      # yes <- lapply(1:length(listAll), function(i){
      #   plotly::plot_ly(dat_list[[i]], x = ~komoditas, y = ~NPV.Privat.RP, type = "bar", color = ~komoditas)
      # })
      # subplot(yes)
      
      dat_df <- NULL
      for (j in 1:nrow(boxData$data)) {
        dataDefine <- listAll[[j]]
        
        dataPlotBAU <- data.frame(wilayah=dataDefine$wilayah,
                                  komoditas=dataDefine$kom,
                                  sut=dataDefine$sut,
                                  tipeKebun=dataDefine$tipeKebun,
                                  NPV.Privat.RP=dataDefine$npv[1,1])
        
        dat_df <- rbind(dat_df,dataPlotBAU)
      }
      
      # yes <- plotly::plot_ly(dat_df, x = ~rownames(dat_df), y = ~NPV.Privat.RP, type = "bar", color = ~komoditas)
      
      # yes <- plotly::plot_ly(dat_df, x = ~wilayah, y = ~NPV.Privat.RP, type = "bar", color = ~komoditas)%>%
      #   layout(yaxis = list(title = "NPV Privat"), barmode = "group") %>%
      #   layout(title = "Bar Chart NPV Privat Berdasarkan Wilayah")
      # yes
      # format(sprintf("%0.2f", round(NPV.Privat.RP, digits = 2)),big.mark=".", trim=TRUE)
      # format(sprintf("%0.2f", round(NPV.Privat.RP, digits = 2)), big.mark=",", scientific=FALSE)
      # library(scales)
      # comma(dat_df$NPV.Privat.RP, digits = 1)
      # sprintf("%0.2f", round(dat_df$NPV.Privat.RP, digits = 2))
      # 
      # comma(sprintf("%0.2f", round(NPV.Privat.RP, digits = 2)), digits = 2)
      # sprintf("%f", pi)
      # sprintf("%.2f", pi)
      # sprintf("%1.2f", pi)
      # sprintf("%5.1f", pi)
      # sprintf("%05.1f", pi)
      # sprintf("%+f", pi)
      # sprintf("% f", pi)
      # sprintf("%-10f", pi) # left justified
      # sprintf("%e", pi)
      # sprintf("%E", pi)
      # sprintf("%g", pi)
      # sprintf("%g",   1e6 * pi) # -> exponential
      # sprintf("%.9g", 1e6 * pi) # -> "fixed"
      # sprintf("%G", 1e-6 * pi)
      # xx <- sprintf("%1$d %1$x %1$X", 0:15)
      # sprintf('%.2f', round(NPV.Privat.RP, digits = 2))
      
      ggplot(data=dat_df, aes(x=wilayah, y=NPV.Privat.RP, fill=komoditas)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=scales::comma(NPV.Privat.RP)), vjust=1, color="black",
                  position = position_dodge(0.9), size=3.5)+
        scale_fill_brewer(palette="Paired")+
        theme_minimal()
    })
    
    output$showBarSut <- renderPlot({
      datapath <- boxData$data[,1]
      nbaris <- nrow(datapath)
      vectorAlamat <- NULL
      for (k in 1:nbaris) {
        ekstractDataTable <- melt.data.table((datapath[k,]), measure.vars = 1)
        vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
      }
      
      listAll <- lapply(vectorAlamat, readRDS)
      
      # dat_list <- list()
      # for (j in 1:nrow(boxData$data)) {
      #   dataDefine <- listAll[[j]]
      #   
      #   dataPlotBAU <- data.frame(wilayah=dataDefine$wilayah,
      #                             komoditas=dataDefine$kom,
      #                             NPV.Privat.RP=dataDefine$npv[1,1])
      #   
      #   dat_list[[j]] <- dataPlotBAU
      # }
      # yes <- lapply(1:length(listAll), function(i){
      #   plotly::plot_ly(dat_list[[i]], x = ~komoditas, y = ~NPV.Privat.RP, type = "bar", color = ~komoditas)
      # })
      # subplot(yes)
      
      dat_df <- NULL
      for (j in 1:nrow(boxData$data)) {
        dataDefine <- listAll[[j]]
        
        dataPlotBAU <- data.frame(wilayah=dataDefine$wilayah,
                                  komoditas=dataDefine$kom,
                                  sut=dataDefine$sut,
                                  tipeKebun=dataDefine$tipeKebun,
                                  NPV.Privat.RP=dataDefine$npv[1,1])
        
        dat_df <- rbind(dat_df,dataPlotBAU)
      }
      
      # yes <- plotly::plot_ly(dat_df, x = ~rownames(dat_df), y = ~NPV.Privat.RP, type = "bar", color = ~komoditas)
      
      yes <- plotly::plot_ly(dat_df, x = ~komoditas, y = ~NPV.Privat.RP, type = "bar", color = ~sut)%>%
        layout(yaxis = list(title = "NPV Privat", title = "Plot NPV Berdasarkan Wilayah"),
               barmode = "group")
      yes
      

      
    })
    
    output$showBarCombine <- renderPlot({
      datapath <- boxData$data[,1]
      nbaris <- nrow(datapath)
      vectorAlamat <- NULL
      for (k in 1:nbaris) {
        ekstractDataTable <- melt.data.table((datapath[k,]), measure.vars = 1)
        vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
      }
      
      listAll <- lapply(vectorAlamat, readRDS)
      
      dat_df <- NULL
      for (j in 1:nrow(boxData$data)) {
        dataDefine <- listAll[[j]]
        
        dataPlotBAU <- data.frame(wilayah=dataDefine$wilayah,
                                  komoditas=dataDefine$kom,
                                  sut=dataDefine$sut,
                                  tipeKebun=dataDefine$tipeKebun,
                                  NPV.Privat.RP=dataDefine$npv[1,1])
        
        dat_df <- rbind(dat_df,dataPlotBAU)
      }
      
      
      # ggplot(data=dat_df, aes(x=komoditas, y=NPV.Privat.RP, fill=wilayah)) +
      #   geom_bar(stat="identity", position=position_dodge())+
      #   geom_text(aes(label=sprintf("%0.2f", round(NPV.Privat.RP, digits = 2))), vjust=-1, color="black",
      #             position = position_dodge(0.9), size=3.5)+
      #   scale_fill_brewer(palette="Paired")+
      #   theme_minimal()
      
      ggplot(data=dat_df, aes(x=komoditas, y=NPV.Privat.RP, fill=wilayah)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=scales::comma(NPV.Privat.RP)), vjust=-1, color="black",
                  position = position_dodge(0.9), size=3.5)+
        scale_fill_brewer(palette="Paired")+
        theme_minimal()
      
      # yes <- plotly::plot_ly(dat_df, x = ~rownames(dat_df), y = ~NPV.Privat.RP, type = "bar", color = ~komoditas)
      
      # yes <- plotly::plot_ly(dat_df, x = ~komoditas, y = ~NPV.Privat.RP, type = "bar", color = ~wilayah)%>%
      #       layout(yaxis = list(title = "NPV Privat" ),barmode = "group") %>%
      #       layout(title = "Bar Chart NPV Privat Berdasarkan Komoditas")
      # yes
      
    })
    
    
    
    output$showPlotProfPrivat <- renderPlotly({
      datapath <- boxData$data[,1]
      nbaris <- nrow(datapath)
      vectorAlamat <- NULL
      for (k in 1:nbaris) {
        ekstractDataTable <- melt.data.table((datapath[k,]), measure.vars = 1)
        vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
      }
      
      listAll <- lapply(vectorAlamat, readRDS)
      
      dat_df <- data.frame()
      for (j in 1:nrow(boxData$data)) {
        dataDefine <- listAll[[j]]
        dataPlotBAU <- data.frame(no = seq(1,length(dataDefine$p.profit.ha), 1),
                                  profit.tahunan=dataDefine$p.profit.ha,
                                  kom = dataDefine$kom,
                                  urutan = rownames(boxData$data)[j]
                                  )
        dat_df <- rbind(dat_df,dataPlotBAU)
      }
      
      dat_df$komUrutan <- paste0(dat_df$kom," (" ,dat_df$urutan,")")
      
      yes <- dat_df %>%
        group_by(komUrutan) %>%
        plot_ly(x=~no, y=~profit.tahunan, type='scatter', color=~komUrutan, mode="lines+markers", yaxis=~komUrutan) %>%
        layout(xaxis=list(title="Tahun", domain = list(0.15, 0.95)), yaxis=list(title="Profit Tahunan Privat"))%>%
        layout(legend = list(title=list(text='<b> Komoditas (Urutan) </b>'),
                                  orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.95, y = -0.25))
      # x = 0.95, y = 1.20))
        # layout(legend = list(title=list(text='<b> Komoditas (Urutan) </b>')))
      yes
      
      

    })
    
    output$showPlotProfSosial <- renderPlotly({
      datapath <- boxData$data[,1]
      nbaris <- nrow(datapath)
      vectorAlamat <- NULL
      for (k in 1:nbaris) {
        ekstractDataTable <- melt.data.table((datapath[k,]), measure.vars = 1)
        vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
      }
      
      listAll <- lapply(vectorAlamat, readRDS)
      
      dat_df <- data.frame()
      for (j in 1:nrow(boxData$data)) {
        dataDefine <- listAll[[j]]
        dataPlotBAU <- data.frame(no = seq(1,length(dataDefine$s.profit.ha), 1),
                                  profit.tahunan=dataDefine$s.profit.ha,
                                  kom = dataDefine$kom,
                                  urutan = rownames(boxData$data)[j]
        )
        dat_df <- rbind(dat_df,dataPlotBAU)
      }
      
      dat_df$komUrutan <- paste0(dat_df$kom," (" ,dat_df$urutan,")")
      
      yes <- dat_df %>%
        group_by(komUrutan) %>%
        plot_ly(x=~no, y=~profit.tahunan, type='scatter', color=~komUrutan, mode="lines+markers", yaxis=~komUrutan) %>%
        layout(xaxis=list(title="Tahun", domain = list(0.15, 0.95)), yaxis=list(title="Profit Tahunan Privat"))%>%
        layout(legend = list(title=list(text='<b> Komoditas (Urutan) </b>'),
                             orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.95, y = -0.25)) 
        # layout(legend = list(title=list(text='<b> Komoditas (Urutan) </b>')))
      yes
    })
    
    
    output$profitPlotKumP <- renderPlotly({
      datapath <- boxData$data[,1]
      nbaris <- nrow(datapath)
      vectorAlamat <- NULL
      for (k in 1:nbaris) {
        ekstractDataTable <- melt.data.table((datapath[k,]), measure.vars = 1)
        vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
      }
      
      listAll <- lapply(vectorAlamat, readRDS)
      
      dat_df <- data.frame()
      for (j in 1:nrow(boxData$data)) {
        dataDefine <- listAll[[j]]
        dataPlotBAU <- data.frame(no = seq(1,length(dataDefine$p.profit.ha), 1),
                                  profit.kumulatif=cumsum(dataDefine$p.profit.ha),
                                  kom = dataDefine$kom,
                                  urutan = rownames(boxData$data)[j]
        )
        dat_df <- rbind(dat_df,dataPlotBAU)
      }
      
      dat_df$komUrutan <- paste0(dat_df$kom," (" ,dat_df$urutan,")")
      
      yes <- dat_df %>%
        group_by(komUrutan) %>%
        plot_ly(x=~no, y=~profit.kumulatif, type='scatter', color=~komUrutan, mode="lines+markers", yaxis=~komUrutan) %>%
        layout(xaxis=list(title="Tahun", domain = list(0.15, 0.95)), yaxis=list(title="Profit Kumulatif Privat"))%>%
        layout(legend = list(title=list(text='<b> Komoditas (Urutan) </b>'),
                             orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.95, y = -0.25))
      
      yes 
      
      # profit.gab %>%
      #   group_by(yaxis) %>%
      #   plot_ly(x=~no, y=~profit.kumulatif, type='scatter', color=~yaxis, mode="lines+markers", yaxis=~yaxis) %>%
      #   layout(xaxis=list(title="Tahun", domain = list(0.15, 0.95)), yaxis=list(title="Profit Kumulatif Privat"), yaxis2=SIMULASI)%>%
      #   layout(legend = list(orientation = "h",   # show entries horizontally
      #                        xanchor = "center",  # use center of legend as anchor
      #                        x = 0.95))             # put legend in center of x-axis
    })
    
    output$profitPlotKumS <- renderPlotly({
      datapath <- boxData$data[,1]
      nbaris <- nrow(datapath)
      vectorAlamat <- NULL
      for (k in 1:nbaris) {
        ekstractDataTable <- melt.data.table((datapath[k,]), measure.vars = 1)
        vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
      }
      
      listAll <- lapply(vectorAlamat, readRDS)
      
      dat_df <- data.frame()
      for (j in 1:nrow(boxData$data)) {
        dataDefine <- listAll[[j]]
        dataPlotBAU <- data.frame(no = seq(1,length(dataDefine$s.profit.ha), 1),
                                  profit.kumulatif=cumsum(dataDefine$s.profit.ha),
                                  kom = dataDefine$kom,
                                  urutan = rownames(boxData$data)[j]
        )
        dat_df <- rbind(dat_df,dataPlotBAU)
      }
      
      dat_df$komUrutan <- paste0(dat_df$kom," (" ,dat_df$urutan,")")
      
      yes <- dat_df %>%
        group_by(komUrutan) %>%
        plot_ly(x=~no, y=~profit.kumulatif, type='scatter', color=~komUrutan, mode="lines+markers", yaxis=~komUrutan) %>%
        layout(xaxis=list(title="Tahun", domain = list(0.15, 0.95)), yaxis=list(title="Profit Kumulatif Sosial"))%>%
        layout(legend = list(title=list(text='<b> Komoditas (Urutan) </b>'),
                             orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.95, y = -0.25))
      
      yes 
    })
    
    
    observeEvent(event_data("plotly_click"), {
      barData = event_data("plotly_click")
      showModal(modalDialog(title = "Informasi Grafik",
                            footer = modalButton("Kembali ke Beranda"),
                            # footer= list(
                            #   actionButton(("batalSunting_button_kuantitasOutput"), "Batal", style="color: white;background-color: red;")
                            # ),
                            size="s",renderPrint(barData[c(3,4)])
                                                                # c(barData$x,barData$y)
                                                ))
    })
    
    
    
    output$showPlotAllKomoditas <- renderPlotly({
      withProgress(message = 'Collecting data in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       sum(runif(10000000,0,1))
                     }
                   })
      
      plotAllKomoditas() 
    })
    ################################################################################
    #                                                                              #
    #                                RESULT                                        #
    #                                                                              #
    ################################################################################

    
    
    data.graph <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
      
      datapath <- boxData$data[,1]
      nbaris <- nrow(datapath)
      vectorAlamat <- NULL
      for (k in 1:nbaris) {
        ekstractDataTable <- melt.data.table((datapath[k,]), measure.vars = 1)
        vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
      }
      
      listAll <- lapply(vectorAlamat, readRDS)
      
      dat_list <- list()
      for (j in 1:nrow(boxData$data)) {
        dataDefine <- listAll[[j]]
        
        print("cek data gab ")
        # datapath <- paste0("data/", input$sut, "/",input$kom, "/")
        # fileName <- paste0(datapath,"resultTemplate","_",
        #                    input$sut,"_",input$kom,"_",
        #                    input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
        # dataDefine <- listAll[[j]]
        
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
        
        
        
        #### price ####
        price.in <-  dataDefine$priceInput
        price.in <- cbind(grup="input",price.in)
        price.out <-  dataDefine$priceOutput
        price.out <- cbind(grup="output",price.out)
        price.in[is.na(price.in)] <- 0
        price.out[is.na(price.out)] <- 0
        price.all <- rbind(price.in, price.out)
        
        
        if (dataDefine$tipeKebun[1] == "LARGE SCALE"){
          
          yearIO <- ncol(io.in)-5 #banyaknya tahun pada tabel io krn ada tambhan kolom bagian pada large scale
          
          p.price<-select(price.all, -harga.sosial) #remove harga sosial
          p.year<-data.frame(replicate(yearIO,p.price$harga.privat)) #replicate nilai private price sebanyak n tahun
          colnames(p.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
          p.price<-cbind(status="harga.privat" ,p.price[c("grup", "bagian","komponen", "jenis", "unit.harga")],p.year)
          p.price <- p.price %>% mutate_if(is.factor,as.character) #change factor var to char var
          
          s.price<-select(price.all, -harga.privat) #remove harga privat
          s.year<-data.frame(replicate(yearIO,s.price$harga.sosial))
          colnames(s.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
          s.price<-cbind(status="harga.sosial",s.price[c("grup", "bagian", "komponen", "jenis", "unit.harga")],s.year)
          s.price <- s.price %>% mutate_if(is.factor,as.character) #change factor var to char
          
          price.all.year <- rbind(p.price, s.price)
          
          data.gab <- bind_rows(io.all,
                                price.all.year) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
          
          # hitung npv --------------------------------------------------------------
          dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
          dataPrivat <- filter(data.gab,status == c("harga.privat")) #filter data private price
          p.budget <- dataGeneral[-(c(1:6,ncol(dataGeneral)))] * dataPrivat[-c(1:6,ncol(dataPrivat))] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
          p.budget <- cbind(dataGeneral[c("status","grup","bagian","komponen","jenis")],dataPrivat["unit.harga"],p.budget) #memunculkan kembali variabel 1 sd 5
          p.budget <- p.budget %>%mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
          
          #perkalian antara general dengan Social Price
          dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
          s.budget <- dataGeneral[-c(1:6,ncol(dataGeneral))] * dataSosial[-c(1:6,ncol(dataSosial))]
          s.budget <- cbind(dataGeneral[c("status","grup","bagian","komponen","jenis")],dataSosial["unit.harga"],s.budget)
          s.budget <- s.budget %>%
            mutate(status = case_when(status == "general" ~ "social budget"))
          
          ################ penghitungan NPV
          p.rev.output <- p.budget %>%
            filter(str_detect(grup,"output"))
          s.rev.output <- s.budget %>%
            filter(str_detect(grup,"output"))
          
          p.sum.rev <- p.rev.output[,-(1:6)] %>%
            colSums(na.rm = T)
          s.sum.rev <- s.rev.output[,-(1:6)] %>%
            colSums(na.rm = T)
          # sum(p.sum.rev)
          sum(s.sum.rev)
          
          p.trad.input <- p.budget %>%
            filter(str_detect(bagian,"input"))
          p.trad.input <- p.trad.input[,-(1:6)] %>%
            colSums(na.rm = T)
          # sum(p.trad.input)
          s.trad.input <- s.budget %>%
            filter(str_detect(bagian,"input"))
          s.trad.input <- s.trad.input[,-(1:6)] %>%
            colSums(na.rm = T)
          sum(s.trad.input)
          
          p.trad.capital <- p.budget %>%
            filter(str_detect(bagian,"tradable capital"))
          p.trad.capital <- p.trad.capital[,-(1:6)] %>%
            colSums(na.rm = T)
          # sum(p.trad.capital)
          s.trad.capital <- s.budget %>%
            filter(str_detect(bagian,"tradable capital"))
          s.trad.capital <- s.trad.capital[,-(1:6)] %>%
            colSums(na.rm = T)
          sum(s.trad.capital)
          
          p.factor.labor<- p.budget %>%
            filter(str_detect(bagian,"labor"))
          
          p.labor.admin<- p.factor.labor %>%
            filter(str_detect(komponen,"admin"))
          p.labor.admin <- p.labor.admin[,-(1:6)] %>%
            colSums(na.rm = T)
          # sum(p.labor.admin)
          
          p.labor.skill <- p.factor.labor %>%
            filter(str_detect(komponen,c("ahli")))
          p.labor.skill <- p.labor.skill[,-(1:6)] %>%
            colSums(na.rm = T)
          # sum(p.labor.skill)
          
          p.labor.unskill<- p.factor.labor %>%
            filter(str_detect(komponen,c("unskill")))
          p.labor.unskill <- p.labor.unskill[,-(1:6)] %>%
            colSums(na.rm = T)
          # sum(p.labor.unskill)
          
          s.factor.labor<- s.budget %>%
            filter(str_detect(bagian,"labor"))
          
          s.labor.admin<- s.factor.labor %>%
            filter(str_detect(komponen,"admin"))
          s.labor.admin <- s.labor.admin[,-(1:6)] %>%
            colSums(na.rm = T)
          sum(s.labor.admin)
          
          s.labor.skill <- s.factor.labor %>%
            filter(str_detect(komponen,c("ahli")))
          s.labor.skill <- s.labor.skill[,-(1:6)] %>%
            colSums(na.rm = T)
          sum(s.labor.skill)
          
          s.labor.unskill<- s.factor.labor %>%
            filter(str_detect(komponen,c("unskill")))
          s.labor.unskill <- s.labor.unskill[,-(1:6)] %>%
            colSums(na.rm = T)
          sum(s.labor.unskill)
          
          p.working.capital<- p.budget %>%
            filter(str_detect(bagian,"working capital privat"))
          p.working.capital <- p.working.capital[,-(1:6)] %>%
            colSums(na.rm = T)
          # sum(p.factor.capital)
          s.working.capital<- s.budget %>%
            filter(str_detect(bagian,"working capital sosial"))
          s.working.capital <- s.working.capital[,-(1:6)] %>%
            colSums(na.rm = T)
          
          
          p.factor.capital<- p.budget %>%
            filter(str_detect(bagian,"factor capital"))
          p.factor.capital <- p.factor.capital[,-(1:6)] %>%
            colSums(na.rm = T)
          # sum(p.factor.capital)
          s.factor.capital<- s.budget %>%
            filter(str_detect(bagian,"factor capital"))
          s.factor.capital <- s.factor.capital[,-(1:6)] %>%
            colSums(na.rm = T)
          # sum(s.factor.capital)
          
          p.total.cost <- p.trad.input + p.trad.capital + p.labor.admin + p.labor.skill + p.labor.unskill + p.working.capital + p.factor.capital
          s.total.cost <- s.trad.input + s.trad.capital + s.labor.admin + s.labor.skill + s.labor.unskill + s.working.capital + s.factor.capital
          
          
          
          # PERHITUNGAN MANUAL
          # p.sum.rev<-c(value0,p.sum.rev)
          # p.trad.input<-c(value0,p.trad.input)
          # p.trad.capital<-c(value0,p.trad.capital)
          # p.labor.admin<-c(value0,p.labor.admin)
          # p.labor.skill<-c(value0,p.labor.skill)
          # p.labor.unskill<-c(value0,p.labor.unskill)
          # p.factor.capital<-c(value0,p.factor.capital)
          # 
          # s.sum.rev<-c(value0,s.sum.rev)
          # s.trad.input<-c(value0,s.trad.input)
          # s.trad.capital<-c(value0,s.trad.capital)
          # s.labor.admin<-c(value0,s.labor.admin)
          # s.labor.skill<-c(value0,s.labor.skill)
          # s.labor.unskill<-c(value0,s.labor.unskill)
          # s.factor.capital<-c(value0,s.factor.capital)
          # 
          # npv.p.sum.rev<-npv(dataDefine$rate.p/100,p.sum.rev)
          # npv.p.trad.input<-npv(dataDefine$rate.p/100,p.trad.input)
          # npv.p.trad.capital<-npv(dataDefine$rate.p/100,p.trad.capital)
          # npv.p.labor.admin<-npv(dataDefine$rate.p/100,p.labor.admin)
          # npv.p.labor.skill<-npv(dataDefine$rate.p/100,p.labor.skill)
          # npv.p.labor.unskill<-npv(dataDefine$rate.p/100,p.labor.unskill)
          # npv.p.factor.capital<-npv(dataDefine$rate.p/100,p.factor.capital)
          # npv.p.total.cost <- npv.p.trad.input +npv.p.trad.capital + npv.p.labor.admin + npv.p.labor.skill + npv.p.labor.unskill + npv.p.factor.capital
          # npv.p <- npv.p.sum.rev - npv.p.total.cost
          # 
          # npv.s.sum.rev<-npv(dataDefine$rate.s/100,s.sum.rev)
          # npv.s.trad.input<-npv(dataDefine$rate.s/100,s.trad.input)
          # npv.s.trad.capital<-npv(dataDefine$rate.s/100,s.trad.capital)
          # npv.s.labor.admin<-npv(dataDefine$rate.s/100,s.labor.admin)
          # npv.s.labor.skill<-npv(dataDefine$rate.s/100,s.labor.skill)
          # npv.s.labor.unskill<-npv(dataDefine$rate.s/100,s.labor.unskill)
          # npv.s.factor.capital<-npv(dataDefine$rate.s/100,s.factor.capital)
          # npv.s.total.cost <- npv.s.trad.input +npv.s.trad.capital + npv.s.labor.admin + npv.s.labor.skill + npv.s.labor.unskill + npv.s.factor.capital
          # npv.s <- npv.s.sum.rev - npv.s.total.cost
          
          
          p.profit <- p.sum.rev - p.total.cost
          s.profit <- s.sum.rev - s.total.cost
          sum(p.profit)
          sum(s.profit)
          
          value0 <- 0
          p.profit <- c(value0,p.profit)
          s.profit <- c(value0,s.profit)
          npv.p<-npv(dataDefine$rate.p/100,p.profit)
          npv.s<-npv(dataDefine$rate.s/100,s.profit)
          
          npv.p.ha <- npv.p/dataDefine$totalArea
          npv.s.ha <- npv.s/dataDefine$totalArea
          
          hsl.npv<-data.frame(PRIVATE=npv.p.ha,SOCIAL=npv.s.ha)
          
          npv.p.ha.us<-npv.p.ha/dataDefine$nilai.tukar
          npv.s.ha.us<-npv.s.ha/dataDefine$nilai.tukar
          npv.us<-data.frame(PRIVATE=npv.p.ha.us,SOCIAL=npv.s.ha.us)
          hsl.npv<-rbind(hsl.npv,npv.us)
          
          #browser()
          
          rownames(hsl.npv)<-c("NPV (Rp/Ha)", "NPV (US/Ha)")
          hsl.npv
          # ending  npv --------------------------------------------------------------
          
          # hitung Discounted estab cost --------------------------------------------------------------
          
          ################ penghitungan dec
          p.profit.ha <- p.profit/dataDefine$totalArea
          p.positif.cashflow <- ifelse(p.profit.ha > 0, 1, 0)
          
          p.total.cost.0 <- c(value0,p.total.cost)
          p.est.cost <- ifelse(p.positif.cashflow == 1, 0, p.total.cost.0)
          
          # p.est.cost <- length(p.positif.cashflow)
          # for(i in seq_along(p.positif.cashflow)){
          #   if(p.positif.cashflow[i] == 1){
          #     p.est.cost[i] <- 0
          #   }else{
          #     p.est.cost[i] <- p.total.cost.0[i]
          #   }
          # }
          
          s.profit.ha <- s.profit/dataDefine$totalArea
          s.positif.cashflow <- ifelse(s.profit.ha > 0, 1, 0)
          
          s.total.cost.0 <- c(value0,s.total.cost)
          s.est.cost <- ifelse(s.positif.cashflow == 1, 0, s.total.cost.0)
          
          # s.est.cost <- length(s.positif.cashflow)
          # for(i in seq_along(s.positif.cashflow)){
          #   if(s.positif.cashflow[i] == 1){
          #     s.est.cost[i] <- 0
          #   }else{
          #     s.est.cost[i] <- s.total.cost.0[i]
          #   }
          # }
          
          npv.p.dec<-npv(dataDefine$rate.p/100,p.est.cost)
          npv.s.dec<-npv(dataDefine$rate.s/100,s.est.cost)
          
          dec.p <- npv.p.dec/1000/dataDefine$totalArea
          dec.s <- npv.s.dec/1000/dataDefine$totalArea
          
          
          dec<-data.frame(PRIVATE=dec.p,SOCIAL=dec.s)
          rownames(dec)<-c("Discounted Est. Cost (MRp/Ha)")
          dec
          # ending  Discounted estab cost ------------------------------------------------------- 
          
          # hitung Year to positive cashflow --------------------------------------------------------------
          ############# PERHITUNGAN ypc
          ypc.p <- yearIO + 1 - sum(p.positif.cashflow)
          ypc.s <- yearIO + 1 - sum(s.positif.cashflow)
          
          ypc<-data.frame(PRIVATE=ypc.p,SOCIAL=ypc.s)
          rownames(ypc)<-c("Year to positive cashflow")
          ypc
          # ending  Year to positive cashflow ------------------------------------------------------- 
          
          # hitung Labor for establishment --------------------------------------------------------------
          # tabel labor/pekerja
          table.labor<- io.in %>%
            filter(str_detect(bagian,"labor"))
          sum.table.labor <- table.labor[,paste0(c(rep("Y", yearIO)),1:yearIO)] %>%
            colSums(na.rm = T)
          
          yearLabor <- ifelse(length(input$tahunPekerja ) == 0,25,numeric(input$tahunPekerja))
          sum.table.labor.25 <- sum.table.labor[1:yearLabor]
          total.labor <- sum(sum.table.labor.25)
          
          #tabel land/lahan
          # baseScene <- filter(readDataTemplate,str_detect(bagian,"dasar"))
          # landScene <- baseScene[,c("bagian","komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
          # transpose.landScene <- data.frame(t(landScene[,c(paste0(c(rep("Y", yearIO)),1:yearIO))]))
          # colnames(transpose.landScene) <- c("per.year")
          # cum.landScene <- within(transpose.landScene,cummulative.land <- cumsum(per.year)) #buat kolom cumulativ land
          cum.landScene <- as.matrix(dataDefine$cum.landScene)
          
          
          ############# PERHITUNGAN lfe
          p.total.labor.est <- ifelse(p.profit.ha[1:yearLabor+1] < 0,sum.table.labor.25 , 0)
          p.total.labor.opr <- ifelse(p.profit.ha[1:yearLabor+1] < 0,0,sum.table.labor.25)
          p.avg.labor.est <- ifelse(p.total.labor.est > 0,ifelse(p.total.labor.est == 0,0,p.total.labor.est/cum.landScene[2,1:yearLabor]) , p.total.labor.est/cum.landScene[1:yearLabor])
          p.avg.labor.opr <- ifelse(p.total.labor.opr > 0,ifelse(p.total.labor.opr == 0,0,p.total.labor.opr/cum.landScene[2,1:yearLabor]) , p.total.labor.opr/cum.landScene[1:yearLabor])
          p.avg.labor.est[is.na(p.avg.labor.est)] <- 0
          p.avg.labor.opr[is.na(p.avg.labor.opr)] <- 0
          
          s.total.labor.est <- ifelse(s.profit.ha[1:yearLabor+1] < 0,sum.table.labor.25 , 0)
          s.total.labor.opr <- ifelse(s.profit.ha[1:yearLabor+1] < 0,0,sum.table.labor.25)
          s.avg.labor.est <- ifelse(s.total.labor.est > 0,ifelse(s.total.labor.est == 0,0,s.total.labor.est/cum.landScene[2,1:yearLabor]) , s.total.labor.est/cum.landScene[1:yearLabor])
          s.avg.labor.opr <- ifelse(s.total.labor.opr > 0,ifelse(s.total.labor.opr == 0,0,s.total.labor.opr/cum.landScene[2,1:yearLabor]) , s.total.labor.opr/cum.landScene[1:yearLabor])
          s.avg.labor.est[is.na(s.avg.labor.est)] <- 0
          s.avg.labor.opr[is.na(s.avg.labor.opr)] <- 0
          
          lfe.p <- sum(p.avg.labor.est)
          lfe.s <- sum(s.avg.labor.est)
          
          lfe<-data.frame(PRIVATE=lfe.p,SOCIAL=lfe.s)
          rownames(lfe)<-c("Labor for Est. (HOK/Ha)")
          lfe
          # ending  Labor for establishment ------------------------------------------------------- 
          
          ############# PERHITUNGAN lfo
          lfo.p <- mean(p.avg.labor.opr[p.avg.labor.opr!=0])
          lfo.s <- mean(s.avg.labor.opr[s.avg.labor.opr!=0])
          
          lfo<-data.frame(PRIVATE=lfo.p,SOCIAL=lfo.s)
          rownames(lfo)<-c("Labor for Operation. (HOK/Ha/th)")
          lfo
          # ending  Labor for Operation -------------------------------------------------------
          
          
          # hitung hp --------------------------------------------------------------
          ############# PERHITUNGAN HARVESTING PRODUCT
          fil.prod <- dataGeneral %>%  filter(str_detect(grup,"output")) #filter io untuk grup output (hasil panen)
          fil.prod <- fil.prod %>%  filter(str_detect(komponen,"utama"))
          sum.prod <- fil.prod[,c(paste0(c(rep("Y", yearIO)),1:yearIO))] %>%
            colSums(na.rm = T)
          tot.prod <- sum(sum.prod)
          
          hp <- data.frame( tot.prod/total.labor)/1000 # karena ton jadi di bagi 1000
          colnames(hp)<-c("Harvesting Product (ton/HOK)")
          rownames(hp) <- c("Value")
          hp
          
          # ending  hp ------------------------------------------------------- 
          
          # Total area
          totArea <- data.frame(dataDefine$totalArea)
          colnames(totArea)<-c("Total Area (Ha)")
          rownames(totArea) <- c("Value")
          totArea
          
          tabel1 <- rbind(hsl.npv,dec,ypc,lfe,lfo)
          tabel1[] <- lapply(tabel1, function(i) sprintf('%.6g', i))
          tabel1
          
          tabel2 <- data.frame(t(cbind(hp,totArea)))
          tabel2[] <- lapply(tabel2, function(i) sprintf('%.6g', i))
          tabel2
          
          # tabel profit 
          tabel.p.profit <- t(as.data.frame(t(p.profit.ha)))
          tabel.s.profit <- t(as.data.frame(t(s.profit.ha)))
          tabel.profit <- cbind(tabel.p.profit,tabel.s.profit)
          rownames(tabel.profit) <- paste0(c(rep("Y", yearIO)),0:yearIO)
          colnames(tabel.profit) <- c("p.profit","s.profit")
          
          dataView <- list(tabel1=tabel1,tabel2=tabel2, tabel.profit = tabel.profit)
          # tabelGab 
          # dataView <- datatable(data.graph()$tabel2, option=list(dom = "t"))
          dat_list[[j]] <- dataView
          print("cek ending LS")
          print(dat_list[[j]]$tabel1)
          
          # datGraph$data[[j]] <- dataView
          dat_list
          
          
        }else{
          
          yearIO <- ncol(io.in)-4 #banyaknya tahun pada tabel io 
          p.price<-select(price.all, -harga.sosial) #remove harga sosial
          p.year<-data.frame(replicate(yearIO,p.price$harga.privat)) #replicate nilai private price sebanyak n tahun
          colnames(p.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
          p.price<-cbind(status="harga.privat" ,p.price[c(1:4)],p.year)
          p.price <- p.price %>% mutate_if(is.factor,as.character) #change factor var to char var
          
          s.price<-select(price.all, -harga.privat) #remove harga sosial
          s.year<-data.frame(replicate(yearIO,s.price$harga.sosial))
          colnames(s.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
          s.price<-cbind(status="harga.sosial",s.price[c(1:4)],s.year)
          s.price <- s.price %>% mutate_if(is.factor,as.character) #change factor var to char
          
          price.all.year <- rbind(p.price, s.price)
          
          if (is.null(dataDefine$capital)){
            # capital = NULL
            data.gab <- bind_rows(io.all,
                                  price.all.year) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
            
            # hitung npv --------------------------------------------------------------
            dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
            dataPrivat <- filter(data.gab,status == c("harga.privat")) #filter data private price
            p.budget <- dataGeneral[-(c(1:5,ncol(dataGeneral)))] * dataPrivat[-c(1:5,ncol(dataPrivat))] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
            p.budget <- cbind(dataGeneral[c(1:4)],dataPrivat["unit.harga"],p.budget) #memunculkan kembali variabel 1 sd 5
            p.budget <- p.budget %>%
              mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
            
            #perkalian antara general dengan Social Price
            dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
            s.budget <- dataGeneral[-c(1:5,ncol(dataGeneral))] * dataSosial[-c(1:5,ncol(dataSosial))]
            s.budget <- cbind(dataGeneral[c(1:4)],dataSosial["unit.harga"],s.budget)
            s.budget <- s.budget %>%
              mutate(status = case_when(status == "general" ~ "social budget"))
            
            ################ penghitungan NPV
            p.cost.input <- p.budget %>%
              filter(str_detect(grup,"input"))
            
            s.cost.input <- s.budget %>%
              filter(str_detect(grup,"input"))
            
            p.sum.cost<- p.cost.input[,-(1:5)] %>%
              colSums(na.rm = T)
            s.sum.cost<- s.cost.input[,-(1:5)] %>%
              colSums(na.rm = T)
            
            p.rev.output <- p.budget %>%
              filter(str_detect(grup,"output"))
            s.rev.output <- s.budget %>%
              filter(str_detect(grup,"output"))
            
            p.sum.rev <- p.rev.output[,-(1:5)] %>%
              colSums(na.rm = T)
            s.sum.rev <- s.rev.output[,-(1:5)] %>%
              colSums(na.rm = T)
            
            
            p.profit <- p.sum.rev - p.sum.cost
            s.profit <- s.sum.rev - s.sum.cost
            profit0 <- 0
            p.profit<-c(profit0,p.profit)
            s.profit<-c(profit0,s.profit)
            
            npv.p<-npv(dataDefine$rate.p/100,p.profit)
            npv.s<-npv(dataDefine$rate.s/100,s.profit)
            
            hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
            
            npv.p.us<-npv.p/dataDefine$nilai.tukar
            npv.s.us<-npv.s/dataDefine$nilai.tukar
            npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
            hsl.npv<-rbind(hsl.npv,npv.us)
            
            #browser()
            
            rownames(hsl.npv)<-c("NPV (Rp/Ha)", "NPV (US/Ha)")
            hsl.npv
            # ending  npv --------------------------------------------------------------
            
          }else if (!is.null(dataDefine$capital)){
            capital <- cbind(grup="input",dataDefine$capital)
            
            # menambahkan pada tabel io matrix bernilai 1
            ioKapital <- data.frame(matrix(data=1,nrow = nrow(capital) , ncol = ncol(dataDefine$ioInput)-3))
            colnames(ioKapital)<-paste0(c(rep("Y", yearIO)),1:yearIO)
            ioKapital<-cbind(status="modal kapital" ,capital[c(1:4)],ioKapital)
            ioKapital <- ioKapital %>% mutate_if(is.factor,as.character) #change factor var to char var
            
            
            kapitalPrivat <- filter(capital,komponen == c("modal kapital privat"))
            kapitalPrivat <- cbind(status ="harga.privat",kapitalPrivat )
            kapitalPrivat <- kapitalPrivat %>% mutate_if(is.factor,as.character) #change factor var to char var
            
            kapitalSosial <- filter(capital,komponen == c("modal kapital sosial"))
            kapitalSosial <- cbind(status ="harga.sosial",kapitalSosial )
            kapitalSosial <- kapitalSosial %>% mutate_if(is.factor,as.character) #change factor var to char var
            
            data.gab <- bind_rows(io.all, ioKapital,
                                  price.all.year, 
                                  kapitalPrivat, kapitalSosial) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
            # hitung npv --------------------------------------------------------------
            dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
            dataCapitalAll <- filter(data.gab,status == c("modal kapital"))
            
            
            dataGeneralPrivat <- filter(dataCapitalAll,komponen == c("modal kapital privat"))
            dataGeneralPrivat <- rbind(dataGeneral,dataGeneralPrivat)
            dataPrivat <- filter(data.gab,status == c("harga.privat"))
            p.budget <- dataGeneralPrivat[-(c(1:5,36))] * dataPrivat[-c(1:5,36)] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
            p.budget <- cbind(dataGeneralPrivat[c(1:4)],dataPrivat["unit.harga"],p.budget) #memunculkan kembali variabel 1 sd 5
            p.budget <- p.budget[-1] #menghilangkan label status yang awal
            p.budget <- cbind(status = "privat budget", p.budget) #merename keseluruhan tabel status
            
            
            
            
            #perkalian antara general dengan Social Price
            dataGeneralSosial <- filter(dataCapitalAll,komponen == c("modal kapital sosial"))
            dataGeneralSosial <- rbind(dataGeneral,dataGeneralSosial)
            dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
            s.budget <- dataGeneralSosial[-c(1:5,36)] * dataSosial[-c(1:5,36)]
            s.budget <- cbind(dataGeneralSosial[c(1:4)],dataSosial["unit.harga"],s.budget)
            s.budget <- s.budget[-1] #menghilangkan label status yang awal
            s.budget <- cbind(status = "sosial budget", s.budget) #merename keseluruhan tabel status
            
            ################ penghitungan NPV
            p.cost.input <- p.budget %>%
              filter(str_detect(grup,"input"))
            
            s.cost.input <- s.budget %>%
              filter(str_detect(grup,"input"))
            
            p.sum.cost<- p.cost.input[,-(1:5)] %>%
              colSums(na.rm = T)
            s.sum.cost<- s.cost.input[,-(1:5)] %>%
              colSums(na.rm = T)
            
            p.rev.output <- p.budget %>%
              filter(str_detect(grup,"output"))
            s.rev.output <- s.budget %>%
              filter(str_detect(grup,"output"))
            
            p.sum.rev <- p.rev.output[,-(1:5)] %>%
              colSums(na.rm = T)
            s.sum.rev <- s.rev.output[,-(1:5)] %>%
              colSums(na.rm = T)
            
            
            p.profit <- p.sum.rev - p.sum.cost
            s.profit <- s.sum.rev - s.sum.cost
            profit0 <- 0
            p.profit<-c(profit0,p.profit)
            s.profit<-c(profit0,s.profit)
            
            npv.p<-npv(dataDefine$rate.p/100,p.profit)
            npv.s<-npv(dataDefine$rate.s/100,s.profit)
            
            hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
            
            npv.p.us<-npv.p/dataDefine$nilai.tukar
            npv.s.us<-npv.s/dataDefine$nilai.tukar
            npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
            hsl.npv<-rbind(hsl.npv,npv.us)
            
            #browser()
            
            rownames(hsl.npv)<-c("NPV (Rp/Ha)", "NPV (US/Ha)")
            hsl.npv
            # ending  npv --------------------------------------------------------------
            
          }
          
          
          # hitung nlc --------------------------------------------------------------
          
          ################ penghitungan NLC
          
          p.tot.cost<- sum(p.sum.cost)
          s.tot.cost<- sum(s.sum.cost)
          
          p.labor.input <- p.budget %>% filter(str_detect(komponen,c("tenaga kerja|tk|kerja|tenaga")))
          s.labor.input <- s.budget %>% filter(str_detect(komponen,c("tenaga kerja|tk|kerja|tenaga")))
          
          p.sum.labor <- p.labor.input[,-(1:5)] %>%
            sum(na.rm = T)
          s.sum.labor <- s.labor.input[,-(1:5)] %>%
            sum(na.rm = T)
          
          
          
          nlc.p <- (p.tot.cost - p.sum.labor)/1000000
          nlc.s <- (s.tot.cost - s.sum.labor)/1000000
          nlc<-data.frame(PRIVATE=nlc.p,SOCIAL=nlc.s)
          rownames(nlc)<-c("Non Labor Cost  for 1 cycle (Juta Rp/Ha)")
          nlc
          lcost <- data.frame(p.sum.labor/1000000)
          colnames(lcost)<-c("Labor Cost for 1 cycle (MRp/ha)")
          rownames(lcost) <- c("Value")
          lcost
          
          nlcost <- data.frame(nlc.p)
          colnames(nlcost)<-c("Non Labor Cost for 1 cycle (MRp/ha)")
          rownames(nlcost) <- c("Value")
          nlcost
          # ending  nlc ------------------------------------------------------- 
          
          # hitung EC --------------------------------------------------------------
          ############# PERHITUNGAN ESTABLISHMENT COST
          p.ec <- p.sum.cost[[1]]/1000000
          s.ec <- s.sum.cost[[1]]/1000000
          ec <- data.frame(p.ec,s.ec)
          ec<-data.frame(PRIVATE=p.ec,SOCIAL=s.ec)
          rownames(ec)<-c("Establishment cost (1st year only, MRp/ha)")
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
          
          # hitung profit --------------------------------------------------------------
          p.profit.ha <- p.profit[-1]/readDataTemplate$total.area[1]
          s.profit.ha <- s.profit[-1]/readDataTemplate$total.area[1]
          # ending  profit ------------------------------------------------------
          
          
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
          
          pvCost <- presentValue(costYear,c(1:length(costYear)),rate.p/100)
          
          revenueYear <- p.budget %>%
            filter(str_detect(grup,"output"))%>%
            select(contains('Y'))%>%
            colSums(na.rm = T)
          
          pvRevenue <- presentValue(revenueYear,c(1:length(revenueYear)),rate.p/100)
          
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
          showRateP <- (dataDefine$rate.p)
          showRateS <- (dataDefine$rate.s)
          showExRate <- (dataDefine$nilai.tukar)
          showBauMacro <- rbind(showRateP,showRateS,showExRate)
          rownames(showBauMacro)<-c("Discount Rate Private", "Discount Rate Social", "Nilai Tukar Rupiah")
          colnames(showBauMacro) <- c("Nilai")
          showBauMacro
          
          # ending  ------------------------------------------------------- 
          
          tabel1 <- rbind(hsl.npv,nlc,ec)
          tabel1[] <- lapply(tabel1, function(i) sprintf('%.6g', i))
          tabel1
          
          tabel2 <- rbind(hp,lr,pi,bcr,rtl,lcost, nlcost, showBauMacro)
          tabel2[] <- lapply(tabel2, function(i) sprintf('%.6g', i))
          tabel2
          
          # tabel profit 
          tabel.p.profit <- t(as.data.frame(t(p.profit)))
          tabel.s.profit <- t(as.data.frame(t(s.profit)))
          tabel.profit <- cbind(tabel.p.profit,tabel.s.profit)
          rownames(tabel.profit) <- paste0(c(rep("Y", yearIO)),0:yearIO)
          colnames(tabel.profit) <- c("p.profit","s.profit")
          
          dataView <- list(tabel1=tabel1,tabel2=tabel2, tabel.profit = tabel.profit)
          # dataView <- datatable(data.graph()$tabel2, option=list(dom = "t"))
          dat_list[[j]] <- dataView
          print("cek ending ")
          print(dat_list[[j]]$tabel1)
          # datGraph$data[[j]] <- dataView
          dat_list
          # tabelGab 
        }
      }
    })
    
    data.graph.new <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
      # observeEvent(input$running_button,{
      # browser()
      # 
      # aktifin dataTemplate
      # agar ketika run pertama kali yang terbaca tetap data default di excel
      
      # resultTemplate()
      # dataTemplate()
      
      #setelah dataTemplate(data default) aktif, 
      # lalu read kembali file rds yang tersimpan dr hasil edit jika ada yang diedit
      # datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      # fileName <- paste0(datapath,"saveData","_",input$th,"_",input$sut,"_",input$kom,"_",input$selected_wilayah,".rds")
      
      
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
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
      
      

      
      #### price ####
      price.in <-  dataDefine$priceInput
      price.in <- cbind(grup="input",price.in)
      price.out <-  dataDefine$priceOutput
      price.out <- cbind(grup="output",price.out)
      price.in[is.na(price.in)] <- 0
      price.out[is.na(price.out)] <- 0
      price.all <- rbind(price.in, price.out)
      
      
      if (dataDefine$tipeKebun[1] == "LARGE SCALE"){
        yearIO <- ncol(io.in)-5 #banyaknya tahun pada tabel io , ada kolom bagiannya jadi pengurangannya 5
        p.price<-select(price.all, -harga.sosial) #remove harga sosial
        p.year<-data.frame(replicate(yearIO,p.price$harga.privat)) #replicate nilai private price sebanyak n tahun
        colnames(p.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
        # p.price<-cbind(status="harga.privat" ,p.price[c("grup", "bagian","komponen", "jenis", "unit.harga")],p.year)
        p.price<-cbind(status="harga.privat" ,p.price[c("grup", "komponen", "jenis", "unit.harga")],p.year)
        p.price <- p.price %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        s.price<-select(price.all, -harga.privat) #remove harga privat
        s.year<-data.frame(replicate(yearIO,s.price$harga.sosial))
        colnames(s.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
        # s.price<-cbind(status="harga.sosial",s.price[c("grup", "bagian", "komponen", "jenis", "unit.harga")],s.year)
        s.price<-cbind(status="harga.sosial",s.price[c("grup", "komponen", "jenis", "unit.harga")],s.year)
        s.price <- s.price %>% mutate_if(is.factor,as.character) #change factor var to char
        
        price.all.year <- rbind(p.price, s.price)
        
        data.gab <- bind_rows(io.all,
                              price.all.year) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
        
        # hitung npv --------------------------------------------------------------
        dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
        dataPrivat <- filter(data.gab,status == c("harga.privat")) #filter data private price
        p.budget <- dataGeneral[-(c(1:6,ncol(dataGeneral)))] * dataPrivat[-c(1:6,ncol(dataPrivat))] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
        p.budget <- cbind(dataGeneral[c("status","grup","bagian","komponen","jenis")],dataPrivat["unit.harga"],p.budget) #memunculkan kembali variabel 1 sd 5
        p.budget <- p.budget %>%mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
        
        #perkalian antara general dengan Social Price
        dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
        s.budget <- dataGeneral[-c(1:6,ncol(dataGeneral))] * dataSosial[-c(1:6,ncol(dataSosial))]
        s.budget <- cbind(dataGeneral[c("status","grup","bagian","komponen","jenis")],dataSosial["unit.harga"],s.budget)
        s.budget <- s.budget %>%
          mutate(status = case_when(status == "general" ~ "social budget"))
        
        ################ penghitungan NPV
        p.rev.output <- p.budget %>%
          filter(str_detect(grup,"output"))
        s.rev.output <- s.budget %>%
          filter(str_detect(grup,"output"))
        
        p.sum.rev <- p.rev.output[,-(1:6)] %>%
          colSums(na.rm = T)
        s.sum.rev <- s.rev.output[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(p.sum.rev)
        sum(s.sum.rev)
        
        p.trad.input <- p.budget %>%
          filter(str_detect(bagian,"input"))
        p.trad.input <- p.trad.input[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(p.trad.input)
        s.trad.input <- s.budget %>%
          filter(str_detect(bagian,"input"))
        s.trad.input <- s.trad.input[,-(1:6)] %>%
          colSums(na.rm = T)
        sum(s.trad.input)
        
        p.trad.capital <- p.budget %>%
          filter(str_detect(bagian,"tradable capital"))
        p.trad.capital <- p.trad.capital[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(p.trad.capital)
        s.trad.capital <- s.budget %>%
          filter(str_detect(bagian,"tradable capital"))
        s.trad.capital <- s.trad.capital[,-(1:6)] %>%
          colSums(na.rm = T)
        sum(s.trad.capital)
        
        p.factor.labor<- p.budget %>%
          filter(str_detect(bagian,"labor"))
        
        p.labor.admin<- p.factor.labor %>%
          filter(str_detect(komponen,"admin"))
        p.labor.admin <- p.labor.admin[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(p.labor.admin)
        
        p.labor.skill <- p.factor.labor %>%
          filter(str_detect(komponen,c("ahli")))
        p.labor.skill <- p.labor.skill[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(p.labor.skill)
        
        p.labor.unskill<- p.factor.labor %>%
          filter(str_detect(komponen,c("unskill")))
        p.labor.unskill <- p.labor.unskill[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(p.labor.unskill)
        
        s.factor.labor<- s.budget %>%
          filter(str_detect(bagian,"labor"))
        
        s.labor.admin<- s.factor.labor %>%
          filter(str_detect(komponen,"admin"))
        s.labor.admin <- s.labor.admin[,-(1:6)] %>%
          colSums(na.rm = T)
        sum(s.labor.admin)
        
        s.labor.skill <- s.factor.labor %>%
          filter(str_detect(komponen,c("ahli")))
        s.labor.skill <- s.labor.skill[,-(1:6)] %>%
          colSums(na.rm = T)
        sum(s.labor.skill)
        
        s.labor.unskill<- s.factor.labor %>%
          filter(str_detect(komponen,c("unskill")))
        s.labor.unskill <- s.labor.unskill[,-(1:6)] %>%
          colSums(na.rm = T)
        sum(s.labor.unskill)
        
        
        
        p.factor.capital<- p.budget %>%
          filter(str_detect(bagian,"factor capital"))
        p.factor.capital <- p.factor.capital[,-(1:6)] %>%
          colSums(na.rm = T)
        # sum(p.factor.capital)
        s.factor.capital<- s.budget %>%
          filter(str_detect(bagian,"factor capital"))
        s.factor.capital <- s.factor.capital[,-(1:6)] %>%
          colSums(na.rm = T)
        sum(s.factor.capital)
        
        p.total.cost <- p.trad.input + p.trad.capital + p.labor.admin + p.labor.skill + p.labor.unskill + p.factor.capital
        s.total.cost <- s.trad.input + s.trad.capital + s.labor.admin + s.labor.skill + s.labor.unskill + s.factor.capital
        
        
        # PERHITUNGAN MANUAL
        # p.sum.rev<-c(value0,p.sum.rev)
        # p.trad.input<-c(value0,p.trad.input)
        # p.trad.capital<-c(value0,p.trad.capital)
        # p.labor.admin<-c(value0,p.labor.admin)
        # p.labor.skill<-c(value0,p.labor.skill)
        # p.labor.unskill<-c(value0,p.labor.unskill)
        # p.factor.capital<-c(value0,p.factor.capital)
        # 
        # s.sum.rev<-c(value0,s.sum.rev)
        # s.trad.input<-c(value0,s.trad.input)
        # s.trad.capital<-c(value0,s.trad.capital)
        # s.labor.admin<-c(value0,s.labor.admin)
        # s.labor.skill<-c(value0,s.labor.skill)
        # s.labor.unskill<-c(value0,s.labor.unskill)
        # s.factor.capital<-c(value0,s.factor.capital)
        # 
        # npv.p.sum.rev<-npv(dataDefine$rate.p/100,p.sum.rev)
        # npv.p.trad.input<-npv(dataDefine$rate.p/100,p.trad.input)
        # npv.p.trad.capital<-npv(dataDefine$rate.p/100,p.trad.capital)
        # npv.p.labor.admin<-npv(dataDefine$rate.p/100,p.labor.admin)
        # npv.p.labor.skill<-npv(dataDefine$rate.p/100,p.labor.skill)
        # npv.p.labor.unskill<-npv(dataDefine$rate.p/100,p.labor.unskill)
        # npv.p.factor.capital<-npv(dataDefine$rate.p/100,p.factor.capital)
        # npv.p.total.cost <- npv.p.trad.input +npv.p.trad.capital + npv.p.labor.admin + npv.p.labor.skill + npv.p.labor.unskill + npv.p.factor.capital
        # npv.p <- npv.p.sum.rev - npv.p.total.cost
        # 
        # npv.s.sum.rev<-npv(dataDefine$rate.s/100,s.sum.rev)
        # npv.s.trad.input<-npv(dataDefine$rate.s/100,s.trad.input)
        # npv.s.trad.capital<-npv(dataDefine$rate.s/100,s.trad.capital)
        # npv.s.labor.admin<-npv(dataDefine$rate.s/100,s.labor.admin)
        # npv.s.labor.skill<-npv(dataDefine$rate.s/100,s.labor.skill)
        # npv.s.labor.unskill<-npv(dataDefine$rate.s/100,s.labor.unskill)
        # npv.s.factor.capital<-npv(dataDefine$rate.s/100,s.factor.capital)
        # npv.s.total.cost <- npv.s.trad.input +npv.s.trad.capital + npv.s.labor.admin + npv.s.labor.skill + npv.s.labor.unskill + npv.s.factor.capital
        # npv.s <- npv.s.sum.rev - npv.s.total.cost
        
        
        p.profit <- p.sum.rev - p.total.cost
        s.profit <- s.sum.rev - s.total.cost
        sum(p.profit)
        sum(s.profit)
        
        value0 <- 0
        p.profit <- c(value0,p.profit)
        s.profit <- c(value0,s.profit)
        npv.p<-npv(dataDefine$rate.p/100,p.profit)
        npv.s<-npv(dataDefine$rate.s/100,s.profit)
        
        npv.p.ha <- npv.p/dataDefine$totalArea
        npv.s.ha <- npv.s/dataDefine$totalArea
        
        hsl.npv<-data.frame(PRIVATE=npv.p.ha,SOCIAL=npv.s.ha)
        
        npv.p.ha.us<-npv.p.ha/dataDefine$nilai.tukar
        npv.s.ha.us<-npv.s.ha/dataDefine$nilai.tukar
        npv.us<-data.frame(PRIVATE=npv.p.ha.us,SOCIAL=npv.s.ha.us)
        hsl.npv<-rbind(hsl.npv,npv.us)
        
        #browser()
        
        rownames(hsl.npv)<-c("NPV (Rp/Ha)", "NPV (US/Ha)")
        hsl.npv
        # ending  npv --------------------------------------------------------------
        
        # hitung Discounted estab cost --------------------------------------------------------------
        
        ################ penghitungan dec
        p.profit.ha <- p.profit/dataDefine$totalArea
        p.positif.cashflow <- ifelse(p.profit.ha > 0, 1, 0)
        
        p.total.cost.0 <- c(value0,p.total.cost)
        p.est.cost <- ifelse(p.positif.cashflow == 1, 0, p.total.cost.0)
        
        # p.est.cost <- length(p.positif.cashflow)
        # for(i in seq_along(p.positif.cashflow)){
        #   if(p.positif.cashflow[i] == 1){
        #     p.est.cost[i] <- 0
        #   }else{
        #     p.est.cost[i] <- p.total.cost.0[i]
        #   }
        # }
        
        s.profit.ha <- s.profit/dataDefine$totalArea
        s.positif.cashflow <- ifelse(s.profit.ha > 0, 1, 0)
        
        s.total.cost.0 <- c(value0,s.total.cost)
        s.est.cost <- ifelse(s.positif.cashflow == 1, 0, s.total.cost.0)
        
        # s.est.cost <- length(s.positif.cashflow)
        # for(i in seq_along(s.positif.cashflow)){
        #   if(s.positif.cashflow[i] == 1){
        #     s.est.cost[i] <- 0
        #   }else{
        #     s.est.cost[i] <- s.total.cost.0[i]
        #   }
        # }
        
        npv.p.dec<-npv(dataDefine$rate.p/100,p.est.cost)
        npv.s.dec<-npv(dataDefine$rate.s/100,s.est.cost)
        
        dec.p <- npv.p.dec/1000/dataDefine$totalArea
        dec.s <- npv.s.dec/1000/dataDefine$totalArea
        
        
        dec<-data.frame(PRIVATE=dec.p,SOCIAL=dec.s)
        rownames(dec)<-c("Discounted Est. Cost (MRp/Ha)")
        dec
        # ending  Discounted estab cost ------------------------------------------------------- 
        
        # hitung Year to positive cashflow --------------------------------------------------------------
        ############# PERHITUNGAN ypc
        ypc.p <- yearIO + 1 - sum(p.positif.cashflow)
        ypc.s <- yearIO + 1 - sum(s.positif.cashflow)
        
        ypc<-data.frame(PRIVATE=ypc.p,SOCIAL=ypc.s)
        rownames(ypc)<-c("Year to positive cashflow")
        ypc
        # ending  Year to positive cashflow ------------------------------------------------------- 
        
        # hitung Labor for establishment --------------------------------------------------------------
        # tabel labor/pekerja
        table.labor<- io.in %>%
          filter(str_detect(bagian,"labor"))
        sum.table.labor <- table.labor[,paste0(c(rep("Y", yearIO)),1:yearIO)] %>%
          colSums(na.rm = T)
        
        yearLabor <- ifelse(length(input$tahunPekerja ) == 0,25,numeric(input$tahunPekerja))
        sum.table.labor.25 <- sum.table.labor[1:yearLabor]
        total.labor <- sum(sum.table.labor.25)
        
        #tabel land/lahan
        # baseScene <- filter(readDataTemplate,str_detect(bagian,"dasar"))
        # landScene <- baseScene[,c("bagian","komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
        # transpose.landScene <- data.frame(t(landScene[,c(paste0(c(rep("Y", yearIO)),1:yearIO))]))
        # colnames(transpose.landScene) <- c("per.year")
        # cum.landScene <- within(transpose.landScene,cummulative.land <- cumsum(per.year)) #buat kolom cumulativ land
        cum.landScene <- as.matrix(dataDefine$cum.landScene)
        
        
        ############# PERHITUNGAN lfe
        p.total.labor.est <- ifelse(p.profit.ha[1:yearLabor+1] < 0,sum.table.labor.25 , 0)
        p.total.labor.opr <- ifelse(p.profit.ha[1:yearLabor+1] < 0,0,sum.table.labor.25)
        p.avg.labor.est <- ifelse(p.total.labor.est > 0,ifelse(p.total.labor.est == 0,0,p.total.labor.est/cum.landScene[2,1:yearLabor]) , p.total.labor.est/cum.landScene[1:yearLabor])
        p.avg.labor.opr <- ifelse(p.total.labor.opr > 0,ifelse(p.total.labor.opr == 0,0,p.total.labor.opr/cum.landScene[2,1:yearLabor]) , p.total.labor.opr/cum.landScene[1:yearLabor])
        p.avg.labor.est[is.na(p.avg.labor.est)] <- 0
        p.avg.labor.opr[is.na(p.avg.labor.opr)] <- 0
        
        s.total.labor.est <- ifelse(s.profit.ha[1:yearLabor+1] < 0,sum.table.labor.25 , 0)
        s.total.labor.opr <- ifelse(s.profit.ha[1:yearLabor+1] < 0,0,sum.table.labor.25)
        s.avg.labor.est <- ifelse(s.total.labor.est > 0,ifelse(s.total.labor.est == 0,0,s.total.labor.est/cum.landScene[2,1:yearLabor]) , s.total.labor.est/cum.landScene[1:yearLabor])
        s.avg.labor.opr <- ifelse(s.total.labor.opr > 0,ifelse(s.total.labor.opr == 0,0,s.total.labor.opr/cum.landScene[2,1:yearLabor]) , s.total.labor.opr/cum.landScene[1:yearLabor])
        s.avg.labor.est[is.na(s.avg.labor.est)] <- 0
        s.avg.labor.opr[is.na(s.avg.labor.opr)] <- 0
        
        lfe.p <- sum(p.avg.labor.est)
        lfe.s <- sum(s.avg.labor.est)
        
        lfe<-data.frame(PRIVATE=lfe.p,SOCIAL=lfe.s)
        rownames(lfe)<-c("Labor for Est. (HOK/Ha)")
        lfe
        # ending  Labor for establishment ------------------------------------------------------- 
        
        ############# PERHITUNGAN lfo
        lfo.p <- mean(p.avg.labor.opr[p.avg.labor.opr!=0])
        lfo.s <- mean(s.avg.labor.opr[s.avg.labor.opr!=0])
        
        lfo<-data.frame(PRIVATE=lfo.p,SOCIAL=lfo.s)
        rownames(lfo)<-c("Labor for Operation. (HOK/Ha/th)")
        lfo
        # ending  Labor for Operation -------------------------------------------------------
        
        nlc.p <- (p.tot.cost - p.sum.labor)/1000000
        nlc.s <- (s.tot.cost - s.sum.labor)/1000000
        nlc<-data.frame(PRIVATE=nlc.p,SOCIAL=nlc.s)
        rownames(nlc)<-c("Non Labor Cost  for 1 cycle (Juta Rp/Ha)")
        nlc
        
        lcost <- data.frame(p.sum.labor/1000000)
        colnames(lcost)<-c("Labor Cost for 1 cycle (MRp/ha)")
        rownames(lcost) <- c("Value")
        lcost
        
        nlcost <- data.frame(nlc.p)
        colnames(nlcost)<-c("Non Labor Cost for 1 cycle (MRp/ha)")
        rownames(nlcost) <- c("Value")
        nlcost
        # ending  nlc ------------------------------------------------------- 
        
        # hitung EC --------------------------------------------------------------
        ############# PERHITUNGAN ESTABLISHMENT COST
        p.ec <- p.sum.cost[[1]]/1000000
        s.ec <- s.sum.cost[[1]]/1000000
        ec <- data.frame(p.ec,s.ec)
        ec<-data.frame(PRIVATE=p.ec,SOCIAL=s.ec)
        rownames(ec)<-c("Establishment cost (1st year only, MRp/ha)")
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
        
        # hitung profit --------------------------------------------------------------
        p.profit.ha <- p.profit[-1]/readDataTemplate$total.area[1]
        s.profit.ha <- s.profit[-1]/readDataTemplate$total.area[1]
        # ending  profit ------------------------------------------------------
        
        
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
        
        pvCost <- presentValue(costYear,c(1:length(costYear)),rate.p/100)
        
        revenueYear <- p.budget %>%
          filter(str_detect(grup,"output"))%>%
          select(contains('Y'))%>%
          colSums(na.rm = T)
        
        pvRevenue <- presentValue(revenueYear,c(1:length(revenueYear)),rate.p/100)
        
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
        showRateP <- (dataDefine$rate.p)
        showRateS <- (dataDefine$rate.s)
        showExRate <- (dataDefine$nilai.tukar)
        showBauMacro <- rbind(showRateP,showRateS,showExRate)
        rownames(showBauMacro)<-c("Discount Rate Private", "Discount Rate Social", "Nilai Tukar Rupiah")
        colnames(showBauMacro) <- c("Nilai")
        showBauMacro
        
        # ending  -------------------------------------------------------
        
        # RESULT 
        dataDefine$npv <- hsl.npv
        dataDefine$nlc <- nlc
        dataDefine$ec <- ec
        dataDefine$hp <- hp
        dataDefine$lr <- lr
        
        dataDefine$rtl <- rtl
        dataDefine$bcr <- bcr
        dataDefine$pi <- pi
        dataDefine$lcost <- lcost
        dataDefine$nlcost <- nlcost
        saveRDS(dataDefine,file = fileName)
        
        # Total area
        totArea <- data.frame(dataDefine$totalArea)
        colnames(totArea)<-c("Total Area (Ha)")
        rownames(totArea) <- c("Value")
        totArea
        

        
        tabel1 <- rbind(hsl.npv,nlc,ec)
        tabel1[] <- lapply(tabel1, function(i) sprintf('%.6g', i))
        tabel1
        
        tabel2 <- rbind(hp,lr,pi,bcr,rtl, lcost,nlcost, showBauMacro,totArea)
        tabel2[] <- lapply(tabel2, function(i) sprintf('%.6g', i))
        tabel2
        
        # tabel1 <- rbind(hsl.npv,ypc,lfe,lfo)
        # tabel1[] <- lapply(tabel1, function(i) sprintf('%.6g', i))
        # tabel1
        # 
        # tabel2 <- data.frame(t(hp))
        # tabel2[] <- lapply(tabel2, function(i) sprintf('%.6g', i))
        # tabel2
        
        # tabel profit 
        tabel.p.profit <- t(as.data.frame(t(p.profit.ha)))
        tabel.s.profit <- t(as.data.frame(t(s.profit.ha)))
        tabel.profit <- cbind(tabel.p.profit,tabel.s.profit)
        rownames(tabel.profit) <- paste0(c(rep("Y", yearIO)),0:yearIO)
        colnames(tabel.profit) <- c("p.profit","s.profit")
        
        tabelGab <- list(tabel1=tabel1,tabel2=tabel2, tabel.profit = tabel.profit)
        tabelGab 
      }else{
        yearIO <- ncol(io.in)-4 #banyaknya tahun pada tabel io 
        p.price<-select(price.all, -harga.sosial) #remove harga sosial
        p.year<-data.frame(replicate(yearIO,p.price$harga.privat)) #replicate nilai private price sebanyak n tahun
        colnames(p.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
        p.price<-cbind(status="harga.privat" ,p.price[c(1:4)],p.year)
        p.price <- p.price %>% mutate_if(is.factor,as.character) #change factor var to char var
        
        s.price<-select(price.all, -harga.privat) #remove harga privat
        s.year<-data.frame(replicate(yearIO,s.price$harga.sosial))
        colnames(s.year)<-paste0(c(rep("Y", yearIO)),1:yearIO)
        s.price<-cbind(status="harga.sosial",s.price[c(1:4)],s.year)
        s.price <- s.price %>% mutate_if(is.factor,as.character) #change factor var to char
        
        price.all.year <- rbind(p.price, s.price)
        
        
        if (is.null(dataDefine$capital)){
          # capital = NULL
          data.gab <- bind_rows(io.all,
                                price.all.year) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
          
          # hitung npv --------------------------------------------------------------
          dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
          dataPrivat <- filter(data.gab,status == c("harga.privat")) #filter data private price
          p.budget <- dataGeneral[-(c(1:5,ncol(dataGeneral)))] * dataPrivat[-c(1:5,ncol(dataPrivat))] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
          p.budget <- cbind(dataGeneral[c(1:4)],dataPrivat["unit.harga"],p.budget) #memunculkan kembali variabel 1 sd 5
          p.budget <- p.budget %>%
            mutate(status = case_when(status == "general" ~ "privat budget")) #mengubah status yg General mjd Private Budget (hasil perkalian io dengan harga privat lalu di tambah modal kapital)
          
          #perkalian antara general dengan Social Price
          dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
          s.budget <- dataGeneral[-c(1:5,ncol(dataGeneral))] * dataSosial[-c(1:5,ncol(dataSosial))]
          s.budget <- cbind(dataGeneral[c(1:4)],dataSosial["unit.harga"],s.budget)
          s.budget <- s.budget %>%
            mutate(status = case_when(status == "general" ~ "social budget"))
          
          ################ penghitungan NPV
          p.cost.input <- p.budget %>%
            filter(str_detect(grup,"input"))
          
          s.cost.input <- s.budget %>%
            filter(str_detect(grup,"input"))
          
          p.sum.cost<- p.cost.input[,-(1:5)] %>%
            colSums(na.rm = T)
          s.sum.cost<- s.cost.input[,-(1:5)] %>%
            colSums(na.rm = T)
          
          p.rev.output <- p.budget %>%
            filter(str_detect(grup,"output"))
          s.rev.output <- s.budget %>%
            filter(str_detect(grup,"output"))
          
          p.sum.rev <- p.rev.output[,-(1:5)] %>%
            colSums(na.rm = T)
          s.sum.rev <- s.rev.output[,-(1:5)] %>%
            colSums(na.rm = T)
          
          
          p.profit <- p.sum.rev - p.sum.cost
          s.profit <- s.sum.rev - s.sum.cost
          profit0 <- 0
          p.profit<-c(profit0,p.profit)
          s.profit<-c(profit0,s.profit)
          
          npv.p<-npv(dataDefine$rate.p/100,p.profit)
          npv.s<-npv(dataDefine$rate.s/100,s.profit)
          
          hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
          
          npv.p.us<-npv.p/dataDefine$nilai.tukar
          npv.s.us<-npv.s/dataDefine$nilai.tukar
          npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
          hsl.npv<-rbind(hsl.npv,npv.us)
          
          #browser()
          
          rownames(hsl.npv)<-c("NPV (Rp/Ha)", "NPV (US/Ha)")
          hsl.npv
          # ending  npv --------------------------------------------------------------
          
        }else if (!is.null(dataDefine$capital)){
          capital <- cbind(grup="input",dataDefine$capital)
          
          # menambahkan pada tabel io matrix bernilai 1
          ioKapital <- data.frame(matrix(data=1,nrow = nrow(capital) , ncol = ncol(dataDefine$ioInput)-3))
          colnames(ioKapital)<-paste0(c(rep("Y", yearIO)),1:yearIO)
          ioKapital<-cbind(status="modal kapital" ,capital[c(1:4)],ioKapital)
          ioKapital <- ioKapital %>% mutate_if(is.factor,as.character) #change factor var to char var
          
          
          kapitalPrivat <- filter(capital,komponen == c("modal kapital privat"))
          kapitalPrivat <- cbind(status ="harga.privat",kapitalPrivat )
          kapitalPrivat <- kapitalPrivat %>% mutate_if(is.factor,as.character) #change factor var to char var
          
          kapitalSosial <- filter(capital,komponen == c("modal kapital sosial"))
          kapitalSosial <- cbind(status ="harga.sosial",kapitalSosial )
          kapitalSosial <- kapitalSosial %>% mutate_if(is.factor,as.character) #change factor var to char var
          
          data.gab <- bind_rows(io.all, ioKapital,
                                price.all.year, 
                                kapitalPrivat, kapitalSosial) ### nanti dibuat if else utk capital jika modal kapital jadi diinputkan
          # hitung npv --------------------------------------------------------------
          dataGeneral <- filter(data.gab,status == c("general")) #filter data input output (yg sudah diberi status=general)
          dataCapitalAll <- filter(data.gab,status == c("modal kapital"))
          
          
          dataGeneralPrivat <- filter(dataCapitalAll,komponen == c("modal kapital privat"))
          dataGeneralPrivat <- rbind(dataGeneral,dataGeneralPrivat)
          dataPrivat <- filter(data.gab,status == c("harga.privat"))
          p.budget <- dataGeneralPrivat[-(c(1:5,ncol(dataGeneralPrivat)))] * dataPrivat[-c(1:5,ncol(dataPrivat))] #perkalian antara unit pada tabel io dg price tanpa variabel 1 sd 5, kolom terakhir adalah kolom unit harga
          p.budget <- cbind(dataGeneralPrivat[c(1:4)],dataPrivat["unit.harga"],p.budget) #memunculkan kembali variabel 1 sd 5
          p.budget <- p.budget[-1] #menghilangkan label status yang awal
          p.budget <- cbind(status = "privat budget", p.budget) #merename keseluruhan tabel status
          
          
          
          
          #perkalian antara general dengan Social Price
          dataGeneralSosial <- filter(dataCapitalAll,komponen == c("modal kapital sosial"))
          dataGeneralSosial <- rbind(dataGeneral,dataGeneralSosial)
          dataSosial <- filter(data.gab, status == c("harga.sosial")) #filter data social price
          s.budget <- dataGeneralSosial[-c(1:5,ncol(dataGeneralSosial))] * dataSosial[-c(1:5,ncol(dataSosial))]
          s.budget <- cbind(dataGeneralSosial[c(1:4)],dataSosial["unit.harga"],s.budget)
          s.budget <- s.budget[-1] #menghilangkan label status yang awal
          s.budget <- cbind(status = "sosial budget", s.budget) #merename keseluruhan tabel status
          
          ################ penghitungan NPV
          p.cost.input <- p.budget %>%
            filter(str_detect(grup,"input"))
          
          s.cost.input <- s.budget %>%
            filter(str_detect(grup,"input"))
          
          p.sum.cost<- p.cost.input[,-(1:5)] %>%
            colSums(na.rm = T)
          s.sum.cost<- s.cost.input[,-(1:5)] %>%
            colSums(na.rm = T)
          
          p.rev.output <- p.budget %>%
            filter(str_detect(grup,"output"))
          s.rev.output <- s.budget %>%
            filter(str_detect(grup,"output"))
          
          p.sum.rev <- p.rev.output[,-(1:5)] %>%
            colSums(na.rm = T)
          s.sum.rev <- s.rev.output[,-(1:5)] %>%
            colSums(na.rm = T)
          
          
          p.profit <- p.sum.rev - p.sum.cost
          s.profit <- s.sum.rev - s.sum.cost
          profit0 <- 0
          p.profit<-c(profit0,p.profit)
          s.profit<-c(profit0,s.profit)
          
          npv.p<-npv(dataDefine$rate.p/100,p.profit)
          npv.s<-npv(dataDefine$rate.s/100,s.profit)
          
          hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
          
          npv.p.us<-npv.p/dataDefine$nilai.tukar
          npv.s.us<-npv.s/dataDefine$nilai.tukar
          npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
          hsl.npv<-rbind(hsl.npv,npv.us)
          
          #browser()
          
          rownames(hsl.npv)<-c("NPV (Rp/Ha)", "NPV (US/Ha)")
          hsl.npv
          # ending  npv --------------------------------------------------------------
          
        }
        
        
        # hitung nlc --------------------------------------------------------------
        
        ################ penghitungan NLC
        
        p.tot.cost<- sum(p.sum.cost)
        s.tot.cost<- sum(s.sum.cost)
        
        p.labor.input <- p.budget %>% filter(str_detect(komponen,c("tenaga kerja|tk|kerja|tenaga")))
        s.labor.input <- s.budget %>% filter(str_detect(komponen,c("tenaga kerja|tk|kerja|tenaga")))
        
        p.sum.labor <- p.labor.input[,-(1:5)] %>%
          sum(na.rm = T)
        s.sum.labor <- s.labor.input[,-(1:5)] %>%
          sum(na.rm = T)
        
        
        
        nlc.p <- (p.tot.cost - p.sum.labor)/1000000
        nlc.s <- (s.tot.cost - s.sum.labor)/1000000
        nlc<-data.frame(PRIVATE=nlc.p,SOCIAL=nlc.s)
        rownames(nlc)<-c("Non Labor Cost  for 1 cycle (Juta Rp/Ha)")
        nlc
        lcost <- data.frame(p.sum.labor/1000000)
        colnames(lcost)<-c("Labor Cost for 1 cycle (MRp/ha)")
        rownames(lcost) <- c("Value")
        lcost
        
        nlcost <- data.frame(nlc.p)
        colnames(nlcost)<-c("Non Labor Cost for 1 cycle (MRp/ha)")
        rownames(nlcost) <- c("Value")
        nlcost
        # ending  nlc ------------------------------------------------------- 
        
        # hitung EC --------------------------------------------------------------
        ############# PERHITUNGAN ESTABLISHMENT COST
        p.ec <- p.sum.cost[[1]]/1000000
        s.ec <- s.sum.cost[[1]]/1000000
        ec <- data.frame(p.ec,s.ec)
        ec<-data.frame(PRIVATE=p.ec,SOCIAL=s.ec)
        rownames(ec)<-c("Establishment cost (1st year only, MRp/ha)")
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
        
        # hitung profit --------------------------------------------------------------
        p.profit.ha <- p.profit[-1]/readDataTemplate$total.area[1]
        s.profit.ha <- s.profit[-1]/readDataTemplate$total.area[1]
        # ending  profit ------------------------------------------------------
        
        
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
        
        pvCost <- presentValue(costYear,c(1:length(costYear)),rate.p/100)
        
        revenueYear <- p.budget %>%
          filter(str_detect(grup,"output"))%>%
          select(contains('Y'))%>%
          colSums(na.rm = T)
        
        pvRevenue <- presentValue(revenueYear,c(1:length(revenueYear)),rate.p/100)
        
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
        showRateP <- (dataDefine$rate.p)
        showRateS <- (dataDefine$rate.s)
        showExRate <- (dataDefine$nilai.tukar)
        showBauMacro <- rbind(showRateP,showRateS,showExRate)
        rownames(showBauMacro)<-c("Discount Rate Private", "Discount Rate Social", "Nilai Tukar Rupiah")
        colnames(showBauMacro) <- c("Nilai")
        showBauMacro
        
        # ending  -------------------------------------------------------
        
        # RESULT 
        dataDefine$npv <- hsl.npv
        dataDefine$nlc <- nlc
        dataDefine$ec <- ec
        dataDefine$hp <- hp
        dataDefine$lr <- lr
        
        dataDefine$rtl <- rtl
        dataDefine$bcr <- bcr
        dataDefine$pi <- pi
        dataDefine$lcost <- lcost
        dataDefine$nlcost <- nlcost
        saveRDS(dataDefine,file = fileName)
        
        
        tabel1 <- rbind(hsl.npv,nlc,ec)
        tabel1[] <- lapply(tabel1, function(i) sprintf('%.6g', i))
        tabel1
        
        tabel2 <- rbind(hp,lr,pi,bcr,rtl, lcost,nlcost, showBauMacro)
        tabel2[] <- lapply(tabel2, function(i) sprintf('%.6g', i))
        tabel2
        
        # tabel profit 
        tabel.p.profit <- t(as.data.frame(t(p.profit)))
        tabel.s.profit <- t(as.data.frame(t(s.profit)))
        tabel.profit <- cbind(tabel.p.profit,tabel.s.profit)
        rownames(tabel.profit) <- paste0(c(rep("Y", yearIO)),0:yearIO)
        colnames(tabel.profit) <- c("p.profit","s.profit")
        
        tabelGab <- list(tabel1=tabel1,tabel2=tabel2, tabel.profit = tabel.profit)
        tabelGab
      }
      
      
    })
    
    output$tableResultBAU1 <- renderDataTable({
      datatable(data.graph()$tabel1, option=list(dom = "t")) 
      # %>%
      # formatStyle("PRIVATE",
      #   backgroundColor = "blue")
    })
    
    output$tableResultBAU2 <- renderDataTable({
      datatable(data.graph()$tabel2, option=list(dom = "t"))
      
    })
    
    output$tableResultSimulasi1 <- renderDataTable({
      datatable(data.graph.new()$tabel1, option=list(dom = "t"))
    })
    
    output$tableResultSimulasi2 <- renderDataTable({
      datatable(data.graph.new()$tabel2, option=list(dom = "t"))
    })
    

    
    preparePlot <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{

      
      
      # print("persiapan membuat plot komoditas simulasi")
      # datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      # fileName <- paste0(datapath,"resultTemplate","_",
      #                    input$sut,"_",input$kom,"_",
      #                    input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      # dataDefine <- readRDS(fileName)


      # dataPlotBAU <- data.frame(tipe.data=dataDefine$tipeData,
      #                           komoditas=dataDefine$kom,
      #                           NPV.Privat.RP=dataDefine$npv[1,1])
      # 
      # fileName <- paste0(datapath,"saveData","_",
      #                    input$sut,"_",input$kom,"_",
      #                    input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      # dataDef <- readRDS(fileName)
      # 
      # dataPlotSimulasi <- data.frame(tipe.data=dataDef$tipeData,
      #                                komoditas=dataDef$kom,
      #                                NPV.Privat.RP=dataDef$npv[1,1])
      # 
      # 
      # dataPlot <- rbind(dataPlotBAU,dataPlotSimulasi)
      

      # contoh 
      datapath <- boxData$data[,1]
      nbaris <- nrow(datapath)
      vectorAlamat <- NULL
      for (k in 1:nbaris) {
        ekstractDataTable <- melt.data.table((datapath[k,]), measure.vars = 1)
        vectorAlamat <- c(vectorAlamat,ekstractDataTable[,value])
      }
      
      listAll <- lapply(vectorAlamat, readRDS)
      
      dat_list <- list()
      for (j in 1:nrow(boxData$data)) {
        dataDefine <- listAll[[j]]
        
        dataPlotBAU <- data.frame(wilayah=dataDefine$wilayah,
                                  komoditas=dataDefine$kom,
                                  NPV.Privat.RP=dataDefine$npv[1,1])
        
        dat_list[[j]] <- dataPlotBAU
        
        print("cek grafik")
        dat_list
      }
      # yes <- lapply(1:length(listAll), function(i){tfc(dat_list[[i]])})
      # yes
      # yes <- lapply(1:length(listAll), function(i){
        # dat_list[[i]]%>%
        #   group_by(wilayah) %>%
        #   plot_ly(x = ~komoditas, y = ~NPV.Privat.RP, type = "bar", color = ~komoditas)
        # })
      # yes
    })
    
    
    # End - Section Popup Modal Dialog ---------------------------------------------
    
    # Start - Section plot tipe kebun---------------------------------------------
    ################################################################################
    #                                                                              #
    #                                 PLOT TIPE KEBUN                          #
    #                                                                              #
    ################################################################################
    
    # output$plotComparingAllProvinsi <- renderPlotly({
    #   plotAllKomoditas()
    # })
    # 
    # plotAllKomoditas <- reactive({
    #   
    #   print("panggil data plot all")
    #   dataPlotAllProvinsi() %>%
    #     group_by(tipe.kebun) %>%
    #     plot_ly(x = ~nama.komoditas, y = ~NPV.Privat.RP, type = "bar", color = ~tipe.kebun)
    # })
    
    plotAllKomoditas <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
    # plotAllKomoditas <- reactive({
      print("persiapan membuat plot seluruh komoditas")
      
      folderSut <- sort(unique(komoditas$sut))
      kabTerpilih <- sort(unique(dat_df$wilayah))
      dat_prov <- NULL

      
      for (i in 1:length(kabTerpilih)) {
        filterKab <- filter(komoditas, wilayah == kabTerpilih[i])
        dat_prov <- rbind(dat_prov,filterKab)
      }
      
      
      
      folderProvinsi <- filter(komoditas, wilayah == input$selected_wilayah)
      folderKom <- sort(unique(folderProvinsi$nama_komoditas))
      
      kombinasiFolder <- as.vector(outer(folderSut, folderKom, paste, sep="/"))
      dirFile <- paste0("data/",kombinasiFolder)
      
      
      
      nameFiles <- list.files(path = paste0(dirFile,"/"),pattern = paste0("resultTemplate"))
      kombinasiFile <- as.vector(outer(dirFile, nameFiles, paste, sep="/"))
      cekFile <- file.exists(kombinasiFile) #cek keberadaan file ini ada atau engga
      
      # remove index yang cekFilenya == F, munculin yang cekFilenya == T aja
      indexFileTrue <- which(cekFile == T)
      kombinasiFile <- kombinasiFile[which(cekFile == T)]
      
      funcFile <- function(x){
        a <- readRDS(x)
        b <- c(x,a)
        b}
      
      
      
      
      # DATA PLOT BAU -----------------------------------------------------------
      folderSut <- sort(unique(komoditas$sut))
      folderProvinsi <- filter(komoditas, wilayah == input$selected_wilayah)
      folderKom <- sort(unique(folderProvinsi$nama_komoditas))

      kombinasiFolder <- as.vector(outer(folderSut, folderKom, paste, sep="/"))
      dirFile <- paste0("data/",kombinasiFolder)

      
      nameFiles <- list.files(path = paste0(dirFile,"/"),pattern = paste0("resultTemplate"))
      kombinasiFile <- as.vector(outer(dirFile, nameFiles, paste, sep="/"))
      cekFile <- file.exists(kombinasiFile) #cek keberadaan file ini ada atau engga
      
      # remove index yang cekFilenya == F, munculin yang cekFilenya == T aja
      indexFileTrue <- which(cekFile == T)
      kombinasiFile <- kombinasiFile[which(cekFile == T)]
      
      funcFile <- function(x){
        a <- readRDS(x)
        b <- c(x,a)
        b}
      
      
      ##### step 2 filter yang ada pattern input$selected_wilayah ex: (_ACEH)
      # cek dari vector kombinasiFile yang sudah di cek T or F nya
      provFile <- kombinasiFile %>% 
        str_subset(pattern = paste0("_",input$selected_wilayah))
      
      
      ##### step 3 filter yang ada pattern input$th ex: (_2020)
      tahunFile <- provFile %>% 
        str_subset(pattern = paste0("_",input$th))
      # tahunFile
      
      patternAll <- lapply(tahunFile, funcFile)
      dataCheck <- patternAll
      
      sut <- unlist(lapply(dataCheck, function(x)x[["sut"]]))
      nama.komoditas <- unlist(lapply(dataCheck, function(x)x[["kom"]]))
      tipe.kebun <- unlist(lapply(dataCheck, function(x)x[["tipeKebun"]][1]))
      tipe.data <- unlist(lapply(dataCheck, function(x)x[["tipeData"]]))
      NPV.Privat.RP <- unlist(lapply(dataCheck, function(x)x[["npv"]][1,1]))
      
      dataPlotBAU <- data.frame(sut=sut,
                                nama.komoditas=nama.komoditas,
                                tipe.kebun = tipe.kebun,
                                tipe.data = tipe.data,
                                NPV.Privat.RP=NPV.Privat.RP)
      # dataPlotBAU.sort <- dataPlotBAU[order(dataPlotBAU$tipe.kebun),]
      # rownames(dataPlotBAU.sort)<-1:nrow(dataPlotBAU.sort)
      
      # DATA PLOT SIMULASI -----------------------------------------------------------
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataCheck <- readRDS(fileName)
      
      sut <- unlist(dataCheck[["sut"]])
      nama.komoditas <- unlist(dataCheck[["kom"]])
      tipe.kebun <- unlist(dataCheck[["tipeKebun"]][1])
      tipe.data <- unlist(dataCheck[["tipeData"]])
      NPV.Privat.RP <- unlist(dataCheck[["npv"]][1,1])
      
      nama.komoditas <- paste0(nama.komoditas," (",tipe.data,")")
      tipe.kebun <- paste0(tipe.kebun," (",tipe.data,")")
      
      dataPlotSimulasi <- data.frame(sut=sut,
                                nama.komoditas = nama.komoditas,
                                tipe.kebun = tipe.kebun,
                                tipe.data = tipe.data,
                                NPV.Privat.RP=NPV.Privat.RP)
      
      
      dataPlot <- rbind(dataPlotBAU,dataPlotSimulasi)
      
      dataPlot$nama.komoditas <- factor(dataPlot$nama.komoditas, levels = unique(dataPlot$nama.komoditas)[order(dataPlot$tipe.kebun, decreasing = FALSE)])
      dataPlot
      
      
      
      print("panggil data plot all")
      dataPlot %>%
          group_by(tipe.kebun) %>%
          plot_ly(x = ~nama.komoditas, y = ~NPV.Privat.RP, type = "bar", color = ~tipe.kebun) 
    })
    
    tableAllProvinsi <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
      
      # DATA PLOT BAU -----------------------------------------------------------
      folderSut <- sort(unique(komoditas$sut))
      folderProvinsi <- filter(komoditas, wilayah == input$selected_wilayah)
      folderKom <- sort(unique(folderProvinsi$nama_komoditas))
      
      kombinasiFolder <- as.vector(outer(folderSut, folderKom, paste, sep="/"))
      dirFile <- paste0("data/",kombinasiFolder)
      
      nameFiles <- list.files(path = paste0(dirFile,"/"),pattern = paste0("resultTemplate"))
      kombinasiFile <- as.vector(outer(dirFile, nameFiles, paste, sep="/"))
      cekFile <- file.exists(kombinasiFile) #cek keberadaan file ini ada atau engga
      
      # remove index yang cekFilenya == F, munculin yang cekFilenya == T aja
      indexFileTrue <- which(cekFile == T)
      kombinasiFile <- kombinasiFile[which(cekFile == T)]
      
      funcFile <- function(x){
        a <- readRDS(x)
        b <- c(x,a)
        b}
      
      
      ##### step 2 filter yang ada pattern input$selected_wilayah ex: (_ACEH)
      # cek dari vector kombinasiFile yang sudah di cek T or F nya
      provFile <- kombinasiFile %>% 
        str_subset(pattern = paste0("_",input$selected_wilayah))
      
      
      ##### step 3 filter yang ada pattern input$th ex: (_2020)
      tahunFile <- provFile %>% 
        str_subset(pattern = paste0("_",input$th))
      # tahunFile
      
      patternAll <- lapply(tahunFile, funcFile)
      dataCheck <- patternAll
      
      sut <- unlist(lapply(dataCheck, function(x)x[["sut"]]))
      nama.komoditas <- unlist(lapply(dataCheck, function(x)x[["kom"]]))
      tipe.kebun <- unlist(lapply(dataCheck, function(x)x[["tipeKebun"]][1]))
      tipe.data <- unlist(lapply(dataCheck, function(x)x[["tipeData"]]))
      NPV.Privat.RP <- unlist(lapply(dataCheck, function(x)x[["npv"]][1,1]))
      
      dataPlotBAU <- data.frame(sut=sut,
                                nama.komoditas=nama.komoditas,
                                tipe.kebun = tipe.kebun,
                                tipe.data = tipe.data,
                                NPV.Privat.RP=NPV.Privat.RP)
      # dataPlotBAU.sort <- dataPlotBAU[order(dataPlotBAU$tipe.kebun),]
      # rownames(dataPlotBAU.sort)<-1:nrow(dataPlotBAU.sort)
      
      # DATA PLOT SIMULASI -----------------------------------------------------------
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataCheck <- readRDS(fileName)
      
      sut <- unlist(dataCheck[["sut"]])
      nama.komoditas <- unlist(dataCheck[["kom"]])
      tipe.kebun <- unlist(dataCheck[["tipeKebun"]][1])
      tipe.data <- unlist(dataCheck[["tipeData"]])
      NPV.Privat.RP <- unlist(dataCheck[["npv"]][1,1])
      
      nama.komoditas <- paste0(nama.komoditas," (",tipe.data,")")
      tipe.kebun <- paste0(tipe.kebun," (",tipe.data,")")
      
      dataPlotSimulasi <- data.frame(sut=sut,
                                     nama.komoditas = nama.komoditas,
                                     tipe.kebun = tipe.kebun,
                                     tipe.data = tipe.data,
                                     NPV.Privat.RP=NPV.Privat.RP)
      
      
      dataPlot <- rbind(dataPlotBAU,dataPlotSimulasi)
      
      dataPlot$nama.komoditas <- factor(dataPlot$nama.komoditas, levels = unique(dataPlot$nama.komoditas)[order(dataPlot$tipe.kebun, decreasing = FALSE)])
      dataPlot
      
    })
    
    output$showTableAllProvinsi <- renderUI({
      datatable(tableAllProvinsi())
    })
    
    output$showPlotProfitPrivat <- renderPlotly({
      profitPlotPrivat()
    })
    
    output$showPlotProfitSosial <- renderPlotly({
      profitPlotSosial()
    })
    
    profitPlotPrivat <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
      bau.profit <- data.graph()$tabel.profit
      simulasi.profit <- data.graph.new()$tabel.profit
      
      profit.tahunan <- bau.profit[,1]
      trace_s_p <- simulasi.profit[,1]
      
      profit.bau <- cbind.data.frame(yaxis = rep("BAU",length(profit.tahunan)),profit.tahunan)
      profit.bau <- cbind.data.frame(no=seq(1,nrow(profit.bau), 1), profit.bau)
      profit.simulasi <- cbind.data.frame(yaxis = rep("SIMULASI",length(trace_s_p)),profit.tahunan = trace_s_p)
      profit.simulasi <- cbind.data.frame(no=seq(1,nrow(profit.simulasi), 1), profit.simulasi)
      profit.gab <- rbind(profit.bau,profit.simulasi)
      
      SIMULASI <- list(
        tickfont = list(color = "green"),
        overlaying = "y",
        side = "right",
        title = "Toyota",
        showgrid = FALSE
      )
      
      
      profit.gab %>%
        group_by(yaxis) %>%
        plot_ly(x=~no, y=~profit.tahunan, type='scatter', color=~yaxis, mode="lines+markers", yaxis=~yaxis) %>%
        layout(xaxis=list(title="Tahun", domain = list(0.15, 0.95)), yaxis=list(title="Profit Tahunan Privat"), yaxis2=SIMULASI)%>%
        layout(legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.95))             # put legend in center of x-axis
      
      # panjangbaris <- length(trace_s_p)-1
      # 
      # x <- c(0:panjangbaris)
      # data <- data.frame(x,profit.tahunan,trace_s_p)
      # 
      # 
      # fig <- plot_ly(data, x = ~x)
      # fig <- fig %>% add_trace(y = ~profit.tahunan, name = 'profit bau privat',mode = 'lines')
      # fig <- fig %>% add_trace(y = ~trace_s_p, name = 'profit simulasi privat', mode = 'lines+markers')
      # fig
    })
    
    profitPlotSosial <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
      bau.profit <- data.graph()$tabel.profit
      simulasi.profit <- data.graph.new()$tabel.profit
      
      profit.tahunan <- bau.profit[,2]
      trace_s_s <- simulasi.profit[,2]
      
      
      profit.bau <- cbind.data.frame(yaxis = rep("BAU",length(profit.tahunan)),profit.tahunan)
      profit.bau <- cbind.data.frame(no=seq(1,nrow(profit.bau), 1), profit.bau)
      profit.simulasi <- cbind.data.frame(yaxis = rep("SIMULASI",length(trace_s_s)),profit.tahunan = trace_s_s)
      profit.simulasi <- cbind.data.frame(no=seq(1,nrow(profit.simulasi), 1), profit.simulasi)
      profit.gab <- rbind(profit.bau,profit.simulasi)
      
      SIMULASI <- list(
        tickfont = list(color = "green"),
        overlaying = "y",
        side = "right",
        title = "Toyota",
        showgrid = FALSE
      )
      
      
      profit.gab %>%
        group_by(yaxis) %>%
        plot_ly(x=~no, y=~profit.tahunan, type='scatter', color=~yaxis, mode="lines+markers", yaxis=~yaxis) %>%
        layout(xaxis=list(title="Tahun", domain = list(0.15, 0.95)), yaxis=list(title="Profit Tahunan Sosial"), yaxis2=SIMULASI)%>%
        layout(legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.95))             # put legend in center of x-axis
      
      # panjangbaris <- length(profit.tahunan)-1
      # 
      # x <- c(0:panjangbaris)
      # data <- data.frame(x,profit.tahunan,trace_s_s)
      # 
      # 
      # fig <- plot_ly(data, x = ~x)
      # fig <- fig %>% add_trace(y = ~profit.tahunan, name = 'profit bau sosial',mode = 'lines')
      # fig <- fig %>% add_trace(y = ~trace_s_s, name = 'profit simulasi sosial', mode = 'lines+markers')
      # fig
    })
    
    
    output$showPlotKumProfitPrivat <- renderPlotly({
      profitPlotKumulatifPrivat()
    })
    
    output$showPlotKumProfitSosial <- renderPlotly({
      profitPlotKumulatifSosial()
    })
    
    profitPlotKumulatifPrivat <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
      bau.profit <- data.graph()$tabel.profit
      simulasi.profit <- data.graph.new()$tabel.profit
      
      profit.kumulatif <- cumsum(bau.profit[,1])
      trace_s_p <- cumsum(simulasi.profit[,1])
      
      profit.bau <- cbind.data.frame(yaxis = rep("BAU",length(profit.kumulatif)),profit.kumulatif)
      profit.bau <- cbind.data.frame(no=seq(1,nrow(profit.bau), 1), profit.bau)
      profit.simulasi <- cbind.data.frame(yaxis = rep("SIMULASI",length(trace_s_p)),profit.kumulatif = trace_s_p)
      profit.simulasi <- cbind.data.frame(no=seq(1,nrow(profit.simulasi), 1), profit.simulasi)
      profit.gab <- rbind(profit.bau,profit.simulasi)
      
      SIMULASI <- list(
        tickfont = list(color = "green"),
        overlaying = "y",
        side = "right",
        title = "Toyota",
        showgrid = FALSE
      )
      
      
      profit.gab %>%
        group_by(yaxis) %>%
        plot_ly(x=~no, y=~profit.kumulatif, type='scatter', color=~yaxis, mode="lines+markers", yaxis=~yaxis) %>%
        layout(xaxis=list(title="Tahun", domain = list(0.15, 0.95)), yaxis=list(title="Profit Kumulatif Privat"), yaxis2=SIMULASI)%>%
        layout(legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.95))             # put legend in center of x-axis
    })
    
    profitPlotKumulatifSosial <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
      bau.profit <- data.graph()$tabel.profit
      simulasi.profit <- data.graph.new()$tabel.profit
      
      profit.kumulatif <- cumsum(bau.profit[,2])
      trace_s_s <- cumsum(simulasi.profit[,2])
      
      profit.bau <- cbind.data.frame(yaxis = rep("BAU",length(profit.kumulatif)),profit.kumulatif)
      profit.bau <- cbind.data.frame(no=seq(1,nrow(profit.bau), 1), profit.bau)
      profit.simulasi <- cbind.data.frame(yaxis = rep("SIMULASI",length(trace_s_s)),profit.kumulatif = trace_s_s)
      profit.simulasi <- cbind.data.frame(no=seq(1,nrow(profit.simulasi), 1), profit.simulasi)
      profit.gab <- rbind(profit.bau,profit.simulasi)
      
      SIMULASI <- list(
        tickfont = list(color = "green"),
        overlaying = "y",
        side = "right",
        title = "Toyota",
        showgrid = FALSE
      )
      
      
      profit.gab %>%
        group_by(yaxis) %>%
        plot_ly(x=~no, y=~profit.kumulatif, type='scatter', color=~yaxis, mode="lines+markers", yaxis=~yaxis) %>%
        layout(xaxis=list(title="Tahun", domain = list(0.15, 0.95)), yaxis=list(title="Profit Kumulatif Sosial"), yaxis2=SIMULASI)%>%
        layout(legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.95))             # put legend in center of x-axis
      
      # panjangbaris <- length(profit.kumulatif)-1
      # 
      # x <- c(0:panjangbaris)
      # data <- data.frame(x,profit.kumulatif,trace_s_s)
      # 
      # 
      # fig <- plot_ly(data, x = ~x)
      # fig <- fig %>% add_trace(y = ~profit.kumulatif, name = 'profit bau sosial',mode = 'lines')
      # fig <- fig %>% add_trace(y = ~trace_s_s, name = 'profit simulasi sosial', mode = 'lines+markers')
      # fig
    })
    # End - Section plot tipe kebun ---------------------------------------------
    
    
    
    observeEvent(input$saveNewPAM, {
      browser()
      datapath <- paste0("data/", input$sut, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      #replace informasi umum -- untuk lbh yakin yang tersave adalah pilihan terakhir user
      dataDefine$sut <- input$sut
      dataDefine$kom <- input$kom
      dataDefine$wilayah <- input$selected_wilayah
      dataDefine$th <- input$th
      dataDefine$tipeLahan <- input$tipeLahan
      # dataDefine$tipeKebun <- readDataTemplate$tipe.kebun
      # dataDefine$tipeData <- c("BAU")
      
      #replace asumsi macro-- untuk lbh yakin yang tersave adalah pilihan terakhir user
      dataDefine$rate.p <- input$rate.p
      dataDefine$rate.s <- input$rate.s
      dataDefine$nilai.tukar <- input$nilai.tukar
      # dataDefine$tipeData <- c("SIMULASI")
      
      saveRDS(dataDefine,file = fileName)
      
    }) 
    
    
    # Start - Section sunting_button---------------------------------------------
    ################################################################################
    #                                                                              #
    #                                 BUTTON SUNTING DARI AWAL                     #
    #                                                                              #
    ################################################################################
    observeEvent(input$sunting_button,{
      showModal(
        modalTahunDaurTanam()
      )
    })
    
    modalTahunDaurTanam <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          actionButton(("sunting_button_1"), "Simpan dan Lanjutkan",style="color: white;background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting1",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Pilih Tahun Daur Tanam",
            active = T,
            selectInput(("ioYear_input"),
                        "pilih tahun skenario:",
                        choices = c(30:60),selected = 30, width = "600px"),# pilihannnya masih 30 tahun sesuai default
            tags$div(id = 'uiTahunDaurTanam')
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$ioYear_input,{
      if (input$ioYear_input > 30){
        insertUI(selector='#uiTahunDaurTanam',
                 where='afterEnd',
                 ui= uiOutput('tahunDaurTanam'))
      } else if(input$ioYear_input == 30){
        removeUI(selector = '#tahunDaurTanam')
      } else if(input$ioYear_input == 30 & input$ioTipeDaurTanam == "tabel berisi data template diambil dari tahun ke-1" ){
        removeUI(selector = '#tahunDaurTanam')
      } else if(input$ioYear_input == 30 & input$ioTipeDaurTanam == "tabel berisi nilai 0" ){
        removeUI(selector = '#tahunDaurTanam')
      }
    })
    
    output$tahunDaurTanam <- renderUI({
      radioButtons("ioTipeDaurTanam",
                   " ",
                   width = "600px",
                   choices = c("tabel berisi data template diambil dari tahun ke-1",
                               "tabel berisi nilai 0"),
                   selected = "tabel berisi data template diambil dari tahun ke-1")
    })
    
    
    observeEvent(input$sunting_button_1,{
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      # replace data price
      # dataDefine$ioYear_input <- input$ioYear_input
      saveRDS(dataDefine,file = fileName)
      
      if(dataDefine$tipeKebun == "LARGE SCALE"){
        showModal(modalScenLand())
      }else{
        showModal(modalPilihBarisOutput())
      }
      
    })
    
    
    modalScenLand <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          actionButton(("scenLand_button"), "Lanjut",style="color: white;background-color: green;")
        ),
        argonTabSet(
          id = "tabTahunScenLand",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Skenario Lahan",
            active = T,
            h3("Apakah user akan menyunting skenario lahan?"),
            radioButtons(("scenLandKomponen"),
                         " ",
                         choices = c("Tidak","Ya"),selected = "Tidak"),
            
            tags$div(id = 'uiScenLand')
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$scenLandKomponen,{
      if(input$scenLandKomponen == "Tidak"){
        removeUI(selector = '#scenLand')
      }else if (input$scenLandKomponen == "Ya"){
        insertUI(selector='#uiScenLand',
                 where='afterEnd',
                 ui= uiOutput('scenLand'))
      }  
    })
    

    
    output$scenLand <- renderUI({
      
      fluidPage(
        tags$br(),
        h3("Tabel Skenario Lahan", align = "center"),
        rHandsontableOutput('editScenLand'),
        tags$br(),
        tags$br(),
        tags$div(id='teksTotalArea'),
        tags$br(),
        tags$br(),
        actionButton(('saveScenLand'), 'simpan tabel'),
        tags$br(),
        tags$div(id='teksSaveScenLand')
      )
      
     
    })
    
    
    
    output$editScenLand <- renderRHandsontable({
      rhandsontable(valScenLand(),
                    rowHeaderWidth = 50,
                    fixedColumnsLeft = 1,
                    height = 100
      )%>%
        hot_col(1, readOnly = TRUE)
    })
    
    valScenLand <- eventReactive(input$scenLandKomponen,{
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      # reactData$tableScenLand <- dataDefine$cum.landScene[1,]
      # idRow <- c("per Tahun")
      # reactData$tableScenLand <- cbind(idRow,reactData$tableScenLand)
      # colnames(reactData$tableScenLand)[1] <- c(" ")
      # rownames(reactData$tableScenLand) <- c(1)
      # reactData$tableScenLand
      
      
      
      if(input$ioYear_input == 30){
        reactData$tableScenLand <- dataDefine$cum.landScene[1,]
        idRow <- c("per Tahun")
        reactData$tableScenLand <- cbind(idRow,reactData$tableScenLand)
        colnames(reactData$tableScenLand)[1] <- c(" ")
        rownames(reactData$tableScenLand) <- c(1)
        reactData$tableScenLand
      } else if(input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi nilai 0" ){
        
        reactData$tableScenLand <- dataDefine$cum.landScene[1,]
        yearIOadd <- as.numeric(input$ioYear_input)
        addCol <- data.frame(matrix(0, nrow = nrow(dataDefine$cum.landScene[1,]), ncol = yearIOadd - 30))
        colnames(addCol)<-paste0(c(rep("Y", yearIOadd)),1:yearIOadd)[31:yearIOadd] # start dari thun 31 utk nama kolom baru
        
        idRow <- c("per Tahun")
        
        reactData$tableScenLand <- cbind(idRow,dataDefine$cum.landScene[1,],addCol)
        colnames(reactData$tableScenLand)[1] <- c(" ")
        rownames(reactData$tableScenLand) <- c(1)
        reactData$tableScenLand
        
      } else if(input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi data template diambil dari tahun ke-1" ){
        reactData$tableScenLand <- dataDefine$cum.landScene[1,]
        yearIOaddMin30 <- as.numeric(input$ioYear_input) - 30
        yearIO <- as.numeric(input$ioYear_input) - yearIOaddMin30
        filterIO <-  dataDefine$cum.landScene[1,][,c(paste0(c(rep("Y", yearIO)),1:yearIO))]
        addCol <- filterIO[,1:yearIOaddMin30] 
        
        yearIOadd <- as.numeric(input$ioYear_input)
        colnames(addCol)<-paste0(c(rep("Y", ncol(addCol))),31:yearIOadd)
        
        idRow <- c("per Tahun")
        
        reactData$tableScenLand <- cbind(idRow,dataDefine$cum.landScene[1,],addCol)
        colnames(reactData$tableScenLand)[1] <- c(" ")
        rownames(reactData$tableScenLand) <- c(1)
        reactData$tableScenLand
      }
    })
    
    
    
    observeEvent(input$saveScenLand,{
      removeUI(selector='#textTampilScenLand')
      removeUI(selector='#textTampilTotalArea')
      # browser()
      
      editNew<-as.data.frame(hot_to_r(input$editScenLand))
      editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
      
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      # replace data price
      landScene <- editNew[,-1]
      yearIO <- 30
      transpose.landScene <- data.frame(t(landScene[,c(paste0(c(rep("Y", yearIO)),1:yearIO))]))
      colnames(transpose.landScene) <- c("per.year")
      cum.landScene <- within(transpose.landScene,cummulative.land <- cumsum(per.year)) #buat kolom cumulativ land
      cum.landScene <- data.frame(t(cum.landScene))
      
      dataDefine$cum.landScene <- cum.landScene
      
      #total area setelah sunting
      totalAreaNew <- sum(landScene)
      
      dataDefine$totalArea <- totalAreaNew
      saveRDS(dataDefine,file = fileName)
      
      textTotalArea <- paste0("Total Area (dalam Ha) = ", totalAreaNew)
      
      insertUI(selector='#teksSaveScenLand',
               where = 'afterEnd',
               ui = tags$div(id="textTampilScenLand","tabel di atas sudah tersimpan"))
      
      insertUI(selector='#teksTotalArea',
               where = 'afterEnd',
               ui = tags$div(id="textTampilTotalArea",textTotalArea))
      
      # insertUI(selector='#teksSaveScenLand',
      #          where = 'afterEnd',
      #          ui = uiOutput("textTampilTotalArea"))
      # browser()
    })
    
    
    # output$textTampilTotalArea <- renderUI({
    #   fluidPage(
    #     textOutput("showTotalArea")
    #   )
    # })
    # 
    # output$showTotalArea <- renderText(
    #   
    #   
    # )
    
    observeEvent(input$scenLand_button,{
      showModal(modalPilihBarisOutput())
    }) 
    
    modalPilihBarisOutput <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          actionButton(("sunting_button_2_output"), "Lanjut",style="color: white;background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting2",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Menentukan Komponen (Baris) pada Output Tabel Kuantitas",
            active = T,
            h3("Apakah user akan menambahkan komponen (baris) pada bagian Output Tabel Kuantitas?"),
            radioButtons(("ioKomponen_output"),
                         " ",
                         choices = c("Tidak","Ya"),selected = "Tidak"), 
            # tags$div(id='tambahBaris')
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$sunting_button_2_output,{
      if (input$ioKomponen_output == "Ya"){
        showModal(modalTambahBarisOutput()) 
      }else if(input$ioKomponen_output == "Tidak"){
        showModal(suntingTabelKuantitas_output())
      }
    })
    
    modalTambahBarisOutput <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("sunting_button_3_output"),"Lanjut",style="color: white;
                         background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting3",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Sunting Tabel Kuantitas (Output)",
            active = T,
            fluidRow(
              column(9,
                     selectInput('pilihKomponenOutput',"Pilih Komponen", width = "600px", 
                                 choices = c(" ","utama", "sampingan")),
                     #dibuat manipulasi option nilai kosong krn utk mengaktifkan nilai input$pilihKomponenOutput saat user kedua kalinya masuk 
                     
              ),
              column(3,
                     br(),
                     actionButton("showTabelJenisOutput","pilih jumlah baris")
              ),
              column(12,
                     tags$div(id = 'uiShowTablePilihJenisOutput')
              )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$showTabelJenisOutput,{
      insertUI(selector='#uiShowTablePilihJenisOutput',
               where='afterEnd',
               ui= uiOutput('showTablePilihJenisOutput'))
    }) 
    
    # Start Modal Pilih Komponen Output ----------------------------------------
    ################################################################################
    #                                                                              #
    #                      MODAL DIALOG PILIH KOMPONEN OUTPUT                       #
    #                                                                              #
    ################################################################################
    
    source("server/showTablePilihJenisOutput.R", local = TRUE)
    
    observeEvent(input$sunting_button_3_output,{
      showModal(suntingTabelKuantitas_output())
    })
    
    
    suntingTabelKuantitas_output <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("batalSunting_button_kuantitasOutput"), "Batal", style="color: white;background-color: red;"),
          actionButton("backtoPilihBaris_output","Kembali"),
          actionButton(("sunting_button_4_output"),"Simpan dan Lanjut",style="color: white;
                         background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting4",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Langkah 6: Sunting Tabel Kuantitas (Output)",
            active = T,
            fluidRow(
              column(12,
                     h1("Tabel kuantitas (Output)",align = "center"),
                     rHandsontableOutput('suntingKuantitasOutput')
              )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$batalSunting_button_kuantitasOutput,{
      removeModal()
    })

    observeEvent(input$backtoPilihBaris_output,{
      showModal(modalTambahBarisOutput())
    })
    
    output$suntingKuantitasOutput <- renderRHandsontable({
      rhandsontable(valIO2(),
                    rowHeaderWidth = 50,
                    fixedColumnsLeft = 2,
                    height = 300
      )%>%
        hot_col(c(1:3), readOnly = TRUE)
    })
    
    valIO2 <- eventReactive(c(input$sunting_button_2_output,input$sunting_button_3_output),{
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      if((is.null(dataDefine$addUtama) | is.null(dataDefine$addSampingan)) & input$ioYear_input == 30 & input$ioKomponen_output == "Tidak"){
        reactData$tableIO2 <- dataDefine$ioOutput
        reactData$tableIO2
      } else if  ((!is.null(dataDefine$addUtama) | !is.null(dataDefine$addSampingan) )  & input$ioYear_input == 30 & input$ioKomponen_output == "Tidak"){
        reactData$tableIO2 <- dataDefine$ioOutput
        reactData$tableIO2
      } else if ((!is.null(dataDefine$addUtama) | !is.null(dataDefine$addSampingan) )  & input$ioYear_input == 30 & input$ioKomponen_output == "Ya"){
        reactData$tableIO2 <- bind_rows(dataDefine$ioOutput,dataDefine$addUtama,dataDefine$addSampingan) 
        reactData$tableIO2
      } else if((is.null(dataDefine$addUtama) | is.null(dataDefine$addSampingan) ) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi nilai 0" ){
        reactData$tableIO2 <- dataDefine$ioOutput
        yearIOadd <- as.numeric(input$ioYear_input)
        addCol <- data.frame(matrix(0, nrow = nrow(dataDefine$ioOutput), ncol = yearIOadd - 30))
        colnames(addCol)<-paste0(c(rep("Y", yearIOadd)),1:yearIOadd)[31:yearIOadd] # start dari thun 31 utk nama kolom baru
        addCol <- cbind(dataDefine$ioOutput,addCol)
        reactData$tableIO2 <- bind_rows(addCol,dataDefine$addUtama,dataDefine$addSampingan) 
        reactData$tableIO2
      } else if((is.null(dataDefine$addUtama) | is.null(dataDefine$addSampingan)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi data template diambil dari tahun ke-1" ){
        reactData$tableIO2 <- dataDefine$ioOutput
        yearIOaddMin30 <- as.numeric(input$ioYear_input) - 30
        yearIO <- as.numeric(input$ioYear_input) - yearIOaddMin30
        filterIO <-  dataDefine$ioOutput[,c(paste0(c(rep("Y", yearIO)),1:yearIO))]
        addCol <- filterIO[,1:yearIOaddMin30] 
        
        yearIOadd <- as.numeric(input$ioYear_input)
        colnames(addCol)<-paste0(c(rep("Y", ncol(addCol))),31:yearIOadd)
        addCol <- cbind(dataDefine$ioOutput,addCol)
        reactData$tableIO2 <- bind_rows(addCol,dataDefine$addUtama,dataDefine$addSampingan) 
        reactData$tableIO2
      }else if((!is.null(dataDefine$addUtama) | !is.null(dataDefine$addSampingan)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi nilai 0" ){
        reactData$tableIO2 <- dataDefine$ioOutput
        yearIOadd <- as.numeric(input$ioYear_input)
        addCol <- data.frame(matrix(0, nrow = nrow(dataDefine$ioOutput), ncol = yearIOadd - 30))
        colnames(addCol)<-paste0(c(rep("Y", yearIOadd)),1:yearIOadd)[31:yearIOadd] # start dari thun 31 utk nama kolom baru
        addCol <- cbind(dataDefine$ioOutput,addCol)
        
        reactData$tableIO2 <- bind_rows(addCol,dataDefine$addUtama,dataDefine$addSampingan) 
        reactData$tableIO2
        
      } else if((!is.null(dataDefine$addUtama) | !is.null(dataDefine$addSampingan)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi data template diambil dari tahun ke-1" ){
        reactData$tableIO2 <- dataDefine$ioOutput
        yearIOaddMin30 <- as.numeric(input$ioYear_input) - 30
        yearIO <- as.numeric(input$ioYear_input) - yearIOaddMin30
        filterIO <-  dataDefine$ioOutput[,c(paste0(c(rep("Y", yearIO)),1:yearIO))]
        addCol <- filterIO[,1:yearIOaddMin30] 
        
        yearIOadd <- as.numeric(input$ioYear_input)
        colnames(addCol)<-paste0(c(rep("Y", ncol(addCol))),31:yearIOadd)
        addCol <- cbind(dataDefine$ioOutput,addCol)
        
        reactData$tableIO2 <- bind_rows(addCol,dataDefine$addUtama,dataDefine$addSampingan) 
        reactData$tableIO2
        
      }
      
    })
    
    
    # End -  Modal Pilih Komponen Output ----------------------------------------
    
    observeEvent(input$sunting_button_4_output,{
      # save data untuk setiap perubahan
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      editNew<-as.data.frame(hot_to_r(input$suntingKuantitasOutput))
      editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
      
      dataDefine$ioOutput <- editNew
      
      saveRDS(dataDefine,file = fileName)

      showModal(modalPilihBarisInput())
    })
    
    modalPilihBarisInput <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          actionButton(("sunting_button_2_input"), "Lanjut",style="color: white;background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting2",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Menentukan Komponen (Baris) pada Input Tabel Kuantitas",
            active = T,
            h3("Apakah user akan menambahkan komponen (baris) pada bagian Input Tabel Kuantitas?"),
            radioButtons(("ioKomponen_input"),
                         " ",
                         choices = c("Tidak","Ya"),selected = "Tidak"), 
            # tags$div(id='tambahBaris')
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    
    observeEvent(input$sunting_button_2_input,{
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      
      if (input$ioKomponen_input == "Ya" & dataDefine$tipeKebun == "LARGE SCALE"){
        showModal(modalTambahBarisInputLargeScale())
      }else if(input$ioKomponen_input == "Ya" & dataDefine$tipeKebun != "LARGE SCALE"){
        showModal(modalTambahBarisInput())
      }else if(input$ioKomponen_input == "Tidak"){
        showModal(suntingTabelKuantitas_input())
      }
      
      # if (input$ioKomponen_input == "Ya"){
      #   showModal(modalTambahBarisInput())
      # }else if(input$ioKomponen_input == "Tidak"){
      #   showModal(suntingTabelKuantitas_input())
      # }
    })
    
    
    modalTambahBarisInputLargeScale <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("sunting_button_3_inputLargeScale"),"Lanjut",style="color: white;
                         background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting3",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Sunting Tabel Kuantitas (Input)",
            active = T,
            fluidRow(
              column(9,
                     selectInput('pilihKomponenInput_LargeScale',"Pilih Komponen", width = "600px",
                                 choices = c(" ","pupuk","pestisida","insektisida","herbisida","bibit" ,"peralatan", "bahan kimia","tradable capital","tenaga kerja ahli","tenaga kerja unskilled","factor capital")),
                     #dibuat manipulasi option nilai kosong krn utk mengaktifkan nilai input$pilihKomponenInput_LargeScale saat user kedua kalinya masuk

              ),
              column(3,
                     br(),
                     actionButton("showTabelJenis_LargeScale","pilih jumlah baris")
              ),
              column(12,
                     tags$div(id = 'uiShowTablePilihJenisInput_LargeScale')
              )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }

    observeEvent(input$pilihKomponenInput_LargeScale,{
      removeUI(selector='#showTablePilihJenisInput_LargeScale')
    })

    observeEvent(input$showTabelJenis_LargeScale,{
      insertUI(selector='#uiShowTablePilihJenisInput_LargeScale',
               where='afterEnd',
               ui= uiOutput('showTablePilihJenisInput_LargeScale'))
    })
    
    modalTambahBarisInput <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("sunting_button_3_input"),"Lanjut",style="color: white;
                         background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting3",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Sunting Tabel Kuantitas (Input)",
            active = T,
            fluidRow(
              column(9,
                     selectInput('pilihKomponenInput',"Pilih Komponen", width = "600px", 
                                 choices = c(" ","pupuk","pestisida","insektisida","herbisida", "bibit", "peralatan","tenaga kerja")),
                     #dibuat manipulasi option nilai kosong krn utk mengaktifkan nilai input$pilihKomponenInput saat user kedua kalinya masuk 
                     
              ),
              column(3,
                     br(),
                     actionButton("showTabelJenis","pilih jumlah baris")
              ),
              column(12,
                     tags$div(id = 'uiShowTablePilihJenisInput')
              )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$pilihKomponenInput,{
      removeUI(selector='#showTablePilihJenisInput')
    }) 
    
    observeEvent(input$showTabelJenis,{
      insertUI(selector='#uiShowTablePilihJenisInput',
               where='afterEnd',
               ui= uiOutput('showTablePilihJenisInput'))
    }) 
    
    
    # Start Modal Pilih Komponen Input ----------------------------------------
    ################################################################################
    #                                                                              #
    #                      MODAL DIALOG PILIH KOMPONEN INPUT                       #
    #                                                                              #
    ################################################################################
    source("server/showTablePilihJenisInput_LargeScale.R", local = TRUE)
    source("server/showTablePilihJenisInput.R", local = TRUE)
    
    # End -  Modal Pilih Komponen Input ----------------------------------------
    observeEvent(input$sunting_button_3_inputLargeScale,{
      showModal(suntingTabelKuantitas_input())
    })
    
    observeEvent(input$sunting_button_3_input,{
      showModal(suntingTabelKuantitas_input())
    })
    
    
    suntingTabelKuantitas_input <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("batalSunting_button_kuantitasInput"), "Batal", style="color: white;background-color: red;"),
          actionButton("backtoPilihBaris_input","Kembali"),
          actionButton(("sunting_button_4"),"Simpan dan Lanjut Membangun Tabel Harga",style="color: white;
                         background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting4",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Langkah 7: Sunting Tabel Kuantitas (Input)",
            active = T,
            fluidRow(
              column(12,
                     h1("Tabel kuantitas (Input)",align = "center"),
                     rHandsontableOutput('suntingKuantitasInput')
              )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$batalSunting_button_kuantitasInput,{
     #  browser()
      removeModal()
    })
    
    observeEvent(input$backtoPilihBaris_input,{
      showModal(modalPilihBarisInput())
    })
    
    
    
    output$suntingKuantitasInput <- renderRHandsontable({
      rhandsontable(valIO1(),
                    rowHeaderWidth = 50,
                    fixedColumnsLeft = 2,
                    height = 300,
      )%>%
        hot_col(c(1:3), readOnly = TRUE)
    })
    
    valIO1 <- eventReactive(c(input$sunting_button_2_input,input$sunting_button_3_input),{
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      if((is.null(dataDefine$addPupuk) | is.null(dataDefine$addBibit) | is.null(dataDefine$addPeralatan) |is.null(dataDefine$addTK) 
          |is.null(dataDefine$addTKUnskilled) |is.null(dataDefine$addBahanKimia) |is.null(dataDefine$addTradCapital) | is.null(dataDefine$addFactorCapital)) & input$ioYear_input == 30 & input$ioKomponen_input == "Tidak"){
        reactData$tableIO1 <- dataDefine$ioInput
        reactData$tableIO1
      } else if  ((!is.null(dataDefine$addPupuk) | !is.null(dataDefine$addBibit) | !is.null(dataDefine$addPeralatan) |!is.null(dataDefine$addTK)
                   |!is.null(dataDefine$addTKUnskilled) |!is.null(dataDefine$addBahanKimia) |!is.null(dataDefine$addTradCapital) | !is.null(dataDefine$addFactorCapital))  & input$ioYear_input == 30 & input$ioKomponen_input == "Tidak"){
        reactData$tableIO1 <- dataDefine$ioInput
        reactData$tableIO1
      } else if ((!is.null(dataDefine$addPupuk) | !is.null(dataDefine$addBibit) | !is.null(dataDefine$addPeralatan) |!is.null(dataDefine$addTK)
                  |!is.null(dataDefine$addTKUnskilled) |!is.null(dataDefine$addBahanKimia) |!is.null(dataDefine$addTradCapital) | !is.null(dataDefine$addFactorCapital))  & input$ioYear_input == 30 & input$ioKomponen_input == "Ya"){
        reactData$tableIO1 <- bind_rows(dataDefine$ioInput,dataDefine$addPupuk,dataDefine$addBahanKimia,dataDefine$addBibit,dataDefine$addPeralatan,
                                        dataDefine$addTK,dataDefine$addTKUnskilled,dataDefine$addTradCapital,dataDefine$addFactorCapital) 
        reactData$tableIO1
      } else if((is.null(dataDefine$addPupuk) | is.null(dataDefine$addBibit) | is.null(dataDefine$addPeralatan) |is.null(dataDefine$addTK)
                 |is.null(dataDefine$addTKUnskilled) |is.null(dataDefine$addBahanKimia) |is.null(dataDefine$addTradCapital) | is.null(dataDefine$addFactorCapital)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi nilai 0" ){
        reactData$tableIO1 <- dataDefine$ioInput
        yearIOadd <- as.numeric(input$ioYear_input)
        addCol <- data.frame(matrix(0, nrow = nrow(dataDefine$ioInput), ncol = yearIOadd - 30))
        colnames(addCol)<-paste0(c(rep("Y", yearIOadd)),1:yearIOadd)[31:yearIOadd] # start dari thun 31 utk nama kolom baru
        addCol <- cbind(dataDefine$ioInput,addCol)
        reactData$tableIO1 <- bind_rows(addCol,dataDefine$addPupuk,dataDefine$addBahanKimia,dataDefine$addBibit,dataDefine$addPeralatan,
                                        dataDefine$addTK,dataDefine$addTKUnskilled,dataDefine$addTradCapital,dataDefine$addFactorCapital) 
        reactData$tableIO1
      } else if((is.null(dataDefine$addPupuk) | is.null(dataDefine$addBibit) | is.null(dataDefine$addPeralatan) |is.null(dataDefine$addTK)
                 |is.null(dataDefine$addTKUnskilled) |is.null(dataDefine$addBahanKimia) |is.null(dataDefine$addTradCapital) | is.null(dataDefine$addFactorCapital)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi data template diambil dari tahun ke-1" ){
        reactData$tableIO1 <- dataDefine$ioInput
        yearIOaddMin30 <- as.numeric(input$ioYear_input) - 30
        yearIO <- as.numeric(input$ioYear_input) - yearIOaddMin30
        filterIO <-  dataDefine$ioInput[,c(paste0(c(rep("Y", yearIO)),1:yearIO))]
        addCol <- filterIO[,1:yearIOaddMin30] 
        
        yearIOadd <- as.numeric(input$ioYear_input)
        colnames(addCol)<-paste0(c(rep("Y", ncol(addCol))),31:yearIOadd)
        addCol <- cbind(dataDefine$ioInput,addCol)
        reactData$tableIO1 <- bind_rows(addCol,dataDefine$addPupuk,dataDefine$addBahanKimia,dataDefine$addBibit,dataDefine$addPeralatan,
                                        dataDefine$addTK,dataDefine$addTKUnskilled,dataDefine$addTradCapital,dataDefine$addFactorCapital)
        reactData$tableIO1
      }else if((!is.null(dataDefine$addPupuk) | !is.null(dataDefine$addBibit) | !is.null(dataDefine$addPeralatan) |!is.null(dataDefine$addTK)
                |!is.null(dataDefine$addTKUnskilled) |!is.null(dataDefine$addBahanKimia) |!is.null(dataDefine$addTradCapital) | !is.null(dataDefine$addFactorCapital)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi nilai 0" ){
        reactData$tableIO1 <- dataDefine$ioInput
        yearIOadd <- as.numeric(input$ioYear_input)
        addCol <- data.frame(matrix(0, nrow = nrow(dataDefine$ioInput), ncol = yearIOadd - 30))
        colnames(addCol)<-paste0(c(rep("Y", yearIOadd)),1:yearIOadd)[31:yearIOadd] # start dari thun 31 utk nama kolom baru
        addCol <- cbind(dataDefine$ioInput,addCol)
        reactData$tableIO1 <- bind_rows(addCol,dataDefine$addPupuk,dataDefine$addBahanKimia,dataDefine$addBibit,dataDefine$addPeralatan,
                                        dataDefine$addTK,dataDefine$addTKUnskilled,dataDefine$addTradCapital,dataDefine$addFactorCapital) 
        reactData$tableIO1
        
      } else if((!is.null(dataDefine$addPupuk) | !is.null(dataDefine$addBibit) | !is.null(dataDefine$addPeralatan) |!is.null(dataDefine$addTK)
                 |!is.null(dataDefine$addTKUnskilled) |!is.null(dataDefine$addBahanKimia) |!is.null(dataDefine$addTradCapital) | !is.null(dataDefine$addFactorCapital)) & input$ioYear_input > 30 & input$ioTipeDaurTanam == "tabel berisi data template diambil dari tahun ke-1" ){
        reactData$tableIO1 <- dataDefine$ioInput
        yearIOaddMin30 <- as.numeric(input$ioYear_input) - 30
        yearIO <- as.numeric(input$ioYear_input) - yearIOaddMin30
        filterIO <-  dataDefine$ioInput[,c(paste0(c(rep("Y", yearIO)),1:yearIO))]
        addCol <- filterIO[,1:yearIOaddMin30] 
        
        yearIOadd <- as.numeric(input$ioYear_input)
        colnames(addCol)<-paste0(c(rep("Y", ncol(addCol))),31:yearIOadd)
        addCol <- cbind(dataDefine$ioInput,addCol)
        reactData$tableIO1 <- bind_rows(addCol,dataDefine$addPupuk,dataDefine$addBahanKimia,dataDefine$addBibit,dataDefine$addPeralatan,
                                        dataDefine$addTK,dataDefine$addTKUnskilled,dataDefine$addTradCapital,dataDefine$addFactorCapital) 
        reactData$tableIO1
        
      }
      
    })

    
    observeEvent(input$sunting_button_4,{
      # browser()
      # save data untuk setiap perubahan
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      editNew<-as.data.frame(hot_to_r(input$suntingKuantitasInput))
      editNew[is.na(editNew)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
      
      # if(dataDefine$tipeKebun == "LARGE SCALE"){
      #   editNew[,1] <- as.factor(editNew[,1]) #ubah char jd faktor, spy bs di drop down
      # }
      
      
      
      dataDefine$ioInput <- editNew
      
      saveRDS(dataDefine,file = fileName)
      showModal(modalTabelHarga())
    })
    
    ################################################################################
    #                                                                              #
    #                                 MODAL  HARGA                                 #
    #                                                                              #
    ######################################F##########################################
    modalTabelHarga <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          actionButton(("batalButtonHarga"), "Batal", style="color: white;background-color: red;"),
          actionButton(("capitalButtonNext"), "Simpan Tabel dan Lanjutkan",style="color: white;background-color: green;")
        ),
        argonTabSet(
          id = "tabHarga",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Langkah 8: Menyunting Tabel Harga",
            active = T,
            h3("Tabel Harga Output", align = "center"),
            rHandsontableOutput('hargaOutput'),
            br(),
            h3("Tabel Harga Input", align = "center"),
            rHandsontableOutput('hargaInput')
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$batalButtonHarga,{
      # browser()
      removeModal()
    })
    
    output$hargaOutput <- renderRHandsontable({
      rhandsontable(valP2(),
                    rowHeaderWidth = 50,
                    fixedColumnsLeft = 2,
                    height = 100,
      )%>%
        hot_col(c(1:3), readOnly = TRUE)
    })
    
    
    valP2 <- eventReactive(input$sunting_button_4,{
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      
      readDataTemplate <- read.table(paste0(datapath,input$sut,"_",input$kom,"_",input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".csv"), header = T, sep = ",")
      readDataTemplate[is.na(readDataTemplate)] <- 0
      readDataTemplate <- lowcase(readDataTemplate, c("faktor","komponen","jenis","unit.harga","unit"))
      
      outputData <- filter(readDataTemplate,faktor == c("output"))
      priceOutput <- outputData[,c("jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
      # priceOutput <- (priceOutput[,-1])
      
      
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      
      dataDefine <- readRDS(fileName)
      
      reactData$tableP2 <- dataDefine$ioOutput[,c("komponen","jenis")]
      no.id <- as.numeric(rownames(reactData$tableP2))
      reactData$tableP2 <- cbind(no.id,reactData$tableP2)
      
      reactData$tableP2 <- merge(reactData$tableP2,unique(priceOutput), by.x = "jenis",by.y = "jenis", all.x = T)
      reactData$tableP2 <- reactData$tableP2[order(reactData$tableP2$no.id),]     #sort by nomor yang disesuaikan pada tabel i-o
      rownames(reactData$tableP2) <- no.id
      reactData$tableP2 <- reactData$tableP2[, c("komponen","jenis","unit.harga","harga.privat","harga.sosial")]
      reactData$tableP2
      
      indexRow <- as.numeric(nrow(dataDefine$ioOutput))
      reactData$tableP2 <- reactData$tableP2[1:indexRow,]
      reactData$tableP2
    })
    
    output$hargaInput <- renderRHandsontable({
      rhandsontable(valP1(),
                    rowHeaderWidth = 50,
                    fixedColumnsLeft = 2,
                    height = 600,
      )%>%
        hot_col(c(1:3), readOnly = TRUE)
    })
    
    valP1 <- eventReactive(input$sunting_button_4,{
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      readDataTemplate <- read.table(paste0(datapath,input$sut,"_",input$kom,"_",input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".csv"), header = T, sep = ",")
      readDataTemplate[is.na(readDataTemplate)] <- 0
      readDataTemplate <- lowcase(readDataTemplate, c("faktor","komponen","jenis","unit.harga","unit"))
      
      inputData <- filter(readDataTemplate,faktor == c("input"))
      priceInput <- inputData[,c("jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
      # priceInput <- (priceInput[,-1])
      
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      reactData$tableP1 <- dataDefine$ioInput[,c("komponen","jenis")]
      no.id <- as.numeric(rownames(reactData$tableP1))
      reactData$tableP1 <- cbind(no.id,reactData$tableP1)
      
      reactData$tableP1 <- merge(reactData$tableP1,unique(priceInput), by.x = "jenis",by.y = "jenis", all.x = T)
      reactData$tableP1 <- reactData$tableP1[order(reactData$tableP1$no.id),]     #sort by nomor yang disesuaikan pada tabel i-o
      rownames(reactData$tableP1) <- no.id
      reactData$tableP1 <- reactData$tableP1[, c("komponen","jenis","unit.harga","harga.privat","harga.sosial")]
      reactData$tableP1
      
      
      indexRow <- as.numeric(nrow(dataDefine$ioInput))
      reactData$tableP1 <- reactData$tableP1[1:indexRow,]
      reactData$tableP1
    })
    

    observeEvent(input$capitalButtonNext,{
      # save data untuk setiap perubahan
      datapath <- paste0("data/", input$sut, "/",input$kom, "/")
      fileName <- paste0(datapath,"saveData","_",
                         input$sut,"_",input$kom,"_",
                         input$selected_wilayah,"_",input$th,"_",input$tipeLahan,".rds")
      dataDefine <- readRDS(fileName)
      
      editNewP1<-as.data.frame(hot_to_r(input$hargaInput))
      editNewP1[is.na(editNewP1)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
      
      editNewP2<-as.data.frame(hot_to_r(input$hargaOutput))
      editNewP2[is.na(editNewP2)] <- 0 
      
      dataDefine$priceInput <- editNewP1
      dataDefine$priceOutput <- editNewP2
      saveRDS(dataDefine,file = fileName)
      print("cek tabel modal kapital dan tipe kebun")
      
      if(is.null(dataDefine$capital) & dataDefine$tipeKebun == "LARGE SCALE"){
        
        # editNewP1<-as.data.frame(hot_to_r(input$hargaInput))
        # editNewP1[is.na(editNewP1)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
        # 
        # editNewP2<-as.data.frame(hot_to_r(input$hargaOutput))
        # editNewP2[is.na(editNewP2)] <- 0 
        # 
        # dataDefine$priceInput <- editNewP1
        # dataDefine$priceOutput <- editNewP2
        # saveRDS(dataDefine,file = fileName)
        showModal(modalLargeScale())
      }else if(!is.null(dataDefine$capital) & dataDefine$tipeKebun != "LARGE SCALE"){
        
        # editNewP1<-as.data.frame(hot_to_r(input$hargaInput))
        # editNewP1[is.na(editNewP1)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
        # 
        # editNewP2<-as.data.frame(hot_to_r(input$hargaOutput))
        # editNewP2[is.na(editNewP2)] <- 0 
        # 
        # dataDefine$priceInput <- editNewP1
        # dataDefine$priceOutput <- editNewP2
        # saveRDS(dataDefine,file = fileName)
        showModal(modalTabelCapital())
      }else if(is.null(dataDefine$capital) & dataDefine$tipeKebun != "LARGE SCALE"){
        # editNewP1<-as.data.frame(hot_to_r(input$hargaInput))
        # editNewP1[is.na(editNewP1)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
        # 
        # editNewP2<-as.data.frame(hot_to_r(input$hargaOutput))
        # editNewP2[is.na(editNewP2)] <- 0 
        # 
        # dataDefine$priceInput <- editNewP1
        # dataDefine$priceOutput <- editNewP2
        # saveRDS(dataDefine,file = fileName)
        showModal(modalTanpaCapital())
      }
    })
    
    modalLargeScale <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          actionButton(("running_button_LargeScale"), "Jalankan Analisis",style="color: white;background-color: green;")
        ),
        argonTabSet(
          id = "tabNew",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Jalankan Analisis",
            active = T,
            h3("Silahkan menekan tombol JALANKAN ANALISIS untuk mendapatkan hasil simulasi", align = "center")
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    modalTanpaCapital <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          actionButton(("running_button_tanpaCapital"), "Jalankan Analisis",style="color: white;background-color: green;")
        ),
        argonTabSet(
          id = "tabNew",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Status Tabel Modal Kapital",
            active = T,
            h3("Komoditas ini Tidak Memiliki Tabel Modal Kapital", align = "center")
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    # observeEvent(input$running_button_tanpaCapital,{
    #  browser()
    # })
    
    
    
    modalTabelCapital <- function(failed = FALSE) {
      modalDialog( 
        footer=tagList(
          # actionButton(("apalah_button"), "Jalankan Analisis",style="color: white;background-color: green;")
        ),
        argonTabSet(
          id = "tabNew",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Tabel Modal Kapital",
            active = T,
            h3("Apakah user akan menambah komponen untuk tabel modal kapital?"),
            radioButtons("tipeTabelCapital",
                         " ",
                         choices = c("Tidak","Ya"),selected = "Tidak"),
            tags$div(id='uiTipeModalKapital')
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$tipeTabelCapital,{
      if (input$tipeTabelCapital == "Tidak"){
        removeUI(selector = '#tipeModalKapital')
        insertUI(selector='#uiTipeModalKapital',
                 where='afterEnd',
                 ui= uiOutput('tipeModalKapital_No'))
      } else if(input$tipeTabelCapital == "Ya"){
        removeUI(selector = '#tipeModalKapital_No')
        insertUI(selector='#uiTipeModalKapital',
                 where='afterEnd',
                 ui= uiOutput('tipeModalKapital'))
      } 
    })
    
    output$tipeModalKapital_No <- renderUI({
      fluidRow(
        column(9,
               
        ),
        column(3,
               actionButton(("running_button_noEditCapital"), "Jalankan Analisis",style="color: white;background-color: green;")
        )
      )
      
    })
    
    
    
    output$tipeModalKapital <- renderUI({
      fluidRow(
        column(6,
               
        ),
        column(6,
               actionButton(("EksisTabelCapital"), "Lanjut Membangun Tabel Modal Kapital")
        )
      )
      
    })
    
    
    observeEvent(input$EksisTabelCapital,{
      if (input$tipeTabelCapital == "Ya"){
        showModal(modalPilihBarisCapital())
      } 
    })
    
    
    modalPilihBarisCapital <- function(failed = FALSE) {
      modalDialog(
        footer=tagList(
          actionButton(("suntingModalKapital"),"Lanjut",style="color: white;
                         background-color: green;")
        ),
        argonTabSet(
          id = "tabSunting",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "l",
          width = 12,
          argonTab(
            tabName = "Sunting Tabel Modal Kapital",
            active = T,
            fluidRow(
              column(9,
                     selectInput('pilihKomponenCapital',"Pilih Komponen", width = "600px", 
                                 choices = c(" ","privat", "sosial")),
                     
              ),
              column(3,
                     br(),
                     actionButton("showTabelJenisCapital","pilih jumlah baris")
              ),
              column(12,
                     tags$div(id = 'uiShowTablePilihJenisCapital')
              )
            )
          ))
        ,
        size="l",
        easyClose = FALSE)
    }
    
    observeEvent(input$showTabelJenisCapital,{
      insertUI(selector='#uiShowTablePilihJenisCapital',
               where='afterEnd',
               ui= uiOutput('showTablePilihJenisCapital'))
    }) 
    
    # Start Modal Pilih Komponen Modal Kapital ----------------------------------------
    ################################################################################
    #                                                                              #
    #                      MODAL DIALOG PILIH KOMPONEN MODAL KAPITAL               #
    #                                                                              #
    ################################################################################
    
    source("server/showTablePilihJenisCapital.R", local = TRUE)
    
    
    
    # End -  Modal Pilih Komponen Modal Kapital ----------------------------------------
    
  
    ################################################################################
    #                                                                              #
    #                               PROSES 2 PAM BARU                             #
    #                                                                              #
    ################################################################################
    source("server/pamBaru_server.R", local = TRUE)
    
    ################################################################################
    #                                                                              #
    #                               PROSES 3 PAM BARU                             #
    #                                                                              #
    ################################################################################
    source("server/pamParsial_server.R", local = TRUE)
    
  }
)

# runApp(app)
