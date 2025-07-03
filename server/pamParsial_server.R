 reactData_par <- reactiveValues(
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
dataTemplate_par <- reactive({
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  print(paste0(datapath," SIMULASI"))
  
  readDataTemplate <- read.table(paste0(datapath,input$sut_par,"_",input$kom_par,"_",input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".csv"), header = T, sep = ",")
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
    sut_par <- input$sut_par
    kom_par <- input$kom_par
    wilayah <- input$selected_wilayah_par
    th_par <- input$th_par
    tipeLahan_par <- input$tipeLahan_par
    tipeKebun <- readDataTemplate$tipe.kebun[1]
    totalArea <- readDataTemplate$total.area[1]
    tipeData <- c("SIMULASI")
    
    # asumsi makro
    rate.p_par <- input$rate.p_par
    rate.s_par <- input$rate.s_par
    nilai.tukar_par <- input$nilai.tukar_par
    
    combineDef <- list(ioInput=ioInput,ioOutput=ioOutput,
                       priceInput=priceInput,priceOutput=priceOutput,
                       cum.landScene = cum.landScene,
                       sut=sut_par,
                       kom=kom_par,
                       wilayah = wilayah,
                       th=th_par,
                       tipeLahan = tipeLahan_par,
                       tipeKebun = tipeKebun,
                       totalArea = totalArea,
                       tipeData = tipeData,
                       rate.p = rate.p_par,
                       rate.s = rate.s_par,
                       nilai.tukar = nilai.tukar_par)
    
    
    # save data untuk setiap perubahan
    datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
    fileName <- paste0(datapath,"saveParDat","_",
                       input$sut_par,"_",input$kom_par,"_",
                       input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
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
    sut_par <- input$sut_par
    kom_par <- input$kom_par
    wilayah <- input$selected_wilayah_par
    th_par <- input$th_par
    tipeLahan_par <- input$tipeLahan_par
    tipeKebun <- readDataTemplate$tipe.kebun[1]
    tipeData <- c("SIMULASI")
    
    # asumsi makro
    rate.p_par <- input$rate.p_par
    rate.s_par <- input$rate.s_par
    nilai.tukar_par <- input$nilai.tukar_par
    
    combineDef <- list(ioInput=ioInput,ioOutput=ioOutput,
                       priceInput=priceInput,priceOutput=priceOutput,
                       capital=capital, capitalPrivat = capitalPrivat, capitalSosial = capitalSosial,
                       sut=sut_par,
                       kom=kom_par,
                       wilayah = wilayah,
                       th=th_par,
                       tipeLahan_par = tipeLahan_par,
                       tipeKebun = tipeKebun,
                       tipeData = tipeData,
                       rate.p = rate.p_par,
                       rate.s = rate.s_par,
                       nilai.tukar_par = nilai.tukar_par)
    
    
    # save data untuk setiap perubahan
    datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
    fileName <- paste0(datapath,"saveParDat","_",
                       input$sut_par,"_",input$kom_par,"_",
                       input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
    saveRDS(combineDef,file = fileName)
    
    
    
    print("pertama kali masuk/login. cek save data default")
    combineDef
  }
})

resultParTemp <- reactive({
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  print(paste0(datapath," BAU"))
  
  readDataTemplate <- read.table(paste0(datapath,input$sut_par,"_",input$kom_par,"_",input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".csv"), header = T, sep = ",")
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
    # asumsiMakroTemplate <- filter(readDataAsumsiMakro,tahun == input$th_par)
    
    rate.p_par <- readDataTemplate$rate.p[1]
    rate.s_par <- readDataTemplate$rate.s[1]
    nilai.tukar_par <- readDataTemplate$nilai.tukar[1]
    
    # memmbuat list gabungan dataDefine
    dataDefine <- list(ioInput=ioInput,ioOutput=ioOutput,
                       priceInput=priceInput,priceOutput=priceOutput,
                       cum.landScene = cum.landScene,
                       rate.p_par = rate.p_par,
                       rate.s_par = rate.s_par,
                       nilai.tukar_par=nilai.tukar_par)
    
    #informasi umum
    dataDefine$sut <- input$sut_par
    dataDefine$kom <- input$kom_par
    dataDefine$wilayah <- input$selected_wilayah_par
    dataDefine$th <- input$th_par
    dataDefine$tipeLahan <- input$tipeLahan_par
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
    # npv.p.sum.rev<-npv(dataDefine$rate.p_par/100,p.sum.rev)
    # npv.p.trad.input<-npv(dataDefine$rate.p_par/100,p.trad.input)
    # npv.p.trad.capital<-npv(dataDefine$rate.p_par/100,p.trad.capital)
    # npv.p.labor.admin<-npv(dataDefine$rate.p_par/100,p.labor.admin)
    # npv.p.labor.skill<-npv(dataDefine$rate.p_par/100,p.labor.skill)
    # npv.p.labor.unskill<-npv(dataDefine$rate.p_par/100,p.labor.unskill)
    # npv.p.factor.capital<-npv(dataDefine$rate.p_par/100,p.factor.capital)
    # npv.p.total.cost <- npv.p.trad.input +npv.p.trad.capital + npv.p.labor.admin + npv.p.labor.skill + npv.p.labor.unskill + npv.p.factor.capital
    # npv.p <- npv.p.sum.rev - npv.p.total.cost
    # 
    # npv.s.sum.rev<-npv(dataDefine$rate.s_par/100,s.sum.rev)
    # npv.s.trad.input<-npv(dataDefine$rate.s_par/100,s.trad.input)
    # npv.s.trad.capital<-npv(dataDefine$rate.s_par/100,s.trad.capital)
    # npv.s.labor.admin<-npv(dataDefine$rate.s_par/100,s.labor.admin)
    # npv.s.labor.skill<-npv(dataDefine$rate.s_par/100,s.labor.skill)
    # npv.s.labor.unskill<-npv(dataDefine$rate.s_par/100,s.labor.unskill)
    # npv.s.factor.capital<-npv(dataDefine$rate.s_par/100,s.factor.capital)
    # npv.s.total.cost <- npv.s.trad.input +npv.s.trad.capital + npv.s.labor.admin + npv.s.labor.skill + npv.s.labor.unskill + npv.s.factor.capital
    # npv.s <- npv.s.sum.rev - npv.s.total.cost
    
    
    p.profit <- p.sum.rev - p.total.cost
    s.profit <- s.sum.rev - s.total.cost
    sum(p.profit)
    sum(s.profit)
    
    value0 <- 0
    p.profit <- c(value0,p.profit)
    s.profit <- c(value0,s.profit)
    npv.p<-npv(dataDefine$rate.p_par/100,p.profit)
    npv.s<-npv(dataDefine$rate.s_par/100,s.profit)
    
    npv.p.ha <- npv.p/dataDefine$totalArea
    npv.s.ha <- npv.s/dataDefine$totalArea
    
    hsl.npv<-data.frame(PRIVATE=npv.p.ha,SOCIAL=npv.s.ha)
    
    npv.p.ha.us<-npv.p.ha/dataDefine$nilai.tukar_par
    npv.s.ha.us<-npv.s.ha/dataDefine$nilai.tukar_par
    npv.us<-data.frame(PRIVATE=npv.p.ha.us,SOCIAL=npv.s.ha.us)
    hsl.npv<-rbind(hsl.npv,npv.us)
    
    #browser()
    
    rownames(hsl.npv)<-c("NPV (Rp/Ha)", "NPV (US/Ha)")
    hsl.npv
    # ending  npv --------------------------------------------------------------
    
    # hitung EAE -------------------------------------------------------------- 
    atas <- rate.p_par*((1+rate.p_par/100)^yearIO)
    bawah <- (1+rate.p_par/100)^yearIO-1
    eae <- data.frame(npv.p*atas/bawah)
    colnames(eae)<-c("Equal Annual Equivalent (EAE)")
    rownames(eae) <- c("Value")
    eae
    
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
    
    npv.p.dec<-npv(dataDefine$rate.p_par/100,p.est.cost)
    npv.s.dec<-npv(dataDefine$rate.s_par/100,s.est.cost)
    
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
    rownames(lfo)<-c("Labor for Operation. (HOK/Ha/th_par)")
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
    
    
    ##### save data template
    # RESULT 
    dataDefine$npv <- hsl.npv
    dataDefine$dec <- dec
    dataDefine$ypc <- ypc
    dataDefine$lfe <- lfe
    dataDefine$lfo <- lfo
    dataDefine$hp <- hp
    
    
    print("save result template untuk klik pertama asumsiMakro_button_par")
    
    fileName <- paste0(datapath,"resultParTemp","_",
                       input$sut_par,"_",input$kom_par,"_",
                       input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
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
    # asumsiMakroTemplate <- filter(readDataAsumsiMakro,tahun == input$th_par)
    
    rate.p_par <- readDataTemplate$rate.p[1]
    rate.s_par <- readDataTemplate$rate.s[1]
    nilai.tukar_par <- readDataTemplate$nilai.tukar[1]
    
    # memmbuat list gabungan dataDefine
    dataDefine <- list(ioInput=ioInput,ioOutput=ioOutput,
                       priceInput=priceInput,priceOutput=priceOutput,
                       capital=capital, capitalPrivat = capitalPrivat, capitalSosial = capitalSosial,
                       rate.p_par = rate.p_par,
                       rate.s_par = rate.s_par,
                       nilai.tukar_par=nilai.tukar_par)
    
    #informasi umum
    dataDefine$sut <- input$sut_par
    dataDefine$kom <- input$kom_par
    dataDefine$wilayah <- input$selected_wilayah_par
    dataDefine$th <- input$th_par
    dataDefine$tipeLahan <- input$tipeLahan_par
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
      
      npv.p<-npv(dataDefine$rate.p_par/100,p.profit)
      npv.s<-npv(dataDefine$rate.s_par/100,s.profit)
      
      hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
      
      npv.p.us<-npv.p/as.numeric(dataDefine$nilai.tukar_par)
      npv.s.us<-npv.s/as.numeric(dataDefine$nilai.tukar_par)
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
      
      npv.p<-npv(dataDefine$rate.p_par/100,p.profit)
      npv.s<-npv(dataDefine$rate.s_par/100,s.profit)
      
      hsl.npv<-data.frame(PRIVATE=npv.p,SOCIAL=npv.s)
      
      npv.p.us<-npv.p/dataDefine$nilai.tukar_par
      npv.s.us<-npv.s/dataDefine$nilai.tukar_par
      npv.us<-data.frame(PRIVATE=npv.p.us,SOCIAL=npv.s.us)
      hsl.npv<-rbind(hsl.npv,npv.us)
      
      #browser()
      
      rownames(hsl.npv)<-c("NPV (Rp/Ha)", "NPV (US/Ha)")
      hsl.npv
      # ending  npv --------------------------------------------------------------
    }
    
    
    # hitung EAE -------------------------------------------------------------- 
    atas <- rate.p_par*((1+rate.p_par/100)^yearIO)
    bawah <- (1+rate.p_par/100)^yearIO-1
    eae <- data.frame(npv.p*atas/bawah)
    colnames(eae)<-c("Equal Annual Equivalent (EAE)")
    rownames(eae) <- c("Value")
    eae
    
    
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
    
    pvCost <- presentValue(costYear,c(1:length(costYear)),rate.p_par/100)
    
    revenueYear <- p.budget %>%
      filter(str_detect(grup,"output"))%>%
      select(contains('Y'))%>%
      colSums(na.rm = T)
    
    pvRevenue <- presentValue(revenueYear,c(1:length(revenueYear)),rate.p_par/100)
    
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
    dataDefine$eae <- eae
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
    
    
    print("save result template untuk klik pertama asumsiMakro_button_par")
    
    fileName <- paste0(datapath,"resultParTemp","_",
                       input$sut_par,"_",input$kom_par,"_",
                       input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
    saveRDS(dataDefine,file = fileName)
    
    
    ##### ending save data template
  }
  
  
  
  
})

# end - Section preparation data --------

# Section input informasi umum  dan asumsi makcro---------------------------------------------
observeEvent(c(input$sut_par,input$kom_par,input$selected_prov_par, input$selected_wilayah_par,input$th_par,input$tipeLahan_par), {
  removeUI(selector='#showResult_par')
  removeUI(selector='#showMakro_par')
  removeUI(selector='#showTable_par')
  removeUI(selector='#showButton_par')
  
})

observeEvent(c(input$sut_par,input$kom_par,input$selected_prov_par, input$selected_wilayah_par,input$th_par,input$tipeLahan_par), {
  removeUI(selector='#showResult_par')
  removeUI(selector='#showTable_par')
  removeUI(selector='#showButton_par')
})


# End - Section input informasi umum  dan asumsi makcro---------------------------------------------

# Section asumsi makro ---------------------------------------------
observeEvent(input$asumsiMakro_button_par, {
  # browser()
  dataTemplate_par()
  resultParTemp()
  insertUI(selector='#uiShowMakro_par',
           where='afterEnd',
           ui= uiOutput('showMakro_par'))
  
  
}) 

output$showMakro_par <- renderUI({
  argonRow(
    argonColumn(
      width = 12,
      argonH1("Asumsi Makro", display = 4),
      h5("Langkah 2: menentukan asumsi makro untuk data PAM yang dibangun"),
      br(),
      fluidRow(
        # column(1,
        #        
        # ),
        column(4,
               tableOutput("showMakroBAU_par")
        ),
        column(2,
               sliderInput(("rate.p_par"), "Discount Rate Private", 7.4 ,min = 0, max = 15, step = 0.01)
        ),
        column(2,
               sliderInput(("rate.s_par"), "Discount Rate Social", 2.4 ,min = 0, max = 8, step = 0.01)
        ),
        
        column(2,
               sliderInput(("nilai.tukar_par"), "Nilai Tukar Rupiah", 14831 ,min = 10000, max = 20000, step = 1)
        ),
        column(2,
               br(),
               actionButton(("tampilkanTabel_button_par"),"Tampilkan Tabel PAM",icon("paper-plane"),style="color: white; 
                         background-color: green;")
        )
      )
    )
  )
})

output$showMakroBAU_par <- renderTable({
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath,"resultParTemp","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  readDataTemplate <- readRDS(fileName)
  dataView <- t(data.frame(rate.p_par.bau = readDataTemplate$rate.p, 
                           rate.s_par.bau = readDataTemplate$rate.s,
                           nilai.tukar_par.bau = readDataTemplate$nilai.tukar,
                           tipe.kebun = readDataTemplate$tipeKebun,
                           lokasi=readDataTemplate$lokasi))
  dataView[is.na(dataView)] <- 0 #NA replace with zero
  
  nameRow <- c("Discount Rate Private", "Discount Rate Social", "Nilai Tukar Rupiah", "Tipe Kebun","Lokasi")
  dataView <- cbind(nameRow,data.frame(dataView))
  
  colnames(dataView) <- c(" ","Nilai BAU pada informasi umum yang terpilih")
  dataView
  
})

# End - Section asumsi makro ---------------------------------------------

# Section input informasi umum  dan asumsi makcro---------------------------------------------
observeEvent(c(input$sut_par,input$kom_par,input$selected_wilayah_par,input$th_par,input$tipeLahan_par), {
  removeUI(selector='#showResult_par')
  removeUI(selector='#showMakro_par')
  removeUI(selector='#showTable_par')
  removeUI(selector='#showButton_par')
  
  # dataTemplate()
  # resultTemplate()
})

observeEvent(c(input$rate.p_par,input$rate.s_par,input$nilai.tukar_par), {
  removeUI(selector='#showResult_par')
  removeUI(selector='#showTable_par')
  removeUI(selector='#showButton_par')
  
  
  # dataTemplate()
  # resultTemplate()
})

# Section tampilkan tabel---------------------------------------------
observeEvent(input$tampilkanTabel_button_par, {
  # browser()
  dataTemplate_par()
  resultParTemp()
  insertUI(selector='#uiShowTable_par',
           where='afterEnd',
           ui= uiOutput('showTable_par'))
  
  insertUI(selector='#uiShowButton_par',
           where='afterEnd',
           ui= uiOutput('showButton_par'))
}) 

output$showTable_par <- renderUI({
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  dataDefine <- readRDS(fileName)
  
  argonRow(
    argonColumn(
      width = 12,
      argonH1("Tabel", display = 4),
      h5("Langkah 3: Menampilkan atau menyunting Tabel PAM yang terpilih"),
      
      # jika tdk bisa jadi input buttton maka coba ubah nama action  buttonnya sepertinya conflict dengan script lain
      argonTabSet(
        id = "tab-tabel_par",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Tabel Harga",
          active = T,
          # tableOutput("cekTable"),
          
          dataTableOutput("showTablePrice_par"),
          actionButton("simPrice_button","Sunting Harga",icon("paper-plane"),style="color: white;
                           background-color: green;"),
          style = "height:650px; overflow-y: scroll;overflow-x: scroll;"
        ),
        argonTab(
          tabName = "Tabel Kuantitas",
          active = F,
          dataTableOutput(("showTableKuantitas_par")),
          actionButton("simIO_button","Sunting Kuantitas",icon("paper-plane"),style="color: white;
                           background-color: green;"),
          style = "height:650px; overflow-y: scroll;overflow-x: scroll;"
        ),
        
        if(dataDefine$tipeKebun == "LARGE SCALE"){
          argonTab(
            tabName = "Tabel Skenario Lahan",
            active = F,
            dataTableOutput("showTableScenLand_par")
            ,
            actionButton("simSkenLahan_button","Sunting Skenario Lahan",icon("paper-plane"),style="color: white;
                           background-color: green;"),
            style = "height:650px; overflow-y: scroll;overflow-x: scroll;"
          )
        }else{
          argonTab(
            tabName = "Tabel Modal Kapital",
            active = F,
            dataTableOutput("showTableKapital_par")
            ,
            actionButton("simCapital_button","Sunting Modal Kapital",icon("paper-plane"),style="color: white;
                           background-color: green;"),
            style = "height:650px; overflow-y: scroll;overflow-x: scroll;"
          )}
        
      ),
    )
  )
})


output$showTablePrice_par <- renderDataTable({
  viewSimP()
})


viewSimP <- eventReactive(c(input$tampilkanTabel_button_par,input$runSimPrice),{
  datapath_par <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath_par,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  dataDefine <- readRDS(fileName)
  dataView <- rbind(dataDefine$priceInput, dataDefine$priceOutput)
  dataView[is.na(dataView)] <- 0 #NA replace with_par zero
  
  print("tabel harga yang terupdate")
  
  dataView

})

output$showTableKuantitas_par <- renderDataTable({
  viewSimIO()
  
})

viewSimIO <- eventReactive(c(input$tampilkanTabel_button_par,input$runSimIO),{
  datapath_par <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath_par,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  # print("data terakhir tersimpan di rds")
  dataDefine <- readRDS(fileName)
  dataView <- rbind(dataDefine$ioInput, dataDefine$ioOutput)
  dataView[is.na(dataView)] <- 0 #NA replace with_par zero
  
  print("tabel harga yang terupdate")
  
  dataView
  
  
  
})

output$showTableKapital_par <- renderDataTable({
  # case for modal kapital
  datapath_par <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath_par,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  dataDefine <- readRDS(fileName)
  
  if (!is.null(dataDefine$capital)){
    dataView <- dataDefine$capital
    dataView[is.na(dataView)] <- 0 #NA replace with zero
    dataView    
  }
  else if (is.null(dataDefine$capital)){
    dataView <- data.frame(matrix("tidak terdapat tabel modal kapital",nrow=1,ncol=1))
    colnames(dataView) <- "Keterangan"
    dataView
  }
})

output$showTableScenLand_par <- renderDataTable({
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  dataDefine <- readRDS(fileName)
  
  # baseScene <- filter(dataDefine,str_detect(bagian,"dasar"))
  # landScene <- baseScene[,c("bagian","komponen","jenis","unit",paste0(c(rep("Y", yearIO)),1:yearIO))] #memfilter tabel kuantitas
  # transpose.landScene <- data.frame(t(landScene[,c(paste0(c(rep("Y", yearIO)),1:yearIO))]))
  # colnames(transpose.landScene) <- c("per.year")
  # cum.landScene <- within(transpose.landScene,cummulative.land <- cumsum(per.year)) #buat kolom cumulativ land
  # cum.landScene <- t(cum.landScene)
  
  dataView <- cbind("Total Area (Ha)" = dataDefine$totalArea, dataDefine$cum.landScene)
  dataView[is.na(dataView)] <- 0 #NA replace with zero
  dataView
  
})


# Section Simulasi/Sunting HARGA ---------------------------------------------
observeEvent(input$simPrice_button,{
  # browser()
  dataTemplate()
  resultParTemp()
  removeUI(selector='#showResult_par')
  showModal(dataModalSimPrice())
  
})

dataModalSimPrice <- function(failed = FALSE) {
  modalDialog( 
    footer=tagList(
      actionButton(("closeModalSimPrice"), "Batal", style="color: white;background-color: red;"),
      actionButton(("runSimPrice"), "Simpan Tabel dan Lanjutkan",style="color: white;background-color: green;")
    ),
    argonTabSet(
      id = "tabSimHarga",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "l",
      width = 12,
      argonTab(
        tabName = "Menyunting Tabel Harga",
        active = T,
        h3("Tabel Harga Output", align = "center"),
        rHandsontableOutput('simHargaOutput'),
        br(),
        h3("Tabel Harga Input", align = "center"),
        rHandsontableOutput('simHargaInput')
      ))
    ,
    size="l",
    easyClose = FALSE)
}

observeEvent(input$closeModalSimPrice,{
  # browser()
  removeUI(selector='#showResult_par')
  removeModal()
})


output$simHargaOutput <- renderRHandsontable({
  rhandsontable(valSimP2(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 100,
  )%>%
    hot_col(c(1:3), readOnly = TRUE)
})


valSimP2 <- eventReactive(input$simPrice_button,{
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  
  readDataTemplate <- read.table(paste0(datapath,input$sut_par,"_",input$kom_par,"_",input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".csv"), header = T, sep = ",")
  readDataTemplate[is.na(readDataTemplate)] <- 0
  readDataTemplate <- lowcase(readDataTemplate, c("faktor","komponen","jenis","unit.harga","unit"))
  
  outputData <- filter(readDataTemplate,faktor == c("output"))
  priceOutput <- outputData[,c("jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
  # priceOutput <- (priceOutput[,-1])
  
  
  fileName <- paste0(datapath,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  
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

output$simHargaInput <- renderRHandsontable({
  rhandsontable(valSimP1(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 600,
  )%>%
    hot_col(c(1:3), readOnly = TRUE)
})

valSimP1 <- eventReactive(input$simPrice_button,{
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  readDataTemplate <- read.table(paste0(datapath,input$sut_par,"_",input$kom_par,"_",input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".csv"), header = T, sep = ",")
  readDataTemplate[is.na(readDataTemplate)] <- 0
  readDataTemplate <- lowcase(readDataTemplate, c("faktor","komponen","jenis","unit.harga","unit"))
  
  inputData <- filter(readDataTemplate,faktor == c("input"))
  priceInput <- inputData[,c("jenis","unit.harga","harga.privat","harga.sosial")] #memfilter tabel harga
  # priceInput <- (priceInput[,-1])
  
  fileName <- paste0(datapath,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
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








observeEvent(input$runSimPrice,{
  # save data untuk setiap perubahan
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  dataDefine <- readRDS(fileName)
  
  editNewP1<-as.data.frame(hot_to_r(input$simHargaInput))
  editNewP1[is.na(editNewP1)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  
  editNewP2<-as.data.frame(hot_to_r(input$simHargaOutput))
  editNewP2[is.na(editNewP2)] <- 0 
  
  dataDefine$priceInput <- editNewP1
  dataDefine$priceOutput <- editNewP2
  saveRDS(dataDefine,file = fileName)
  
  print("tabel harga yang baru")
  
})

observeEvent(c(input$runSimPrice, input$runSimIO),{
  removeModal()
  removeUI(selector='#showResult_par')
  insertUI(selector='#uiShowResult_par',
           where='afterEnd',
           ui= uiOutput('showResult_par'))
})

output$showResult_par <- renderUI({
  fluidPage(
    fluidRow(
      column(11,
             br(),
             br(),
             h1(paste0("HASIL ANALISIS"," ",input$kom_par," ",input$sut_par), align = "center"),
             h1(paste0("di ",input$selected_wilayah_par," pada tahun ",input$th_par," dengan tipe lahan ", input$tipeLahan_par), align = "center"),
             br(),
      )
    ),
    br(),
    fluidRow(
      column(6,
             id = 'bau_par',
             tags$style('#bau_par {
                            background-color: #00cca3;
                            }'),
             # tags$head(tags$style('h3 {color:white;}')),
             h3("Business As Usual (BAU)", align = "center")
             
      ),
      column(6,
             id = 'sim_par',
             tags$style('#sim_par {
                            background-color: #b3b3ff;
                            }'),
             h3("Simulasi", align = "center")
      )
    ),
    fluidRow(
      column(6,
             dataTableOutput("tableResultBAU1_par"),
      ),
      
      column(6,
             dataTableOutput("tableResultSimulasi1_par"),
             
      ),
      column(6,
             dataTableOutput("tableResultBAU2_par")
             
      ),
      column(6,
             dataTableOutput("tableResultSimulasi2_par")
             
      ),
    ),
    
    br(),
    br(),
    # column(12,
    #        id = 'tableNPV_par',
    #        tags$style('#tableNPV_par {
    #                         background-color: #CCFFCC;
    #                         }'),
    #        h3("Tabel NPV dalam 1 Wilayah", align = "center")
    #        
    # ),
    # fluidRow(
    #   column(12,
    #          dataTableOutput('showTableAllKomoditas_par')
    #   )
    # ),
    
    br(),
    br(),
    fluidRow(
      column(4,
             id = 'plotCom_par',
             tags$style('#plotCom_par {
                            background-color: #CCFFCC;
                            }'),
             h3("Barchart NPV BAU vs Simulasi", align = "center")
             
      ),
      column(8,
             id = 'plotAll_par',
             tags$style('#plotAll_par {
                            background-color: #CCFFCC;
                            }'),
             h3("Barchart NPV seluruh SUT dalam 1 Wilayah", align = "center")
             
      ),
      column(4,
             tags$div(id = 'uiplotComparing_par')
      ),
      column(8,
             tags$div(id = 'uiShowPlotAllKomoditas_par')
      )
    ),
    br(),
    br(),
    column(12,
           id = 'grafikProfit_par',
           tags$style('#grafikProfit_par {
                            background-color: #CCFFCC;
                            }'),
           h3("Grafik Profit Tahunan", align = "center")
           
    ),
    fluidRow(
      column(6,
             plotlyOutput('showPlotProfitPrivat_par')
      ),
      column(6,
             plotlyOutput('showPlotProfitSosial_par')
      )
    ),
    fluidRow(
      column(6,
             plotlyOutput('showPlotKumProfitPrivat_par')
      ),
      column(6,
             plotlyOutput('showPlotKumProfitSosial_par')
      )
    ),
    fluidRow(
      column(2,
             actionButton(("saveNewPAM_par"),"Simpan PAM baru",icon("paper-plane"),style="color: white;background-color: green;"),
             br(),
             tags$div(id='teksNewPamSave_par')
      )
    )
    
    
  )
})




observeEvent(c(input$runSimPrice, input$runSimIO,
               input$sut_par,input$kom_par,input$selected_prov_par, input$selected_wilayah_par,input$th_par,input$tipeLahan_par),{
  removeUI(selector = '#showplotComparing_par')
  removeUI(selector = '#showPlotAllKomoditas_par')
  
  insertUI(selector='#uiplotComparing_par',
           where='afterEnd',
           ui= plotlyOutput('showplotComparing_par'))
  
  
  insertUI(selector='#uiShowPlotAllKomoditas_par',
           where='afterEnd',
           ui= plotlyOutput('showPlotAllKomoditas_par'))
})


output$showPlotAllKomoditas_par <- renderPlotly({
  withProgress(message = 'Collecting data in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   sum(runif(10000000,0,1))
                 }
               })
  
  plotAllKomoditas_par() 
})

output$showplotComparing_par <- renderPlotly({
  # withProgress(message = 'Collecting data in progress',
  #              detail = 'This may take a while...', value = 0, {
  #                for (i in 1:15) {
  #                  incProgress(1/15)
  #                  sum(runif(10000000,0,1))
  #                }
  #              })
  
  preparePlot_par()
})

# end - simulasi tabel harga ---------------------------------------------


# Section Simulasi/Sunting kuantitas
observeEvent(input$simIO_button,{
  showModal(suntingSimKuantitas())
})

suntingSimKuantitas <- function(failed = FALSE) {
  modalDialog(
    footer=tagList(
      actionButton(("closeModalSimIO"), "Batal", style="color: white;background-color: red;"),
      actionButton(("runSimIO"),"Simpan dan Lanjut",style="color: white;
                         background-color: green;")
    ),
    argonTabSet(
      id = "tabSimIO",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "l",
      width = 12,
      argonTab(
        tabName = "Menyunting Tabel Kuantitas",
        active = T,
        h3("Tabel Kuantitas Output", align = "center"),
        rHandsontableOutput('simIOOutput'),
        br(),
        h3("Tabel Kuantitas Input", align = "center"),
        rHandsontableOutput('simIOInput')
      ))
    ,
    size="l",
    easyClose = FALSE)
}

observeEvent(input$closeModalSimIO,{
  removeUI(selector='#showResult_par')
  removeModal()
})

output$simIOOutput <- renderRHandsontable({
  rhandsontable(valSimIO2(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 100,
  )%>%
    hot_col(c(1:3), readOnly = TRUE)
})


valSimIO2 <- eventReactive(input$simIO_button,{
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  dataDefine <- readRDS(fileName)
  
  reactData$tableIO2 <- dataDefine$ioOutput
  reactData$tableIO2
})

output$simIOInput <- renderRHandsontable({
  rhandsontable(valSimIO1(),
                rowHeaderWidth = 50,
                fixedColumnsLeft = 2,
                height = 500,
  )%>%
    hot_col(c(1:3), readOnly = TRUE)
})


valSimIO1 <- eventReactive(input$simIO_button,{
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  dataDefine <- readRDS(fileName)
  
  reactData$tableIO2 <- dataDefine$ioInput
  reactData$tableIO2
})




observeEvent(input$runSimIO,{
  # save data untuk setiap perubahan
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  dataDefine <- readRDS(fileName)
  
  editNewOutput<-as.data.frame(hot_to_r(input$simIOOutput))
  editNewOutput[is.na(editNewOutput)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  dataDefine$ioOutput <- editNewOutput
  
  editNewInput<-as.data.frame(hot_to_r(input$simIOInput))
  editNewInput[is.na(editNewInput)] <- 0 #jika ada nilai numeric yang kosong, klo kol 1:3 kosong dia baca nya ttp ada nilai bukan null atau na
  dataDefine$ioInput <- editNewInput
  
  saveRDS(dataDefine,file = fileName)
})

# end - simulasi tabel kuantitas ---------------------------------------------

################################################################################
#                                                                              #
#                                RESULT                                        #
#                                                                              #
################################################################################
data.graph_par <- eventReactive(c(input$runSimPrice,input$runSimIO),{
  # data.graph <- reactive({  
  # observeEvent(input$running_button,{
  # browser()

  print("cek data gab ")
  
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath,"resultParTemp","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
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
    
    
    # hitung EAE -------------------------------------------------------------- 
    atas <- rate.p*((1+rate.p/100)^yearIO)
    bawah <- (1+rate.p/100)^yearIO-1
    eae <- data.frame(npv.p*atas/bawah)
    colnames(eae)<-c("Equal Annual Equivalent (EAE)")
    rownames(eae) <- c("Value")
    eae
    
    # hitung Discounted estab cost --------------------------------------------------------------
    
    ################ penghitungan dec
    p.profit.ha <- p.profit/dataDefine$totalArea
    p.positif.cashflow <- ifelse(p.profit.ha > 0, 1, 0)
    
    p.total.cost.0 <- c(value0,p.total.cost)
    p.est.cost <- ifelse(p.positif.cashflow == 1, 0, p.total.cost.0)

    
    s.profit.ha <- s.profit/dataDefine$totalArea
    s.positif.cashflow <- ifelse(s.profit.ha > 0, 1, 0)
    
    s.total.cost.0 <- c(value0,s.total.cost)
    s.est.cost <- ifelse(s.positif.cashflow == 1, 0, s.total.cost.0)

    
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
    
    tabelGab <- list(tabel1=tabel1,tabel2=tabel2, tabel.profit = tabel.profit)
    tabelGab 
    
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
    
    # hitung EAE -------------------------------------------------------------- 
    atas <- dataDefine$rate.p*((1+dataDefine$rate.p/100)^yearIO)
    bawah <- (1+dataDefine$rate.p/100)^yearIO-1
    eae <- data.frame(npv.p*atas/bawah)
    colnames(eae)<-c("Equal Annual Equivalent (EAE)")
    rownames(eae) <- c("Value")
    eae
    
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
    showRateP <- (dataDefine$rate.p)
    showRateS <- (dataDefine$rate.s)
    showExRate <- (dataDefine$nilai.tukar)
    showBauMacro <- rbind(showRateP,showRateS,showExRate)
    rownames(showBauMacro)<-c("Discount Rate Private", "Discount Rate Social", "Nilai Tukar Rupiah")
    colnames(showBauMacro) <- c("Value")
    showBauMacro
    
    # ending  ------------------------------------------------------- 
    
    tabel1 <- rbind(hsl.npv,nlc,ec)
    tabel1[] <- lapply(tabel1, function(i) sprintf('%.6g', i))
    tabel1
    
    tabel2 <- rbind(t(eae),t(hp),t(lr),t(pi),t(bcr),t(rtl),t(lcost), t(nlcost),(showBauMacro))
    # tabel2[] <- lapply(tabel2, function(i) sprintf('%.6g', i))
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

data.graph.new_par <- eventReactive(c(input$runSimPrice,input$runSimIO),{
  # observeEvent(input$running_button,{
  # browser()
  
  # save data untuk setiap perubahan
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
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
    
    
    
    s.profit.ha <- s.profit/dataDefine$totalArea
    s.positif.cashflow <- ifelse(s.profit.ha > 0, 1, 0)
    
    s.total.cost.0 <- c(value0,s.total.cost)
    s.est.cost <- ifelse(s.positif.cashflow == 1, 0, s.total.cost.0)
    
    
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
    
    tabel1 <- rbind(hsl.npv,dec,ypc,lfe,lfo)
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
    
    
    # hitung EAE -------------------------------------------------------------- 
    atas <- dataDefine$rate.p*((1+dataDefine$rate.p/100)^yearIO)
    bawah <- (1+dataDefine$rate.p/100)^yearIO-1
    eae <- data.frame(npv.p*atas/bawah)
    colnames(eae)<-c("Equal Annual Equivalent (EAE)")
    rownames(eae) <- c("Value")
    eae
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
    # p.profit.ha <- p.profit[-1]/readDataTemplate$total.area[1]
    # s.profit.ha <- s.profit[-1]/readDataTemplate$total.area[1]
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
    
   
    
    # ending  lr ------------------------------------------------------- 
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
    dataDefine$eae <- eae
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
    
    tabel2 <- rbind(t(eae),t(hp),t(lr),t(pi),t(bcr),t(rtl),t(lcost), t(nlcost),(showBauMacro))
    # tabel2[] <- lapply(tabel2, function(i) sprintf('%.6g', i))
    # tabel2 <- formatC(tabel2, digits = 2, format = "f")
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

output$tableResultBAU1_par <- renderDataTable({
  datatable(data.graph_par()$tabel1, option=list(dom = "t")) 
  # %>%
  # formatStyle("PRIVATE",
  #   backgroundColor = "blue")
})

output$tableResultBAU2_par <- renderDataTable({
  datatable(data.graph_par()$tabel2, option=list(dom = "t"))
  
})

output$tableResultSimulasi1_par <- renderDataTable({
  datatable(data.graph.new_par()$tabel1, option=list(dom = "t"))
})

output$tableResultSimulasi2_par <- renderDataTable({
  datatable(data.graph.new_par()$tabel2, option=list(dom = "t"))
})

# output$plotComparing <- renderPlotly({
#   preparePlot_par()
# })


preparePlot_par <- eventReactive(c(input$runSimPrice,input$runSimIO),{

  print("persiapan membuat plot komoditas simulasi")
  
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath,"resultParTemp","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  dataDefine <- readRDS(fileName)

  
  
  dataPlotBAU <- data.frame(tipe.data=dataDefine$tipeData,
                            komoditas=dataDefine$kom,
                            NPV.Privat.RP=dataDefine$npv[1,1])

  fileName <- paste0(datapath,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  dataDefine <- readRDS(fileName)

  
  dataPlotSimulasi <- data.frame(tipe.data=dataDefine$tipeData,
                                 komoditas=dataDefine$kom,
                                 NPV.Privat.RP=dataDefine$npv[1,1])
  
  
  dataPlot <- rbind(dataPlotBAU,dataPlotSimulasi)
  
  
  dataPlot %>%
    group_by(tipe.data) %>%
    plot_ly(x = ~komoditas, y = ~NPV.Privat.RP, type = "bar", color = ~tipe.data)
})

# Start - Section plot seluruh komoditas---------------------------------------------
################################################################################
#                                                                              #
#                                 PLOT all komoditas                          #
#                                                                              #
################################################################################

# plotAllKomoditas_par <- eventReactive(c(input$runSimPrice,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
plotAllKomoditas_par <- eventReactive(c(input$runSimPrice,input$runSimIO),{
  print("persiapan membuat plot seluruh komoditas")
    
  # DATA PLOT BAU -----------------------------------------------------------
  folderSut <- sort(unique(komoditas$sut))
  folderwilayah <- filter(komoditas, wilayah == input$selected_wilayah_par)
  folderKom <- sort(unique(folderwilayah$nama_komoditas))
  
  kombinasiFolder <- as.vector(outer(folderSut, folderKom, paste, sep="/"))
  dirFile <- paste0("data/",kombinasiFolder)
  
  nameFiles <- list.files(path = paste0(dirFile,"/"),pattern = paste0("resultParTemp"))
  kombinasiFile <- as.vector(outer(dirFile, nameFiles, paste, sep="/"))
  cekFile <- file.exists(kombinasiFile) #cek keberadaan file ini ada atau engga
  
  # remove index yang cekFilenya == F, munculin yang cekFilenya == T aja
  indexFileTrue <- which(cekFile == T)
  kombinasiFile <- kombinasiFile[which(cekFile == T)]
  
  funcFile <- function(x){
    a <- readRDS(x)
    b <- c(x,a)
    b}
  
  
  ##### step 2 filter yang ada pattern input$selected_wilayah_par ex: (_ACEH)
  # cek dari vector kombinasiFile yang sudah di cek T or F nya
  provFile <- kombinasiFile %>% 
    str_subset(pattern = paste0("_",input$selected_wilayah_par))
  
  
  ##### step 3 filter yang ada pattern input$th_par ex: (_2020)
  tahunFile <- provFile %>% 
    str_subset(pattern = paste0("_",input$th_par))
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
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
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

# tableAllKomoditas_par <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
tableAllKomoditas_par <- eventReactive(c(input$runSimPrice,input$runSimIO),{

  folderSut <- sort(unique(komoditas$sut))
  folderwilayah <- filter(komoditas, wilayah == input$selected_wilayah)
  folderKom <- sort(unique(folderwilayah$nama_komoditas))
  
  kombinasiFolder <- as.vector(outer(folderSut, folderKom, paste, sep="/"))
  dirFile <- paste0("data/",kombinasiFolder)

  
  nameFiles <- list.files(path = paste0(dirFile,"/"),pattern = paste0("resultParTemp"))
  kombinasiFile <- as.vector(outer(dirFile, nameFiles, paste, sep="/"))
  cekFile <- file.exists(kombinasiFile) #cek keberadaan file ini ada atau engga
  
  # remove index yang cekFilenya == F, munculin yang cekFilenya == T aja
  indexFileTrue <- which(cekFile == T)
  kombinasiFile <- kombinasiFile[which(cekFile == T)]
  
  funcFile <- function(x){
    a <- readRDS(x)
    b <- c(x,a)
    b}
  
  
  ##### step 2 filter yang ada pattern input$selected_wilayah_par ex: (_ACEH)
  # cek dari vector kombinasiFile yang sudah di cek T or F nya
  provFile <- kombinasiFile %>% 
    str_subset(pattern = paste0("_",input$selected_wilayah_par))
  
  
  ##### step 3 filter yang ada pattern input$th_par ex: (_2020)
  tahunFile <- provFile %>% 
    str_subset(pattern = paste0("_",input$th_par))
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
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
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

output$showTableAllKomoditas_par <- renderDataTable({
  datatable(tableAllKomoditas_par())
})

output$showPlotProfitPrivat_par <- renderPlotly({
  profitPlotPrivat_par()
})

output$showPlotProfitSosial_par <- renderPlotly({
  profitPlotSosial_par()
})

# profitPlotPrivat_par <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
profitPlotPrivat_par <- eventReactive(c(input$runSimPrice,input$runSimIO),{
  bau.profit <- data.graph_par()$tabel.profit
  simulasi.profit <- data.graph.new_par()$tabel.profit
  
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

})

# profitPlotSosial_par <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
profitPlotSosial_par <- eventReactive(c(input$runSimPrice,input$runSimIO),{
  bau.profit <- data.graph_par()$tabel.profit
  simulasi.profit <- data.graph.new_par()$tabel.profit
  
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
  
})


output$showPlotKumProfitPrivat_par <- renderPlotly({
  profitPlotKumulatifPrivat_par()
})

output$showPlotKumProfitSosial_par <- renderPlotly({
  profitPlotKumulatifSosial_par()
})

# profitPlotKumulatifPrivat_par <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
profitPlotKumulatifPrivat_par <- eventReactive(c(input$runSimPrice,input$runSimIO),{
    
  
  bau.profit <- data.graph_par()$tabel.profit
  simulasi.profit <- data.graph.new_par()$tabel.profit
  
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

# profitPlotKumulatifSosial_par <- eventReactive(c(input$running_button,input$running_button_tanpaCapital, input$runningButton_capital, input$running_button_noEditCapital,input$running_button_LargeScale),{
profitPlotKumulatifSosial_par <- eventReactive(c(input$runSimPrice,input$runSimIO),{
  bau.profit <- data.graph_par()$tabel.profit
  simulasi.profit <- data.graph.new_par()$tabel.profit
  
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

})
# End - Section plot tipe kebun ---------------------------------------------



observeEvent(input$saveNewPAM, {
  # browser()
  datapath <- paste0("data/", input$sut_par, "/",input$kom_par, "/")
  fileName <- paste0(datapath,"saveParDat","_",
                     input$sut_par,"_",input$kom_par,"_",
                     input$selected_wilayah_par,"_",input$th_par,"_",input$tipeLahan_par,".rds")
  dataDefine <- readRDS(fileName)
  
  #replace informasi umum -- untuk lbh yakin yang tersave adalah pilihan terakhir user
  dataDefine$sut <- input$sut_par
  dataDefine$kom <- input$kom_par
  dataDefine$wilayah <- input$selected_wilayah_par
  dataDefine$th <- input$th_par
  dataDefine$tipeLahan <- input$tipeLahan_par
  # dataDefine$tipeKebun <- readDataTemplate$tipe.kebun
  # dataDefine$tipeData <- c("BAU")
  
  #replace asumsi macro-- untuk lbh yakin yang tersave adalah pilihan terakhir user
  dataDefine$rate.p <- input$rate.p
  dataDefine$rate.s <- input$rate.s
  dataDefine$nilai.tukar <- input$nilai.tukar
  # dataDefine$tipeData <- c("SIMULASI")
  
  saveRDS(dataDefine,file = fileName)
  
}) 



# End - Section Popup Modal Dialog ---------------------------------------------
# End - Section simulasi/sunting ---------------------------------------------


# End - Section tampilkan tabel ---------------------------------------------
