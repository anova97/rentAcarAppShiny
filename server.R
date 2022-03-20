
PYTHON_DEPENDENCIES = c('litex.regon')

login_details <- data.frame(user = c("user"),
                            pswd = c("user"))

login <- box(
    title = "Login",
    textInput("userName", "Nazwa użytkownika", value = "user"),
    passwordInput("passwd", "Hasło", value = "user"),
    br(),
    actionButton("Login", "Loguj")
)


source("R/firstStart.R")


server <- function(input, output, session) {

    source("R/libraries.R")
    source("R/functions.R")
    
    Sys.setenv(TZ='Europe/Warsaw')
    
    # commented so it is rreproductible
    # token <- drop_auth()
    # saveRDS(token, "droptoken.rds")
    # 
    # 
    # token <- readRDS("droptoken.rds")
    # 
    # drop_acc(dtoken = token)
    # 
    # outputDir <- "git"
    # 
    #funkcje do ladowania i zapisywania danych z dropboxa
    
    
    #ładowanie danych do wyboru w formularzu
    samochod<-reactive({
        samochody <- loadSam()
        wyborSam <- paste(samochody$numRej, samochody$model, sep = " ")
    })
    
    observe({
        wyborSam<-paste(samochody$numRej, samochody$model, sep = " ")
        wyb<-as.list(wyborSam)
        updateSelectInput(session, "samochod", "Wybierz samochod", choices = wyb)
        append(wyb, "wszystkie")
        updateSelectInput(session, "samochod1", "Wybierz samochod", choices = wyb)
        
    })
    
    osoba<-reactive({
        klienci<-loadKlient()
        klienci<-map_df(klienci, rev)
        saveKlient(klienci)
        wyborOs<-paste(klienci$imie, klienci$nazwisko, klienci$telefon, sep = " ")
    })
    
    observe({
        updateSelectInput(session, "osoba", "Wybierz osobę", choices = wyborOs)
        updateSelectInput(session, "osobaDod", "Wybierz osobę", choices = wyborOs)
    })
    
    wybFirma<-reactive({
        firmy<-loadFir()
        wyborFirm<- paste("Firma:", firmy$nazwa, sep = " ")
    })
    
    observe({
        updateSelectInput(session, "wybFirma", "Wybierz firmę", choices = wyborFirm)
        updateSelectInput(session, "wybFirmaDod", "Wybierz firmę", choices = wyborFirm)
    })
    
    
    firmy<-loadFir()
    firmy<-map_df(firmy, rev)
    wyborFirm<- paste("Firma:", firmy$nazwa, sep = " ")


    
    output$rezerwacje<-DT::renderDataTable({
        rezerwacje<-data.frame(0,0,0,0,0,0)
        rezerwacje<-loadData()
        rezerwacje<-map_df(rezerwacje, rev)
        rezerwacje<-datatable(rezerwacje, editable = TRUE, selection='single', options = list(autoWidth = TRUE, pageLength=15, columnDefs = list(list(visible=FALSE, targets=c(7:15, 17:22))), language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
        
    })
    
    observeEvent(input$rezerwacje_cell_edit, {
        rezerwacje<-loadData()
        info = input$rezerwacje_cell_edit
        rezerwacje<-map_df(rezerwacje, rev)
        rezerwacje <- editData(rezerwacje, info)
        rezerwacje<-map_df(rezerwacje, rev)
        saveData(rezerwacje)
    })
    
    observeEvent(input$addDod, {
        if((input$dataDoDod>=input$dataOdDod))
        {
            
            rezerwacje<-loadData()
            
            n<-(nrow(rezerwacje)+1)
            rezerwacje[n,1]=as.character(input$dataOdDod)
            rezerwacje[n,2]=as.character(input$dataDoDod)
            rezerwacje[n,3]=input$samochodDod
            rezerwacje[n,4]=input$kwotaDod
            
            if(input$klFirmaDod==0)
            {
                rezerwacje[n,5]=input$osobaDod
            }
            if(input$klFirmaDod==1)
            {
                rezerwacje[n,5]=input$wybFirmaDod
            }
            
            rezerwacje[n,6]=input$komentarzDod
            
            odKtorej<-as.character(input$godzinaOdDod)
            godz<-strsplit(odKtorej, " ")
            odTej<-unlist(godz)[2]
            
            doKtorej<-as.character(input$godzinaDoDod)
            
            doKtorej<-as.character(input$godzinaDoDod)
            godzi<-strsplit(doKtorej, " ")
            doTej<-unlist(godzi)[2]
            
            rezerwacje[n,7]=odTej
            rezerwacje[n,8]=doTej
            rezerwacje[n,9]=as.character(input$dataZawUmowy)
            rezerwacje[n,10]=as.character(input$numerUmowy)
            rezerwacje[n,11]=input$limit1
            rezerwacje[n,12]=input$limit2
            rezerwacje[n,13]=input$cena1
            rezerwacje[n,14]=input$jednCzasu
            rezerwacje[n,15]=input$cena2
            rezerwacje[n,16]=input$kaucja
            
            
            czystosc<-c("czysty", "brudny")
            
            rezerwacje[n,17]=czystosc[as.numeric(input$zewnatrz)]
            rezerwacje[n,18]=czystosc[as.numeric(input$wewnatrz)]
            rezerwacje[n,19]=input$stanLicznika
            rezerwacje[n,20]=input$stanPaliwa
            
            przekaz<-input$przekazano
            przekazano<-str_c(przekaz, collapse = ", ")
            
            rezerwacje[n,21]=przekazano
            rezerwacje[n,22]=input$zagranica
            
            
            
            saveData(rezerwacje)
            
            showModal(modalDialog(
                radioButtons(inputId="firmaDod", label = "Wybierz firmę",
                             choices = list("Company 1" = 1, "Company 2" = 2)),
                downloadBttn(
                    outputId = "reportAuto",
                    style = "material-flat",
                    color = "primary", 
                    label = "Pobierz umowę"
                ),
                size = "s", easyClose = TRUE, footer = modalButton(label = "Zamknij"))
            )
            
            output$reportAuto <- downloadHandler(
                
                filename = paste('Umowa', Sys.time()+hours(czasZimaLato()),
                                 '.pdf', sep=''),
                content = function(file) {
                    {
                        rezerwacje<-loadData()
                        klienci<-loadKlient()
                        klienci<-map_df(klienci, rev)
                        firmy<-loadFir()
                        samochody<-loadSam()
                        rezerwacja<-rezerwacje[nrow(rezerwacje),]
                        
                        spl<-strsplit(rezerwacja$imieNazw, " ")
                        str<-unlist(spl)
                        
                        if(sum(str==("Firma:"))==0)
                        {
                            
                            klient<-klienci[which(klienci$telefon==str[3]),]
                            
                            if(nrow(klient)==0)
                            {
                                klient<-klienci[which(klienci$telefon==str[1]),]
                            }
                            if(nrow(klient)==0)
                            {
                                klient<-klienci[which(klienci$telefon==str[2]),]
                            }
                            imie=klient$imie %>% replace(., is.na(.), " ")
                            nazwisko=klient$nazwisko %>% replace(., is.na(.), " ")
                            kod=klient$kod %>% replace(., is.na(.), " ")
                            miasto=klient$miasto %>% replace(., is.na(.), " ")
                            ulica=klient$ulica %>% replace(., is.na(.), " ")
                            telefon=klient$telefon %>% replace(., is.na(.), " ")
                            dowod=klient$dowod %>% replace(., is.na(.), " ")
                            pesel=klient$pesel %>% replace(., is.na(.), " ")
                            katPraw=klient$kat %>% replace(., is.na(.), " ")
                            numPraw=klient$numPraw %>% replace(., is.na(.), " ")
                            
                            aut<-strsplit(rezerwacja$samochod, " ")
                            aut1<-unlist(aut)
                            auto<-samochody[which(samochody$numRej==aut1[1]),] 
                            marka=auto$marka %>% replace(., is.na(.), " ")
                            model=auto$model %>% replace(., is.na(.), " ")
                            vin=auto$vin %>% replace(., is.na(.), " ")
                            numRej=auto$numRej %>% replace(., is.na(.), " ")
                            dowodRej=auto$dowod %>% replace(., is.na(.), " ")
                            paliwo=auto$paliwo %>% replace(., is.na(.), " ")
                            
                            stanPaliwa=rezerwacja$stanPaliwa %>% replace(., is.na(.), " ")
                            stanLicznika=rezerwacja$stanLicznika %>% replace(., is.na(.), " ")
                            zewnatrz=rezerwacja$zewnatrz %>% replace(., is.na(.), " ")
                            wewnatrz=rezerwacja$wewnatrz %>% replace(., is.na(.), " ")
                            przekazano=rezerwacja$przekazano %>% replace(., is.na(.), " ")
                            
                            dataOd=rezerwacja$dataOd %>% replace(., is.na(.), " ")
                            dataDo=rezerwacja$dataDo %>% replace(., is.na(.), " ")
                            godzinaOd=rezerwacja$godzinaOd %>% replace(., is.na(.), " ")
                            godzinaDo=rezerwacja$godzinaDo %>% replace(., is.na(.), " ")
                            dataZawUmowy=rezerwacja$dataZawUmowy %>% replace(., is.na(.), " ")
                            numerUmowy=rezerwacja$numerUmowy %>% replace(., is.na(.), " ")
                            limit1=rezerwacja$limit1 %>% replace(., is.na(.), "")
                            if(limit1=="")
                            {
                                limit1="........."
                            }
                            limit2=rezerwacja$limit2 %>% replace(., is.na(.), "")
                            if(limit2=="")
                            {
                                limit2="........."
                            }
                            cena1=rezerwacja$cena1 %>% replace(., is.na(.), "")
                            if(cena1=="")
                            {
                                cena1="........."
                            }
                            jednCzasu=rezerwacja$jednCzasu %>% replace(., is.na(.), "")
                            if(jednCzasu=="")
                            {
                                jednCzasu="........."
                            }
                            cena2=rezerwacja$cena2 %>% replace(., is.na(.), "")
                            if(cena2=="")
                            {
                                cena2="........."
                            }
                            kaucja=rezerwacja$kaucja %>% replace(., is.na(.), "")
                            if(kaucja=="")
                            {
                                kaucja="........."
                            }
                            zagranica=rezerwacja$zagranica %>% replace(., is.na(.), "")
                            if(zagranica=="")
                            {
                                zagranica="........."
                            }
                            
                            
                            params <- list(Imie = imie, Nazwisko=nazwisko, Telefon=telefon, Kod=kod, Miasto=miasto, Ulica=ulica, Pesel=pesel, Dowod=dowod, KatPraw=katPraw, NumPraw=numPraw,
                                           Marka=marka, Model=model, Vin=vin, NumRej=numRej, DowodRej=dowodRej, DataOd=dataOd, DataDo=dataDo, 
                                           Paliwo=paliwo, StanPaliwa=stanPaliwa, StanLicznika=stanLicznika, Zewnatrz=zewnatrz, Wewnatrz=wewnatrz, Przekazano=przekazano, 
                                           GodzinaOdDod=godzinaOd, GodzinaDoDod=godzinaDo, DataZawUmowy=dataZawUmowy, NumerUmowy=numerUmowy,
                                           Limit1=limit1, Limit2=limit2, Cena1=cena1, JednCzasu=jednCzasu, Cena2=cena2, Kaucja=kaucja, Zagranica=zagranica)
                            
                            if(input$firmaDod==2)
                            {
                                rmarkdown::render("RMD/automatyczny.rmd", output_file = file,
                                                  params = params,
                                                  envir = new.env(parent = globalenv())
                                )
                                output$pobrano<-renderText({
                                    return("Pobrano umowę, sprawdź w pobranych")
                                })
                            }
                            
                            if(input$firmaDod==1)
                            {
                                rmarkdown::render("RMD/auto2.rmd", output_file = file,
                                                  params = params,
                                                  envir = new.env(parent = globalenv())
                                )
                                
                            }
                        }
                        
                        if(sum(str==("Firma:"))>0)
                        {
                            podzial<-strsplit(rezerwacja$imieNazw, "Firma: ")
                            podzial2=unlist(podzial)
                            nazwa=podzial2[2]
                            firma<-firmy[which(firmy$nazwa==nazwa),]
                            
                            imie=firma$imie %>% replace(., is.na(.), " ")
                            nazwisko=firma$nazwisko %>% replace(., is.na(.), " ")
                            telefon=firma$telefon %>% replace(., is.na(.), " ")
                            dowod=firma$dowod %>% replace(., is.na(.), " ")
                            pesel=firma$pesel %>% replace(., is.na(.), " ")
                            katPraw=firma$katPraw %>% replace(., is.na(.), " ")
                            numPraw=firma$numPraw %>% replace(., is.na(.), " ")
                            
                            nazwaFir = firma$nazwa %>% replace(., is.na(.), " ")
                            nipFir = firma$nip %>% replace(., is.na(.), " ")
                            uLFir = firma$ul %>% replace(., is.na(.), " ")
                            miastoFir = firma$miejsc %>% replace(., is.na(.), " ")
                            kodFir = firma$kod %>% replace(., is.na(.), " ")
                            regonFir = firma$regon %>% replace(., is.na(.), " ")
                            
                            
                            aut<-strsplit(rezerwacja$samochod, " ")
                            aut1<-unlist(aut)
                            auto<-samochody[which(samochody$numRej==aut1[1]),]
                            marka=auto$marka %>% replace(., is.na(.), " ")
                            model=auto$model %>% replace(., is.na(.), " ")
                            vin=auto$vin %>% replace(., is.na(.), "")
                            numRej=auto$numRej %>% replace(., is.na(.), " ")
                            dowodRej=auto$dowod %>% replace(., is.na(.), " ")
                            paliwo=auto$paliwo %>% replace(., is.na(.), " ")
                            
                            stanPaliwa=rezerwacja$stanPaliwa %>% replace(., is.na(.), " ")
                            stanLicznika=rezerwacja$stanLicznika %>% replace(., is.na(.), " ")
                            zewnatrz=rezerwacja$zewnatrz %>% replace(., is.na(.), " ")
                            wewnatrz=rezerwacja$wewnatrz %>% replace(., is.na(.), " ")
                            przekazano=rezerwacja$przekazano %>% replace(., is.na(.), " ")
                            
                            dataOd=rezerwacja$dataOd %>% replace(., is.na(.), " ")
                            dataDo=rezerwacja$dataDo %>% replace(., is.na(.), " ")
                            godzinaOd=rezerwacja$godzinaOd %>% replace(., is.na(.), " ")
                            godzinaDo=rezerwacja$godzinaDo %>% replace(., is.na(.), " ")
                            dataZawUmowy=rezerwacja$dataZawUmowy %>% replace(., is.na(.), " ")
                            numerUmowy=rezerwacja$numerUmowy %>% replace(., is.na(.), " ")
                            limit1=rezerwacja$limit1 %>% replace(., is.na(.), "")
                            if(limit1=="")
                            {
                                limit1="........."
                            }
                            limit2=rezerwacja$limit2 %>% replace(., is.na(.), "")
                            if(limit2=="")
                            {
                                limit2="........."
                            }
                            cena1=rezerwacja$cena1 %>% replace(., is.na(.), "")
                            if(cena1=="")
                            {
                                cena1="........."
                            }
                            jednCzasu=rezerwacja$jednCzasu %>% replace(., is.na(.), "")
                            if(jednCzasu=="")
                            {
                                jednCzasu="........."
                            }
                            cena2=rezerwacja$cena2 %>% replace(., is.na(.), "")
                            if(cena2=="")
                            {
                                cena2="........."
                            }
                            kaucja=rezerwacja$kaucja %>% replace(., is.na(.), "")
                            if(kaucja=="")
                            {
                                kaucja="........."
                            }
                            zagranica=rezerwacja$zagranica %>% replace(., is.na(.), "")
                            if(zagranica=="")
                            {
                                zagranica="........."
                            }
                            
                            
                            params2 <- list(NazwaFir = nazwaFir, NipFir = nipFir, ULFir = uLFir, KodFir = kodFir, MiastoFir = miastoFir, RegonFir = regonFir,
                                            Imie = imie, Nazwisko = nazwisko, Telefon = telefon, Dowod = dowod, Pesel = pesel, KatPraw = katPraw, NumPraw = numPraw,
                                            Marka=marka, Model=model, Vin=vin, NumRej=numRej, DowodRej=dowodRej, DataOd=dataOd, DataDo=dataDo, Paliwo=paliwo,
                                            StanPaliwa=stanPaliwa, StanLicznika=stanLicznika, Zewnatrz=zewnatrz, Wewnatrz=wewnatrz, Przekazano=przekazano, 
                                            GodzinaOdDod=godzinaOd, GodzinaDoDod=godzinaDo, DataZawUmowy=dataZawUmowy, NumerUmowy=numerUmowy,
                                            Limit1=limit1, Limit2=limit2, Cena1=cena1, JednCzasu=jednCzasu, Cena2=cena2, Kaucja=kaucja, Zagranica=zagranica)
                            
                            if(input$firmaDod==2)
                            {
                                rmarkdown::render("RMD/autoFir1.rmd", output_file = file,
                                                  params = params2,
                                                  envir = new.env(parent = globalenv())
                                )
                            }
                            
                            if(input$firmaDod==1)
                            {
                                rmarkdown::render("RMD/autoFir2.rmd", output_file = file,
                                                  params = params2,
                                                  envir = new.env(parent = globalenv())
                                )
                            }
                        }
                        
                    }
                }
            )
            
            output$rezerwacje<-renderDataTable({
                rezerwacje<-loadData()
                rezerwacje<-map_df(rezerwacje, rev)
                rezerwacje<-datatable(rezerwacje, editable = TRUE, selection='single', options = list(autoWidth = TRUE, pageLength=15, columnDefs = list(list(visible=FALSE, targets=c(7:15, 17:22))), language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
                
            })

            shinyjs::reset("dataOdDod")
            shinyjs::reset("dataDoDod")
            shinyjs::reset("samochodDod")
            shinyjs::reset("kwotaDod")
            shinyjs::reset("osobaDod")
            shinyjs::reset("komentarzDod")
        }
        output$sprawdzenie1<-renderText({
            if(input$dataDo<input$dataOd)
            {
                return("Sprawdz daty!")
            }
        })
        
    })
    
    observeEvent(input$dataDoDod, 
                 {
                     ile=difftime(as.Date(input$dataDoDod), as.Date(input$dataOdDod))
                     output$ileDni<-renderText({
                         if(ile==1)
                         {
                             return( paste(ile, "doba", sep = " ") )
                         }
                         if(ile>1)
                         {
                             return( paste(ile, "dni", sep = " ") )   
                         }
                     })
                 })
    
    observeEvent(input$dataZawUmowy,
                 {
                     data=as.character(input$dataZawUmowy)
                     numer=str_replace_all(data, "-", "/")
                     updateTextInput(session, "numerUmowy", value = numer)
                 })
    observeEvent(input$cena2,
                 {
                     ile=parse_number(input$cena2)
                     updateTextInput(session, "kwotaDod", value = ile)
                 })
    
    observeEvent(input$add, {
        rezerwacje<-loadData()
    
        n<-(nrow(rezerwacje)+1)
        rezerwacje[n,1]=as.character(input$dataOd)
        rezerwacje[n,2]=as.character(input$dataDo)
        rezerwacje[n,3]=input$samochod
        rezerwacje[n,4]=input$kwota
        
        if(input$klFirma==0)
        {
            rezerwacje[n,5]=input$osoba
        }
        if(input$klFirma==1)
        {
            rezerwacje[n,5]=input$wybFirma
        }
        

        rezerwacje[n,6]=input$komentarz
        
        
        if((input$dataDo>=input$dataOd))
        {
            saveData(rezerwacje) 
            output$rezerwacje<-renderDataTable({
                rezerwacje<-loadData()
                rezerwacje<-map_df(rezerwacje, rev)
                rezerwacje<-datatable(rezerwacje, editable = TRUE, selection='single', options = list(autoWidth = TRUE, pageLength=15, columnDefs = list(list(visible=FALSE, targets=c(7:15, 17:22))), language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
                
            })
            output$dodajRez<-renderText({
                return("Dodano rezerwację")
            })
            show("dodajRez")
            delay(6000,hide("dodajRez"))
            shinyjs::reset("dataOd")
            shinyjs::reset("dataDo")
            shinyjs::reset("samochod")
            shinyjs::reset("kwota")
            shinyjs::reset("osoba")
            shinyjs::reset("komentarz")
        }
        output$sprawdzenie1<-renderText({
            if(input$dataDo<input$dataOd)
            {
                return("Sprawdz daty!")
            }
        })
        
    })
    
    observeEvent(input$delete, 
                 {
                     rezerwacje<-loadData()
                     rezerwacje<-map_df(rezerwacje, rev)
                     if(!is.null(input$rezerwacje_rows_selected))
                     {
                         
                         confirmSweetAlert(
                             session = session,
                             inputId = "usunRezPotwierdz",
                             title = "Czy potwierdzasz usunięcie rezerwacji?",
                             text = paste(rezerwacje$samochod[as.numeric(input$rezerwacje_rows_selected)],
                                           "przez", rezerwacje$imieNazw[as.numeric(input$rezerwacje_rows_selected)], sep = " "), 
                             btn_labels = c("Anuluj", "Potwierdz")
                         )
                     }

                 })
    
    observeEvent(input$usunRezPotwierdz,
                 if(isTRUE(input$usunRezPotwierdz))
                 {
                     rezerwacje<-loadData()
                     rezerwacje<-map_df(rezerwacje, rev)
                     rezerwacje<-rezerwacje[-as.numeric(input$rezerwacje_rows_selected),]
                     rezerwacje<-map_df(rezerwacje, rev)
                     saveData(rezerwacje)
                     output$rezerwacje<-renderDataTable(
                         {
                             rezerwacje<-loadData()
                             rezerwacje<-map_df(rezerwacje, rev)
                             rezerwacje<-datatable(rezerwacje, editable = TRUE, selection='single', options = list(autoWidth = TRUE, pageLength=15, columnDefs = list(list(visible=FALSE, targets=c(7:15, 17:22))), language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
                         }
                     )
                     output$usunRez<-renderText({
                         return(input$usunRezPotwierdz)
                     })
                     show("usunRez")
                     delay(7000,hide("usunRez"))
                 }
                 
    )
    
    observeEvent(input$refresh, 
                 {
                     rezerwacje<-loadData()
                     samochody<-loadSam()
                     klienci<-loadKlient()
                     klienci<-map_df(klienci, rev)
                     firmy<-loadFir()
                     firmy<-map_df(firmy, rev)
                     wyborSam<-paste(samochody$numRej, samochody$model, sep = " ")
                     wyborOs<-paste(klienci$imie, klienci$nazwisko, klienci$telefon, sep = " ")
                    
                     updateSelectInput(session, "osoba", "Wybierz osobę", choices = wyborOs)
                     updateSelectInput(session, "samochod", "Wybierz samochód:", choices = wyborSam)
                     updateSelectInput(session, "osobaDod", "Wybierz osobę", choices = wyborOs)
                     updateSelectInput(session, "samochodDod", "Wybierz samochód:", choices = wyborSam)
                     
                     output$rezerwacje<-renderDataTable(
                         {
                             rezerwacje<-loadData()
                             rezerwacje<-map_df(rezerwacje, rev)
                             rezerwacje<-datatable(rezerwacje, editable = TRUE, selection='single', options = list(autoWidth = TRUE, pageLength=15, columnDefs = list(list(visible=FALSE, targets=c(7:15, 17:22))), language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
                         }
                     )
                 })
    
    observeEvent(input$aktualizuj, 
                 {
                     rezerwacje<-loadData()
                     rezerwacje<-map_df(rezerwacje, rev)
                     klienci<-loadKlient()
                     
                     if(!is.null(input$rezerwacje_rows_selected))
                     {
                         rezerwacja<-rezerwacje[as.numeric(input$rezerwacje_rows_selected),]
                         ktore<-as.numeric(input$rezerwacje_rows_selected)
                         spl<-strsplit(rezerwacja$imieNazw, " ")
                         str<-unlist(spl)
                         klient<-klienci[which(klienci$telefon==str[3]),]
                         if(nrow(klient)==0)
                         {
                             klient<-klienci[which(klienci$telefon==str[1]),]
                         }
                         if(nrow(klient)==0)
                         {
                             klient<-klienci[which(klienci$telefon==str[2]),]
                         }
                         osoba<-paste(klient$imie, klient$nazwisko, klient$telefon, sep = " ")
                         rezerwacje[ktore,5]<-osoba
                         rezerwacje<-map_df(rezerwacje, rev)
                         saveData(rezerwacje)
                         
                         output$rezerwacje<-renderDataTable(
                             {
                                 rezerwacje<-loadData()
                                 rezerwacje<-map_df(rezerwacje, rev)
                                 rezerwacje<-datatable(rezerwacje, editable = TRUE, selection='single', options = list(autoWidth = TRUE, pageLength=15, columnDefs = list(list(visible=FALSE, targets=c(7:15, 17:22))), language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
                             }
                         )
                     } 
                 })
    output$reportNowy <- downloadHandler(
        
        filename = paste('Umowa', Sys.time()+hours(czasZimaLato()),
                         '.pdf', sep=''),
        content = function(file) {
            {
                if(!is.null(input$rezerwacje_rows_selected))
                {
                
                rezerwacje<-loadData()
                rezerwacje<-map_df(rezerwacje, rev)
                klienci<-loadKlient()
                klienci<-map_df(klienci, rev)
                firmy<-loadFir()
                samochody<-loadSam()
                rezerwacja<-rezerwacje[as.numeric(input$rezerwacje_rows_selected),]
                
                spl<-strsplit(rezerwacja$imieNazw, " ")
                str<-unlist(spl)
                
                if(sum(str==("Firma:"))==0)
                {
                    
                    klient<-klienci[which(klienci$telefon==str[3]),]
                    
                    if(nrow(klient)==0)
                    {
                        klient<-klienci[which(klienci$telefon==str[1]),]
                    }
                    if(nrow(klient)==0)
                    {
                        klient<-klienci[which(klienci$telefon==str[2]),]
                    }
                    imie=klient$imie %>% replace(., is.na(.), " ")
                    nazwisko=klient$nazwisko %>% replace(., is.na(.), " ")
                    kod=klient$kod %>% replace(., is.na(.), " ")
                    miasto=klient$miasto %>% replace(., is.na(.), " ")
                    ulica=klient$ulica %>% replace(., is.na(.), " ")
                    telefon=klient$telefon %>% replace(., is.na(.), " ")
                    dowod=klient$dowod %>% replace(., is.na(.), " ")
                    pesel=klient$pesel %>% replace(., is.na(.), " ")
                    katPraw=klient$kat %>% replace(., is.na(.), " ")
                    numPraw=klient$numPraw %>% replace(., is.na(.), " ")
                    
                    aut<-strsplit(rezerwacja$samochod, " ")
                    aut1<-unlist(aut)
                    auto<-samochody[which(samochody$numRej==aut1[1]),] 
                    marka=auto$marka %>% replace(., is.na(.), " ")
                    model=auto$model %>% replace(., is.na(.), " ")
                    vin=auto$vin %>% replace(., is.na(.), " ")
                    numRej=auto$numRej %>% replace(., is.na(.), " ")
                    dowodRej=auto$dowod %>% replace(., is.na(.), " ")
                    paliwo=auto$paliwo %>% replace(., is.na(.), " ")
                    
                    stanPaliwa=rezerwacja$stanPaliwa %>% replace(., is.na(.), " ")
                    stanLicznika=rezerwacja$stanLicznika %>% replace(., is.na(.), " ")
                    zewnatrz=rezerwacja$zewnatrz %>% replace(., is.na(.), " ")
                    wewnatrz=rezerwacja$wewnatrz %>% replace(., is.na(.), " ")
                    
                    przekazano=rezerwacja$przekazano %>% replace(., is.na(.), "")
                    if(przekazano=="")
                    {
                        przekazano="........."
                    }
                    
                    dataOd=rezerwacja$dataOd %>% replace(., is.na(.), " ")
                    dataDo=rezerwacja$dataDo %>% replace(., is.na(.), " ")
                    godzinaOd=rezerwacja$godzinaOd %>% replace(., is.na(.), " ")
                    godzinaDo=rezerwacja$godzinaDo %>% replace(., is.na(.), " ")
                    
                    dataZawUmowy=rezerwacja$dataZawUmowy %>% replace(., is.na(.), "")
                    if(dataZawUmowy=="")
                    {
                        dataZawUmowy="........."
                    }
                    numerUmowy=rezerwacja$numerUmowy %>% replace(., is.na(.), "")
                    if(numerUmowy=="")
                    {
                        numerUmowy="........."
                    }
                    limit1=rezerwacja$limit1 %>% replace(., is.na(.), "")
                    if(limit1=="")
                    {
                        limit1="........."
                    }
                    limit2=rezerwacja$limit2 %>% replace(., is.na(.), "")
                    if(limit2=="")
                    {
                        limit2="........."
                    }
                    cena1=rezerwacja$cena1 %>% replace(., is.na(.), "")
                    if(cena1=="")
                    {
                        cena1="........."
                    }
                    jednCzasu=rezerwacja$jednCzasu %>% replace(., is.na(.), "")
                    if(jednCzasu=="")
                    {
                        jednCzasu="........."
                    }
                    cena2=rezerwacja$cena2 %>% replace(., is.na(.), "")
                    if(cena2=="")
                    {
                        cena2="........."
                    }
                    kaucja=rezerwacja$kaucja %>% replace(., is.na(.), "")
                    if(kaucja=="")
                    {
                        kaucja="........."
                    }
                    zagranica=rezerwacja$zagranica %>% replace(., is.na(.), "")
                    if(zagranica=="")
                    {
                        zagranica="........."
                    }
                    
                    
                    params <- list(Imie = imie, Nazwisko=nazwisko, Telefon=telefon, Kod=kod, Miasto=miasto, Ulica=ulica, Pesel=pesel, Dowod=dowod, KatPraw=katPraw, NumPraw=numPraw,
                                   Marka=marka, Model=model, Vin=vin, NumRej=numRej, DowodRej=dowodRej, DataOd=dataOd, DataDo=dataDo, 
                                   Paliwo=paliwo, StanPaliwa=stanPaliwa, StanLicznika=stanLicznika, Zewnatrz=zewnatrz, Wewnatrz=wewnatrz, Przekazano=przekazano, 
                                   GodzinaOdDod=godzinaOd, GodzinaDoDod=godzinaDo, DataZawUmowy=dataZawUmowy, NumerUmowy=numerUmowy,
                                   Limit1=limit1, Limit2=limit2, Cena1=cena1, JednCzasu=jednCzasu, Cena2=cena2, Kaucja=kaucja, Zagranica=zagranica)
                    
                    if(input$firmaKtora==2)
                    {
                        rmarkdown::render("RMD/automatyczny.rmd", output_file = file,
                                          params = params,
                                          envir = new.env(parent = globalenv())
                        )
                        output$pobrano<-renderText({
                            return("Pobrano umowę, sprawdź w pobranych")
                        })
                    }
                    
                    if(input$firmaKtora==1)
                    {
                        rmarkdown::render("RMD/auto2.rmd", output_file = file,
                                          params = params,
                                          envir = new.env(parent = globalenv())
                        )
                        
                    }
                }
                
                if(sum(str==("Firma:"))>0)
                {
                    podzial<-strsplit(rezerwacja$imieNazw, "Firma: ")
                    podzial2=unlist(podzial)
                    nazwa=podzial2[2]
                    firma<-firmy[which(firmy$nazwa==nazwa),]
                    
                    imie=firma$imie %>% replace(., is.na(.), " ")
                    nazwisko=firma$nazwisko %>% replace(., is.na(.), " ")
                    telefon=firma$telefon %>% replace(., is.na(.), " ")
                    dowod=firma$dowod %>% replace(., is.na(.), " ")
                    pesel=firma$pesel %>% replace(., is.na(.), " ")
                    katPraw=firma$katPraw %>% replace(., is.na(.), " ")
                    numPraw=firma$numPraw %>% replace(., is.na(.), " ")
                    
                    nazwaFir = firma$nazwa %>% replace(., is.na(.), " ")
                    nipFir = firma$nip %>% replace(., is.na(.), " ")
                    uLFir = firma$ul %>% replace(., is.na(.), " ")
                    miastoFir = firma$miejsc %>% replace(., is.na(.), " ")
                    kodFir = firma$kod %>% replace(., is.na(.), " ")
                    regonFir = firma$regon %>% replace(., is.na(.), " ")
                    
                    
                    aut<-strsplit(rezerwacja$samochod, " ")
                    aut1<-unlist(aut)
                    auto<-samochody[which(samochody$numRej==aut1[1]),]
                    marka=auto$marka %>% replace(., is.na(.), " ")
                    model=auto$model %>% replace(., is.na(.), " ")
                    vin=auto$vin %>% replace(., is.na(.), "")
                    numRej=auto$numRej %>% replace(., is.na(.), " ")
                    dowodRej=auto$dowod %>% replace(., is.na(.), " ")
                    paliwo=auto$paliwo %>% replace(., is.na(.), " ")
                    
                    stanPaliwa=rezerwacja$stanPaliwa %>% replace(., is.na(.), " ")
                    stanLicznika=rezerwacja$stanLicznika %>% replace(., is.na(.), " ")
                    zewnatrz=rezerwacja$zewnatrz %>% replace(., is.na(.), " ")
                    wewnatrz=rezerwacja$wewnatrz %>% replace(., is.na(.), " ")
                    przekazano=rezerwacja$przekazano %>% replace(., is.na(.), "")
                    if(przekazano=="")
                    {
                        przekazano="........."
                    }
                    
                    dataOd=rezerwacja$dataOd %>% replace(., is.na(.), " ")
                    dataDo=rezerwacja$dataDo %>% replace(., is.na(.), " ")
                    godzinaOd=rezerwacja$godzinaOd %>% replace(., is.na(.), " ")
                    godzinaDo=rezerwacja$godzinaDo %>% replace(., is.na(.), " ")
                    
                    dataZawUmowy=rezerwacja$dataZawUmowy %>% replace(., is.na(.), "")
                    if(dataZawUmowy=="")
                    {
                        dataZawUmowy="........."
                    }
                    numerUmowy=rezerwacja$numerUmowy %>% replace(., is.na(.), "")
                    if(numerUmowy=="")
                    {
                        numerUmowy="........."
                    }
                    
                    limit1=rezerwacja$limit1 %>% replace(., is.na(.), "")
                    if(limit1=="")
                    {
                        limit1="........."
                    }
                    limit2=rezerwacja$limit2 %>% replace(., is.na(.), "")
                    if(limit2=="")
                    {
                        limit2="........."
                    }
                    cena1=rezerwacja$cena1 %>% replace(., is.na(.), "")
                    if(cena1=="")
                    {
                        cena1="........."
                    }
                    jednCzasu=rezerwacja$jednCzasu %>% replace(., is.na(.), "")
                    if(jednCzasu=="")
                    {
                        jednCzasu="........."
                    }
                    cena2=rezerwacja$cena2 %>% replace(., is.na(.), "")
                    if(cena2=="")
                    {
                        cena2="........."
                    }
                    kaucja=rezerwacja$kaucja %>% replace(., is.na(.), "")
                    if(kaucja=="")
                    {
                        kaucja="........."
                    }
                    zagranica=rezerwacja$zagranica %>% replace(., is.na(.), "")
                    if(zagranica=="")
                    {
                        zagranica="........."
                    }
                    
                    
                    params2 <- list(NazwaFir = nazwaFir, NipFir = nipFir, ULFir = uLFir, KodFir = kodFir, MiastoFir = miastoFir, RegonFir = regonFir,
                                    Imie = imie, Nazwisko = nazwisko, Telefon = telefon, Dowod = dowod, Pesel = pesel, KatPraw = katPraw, NumPraw = numPraw,
                                    Marka=marka, Model=model, Vin=vin, NumRej=numRej, DowodRej=dowodRej, DataOd=dataOd, DataDo=dataDo, Paliwo=paliwo,
                                    StanPaliwa=stanPaliwa, StanLicznika=stanLicznika, Zewnatrz=zewnatrz, Wewnatrz=wewnatrz, Przekazano=przekazano, 
                                    GodzinaOdDod=godzinaOd, GodzinaDoDod=godzinaDo, DataZawUmowy=dataZawUmowy, NumerUmowy=numerUmowy,
                                    Limit1=limit1, Limit2=limit2, Cena1=cena1, JednCzasu=jednCzasu, Cena2=cena2, Kaucja=kaucja, Zagranica=zagranica)
                    
                    if(input$firmaKtora==2)
                    {
                        rmarkdown::render("RMD/autoFir1.rmd", output_file = file,
                                          params = params2,
                                          envir = new.env(parent = globalenv())
                        )
                    }
                    
                    if(input$firmaKtora==1)
                    {
                        rmarkdown::render("RMD/autoFir2.rmd", output_file = file,
                                          params = params2,
                                          envir = new.env(parent = globalenv())
                        )
                    }
                }
            }
            }
        }
    )
    
    
    #do kalendarza
    samochody<-loadSam()
    wyborSam<-paste(samochody$numRej, samochody$model, sep = " ")
    updateSelectInput(session, "samochod", "Wybierz samochod", choices = wyborSam)
    
    
    klienci<-loadKlient()
    klienci<-map_df(klienci, rev)
    wyborOs<-paste(klienci$imie, klienci$nazwisko, klienci$telefon, sep = " ")
    updateSelectInput(session, "osoba", "Wybierz osobę", choices = wyborOs)
    
    
    rezerwacje<-loadData()
    
    
    for (i in 1:nrow(rezerwacje)) {
        aut<-strsplit(rezerwacje$samochod[i], " ")
        aut1<-unlist(aut)
        auto<-samochody[which(samochody$numRej==aut1[1]),]
        if(nrow(auto)>0)
        {
            rezerwacje$color[i]=auto$kolor
        }
        else
            rezerwacje$color[i]="#5E97B8"
    }

    
    daneKalendarz = data.frame(title = rezerwacje$imieNazw,
                      start = rezerwacje$dataOd,
                      end = rezerwacje$dataDo,
                      color=rezerwacje$color
                      )
    #kalendarz
     output$kalendarz<-renderFullcalendar(
         
         fullcalendar(daneKalendarz, settings = list(firstDay=1))
     )
    

    
    output$samochodyKolory<-DT::renderDataTable({
        samochody<-loadSam()
        colnames(samochody)<-c("marka", "model", "nrRej", "vin", "dowod", "paliwo", "kolor")
        
        samochody$cztery<-seq(1, nrow(samochody), by=1)

        samochodyKolory<-DT::datatable(samochody[,c(2,3,7,8)], options=list(columnDefs = list(list(visible=FALSE, targets=c(3,4))), paging = FALSE, searching = FALSE, ordering=FALSE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json'))) %>% 
            formatStyle(
                "model", 'cztery',
                backgroundColor = styleEqual(samochody$cztery, samochody$kolor)) %>% 
            formatStyle(
                "nrRej", 'cztery',
                backgroundColor = styleEqual(samochody$cztery, samochody$kolor))

    })

    observeEvent(input$refreshCal, 
                 {
                     rezerwacje<-loadData()
                     samochody<-loadSam()
                    
                     for (i in 1:nrow(rezerwacje)) {
                         aut<-strsplit(rezerwacje$samochod[i], " ")
                         aut1<-unlist(aut)
                         auto<-samochody[which(samochody$numRej==aut1[1]),]
                         if(nrow(auto)>0)
                         {
                             rezerwacje$color[i]=auto$kolor
                         }
                         else
                             rezerwacje$color[i]="#5E97B8"
                     }
                     
                     daneKalendarz = data.frame(title = rezerwacje$imieNazw,
                                                start = rezerwacje$dataOd,
                                                end = rezerwacje$dataDo,
                                                color=rezerwacje$color
                     )
                     #kalendarz
                     output$kalendarz<-renderFullcalendar(
                         fullcalendar(daneKalendarz)
                     )
                     
                     output$samochodyKolory<-DT::renderDataTable({
                         colnames(samochody)<-c("marka", "model", "nrRej", "vin", "dowod", "paliwo", "kolor")
                         
                         samochody$cztery<-seq(1, nrow(samochody), by=1)
                         
                         samochodyKolory<-DT::datatable(samochody[,c(2,3,7,8)], options=list(columnDefs = list(list(visible=FALSE, targets=c(3,4))), paging = FALSE, searching = FALSE, ordering=FALSE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json'))) %>% 
                             formatStyle(
                                 "model", 'cztery',
                                 backgroundColor = styleEqual(samochody$cztery, samochody$kolor)) %>% 
                             formatStyle(
                                 "nrRej", 'cztery',
                                 backgroundColor = styleEqual(samochody$cztery, samochody$kolor))
                         
                     })
                 })
    
    ##Klienci
    output$klienci<-DT::renderDataTable({
        
        klienci<-loadKlient()
        klienci<-map_df(klienci, rev)
        klienci<-datatable(klienci, editable = TRUE, selection='single', options = list(autoWidth = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
    })
    
    observeEvent(input$klienci_cell_edit, {
        klienci<-loadKlient()
        info = input$klienci_cell_edit
        klienci<-map_df(klienci, rev)
        klienciZm <- editData(klienci, info)
        klienci<-map_df(klienciZm, rev)
        saveKlient(klienci)
    })
    
    observeEvent(input$nowyKlient,{
        
        showModal(
            modalDialog(
                        
                        textInput(inputId="imieNowy", label = ("Imię")),
                        textInput(inputId="nazwiskoNowy", label = ("Nazwisko")),
                        textInput(inputId="telefonNowy", label = ("Numer telefonu")),
                        textInput(inputId="kodNowy", label = ("Kod pocztowy")),
                        textInput(inputId="miastoNowy", label = ("Miasto")),
                        textInput(inputId="ulicaNowy", label = ("Ulica i numer")),
                        textInput(inputId="dowodNowy", label = ("Seria i numer dowodu")),
                        textInput(inputId="peselNowy", label = ("PESEL")),
                        textInput(inputId="katPrawNowy", label = ("Kategoria prawa jazdy")),
                        textInput(inputId="numPrawNowy", label = ("Numer prawa jazdy")),
                        
                        selectInput(inputId = "klientFirmaNowy",
                                    label = "Wybierz firmę:",
                                    choices = c("",wyborFirm), selected = ""),
                        actionButton(inputId="zatwierdzNowyKlient", label = "Zatwierdź", icon("plus"), 
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        span(textOutput("sukcesNowyKl"), style="color:green"),
                        
                        title = "Nowy klient", 
                        
                        size = "s", easyClose = TRUE, footer = modalButton(label = "Zamknij")
            )
        )
        
    })
    
    observeEvent(input$zatwierdzNowyKlient, {
        req(input$telefonNowy)
        
        klienci<-loadKlient()
        

        if(input$telefonNowy %in% klienci$telefon)
        {
            shinyalert("Numer telefonu!", "Klient z takim numerem telefonu jest już w bazie", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, timer = 5000)
            
        }
        else
        {   
            n<-(nrow(klienci)+1)
            
            klienci[n,1]=gsub(" ", "",input$imieNowy)
            klienci[n,2]=gsub(" ", "",input$nazwiskoNowy)
            klienci[n,3]=gsub(" ", "",input$telefonNowy)
            klienci[n,4]=input$kodNowy
            klienci[n,5]=input$miastoNowy
            klienci[n,6]=input$ulicaNowy
            klienci[n,7]=input$dowodNowy
            klienci[n,8]=input$peselNowy
            klienci[n,9]=input$katPrawNowy
            klienci[n,10]=input$numPrawNowy
            klienci[n,11]=input$klientFirmaNowy
            
            if(input$klientFirmaNowy=="")
            {
                saveKlient(klienci)
            }
            else
            {
                firmy<-loadFir()
                z=input$klientFirmaNowy
                p=gsub("\\Firma: ", "",z)
                ktore=which(firmy$nazwa==p)
                
                firmy[ktore,"imie"]<-input$imieNowy
                firmy[ktore,"nazwisko"]<-input$nazwiskoNowy
                firmy[ktore,"telefon"]<-input$telefonNowy
                firmy[ktore,"dowod"]<-input$dowodNowy
                firmy[ktore,"pesel"]<-input$peselNowy
                firmy[ktore,"katPraw"]<-input$katPrawNowy
                firmy[ktore,"numPraw"]<-input$numPrawNowy
                
                saveKlient(klienci)
                saveFir(firmy)
            }
            
            output$sukcesNowyKl<-renderText({
                return("Dodano klienta")
            })
            
            output$klienci<-DT::renderDataTable({
                klienci<-loadKlient()
                klienci<-map_df(klienci, rev)
                klienci<-datatable(klienci, editable = TRUE, selection='single', options = list(autoWidth = TRUE,  language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
            }) 
            
            klienci<-map_df(klienci, rev)
            wyborOs<-paste(klienci$imie, klienci$nazwisko, klienci$telefon, sep = " ")
            
            updateSelectInput(session, "osoba", "Wybierz osobę", choices = wyborOs)
            updateSelectInput(session, "osobaDod", "Wybierz osobę", choices = wyborOs)
            
            shinyjs::reset("imieNowy")
            shinyjs::reset("nazwiskoNowy")
            shinyjs::reset("telefonNowy")
            shinyjs::reset("kodNowy")
            shinyjs::reset("miastoNowy")
            shinyjs::reset("ulicaNowy")
            shinyjs::reset("dowodNowy")
            shinyjs::reset("peselNowy")
            shinyjs::reset("katPrawNowy")
            shinyjs::reset("numPrawNowy")
            shinyjs::reset("klientFirmaNowy")
            
            removeModal(session)
            
        }
        
        
    })
    
    observeEvent(input$zatwierdz, {
        req(input$telefon)
        
        klienci<-loadKlient()
        
        if(input$telefon %in% klienci$telefon)
        {
            showModal(modalDialog(
                title = "Numer telefonu w bazie",
                textInput(inputId="imieDupl", label = ("Imię")),
                textInput(inputId="nazwiskoDupl", label = ("Nazwisko")),
                textInput(inputId="telefonDupl", label = ("Numer telefonu")),
                textInput(inputId="kodDupl", label = ("Kod pocztowy")),
                textInput(inputId="miastoDupl", label = ("Miasto")),
                textInput(inputId="ulicaDupl", label = ("Ulica i numer")),
                textInput(inputId="dowodDupl", label = ("Seria i numer dowodu")),
                textInput(inputId="peselDupl", label = ("PESEL")),
                textInput(inputId="katPrawDupl", label = ("Kategoria prawa jazdy")),
                textInput(inputId="numPrawDupl", label = ("Numer prawa jazdy")),
                actionButton(inputId="zatwierdzDupl", label = "Zatwierdź", icon("plus"), width = '75%',
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                
                size = "s", easyClose = TRUE, footer = modalButton(label = "Zamknij"))
            )
            
            klient<-klienci[klienci$telefon==input$telefon,]
            updateTextInput(session, "imieDupl", value = input$imie)
            updateTextInput(session, "nazwiskoDupl", value = input$nazwisko)
            updateTextInput(session, "telefonDupl", value = input$telefon)
            updateTextInput(session, "kodDupl", value = input$kod)
            updateTextInput(session, "miastoDupl", value = input$miasto)
            updateTextInput(session, "ulicaDupl", value = input$ulica)
            updateTextInput(session, "dowodDupl", value = input$dowod)
            updateTextInput(session, "peselDupl", value = input$pesel)
            updateTextInput(session, "katPrawDupl", value = input$katPraw)
            updateTextInput(session, "numPrawDupl", value = input$numPraw)
            
            
            observeEvent(input$zatwierdzDupl,{
                ktory<-which(klienci$telefon==input$telefon)
                klienci$imie[ktory]<-input$imieDupl
                klienci$nazwisko[ktory]<-input$nazwiskoDupl
                klienci$telefon[ktory]<-input$telefonDupl
                klienci$miasto[ktory]<-input$miastoDupl
                klienci$kod[ktory]<-input$kodDupl
                klienci$ulica[ktory]<-input$ulicaDupl
                klienci$dowod[ktory]<-input$dowodDupl
                klienci$pesel[ktory]<-input$peselDupl
                klienci$kat[ktory]<-input$katPrawDupl
                klienci$numPraw[ktory]<-input$numPrawDupl
                
                saveKlient(klienci)
                
                removeModal()
                
                shinyjs::reset("imie")
                shinyjs::reset("nazwisko")
                shinyjs::reset("telefon")
                shinyjs::reset("kod")
                shinyjs::reset("miasto")
                shinyjs::reset("ulica")
                shinyjs::reset("dowod")
                shinyjs::reset("pesel")
                shinyjs::reset("katPraw")
                shinyjs::reset("numPraw")
                shinyjs::reset("klientFirma")
                
                output$klienci<-DT::renderDataTable({
                    klienci<-map_df(klienci, rev)
                    klienci<-datatable(klienci, editable = TRUE, selection='single', options = list(autoWidth = TRUE,  language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
                })
            })
            
            }
        else
            {   
            n<-(nrow(klienci)+1)

            klienci[n,1]=gsub(" ", "",input$imie)
            klienci[n,2]=gsub(" ", "",input$nazwisko)
            klienci[n,3]=gsub(" ", "",input$telefon)
            klienci[n,4]=input$kod
            klienci[n,5]=input$miasto
            klienci[n,6]=input$ulica
            klienci[n,7]=input$dowod
            klienci[n,8]=input$pesel
            klienci[n,9]=input$katPraw
            klienci[n,10]=input$numPraw
            klienci[n,11]=input$klientFirma
            
            if(input$klientFirma=="")
            {
                saveKlient(klienci)
            }
            else
            {
                firmy<-loadFir()
                z=input$klientFirma
                p=gsub("\\Firma: ", "",z)
                ktore=which(firmy$nazwa==p)
                
                firmy[ktore,"imie"]<-input$imie
                firmy[ktore,"nazwisko"]<-input$nazwisko
                firmy[ktore,"telefon"]<-input$telefon
                firmy[ktore,"dowod"]<-input$dowod
                firmy[ktore,"pesel"]<-input$pesel
                firmy[ktore,"katPraw"]<-input$katPraw
                firmy[ktore,"numPraw"]<-input$numPraw
                
                saveKlient(klienci)
                saveFir(firmy)
            }
            
            output$dodaj<-renderText({
                return("Dodano klienta")
            })
            show("dodaj")
            delay(6000, hide("dodaj"))
            
            output$klienci<-DT::renderDataTable({
                klienci<-loadKlient()
                klienci<-map_df(klienci, rev)
                klienci<-datatable(klienci, editable = TRUE, selection='single', options = list(autoWidth = TRUE,  language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
            }) 
            
            klienci<-map_df(klienci, rev)
            wyborOs<-paste(klienci$imie, klienci$nazwisko, klienci$telefon, sep = " ")
            
            updateSelectInput(session, "osoba", "Wybierz osobę", choices = wyborOs)
            updateSelectInput(session, "osobaDod", "Wybierz osobę", choices = wyborOs)
            
            shinyjs::reset("imie")
            shinyjs::reset("nazwisko")
            shinyjs::reset("telefon")
            shinyjs::reset("kod")
            shinyjs::reset("miasto")
            shinyjs::reset("ulica")
            shinyjs::reset("dowod")
            shinyjs::reset("pesel")
            shinyjs::reset("katPraw")
            shinyjs::reset("numPraw")
            shinyjs::reset("klientFirma")
        }
        
        
    })
   
    
    output$sprawdzenieTel<-renderText({
        if(input$telefon=="")
        {
            return("Musisz podać telefon!")
        }
        if(sum(klienci$telefon==input$telefon)>0)
        {
            return("Każdy klient musi mieć inny numer telefonu")
        }
        if(str_detect(input$telefon, " ")==TRUE)
        {
            return("Numer telefonu nie może zawierać spacji!")
        }
    })
    
    
    observeEvent(input$deleteKl, 
                 {
                     klienci<-loadKlient()
                     klienci<-map_df(klienci, rev)
                     
                     if(!is.null(input$klienci_rows_selected)){
                         
                         confirmSweetAlert(
                             session = session,
                             inputId = "klientUsunPotwierdz",
                             title = "Czy potwierdzasz usunięcie klienta?",
                             text = paste(klienci$imie[as.numeric(input$klienci_rows_selected)],
                                          klienci$nazwisko[as.numeric(input$klienci_rows_selected)],
                                          klienci$telefon[as.numeric(input$klienci_rows_selected)], sep = " "), 
                             btn_labels = c("Anuluj", "Potwierdz"),
                             closeOnClickOutside = FALSE
                         )
                         
                         }
                     })
    
    
    observeEvent(input$klientUsunPotwierdz,
                 {
                     if(isTRUE(input$klientUsunPotwierdz))
                     {
                         klienci<-loadKlient()
                         klienci<-map_df(klienci, rev)
                         klienci<-klienci[-as.numeric(input$klienci_rows_selected),]
                         klienci<-map_df(klienci, rev)
                         saveKlient(klienci)
                         output$usun<-renderText({
                             return("Usunięto klienta")
                         })
                         
                         show("usun")
                         delay(6000, hide("usun"))
                         
                         output$klienci<-renderDataTable(
                             {
                                 #klienci<-loadKlient()
                                 klienci<-map_df(klienci, rev)
                                 klienci<-datatable(klienci, editable = TRUE, selection='single', options = list(autoWidth = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
                             }
                         )
                         updateCheckboxInput(session, "klientUsunPotwierdz", value = 0)
                         wyborOs<-paste(klienci$imie, klienci$nazwisko, klienci$telefon, sep = " ")
                         updateSelectInput(session, "osoba", "Wybierz osobę", choices = wyborOs)
                         updateSelectInput(session, "osobaDod", "Wybierz osobę", choices = wyborOs)
                     }
                     
                 }
    )
    
    observeEvent(input$edytujKlient, {
        klienci<-loadKlient()
        klienci<-map_df(klienci, rev)
        if(!is.null(input$klienci_rows_selected))
        {
            klient<-klienci[(input$klienci_rows_selected),]
            
            updateTextInput(session, "imieEd", value = klient$imie)
            updateTextInput(session, "nazwiskoEd", value = klient$nazwisko)
            updateTextInput(session, "telefonEd", value = klient$telefon)
            updateTextInput(session, "miastoEd", value = klient$miasto)
            updateTextInput(session, "kodEd", value = klient$kod)
            updateTextInput(session, "ulicaEd", value = klient$ulica)
            updateTextInput(session, "dowodEd", value = klient$dowod)
            updateTextInput(session, "peselEd", value = klient$pesel)
            updateTextInput(session, "katPrawEd", value = klient$kat)
            updateTextInput(session, "numPrawEd", value = klient$numPraw)
            selectInput(inputId = "klientFirmaEd",
                        label = "Wybierz firmę:",
                        choices = c("",wyborFirm), selected = "")
            
        }
    })
    
    observeEvent(input$zatwierdzEdycja, 
                 {
                     firmy<-loadFir()
                     #klient<-loadKlient()
                     #klienci<-map_df(klienci, rev)
                     
                     if(!is.null(input$klienci_rows_selected))
                     {
                         
                         klienci$imie[(input$klienci_rows_selected)]<-input$imieEd
                         klienci$nazwisko[(input$klienci_rows_selected)]<-input$nazwiskoEd
                         klienci$telefon[(input$klienci_rows_selected)]<-input$telefonEd
                         klienci$miasto[(input$klienci_rows_selected)]<-input$miastoEd
                         klienci$kod[(input$klienci_rows_selected)]<-input$kodEd
                         klienci$ulica[(input$klienci_rows_selected)]<-input$ulicaEd
                         klienci$dowod[(input$klienci_rows_selected)]<-input$dowodEd
                         klienci$pesel[(input$klienci_rows_selected)]<-input$peselEd
                         klienci$kat[(input$klienci_rows_selected)]<-input$katPrawEd
                         klienci$numPraw[(input$klienci_rows_selected)]<-input$numPrawEd
                         klienci$NIP[(input$klienci_rows_selected)]<-input$klientFirmaEd
                         
                         if(input$klientFirmaEd=="")
                         {
                             klienci<-map_df(klienci, rev)
                             saveKlient(klienci)
                         }
                         else
                         {
                             firmy<-loadFir()
                             z=input$klientFirmaEd
                             #z="Firma: ANOVA Jakub Augustynek"
                             p=gsub("\\Firma: ", "",z)
                             ktore=which(firmy$nazwa==p)
                             
                             firmy[ktore,"imie"]<-input$imieEd
                             firmy[ktore,"nazwisko"]<-input$nazwiskoEd
                             firmy[ktore,"telefon"]<-input$telefonEd
                             firmy[ktore,"dowod"]<-input$dowodEd
                             firmy[ktore,"pesel"]<-input$peselEd
                             firmy[ktore,"katPraw"]<-input$katPrawEd
                             firmy[ktore,"numPraw"]<-input$numPrawEd
                             
                             saveFir(firmy)
                         }
                     }
                     
                     output$sukcesEdycja<-renderText({
                         return("Zmieniono dane klienta")
                     })
                     
                     show("sukcesEdycja")
                     delay(6000,hide("sukcesEdycja"))
                     
                     output$klienci<-renderDataTable(
                         {
                             klienci<-loadKlient()
                             klienci<-map_df(klienci, rev)
                             klienci<-datatable(klienci, editable = TRUE, selection='single', options = list(autoWidth = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
                         }
                     )
                 })
    
    
    
   i=0
    ###firmy
    
    observeEvent((input$GUS),
                 {
                         # ------------------ App virtualenv setup (Do not edit) ------------------- #
                         
                     withProgress(message = 'Ładowanie wyszukiwarki', value = 0, {

                         virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
                         python_path = Sys.getenv('PYTHON_PATH')
                         
                         incProgress(1/10)
                         
                         # Create virtual env and install dependencies
                         #install.packages("reticulate")
                         reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
                         incProgress(2/10)
                         #reticulate::virtualenv_remove(envname = virtualenv_dir, packages = "pip")
                         incProgress(3/10)
                         reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES)
                         reticulate::use_virtualenv(virtualenv_dir, required = T)
                         incProgress(4/10)
                         
                         # ------------------ App server logic (Edit anything below) --------------- #
                         
                         
                         # Import python functions to R
                         #py_install("numpy", envname = VIRTUALENV_NAME)
                         py_install("virtualenv", envname = VIRTUALENV_NAME)
                         reticulate::use_virtualenv(virtualenv_dir, required = T)
                         
                         incProgress(5/10)
                         
                         #reticulate::import_from_path(module = numpy, path = '//Users/kuba/.virtualenvs/example_env_name/lib/python3.8/site-packages/numpy')
                         #reticulate::use_python("//Users/kuba/Desktop/shiny/pythonshiny/example_env_name/bin/python")
                         py_install("litex.regon", envname = virtualenv_dir)
                         py_install("REGONAPI", envname = virtualenv_dir)
                         incProgress(7/10)
                         source_python('szukaj.py')
                         incProgress(9/10)
                         
                         output$wyszukiwarka<-renderText({
                             return("Wyszukiwarka włączona")
                         })
                         
                     })
                         
                         observeEvent(input$szukajFir,
                                      {
                                          source_python('szukaj.py')
                                          nip=gsub("\\ ", "", gsub("\\-", "", input$nipFir))
                                         
                                          
                                          dlugosc<-str_count(nip, "[0-9]")
                                          if(dlugosc==10)
                                          {
                                              tryCatch(
                                                  {
                                                      updateTextInput(session, "nipFir", value = nip)
                                                      
                                                      x=szukajNIP2(nip)
                                                      
                                                      nazwaFir=as.character(x[[1]])
                                                      updateTextInput(session, "nazwa", value = nazwaFir)
                                                      
                                                      if(x[[4]]=="")
                                                      {
                                                          ulica=as.character(paste(x[[2]], x[[3]], x[[4]], sep = " "))
                                                      }
                                                      else
                                                      {
                                                          ulica=as.character(paste(x[[2]], x[[3]], "/", x[[4]], sep = " "))
                                                      }
                                    
                                                      updateTextInput(session, "ulicaFir", value = ulica)
                                                      
                                                      miejscowoscFir=as.character(x[[5]])
                                                      updateTextInput(session, "miejscowoscFir", value = miejscowoscFir)
                                                      
                                                      KodFir=as.character(x[[6]])
                                                      updateTextInput(session, "KodFir", value = KodFir)
                                                      
                                                      regon=as.character(x[[7]])
                                                      updateTextInput(session, "regon", value = regon)
                                                      
                                                      email=as.character(x[[8]])
                                                      updateTextInput(session, "email", value = email)
                                                      
                                                      telefon=as.character(x[[9]])
                                                      updateTextInput(session, "telefonFir", value = telefon)
                                                      
                                                      
                                                      output$sukcesNIP<-renderText({
                                                          return("Znaleziono firmę")
                                                      })
                                                      
                                                      show("sukcesNIP")
                                                      
                                                      delay(6000,hide("sukcesNIP"))
                                                      
                                                  }, error =  function(cond)
                                                  {
                                                      output$blednyNIP<-renderText({
                                                          return("Wpisano błędny numer NIP")
                                                      })
                                                      
                                                      show("blednyNIP")
                                                      delay(6000,hide("blednyNIP"))
                                                  }
                                                  
                                              )
                                          }
                                      })
                         
                 })
    
    
    
    
    
    
    observeEvent(input$dodajFir, {
        firmy<-loadFir()
        firmy<-as.data.frame(firmy)
        firmy<-firmy %>% replace(., is.na(.), "")
        
        ## zmiana 1
        nipFirmy=input$nipFir
        
        if(nipFirmy %in% firmy$nip)
        {
            shinyalert("Firma!", "Firma z takim numerem NIP jest już w bazie \n Usuń firmę, lub edytuj jej dane", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, timer = 5000)
        }else
        {
            n<-(nrow(firmy)+1)
            firmy[n,1]=input$nipFir
            firmy[n,2]=input$nazwa
            firmy[n,3]=input$ulicaFir
            firmy[n,4]=input$miejscowoscFir
            firmy[n,5]=input$KodFir
            firmy[n,6]=input$regon
            firmy[n,7]=input$email
            firmy[n,8]=input$telefonFir
            
            firmy %>% replace(., is.na(.), "")
            saveFir(firmy)
            
            
            output$firmy<-DT::renderDataTable({
                firmy<-loadFir()
                firmy<-map_df(firmy, rev)
                firmy<-as.data.frame(firmy)
                
                if(nrow(firmy)>0)
                {
                    firmy<-firmy[,c(1:8)]
                    firmy<-datatable(firmy, editable = TRUE, selection='single',options = list(autoWidth = TRUE, pageLength=15, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
                    
                }
            })
            
            firmy<-map_df(firmy, rev)
            wyborFirm<- paste("Firma:", firmy$nazwa, sep = " ")
            updateSelectInput(session, "wybFirma", "Wybierz firmę", choices = wyborFirm)
            updateSelectInput(session, "wybFirmaDod", "Wybierz firmę", choices = wyborFirm)
            
            
            
            shinyjs::reset("nipFir")
            shinyjs::reset("nazwa")
            shinyjs::reset("ulicaFir")
            shinyjs::reset("miejscowoscFir")
            shinyjs::reset("KodFir")
            shinyjs::reset("regon")
            shinyjs::reset("email")
            shinyjs::reset("telefonFir")
            
        }
        
        
        
    })
    
    observeEvent(input$usunFir, 
                 {
                     firmy<-loadFir()
                     firmy<-map_df(firmy, rev)
                     firmy<-as.data.frame(firmy)
                     
                     if(!is.null(input$firmy_rows_selected))
                     {
                         confirmSweetAlert(
                             session = session,
                             inputId = "usunFirPotwierdz",
                             title = "Czy potwierdzasz usunięcie firmy?",
                             text = paste(firmy$nazwa[as.numeric(input$firmy_rows_selected)],
                                          firmy$nip[as.numeric(input$firmy_rows_selected)], sep = " "), 
                             btn_labels = c("Anuluj", "Potwierdz")
                         )
                         
                     }
                     
                     
                     
                 })
    
    observeEvent(input$usunFirPotwierdz,
                 if(isTRUE(input$usunFirPotwierdz))
                 {
                     firmy<-loadFir()
                     firmy<-map_df(firmy, rev)
                     firmy<-as.data.frame(firmy)
                     
                     firmy<-firmy[-as.numeric(input$firmy_rows_selected),]
                     firmy<-map_df(firmy, rev)
                     saveFir(firmy)
                     
                     firmy<-map_df(firmy, rev)
                     
                     output$firmy<-DT::renderDataTable({
                         #firmy<-loadFir()
                         if(nrow(firmy)>0)
                         {
                             firmy<-firmy[,c(1:8)]
                             firmy<-datatable(firmy, editable = TRUE, selection='single', options = list(autoWidth = TRUE, pageLength=15, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
                             
                         }
                     })
                     
                     
                     #firmy<-map_df(firmy, rev)
                     wyborFirm<- paste("Firma:", firmy$nazwa, sep = " ")
                     updateSelectInput(session, "wybFirma", "Wybierz firmę", choices = wyborFirm)
                     updateSelectInput(session, "wybFirmaDod", "Wybierz firmę", choices = wyborFirm)
                     
                 }
                 
    )
    
    observeEvent(input$dodajReprez, {
        firmy<-loadFir()
        firmy<-map_df(firmy, rev)
        firmy<-as.data.frame(firmy)
        klienci<-loadKlient()
        if(!is.null(input$firmy_rows_selected))
        {
            firma<-firmy[(input$firmy_rows_selected),]
            

            updateTextInput(session, "imieRep", value = firma$imie)
            updateTextInput(session, "nazwiskoRep", value = firma$nazwisko)
            updateTextInput(session, "telefonRep", value = firma$telefon)
            updateTextInput(session, "dowodRep", value = firma$dowod)
            updateTextInput(session, "peselRep", value = firma$pesel)
            updateTextInput(session, "katPrawRep", value = firma$katPraw)
            updateTextInput(session, "numPrawRep", value = firma$numPraw)
            
        }
    })
    
    
    
    
    output$firmy<-DT::renderDataTable({
        firmy<-loadFir()
        firmy<-map_df(firmy, rev)
        firmy<-as.data.frame(firmy)
        if(nrow(firmy)>0)
        {
            firmy<-firmy[,c(1:8)]
            firmy<-datatable(firmy, editable = TRUE, selection='single', options = list(autoWidth = TRUE, pageLength=15, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
            
        }
    })
    
    observeEvent(input$firmy_cell_edit, {
        firmy<-loadFir()
        info = input$firmy_cell_edit
        firmy<-map_df(firmy, rev)
        firmy <- editData(firmy, info)
        firmy<-map_df(firmy, rev)
        saveFir(firmy)
    })
    
    
    ###samochody

    output$samochody<-DT::renderDataTable({

        colnames(samochody)<-c("marka", "model", "nrRej", "vin", "dowod", "paliwo", "kolor")
        
        samochody$cztery<-seq(1, nrow(samochody), by=1)
        
        samochody<-DT::datatable(samochody, editable = TRUE, 
                                 options = list(autoWidth = TRUE, columnDefs = list(list(visible=FALSE, targets=c(8))), 
                                                pageLength=15, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json'))) %>%  
            formatStyle(
                "kolor", 'cztery',
                backgroundColor = styleEqual(samochody$cztery, samochody$kolor)) 
        
    })
    
    
    observeEvent(input$samochody_cell_edit, {
        samochody <- loadSam()
        info = input$samochody_cell_edit
        str(info) 
        samochody <<- editData(samochody, info)
        saveSam(samochody)
    })
    
    observeEvent(input$zatwierdz2, {
        
        samochody<-loadSam()
        
        req(input$numRej)
        c<-samochody$numRej==input$numRej
        x=sum(c)
        
        if(x==0)
        {
            n<-(nrow(samochody)+1)
            samochody[n,1]=input$marka
            samochody[n,2]=input$model
            samochody[n,3]=input$numRej
            samochody[n,4]=input$vin
            samochody[n,5]=input$dowodRej
            samochody[n,6]=input$paliwo
            samochody[n,7]=input$col
            
            saveSam(samochody)
            output$dodajSam<-renderText({
                return("Dodano samochód")
            })
            
            show("dodajSam")
            delay(6000, hide("dodajSam"))
            
            output$samochody<-DT::renderDataTable({
                samochody<-loadSam()
                colnames(samochody)<-c("marka", "model", "nrRej", "vin", "dowod", "paliwo", "kolor")
                
                samochody$cztery<-seq(1, nrow(samochody), by=1)
                
                samochody<-DT::datatable(samochody, editable = TRUE, options = list(autoWidth = TRUE, columnDefs = list(list(visible=FALSE, targets=c(8))), pageLength=15, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json'))) %>%  
                    formatStyle(
                        "kolor", 'cztery',
                        backgroundColor = styleEqual(samochody$cztery, samochody$kolor)) 
                })
            
        }
        shinyjs::reset("marka")
        shinyjs::reset("model")
        shinyjs::reset("numRej")
        shinyjs::reset("vin")
        shinyjs::reset("dowodRej")
        shinyjs::reset("paliwo")
        
        wyborSam<-paste(samochody$numRej, samochody$model, sep = " ")
        updateSelectInput(session, "samochod", "Wybierz samochod", choices = wyborSam)
        updateSelectInput(session, "samochodDod", "Wybierz samochod", choices = wyborSam)
    })
    
    observeEvent(input$edytujSam, {
        samochody<-loadSam()
        if(!is.null(input$samochody_rows_selected))
        {
            samochod<-samochody[(input$samochody_rows_selected),]
            
            updateTextInput(session, "markaEd", value = samochod$marka)
            updateTextInput(session, "modelEd", value = samochod$model)
            updateTextInput(session, "numRejEd", value = samochod$numRej)
            updateTextInput(session, "vinEd", value = samochod$vin)
            updateTextInput(session, "dowRejEd", value = samochod$dowod)
            updateTextInput(session, "paliwoEd", value = samochod$paliwo)
            updateTextInput(session, "kolorEd", value = samochod$kolor)
        }
    })
    

    observeEvent(input$zatwierdzEdycjaSam, 
                 {
                     
                     if(!is.null(input$samochody_rows_selected))
                     {
                         samochody<-loadSam()
                         rowNum <- input$samochody_rows_selected
                         
                         samochody$marka[rowNum]       <- input$markaEd
                         samochody$model[rowNum]       <- input$modelEd
                         samochody$numRej[rowNum]      <- input$numRejEd
                         samochody$vin[rowNum]         <- input$vinEd
                         samochody$dowod[rowNum]       <- input$dowRejEd
                         samochody$paliwo[rowNum]      <- input$paliwoEd
                         samochody$kolor[rowNum]       <- input$kolorEd
                         samochody$numRejPoprz[rowNum] <- input$numRejPoprz
                         
                         
                         saveSam(samochody)
                         
                     }
                     
                     output$sukcesEdycjaSam<-renderText({
                         return("Zmieniono dane samochodu")
                     })
                     
                     show("sukcesEdycjaSam")
                     delay(6000,hide("sukcesEdycjaSam"))
                     
                     output$samochody<-renderDataTable({
                         samochody<-loadSam()
                         colnames(samochody)<-c("marka", "model", "nrRej", "vin", "dowod", "paliwo", "kolor")
                         
                         samochody$cztery<-seq(1, nrow(samochody), by=1)
                         
                         samochody<-DT::datatable(samochody, editable = TRUE, options = list(autoWidth = TRUE, columnDefs = list(list(visible=FALSE, targets=c(8))), pageLength=15, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json'))) %>%  
                             formatStyle(
                                 "kolor", 'cztery',
                                 backgroundColor = styleEqual(samochody$cztery, samochody$kolor)) 
                     })
                     wyborSam<-paste(samochody$numRej, samochody$model, sep = " ")
                     updateSelectInput(session, "samochod", "Wybierz samochod", choices = wyborSam)
                     updateSelectInput(session, "samochodDod", "Wybierz samochod", choices = wyborSam)
                 })
    
    observeEvent(input$deleteSam, 
                 {
                     samochody<-loadSam()
                     if(!is.null(input$samochody_rows_selected))
                     {
                         confirmSweetAlert(
                         session = session,
                         inputId = "usunSamPotwierdz",
                         title = "Czy potwierdzasz usunięcie samochodu?",
                         text = paste(samochody$marka[as.numeric(input$samochody_rows_selected)],
                                      samochody$model[as.numeric(input$samochody_rows_selected)],
                                      samochody$numRej[as.numeric(input$samochody_rows_selected)], sep = " "), 
                         btn_labels = c("Anuluj", "Potwierdz")
                     )
                     }
                 })
    
    observeEvent(input$usunSamPotwierdz,
                 if(isTRUE(input$usunSamPotwierdz))
                 {
                     samochody <- loadSam()
                     
                     samochody<-samochody[-as.numeric(input$samochody_rows_selected),]
                     saveSam(samochody)
                     
                     
                     output$samochody<-renderDataTable({
                         samochody<-loadSam()
                         colnames(samochody)<-c("marka", "model", "nrRej", "vin", "dowod", "paliwo", "kolor")
                         
                         samochody$cztery<-seq(1, nrow(samochody), by=1)
                         
                         samochody<-DT::datatable(samochody, editable = TRUE, options = list(autoWidth = TRUE, columnDefs = list(list(visible=FALSE, targets=c(8))), pageLength=15, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json'))) %>%  
                             formatStyle(
                                 "kolor", 'cztery',
                                 backgroundColor = styleEqual(samochody$cztery, samochody$kolor)) 
                     })
                     wyborSam<-paste(samochody$numRej, samochody$model, sep = " ")
                     updateSelectInput(session, "samochod", "Wybierz samochod", choices = wyborSam)
                     updateSelectInput(session, "samochodDod", "Wybierz samochod", choices = wyborSam)
                     
                 }
                 
    )
    

    
    output$sprawdzenieAut<-renderText({
        if(input$numRej=="")
        {
            return("Musisz podać numer rejestracyjny!")
        }
        if(sum(samochody$numRej==input$numRej)>0)
        {
            return("Każdy samochód musi mieć inny numer rejestracyjny!")
        }
        if(str_detect(input$numRej, " ")==TRUE)
        {
            return("Numer rejestracyjny nie może zawierać spacji!")
        }
    })
    
    observeEvent(input$szukaj, 
                 {
                     if(input$zakresOd<=input$zakresDo)
                     {
                      rezerwacje<-loadData()   
                         if((input$samochod1)=="wszystkie")
                         {
                             
                             zakres<-which((rezerwacje$dataOd>=input$zakresOd)&(rezerwacje$dataOd<=input$zakresDo))

                             wybrane<-rezerwacje[zakres,]
                             
                             wybrane$kwota<-as.numeric(wybrane$kwota)

                             
                             output$wykresZarobki<-renderPlot({
                                     theme_set(theme_minimal())
                                     ggplot(wybrane, aes(x=samochod, y=kwota, fill=samochod, col=kwota))+
                                         geom_bar(stat = "identity")+guides(col=FALSE)+
                                         ggtitle("Wykres zarobków z danego okresu       
                                                                     
                                                                     ")+ 
                                         stat_summary(aes(label = stat(y)), fun = 'sum', geom = 'text', col = 'black', vjust = -0.5) +
                                         
                                         theme(plot.title = element_text(size=34))+
                                         theme(legend.title = element_text(size = 22),
                                               legend.text = element_text(size = 20),
                                               axis.text.x=element_blank(),
                                               axis.text.y=element_text(size=18,face="bold"),
                                               axis.title=element_text(size=18,face="bold"))
                                 }, height=450, width=800)
                             
                             
                             output$tabelaZar<-renderDataTable({
                                     tabelaZaro<-data.frame("Data od"=wybrane$dataOd, "Data do"=wybrane$dataDo, "Osoba"= wybrane$imieNazw, "auto"=wybrane$samochod, "kwota"= wybrane$kwota)
                                     tabelaZar<-datatable(tabelaZaro, options = list(autoWidth = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
                                     })

                         }
                         else
                             {
                                 wyborS1<-input$samochod1
                                 if(!is.null(input$innySam)){
                                     inny <- unlist(strsplit(gsub(" ", "", input$innySam), ","))
                                     wyborS1 <- c(wyborS1, inny)
                                 }
                                 
                                 zakres<-list()
                                 
                                 for(sam in wyborS1){
                                     zakres <- append(zakres,which((rezerwacje$dataOd>=input$zakresOd)&(rezerwacje$dataOd<=input$zakresDo)&(word(rezerwacje$samochod,1)==word(sam,1))))
                                 }
                                 
                                 zakres<-unlist(zakres)
                                 wybrane<-rezerwacje[zakres,]
                                 wybrane$kwota<-as.numeric(wybrane$kwota)
                                 
                                 output$wykresZarobki<-renderPlot({
                                     theme_set(theme_minimal())
                                     ggplot(wybrane, aes(x=samochod, y=kwota, fill=samochod, col=kwota))+
                                     geom_bar(stat = "identity")+guides(col=FALSE)+
                                     ggtitle("Wykres zarobków z danego okresu       
                                                                     
                                                                     ")+ 
                                     stat_summary(aes(label = stat(y)), fun = 'sum', geom = 'text', col = 'black', vjust = -0.5) +
                                     
                                     theme(plot.title = element_text(size=34))+
                                     theme(legend.title = element_text(size = 22),
                                           legend.text = element_text(size = 20),
                                           axis.text.x=element_blank(),
                                           axis.text.y=element_text(size=18,face="bold"),
                                           axis.title=element_text(size=18,face="bold"))
                                     }, height=450, width=800)
                                 
                                 output$tabelaZar<-renderDataTable({
                                     tabelaZaro<-data.frame("Data od"=wybrane$dataOd, "Data do"=wybrane$dataDo, "Osoba"= wybrane$imieNazw, "auto"=wybrane$samochod, "kwota"= wybrane$kwota)
                                     tabelaZar<-datatable(tabelaZaro, options = list(autoWidth = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Polish.json')))
                                     
                                     })
                             }
                         
                         zarobkiKw=sum(as.numeric(wybrane$kwota), na.rm=TRUE)
                         output$zarobki<-renderText({
                             zarobki<-paste("W wybranym okresie zarobiłeś:", zarobkiKw, "złotych")
                         })
                         }
                     })
    
    output$dynamika<-renderPlot(
        {
            #rezerwacje<-loadData()
            zarobki<-dynamikaSprzedazy(rezerwacje, Sys.time())
            
            wykres<-ggplot(zarobki, aes(x=factor(miesiacSlownie, level = miesiacSlownie), y=wartosc))+
                geom_col(fill="#81E88C", col="grey")+ 
                stat_summary(aes(label = stat(y)), fun = 'sum', geom = 'text', col = 'black', vjust = -0.5)+
                theme_minimal()+
                ggtitle("Dynamika zarobków w bieżącym roku")+
                theme(plot.title = element_text(size = 20, face = "bold")) +ylab("wartość w zł")+xlab("miesiąc")
            
            wykres<-wykres + geom_line(aes(x=(miesiacSlownie), y=poprzedni), colour="red", size=2)#+
                #geom_line(colour="red")
            wykres
        }
    )
    
    #samochody<-loadSam()
    wyborS1<-paste(samochody$numRej, samochody$model, sep = " ")
    wybory1<-list("wszystkie")
    wybory1<-append(wybory1, wyborS1)
    
    observeEvent(input$kopia,
                 {
                     rezerwacje<-loadData()
                     samochody<-loadSam()
                     klienci<-loadKlient()
                     write.csv(rezerwacje, "dane/rezerwacje-kopia.csv")
                     # drop_upload("rezerwacje-kopia.csv", path = outputDir)
                     write.csv(klienci, "dane/klienci-kopia.csv")
                     # drop_upload("klienci-kopia.csv", path = outputDir)
                     write.csv(samochody, "dane/samochody-kopia.csv")
                     # drop_upload("samochody-kopia.csv", path = outputDir)
                     output$udanyZapis<-renderText("Kopia zapasowa została wykonana")
                 })
    
    observeEvent(input$odczyt,
                 {
                     # samochody
                     # filesInfo <- drop_dir(outputDir)
                     # filesSam<-filesInfo[which(filesInfo$name=="samochody-kopia.csv"),]
                     # filePaths <- filesSam$path_lower
                     # samochody <- drop_read_csv(file = filePaths, stringsAsFactors = FALSE)
                     # samochody<-samochody[,2:7]
                     samochody <- read.csv(file = "dane/samochody-kopia.csv", stringsAsFactors = FALSE)
                     samochody<-samochody[,2:(ncol(samochody))]
                     
                     # Rezerwacje
                     # filesInfo <- drop_dir(outputDir)
                     # filesRez<-filesInfo[which(filesInfo$name=="rezerwacje-kopia.csv"),]
                     # filePaths <- filesRez$path_lower
                     # rezerwacje <- drop_read_csv(file = filePaths, stringsAsFactors = FALSE)
                     # rezerwacje<-rezerwacje[,2:6]
                     rezerwacje <- read.csv(file = "dane/rezerwacje-kopia.csv", stringsAsFactors = FALSE)
                     rezerwacje<-rezerwacje[,2:(ncol(rezerwacje))]
                     
                     # Klient
                     # filesInfo <- drop_dir(outputDir)
                     # filesKlient<-filesInfo[which(filesInfo$name=="klienci-kopia.csv"),]
                     # filePaths <- filesKlient$path_lower
                     # klienci <- drop_read_csv(file = filePaths, stringsAsFactors = FALSE)
                     # klienci<-klienci[,2:11] 
                     klienci <- read.csv(file = "dane/klienci-kopia.csv", stringsAsFactors = FALSE)
                     klienci<-klienci[,2:ncol(klienci)] 
                     
                     saveData(rezerwacje)
                     saveSam(samochody)
                     saveKlient(klienci)
                     output$udanyOdczyt<-renderText("Dane zostały odczytane z kopii zapasowej")

                 })

    
    #protokol
    output$protokol<- downloadHandler(
        filename = function()
        {
            paste("protokol", "pdf", sep = ".")
        },
        content<-function(file)
        {
            file.copy("pliki/protokol.pdf", file)
        },
        contentType = "application/pdf"
    )

    
    
    ### tu logowanie
    
    login.page = paste(
        isolate(session$clientData$url_protocol),
        "//",
        isolate(session$clientData$url_hostname),
        ":",
        isolate(session$clientData$url_port),
        sep = ""
    )
    histdata <- rnorm(500)
    USER <- reactiveValues(Logged = F)
    observe({
        if (USER$Logged == FALSE) {
            if (!is.null(input$Login)) {
                if (input$Login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    Id.username <- which(login_details$user %in% Username)
                    Id.password <- which(login_details$pswd %in% Password)
                    if (length(Id.username) > 0 & length(Id.password) > 0){
                        if (Id.username == Id.password) {
                            USER$Logged <- TRUE
                        }
                    }
                }
            }
        }
    })
    output$sidebarpanel <- renderUI({
        if (USER$Logged == TRUE) {
            div(
                sidebarUserPanel(
                    isolate(input$userName),
                    subtitle = a(icon=icon("box"), "Wyloguj", href = "https://shinyapps.io/")
                ),
                sidebarMenu(
                    menuItem(
                        "Dodaj rezerwację",
                        tabName = "DodajRezerwacje",
                        icon = icon("calendar-plus")
                    ),
                    
                    menuItem(
                        "Rezerwacje",
                        tabName = "Rezerwacje",
                        icon = icon("calendar-alt")
                    ),
                    
                    menuItem("Kalendarz",
                             tabName = "Kalendarz",
                             icon = icon("calendar-times")),
                    
                    menuItem("Klient",
                             tabName = "Klient",
                             icon = icon("address-card")),
                    
                    menuItem("Firma",
                             tabName = "Firma",
                             icon = icon("building")),
                    
                    menuItem("Samochód",
                             tabName = "Samochod",
                             icon = icon("car")),
                    
                    menuItem("Zestawienia",
                             tabName = "Zestawienia",
                             icon = icon("chart-line")),
                    
                    menuItem("Pliki do pobrania",
                             tabName = "Pliki",
                             icon = icon("file-download")),
                    menuItem("Ustawienia",
                             tabName = "Ustawienia",
                             icon = icon("cogs"))
                )
            )
        }
    })
    
    output$body <- renderUI({
        if (USER$Logged == TRUE) {
            tabItems(
                
                tabItem(tabName = "DodajRezerwacje",
                        fluidRow(
                            h3("Dodawanie rezerwacji:"),
                            column(12,
                                   fluidRow(
                                       useShinyjs(), 
                                       style = "position:relative",
                                       
                                       column(3,
                                              wellPanel(
                                                  selectInput(inputId = "samochodDod",
                                                              label = "Wybierz samochód:",
                                                              choices=wyborSam),
                                                  radioButtons("zewnatrz", 
                                                               h4("Samochód na zewnątrz:"), 
                                                               choices = list("Czysty" = 1, 
                                                                              "Brudny" = 2),
                                                               selected = 1),
                                                  radioButtons("wewnatrz", 
                                                               h4("Samochód wewnątrz:"), 
                                                               choices = list("Czysty" = 1, 
                                                                              "Brudny" = 2),
                                                               selected = 1),
                                                  textInput("stanLicznika", label = "Stan licznika", value = "GPS"),
                                                  textInput("stanPaliwa", label = "Stan paliwa", value = "100% pojemności"),
                                                  checkboxGroupInput(inputId = "przekazano",
                                                                     label = "Przekazano:",
                                                                     choices = list("kluczyk 1 szt do samochodu", "dowód rejestracyjny", "kserokopia polisy ubezpieczenia", "pasy transportowe"), selected = "kluczyk 1 szt do samochodu")
                                                  )),
                                       
                                       column(4,
                                              wellPanel(
                                                  splitLayout(dateInput(inputId="dataOdDod", label = "Data początkowa", weekstart = 1,
                                                                        language = "pl"),
                                                              timeInput("godzinaOdDod", "Godzina:", minute.steps = 5, value = Sys.time()+hours(czasZimaLato()))),
                                                  splitLayout(dateInput(inputId="dataDoDod", label = "Data końcowa", weekstart = 1,
                                                                        language = "pl"),
                                                              timeInput("godzinaDoDod", "Godzina:", minute.steps = 5, value = Sys.time()+hours(czasZimaLato()))),
                                                  verbatimTextOutput("ileDni"),
                                                  dateInput(inputId="dataZawUmowy", label = "Data zawarcia umowy", weekstart = 1,
                                                            language = "pl"),
                                                  textInput("numerUmowy", label = "Numer umowy:"),
                                                  textAreaInput(inputId="komentarzDod", label = ("Komentarz"))
                                                  )),
                                       
                                       column(4,
                                              wellPanel(
                                                  conditionalPanel(
                                                      condition="input.klFirmaDod==0",
                                                      selectInput(inputId = "osobaDod",
                                                                  label = "Wybierz osobę:",
                                                                  choices = wyborOs),
                                                      
                                                      actionButton(
                                                          inputId = "nowyKlient",
                                                          label = "Nowy klient",
                                                          icon("plus"), 
                                                          style="color: #fff; background-color: #337ab7")
                                                      ),
                                                  
                                                  conditionalPanel(
                                                      condition="input.klFirmaDod==1",
                                                      selectInput(inputId = "wybFirmaDod",
                                                                  label = "Wybierz firmę:",
                                                                  choices = wyborFirm)
                                                      ),
                                                  
                                                  checkboxInput(inputId = "klFirmaDod", label = ("Firma"), value = FALSE),
                                                  textInput("zagranica", label = "Pojazd będzie użytkowany na terenie Polski oraz:"),
                                                  splitLayout(
                                                      textInput("limit1", label = "Limit km:", value = "300"),
                                                      textInput("limit2", label = "na:", value = "dobę")
                                                  ), 
                                                  strong("Opłata za wynajem "),
                                                  splitLayout(
                                                      textInput("cena1", label = "za:"),
                                                      textInput("jednCzasu", value = "dni", label = "jedn. czas."),
                                                      textInput("cena2", label = "zł:")
                                                  ),
                                                  h2(),
                                                  textInput("kwotaDod", label = "Łącznie kwota:"),
                                                  textInput("kaucja", label = "Kaucja zwrotna:", value = "1000 zł")
                                                  ))
                                       )
                                   ),
                            column(2, offset = 4,
                                   actionBttn(
                                       inputId = "addDod",
                                       label = "Dodaj rezerwację!",
                                       color = "success",
                                       style = "material-flat",
                                       block = TRUE)
                                  )
                            )
                        ),
                
                tabItem(tabName = "Rezerwacje",
                        fluidRow(useShinyjs(),
                            sidebarPanel(
                                strong("Dodawanie rezerwacji:"),
                                h1(" "),
                                dateInput(inputId="dataOd", label = "Data początkowa", weekstart = 1,
                                          language = "pl"),
                                
                                dateInput(inputId="dataDo", label = "Data końcowa", weekstart = 1,
                                          language = "pl"),
                                
                                selectInput(inputId = "samochod",
                                            label = "Wybierz samochód:",
                                            choices=wyborSam),
                                
                                textInput(inputId="kwota", label = ("Kwota")),
                                
                                conditionalPanel(
                                    condition="input.klFirma==0",
                                    selectInput(inputId = "osoba",
                                                label = "Wybierz osobę:",
                                                choices = wyborOs)
                                ),
                                
                                conditionalPanel(
                                    condition="input.klFirma==1",
                                    selectInput(inputId = "wybFirma",
                                                label = "Wybierz firmę:",
                                                choices = wyborFirm)
                                ),
                                
                                checkboxInput(inputId = "klFirma", label = ("Firma"), value = FALSE),
                                
                                textInput(inputId="komentarz", label = ("Komentarz")),
                                actionButton(inputId="add", label = "Dodaj rezerwację", icon("plus"), width = '75%',
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                br(),
                                span(textOutput("dodajRez"), style="color:green"),
                                span(textOutput("sprawdzenie1"), style="color:red"),
                                tags$hr(style="border-color: #4287f5"),
                                strong("Drukowanie umowy"),
                                h5("Kliknij na wybraną rezerwację w tabeli"),
                                h3(),
                                radioButtons(inputId="firmaKtora", label = "Wybierz firmę",
                                             choices = list("Company 1" = 1, "Company 2" = 1)),
                                downloadBttn(
                                    outputId = "reportNowy",
                                    style = "bordered",
                                    color = "primary", 
                                    label = "Pobierz wybraną umowę"),
                                
                                span(textOutput("pobrano"), style="color:green")
                                ),
                            
                            
                            mainPanel(
                                      fluidRow(
                                          column(2, offset = 2,
                                                 actionButton(inputId="refresh", label = "Odśwież", icon("sync-alt"), width = '150px', 
                                                                            style="color: #444347; background-color: #a8ee90")),
                                          
                                          
                                          column(2, offset = 1,
                                                 actionButton(inputId="aktualizuj", label = "Aktualizuj klienta", icon("retweet"), width = '150px',
                                                              style="color: #444347; background-color: #ffff99")),
                                          
                                          column(2, offset = 1,
                                                 actionButton(inputId="delete", label = "Usuń", icon("minus"), width = '150px',
                                                              style="color: #fff; background-color: #ff6161"))
                                          ),
                                      fluidRow(
                                          h2(),
                                          span(textOutput("usunRez"), style="color:green"),
                                          h1()
                                          ),
                                      DT::dataTableOutput("rezerwacje")
                                      )
                            )
                        ),
            
            tabItem(
                tabName = "Kalendarz",
                fluidRow(useShinyjs(),
                         sidebarPanel(
                             DT::dataTableOutput("samochodyKolory"), 
                             actionButton(inputId="refreshCal", label = "Odśwież kalendarz", icon("sync-alt"), 
                                          style="color: #444347; background-color: #a8ee90"),
                             width = 3),
                         
                         mainPanel(
                              fullcalendarOutput("kalendarz", width = "100%", height = "400px"),
                              h1(),
                         )
                         )
                ),
                
                tabItem(
                    tabName = "Klient",
                    fluidRow(useShinyjs(),
                        sidebarPanel(
                        strong("Dodawanie klienta:"),
                        h1(" "),
                        textInput(inputId="imie", label = ("Imię")),
                        textInput(inputId="nazwisko", label = ("Nazwisko")),
                        textInput(inputId="telefon", label = ("Numer telefonu")),
                        span(textOutput("sprawdzenieTel"), style="color:red"),
                        textInput(inputId="kod", label = ("Kod pocztowy")),
                        textInput(inputId="miasto", label = ("Miasto")),
                        textInput(inputId="ulica", label = ("Ulica i numer")),
                        textInput(inputId="dowod", label = ("Seria i numer dowodu")),
                        textInput(inputId="pesel", label = ("PESEL")),
                        textInput(inputId="katPraw", label = ("Kategoria prawa jazdy")),
                        textInput(inputId="numPraw", label = ("Numer prawa jazdy")),
                        selectInput(inputId = "klientFirma",
                                    label = "Wybierz firmę:",
                                    choices = c("",wyborFirm), selected = ""),
                        actionButton(inputId="zatwierdz", label = "Dodaj klienta", icon("plus"), 
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        span(textOutput("dodaj"), style="color:green"),
                        
                        h1(" "),
                        
                        span(textOutput("usun"), style="color:green"), 
                        
                        
                        bsModal("klientEdycja", "Edytuj klienta", "edytujKlient", size = "medium",
                                
                                textInput(inputId="imieEd", label = ("Imię")),
                                textInput(inputId="nazwiskoEd", label = ("Nazwisko")),
                                textInput(inputId="telefonEd", label = ("Numer telefonu")),
                                textInput(inputId="kodEd", label = ("Kod pocztowy")),
                                textInput(inputId="miastoEd", label = ("Miasto")),
                                textInput(inputId="ulicaEd", label = ("Ulica i numer")),
                                textInput(inputId="dowodEd", label = ("Seria i numer dowodu")),
                                textInput(inputId="peselEd", label = ("PESEL")),
                                textInput(inputId="katPrawEd", label = ("Kategoria prawa jazdy")),
                                textInput(inputId="numPrawEd", label = ("Numer prawa jazdy")),
                                
                                selectInput(inputId = "klientFirmaEd",
                                            label = "Wybierz firmę:",
                                            choices = c("",wyborFirm), selected = ""),
                                actionButton(inputId="zatwierdzEdycja", label = "Zatwierdź", icon("plus"), 
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                span(textOutput("sukcesEdycja"), style="color:green")
                                ),
                        width = 3),
                    
                    mainPanel(
                        fluidRow(
                            
                            column(2, offset = 3,
                                   actionButton(inputId="deleteKl", label = "Usuń klienta", icon("minus"), 
                                                style="color: #fff; background-color: #ff6161")),
                            
                            column(2, offset = 2,
                                   actionButton("edytujKlient", "Edytuj klienta", 
                                                icon = icon("user-tie"),style="color: #000000; background-color: #93D151")),
                            h1(),
                            h2(),
                        ),
                        fluidRow(
                            h2(),
                            h1()
                        ),
                        DT::dataTableOutput("klienci")
                        )
                    )
                    ),
                tabItem(
                    tabName = "Firma",
                    fluidRow(useShinyjs(),
                             sidebarPanel(
                                 strong("Szukaj firmy:"),
                                 h1(" "),
                                 actionButton(inputId="GUS", label = "Włącz wyszukiwarkę", icon("eye"), 
                                              style="color: white; background-color: #187FF5"), 
                                 span(textOutput("wyszukiwarka"), style="color:green"),
                                 textInput(inputId="nipFir", label = ("NIP")),
                                 actionButton(inputId="szukajFir", label = "Szukaj", icon("search"), 
                                              style="color: #444347; background-color: #a8ee90"),
                                 span(textOutput("blednyNIP"), style="color:red"),
                                 span(textOutput("sukcesNIP"), style="color:green"),
                                 textInput(inputId="nazwa", label = ("Nazwa:")),
                                 textInput(inputId="ulicaFir", label = ("Ulica:")),
                                 textInput(inputId="miejscowoscFir", label = ("Miejscowość:")),
                                 textInput(inputId="KodFir", label = ("Kod:")),
                                 textInput(inputId="regon", label = ("Regon:")),
                                 textInput(inputId="email", label = ("Email:")),
                                 textInput(inputId="telefonFir", label = ("Telefon:")),
                                 actionButton(inputId="dodajFir", label = "Dodaj firmę", icon("plus"), width = '75%',
                                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 
                                 bsModal("reprezentant", "Dodaj reprezentanta firmy", "dodajReprez", size = "medium",
                                         conditionalPanel(
                                             condition="input.listaKl==1",
                                             selectInput(inputId = "klientFir",
                                                         label = "Wybierz osobę:",
                                                         choices = wyborOs)
                                         ),
                                         textInput(inputId="imieRep", label = ("Imię")),
                                         textInput(inputId="nazwiskoRep", label = ("Nazwisko")),
                                         textInput(inputId="telefonRep", label = ("Numer telefonu *")),
                                         textInput(inputId="dowodRep", label = ("Seria i numer dowodu")),
                                         textInput(inputId="peselRep", label = ("PESEL")),
                                         textInput(inputId="katPrawRep", label = ("Kategoria prawa jazdy")),
                                         textInput(inputId="numPrawRep", label = ("Numer prawa jazdy")),
                                         actionButton(inputId="zatwierdzRep", label = "Zatwierdź", icon("plus"), 
                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                         span(textOutput("sukcesRep"), style="color:green")),
                                             
                                 h1(" "),
                                 
                                 h1(" ")
                                 ),
                             
                             mainPanel(
                                 
                                 fluidRow(
                                     
                                     column(2, offset = 2,
                                            actionButton(inputId="usunFir", label = "Usuń firmę", icon("minus"), 
                                                         style="color: #fff; background-color: #ff6161")),
                                     
                                     column(2, offset = 2,
                                            actionButton("dodajReprez", "Rezprezntant danej firmy", 
                                                         icon = icon("user-tie"),style="color: #000000; background-color: #ffff99")),
                                     h1()),
                                 fluidRow(
                                     h2(),
                                     h1()
                                     ),
                                 DT::dataTableOutput("firmy")
                                 )
                             )
                    ),
                tabItem(
                    tabName = "Samochod",
                    fluidRow(useShinyjs(),
                        sidebarPanel(
                            strong("Dodawanie samochodu:"),
                            h1(" "),
                            textInput(inputId="marka", label = ("Marka")),
                            textInput(inputId="model", label = ("Model")),
                            textInput(inputId="numRej", label = ("Numer rejestracyjny")),
                            textInput(inputId="vin", label = ("Numer VIN")),
                            textInput(inputId="dowodRej", label = ("Numer dowodu rej.")),
                            radioButtons(inputId="paliwo", label = "Wybierz rodzaj paliwa",
                                         choices = list("Diesel", "Benzyna")),
                            colourpicker::colourInput("col", "Wybierz kolor", value = randomColor()),
                            actionButton(inputId="zatwierdz2", label = "Dodaj samochód",icon("plus"), 
                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            span(textOutput("sprawdzenieAut"), style="color:red"),
                            span(textOutput("dodajSam"), style="color:green"),
                            h1(" ")
                            ),
                        
                        mainPanel(
                            fluidRow(
                                column(2, offset = 3,
                                       actionBttn(
                                           inputId = "deleteSam",
                                           label = HTML("Usuń samochód"),
                                           color = "danger",
                                           style = "material-flat",
                                           icon = icon("minus"),
                                           block = TRUE, 
                                           size = "sm")
                                       ),
                                
                                column(2, offset = 1,
                                       actionBttn(
                                           inputId = "edytujSam",
                                           label = HTML("Edytuj samochód"),
                                           color = "primary",
                                           style = "material-flat",
                                           icon = icon("car-side"),
                                           block = TRUE, 
                                           size = "sm")
                                       ),
                                
                                bsModal("samEdycja", "Edytuj samochód", "edytujSam", size = "small",
                                        
                                        textInput(inputId="markaEd", label = ("Marka")),
                                        textInput(inputId="modelEd", label = ("Model")),
                                        textInput(inputId="numRejEd", label = ("Numer rejestracyjny")),
                                        textInput(inputId="vinEd", label = ("Vin")),
                                        textInput(inputId="dowRejEd", label = ("Dowód rej.")),
                                        textInput(inputId="paliwoEd", label = ("Rodzaj paliwa")),
                                        textInput(inputId="kolorEd", label = ("Kolor")),
                                        
                                        actionButton(inputId="zatwierdzEdycjaSam", label = "Zatwierdź", icon("plus"), 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        span(textOutput("sukcesEdycjaSam"), style="color:green")
                                ),
                                h1()
                            ),
                            fluidRow(
                                h2(),
                                h1()
                            ),
                            DT::dataTableOutput("samochody")
                            )
                        )
                    ),
                tabItem(
                    tabName = "Zestawienia",
                    fluidRow(
                        tabsetPanel(
                            tabPanel("Zarobki z danego okresu",
                                     fluidRow(
                                         sidebarPanel(
                                             dateInput(inputId="zakresOd", label = "Data początkowa", weekstart = 1,
                                                       language = "pl"),
                                             
                                             dateInput(inputId="zakresDo", label = "Data końcowa", weekstart = 1,
                                                       language = "pl"),
                                             
                                             checkboxGroupInput(inputId = "samochod1",
                                                                label = "Wybierz samochód:",
                                                                choices = wybory1, selected = "wszystkie"),
                                             textInput("innySam", "Nie ma samochodu powyżej?", 
                                                       placeholder = "Wpisz tu numery rej."),
                                             actionButton(inputId="szukaj", label = "szukaj", icon("search-dollar"), 
                                                          style="color: #444347; background-color: #a8ee90"),
                                             
                                             span(textOutput("brakSam"), style="color:green"),
                                             width = 3
                                         ),
                                         
                                         mainPanel(
                                             h2(span(textOutput("zarobki"))),
                                             h1(" "),
                                             plotOutput("wykresZarobki"), 
                                             h1(" "),
                                             h1(" "),
                                             h1(" "),
                                             h1(" "),
                                             h1(" "),
                                             dataTableOutput("tabelaZar")
                                         )
                                     )
                                     ),
                            tabPanel("Dynamika sprzedaży",
                                     fluidRow(
                                         column(10, offset = 1,
                                                plotOutput("dynamika")
                                                )
                                         )
                                     )
                            )
                        )
                    ),
            tabItem(
                tabName = "Pliki",
                fluidRow(
                    mainPanel(
                        h3("Dokumenty pdf do pobrania:"),
                        h2(" "),
                        downloadButton("warunkiLaweta", label = "Umowa szcz. autolaweta", icon("truck-pickup"), 
                                     style="color: white; background-color: #1397bf"),
                        h3(" "),
                        downloadButton("warunkiBus", label = "Umowa szcz. bus", icon("car"), 
                                     style="color: white; background-color: #1397bf"),
                        h3(" "),
                        downloadButton("przedwstepna", label = "Umowa przedwstępna", icon("calendar-alt"), 
                                     style="color: white; background-color: #1397bf"),
                        h3(" "),
                        h3("Protokół zdawczo odbiorczy:"),
                        h2(" "),
                        downloadButton("protokol", label = "Protokół", icon("pencil-alt"), 
                                     style="color: white; background-color: #e89a13")
                        )
                    )
                ),

            tabItem(
                tabName = "Ustawienia",
                fluidRow(
                    sidebarPanel(
                        ("Przed zmianami warto zrobić kopię zapasową!"),
                        actionButton(inputId="kopia", label = "Zrób kopię zapasową", icon("file-export"), 
                                     style="color: #444347; background-color: #a8ee90"),
                        span(textOutput("udanyZapis"), style="color:green"),
                        span(textOutput("DataKopii"), style="color:blue"),
                        h1(" "),
                        
                        ("Jesteś pewien? Wczytane będą dane z ostatniego zapisu, niezapisane dane mogą zniknąć."),
                        actionButton(inputId="odczyt", label = "Wczytaj dane z kopii zapasowej", icon("file-import"), 
                                     style="color: #444347; background-color: #a8ee90"),
                        span(textOutput("udanyOdczyt"), style="color:green"),
                        h1(" ")
                        ),
                    mainPanel()
                    )
                )
            )
            } else {
                login
                }
        })

}
