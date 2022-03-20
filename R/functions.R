print("loading Functions")

loadSam <- function() {
  # filesInfo <- drop_dir(outputDir)
  # filesSam<-filesInfo[which(filesInfo$name=="samochody.csv"),]
  # filePaths <- filesSam$path_lower
  # data <- drop_read_csv(file = filePaths, stringsAsFactors = FALSE)
  data <- read.csv(file = "dane/samochody.csv", stringsAsFactors = FALSE)
  data<-data[,2:(ncol(data))]
}

loadData <- function() {
  # filesInfo <- drop_dir(outputDir)
  # filesRez<-filesInfo[which(filesInfo$name=="rezerwacje.csv"),]
  # filePaths <- filesRez$path_lower
  # data <- drop_read_csv(file = filePaths, stringsAsFactors = FALSE)
  data <- read.csv(file = "dane/rezerwacje.csv", stringsAsFactors = FALSE)
  data<-data[,2:(ncol(data))]
  
}

loadKlient <- function() {
  # filesInfo <- drop_dir(outputDir)
  # filesKlient<-filesInfo[which(filesInfo$name=="klienci.csv"),]
  # filePaths <- filesKlient$path_lower
  # data <- drop_read_csv(file = filePaths, stringsAsFactors = FALSE)
  data <- read.csv(file = "dane/klienci.csv", stringsAsFactors = FALSE)
  data<-data[,2:ncol(data)] 
}

loadRep <- function() {
  # filesInfo <- drop_dir(outputDir)
  # filesKlient<-filesInfo[which(filesInfo$name=="reprezentanci.csv"),]
  # filePaths <- filesKlient$path_lower
  # data <- drop_read_csv(file = filePaths, stringsAsFactors = FALSE)
  data <- read.csv(file = "dane/reprezentanci.csv", stringsAsFactors = FALSE)
  data<-data[,2:ncol(data)] 
}

loadFir <- function() {
  # filesInfo <- drop_dir(outputDir)
  # filesFir<-filesInfo[which(filesInfo$name=="firmy.csv"),]
  # filePaths <- filesFir$path_lower
  # data <- drop_read_csv(file = filePaths, stringsAsFactors = FALSE)
  data <- read.csv(file = "dane/firmy.csv", stringsAsFactors = FALSE)
  data<-data[,2:ncol(data)] 
}

saveData <- function(data) {
  fileName <- "rezerwacje.csv"
  write.csv(data, "dane/rezerwacje.csv")
  # drop_upload("rezerwacje.csv", path = outputDir)
}

saveKlient <- function(data) {
  fileName <- "klienci.csv"
  write.csv(data, "dane/klienci.csv")
  # drop_upload("klienci.csv", path = outputDir)
}

saveRep <- function(data) {
  fileName <- "reprezentanci.csv"
  write.csv(data, "dane/reprezentanci.csv")
  # drop_upload("reprezentanci.csv", path = outputDir)
}

saveSam <- function(data) {
  fileName <- "samochody.csv"
  write.csv(data, "dane/samochody.csv")
  # drop_upload("samochody.csv", path = outputDir)
}

saveFir <- function(data) {
  fileName <- "firmy.csv"
  write.csv(data, "dane/firmy.csv")
  # drop_upload("firmy.csv", path = outputDir)
}

czasZimaLato<-function(){
  miesiac=month(Sys.time())
  if(miesiac>3 && miesiac<11)
  {
    czas=2
  }else
  {
    czas=1
  }
  return(czas)
}

dynamikaSprzedazy<-function(rezerwacje, data)
{
  miesiace<-seq(1,12)
  biezacyNumer<-month(data)
  biezacyRokMies<-miesiace 
  zarobki<-data.frame("miesiąc"=biezacyRokMies, "wartosc"=0) 
  biezacyRok<-rezerwacje[which(year(rezerwacje$dataOd)==year(data)),]
  poprzedniRok<-rezerwacje[which(year(rezerwacje$dataOd)==year(data)-1),]
  for(i in 1:nrow(zarobki))
  {
    zarobki$wartosc[i]<-round(sum(biezacyRok$kwota[which(month(biezacyRok$dataOd)==zarobki$miesiąc[i])]),0)
    zarobki$poprzedni[i]<-round(sum(poprzedniRok$kwota[which(month(poprzedniRok$dataOd)==zarobki$miesiąc[i])]),0)
  }
  miesiaceSlownie<-c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec", "lipiec", "sierpień", "wrzesień", 
                     "październik", "listopad", "grudzień")
  zarobki$miesiacSlownie<-miesiaceSlownie
  return(zarobki)
}

print("done Functions")

