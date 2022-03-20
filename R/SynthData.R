## Synthetic Data ----
## Generate synthetic data 

# definicje wartości kolumn klient
imiona <- c("Anna", "Jan", "Agnieszka", "Leszek", "Ilona", "Krzysztof", "Julia", "Jakub", 
            "Janina", "Filip", "Elżbieta", "Marcin", "Maria", "Janusz", "Ola", "Jerzy")

nazwiska <- c("Nowak", "Augustyniak", "Kuć", "Głowacz", "Noworyta", "Kowal", "Rozmus", 
              "Klimczak", "Zając", "Goryl", "Kogut")

kody <- c("32-500", "12-670", "12-098", "31-317", "31-314", "00-123", "10-133", "34-908", 
          "90-890", "70-890", "32-671")

miasta <- c("Chrzanów", "Kraków", "Krzeszowice", "Zabierzów", "Łódź", "Warszawa", "Lublin")

ulice <- c("Nowa", "Krzywa", "Prosta", "Krakowska", "Poznańska", "Wadowicka", "Oświęcimska")

klienci<-data.frame(
  "imie" = sample(imiona, 40, replace = T),
  "nazwisko" = sample(nazwiska, 40, replace = T),
  "telefon" = sample((500000000 : 999999999), 40, replace = F),
  "kod" = sample(kody, 40, T),
  "miasto" = sample(miasta, 40, T),
  "ulica" = paste(sample(ulice, 40, T), sample((1:200), 40, T)),
  "dowod" = paste(sample(LETTERS, 40, T),sample(LETTERS, 40, T),sample(LETTERS, 40, T), sample((100:999), 40, T), sep = ""),
  "pesel" = sample((40000000000:99123155555), 40, F),
  "kat" = sample(c("B", "B+E", "B,C", "A,B,C"), 40, T),
  "numPraw" = paste(sample((1000:9999), 40, T), sample((10:99), 40, T), sample((100:999), 40, T), sep = "/"),
  "NIP" = NA
  
)

saveKlient(klienci)






