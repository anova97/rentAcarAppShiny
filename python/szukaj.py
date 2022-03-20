import litex.regon
from litex.regon import REGONAPI
api = REGONAPI('https://wyszukiwarkaregon.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc')
api.login('MY-API-LOGIN')

def szukajNazwa(numer):
  import litex.regon
  from litex.regon import REGONAPI
  api = REGONAPI('https://wyszukiwarkaregon.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc')
  api.login('MY-API-LOGIN')
  result = api.search(nip=numer)
  return result[0].Nazwa

def szukajUlica(numer):
  result = api.search(nip=numer)
  return result[0].Ulica
  
def szukajMiejscowosc(numer):
  result = api.search(nip=numer)
  return result[0].Miejscowosc  
  
def szukajKod(numer):
  result = api.search(nip=numer)
  return result[0].KodPocztowy
  
def szukajRegon(numer):
  result = api.search(nip=numer)
  return result[0].Regon
  

def szukajNIP2(numer):
    import litex.regon
    from litex.regon import REGONAPI
    api = REGONAPI('https://wyszukiwarkaregon.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc')
    api.login('MY-API-LOGIN')
    result = api.search(nip=numer, detailed=True)
    if result[0].Typ == 'F':
        wynik =  (result[0].Nazwa, result[0].Ulica, result[0].detailed.fiz_adSiedzNumerNieruchomosci, 
                 result[0].detailed.fiz_adSiedzNumerLokalu, result[0].Miejscowosc, result[0].KodPocztowy,
                 result[0].Regon, result[0].detailed.fiz_adresEmail, result[0].detailed.fiz_numerTelefonu)
    if result[0].Typ == 'P':
        wynik =  (result[0].Nazwa, result[0].Ulica, result[0].detailed.praw_adSiedzNumerNieruchomosci, 
                 result[0].detailed.praw_adSiedzNumerLokalu, result[0].Miejscowosc, result[0].KodPocztowy,
                 result[0].Regon, result[0].detailed.praw_adresEmail, result[0].detailed.praw_numerTelefonu)
    return wynik
