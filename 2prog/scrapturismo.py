import requests
from bs4 import BeautifulSoup
from urllib import request


link = "http://dados.turismo.gov.br/index.php/chegada-de-turistas-internacionais"
html = requests.get(link).content
bsobj = BeautifulSoup(html, "lxml")
menu = bsobj.find("details").find_all("li")

for x in menu:
    linkcsv =  x.find("a")["href"]
    print("=======================================================")
    print(linkcsv)
    request.urlretrieve(linkcsv, linkcsv.split("/")[-1])

