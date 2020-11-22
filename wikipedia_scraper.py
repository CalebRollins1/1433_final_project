from bs4 import BeautifulSoup
import pandas as pd
import requests
from selenium import webdriver
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
import numpy as np
import re

cities = pd.read_csv("uscities.csv")
#get 500 largest cities
city_list = list(zip(list(cities.city),list(cities.state_name)))[0:500]

#properly formats city names so they are consistent with wikipedia's formatting
def format(city,state):
    return city.replace(' ','+')+'+'+state.replace(' ',"+")
#takes percents that written as strings and converts them to floats
def fix_percent(p):
    s = p.strip('%').split('.')
    if len(s)==1:
        return int(s[0])
    else:
        return int(s[0])+int(s[1])/(10**len(s[1]))



n=0

nominees = []
parties = []
percentages = []
election = []
states = []
#loop that iterates through all cities of interest
for city,state in city_list:
    try:
        page = requests.get("https://en.wikipedia.org/w/index.php?search="+format(city,state)+"+mayoral+election"+"&title=Special%3ASearch&go=Go&ns0=1")

        soup = BeautifulSoup(page.content, 'html.parser')
        p = soup.find_all(class_ = "mw-search-result-heading")

        #loop iterating through all wikipedia search results about the cities mayoral races in question
        for elem in p:
            #checks if there is a page about mayoral race results in 2017, 2018, or 2019
            if (elem.get_text()[0:4] in set(["2017","2018","2019"])) and ("mayoral" in elem.get_text()) and (city in elem.get_text()):
                page = requests.get("https://en.wikipedia.org/"+elem.find('a').get('href'))
                soup = BeautifulSoup(page.content, 'html.parser')
                names = soup.find('th',text=re.compile("^Nominee|^Candidate"))
                #iterates through all candidates in the race to collect names
                for elem in names.find_next_siblings("td"):
                    name = elem.get_text().rstrip()
                    print(name)
                    if name!='':
                        nominees.append(name)
                        election.append(city)
                        states.append(state)

                perc = soup.find('th',text=re.compile("^Percentage|^Runoff percentage|^Second-round percentage"))
                #gets percentages won by each candidate
                for elem in perc.find_next_siblings("td"):
                    percentage = elem.get_text().rstrip()
                    print(percentage)
                    if percentage!='':
                        percentages.append(fix_percent(percentage))
                p = soup.find('th',text=re.compile("^Party"))
                #gets party of each candidate
                for elem in p.find_next_siblings("td"):
                    party = elem.get_text().rstrip()
                    print(party)
                    if party!='':
                        parties.append(party)

                #correct with NAs for any missing parties or percents
                for i in range(len(nominees)-len(parties)):
                    parties.append(None)
                for i in range(len(nominees)-len(percentages)):
                    percentages.append(None)

                n+=1
                break
    except:
        #corrects with NAs for missing parties and percents
        for i in range(len(nominees)-len(parties)):
            parties.append(None)
        for i in range(len(nominees)-len(percentages)):
            percentages.append(None)

#takes the data collected above and puts it into a CSV
results = pd.DataFrame({
"nominee":nominees,
"party":parties,
"percentage":percentages,
"city":election,
"state":states
})
results.to_csv("city_elections.csv")
