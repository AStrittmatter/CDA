#!/usr/bin/python3

from csv import writer
from pprint import pprint
import mechanicalsoup
# mechanicalsoup imports bs
# from bs4 import BeautifulSoup

# site to scrape
site = "http://nap.psa.gov.ph/ggi/default.asp"

# output file
csvfile = open(r'Data/ggi-py.csv','wb')
output = writer(csvfile)

# open site and get menu options
br = mechanicalsoup.StatefulBrowser()
br.open(site)
page = br.get_current_page()
menu = page.find('select', id='strMunicipality2').find_all('option')
# equivalent
# menu = page.select('#strMunicipality2 > option')
# extract options, only using options, not location text
options = [el['value'] for el in menu]
# locations = [el.text for el in menu]

# write header
header = ["indicator", "value2005", "rank2005", "value2008", "rank2008", "municipality "]
output.writerow(header)


# iterate over options, limit iterations
for pos, item in enumerate(options[:5]):

    # feedback on current item and position
    print(pos, len(options), item)

    # open site, select form, submit option
    br.open(site)
    br.select_form('#form2')
    br['strMunicipality2'] = item
    br.submit_selected()

    # extract table from page
    table = br.get_current_page().select('.dataTable')[0]
    # pprint(table)

    # prep and write to csv linewise, slice away header
    for row in table.find_all('tr')[2:]:
        line = []
        for cell in row.find_all('td'):
            line.append(cell.get_text().strip())
        line.append(item)
        assert len(line)==6
        # print(line)
        output.writerow(line)

    # alternatively, this list comprehension extracts the table
    # as a nested list for further processing
    # data = [[cell.text.strip() for cell in row.find_all('td')]
    #             for row in table.find_all('tr')]


# close output file connection
csvfile.close()
