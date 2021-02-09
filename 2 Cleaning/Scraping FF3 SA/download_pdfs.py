#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 30 08:29:54 2018

@author: kevin
"""

import csv
from selenium import webdriver
import os
import time



# Download pdf files ============================

# Load well names which need to be downloaded
wellNames = []
with open('well names to search.csv') as wnf:
    csvReader = csv.reader(wnf)
    for row in csvReader:
        wellNames.append(row[0])

# Set up Firefox profile to enable pdf downloading
fprofile = webdriver.FirefoxProfile()
fprofile.set_preference('browser.download.folderList', 2)
fprofile.set_preference('browser.download.manager.showWhenStarting', False)
fprofile.set_preference('browser.download.dir', os.getcwd() + '/A pdf files')
fprofile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'application/pdf')
fprofile.set_preference("pdfjs.disabled", True)

driver = webdriver.Firefox(firefox_profile=fprofile)

driver.get('https://fracfocusdata.org/DisclosureSearch/Search.aspx')

assert 'Find a Well' in driver.title

# Download pdfs, keeping track of which wells are successfully downloaded
start_value = 1
for i in range(start_value, len(wellNames)):

    wellName = wellNames[i]

    wellNameField = driver.find_element_by_id('MainContent_tbWellName')
    wellNameField.clear()
    wellNameField.send_keys(wellName)

    searchButton = driver.find_element_by_id('MainContent_btnSearch')
    searchButton.click()

    resultsTable = driver.find_element_by_id('MainContent_GridView1')
    resultRows = resultsTable.find_elements_by_tag_name('tr')
    resultRows.pop(0)

    print('Downloading ' + str(i) + '/' + str(len(wellNames)) + ': ' + wellName)

    for r in resultRows:
        pdfButton = r.find_element_by_css_selector('input[src=\"../Images/pdf-icon-48x48.png\"]')
        pdfButton.click()
        time.sleep(5)

    # Keep track of wells downloaded
    with open('downloaded_wells.txt', 'a') as f:
        f.write(wellName + '\n')

driver.close()


