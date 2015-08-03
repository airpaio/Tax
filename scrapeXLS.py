# Python script to scrape all .xls (Excel Spreadsheet) files from the 
#	indicated website.

import urllib
from bs4 import BeautifulSoup
import os
import re
import pdb

# Here we want to scrape .xls files of daily crime stats in Houston, TX
#	from 2009 to 2014.
url = "http://www.irs.gov/uac/SOI-Tax-Stats---Individual-Statistical-Tables-by-Size-of-Adjusted-Gross-Income#_grp1"
url2 = "http://www.irs.gov/file_source/pub/irs-soi/"
req = urllib.urlopen(url) 		# navigates to the url

soup = BeautifulSoup(req)

# Find each <a href="...">XLS</a> and download the file pointed to by href="..."
yearDirectories = map(str, range(1995, 2013))
count = 0
for link in soup.findAll('a'):
	if link.string in yearDirectories:
	    if re.search(r".*11si\.xls", link.get('href')):
	        webFile, fileExtension = os.path.splitext(link.get('href'))
	        fileName = webFile.split("/")[-1]
	        saveAs = os.path.join("data/", fileName + fileExtension)
	        print("Retreiving " + fileName + fileExtension)
	        urlFile = url2 + fileName + fileExtension
	        urllib.urlretrieve(urlFile, saveAs)
	        count += 1

print("Successfully retrieved ", count, " files") 


#re.search(r"(199\d|200\d|2012)", str(link.string)):
