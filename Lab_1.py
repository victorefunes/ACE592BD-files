## Here we are importing the requests and BeautifulSoup modules
import requests
from bs4 import BeautifulSoup

## Requests is the module that actually goes out and accesses the website.
## BeautifulSoup helps with formatting and parsing the html.
page = requests.get("https://en.wikipedia.org/wiki/Nineteen_Eighty-Four")


## Here we are accessing wikipedia using the requests module, running this block will 
## give us a snapshot of the page at the time of access.

## When you run this block there should be no output, go ahead, give it a try.

## Now that we have grabbed a snapshot of the page, let's see what happens when we print it out!
print(page)

## As you can see, printing the page just gives us a response code, when what we want is the page content.
## This can be simply accomplished by accessing the .content of the page object
## we created earlier and printing it out.
print(page.content)

## This command will give you all of the page's content.
## As you can see we have the page content now, but its not that easy to read, that's why Beautiful Soup is very useful.
## Now we are going to access and parse the content as html using beautiful soup's html.parser.
soup = BeautifulSoup(page.content, 'html.parser')

## Let's see what the parser got us!
print(soup)

## As you can see the HTML is now much easier to read, and with some simple commands we can search for objects within the html very easily.
## The find_all command is very useful in finding all instances of the text or class you give it.
## Text parameters go into the first argument, class in the second.
## For this notebook we are going to find the heading of the wiki article. 
## We know the class we are looking for is 'firstheading', you can see this in the output above.
## Google chromes inspect element is very useful for finding the object that you're looking for as well.
body = soup.find_all('', class_='firstHeading')

## Let's see what the soup found.
print(body)

## So its easy as that. All we have is the HTML line, so some trimming is necessary. If you want just the title 
## you would omit the rest of the line using python's string commands. (string[startIndex:EndIndex])
## First we have to cast it as a string though.
title = str(body)
title = title[57:77]
print(title)

## 57 and 77 are the start and end positions of the title.
## These first lines are just like we saw earlier. Getting the webpage, and getting a soup object of the page content.
## For easy to read formatting and parsing.
page = requests.get("https://datatables.net/examples/basic_init/zero_configuration.html")
soup = BeautifulSoup(page.content, 'html.parser')


## The complex part is working with tables, which are a very common and relevant piece of data
## on a webpage. The only thing you have to know is that a table is made of 'tr' and 'td' objects.
## A 'tr' object is a row and 'td' is all of the data in that row. (tableRow) and (tableData).


## So here we are simply running a for loop that gets the first and only 'tr' object and all of it's corresponding
## 'td' objects.
data = [[cell.get_text(strip=True) for cell in row.find_all('td')] for row in soup.find_all("tr")]

## Now let's print out all of the row data for the table on the page!
for entry in data:
    print(str(entry))

## Here we are creating a txtfile called "scraper.txt" in your outputs/ folder.
import os
txtfile = open(os.path.join(os.path.expanduser('~'), "outputs/Lab1", "scraper.txt"),"w")

## Here we are writing the header to the file.
txtfile.write("Name, Position, Age, Start_date, Salary")

##Here we are taking each entry and trimming the undesirable characters and writing it to the file
for entry in data:
    text = str(entry)
    text = text.replace(']', '')
    text = text.replace('[', '')
    text = text.replace("'", '')
    print(text)
    txtfile.write(text + "\n")
    
txtfile.close()

## Go check the file out in the outputs/ folder, it's named scraper.txt!

import requests
from bs4 import BeautifulSoup

page = requests.get("https://www.immobiliare.it/agenzie-immobiliari/roma-provincia/")

# Print contents of page
#print(page.content)

soup = BeautifulSoup(page.content, 'html.parser')
#print(soup.prettify())

# soup title
#soup.title

# soup head
#soup.head


text = []
phone = [] 
sites = []
titles = []
featured = []

for link in soup.find_all('div', {'class': 'block__data'}): 
    text.append(link.find('p').getText())
   
for link in soup.find_all('span', {'class': 'btn btn-white info-agenzia hidden js-clickable'}): 
    phone.append(link.text)
    
for link in soup.find_all('a', href=True): 
    sites.append(link['href'])  
    titles.append(link.get('title'))
    
for link in soup.find_all('span', attrs={'class': 'strip'}):
    featured.append(link.text)
    
sites = sites[138:168]    
titles = titles[138:168]


c1=[]
for entry in titles:
    title = str(entry)
    c1.append(title)

c2=[]
for site in sites:
    string = str(site)
    c2.append(string)
    
c3=[]  
for entry in text:
    string = str(entry)
    string = string.replace("\n", "")
    string = string.replace("'", "")
    c3.append(string)
    
c4=[]
for entry in phone:
    string = str(entry)
    c4.append(string)
    
c5=[]
for entry in featured:
    string = str(entry)
    c5.append(string)     
   
import os
txtfile = open(os.path.join(os.path.expanduser('~'), "my_labs/outputs/Lab 1", "scraper_italy.txt"),"w")

txtfile.write("Name, Web_site, Address, Phone_number, Featured")

for index in range(len(c1)):
    txtfile.write(str(c1[index]) + ", " + str(c2[index]) + ", "+
               str(c3[index]) + ", " + str(c4[index]) + ", " + str(c5[index]) + "\n")
txtfile.close()