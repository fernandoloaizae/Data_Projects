# -*- coding: utf-8 -*-
"""
Created on Tue Jul 27 10:10:10 2021

@author: loaiza
"""

import sys
sys.path.append("H:\Python_scrap")


#%%


#%%

# importing required modules 
import PyPDF2 
import openpyxl
    
# creating a pdf file object 
pdfFileObj = open('H:\Python_scrap\global.pdf', 'rb') 
    
# creating a pdf reader object 
pdfReader = PyPDF2.PdfFileReader(pdfFileObj) 
    
pageObj = pdfReader.getPage(25)

mytext = pageObj.extractText()
   

wb = openpyxl.load_workbook('H:\Python_scrap\excel.xlsx')
sheet = wb.active
sheet.title = 'MyPDF'
sheet['A1'] = mytext
wb.save('H:\Python_scrap\excel.xlsx')

# closing the pdf file object 
#pdfFileObj.close() 

#%%

from io import StringIO
from pdfminer.high_level import extract_text_to_fp
from pdfminer.layout import LAParams
output = StringIO()
with open('example.pdf', 'rb') as pdf_file:
    extract_text_to_fp(pdf_file, output, laparams=LAParams(), output_type='html', codec=None)
with open('example.html', 'a') as html_file:
    html_file.write(output.getvalue())