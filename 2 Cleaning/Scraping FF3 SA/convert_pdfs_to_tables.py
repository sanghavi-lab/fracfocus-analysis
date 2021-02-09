
import pandas as pd
import csv
import os
import tabula


allpdfs = os.listdir("A pdf files")

for i in range(0, len(allpdfs)):
    pdfFile = allpdfs[i]

    try:
        # Convert pdfs to tabular outputs
        tabula.convert_into('A pdf files/' + pdfFile, 'B initial tabula output/' + pdfFile + '.csv', pages='all', lattice=True)
        
        # Convert tabular outputs into readable rows for R
        with open('B initial tabula output/' + pdfFile + '.csv') as f:
            skip = 0
            while True:
                line = f.readline()
                skip += 1
                if line.startswith('Trade Name'):
                    break
        df = pd.read_csv('B initial tabula output/' + pdfFile + '.csv', skiprows=skip, 
                         names=['Trade name','Supplier','Purpose','Ingredients', 
                                'CAS','Max additive','Max HF fluid','Comments'])
        df.to_csv('C ingredient table output/' + pdfFile + '.csv', quoting=csv.QUOTE_ALL)
    
        print('Finished ' + str(i+1) + '/' + str(len(allpdfs)))
    
    except:
        # Append file name to list of failed conversions
        with open('failed_conversions.txt', 'a') as f:
            f.write(pdfFile + '\n')
        
        print('FAILED PDF CONVERSION FOR FILE: ' + pdfFile)
