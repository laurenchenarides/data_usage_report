import pdfplumber
import pandas as pd

# Open the PDF file
pdf_path = r"C:\Users\wangm\OneDrive - Colostate\RA\Sopus_OpenAlex\git\compare_scopus_openalex\resources\IPEDS\MSI List 2021.pdf"
import os

pdf_path = r"C:\Users\wangm\OneDrive - Colostate\RA\Sopus_OpenAlex\git\compare_scopus_openalex\resources\IPEDS\MSI List 2021.pdf"

if os.path.exists(pdf_path):
    print("File exists!")
else:
    print("File not found!")


data = []
with pdfplumber.open(pdf_path) as pdf:
    for page in pdf.pages:
        tables = page.extract_tables()
        for table in tables:
            data.extend(table)  # Append all table data

# Convert extracted data to a Pandas DataFrame

df = pd.DataFrame(data)

# Save to Excel
excel_path = r"C:\Users\wangm\OneDrive - Colostate\RA\Sopus_OpenAlex\git\compare_scopus_openalex\resources\IPEDS\MSI_List_2021.xlsx"
df.to_excel(excel_path, index=False)

print(f"Excel file saved: {excel_path}")



# Open the PDF file
pdf_path = r"C:\Users\wangm\OneDrive - Colostate\RA\Sopus_OpenAlex\git\compare_scopus_openalex\resources\IPEDS\2022 MSI List.pdf"
import os

pdf_path = r"C:\Users\wangm\OneDrive - Colostate\RA\Sopus_OpenAlex\git\compare_scopus_openalex\resources\IPEDS\2022 MSI List.pdf"

if os.path.exists(pdf_path):
    print("File exists!")
else:
    print("File not found!")


data = []
with pdfplumber.open(pdf_path) as pdf:
    for page in pdf.pages:
        tables = page.extract_tables()
        for table in tables:
            data.extend(table)  # Append all table data

# Convert extracted data to a Pandas DataFrame

df = pd.DataFrame(data)

# Save to Excel
excel_path = r"C:\Users\wangm\OneDrive - Colostate\RA\Sopus_OpenAlex\git\compare_scopus_openalex\resources\IPEDS\MSI_List_2022.xlsx"
df.to_excel(excel_path, index=False)

print(f"Excel file saved: {excel_path}")


# Open the PDF file
pdf_path = r"C:\Users\wangm\OneDrive - Colostate\RA\Sopus_OpenAlex\git\compare_scopus_openalex\resources\IPEDS\2023 CMSI MSI List.pdf"
import os

pdf_path = r"C:\Users\wangm\OneDrive - Colostate\RA\Sopus_OpenAlex\git\compare_scopus_openalex\resources\IPEDS\2023 CMSI MSI List.pdf"

if os.path.exists(pdf_path):
    print("File exists!")
else:
    print("File not found!")


data = []
with pdfplumber.open(pdf_path) as pdf:
    for page in pdf.pages:
        tables = page.extract_tables()
        for table in tables:
            data.extend(table)  # Append all table data

# Convert extracted data to a Pandas DataFrame

df = pd.DataFrame(data)

# Save to Excel
excel_path = r"C:\Users\wangm\OneDrive - Colostate\RA\Sopus_OpenAlex\git\compare_scopus_openalex\resources\IPEDS\MSI_List_2023.xlsx"
df.to_excel(excel_path, index=False)

print(f"Excel file saved: {excel_path}")

