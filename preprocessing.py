import csv
import pandas as pd 

def default_int(x):
    if x == 'yes': 
        return 1
    else: 
        return 0

def job_int(x):
    jobs = ["admin.","unknown","unemployed","management","housemaid","entrepreneur","student", "blue-collar","self-employed","retired","technician","services"]
    return jobs.index(x)

def mar_int(x):
    marital = ["married","divorced","single"]
    return marital.index(x)

def contat_int(x):
    contact = ["unknown","telephone","cellular"]
    return contact.index(x)

def edu_int(x):
    education = ["unknown","secondary","primary","tertiary"]
    return education.index(x)

def pout_int(x):
    poutcome = ["unknown","other","failure","success"]
    return poutcome.index(x)

def month_int(x):
    month = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']
    return month.index(x)
'''
categorical = {}
categorical['job'] = ["admin.","unknown","unemployed","management","housemaid","entrepreneur","student", "blue-collar","self-employed","retired","technician","services"]
categorical['martital'] = ["married","divorced","single"]
categorical['education'] = ["unknown","secondary","primary","tertiary"]
categorical['default'] = ["yes", "no"]
categorical['housing'] = ["yes","no"]
categorical['loan'] = ["yes", "no"]
categorical['contact'] = ["unknown","telephone","cellular"]
categorical['month'] = ["may"]
'''

data = pd.read_csv("bank.csv") 

print(data.head())

data['default'] = data['default'].apply(default_int)
data['housing'] = data['housing'].apply(default_int)
data['loan'] = data['loan'].apply(default_int)
data['job'] = data['job'].apply(job_int)
data['marital'] = data['marital'].apply(mar_int)
data['contact'] = data['contact'].apply(contat_int)
data['education'] = data['education'].apply(edu_int)
data['poutcome'] = data['poutcome'].apply(pout_int)
data['month'] = data['month'].apply(month_int)
data['y'] = data['y'].apply(default_int)

print(data.head())


data.to_csv(path_or_buf = "bank-int.csv")

