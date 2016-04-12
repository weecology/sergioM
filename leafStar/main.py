# -*- coding: utf-8 -*-
"""
Created on Tue Feb 16 14:53:46 2016

@author: sergiomarconi
"""
import numpy.linalg
from numpy import *
from numpy import genfromtxt
import csv
import functions

meteo = genfromtxt('imputLeaffal.csv', delimiter=',')
Rad = meteo[:,0]
temperature = meteo[:,1]

k_lamb = 0.72
albedo = 0.12
F0CTEM = 0.35
efficiency  = 0.045  #light use efficiency


alpha = [0.6, 0.7, 0.78, 0.3, 0.15]
beta = [0.6, 0.7, 0.78, 0.0, 0.0]

nLayers = 5
LAI = [2.5, 1.8, 1.0, 0.5, 0.2]

# A = [[alpha[0], alpha[1], alpha[2], alpha[3],alpha[4]],  #Leslie Matrix
#      [beta[0], 0.0, 0.0, 0.0, 0.0],
#      [0.0, beta[1], 0.0, 0.0, 0.0],
#      [0.0, 0.0, beta[2], 0.0, 0.0],
#      [0.0, 0.0, 0.0, beta[3], 0.0]]

with open('LAI.csv', 'wb') as f:
    writer = csv.writer(f)
    header = ['LAI0', 'LAI1', 'LAI2', 'LAI3', 'LAI4', 'mos']
    writer.writerows([header])

with open('leslie.csv', 'wb') as f:
    writer = csv.writer(f)
    header = ['DoS', 'beta.0','beta.1','beta.2','beta.3','beta.4', \
              'alpha.0','alpha.1','alpha.2','alpha.3','alpha.4', \
              'eigVal.0','eigVal.1','eigVal.2','eigVal.3','eigVal.4', \
              'eigVect.0','eigVect.1','eigVect.2','eigVect.3','eigVect.4']
    writer.writerows([header])

with open('plasticity.csv', 'wb') as f:
    writer = csv.writer(f)
    header = ['DoS', 'ageClass', 'Aa', 'Pi', 'NitA', 'En']
    writer.writerows([header])

for dos in range(len(Rad)):
    # here call the functions to update the leslie matrix with the new probability of survivorship and rates of reproduction
    beta = functions.alphabetai(dos, Rad, temperature, LAI, alpha)
    #alpha = functions.alphai(dos)

    A = [[alpha[0], alpha[1], alpha[2], alpha[3], alpha[4]],  # Leslie Matrix
         [beta[0], 0.0, 0.0, 0.0, 0.0],
         [0.0, beta[1], 0.0, 0.0, 0.0],
         [0.0, 0.0, beta[2], 0.0, 0.0],
         [0.0, 0.0, 0.0, beta[3], 0.0]]

    LAI = numpy.dot(A, LAI)
    with open('LAI.csv', 'a') as f:
        writer = csv.writer(f)
        data = append(LAI, dos)
        writer.writerows([data])

    evals, evect = numpy.linalg.eig(A)
    with open('leslie.csv', 'a') as fp:
        a = csv.writer(fp)
        data = append(int(dos), beta)
        data = append(data, alpha)
        data = append(data, evals)
        if(abs(max(evals)) > abs(min(evals))):
            maxEig = max(evals)
        else:
            maxEig = min(evals)
        data = append(data, maxEig)
        #data = append(data,  evect)
        a.writerows([data])

    print LAI
    print alpha
    print beta
    print
        #moltiplica per LAI >> updated LAI per ogni piano
    #calcola gli evals
    #store gli N and Evals
