#libraries
import numpy as np
from scipy import special as scsp
import math
import csv

k_lamb = 0.72
albedo = 0.12
F0CTEM = 0.35
nLayers = 5
efficiency  = 0.045  #light use efficiency

# Betas and Alphas
def alphabetai(dos, Rad, temperature, LAI, alpha):
    doy = dos % 12
    a = supplyRate(doy)
    growthR = np.zeros(nLayers)
    resSup = np.zeros(nLayers)
    resUsed = np.zeros(nLayers)
    Rstar = np.zeros(nLayers)
    betas = np.zeros(nLayers)

    for i in range(nLayers):
        resSup[i] = avaiLight(dos, i, Rad, LAI, nLayers)
        resUsed[i] = absorbRate(albedo, resSup[i])/30.0
        growthR[i], Rstar[i] = NEP(i, dos, temperature, alpha, resUsed[i])
        # betas[i] = (a * (growthR[i]) *(resSup[i] - resUsed[i]))/(LAI[i] )
        betas[i] = 1- (a * growthR[i] * (resUsed[i] -Rstar[i]) / LAI[i])
        if betas[i] < 0:
            betas[i]= 1.0
        elif betas[i] > 1:
            betas[i]=0.0

    return betas

#daylenght
def supplyRate(doy, gamma = 15):

    delta_max = 23.44 * np.pi/180.  #0.40910517666747087
    # DL = np.zeros(12)
    #for JD in xrange(1, 365, 30):
    JD = (doy +1) * 30
    delta_sin = np.sin(delta_max)*np.cos(2*np.pi * (JD + 10)/365.24)
    delta_cos = np.sqrt(1 - delta_sin**2)
        #gamma = 15

    a = np.sin(gamma) * delta_sin
    b = np.cos(gamma) * delta_cos
    DL = 12 *(1 + 2*np.arcsin(a/b)/np.pi)/24
    return DL

#NEP ######################
#values taken from Niinemetes et al, 2015
def NEP(i, dos, temperature, alpha, par):

    # global temperature
    En_param = [0.533, 4.22, 1.29, 1.39]
    NitA_param = [1.01, 4.75, 1.9, 2.02]
    # values extracted from exponential function exp(-x/1.5) the 5 quantities have to sum to 1
    #Z = [0.495910134, 0.263597138, 0.135335283, 0.069483451, 0.035673993]
    Z = [0.48, 0.27, 0.13, 0.07, 0.05]
    aveT = temperature[dos]

    #calculate the parameters for the equations involving Aa estimation:
    # En: photosintetic nitrogen use efficiency
    # NitA: Nitrogen content per unif leaf area
    En_alpha = En_param[3]* math.log(En_param[3]*(0.719-En_param[0])+En_param[3]*En_param[1])
    En_min = 0.719-En_param[0]
    En_lambda = 1/En_param[3]

    NitA_alpha = NitA_param[3]* math.log(NitA_param[3]*(0.719-NitA_param[0])+NitA_param[3]*NitA_param[1])
    NitA_min = 0.719-NitA_param[0]
    NitA_lambda = 1/NitA_param[3]

    En = En_lambda * math.exp(En_lambda *En_alpha * Z[i])-En_min
    NitA = NitA_lambda * math.exp(NitA_lambda *NitA_alpha * Z[i])-NitA_min
    Aa = En * NitA

    Mr = respiration(Aa, NitA, aveT)
    Pi = Aa - Mr
    Rstar = Mr/Aa
    with open('plasticity.csv', 'a') as fp:
        a = csv.writer(fp)
        data = np.append(dos, i)
        data = np.append(data, Aa)
        data = np.append(data, Mr)
        data = np.append(data, Pi)
        data = np.append(data,  NitA)
        data = np.append(data,  En)
        a.writerows([data])
    F0CTEM = 0.35
    alpha[i] = Pi * F0CTEM * efficiency * (1- albedo) * par
    #alpha[i] = Pi * F0CTEM * efficiency

    return (Pi, Rstar)

def respiration(Aa, NitA, aveT):
    q10 = 2.2
    yld = 0.4
    mpterm = 0.218
    tempF = q10 **((aveT-20)/10)
    growth = (1-yld)/yld * Aa/10
    maint = tempF * NitA * mpterm
    return(growth + maint)

# res supply #############################

def avaiLight(dos, layer, Rad, LAI,  nLayer = 5,):

    rad = Rad[dos]
    #proportion of scattered light going into canopy and not on the atmosphere
    propFactor = 0.1
    fc = 0.32
    CC, sumCC = Cover(LAI, fc)
    #for layer in nLayer:
    # direct = 1 - sumCC(layer, CC) * NetRad(rad) #, dos)
    # diffuse = sumCC(layer, CC) * DiffRad(rad, albedo)*propFactor
    direct = max(0, (1 - sumCC[layer])) * NetRad(rad)  # , dos)
    if layer == 0:
        diffuse = 0.0
    else:
        top = sumAbove(CC, layer)
        diffuse = (sumCC[layer] - top) * DiffRad(rad, albedo) * propFactor
    resSupp = direct + diffuse
    return resSupp

#todo chiamala per ogni generazione da fuori
def IncomingRad(netRad, LAI, k):
    return(netRad * np.exp(-k * LAI))

def NetRad(Rad):
    Qa = 90
    Qb = 0.8
    MOLPAR_MJ = 2.3
    netRad = (Qa + Qb * Rad) # * 10 ** 6)
    par = (netRad * MOLPAR_MJ) # * sun_bedo
    return(par)

    # todo outside of the function apar = par * absorbance()

def DiffRad(Rad, albedo):
    Qa = 90
    Qb = 0.8
    MOLPAR_MJ = 2.3
    netRad = (Qa + Qb * Rad) # * 10 ** 6)
    netRad_bedo = netRad * (1-albedo) #or sum(sun_bedo???)
    par = (netRad_bedo * MOLPAR_MJ) # * sun_bedo
    return(par)
    # todo outside of the function apar = par * absorbance()

def Cover(LAI, fc, k=0.2, nLayers =5):
    CC = np.zeros(nLayers)
    sumCC = np.zeros(nLayers)

    for i in range(nLayers):
        if (i == 0):
            CC[i] = - k * LAI[i] / np.log(fc)
            sumCC[i]=CC[i]
        else:
            sLAI = sumLAI(LAI,i)
            sumCC[i] = - k * sLAI / np.log(fc)
            CC[i] = sumCC[i] - sumCC[i-1]
    return CC, sumCC


# def Cover(LAI, fc, k=0.72, nLayers =5):
#     CC = [0.4, 0.3, 0.2, 0.08, 0.02]
#     sumCC=[0.4, 0.7, 0.9, 0.98, 1.0]
#     return CC, sumCC

def sumLAI(lai, i):
    for j in range(i+1):
        if (j == 0):
            sLai = lai[0]
        else:
            sLai = sLai + lai[j]
    return(sLai)
#
# def sumCCdiff(layer, nLayers, CC):
#     f = filter(lambda x: x > 4, layer)
#     return(sum(CC[f]))

def absorbRate(albedo, light):
    return light*(1 - albedo)

def sumAbove(sumCC, i):
    for j in range(i):
        if(j==0):
            top = sumCC[j]
        else:
            top = top + sumCC[j]
    return( top)
