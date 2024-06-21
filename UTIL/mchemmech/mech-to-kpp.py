# load modules
import re
import numpy as np

# define functions
def form(string,dp=False):
    try:
        string = float(string)
    except:
        expo = ['E','e','d','D']
        lexpo = False
        for ex in expo:
            if ex in string: lexpo = True
        if not lexpo: print('No expo in string:',string,rx.rxlabel)
        if '-' in string: sep = '-'
        if '+' in string: sep = '+'
        parts = string.split(sep)
        if '.' in string:
            string = float(parts[0]+'E'+sep+parts[1])
        else:
            string = float(parts[0]+parts[1])
        if not lexpo: print(string)
    string = str('{:.4e}'.format(float(string)))
    replace = 'D' if dp else 'E'
    string = string.replace('e',replace)
    return string

def change_sign(strin):
    strout = strin.strip()
    if '-' in strout:
        strout = strout.replace('-','+')
    else:
        strout = '-'+strout
    return strout

# define constants
zero = '0.0'
one  = '1.0'
dummy = 'DUMMY'

# define classes
class reaction:
    # define non-init variables
    rxout   = ''
    rateout = ''
    note    = ''
    # init class
    def __init__(self, reaction):
        self.reaction = reaction
        self.ratein   = re.findall(str(re.escape('#'))+"(.*?)"+str(re.escape(';')),self.reaction)[0]
        self.rxlabel  = re.findall(str(re.escape('<'))+"(.*?)"+str(re.escape('>')),self.reaction)[0].strip()
        self.react_prod = re.findall(str(re.escape('>'))+"(.*?)"+str(re.escape('#')),self.reaction)[0].split('%')[0]
        reactants_temp = re.findall(str(re.escape('>'))+"(.*?)"+str(re.escape('=')),self.reaction)[0]
        reactants_temp = reactants_temp.split('+')
        reactants = []
        for i in reactants_temp:
            j = i.replace(' ','')
            reactants.append(j)
        reactants.sort()
        reactants[:] = [x for x in reactants if x]
        if not reactants: reactants.append(dummy)
        self.reactants = reactants
        products_temp = re.findall(str(re.escape('='))+"(.*?)"+str(re.escape('#')),self.reaction)[0]
        products_temp = products_temp.split('%')[0].split('+')
        products = []
        for i in products_temp:
            j = i.replace(' ','')
            products.append(j)
        products.sort()
        products[:] = [x for x in products if x]
        if not products:
            products.append(dummy)
            self.react_prod += dummy
        # test for negative products
        for i in range(len(products)):
            spc = products[i]
            if '-' in spc:
                products[i] = spc.split('-')[0]
                products.append(spc.split('-')[1])
        self.products = products
        if '%' in self.reaction:
            prerx    = re.findall("(.*?)"+str(re.escape('%')),self.reaction)[0]
        else:
            prerx    = re.findall("(.*?)"+str(re.escape('#')),self.reaction)[0]
        self.prerx   = prerx

        # find reaction type
        rxtype = 1
        if '%' in self.reaction:
            ident = self.reaction.split('%',1)[1][0]
            if ident == '1':
                rxtype = 7
            if ident == '2':
                rxtype = 8
            if ident == '3':
                if not '^' in self.ratein:
                    rxtype = 9
                else:
                    rxtype = 9.1
            if ident == '4':
                rxtype = 13
            if ident == 'H':
                rxtype = 12
        else:
            if '~' in self.ratein:
                rxtype = -1
            if '/' in self.ratein:
                rxtype = 0
            if '?' in self.ratein:
                rxtype = 11
            if '*' in self.ratein:
                rxtype = 6
            if '^' in self.ratein and not '&' in self.ratein:
                rxtype = 2
            if '@' in self.ratein and not '&' in self.ratein:
                rxtype = 3
            if '^' in self.ratein and '@' in self.ratein:
                rxtype = 4
            if '@' in self.ratein and '*' in self.ratein:
                rxtype = 5
            #if '^' in self.ratein and '@' in self.ratein and '&' in self.ratein:
            #    rxtype = 10
            #if '^' in self.ratein and '&' in self.ratein:
            #    rxtype = 10
            if self.ratein.count('&') == 2:
                rxtype = 10
            if self.ratein.count('&') == 3:
                rxtype = 10
        self.rxtype = rxtype

class const_spc:
    # init class
    def __init__(self, definition):
        self.definition = definition
        self.spc        = re.findall(str(re.escape('ATM_'))+"(.*?)"+str(re.escape('=')),self.definition)[0]
        self.spc        = self.spc.replace(' ','')
        if self.spc == 'AIR': self.spc = 'M'
        self.conc       = self.definition.split('=')[1]
        self.conc       = self.conc.replace(' ','')

# define mechanism
filename = input('Provide total path to gas-phase mechanism file: ')

# load mechanism
mechin = []
with open(filename) as infile:
    copy = False
    for line in infile:
        if 'REACTIONS[CM]' in line.strip():
            copy = True
            continue
        if 'END MECH' in line.strip():
            copy = False
            continue
        elif copy:
            add = True
            # exclude empty lines and comments
            if line.strip() == '':
                add = False
            elif line.strip()[0] == '!':
                add = False
            if add: mechin.append(line.strip())

# load constant species
constin = []
with open(filename) as infile:
    copy = False
    for line in infile:
        if 'CONSTANTS' in line.strip():
            copy = True
            continue
        if 'END CONSTANTS' in line.strip():
            copy = False
            continue
        elif copy:
            add = True
            # exclude empty lines and comments
            if line.strip() == '':
                add = False
            elif line.strip()[0] == '!':
                add = False
            if add: constin.append(line.strip())

# Add special case for H2O
constin.append('ATM_H2O = 0')

const = []
for definition in constin:
    const.append(const_spc(definition))

# combine reactions into one line and define class objects
mech = []
index = 0
while index < len(mechin):
    line = mechin[index]
    add  = False
    while not add:
        if line[-1] == ';':
            add = True
        else:
            index += 1
            line += mechin[index]
    mech.append(reaction(line))
    index += 1

rxhet     = {}
rxphot    = {}
rxfalloff = {}

for rx in mech:
    if rx.rxtype == -1:
        A = rx.ratein.split('~')[0]
        H = re.match(r'^.*<(.*)>.*$',rx.ratein)
        H = H.group(1)
        rx.rateout = A+' * KHETCELL( IK_'+H+' )'
        rxhet[H] = 'IK_'+H
    if rx.rxtype == 0:
        A = rx.ratein.split('/')[0]
        J = re.match(r'^.*<(.*)>.*$',rx.ratein)
        J = J.group(1)
        rx.rateout = A+' * RJCELL( IJ_'+J+' )'
        rxphot[J] = 'IJ_'+J
    if rx.rxtype == 1:
        rx.rateout = form(rx.ratein,dp=True) if float(rx.ratein) < 1e-30 else form(rx.ratein)
    if rx.rxtype == 2:
        A0 = rx.ratein.split('^')[0]
        B0 = rx.ratein.split('^')[1]
        rx.rateout = 'POWER_T02( '+form(A0,dp=True)+', '+form(B0,dp=True)+' )'
    if rx.rxtype == 3:
        A0 = rx.ratein.split('@')[0]
        B0 = rx.ratein.split('@')[1]
        # Adjust temperature sign - needed for ARR2
        B0 = change_sign(B0)
        rx.rateout = 'ARR2_DP( '+form(A0,dp=True)+','+form(B0,dp=True)+' )'
        rx.note    += 'Sign of temperature dependence (B0) adjusted (needed for ARR2)'
    if rx.rxtype == 4:
        AB = rx.ratein.split('@')[0]
        A0 = AB.split('^')[0]
        C0 = AB.split('^')[1]
        B0 = rx.ratein.split('@')[1]
        B0 = change_sign(B0)
        rx.rateout = 'ARRHENUIS_T04( '+form(A0,dp=True)+','+form(B0,dp=True)+','+form(C0,dp=True)+' )'
    if rx.rxtype == 5:
        rx.rateout = '5() '#+A0+','+B0+','+C0+' )'
    if rx.rxtype == 6:
        A0 = rx.ratein.split('*')[0]
        R0 = re.match(r'^.*<R(.*)>.*$',rx.ratein)
        R0 = R0.group(1)
        rx.rateout = A0+' * RCONST('+str(R0)+')'
        #rx.rateout = mech[int(R0)].rateout
        rx.note = 'Using rate constant from reaction '+R0
    if rx.rxtype == 7:
        rx.rateout = '7()'
    if rx.rxtype == 8:
        AC0 = rx.ratein.split('&')[0]
        A0  = AC0.split('@')[0]
        C0  = AC0.split('@')[1]
        C0  = change_sign(C0)
        AC2 = rx.ratein.split('&')[1]
        A2  = AC2.split('@')[0]
        C2  = AC2.split('@')[1]
        C2  = change_sign(C2)
        AC3 = rx.ratein.split('&')[2]
        A3  = AC3.split('@')[0]
        C3  = AC3.split('@')[1]
        C3  = change_sign(C3)
        rxfalloff['FALLOFF_T08_'+rx.rxlabel] = 'FALLOFF_T08_'+rx.rxlabel+' = FALLOFF_T08( '+form(A0,dp=True)+','+form(C0,dp=True)+', &\n' \
                                                                                           +form(A2,dp=True)+','+form(C2,dp=True)+', &\n' \
                                                                                           +form(A3,dp=True)+','+form(C3,dp=True)+' )'
        rx.rateout = 'FALLOFF_T08_'+rx.rxlabel
        rx.note    += 'Sign of temperature dependence (C0, C2, C3) adjusted'
    if rx.rxtype == 9:
        AC0 = rx.ratein.split('&')[0]
        A0  = AC0.split('@')[0]
        C0  = AC0.split('@')[1]
        C0  = change_sign(C0)
        AC1 = rx.ratein.split('&')[1]
        A1  = AC1.split('@')[0]
        C1  = AC1.split('@')[1]
        C1  = change_sign(C1)
        rx.rateout = 'FALLOFF_T09( '+form(A0,dp=True)+','+form(C0,dp=True)+','+form(A1,dp=True)+','+form(C1,dp=True)+' )'
        rx.note    += 'Sign of temperature dependence (C0 & C1) adjusted'
    if rx.rxtype == 9.1:
        ABC0 = rx.ratein.split('&')[0]
        AB0  = ABC0.split('@')[0]
        A0   = AB0.split('^')[0]
        B0   = AB0.split('^')[1]
        C0   = ABC0.split('@')[1]
        ABC1 = rx.ratein.split('&')[1]
        AB1  = ABC1.split('@')[0]
        A1   = AB1.split('^')[0]
        B1   = AB1.split('^')[1]
        C1   = ABC1.split('@')[1]
        AC2  = rx.ratein.split('&')[2]
        A2  = AC2.split('@')[0]
        C2  = AC2.split('@')[1]
        rx.rateout = '9.1( '+A0+','+B0+','+C0+','+A1+','+B0+','+C1+','+A2+','+C2+' )'
    if rx.rxtype == 10:
        ABC0 = rx.ratein.split('&')[0]
        AB0  = ABC0.split('@')[0]
        A0   = AB0.split('^')[0]
        if '^' in AB0:
            B0   = AB0.split('^')[1]
        else:
            B0 = one
        if '@' in ABC0:
            C0   = ABC0.split('@')[1]
        else:
            C0   = zero
        ABC1 = rx.ratein.split('&')[1]
        AB1  = ABC1.split('@')[0]
        A1   = AB1.split('^')[0]
        if '^' in AB1:
            B1   = AB1.split('^')[1]
        else:
            B1 = one
        if '@' in ABC0:
            C1   = ABC1.split('@')[1]
        else:
            C1   = zero
        A2 = rx.ratein.split('&')[2]
        try:
            C2 = rx.ratein.split('&')[3]
        except:
            C2 = one
        C0 = change_sign(C0)
        C1 = change_sign(C1)
        rxfalloff['FALLOFF_T10_'+rx.rxlabel] = 'FALLOFF_T10_'+rx.rxlabel+' = FALLOFF_T10( '+form(A0,dp=True)+','+form(C0,dp=True)+', &\n' \
                                                                                           +form(B0,dp=True)+','+form(A1,dp=True)+', &\n' \
                                                                                           +form(C1,dp=True)+','+form(B1,dp=True)+', &\n' \
                                                                                           +form(C2,dp=True)+','+form(A2,dp=True)+')'
        rx.rateout = 'FALLOFF_T10_'+rx.rxlabel
    if rx.rxtype == 11:
        rx.rateout = '11()'
    if rx.rxtype == 12:
        AC0 = rx.ratein.split('&')[0]
        A0  = AC0.split('@')[0]
        C0  = AC0.split('@')[1]
        C0  = change_sign(C0)
        AC1 = rx.ratein.split('&')[1]
        A1  = AC1.split('@')[0]
        C1  = AC1.split('@')[1]
        C1  = change_sign(C1)
        A2  = rx.ratein.split('&')[2]
        rx.rateout = 'HALOGEN_FALLOFF( '+form(A0,dp=True)+','+form(C0,dp=True)+','+form(A1,dp=True)+','+form(C1,dp=True)+','+form(A2,dp=True)+' )'
    if rx.rxtype == 13:
        rx.rateout = '13()'

for rx in mech:
    if len(rx.reactants) == 2 and rx.rxtype == -1:
        rx.rateout += '/ CAIR'

# find duplicate reactions
lduplicate = []
for index in range(len(mech)):
    duplicate = False
    rxreact = mech[index].reactants
    rxprod  = mech[index].products
    for j in range(len(mech)):
        if j == index:
            continue
        if mech[j].reactants == rxreact:
            if mech[j].products == rxprod:
                duplicate = True
                break
    ind = [index,j]
    ind.sort()
    if duplicate: lduplicate.append(ind)

rduplicate = []
[rduplicate.append(x) for x in lduplicate if x not in rduplicate]
lduplicate = rduplicate


# add dummy product to duplicate reaction
for ind in lduplicate:
    index = ind[1]
    mech[index].products.append(dummy)
    mech[index].react_prod += ' + '+dummy

for line in mech:
    line.rxout = '<'+line.rxlabel+'> '+line.react_prod.replace('*',' ')+' : '+line.rateout+';'
    if line.note != '':
        line.rxout += ' //'+line.note

# write mechanism to file
outfile = open('gas.eqn','w')
with open('inline_rates.kpp') as infile:
    for line in infile:
        outfile.write(line)
outfile.write('#INLINE F90_GLOBAL\n')
with open('inline_global.kpp') as infile:
    for line in infile:
        outfile.write(line)
outfile.write('\n')

# add phot
outfile.write('INTEGER, PARAMETER  :: KPP_NPHOTAB  =  '+str(len(rxphot))+'     ! number of photolysis rates\n')
outfile.write('CHARACTER(16), SAVE :: KPP_PHOTAB( KPP_NPHOTAB )  ! Names of  photolysis\n')
outfile.write('REAL(dp)            :: RJCELL( KPP_NPHOTAB )  ! grid cell photolysis rates ,[min-1]\n')
index = 1
for phot in rxphot:
    outfile.write('INTEGER, PARAMETER  :: '+rxphot[phot].ljust(19)+'      =   '+str(index)+'\n')
    index += 1
index = 1
for phot in rxphot:
    outfile.write("DATA KPP_PHOTAB( "+str(index).rjust(2)+" ) / '"+phot.ljust(16)+"' /\n")
    index += 1
outfile.write('\n')
# add het
outfile.write('INTEGER, PARAMETER  :: KPP_NHETERO  =  '+str(len(rxhet))+'     ! number of heterogeneous rates\n')
outfile.write('CHARACTER(16), SAVE :: KPP_HETERO( KPP_NHETERO )  ! Names of  heterogeneous\n')
outfile.write('REAL(dp)            :: KHETCELL( KPP_NHETERO )  ! grid cell heterogeneous rates ,[min-1]\n')
index = 1
for het in rxhet:
    outfile.write('INTEGER, PARAMETER  :: '+rxhet[het].ljust(19)+'      =   '+str(index)+'\n')
    index += 1
index = 1
for het in rxhet:
    outfile.write("DATA KPP_HETERO( "+str(index).rjust(2)+" ) / '"+het.ljust(16)+"' /\n")
    index += 1
outfile.write('\n')
outfile.write('#ENDINLINE\n')

outfile.write('#INLINE F90_RCONST\n')
outfile.write('  USE MCHEM_CLOUDS, ONLY: UPDATE_GM1_GM2\n')
outfile.write('\n')
for falloff in rxfalloff:
    outfile.write('REAL(dp)  :: '+falloff+'\n')
outfile.write('\n')
for falloff in rxfalloff:
    outfile.write(rxfalloff[falloff]+'\n')
outfile.write('\n')
outfile.write('#ENDINLINE\n')

outfile.write('#EQUATIONS\n')
for line in mech:
    outfile.write(line.rxout+'\n')
outfile.close()

# create species file
species = []
for line in mech:
    for spc in line.reactants:
        spc_nomulti = re.sub(r'^.*?\*', '', spc)
        if spc_nomulti not in species:
            species.append(spc_nomulti)
    for spc in line.products:
        spc_nomulti = re.sub(r'^.*?\*', '', spc)
        if spc_nomulti not in species:
            species.append(spc_nomulti)

# remove negative values

outfile = open('gas.spc','w')
#outfile = open('outfile.spc','w')
outfile.write('#INCLUDE atoms\n')
outfile.write('#DEFVAR\n')
for spc in species:
    var_spc = True
    for i in const:
        if spc == i.spc: var_spc = False
    if var_spc: outfile.write(' '+spc+' = IGNORE;\n')
outfile.write('#DEFFIX\n')
for spc in const:
    outfile.write(' '+spc.spc+' = IGNORE;\n')
outfile.close()
