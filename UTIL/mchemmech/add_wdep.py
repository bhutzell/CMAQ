import pandas as pd
import os

def check_spc(var, spc):
    check = False
    for i in spc:
        if i == var: check = True
    return check

def adjust_AE(wdep, species):
    wdep_new    = []
    species_new = []
    for i in range(len(wdep)):
        for AE in ['I','J','K']:
            wdep_new.append(wdep[i])
            species_new.append(species[i]+AE)
    return wdep_new, species_new

def creat_tmp(filename):
    # creates temp file since some nml-files miss
    # a ',' at the end of the header which causes
    # issues with pandas read_csv()
    inp = open(filename, 'r')
    with open(filename, 'r') as inp:
        lines = inp.read().splitlines()
    tmp_name = filename+'_tmp'
    tmp = open(tmp_name, 'w')
    for i in lines:
        if '!SPECIES' in i:
            if i[-1] != ',':
                tmp.write(i+',\n')
            else:
                tmp.write(i+'\n')
        else:
            tmp.write(i+'\n')
    tmp.close()
    return tmp_name

def load_wdep(filename,AE=False):
    tmp_name = creat_tmp(filename)
    data = pd.read_csv(filename+'_tmp',header=2)
    data = data.rename(columns=lambda x: x.strip())
    # drop last row
    data = data[:-1]

    # define entries
    wdep    = data['WET-SCAV SURR']
    wdep    = [x.strip(' ') for x in wdep]
    wdep    = [x.strip("'") for x in wdep]
    species = data['!SPECIES']
    species = [x.strip(' ') for x in species]
    species = [x.strip("'") for x in species]
    if AE: wdep, species = adjust_AE(wdep, species)
    # delete tmp file
    os.remove(tmp_name)
    print(wdep)
    return wdep, species

# load mchem species
data = pd.read_csv('mchem.spc', sep='=', comment='#', names=['Species','Formula'])
spc  = data['Species']
spc  = [x.strip(' ') for x in spc]

# load species information
wdep_GC, species_GC = load_wdep('../../CCTM/src/MECHS/cb6r5_ae7_aqkmt2/GC_cb6r5_ae7_aq.nml')
wdep_AE, species_AE = load_wdep('../../CCTM/src/MECHS/cb6r5_ae7_aqkmt2/AE_cb6r5_ae7_aq.nml', AE=True)
wdep_NR, species_NR = load_wdep('../../CCTM/src/MECHS/cb6r5_ae7_aqkmt2/NR_cb6r5_ae7_aq.nml')
wdep    = wdep_GC + wdep_AE + wdep_NR
species = species_GC + species_AE + species_NR

j = 1
lines_eqn = []
lines_spc = []
for i in range(len(wdep)):
    if wdep[i].strip() != '':
        if check_spc(species[i], spc):
            line = '<GASWD'+str(j).rjust(2,'0')+'> '+species[i].ljust(12,' ')+'   = WD_'+species[i].ljust(12,' ')+' : KPP_RSCAV( IND_'+species[i]+' );'
            lines_eqn.append(line)
            if not check_spc('WD_'+species[i], spc):
                line = 'WD_'+species[i]+' = IGNORE;'
                lines_spc.append(line)
            j += 1

with open("mchem.eqn", "a") as myfile:
    myfile.write('\n')
    myfile.write('// Gas & Aerosol scavenging\n')
    for i in lines_eqn:
        myfile.write(i+'\n')

with open("mchem.spc", "a") as myfile:
    myfile.write('#DEFVAR\n')
    for i in lines_spc:
        myfile.write(i+'\n')
