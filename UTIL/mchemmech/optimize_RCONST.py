# Script to improve computation efficiency by removing
# constant rate constants from Update_RCONST()

# import necessary modules
import sys
from shutil import copyfile

# define functions
def check_RCONST(line):
    const = True
    for string in variable_rconsts:
        if string in line.split('=')[-1]: const = False
    # exclude rate constants that are already
    # declared by KPP to be constant to avoid
    # double listing in Initialize file
    if 'constant rate coefficient' in line: const = False
    return const

def write_file(filename, lines):
    with open(filename, 'w') as fp:
        for line in lines:
            fp.write(line)
    return

# declare local variables
mech = sys.argv[1]
variable_rconsts = ['GM1','GM2','KRXN','KIEPOX','FALLOFF','RCONST']
constRCONST = []

# define file names
frates = mech+'_Rates.F90'
finit  = mech+'_Initialize.F90'

# create backup of original fortran file
copyfile(frates, frates+'.original')
copyfile(finit,  finit+'.original')

# open and read file
lrconst = False
lout_rates = []
with open(frates, 'r') as fp:
    # read all lines using readline()
    lines = fp.readlines()
    for line in lines:
        # check if string present on a current line
        if '! End INLINED RCONST' in line:         lrconst = True
        if 'END SUBROUTINE Update_RCONST' in line: lrconst = False
        # check if RCONST in line
        if lrconst and 'RCONST(' in line:
            if check_RCONST( line ):
                constRCONST.append( line )
                line = '! Constant rate moved to initialize : '+line
        lout_rates.append(line)

# get row index for Initialize file
linit = False
lout_init = []
with open(finit, 'r') as fp:
    lines = fp.readlines()
    for line in lines:
        lout_init.append(line)
        if 'SUBROUTINE Initialize' in line:     linit = True
        if 'END SUBROUTINE Initialize' in line: linit = False
        if linit and line.find('USE '+mech+'_Global') != -1:
            lout_init.append('  USE '+mech+'_Rates\n')
            lout_init.append('  USE '+mech+'_Parameters\n')
        if linit and line.find('! END constant rate coefficients') != -1:
            lout_init.append('\n')
            lout_init.append('! Further constant rate constants from optimization script\n')
            for rate in constRCONST:
                lout_init.append(rate)
            lout_init.append('! END further constant rate constants from optimization script\n')
            lout_init.append('\n')

# write optimization to files
write_file(frates, lout_rates)
write_file(finit,  lout_init)

