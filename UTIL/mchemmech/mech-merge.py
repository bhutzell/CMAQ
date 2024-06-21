# Import necessary modules

# Define functions
def read_spc(filename):
    var_spc = []
    fix_spc = []
    with open(filename) as infile:
        var = False
        fix = False
        for line in infile:
            if '#DEFVAR' in line.strip():
                var = True
                fix = False
                continue
            if '#DEFFIX' in line.strip():
                var = False
                fix = True
                continue
            elif var:
                add = True
                # exclude empty lines and comments
                if line.strip() == '':
                    add = False
                elif line.strip()[0] == '!':
                    add = False
                if add: var_spc.append(line.strip())
            elif fix:
                add = True
                # exclude empty lines and comments
                if line.strip() == '':
                    add = False
                elif line.strip()[0] == '!':
                    add = False
                if add: fix_spc.append(line.strip())
    return var_spc, fix_spc

# define mechanism
filename = 'aqueous.eqn'

# load mechanism
mechin = []
with open(filename) as infile:
    copy = False
    for line in infile:
        if '#EQUATIONS' in line.strip():
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

mech = []
mech.append('// Resolved clouds')
for rx in mechin:
    mech.append(rx.replace('a##','RS'))
mech.append('') # Add empty line for better readability
mech.append('// Convective clouds')
for rx in mechin:
    mech.append(rx.replace('a##','CV'))


# write mechanism to file
outfile = open('mchem.eqn','w')
with open('gas.eqn') as infile:
    for line in infile:
        if '#EQUATIONS' in line:
            with open('inline_rconst.kpp') as infile_rconst:
                for line_rconst in infile_rconst:
                    outfile.write(line_rconst)
        outfile.write(line)
outfile.write('\n')
outfile.write('// In-cloud chemistry\n')
for line in mech:
    outfile.write(line+'\n')
outfile.close()

# write spc to file
gas_var, gas_fix = read_spc('gas.spc')
aq_var,  aq_fix  = read_spc('aqueous.spc')

mchem_var = gas_var
for var in aq_var:
    if 'a##' in var:
        mchem_var.append(var.replace('a##','RS'))
        mchem_var.append(var.replace('a##','CV'))
    else:
        mchem_var.append(var)

mchem_fix = gas_fix
for fix in aq_fix:
    if 'a##' in fix:
        mchem_fix.append(fix.replace('a##','RS'))
        mchem_fix.append(fix.replace('a##','CV'))
    else:
        mchem_fix.append(fix)

outfile = open('mchem.spc','w')
outfile.write('#INCLUDE atoms\n')
outfile.write('#DEFVAR\n')
for var in mchem_var:
    outfile.write(var+'\n')
outfile.write('#DEFFIX\n')
for fix in mchem_fix:
    outfile.write(fix+'\n')
outfile.close()
