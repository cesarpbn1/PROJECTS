import numpy as np
import os
from conformer import *
from copy import copy as cp
from scipy import interpolate


class Space(list):

    '''A conformational space consisting of all conformers sfound in specified directory.
    The directory tree should have a structure:
    'molecule'/*/*log
    if directory 'molecule' holds a directory 'experimetnal', an attibute self.expIR is 
    created using the data found there. 
    for different molecules, different lists can (meaning should!) be made.'''

    _temp = 298.15
    _kT=0.0019872036*_temp
    _Ha2kcal=627.5095
    w_incr = 0.5

    def __init__(self, molecule, ir_resolution=1):
        
        self.ir_resolution = ir_resolution #check whether float will work here
        
        for (root, dirs, files) in os.walk('./'+molecule):
            for dirname in dirs:
                print dirname
                #oldername = os.path.basename(dirpath)
                if dirname == 'experimental':
                    self.ir_resolution = np.genfromtxt(molecule+'/'+dirname+'/exp.dat')
                        if self.ir_resultion[0]=="w_incr":
                        w_incr=float(line[1])
                        incr = 0.1*w_incr
                        grid_old = numpy.arange(0,len(integrand))*w_incr
                        grid_new = numpy.arange(grid_old[0],grid_old[-1]+incr,incr)
                        spl = interpolate.splrep(grid_old,integrand.T[1],k=3,s=0)
                        integrand_dense = interpolate.splev(grid_new,spl,der=0)
                for ifiles in os.walk(molecule+'/'+dirname):
                    for filename in ifiles[2]:
                        if filename.endswith('.log'):
                            self.append(Conformer(molecule+'/'+dirname+'/'+filename))

    def __str__(self):
         
        '''Prints a nice table with coded molecular values'''

        print "%20s%20s%20s%20s\n" %('id', 'E', 'H', 'F'),
        for conf in self: 
            print "%20s%20.2f%20.2f%20.2f\n" %(conf._id, conf.E*self._Ha2kcal, conf.H*self._Ha2kcal, conf.F*self._Ha2kcal),
        return ''

    def gaussian_broadening(self, broaden=5, self._ir_resolution):

        ''' Performs gaussian broadening for the set''' 

        for conf in self: conf.gaussian_broadening(broaden, resolution=self._ir_resolution)

    def reference_to_zero(self, energy_function='E'):

        '''Finds a conformer with the lowest specified energy function and 
        references remainins conformers to this.'''

        Eref = 0.0 ; Fref = 0.0 ; Href = 0.0 
        for conf in self: 
              if energy_function == 'E' and  conf.E < Eref: 
                    Eref = cp(conf.E) ; Href = cp(conf.H) ; Fref = cp(conf.F)
              elif energy_function == 'H' and  conf.H < Href: 
                    Eref = cp(conf.E) ; Href = cp(conf.H) ; Fref = cp(conf.F)
              elif energy_function == 'F' and  conf.F < Fref: 
                    Eref = cp(conf.E) ; Href = cp(conf.H) ; Fref = cp(conf.F)
        for conf in self: 
              conf.E -= Eref;  conf.H -= Href ;  conf.F -= Fref
