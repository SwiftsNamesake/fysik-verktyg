#
# konstanter.py
#
# - Enheter och storheter
# - Fysiska konstanter för matematiska beräkningar
#
# Jonatan Sundqvist
# 31 mars 2017

# TODO | - Signifikanta siffror, noggrannhet, ungefära jämförelser
#        - Prestanda (memoisering, osv.)
#        - Konvertera mellan enheter
#        - Tillåt a + b när a och b har samma storhet
#        - Namnge härledda enheter och storheter (tabell)
#        - Namnge kombinationer av härledda enheter
#        - Läs ut enhetsuttryck (ex. meter per sekund-kvadrat, Pascal)
#        - Tolka enhetsuttryck (ex. N*m^(-2)), inklusive prefix
#        - Typsäkerhet (i synnerhet för heltalsexponenter)
#        - Unicode-exponenter

# Importera moduler som behövs
from enum        import Enum
from collections import OrderedDict, namedtuple
from itertools   import chain
from operator    import add as addera, sub as subtrahera, mul as multiplicera
from math        import sin, cos, tan, asin, acos, atan, copysign, floor, ceil, e, radians as radianer, degrees as grader, pi as π

import cmath

# Klasser
class Grundstorheter(Enum):
  Längd      = 0
  Tid        = 1
  Massa      = 2
  Ström      = 3
  Temperatur = 4


SI = namedtuple('SI', ('symbol', 'storhet'))


class Grundenheter(Enum):
  # TODO: Typer för grader (?)
  meter    = SI('m',  Grundstorheter.Längd)      # 
  sekund   = SI('s',  Grundstorheter.Tid)        # 
  kilogram = SI('kg', Grundstorheter.Massa)      # 
  ampere   = SI('A',  Grundstorheter.Ström)      # 
  kelvin   = SI('K',  Grundstorheter.Temperatur) # 


# class HärleddaEnheter(Enum):


class Enhet(object):

  ''' Representerar en härledd eller fundamental enhet '''

  # TODO: Bör värdet vara en del av enheten för den dimensionslösa 'enheten'
  # TODO: Bättre sätt att hantera argument-typer

  def __init__(denna, enhet):
    # TODO: Döp om 'enhet' för att undvika missförstånd (klassen heter ju samma sak...)
    denna.enhet = enhet

  def förenkla(denna):
    pass

  def visa(denna):
    # TODO: Förbättra...
    if denna.enhet == ett.enhet:
      return '1'
    else:
      exp = lambda n: ['^('+str(n)+')', '', '^'+str(n)][1+(0 if n == 1 else floor(copysign(1,n)))]
      return '*'.join('{0}{1}'.format(si.value.symbol, exp(n)) for si,n in denna.enhet.items())
  
  def kombinera(denna, andra, op):
    ''' Kombinerar två enheter med den givna operatorn '''
    # TODO: Förenkla och bryt ut logik (kanske behöver vi en särskild datastruktur, jag saknar Haskell just nu)
    # TODO: Flytta negative exponeneter
    snitt     = ((si, op(a, andra.enhet.get(si,0))) for si,a in denna.enhet.items() if op(a,andra.enhet.get(si,0)) != 0)
    differens = ((si, op(0, n)) for si,n in andra.enhet.items() if si not in denna.enhet)
    return Enhet(OrderedDict(chain(snitt, differens)))
  
  # def storhet(denna):

  def __eq__(denna, andra):
    # TODO: Typer
    # TODO: Städa
    return (denna is andra) or  (all((k in andra.enhet) and (a == andra.enhet[k]) for k,a in denna.enhet.items())
                            and  all((k in denna.enhet) and (a == denna.enhet[k]) for k,a in andra.enhet.items()))

  def __add__(denna, andra):
    return self # Vi tar för givet att enheterna stämmer

  def __sub__(denna, andra):
    return self # Vi tar för givet att enheterna stämmer

  def __mul__(denna, andra):
    return denna.kombinera(andra, addera)

  # def __rmul__(denna, andra):
  #   print(denna, '__rmul__')
  #   pass

  def __truediv__(denna, andra):
    return denna.kombinera(andra, subtrahera)

  # def __rtruediv__(denna, andra):
  #   pass

  def __pow__(denna, n):
    # TODO: Noggrannare typ-hantering, men tillåt duck-typing
    if any(isinstance(n, t) for t in (int, float)):
      return Enhet(OrderedDict((si,a*n) for si,a in denna.enhet.items()))
    else:
      # TODO: Bättre meddelande
      raise ValueError('En storhet med enheten {0} kan inte stå som exponent.'.format(n))

  def __rpow__(denna, n):
    pass

  def __str__(denna):
    return denna.visa()


class Storhet(object):

  ''' Representerar en storhet, det vill säga en egenskap som beskrivs med ett värde och en enhet. '''

  # TODO: Döp om (ex. Kvantitet, Dimensionerad, Värde)
  def __init__(denna, v, e):
    denna.värde = v
    denna.enhet = e

  def visa(denna):
    return '{0} {1}'.format(denna.värde, denna.enhet)

  def __eq__(denna, andra):
    # TODO: Typer
    return (denna.värde == andra.värde) and (denna.enhet == andra.enhet)

  def __add__(denna, andra):
    if denna.enhet == andra.enhet:
      raise ValueError('Kan inte addera {0} med {1}'.format(denna.enhet, andra.enhet))
    else:
      return Storhet(denna.värde + andra.värde, denna.enhet + andra.enhet)

  def __sub__(denna, andra):
    if denna.enhet == andra.enhet:
      raise ValueError('Kan inte subtrahera {0} med {1}'.format(denna.enhet, andra.enhet))
    else:
      return Storhet(denna.värde - andra.värde, denna.enhet - andra.enhet)

  def __mul__(denna, andra):
    return Storhet(denna.värde * andra.värde, denna.enhet * andra.enhet)

  # def __rmul__(denna, andra):
  #   pass

  def __truediv__(denna, andra):
    return Storhet(denna.värde / andra.värde, denna.enhet / andra.enhet)

  # def __rtruediv__(denna, andra):
  #   pass

  def __str__(denna):
    return denna.visa()



class Konstant(object):

  def __init__(denna, storhet, beskrivning):
    denna.storhet     = storhet
    denna.beskrivning = besrivning



class Planet(object):
  def __init__(denna, data):
    denna.namn        = data['namn']        # Vad heter planeten?
    denna.massa       = data['massa']       # Hur mycket väger planeten?
    denna.radie       = data['radie']       # Vilken radie har planeten?
    denna.solavstånd  = data['solavstånd']  # Hur stort är planetens avstånd från solen?
    denna.gravitation = data['gravitation'] # Hur stor är tyngdacceleration på planeten, i genomsnitt?
    denna.dygn        = data['dygn']        # Hur långt är ett dygn på planeten?


# Enheter
ett      = Enhet(OrderedDict([]))
meter    = Enhet(OrderedDict([(Grundenheter.meter,    1)]))
sekund   = Enhet(OrderedDict([(Grundenheter.sekund,   1)]))
kilogram = Enhet(OrderedDict([(Grundenheter.kilogram, 1)]))
ampere   = Enhet(OrderedDict([(Grundenheter.ampere,   1)]))
kelvin   = Enhet(OrderedDict([(Grundenheter.kelvin,   1)]))

hertz    = sekund**(-1)
newton   = kilogram*meter/sekund**2
pascal   = newton/meter**2
joule    = newton*meter
watt     = joule/sekund
coulomb  = sekund*ampere
volt     = newton*meter/coulomb
farad    = kilogram**(-1)*meter**(-2)*sekund**4*ampere**2 # TODO: Förenkla
ohm      = kilogram*meter**2*sekund**(-3)*ampere**(-2)    # TODO: Förenkla
siemens  = kilogram**(-1)*meter**(-2)*sekund**3*ampere**2 # TODO: Förenkla
weber    = kilogram*meter**2*sekund**(-2)*ampere**(-1)    # TODO: Förenkla
tesla    = kilogram*sekund**(-2)*ampere**(-1)             # TODO: Förenkla
henry    = kilogram*meter**2*sekund**(-2)*ampere**(-2)    # TODO: Förenkla

# Enheter (kortnamn)
m  = meter
s  = sekund
kg = kilogram
A  = ampere
K  = kelvin

Hz = hertz
N  = newton
Pa = pascal
J  = joule
W  = watt
C  = coulomb
V  = volt
# f = farad
# o = ohm
# s = siemens
# w = weber
# t = tesla
# h = henry

# Geometriska data
jorden = Planet({ 'namn':        'jorden',
                  'massa':       Storhet(5.972e24,     kilogram),
                  'radie':       Storhet(6371e3,       meter),
                  'solavstånd':  Storhet(149597870700, meter),
                  'gravitation': Storhet(9.82,         meter/(sekund**2)),
                  'dygn':        Storhet(60*60*24,     sekund) })

# Jordens gravitation

# Här testar vi funktionerna i modulen
if __name__ == '__main__':
  print(jorden.gravitation)
  # print(5*meter)
  print(newton/kilogram, joule/kilogram)
  print(newton.kombinera(newton, addera))
  print(jorden.gravitation * Storhet(5, sekund) * Storhet(5, sekund)/Storhet(2, ett))

  print('FJÄRDAR')
  print((N/m/kg)**(1/2)*s, '(y)')
  print((N/m/kg)**(1/2)*m, '(v)')
  print((N/m/kg)*m,        '(a)')