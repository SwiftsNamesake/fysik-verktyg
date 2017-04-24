#
# Enheter.py
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
#        - Matematiska funktioner (trig, log, sqrt, etc.)

# Importera moduler som behövs
from enum        import Enum
from collections import namedtuple
from numbers     import Number
from operator    import add as addera, sub as subtrahera, mul as multiplicera
from math        import sin, cos, tan, asin, acos, atan, copysign, sqrt, floor, ceil, e, radians as radianer, degrees as grader, pi as π, pi

import cmath


# Klasser
class Grundstorheter(Enum):
  Längd      = 0
  Tid        = 1
  Massa      = 2
  Ström      = 3
  Temperatur = 4


SI = namedtuple('SI', ('symbol', 'storhet'))


# class Grundenheter(Enum):
#   # TODO: Typer för grader (?)
#   meter    = SI('m',  Grundstorheter.Längd)      # 
#   sekund   = SI('s',  Grundstorheter.Tid)        # 
#   kilogram = SI('kg', Grundstorheter.Massa)      # 
#   ampere   = SI('A',  Grundstorheter.Ström)      # 
#   kelvin   = SI('K',  Grundstorheter.Temperatur) # 


# class HärleddaEnheter(Enum):


class Enhet(object):

  ''' Representerar en härledd eller fundamental enhet '''

  # TODO: Bör värdet vara en del av enheten för den dimensionslösa 'enheten'
  # TODO: Bättre sätt att hantera argument-typer

  Exponenter = namedtuple('Exponenter', ('meter', 'sekund', 'kilogram', 'ampere', 'kelvin', 'candela'))
  symboler   = Exponenter('m', 's', 'kg', 'A', 'K', 'Ca')

  def __init__(denna, exponenter):
    # TODO: Döp om 'exponenter' för att undvika missförstånd (klassen heter ju samma sak...)
    assert isinstance(exponenter, Enhet.Exponenter), 'Argumentet \'exponenter\' bör vara av typ Enhet.Exponenter (inte {})'.format(type(exponenter))
    denna.exponenter = exponenter

  def visa(denna):
    # TODO: Hur gör vi med 'ett'?
    # TODO: Valfrihet
    behandlad = filter(lambda t: t[1] != 0, sorted(zip(Enhet.symboler, denna.exponenter), key=lambda t: -t[1]))
    grund     = (0, '*'.join('{0}{1}'.format(enhet, ('^'+str(exp)) if exp != 1 else '') for enhet, exp in behandlad), 0)
    return enhetsnamn.get(denna, grund)[1]
  
  def __eq__(denna, andra):
    return isinstance(denna, Enhet) and isinstance(andra, Enhet) and (denna.exponenter == andra.exponenter)

  def __neg__(denna):
    return Enhet(Enhet.Exponenter(*(-exp for exp in denna.exponenter)))

  def __mul__(denna, andra):
    if isinstance(andra, Enhet):
      return Enhet(Enhet.Exponenter(*(m+n for m,n in zip(denna.exponenter, andra.exponenter))))
    elif isinstance(andra, Number):
      return Storhet(andra, denna)
    else:
      raise ValueError('Båda faktorer måste vara av typ {} eller {}'.format(Enhet, Number))
  
  def __rmul__(denna, andra):
    return denna * andra # TODO: Funkar detta

  def __truediv__(denna, andra):
    return denna * (-andra)

  def __pow__(denna, n):
    # TODO: Noggrannare typ-hantering, men tillåt duck-typing
    # TODO: Se till att alla exponenter är heltal (efteråt)
    L, H = isinstance(denna, Enhet), (isinstance(n, Number) or (n == ett))
    if L and H:
      n = n.värde if n == ett else n
      return Enhet(Enhet.Exponenter(*(m*n for m in denna.exponenter)))
    elif not L:
      raise ValueError('Basen måste vara av typ Enhet (inte {})'.format(type(denna)))
    else:
      raise ValueError('Exponenten måste vara ett nummer (inte {})'.format(type(n)))

  def __hash__(denna):
    return hash(denna.exponenter)

  def __str__(denna):
    return denna.visa()

  def __repr__(denna):
    return 'Enhet({0})'.format(denna.visa())


class Storhet(namedtuple('StorhetsBas', ('värde', 'enhet'))):

  ''' Representerar en storhet, det vill säga en egenskap som beskrivs med ett värde och en enhet. '''

  # TODO: Döp om (ex. Kvantitet, Dimensionerad, Värde)

  def visa(denna):
    return '{0} {1}'.format(denna.värde, denna.enhet)
  
  def __neg__(denna):
    return Storhet(-denna.värde, denna.enhet)

  def __add__(denna, andra):
    assert isinstance(andra, Storhet) # TODO: Meddelande
    if denna.enhet == andra.enhet:
      return Storhet(denna.värde + andra.värde, denna.enhet)
    else:
      raise ValueError('Kan inte addera värden med olika enheter')

  def __sub__(denna, andra):
    return denna + (-andra)

  def __mul__(denna, andra):
    if isinstance(andra, Storhet):
      return Storhet(denna.värde * andra.värde, denna.enhet * andra.enhet)
    elif isinstance(andra, Enhet):
      return Storhet(denna.värde, denna.enhet*andra)
    elif isinstance(andra, Number):
      return Storhet(denna.värde * andra, denna.enhet)
    else:
      raise ValueError('Kan inte multiplicera med {}'.format(type(andra)))

  def __truediv__(denna, andra):
    # return denna * (andra)**(-1)
    if isinstance(andra, Storhet):
      return Storhet(denna.värde / andra.värde, denna.enhet / andra.enhet)
    elif isinstance(andra, Enhet):
      return Storhet(denna.värde, denna.enhet/andra)
    elif isinstance(andra, Number):
      return Storhet(denna.värde/andra, denna.enhet)
    else:
      raise ValueError('Kan inte dividera med {}'.format(type(andra)))

  def __pow__(denna, n):
    return Storhet(denna.värde**n, denna.enhet**n)

  def __rmul__(denna, andra):
    return denna*andra

  # def __rtruediv__(denna, andra):
  #   pass

  def __str__(denna):
    return denna.visa()

  def __repr__(denna):
    return denna.visa()


class Konstant(object):

  def __init__(denna, storhet, beskrivning):
    denna.storhet     = storhet
    denna.beskrivning = beskrivning


# Enheter
ett      = Enhet(Enhet.Exponenter(0,0,0,0,0,0))
meter    = Enhet(Enhet.Exponenter(1,0,0,0,0,0))
sekund   = Enhet(Enhet.Exponenter(0,1,0,0,0,0))
kilogram = Enhet(Enhet.Exponenter(0,0,1,0,0,0))
ampere   = Enhet(Enhet.Exponenter(0,0,0,1,0,0))
kelvin   = Enhet(Enhet.Exponenter(0,0,0,0,1,0))
candela  = Enhet(Enhet.Exponenter(0,0,0,0,0,1))

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

# Härledda enheter med särskilda namn
# TODO | - Härledda storheter med särskilda namn (särskild klass, kanske namedtuple)
#        - Stöd för flerspråkighet och konfigurering
#        - namedtuple
#        - Fler enheter
#        - Fyll i förkortningar som saknas
enhetsnamn = {
  hertz   : ('Hertz'   , 'Hz', 'frekvens'),
  newton  : ('Newton'  , 'N',  'kraft'),
  pascal  : ('Pascal'  , 'Pa', 'tryck'),
  joule   : ('Joule'   , 'J',  'energi'),
  watt    : ('Watt'    , 'W',  'effekt'),
  coulomb : ('Coulomb' , 'C',  'laddning'),
  volt    : ('Volt'    , 'V',  'spänning'),
  farad   : ('Farad'   , 'F',  'kapacitans'),
  ohm     : ('Ohm'     , 'Ω',  'resistans'),  # TODO: Detta kan vara farligt (för idiotiska ASCII-konsoler)...
  siemens : ('Siemens' , '???', 'konduktans'),
  weber   : ('Weber'   , '???', 'magnetiskt flöde'),
  tesla   : ('Tesla'   , '???', 'magnetisk flödestäthet'),
  henry   : ('Henry'   , '???', 'induktans')
}


# Prefix
yotta = 10e24
zetta = 10e21
exa   = 10e18
peta  = 10e15
tera  = 10e12
giga  = 10e09
mega  = 10e06
kilo  = 10e03
hekto = 10e02
deka  = 10e01

deci  = 10e-01
centi = 10e-02
milli = 10e-03
mikro = 10e-06
nano  = 10e-09
piko  = 10e-12
femto = 10e-15
atto  = 10e-18
zepto = 10e-21
yokto = 10e-24


# Här testar vi funktionerna i modulen
if __name__ == '__main__':
  # print(5*meter)
  print(newton/kilogram, joule/kilogram)

  print('FJÄRDAR')
  print((N/m/kg)**(1/2)*s, '(y)')
  print((N/m/kg)**(1/2)*m, '(v)')
  print((N/m/kg)*m,        '(a)')