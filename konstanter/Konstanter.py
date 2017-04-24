#
# Konstanter.py
#
# - Fysiska konstanter för matematiska beräkningar
#
# Jonatan Sundqvist
# 23 april 2017

# TODO | - 
#        - 

# Importera moduler som behövs
from Enheter import *


# Klasser
class Planet(object):
  def __init__(denna, data):
    denna.namn        = data['namn']        # Vad heter planeten?
    denna.massa       = data['massa']       # Hur mycket väger planeten?
    denna.radie       = data['radie']       # Vilken radie har planeten?
    denna.solavstånd  = data['solavstånd']  # Hur stort är planetens avstånd från solen?
    denna.gravitation = data['gravitation'] # Hur stor är tyngdacceleration på planeten, i genomsnitt?
    denna.dygn        = data['dygn']        # Hur långt är ett dygn på planeten?


# Geometriska data
jorden = Planet({ 'namn':        'jorden',
                  'massa':       5.972e24     * kg,
                  'radie':       6371e3       * m,
                  'solavstånd':  149597870700 * m,
                  'gravitation': 9.82         * m/(s**2),
                  'dygn':        60*60*24     * s })


# Konstanter
Partikel = namedtuple('Partikel', ('m', 'Q'))

elektron = Partikel(9.10938356e-31 * kg, 1.602e-19 * C)

if __name__ == '__main__':
  print(jorden.gravitation * Storhet(5, sekund) * Storhet(5, sekund)/Storhet(2, ett))
  print('jorden.gravitation', jorden.gravitation)

  print()
