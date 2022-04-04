import mesa_reader as mr
import numpy as np
import matplotlib.pyplot as plt

h= mr.MesaData('LOGS/history.data') 
ages = h.data('star_age')
logRadius = h.data('log_R')
logLS = h.data('log_L')
logTeff = h.data('log_Teff')
zoneNum = h.data('num_zones')

fig1, ax1 = plt.subplots()
ax1.plot(ages, logLS)
ax1.set_xlabel('age (yr)')
ax1.set_xscale('log')
ax1.set_ylabel('log L (solar)')
plt.savefig('L_vs_age.pdf')
plt.close(fig1)

fig2, ax2 = plt.subplots()
ax2.plot(ages, logTeff)
ax2.set_xlabel('age (yr)')
ax2.set_xscale('log')
ax2.set_ylabel('log effective temp')
plt.savefig('teff_vs_age.pdf')
plt.close(fig2)

fig3, ax3 = plt.subplots()
ax3.plot(ages, zoneNum)
ax3.set_xlabel('age (yr)')
ax3.set_xscale('log')
ax3.set_ylabel('number of zones')
plt.savefig('zones_vs_age.pdf')
plt.close(fig3)

fig4, ax4 = plt.subplots()
ax4.plot(ages, logRadius)
ax4.set_xlabel('age (yr)')
ax4.set_xscale('log')
ax4.set_ylabel('log Radius (solar)')
plt.savefig('Radius_vs_age.pdf')
plt.close(fig4)
