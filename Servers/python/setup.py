'''
Python distutils setup file for installing multicopter_sim

Copyright(C) 2019 Simon D.Levy

MIT License
'''

from distutils.core import setup

setup(name='MulticopterSim',
      packages=['multicopter_server'],
      version='0.1',
      description='Talk to MulticopterSim over sockets',
      author_email='simon.d.levy@gmail.com',
      url='https://github.com/simondlevy/MulticopterSim/Extras/python',
      license='MIT',
      platforms='Linux; Windows')
