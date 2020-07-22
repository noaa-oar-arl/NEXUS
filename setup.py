try:
    from setuptools import setup, find_packages
except ImportError:
    from distutils.core import setup

setup(
    name='dust_map',
    version='0.0.1',
    url='https://github.com/bbakernoaa/bs_map',
    license='GNU',
    include_package_data=True,
    author='Barry D. Baker, Kerstin Schepanski',
    author_email='barry.baker@noaa.gov',
    maintainer='Barry Baker',
    maintainer_email='barry.baker@noaa.gov',
    packages=find_packages(),
    keywords=['dust', 'model', 'satellite'],
    description='Sedimiment supply map generator from satellites',
    install_requires=[
        'netcdf4', 'xarray', 'numpy', 'argparse', 'pandas'
    ])
