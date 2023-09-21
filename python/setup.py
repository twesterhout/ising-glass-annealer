from setuptools import setup, Extension
import os
import re


def get_version(package):
    pwd = os.path.dirname(os.path.realpath(__file__))
    with open(os.path.join(pwd, package, "__init__.py"), "r") as input:
        result = re.search(r'__version__\s*=\s*[\'"]([^\'"]*)[\'"]', input.read())
    if not result:
        raise ValueError("failed to determine {} version".format(package))
    return result.group(1)


setup(
    name="ising-glass-annealer",
    version=get_version("ising_glass_annealer"),
    description="See README.md",
    url="http://github.com/twesterhout/ising-glass-annealer",
    author="Tom Westerhout",
    author_email="14264576+twesterhout@users.noreply.github.com",
    license="BSD3",
    packages=["ising_glass_annealer"],
    setup_requires=["cffi>=1.15.0"],
    cffi_modules=["ising_glass_annealer/build_extension.py:ffibuilder"],
    install_requires=[
        "cffi>=1.15.0",
        "numpy>=1.23.0",
        "scipy>=1.8.0",
        "loguru",
    ],
    test_requires=["pytest", "h5py"],
    zip_safe=False,
)
