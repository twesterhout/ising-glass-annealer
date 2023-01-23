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
    install_requires=[
        "cffi>=1.15.0",
        "numpy>=1.19.0",
        "scipy",
    ],
    package_data={"ising_glass_annealer": ["lib/**"]},
    zip_safe=False,
)
