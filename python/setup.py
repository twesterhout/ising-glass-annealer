from setuptools import setup
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
    name="ising-ground-state",
    version=get_version("ising_ground_state"),
    description="See README.md",
    url="http://github.com/twesterhout/ising-ground-state",
    author="Tom Westerhout",
    author_email="14264576+twesterhout@users.noreply.github.com",
    license="BSD3",
    packages=["ising_ground_state"],
    install_requires=["numpy", "scipy"],
    zip_safe=False,
)
