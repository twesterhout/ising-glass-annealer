FROM centos:7 as base

# Set an encoding to make things work smoothly.
ENV LANG en_US.UTF-8

# Add qemu in here so that we can use this image on regular linux hosts with qemu user installed
# ADD qemu-aarch64-static /usr/bin/qemu-aarch64-static
# ADD qemu-arm-static /usr/bin/qemu-arm-static
# ADD qemu-ppc64le-static /usr/bin/qemu-ppc64le-static
# ADD qemu-s390x-static /usr/bin/qemu-s390x-static

# bust the docker cache so that we always rerun the installs below
# ADD https://loripsum.net/api /opt/docker/etc/gibberish

# Resolves a nasty NOKEY warning that appears when using yum.
RUN rpm --import /etc/pki/rpm-gpg/RPM-GPG-KEY-CentOS-7

# Install basic requirements.
RUN yum update -y && \
    yum install -y \
    gcc \
    gcc-c++ \
    gmp \
    gmp-devel \
    make \
    ncurses \
    ncurses-compat-libs \
    xz \
    perl \
    sudo \
    tar \
    which && \
    yum clean all && \
    rm -rf /var/cache/yum/*

ENV GHCUP_INSTALL_BASE_PREFIX /opt
ENV CABAL_DIR /opt/cabal

RUN curl -s -L https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup

ENV PATH /opt/.ghcup/bin:$PATH

RUN ghcup install ghc 9.2.5 && \
    ghcup set ghc 9.2.5 && \
    ghcup install cabal 3.6.2.0 && \
    ghcup set cabal 3.6.2.0

RUN cabal update

RUN curl -s -L -O https://github.com/NixOS/patchelf/releases/download/0.17.2/patchelf-0.17.2-x86_64.tar.gz && \
    tar -x ./bin/patchelf -f patchelf-0.17.2-x86_64.tar.gz -C /usr && \
    rm patchelf-0.17.2-x86_64.tar.gz && \
    chmod +x /usr/bin/patchelf

FROM base as dependencies

RUN mkdir -p /work/ising-glass-annealer
ADD cabal.project               /work/ising-glass-annealer/
ADD ising-glass-annealer.cabal  /work/ising-glass-annealer/

WORKDIR /work/ising-glass-annealer

RUN cabal build --dependencies-only && \
    rm cabal.project ising-glass-annealer.cabal

CMD [ "/bin/bash", "-i" ]