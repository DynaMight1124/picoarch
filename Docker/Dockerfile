# Containerfile to build PicoArch in the FunKey-S v2.3.0 dev environment
FROM debian:bookworm-slim

LABEL maintainer="you@example.com"

# 1. Install basics
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      ca-certificates wget rsync tar file build-essential \
      git pkg-config patch \
      libsdl1.2-dev libasound2-dev libncurses-dev squashfs-tools curl imagemagick zip && \
    rm -rf /var/lib/apt/lists/*

# 2. Download and extract the FunKey-S toolchain release
ARG TOOLCHAIN_URL="https://github.com/DrUm78/FunKey-OS/releases/download/FunKey-OS-DrUm78/FunKey-sdk-2.3.0.tar.gz"
RUN mkdir -p /opt && \
    wget -qO /opt/toolchain.tar.gz ${TOOLCHAIN_URL} && \
    echo "Toolchain downloaded, extracting..." && \
    tar zxvf /opt/toolchain.tar.gz -C /opt && \
    rm /opt/toolchain.tar.gz

# 3. Relocate the SDK
WORKDIR /opt/FunKey-sdk-2.3.0
RUN chmod +x relocate-sdk.sh && ./relocate-sdk.sh

# 4. Export FunKey-S cross-compile environment permanently
ENV SDK_ROOT="/opt/FunKey-sdk-2.3.0"
ENV TOOLCHAIN_PREFIX="arm-funkey-linux-musleabihf"
ENV TOOLBIN="${SDK_ROOT}/bin/${TOOLCHAIN_PREFIX}-"
ENV SYSROOT="${SDK_ROOT}/${TOOLCHAIN_PREFIX}/sysroot"
ENV CROSS_COMPILE="${TOOLBIN}"
ENV CC="${TOOLBIN}gcc"
ENV CXX="${TOOLBIN}g++"
ENV LD="${TOOLBIN}ld"
ENV AS="${TOOLBIN}as"
ENV AR="${TOOLBIN}ar"
ENV RANLIB="${TOOLBIN}ranlib"
ENV STRIP="${TOOLBIN}strip"
# Set pkg-config to look in the SDK's sysroot
ENV PKG_CONFIG_PATH="${SYSROOT}/usr/lib/pkgconfig"
ENV PKG_CONFIG_LIBDIR="${SYSROOT}/usr/lib/pkgconfig"
ENV PKG_CONFIG_SYSROOT_DIR="${SYSROOT}"
# Add the toolchain's bin directory to the main PATH
ENV PATH="${SDK_ROOT}/bin:${PATH}"

# 5. Download and build libpng12 using the cross-compiler
WORKDIR /build
RUN wget https://ftp-osl.osuosl.org/pub/libpng/src/libpng12/libpng-1.2.59.tar.gz && \
    tar -xvzf libpng-1.2.59.tar.gz && \
    cd libpng-1.2.59 && \
    # We explicitly set the cross-compilers for configure
    CC="${CC}" \
    AR="${AR}" \
    RANLIB="${RANLIB}" \
    ./configure \
      --host=${TOOLCHAIN_PREFIX} \
      --prefix=${SYSROOT}/usr && \
    make && \
    make install && \
    cd .. && \
    rm -rf libpng-1.2.59 libpng-1.2.59.tar.gz

# 6. Clone the PicoArch repo
WORKDIR /build
RUN git clone --recurse-submodules https://github.com/DrUm78/picoarch

# 7. Set build directory ---
# Set the working directory for the make command
WORKDIR /build/picoarch

# 8. Setup final workspace for the user
# The compiled files will be created in /build/picoarch/
WORKDIR /workspace
VOLUME /workspace

# 9. Default to running the build
WORKDIR /build/picoarch
CMD ["bash", "-c", "echo '--- Pulling latest changes from Git ---' && \
                     git pull --rebase && \
                     git submodule update --init --recursive && \
                     echo '--- Starting build ---' && \
                     make platform=funkey-s picoarch-funkey-s.zip cores-funkey-s.zip && \
                     echo '--- Copying PicoArch & Cores to /workspace ---' && \
                     cp -f picoarch-funkey-s.zip /workspace && \
                     cp -f cores-funkey-s.zip /workspace"]