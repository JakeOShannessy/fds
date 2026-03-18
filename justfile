# set windows-powershell := true
set shell := ["cmd.exe", "/c"]
alias b := build
alias br := build-release

_default:
    @just --list

# Run the tests
test:
    ctest --test-dir cbuild

# Build the debug binaries
build:
    cmake -B cbuild -DCMAKE_BUILD_TYPE=Debug
    cmake --build cbuild -j6 --verbose --config Debug
    cmake --install cbuild --prefix dist-debug --config Debug

build-intel:
    cmake -B cbuild -DCMAKE_BUILD_TYPE=Debug -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icx -GNinja -DUSE_HYPRE=OFF -DUSE_SUNDIALS=OFF -DDUMP_JSON=ON
    cmake --build cbuild -j6 --verbose --config Debug
    cmake --install cbuild --prefix dist-debug --config Debug

build-release-intel:
    cmake -B cbuild -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icx -GNinja -DUSE_HYPRE=OFF -DUSE_SUNDIALS=OFF -DDUMP_JSON=ON
    cmake --build cbuild -v -j6 --config Release
    cmake --install cbuild --prefix dist --config Release

# Build the release binaries
build-release:
    cmake -B cbuild -DCMAKE_BUILD_TYPE=Release
    cmake --build cbuild -v -j6 --config Release
    cmake --install cbuild --prefix dist --config Release

# Build release and create MSI package
package-windows: build-release-intel
    wix build .\FdsVerifyInstaller.wxs  -out FdsVerifyInstaller_unsigned.msi

# Build release and create RPM package
package-rpm:
    ./buildrpm.sh

# Clean everything
clean:
    cmake -E rm -rf cbuild
