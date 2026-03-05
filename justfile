set windows-powershell := true
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
    cmake -B cbuild -DCMAKE_BUILD_TYPE=Debug -DCMAKE_Fortran_COMPILER=ifx
    cmake --build cbuild -j6 --verbose --config Debug
    cmake --install cbuild --prefix dist-debug --config Debug

# Build the release binaries
build-release:
    cmake -B cbuild -DCMAKE_BUILD_TYPE=Release
    cmake --build cbuild -v -j6 --config Release
    cmake --install cbuild --prefix dist --config Release

# Build release and create MSI package
package-windows: build-release
    candle "FdsVerifyInstaller.wxs"
    light "FdsVerifyInstaller.wixobj"

# Build release and create RPM package
package-rpm:
    ./buildrpm.sh

# Clean everything
clean:
    cmake -E rm -rf cbuild
