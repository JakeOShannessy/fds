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
    cmake -B cbuild -DCMAKE_BUILD_TYPE=Debug -G Ninja
    cmake --build cbuild -j6
    cmake --install cbuild --prefix dist-debug

# Build the release binaries
build-release:
    cmake -B cbuild -DCMAKE_BUILD_TYPE=Release -G Ninja
    cmake --build cbuild -v -j6
    cmake --install cbuild --prefix dist

# Build release and create MSI package
package-windows:
    candle "FdsVerifyInstaller.wxs"
    light "FdsVerifyInstaller.wixobj"

# Build release and create RPM package
package-rpm:
    ./buildrpm.sh

# Clean everything
clean:
    cmake -E rm -rf cbuild
