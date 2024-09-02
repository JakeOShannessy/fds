set windows-powershell := true
alias b := build
alias br := build-release

_default:
    @just --list

# Run the tests
test:
    ctest --test-dir cbuild -C Debug

# Build the debug binaries
build:
    cmake -B cbuild -DCMAKE_BUILD_TYPE=Debug -G Ninja
    cmake --build cbuild --config Debug -j6 -v
    cmake --install cbuild --config Debug --prefix dist-debug

# Build the release binaries
build-release:
    cmake -B cbuild -DCMAKE_BUILD_TYPE=Release -G Ninja
    cmake --build cbuild --config Release -v -j6
    cmake --install cbuild --config Release --prefix dist

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
