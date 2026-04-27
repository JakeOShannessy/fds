#!/bin/bash

temp_fds_repo=$(realpath "$(mktemp -d ./temp-fds.XXXXXX)")

git clone https://github.com/JakeOShannessy/fds.git --branch new-vv --depth 1 "$temp_fds_repo"

mkdir -p fds-local
cp -r "$temp_fds_repo"/Verification fds-local/

mkdir -p fds-nist
cp -r "$temp_fds_repo"/Verification fds-nist/

pushd fds-local/Verification || exit 1
QFDS="$temp_fds_repo/Utilities/Scripts/qfds_simple.sh -q sc-part -j local_" ./FDS_Cases.sh
popd || exit 1

pushd fds-nist/Verification || exit 1
QFDS="$temp_fds_repo/Utilities/Scripts/qfds_simple_nist.sh -q sc-part -j nist_" ./FDS_Cases.sh
popd || exit 1
