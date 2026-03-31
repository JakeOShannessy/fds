#!/bin/bash

QFDS="../Utilities/Scripts/qfds_simple.sh -q sc-part -j local_" ./FDS_Cases.sh
QFDS="../Utilities/Scripts/qfds_simple_nist.sh -q sc-part -j nist_" ./FDS_Cases.sh
