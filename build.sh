#!/usr/bin/env bash

# Experimental static build script

# If the build fails with 'no space left on device', you may need to enlarge or
# move TMPDIR. Ideally put it in RAM. Example:
# sudo mount -o remount,size=30G,nr_inodes=10000000 /run/user/1000
# export TMPDIR=/run/user/1000

set -x

stack_build() {
	os="$1"
	stack build
	code=$?
	[[ $code == 0 ]] || exit $code
	bindir=$(find .stack-work -type d -name bin | sort | tail -n1)
	rm -f gander-${os}
	cp "${bindir}/gander" gander-${os}
}

# TODO remove? not getting cache hits anymore
use_cachix() {
	which cachix || nix-env -iA cachix -f https://cachix.org/api/v1/install
	cachix use static-haskell-nix
}

nix_build() {
	os="$1"
	use_cachix
	nix build
	code=$?
	[[ $code == 0 ]] || exit $code
	rm -f gander-${os}
	cp ./result/bin/gander gander-${os}

}

case $(uname) in
	Darwin) stack_build mac && otool -L gander-mac;;
	Linux) nix_build linux && ldd gander-linux;;
	*) echo "unknown OS! trying nix build"; nix_build unknown;;
esac
