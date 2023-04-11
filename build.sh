#!/usr/bin/env bash
# Experimental static build script

set -x

stack_build() {
	os="$1"
	stack build
	code=$?
	[[ $code == 0 ]] || exit $code
	bindir=$(find .stack-work -type d -name bin | sort | tail -n1)
	cp "${bindir}/gander" gander-static-${os}
}

use_cachix() {
	nix-env -iA cachix -f https://cachix.org/api/v1/install
	cachix use static-haskell-nix
}

nix_build() {
	os="$1"
	use_cachix
	nix build
	code=$?
	[[ $code == 0 ]] || exit $code
	result=$(readlink result)
	cp ${result}/bin/gander gander-static-${os}

}

case $(uname) in
	Darwin) stack_build macos;;
	Linux) nix_build linux;;
	*) echo "unknown OS! trying nix build"; nix_build unknown;;
esac
