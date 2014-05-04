help:;@grep -v ^help: Makefile

ghcVer := 7.6.3
packageDb := .cabal-sandbox/x86_64-linux-ghc-$(ghcVer)-packages.conf.d

export GHC_PACKAGE_PATH := $(packageDb):/usr/lib/ghc-$(ghcVer)/package.conf.d

ghci: $(packageDb)
	ghci -ilib -no-user-package-db -package-db $(packageDb)

push:
	git push github master
	git push destroy master

reload:
	beep -l 100 -f 2000
	xmonad --recompile && xmonad --restart \
		&& beep -l 100 -f 2000 -n -l 100 -f 4000 \
		|| beep -l 50 -f 200 -n -l 100 -f 100
