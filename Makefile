PKGNAME := konffaa
VERSION :=
DISTSUFFIX := $(and $(VERSION),-$(VERSION))

default : setup

-include local.mk

install :
	raco pkg install --name $(PKGNAME)

setup :
	raco setup $(PKGNAME)

api-doc :
	-rm -r doc
	scribble ++xref-in setup/xref load-collections-xref --html --dest doc --dest-name index.html manual-src/konffaa.scrbl

clean :
	find -name compiled -type d -print0 | xargs -0 --no-run-if-empty rm -r

check-pkg-deps :
	raco setup --check-pkg-deps $(PKGNAME)
