PKGNAME := konffaa
VERSION :=
DISTSUFFIX := $(and $(VERSION),-$(VERSION))
DISTNAME := $(PKGNAME)$(DISTSUFFIX)
DISTHOME := $(PWD)/dist

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

MIRROR_DIR := /tmp/raco-tmp/$(PKGNAME)

# this indirection ensures that we only get what we would have in a Git repo
# (using 'git archive' would be more straightforward, but for future proofing)
pkg :
	-mkdir $(DISTHOME)
	-rm -r $(MIRROR_DIR)
	mkdir -p $(MIRROR_DIR)
	cp -ai ./ $(MIRROR_DIR)/
	( cd $(MIRROR_DIR) && git clean -dxff && rm -rf $(MIRROR_DIR)/.git && raco pkg create --format tgz --dest $(DISTHOME) --from-dir $(MIRROR_DIR) )

gh-homepage :
	( cd gh-pages && git clean -d -f && git rm --ignore-unmatch -rf . )
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --html --dest gh-pages --dest-name gh-pages/index.html manual-src/konffaa.scrbl
	( cd gh-pages && git add . && git status )

gh-upload :
	( cd gh-pages && git commit -m "update $$(date -u)" && git push --set-upstream github gh-pages:gh-pages )
