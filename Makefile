
HSC     = ghc
WARNS   = -Wall -Wno-tabs -Wno-name-shadowing -Wno-unused-top-binds
HSFLAGS = -O2 $(WARNS)
LDFLAGS = -dynamic

.PHONY: base doc

base: base.hs
	$(HSC) --make $(HSFLAGS) $(LDFLAGS) -o $@ $^

doc:
	haddock -o doc --html --package-name Data.String.Conv.Base base.hs
