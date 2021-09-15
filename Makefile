
HSC     = ghc
WARNS   = -Wall -Wno-tabs -Wno-name-shadowing
HSFLAGS = -O2 $(WARNS)
LDFLAGS = -dynamic

.PHONY: base doc clean

base: base.hs
	+$(HSC) -j --make $(HSFLAGS) $(LDFLAGS) -o $@ $^

doc:
	haddock -o doc --html --package-name Data.String.Conv.Base base.hs

clean:
	find -type f -name '*.o' -or -name '*.hi' -delete
