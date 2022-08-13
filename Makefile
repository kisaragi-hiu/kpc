dist: kpc.rkt cmd.rkt
	raco exe cmd.rkt
	mv cmd kpc
	raco distribute dist kpc

setup:
	raco setup kpc
