dist: kpc.rkt cmd.rkt
	raco exe cmd.rkt
	mv cmd kpc
	raco distribute dist kpc

compile:
	scribble --html --dest docs kpc.scrbl
