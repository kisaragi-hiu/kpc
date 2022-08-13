compile:
	scribble +m ++style custom.css --redirect-main "https://docs.racket-lang.org" --html --dest ../docs kpc.scrbl
	-echo # ugh.
	sed -i s/Cooper-Hewitt/Inter/g		../docs/manual-style.css
	sed -i "s/Fira-Mono/Iosevka/g"		../docs/manual-style.css
	sed -i "s/Fira/Source Sans Pro/g"	../docs/manual-style.css
	sed -i "s/Charter-Racket/Equity A/g"	../docs/manual-style.css
	sed -i s/@import.*//g				../docs/manual-style.css

dev:
	npx live-server ../docs
