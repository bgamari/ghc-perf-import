PATH:=node_modules/.bin:${PATH}

all : bundle.js

bundle.js : ghc_perf.js
	npm install
	browserify $< -o $@ -t [ babelify --presets [ es2015 ] ]

install : bundle.js index.html ghc_perf.css spinner.css
	install -t ${DESTDIR} $+

