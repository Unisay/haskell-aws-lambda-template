all:
	@mkdir -p build
	@rm -rf ./build/*
	@stack clean
	@stack build
	@cp `stack path --local-install-root`/bin/bootstrap build
	@cd build && zip function.zip bootstrap && rm bootstrap && cd ..
