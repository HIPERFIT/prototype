TESTS=./dist/build/tests/tests
WEB=./dist/build/web/web
API=./dist/build/API/API
GENERIC_PRICING=./finpar/GenericPricing
PRICING_ENGINE=$(GENERIC_PRICING)/CppOpenCL
PRICING_ENGINE_CODE=$(PRICING_ENGINE)/ContractDefs
PRICING_ENGINE_DATA=$(GENERIC_PRICING)/Data

compile_opencl:
	make -C $(PRICING_ENGINE) clean gpu

dependencies:
	cabal install --dependencies-only

build_examples: dependencies
	cabal build tests

run_examples:
	$(TESTS)

run_test: build_examples run_examples

clean_opencl:
	rm -f ./finpar/GenericPricing/CppOpenCL/*.ptx
	rm -rf $(HOME)/.nv/ComputeCache/*

run_web:dependencies
	cabal build web
	$(WEB)

