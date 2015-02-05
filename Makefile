EXECUTABLE=./dist/build/tests/tests
GENERIC_PRICING=./finpar/GenericPricing
PRICING_ENGINE=$(GENERIC_PRICING)/CppOpenCL
PRICING_ENGINE_CODE=$(PRICING_ENGINE)/ContractDefs
PRICING_ENGINE_DATA=$(GENERIC_PRICING)/Data

build_examples: 
	cabal build tests

run_examples:
	$(EXECUTABLE)

run_test: build_examples run_examples

clean_opencl:
	rm -f ./finpar/GenericPricing/CppOpenCL/*.ptx
	rm -rf $(HOME)/.nv/ComputeCache/*
