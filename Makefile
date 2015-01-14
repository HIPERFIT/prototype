EXECUTABLE=./dist/build/examples/examples
GENERIC_PRICING=./finpar/GenericPricing
PRICING_ENGINE=$(GENERIC_PRICING)/CppOpenCL
PRICING_ENGINE_CODE=$(PRICING_ENGINE)/ContractDefs
PRICING_ENGINE_DATA=$(GENERIC_PRICING)/Data

build_examples: 
	cabal build examples

copy_generated: 
	cp ./generated/*.cl $(PRICING_ENGINE_CODE)
	cp ./generated/input.data $(PRICING_ENGINE_DATA)/Medium/input.data

run_pricing:
	$(MAKE) -C $(PRICING_ENGINE) clean && $(MAKE) -C $(PRICING_ENGINE) gpu && $(MAKE) -C $(PRICING_ENGINE) run_medium

generate:
	$(EXECUTABLE)

run_test: build_examples generate copy_generated run_pricing

