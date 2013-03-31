REBAR := bin/rebar
DIALYZER := dialyzer
DIALYZER_APPS := kernel stdlib sasl inets crypto public_key ssl
GIT_SERVER := https://github.com/basho
DEPS := $(CURDIR)/deps
BIN := $(CURDIR)/bin
APP := zerpc

BASIC_PLT := $(APP).plt

CURRENT_BRANCH := $(shell git branch --no-color 2> /dev/null | grep \* | cut -d " " -f 2)
ifeq (master, $(CURRENT_BRANCH))
REBAR_EXE := $(REBAR) -C rebar.config.lock
else
REBAR_EXE := $(REBAR) -C rebar.config
endif

.PHONY: all deps clean test ct xref docs lock-deps

all: app

app: $(REBAR) deps
	@$(REBAR_EXE) compile

deps: $(REBAR) 
	@$(REBAR_EXE) get-deps

clean: $(REBAR)
	@$(REBAR_EXE) clean

ifndef SUITES
EUNIT_SUITES =
else
EUNIT_SUITES = suites=$(SUITES)
endif
test: $(REBAR) app
	@$(REBAR_EXE) eunit skip_deps=true $(EUNIT_SUITES)

ct: $(REBAR) app
	@$(REBAR_EXE) ct skip_deps=true

rel: app rel/$(APP)

rel/$(APP):
	@$(REBAR_EXE) generate

relclean:
	@rm -rf rel/$(APP)

$(BASIC_PLT): build-plt

build-plt: 
	@$(DIALYZER) --build_plt --output_plt $(BASIC_PLT) --apps $(DIALYZER_APPS)

dialyze: $(BASIC_PLT)
	@$(DIALYZER) -r src deps/*/src --no_native --src --plt $(BASIC_PLT) -Werror_handling \
		-Wrace_conditions -Wunmatched_returns # -Wunderspecs

xref: $(REBAR) clean app
	@$(REBAR_EXE) xref skip_deps=true

docs: $(REBAR)
	@$(REBAR_EXE) doc skip_deps=true

lock-deps: $(REBAR) app
	@$(REBAR_EXE) lock-deps ignore=meck,proper,rebar

bin/%:
	@mkdir -p $(DEPS) $(BIN)
	git clone $(GIT_SERVER)/$*.git $(DEPS)/$*
	$(MAKE) -C $(DEPS)/$*
	cp $(DEPS)/$*/$* $(BIN)/$*
