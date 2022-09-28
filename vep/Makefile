CURRENT_DIR=.
COQBIN=

-include CONFIGURE

COQC=$(COQBIN)coqc
COQDEP=$(COQBIN)coqdep
CLIGHTGEN=$(COQBIN)clightgen

DIRS = \
	EbpfExec example
	
COQ_FLAG = -Q . Ebpf  -Q $(VSTCFORMDIR) VSTCForm

DEP_FLAG = -Q . Ebpf  -Q $(VSTCFORMDIR) VSTCForm

CLIGHTGEN_FLAG = -normalize

example_FILES = \
    Extraction.v test.v
    
EbpfExec_FILES = \
	Ebpfstatement.v EbpfExec.v Ebpfop.v Ebpfstate.v Ebpf_helper.v Map_sep.v
    
FILES = \
  $(example_FILES:%.v=example/%.v) \
  $(EbpfExec_FILES:%.v=EbpfExec/%.v)

$(FILES:%.v=%.vo): %.vo: %.v
	@echo COQC $*.v
	@$(COQC) $(COQ_FLAG) $(CURRENT_DIR)/$*.v

all: \
  $(C_FILES:%.c=%.v) \
  $(FILES:%.v=%.vo) \

_CoqProject:
	@echo $(COQ_FLAG) > _CoqProject

depend:
	$(COQDEP) $(DEP_FLAG) $(FILES) > .depend

.depend:
	@$(COQDEP) $(DEP_FLAG) $(FILES) > .depend

clean:
	@rm */*.vo */*.glob */*.vok */*.vos */.*.aux

.DEFAULT_GOAL := all

include .depend

