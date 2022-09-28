## Samples used in VEP
This repository contains examples used in VEP:  
./common-examples: Examples that VEP used to evaluate time and memory usage. Mainly taken from Linux kernel.  
./counter-examples: Examples we design to demonstrate full programmability of VEP.  
./vep/: Source code of VEP (source code of VSTC is not open-sourced)  
helper_support.md: List helper functions we have supported.  
example_for_kmp.md: Show how to write assertions for an eBPF program.  

## How to reproduce our results
Considering that VSTC is not open-sourced, we provide a docker image for reproduction. To get the image, use the following command: <br>
```
docker pull hty690/vep
```
In this docker image, our codes are stored in /home/opam/vep. Samples we have tested are stored in /home/opam/vep/Verfied_examples. The following commands show how to verify different test cases:
```
eval $(opam env)
cd /home/opam/vep
cp Verified_examples/<examples you want to test>.v example/test.v
make
dune build
./_build/default/main.exe
```