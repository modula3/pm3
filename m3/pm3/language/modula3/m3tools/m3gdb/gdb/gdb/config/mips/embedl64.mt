# Target: Big-endian mips board, typically an IDT.
TDEPFILES= mips-tdep.o remote-mips.o remote-array.o
TM_FILE= tm-embedl64.h
SIM_OBS = remote-sim.o
SIM = ../sim/mips/libsim.a
