# Target: Big-endian SIM monitor board.
TDEPFILES= mips-tdep.o remote-mips.o
TM_FILE= tm-vr4300.h
SIM_OBS = remote-sim.o
SIM = ../sim/mips/libsim.a
