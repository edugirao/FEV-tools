# Compiler and flags
FC      = gfortran
FFLAGS2  = -g -pg
FFLAGS  = -fbounds-check -fcheck=all -Wall -g -Wall -Wmaybe-uninitialized

# Directories
SRC_DIR = Src
OBJ_DIR = Obj
BIN_DIR = .

# Automatically find all source files
SRC_FILES := $(wildcard $(SRC_DIR)/*.f90)

# Default module and object handling
ALL_OBJS := $(patsubst $(SRC_DIR)/%.f90, $(OBJ_DIR)/%.o, $(SRC_FILES))

# ====== PROGRAM-SPECIFIC DEPENDENCIES ======
# List explicitly which modules each main program depends on (no need for .f90 extension)

# fcs2flg depends on ...
FCS2FEV_DEPS = tools_read

# fcs2flg depends on ...
FCS2FLG_DEPS = tools_read tools_flag

# fcs2kid depends on ...
FCS2KID_DEPS = tools_adim tools_flag tools_maps

# fcs2smb depends on ...
FCS2SMB_DEPS = tools_read

# fcs2xyz depends on ...
FCS2XYZ_DEPS = tools_read tools_coor

# fev2fcs depends on ...
FEV2FCS_DEPS = tools_read tools_flag tools_rule tools_bipt tools_maps tools_adim

# fev2flg depends on ...
FEV2FLG_DEPS = tools_read tools_flag tools_rule tools_bipt tools_maps

# fcs2flg depends on ...
FEV2POV_DEPS = 

# fev2sum depends on ...
FEV2SUM_DEPS = tools_read tools_rule tools_flag

# flg2fcs depends on ...
FLG2FCS_DEPS = tools_read tools_adim

# fcs2flg depends on ...
FLG2FEV_DEPS = tools_read

# flg2sym depends on ...
FLG2ISO_DEPS = tools_read tools_maps tools_flag

# flg2pri depends on ...
FLG2PRI_DEPS = tools_read tools_flag tools_maps

# flg2sym depends on ...
FLG2SYM_DEPS = tools_read tools_flag tools_maps tools_symm

# for2bin depends on ...
FOR2BIN_DEPS = tools_read

# ====== END OF PROGRAM-SPECIFIC CONFIG ======

# Derived object lists
FCS2FEV_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FCS2FEV_DEPS))) $(OBJ_DIR)/fcs2fev.o
FCS2FLG_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FCS2FLG_DEPS))) $(OBJ_DIR)/fcs2flg.o
FCS2KID_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FCS2KID_DEPS))) $(OBJ_DIR)/fcs2kid.o
FCS2SMB_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FCS2SMB_DEPS))) $(OBJ_DIR)/fcs2smb.o
FCS2XYZ_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FCS2XYZ_DEPS))) $(OBJ_DIR)/fcs2xyz.o
FEV2FCS_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FEV2FCS_DEPS))) $(OBJ_DIR)/fev2fcs.o
FEV2FLG_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FEV2FLG_DEPS))) $(OBJ_DIR)/fev2flg.o
FEV2POV_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FEV2POV_DEPS))) $(OBJ_DIR)/fev2pov.o
FEV2SUM_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FEV2SUM_DEPS))) $(OBJ_DIR)/fev2sum.o
FLG2FEV_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FLG2FEV_DEPS))) $(OBJ_DIR)/flg2fev.o
FLG2FCS_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FLG2FCS_DEPS))) $(OBJ_DIR)/flg2fcs.o
FLG2ISO_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FLG2ISO_DEPS))) $(OBJ_DIR)/flg2iso.o
FLG2PRI_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FLG2PRI_DEPS))) $(OBJ_DIR)/flg2pri.o
FLG2SYM_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FLG2SYM_DEPS))) $(OBJ_DIR)/flg2sym.o
FOR2BIN_OBJS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(FOR2BIN_DEPS))) $(OBJ_DIR)/for2bin.o

# Executables
EXE01 = $(BIN_DIR)/fcs2fev
EXE02 = $(BIN_DIR)/fcs2flg
EXE03 = $(BIN_DIR)/fcs2kid
EXE04 = $(BIN_DIR)/fcs2smb
EXE05 = $(BIN_DIR)/fcs2xyz
EXE06 = $(BIN_DIR)/fev2fcs
EXE07 = $(BIN_DIR)/fev2flg
EXE08 = $(BIN_DIR)/fev2pov
EXE09 = $(BIN_DIR)/fev2sum
EXE10 = $(BIN_DIR)/flg2fcs
EXE11 = $(BIN_DIR)/flg2fev
EXE12 = $(BIN_DIR)/flg2iso
EXE13 = $(BIN_DIR)/flg2pri
EXE14 = $(BIN_DIR)/flg2sym
EXE15 = $(BIN_DIR)/for2bin


# Default target: build both
all: $(EXE01) $(EXE02) $(EXE03) $(EXE04) $(EXE05) $(EXE06) $(EXE07) $(EXE08) $(EXE09) $(EXE10) $(EXE11) $(EXE12) $(EXE13) $(EXE14) $(EXE15)

# Build rules
$(EXE01): $(FCS2FEV_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE02): $(FCS2FLG_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE03): $(FCS2KID_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE04): $(FCS2SMB_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE05): $(FCS2XYZ_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE06): $(FEV2FCS_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE07): $(FEV2FLG_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE08): $(FEV2POV_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE09): $(FEV2SUM_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE10): $(FLG2FCS_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE11): $(FLG2FEV_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE12): $(FLG2ISO_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE13): $(FLG2PRI_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE14): $(FLG2SYM_OBJS)
	$(FC) $(FFLAGS) -o $@ $^

$(EXE15): $(FOR2BIN_OBJS)
	$(FC) $(FFLAGS) -o $@ $^
	

# Rule for object file compilation
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90 | $(OBJ_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Ensure Obj directory exists
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

# Named builds
build-fcs2fev: $(EXE01)
build-fcs2flg: $(EXE02)
build-fcs2kid: $(EXE03)
build-fcs2smb: $(EXE04)
build-fcs2xyz: $(EXE05)
build-fev2fcs: $(EXE06)
build-fev2flg: $(EXE07)
build-fev2pov: $(EXE08)
build-fev2sum: $(EXE09)
build-flg2fcs: $(EXE10)
build-flg2fev: $(EXE11)
build-flg2iso: $(EXE12)
build-flg2pri: $(EXE13)
build-flg2sym: $(EXE14)
build-for2bin: $(EXE15)

# Clean
clean:
	rm -f $(OBJ_DIR)/*.o $(EXE01) $(EXE02) $(EXE03) $(EXE04) $(EXE05) $(EXE06) $(EXE07) $(EXE08) $(EXE09) $(EXE10) $(EXE11) $(EXE12) $(EXE13) $(EXE14) $(EXE15)

# Phony targets
.PHONY: all clean build-fcs2fev build-fcs2flg build-fcs2kid build-fcs2smb build-fcs2xyz build-fev2fcs build-fev2flg build-fev2pov build-fev2sum build-flg2fcs build-flg2fev build-flg2iso build-flg2pri build-flg2sym build-for2bin
