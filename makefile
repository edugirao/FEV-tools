FC      = gfortran
# -J $(OBJ_DIR) tells gfortran to put/look for .mod files in the Obj folder
FFLAGS  = -fbounds-check -fcheck=all -Wall -g -Wmaybe-uninitialized -J $(OBJ_DIR)
SRC_DIR = Src
OBJ_DIR = Obj
BIN_DIR = .

# 1. Dependency Variables (Single point of truth)
fcs2fev_D = tools_read tools_writ tools_conv
fcs2flg_D = tools_read tools_writ tools_conv 
fcs2kid_D = tools_read tools_writ tools_conv tools_maps tools_adim   
fcs2smb_D = tools_read
fcs2xyz_D = tools_read tools_writ tools_coor 
fev2fcs_D = tools_read tools_writ tools_flag tools_maps tools_rule tools_bipt tools_conv
fev2flg_D = tools_read tools_writ tools_flag tools_maps tools_rule tools_bipt
fev2pov_D = tools_read
fev2sum_D = tools_read tools_rule tools_flag
flg2fcs_D = tools_read tools_writ tools_conv tools_maps 
flg2fev_D = tools_read tools_writ tools_conv 
flg2iso_D = tools_read tools_maps 
flg2pri_D = tools_read tools_writ tools_maps 
flg2sym_D = tools_read tools_writ tools_symm tools_maps
for2bin_D = tools_read tools_writ

# 2. Targets
PROGS = fcs2fev fcs2flg fcs2kid fcs2smb fcs2xyz fev2fcs fev2flg \
        fev2pov fev2sum flg2fcs flg2fev flg2iso flg2pri flg2sym for2bin
EXES  = $(patsubst %,$(BIN_DIR)/%,$(PROGS))

all: $(EXES)

# 3. Explicit Dependencies (Prevents Fatal Errors & allows surgical recompilation)
# This section ensures that if a tool changes, only the programs using it recompile.
$(OBJ_DIR)/fcs2fev.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(fcs2fev_D)))
$(OBJ_DIR)/fcs2flg.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(fcs2flg_D)))
$(OBJ_DIR)/fcs2kid.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(fcs2kid_D)))
$(OBJ_DIR)/fcs2smb.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(fcs2smb_D)))
$(OBJ_DIR)/fcs2xyz.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(fcs2xyz_D)))
$(OBJ_DIR)/fev2fcs.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(fev2fcs_D)))
$(OBJ_DIR)/fev2flg.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(fev2flg_D)))
$(OBJ_DIR)/fev2sum.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(fev2sum_D)))
$(OBJ_DIR)/flg2fcs.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg2fcs_D)))
$(OBJ_DIR)/flg2fev.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg2fev_D)))
$(OBJ_DIR)/flg2iso.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg2iso_D)))
$(OBJ_DIR)/flg2pri.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg2pri_D)))
$(OBJ_DIR)/flg2sym.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg2sym_D)))
$(OBJ_DIR)/for2bin.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(for2bin_D)))

# Inter-module dependency
$(OBJ_DIR)/tools_coor.o: $(OBJ_DIR)/tools_read.o

# 4. Linking Rules
# Using $*_D to pull in the correct objects for the linker
$(EXES): $(BIN_DIR)/%: $(OBJ_DIR)/%.o
	$(FC) $(FFLAGS) -o $@ $^ $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$($*_D)))

# 5. Generic Compilation Rule
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90 | $(OBJ_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

clean:
	rm -rf $(OBJ_DIR) $(EXES)

.PHONY: all clean
