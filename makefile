FC      = gfortran
# -J $(OBJ_DIR) tells gfortran to put/look for .mod files in the Obj folder
FFLAGS  = -fbounds-check -fcheck=all -Wall -g -Wmaybe-uninitialized -J $(OBJ_DIR)
SRC_DIR = Src
OBJ_DIR = Obj
BIN_DIR = .

# 1. Dependency Variables (Single point of truth)
fev2flg_D = tools_read tools_writ tools_flag tools_maps tools_rule tools_bipt
fev2pov_D = tools_read tools_povr tools_conv
fev2sum_D = tools_read tools_writ tools_rule tools_conv
flg2fev_D = tools_read tools_writ tools_conv 
flg2gen_D = tools_read tools_writ tools_conv tools_maps tools_adim   
flg2iso_D = tools_read tools_maps tools_conv
flg2pri_D = tools_read tools_writ tools_maps tools_flag tools_conv
flg2smb_D = tools_read tools_writ tools_symb tools_conv
flg2sym_D = tools_read tools_writ tools_symm tools_maps tools_conv
flg2xyz_D = tools_read tools_writ tools_relx 
flg4xyz_D = tools_read tools_writ tools_conv tools_make tools_maps
for2bin_D = tools_read tools_writ
flg2svg_D = tools_read tools_make tools_msvg
#fcs2coo_D = tools_read tools_conv tools_maps tools_writ tools_adim tools_ddim tools_relx 

# 2. Targets
PROGS = fev2flg fev2pov fev2sum flg2fev flg2gen flg2iso flg2pri \
        flg2svg flg2sym flg2smb flg2xyz flg4xyz for2bin
EXES  = $(patsubst %,$(BIN_DIR)/%,$(PROGS))

all: $(EXES)

# 3. Explicit Dependencies (Prevents Fatal Errors & allows surgical recompilation)
# This section ensures that if a tool changes, only the programs using it recompile.
$(OBJ_DIR)/fev2flg.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(fev2flg_D)))
$(OBJ_DIR)/fev2pov.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(fev2pov_D)))
$(OBJ_DIR)/fev2sum.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(fev2sum_D)))
$(OBJ_DIR)/flg2gen.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg2gen_D)))
$(OBJ_DIR)/flg2fev.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg2fev_D)))
$(OBJ_DIR)/flg2iso.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg2iso_D)))
$(OBJ_DIR)/flg2pri.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg2pri_D)))
$(OBJ_DIR)/flg2smb.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg2smb_D)))
$(OBJ_DIR)/flg2sym.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg2sym_D)))
$(OBJ_DIR)/flg2xyz.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg2xyz_D)))
$(OBJ_DIR)/flg4xyz.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg4xyz_D)))
$(OBJ_DIR)/for2bin.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(for2bin_D)))
$(OBJ_DIR)/flg2svg.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(flg2svg_D)))
#$(OBJ_DIR)/fcs2coo.o: $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(fcs2coo_D)))

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
