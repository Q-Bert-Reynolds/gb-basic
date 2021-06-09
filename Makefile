GAME_NAME = GB_BASIC
ROM_NAME = $(GAME_NAME).gbc
DIST_DIR = ./bin
ROM_FILE = $(DIST_DIR)/$(ROM_NAME)
SYM_FILE = $(DIST_DIR)/$(GAME_NAME).sym
OBJ_DIR = ./build
SRC_DIR = ./src


all:
	@mkdir -p $(OBJ_DIR)
	@mkdir -p $(DIST_DIR)
	
	rgbasm -o $(OBJ_DIR)/main.o $(SRC_DIR)/main.asm
	rgblink  -m $(OBJ_DIR)/main.map -n $(SYM_FILE) -o $(ROM_FILE) $(OBJ_DIR)/main.o
	rgbfix -v $(ROM_FILE)

ifeq ($(OS), Windows_NT)
	start "$(ROM_FILE)"
else
	open $(ROM_FILE)
endif
	

clean:
	@rm -rf $(DIST_DIR)
	@rm -rf $(OBJ_DIR)

