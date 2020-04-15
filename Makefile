MAIN_FILE = Main.hs
BIN_DIR   = bin
OUT_NAME  = story_generator.out
FLAGS     = -Wall -Werror

.PHONY: setup run

run:
	@./$(BIN_DIR)/$(OUT_NAME)

build:
	ghc $(MAIN_FILE) -o $(BIN_DIR)/$(OUT_NAME) $(FLAGS)

clean:
	@rm -rf *.o
	@rm -rf *.hi
	@rm -rf bin/*

setup:
	@mkdir bin
