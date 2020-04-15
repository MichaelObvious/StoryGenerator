MAIN_FILE = Main.hs
BIN_DIR   = bin
OUT_NAME  = quotegenerator.out
FLAGS     = -Wall -Werror

.PHONY: setup run

run: build clean
	@./$(BIN_DIR)/$(OUT_NAME)

build:
	ghc $(MAIN_FILE) -o $(BIN_DIR)/$(OUT_NAME) $(FLAGS)

clean:
	@rm -rf *.o
	@rm -rf *.hi

setup:
	@mkdir bin
