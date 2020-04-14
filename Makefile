MAIN_FILE = Main.hs
BIN_DIR   = bin
OUT_NAME  = quotegenerator.out

clean:
	@rm -rf *.o
	@rm -rf *.hi

build:
	ghc $(MAIN_FILE) -o $(BIN_DIR)/$(OUT_NAME)

run:
	@./$(BIN_DIR)/$(OUT_NAME)
