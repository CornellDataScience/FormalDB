MODULES=Table
CSV_MODULES=csv_table
OBJECTS=$(MODULES:=.cmo)
CSV_OBJECTS=$(CSV_MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
TEST_CMD=test_commands.byte
TEST_DIFF=diff_tester.byte
MAIN=Table.byte
MAIN_CSV=csv_table.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -pkgs $(PKGS)
OCAMLBUILD2=ocamlbuild -use-ocamlfind -pkgs $(PKGS2)
PKGS=csv
PKGS2=csv
default: build
	utop
table-csv:
	ocamlbuild -use-ocamlfind -pkgs csv csv_table.cmo csv_table.byte
build: 
	$(OCAMLBUILD) $(OBJECTS) $(MAIN)
test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)
test-diff:
	$(OCAMLBUILD) -tag debug $(TEST_DIFF) && ./$(TEST_DIFF)
test-cmd:
	$(OCAMLBUILD) -tag debug $(TEST_CMD) && ./$(TEST_CMD)
docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
	-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)
clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private .git-ml search_src.zip
zip:
	zip git-ml_src.zip *.ml* _tags git-ml test_routine Makefile
production:
	node formal_db_server.js & echo "$$!" > server_pid.txt

rebuild: clean build
